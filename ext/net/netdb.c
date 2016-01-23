/*
 * netdb.c - obtain information about network
 *
 *   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   3. Neither the name of the authors nor the names of its contributors
 *      may be used to endorse or promote products derived from this
 *      software without specific prior written permission.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "gauche-net.h"
#include <gauche/class.h>

#define DATA_BUFSIZ  980

static struct netdb_data_rec {
    int dummy;                  /* dummy var to make sure this structure is
                                   placed in data area. */
    ScmInternalMutex hostent_mutex;
    ScmInternalMutex protoent_mutex;
    ScmInternalMutex servent_mutex;
} netdb_data = { 1 };

#define WITH_GLOBAL_LOCK(mutex, body)           \
    SCM_UNWIND_PROTECT {                        \
      SCM_INTERNAL_MUTEX_LOCK(mutex);           \
      body;                                     \
    } SCM_WHEN_ERROR {                          \
        SCM_INTERNAL_MUTEX_UNLOCK(mutex);       \
        SCM_NEXT_HANDLER;                       \
    } SCM_END_PROTECT;                          \
    SCM_INTERNAL_MUTEX_UNLOCK(mutex)

/*-------------------------------------------------------------
 * Hostent
 */

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_SysHostentClass, NULL);

static ScmSysHostent *make_hostent(struct hostent *he)
{
    ScmSysHostent *entry = SCM_NEW(ScmSysHostent);
    ScmObj h = SCM_NIL, t = SCM_NIL;

    SCM_SET_CLASS(entry, SCM_CLASS_SYS_HOSTENT);
    entry->name = SCM_MAKE_STR_COPYING(he->h_name);
    entry->aliases = Scm_CStringArrayToList((const char**)he->h_aliases, -1,
                                            SCM_STRING_COPYING);
    if (he->h_addrtype == AF_INET) {
        for (char **p = he->h_addr_list; *p; p++) {
            char buf[50];
            struct in_addr *addr = (struct in_addr*)*p;
            unsigned long addrval = ntohl(addr->s_addr);
            /* avoid using inet_ntoa, for it is not reentrant */
            snprintf(buf, 50, "%ld.%ld.%ld.%ld",
                     ((addrval >> 24)& 0xff), ((addrval >> 16) & 0xff),
                     ((addrval >> 8) & 0xff), (addrval & 0xff));
            SCM_APPEND1(h, t, SCM_MAKE_STR_COPYING(buf));
        }
    } else {
        /* soon AF_INET6 will be supported (hopefully) */
        Scm_Error("unknown address type (%d)", he->h_addrtype);
    }
    entry->addresses = h;
    return entry;
}

ScmObj Scm_GetHostByName(const char *name)
{
#if defined(GETHOSTBYNAME_R_NUMARGS)
    ScmObj entry = SCM_FALSE;
    {
        int herr = 0, bufsiz = DATA_BUFSIZ;
        struct hostent he;
        char staticbuf[DATA_BUFSIZ], *buf = staticbuf;

        for (;;) {
#if GETHOSTBYNAME_R_NUMARGS == 5
            if (gethostbyname_r(name, &he, buf, bufsiz, &herr) != NULL) break;
            if (herr != ERANGE) return SCM_FALSE;
            bufsiz *= 2;
            buf = SCM_NEW_ATOMIC2(char*, bufsiz);
#elif GETHOSTBYNAME_R_NUMARGS == 6
            struct hostent *rhe;
            gethostbyname_r(name, &he, buf, bufsiz, &rhe, &herr);
            if (rhe != 0) break;
            else if (herr != ERANGE) return SCM_FALSE;
            bufsiz *= 2;
            buf = SCM_NEW_ATOMIC2(char*, bufsiz);
#else
#error unsupported gethostbyname_r variant (configuration error?)
#endif
        }
        entry = SCM_OBJ(make_hostent(&he));
    }
    return entry;
#else /* !defined(GETHOSTBYNAME_R_NUMARGS) */
    /* We don't have thread-safe call.  Using global lock. */
    volatile ScmObj entry = SCM_FALSE;
    WITH_GLOBAL_LOCK(netdb_data.hostent_mutex,
                     do {
                         struct hostent *he;
                         he = gethostbyname(name);
                         if (he != NULL) entry = SCM_OBJ(make_hostent(he));
                     } while (0));
    return entry;
#endif /* !defined(GETHOSTBYNAME_R_NUMARGS) */
}

ScmObj Scm_GetHostByAddr(const char *addr, int type)
{
    struct in_addr iaddr;
    if (type != AF_INET) {
        Scm_Error("unsupported address type: %d", type);
    }
    if (inet_pton(AF_INET, addr, &iaddr) <= 0) {
        Scm_Error("bad inet address format: %s", addr);
    }

#if defined(GETHOSTBYADDR_R_NUMARGS)
    {
        ScmObj entry = SCM_FALSE;
        int herr = 0, bufsiz = DATA_BUFSIZ;
        struct hostent he;
        char staticbuf[DATA_BUFSIZ], *buf = staticbuf;
        for (;;) {
#if GETHOSTBYADDR_R_NUMARGS == 7
            if (gethostbyaddr_r((void *)&iaddr, sizeof(struct in_addr),
                                AF_INET, &he, buf, bufsiz, &herr) != NULL) {
                break;
            }
            if (herr != ERANGE) return SCM_FALSE;
            bufsiz *= 2;
            buf = SCM_NEW_ATOMIC2(char*, bufsiz);
#elif GETHOSTBYADDR_R_NUMARGS == 8
            struct hostent *rhe;
            gethostbyaddr_r((void *)&iaddr, sizeof(struct in_addr),
                            AF_INET, &he, buf, bufsiz, &rhe, &herr);
            if (rhe != NULL) break;
            if (herr != ERANGE) return SCM_FALSE;
            bufsiz *= 2;
            buf = SCM_NEW_ATOMIC2(char*, bufsiz);
#else
#error unsupported gethostbyaddr_r variant (configuration error?)
#endif
        }
        entry = SCM_OBJ(make_hostent(&he));
        return entry;
    }
#else /* !defined(GETHOSTBYADDR_R_NUMARGS) */
    {
        volatile ScmObj entry = SCM_FALSE;
        struct hostent *he;

        WITH_GLOBAL_LOCK(netdb_data.hostent_mutex,
                         do {
                             he = gethostbyaddr((void*)&iaddr,
                                                sizeof(struct in_addr),
                                                AF_INET);
                             if (he != NULL) {
                                 entry = SCM_OBJ(make_hostent(he));
                             }
                         } while (0));
        return entry;
    }
#endif /* !defined(GETHOSTBYADDR_R_NUMARGS) */
}

static ScmObj hostent_name(ScmSysHostent *entry)
{
    return entry->name;
}

static ScmObj hostent_aliases(ScmSysHostent *entry)
{
    return entry->aliases;
}

static ScmObj hostent_addresses(ScmSysHostent *entry)
{
    return entry->addresses;
}

static ScmClassStaticSlotSpec hostent_slots[] = {
    SCM_CLASS_SLOT_SPEC("name", hostent_name, NULL),
    SCM_CLASS_SLOT_SPEC("aliases", hostent_aliases, NULL),
    SCM_CLASS_SLOT_SPEC("addresses", hostent_addresses, NULL),
    SCM_CLASS_SLOT_SPEC_END()
};

/*-------------------------------------------------------------
 * Protoent
 */

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_SysProtoentClass, NULL);

static ScmSysProtoent *make_protoent(struct protoent *pe)
{
    ScmSysProtoent *entry = SCM_NEW(ScmSysProtoent);

    SCM_SET_CLASS(entry, SCM_CLASS_SYS_PROTOENT);
    entry->name = SCM_MAKE_STR_COPYING(pe->p_name);
    entry->aliases = Scm_CStringArrayToList((const char **)pe->p_aliases, -1,
                                            SCM_STRING_COPYING);
    entry->proto = Scm_MakeInteger(pe->p_proto);
    return entry;
}

ScmObj Scm_GetProtoByName(const char *name)
{
#if defined(GETPROTOBYNAME_R_NUMARGS)
    ScmObj entry = SCM_FALSE;
    {
        int bufsiz = DATA_BUFSIZ;
        struct protoent pe;
        char staticbuf[DATA_BUFSIZ], *buf = staticbuf;

        for (;;) {
#if GETPROTOBYNAME_R_NUMARGS == 4
            if (getprotobyname_r(name, &pe, buf, bufsiz) != NULL) break;
            if (errno != ERANGE) return SCM_FALSE;
            bufsiz *= 2;
            buf = SCM_NEW_ATOMIC2(char*, bufsiz);
#elif GETPROTOBYNAME_R_NUMARGS == 5
            struct protoent *rpe;
            getprotobyname_r(name, &pe, buf, bufsiz, &rpe);
            if (rpe != 0) break;
            else if (errno != ERANGE) return SCM_FALSE;
            bufsiz *= 2;
            buf = SCM_NEW_ATOMIC2(char*, bufsiz);
#else
#error unsupported getprotobyname_r variant (configuration error?)
#endif
        }
        entry = SCM_OBJ(make_protoent(&pe));
    }
    return entry;
#else /* !defined(GETPROTOBYNAME_R_NUMARGS) */
    volatile ScmObj entry = SCM_FALSE;
    WITH_GLOBAL_LOCK(netdb_data.protoent_mutex,
                     do {
                         struct protoent *pe = getprotobyname(name);
                         if (pe != NULL) entry = SCM_OBJ(make_protoent(pe));
                     } while (0));
    return entry;
#endif /* !defined(GETPROTOBYNAME_R_NUMARGS) */
}

ScmObj Scm_GetProtoByNumber(int number)
{
#if defined(GETPROTOBYNUMBER_R_NUMARGS)
    ScmObj entry = SCM_FALSE;
    {
        int bufsiz = DATA_BUFSIZ;
        struct protoent pe;
        char staticbuf[DATA_BUFSIZ], *buf = staticbuf;

        for (;;) {
#if GETPROTOBYNUMBER_R_NUMARGS == 4
            if (getprotobynumber_r(number, &pe, buf, bufsiz) != NULL) break;
            if (errno != ERANGE) return SCM_FALSE;
            bufsiz *= 2;
            buf = SCM_NEW_ATOMIC2(char*, bufsiz);
#elif GETPROTOBYNUMBER_R_NUMARGS == 5
            struct protoent *rpe;
            getprotobynumber_r(number, &pe, buf, bufsiz, &rpe);
            if (rpe != 0) break;
            else if (errno != ERANGE) return SCM_FALSE;
            bufsiz *= 2;
            buf = SCM_NEW_ATOMIC2(char*, bufsiz);
#else
#error unsupported getprotobyname_r variant (configuration error?)
#endif
        }
        entry = SCM_OBJ(make_protoent(&pe));
    }
    return entry;
#else /* !defined(GETPROTOBYNUMBER_R_NUMARGS) */
    volatile ScmObj entry = SCM_FALSE;
    WITH_GLOBAL_LOCK(netdb_data.protoent_mutex,
                     do {
                         struct protoent *pe = getprotobynumber(number);
                         if (pe != NULL) entry = SCM_OBJ(make_protoent(pe));
                     } while (0));
    return entry;
#endif /* !defined(GETPROTOBYNUMBER_R_NUMARGS) */
}

static ScmObj protoent_name(ScmSysProtoent *entry)
{
    return entry->name;
}

static ScmObj protoent_aliases(ScmSysProtoent *entry)
{
    return entry->aliases;
}

static ScmObj protoent_proto(ScmSysProtoent *entry)
{
    return entry->proto;
}

static ScmClassStaticSlotSpec protoent_slots[] = {
    SCM_CLASS_SLOT_SPEC("name", protoent_name, NULL),
    SCM_CLASS_SLOT_SPEC("aliases", protoent_aliases, NULL),
    SCM_CLASS_SLOT_SPEC("proto", protoent_proto, NULL),
    SCM_CLASS_SLOT_SPEC_END()
};

/*-------------------------------------------------------------
 * Servent
 */

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_SysServentClass, NULL);

static ScmSysServent *make_servent(struct servent *se)
{
    ScmSysServent *entry = SCM_NEW(ScmSysServent);

    SCM_SET_CLASS(entry, SCM_CLASS_SYS_SERVENT);
    entry->name = SCM_MAKE_STR_COPYING(se->s_name);
    entry->aliases = Scm_CStringArrayToList((const char **)se->s_aliases, -1,
                                            SCM_STRING_COPYING);
    entry->port = Scm_MakeInteger(ntohs(se->s_port));
    entry->proto = SCM_MAKE_STR_COPYING(se->s_proto);
    return entry;
}

ScmObj Scm_GetServByName(const char *name, const char *proto)
{
#if defined(GETSERVBYNAME_R_NUMARGS)
    ScmObj entry = SCM_FALSE;
    {
        int bufsiz = DATA_BUFSIZ;
        struct servent se;
        char staticbuf[DATA_BUFSIZ], *buf = staticbuf;

        for (;;) {
#if GETSERVBYNAME_R_NUMARGS == 5
            if (getservbyname_r(name, proto, &se, buf, bufsiz) != NULL) break;
            if (errno != ERANGE) return SCM_FALSE;
            bufsiz *= 2;
            buf = SCM_NEW_ATOMIC2(char*, bufsiz);
#elif GETSERVBYNAME_R_NUMARGS == 6
            struct servent *rse;
            getservbyname_r(name, proto, &se, buf, bufsiz, &rse);
            if (rse != 0) break;
            else if (errno != ERANGE) return SCM_FALSE;
            bufsiz *= 2;
            buf = SCM_NEW_ATOMIC2(char*, bufsiz);
#else
#error unsupported getservbyname_r variant (configuration error?)
#endif
        }
        entry = SCM_OBJ(make_servent(&se));
    }
    return entry;
#else /* !defined(GETSERVBYNAME_R_NUMARGS) */
    volatile ScmObj entry = SCM_FALSE;
    WITH_GLOBAL_LOCK(netdb_data.servent_mutex,
                     do {
                         struct servent *se = getservbyname(name, proto);
                         if (se != NULL) entry = SCM_OBJ(make_servent(se));
                     } while (0));
    return entry;
#endif /* !defined(GETSERVBYNAME_R_NUMARGS) */
}

ScmObj Scm_GetServByPort(int port, const char *proto)
{
#if defined(GETSERVBYPORT_R_NUMARGS)
    ScmObj entry = SCM_FALSE;
    {
        int bufsiz = DATA_BUFSIZ;
        struct servent se;
        char staticbuf[DATA_BUFSIZ], *buf = staticbuf;

        for (;;) {
#if GETSERVBYPORT_R_NUMARGS == 5
            if (getservbyport_r(htons(port), proto, &se, buf, bufsiz) != NULL)
                break;
            if (errno != ERANGE) return SCM_FALSE;
            bufsiz *= 2;
            buf = SCM_NEW_ATOMIC2(char*, bufsiz);
#elif GETSERVBYPORT_R_NUMARGS == 6
            struct servent *rse;
            getservbyport_r(htons(port), proto, &se, buf, bufsiz, &rse);
            if (rse != 0) break;
            else if (errno != ERANGE) return SCM_FALSE;
            bufsiz *= 2;
            buf = SCM_NEW_ATOMIC2(char*, bufsiz);
#else
#error unsupported getservbyport_r variant (configuration error?)
#endif
        }
        entry = SCM_OBJ(make_servent(&se));
    }
    return entry;
#else /* !defined(GETSERVBYPORT_R_NUMARGS) */
    volatile ScmObj entry = SCM_FALSE;
    WITH_GLOBAL_LOCK(netdb_data.servent_mutex,
                     do {
                         struct servent *se =
                             getservbyport(htons(port), proto);
                         if (se != NULL) entry = SCM_OBJ(make_servent(se));
                     } while (0));
    return entry;
#endif /* !defined(GETSERVBYPORT_R_NUMARGS) */
}

static ScmObj servent_name(ScmSysServent *entry)
{
    return entry->name;
}

static ScmObj servent_aliases(ScmSysServent *entry)
{
    return entry->aliases;
}

static ScmObj servent_port(ScmSysServent *entry)
{
    return entry->port;
}

static ScmObj servent_proto(ScmSysServent *entry)
{
    return entry->proto;
}

static ScmClassStaticSlotSpec servent_slots[] = {
    SCM_CLASS_SLOT_SPEC("name", servent_name, NULL),
    SCM_CLASS_SLOT_SPEC("aliases", servent_aliases, NULL),
    SCM_CLASS_SLOT_SPEC("port", servent_port, NULL),
    SCM_CLASS_SLOT_SPEC("proto", servent_proto, NULL),
    SCM_CLASS_SLOT_SPEC_END()
};

/*-------------------------------------------------------------
 * Addrinfo
 */

#ifdef HAVE_IPV6

ScmObj addrinfo_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmSysAddrinfo *info = SCM_NEW_INSTANCE(ScmSysAddrinfo, klass);
    info->canonname = NULL;
    info->addr = NULL;
    return SCM_OBJ(info);
}

static ScmSysAddrinfo *make_addrinfo(struct addrinfo *ai)
{
    ScmSysAddrinfo *info = SCM_NEW(ScmSysAddrinfo);

    SCM_SET_CLASS(info, SCM_CLASS_SYS_ADDRINFO);
    info->flags = ai->ai_flags;
    info->family = ai->ai_family;
    info->socktype = ai->ai_socktype;
    info->protocol = ai->ai_protocol;
    info->addrlen = ai->ai_addrlen;
    if (ai->ai_canonname != NULL)
        info->canonname = SCM_STRING(SCM_MAKE_STR_COPYING(ai->ai_canonname));
    if (ai->ai_addr != NULL)
        info->addr = SCM_SOCKADDR(Scm_MakeSockAddr(NULL,
                                                   ai->ai_addr,
                                                   ai->ai_addrlen));
    return info;
}

ScmObj Scm_GetAddrinfo(const char *nodename,
                       const char *servname,
                       struct addrinfo *hints)
{
    ScmObj h = SCM_NIL, t = SCM_NIL;
    struct addrinfo *res0;

    int r = getaddrinfo(nodename, servname, hints, &res0);
#if !defined(GAUCHE_WINDOWS)
    if (r) Scm_Error("getaddrinfo failed: %s", gai_strerror(r));
#else  /*GAUCHE_WINDOWS*/
    if (r) Scm_SysError("getaddrinfo failed");
#endif /*GAUCHE_WINDOWS*/

    for (struct addrinfo *res = res0; res != NULL; res = res->ai_next) {
        SCM_APPEND1(h, t, SCM_OBJ(make_addrinfo(res)));
    }
    freeaddrinfo(res0);
    return h;
}

ScmObj Scm_GetNameinfo(ScmSockAddr *addr, int flags)
{
    char host[NI_MAXHOST], serv[NI_MAXSERV];

    int r = getnameinfo(&addr->addr, addr->addrlen,
                    host, sizeof(host), serv, sizeof(serv), flags);
#if !defined(GAUCHE_WINDOWS)
    if (r) Scm_Error("getnameinfo failed: %s", gai_strerror(r));
#else  /*GAUCHE_WINDOWS*/
    if (r) Scm_SysError("getnameinfo failed");
#endif /*GAUCHE_WINDOWS*/
    return Scm_Values2(SCM_MAKE_STR_COPYING(host), SCM_MAKE_STR_COPYING(serv));
}

#endif /* HAVE_IPV6 */

/*-------------------------------------------------------------
 * Initialize
 */

void Scm_Init_NetDB(ScmModule *mod)
{
    Scm_InitStaticClass(&Scm_SysHostentClass, "<sys-hostent>", mod,
                        hostent_slots, 0);
    Scm_InitStaticClass(&Scm_SysProtoentClass, "<sys-protoent>", mod,
                        protoent_slots, 0);
    Scm_InitStaticClass(&Scm_SysServentClass, "<sys-servent>", mod,
                        servent_slots, 0);
    SCM_INTERNAL_MUTEX_INIT(netdb_data.hostent_mutex);
    SCM_INTERNAL_MUTEX_INIT(netdb_data.protoent_mutex);
    SCM_INTERNAL_MUTEX_INIT(netdb_data.servent_mutex);
}
