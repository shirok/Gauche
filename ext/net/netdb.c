#include "net.h"
#include <gauche/class.h>

#define DATA_BUFSIZ  980

static struct netdb_data_rec {
    int dummy;                  /* dummy var to make sure this structure is
                                   placed in data area. */
    ScmInternalMutex hostent_mutex;
    ScmInternalMutex protoent_mutex;
    ScmInternalMutex servent_mutex;
} netdb_data = { 0 };

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
    char **p;
    
    SCM_SET_CLASS(entry, SCM_CLASS_SYS_HOSTENT);
    entry->name = SCM_MAKE_STR_COPYING(he->h_name);
    for (p = he->h_aliases; *p; p++) {
        SCM_APPEND1(h, t, SCM_MAKE_STR_COPYING(*p));
    }
    entry->aliases = h;
    h = t = SCM_NIL;
    if (he->h_addrtype == AF_INET) {
        for (p = he->h_addr_list; *p; p++) {
            char buf[50];
            struct in_addr *addr = (struct in_addr*)*p;
            unsigned long addrval = ntohl(addr->s_addr);
            /* avoid using inet_ntoa, for it is not reentrant */
            snprintf(buf, 50, "%d.%d.%d.%d",
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
    if (!inet_aton(addr, &iaddr)) {
        Scm_Error("bad inet address format: %s", addr);
    }
    
#if defined(GETHOSTBYNAME_R_NUMARGS)
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
#else /* !defined(GETHOSTBYNAME_R_NUMARGS) */
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
#endif /* !defined(GETHOSTBYNAME_R_NUMARGS) */
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
    { NULL }
};

/*-------------------------------------------------------------
 * Protoent
 */

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_SysProtoentClass, NULL);

static ScmSysProtoent *make_protoent(struct protoent *pe)
{
    ScmSysProtoent *entry = SCM_NEW(ScmSysProtoent);
    ScmObj h = SCM_NIL, t = SCM_NIL;
    char **p;
    
    SCM_SET_CLASS(entry, SCM_CLASS_SYS_PROTOENT);
    entry->name = SCM_MAKE_STR_COPYING(pe->p_name);
    for (p = pe->p_aliases; *p; p++) {
        SCM_APPEND1(h, t, SCM_MAKE_STR_COPYING(*p));
    }
    entry->aliases = h;
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
    { NULL }
};

/*-------------------------------------------------------------
 * Servent
 */

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_SysServentClass, NULL);

static ScmSysServent *make_servent(struct servent *se)
{
    ScmSysServent *entry = SCM_NEW(ScmSysServent);
    ScmObj h = SCM_NIL, t = SCM_NIL;
    char **p;
    
    SCM_SET_CLASS(entry, SCM_CLASS_SYS_SERVENT);
    entry->name = SCM_MAKE_STR_COPYING(se->s_name);
    for (p = se->s_aliases; *p; p++) {
        SCM_APPEND1(h, t, SCM_MAKE_STR_COPYING(*p));
    }
    entry->aliases = h;
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
    { NULL }
};

/*-------------------------------------------------------------
 * Initialize
 */

void Scm_Init_NetDB(ScmModule *mod)
{
    Scm_InitBuiltinClass(&Scm_SysHostentClass, "<sys-hostent>",
                         hostent_slots, sizeof(ScmSysHostent), mod);
    Scm_InitBuiltinClass(&Scm_SysProtoentClass, "<sys-protoent>",
                         protoent_slots, sizeof(ScmSysProtoent), mod);
    Scm_InitBuiltinClass(&Scm_SysServentClass, "<sys-servent>",
                         servent_slots, sizeof(ScmSysServent), mod);
    SCM_INTERNAL_MUTEX_INIT(netdb_data.hostent_mutex);
    SCM_INTERNAL_MUTEX_INIT(netdb_data.protoent_mutex);
    SCM_INTERNAL_MUTEX_INIT(netdb_data.servent_mutex);
}
