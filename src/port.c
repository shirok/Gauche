/*
 * port.c - port implementation
 *
 *  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, distribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: port.c,v 1.13 2001-03-12 06:15:31 shiro Exp $
 */

#include <errno.h>
#include "gauche.h"

/*================================================================
 * Common
 */

static int port_print(ScmObj obj, ScmPort *port, int mode);
static void port_finalize(GC_PTR obj, GC_PTR data);

SCM_DEFCLASS(Scm_PortClass, "<port>", port_print, SCM_CLASS_DEFAULT_CPL);

/* Cleaning up:
 *   The underlying file descriptor/stream may be closed when the port
 *   is explicitly closed by close-port, or implicitly destroyed by
 *   garbage collector.  To keep consistency, Scheme ports should never
 *   share the same file descriptor.  However, C code and Scheme port
 *   may share the same file descriptor for efficiency (e.g. stdios).
 *   In such cases, it is C code's responsibility to destroy the port.
 */

static int port_cleanup(ScmPort *port)
{
    if (port->ownerp) {
        switch (SCM_PORT_TYPE(port)) {
        case SCM_PORT_FILE:
            return fclose(port->src.file.fp);
        case SCM_PORT_PROC:
            if (port->src.proc.vtable->Close) {
                return port->src.proc.vtable->Close(SCM_PORT(port));
            }
        }
    }
    return 0;
}

/* called by GC */
static void port_finalize(GC_PTR obj, GC_PTR data)
{
    port_cleanup((ScmPort *)obj);
}

/*
 * Internal Constructor.
 *   If this port owns the underlying file descriptor/stream, 
 *   ownerp must be TRUE.
 */
static ScmPort *make_port(int dir, int type, int ownerp)
{
    ScmPort *port;
    GC_finalization_proc ofn; GC_PTR ocd;

    port = SCM_NEW(ScmPort);
    SCM_SET_CLASS(port, SCM_CLASS_PORT);
    port->direction = dir;
    port->type = type;
    port->bufcnt = 0;
    port->ungotten = SCM_CHAR_INVALID;
    port->ownerp = ownerp;
    if (ownerp) {
        GC_REGISTER_FINALIZER(port,
                              port_finalize,
                              NULL,
                              &ofn, &ocd);
    }
    return port;
}

/*
 * Close
 */
ScmObj Scm_ClosePort(ScmPort *port)
{
    int result = port_cleanup(port);
    port->type = SCM_PORT_CLOSED;
    return result? SCM_FALSE : SCM_TRUE;
}

/*===============================================================
 * Getting information
 */
ScmObj Scm_PortName(ScmPort *port)
{
    ScmObj z;
    
    switch (SCM_PORT_TYPE(port)) {
    case SCM_PORT_FILE:
        if (SCM_STRINGP(port->src.file.name)) z = port->src.file.name;
        else z = SCM_MAKE_STR("(unknown file)");
        break;
    case SCM_PORT_ISTR:
        z = SCM_MAKE_STR("(input string)");
        break;
    case SCM_PORT_OSTR:
        z = SCM_MAKE_STR("(output string)");
        break;
    case SCM_PORT_CLOSED:
        z = SCM_MAKE_STR("(closed port)");
        break;
    case SCM_PORT_PROC:
        {
            ScmProcPortInfo *info = port->src.proc.vtable->Info(port);
            z = info ? info->name : SCM_MAKE_STR("(proc port)");
        }
        break;
    default:
        Scm_Panic("Scm_PortName: something screwed up");
    }
    return z;
}

int Scm_PortLine(ScmPort *port)
{
    int l;
    
    switch (SCM_PORT_TYPE(port)) {
    case SCM_PORT_FILE:
        l = port->src.file.line;
        break;
    case SCM_PORT_ISTR:;
    case SCM_PORT_OSTR:;
    case SCM_PORT_CLOSED:
        l = -1;
        break;
    case SCM_PORT_PROC:
        {
            ScmProcPortInfo *info = port->src.proc.vtable->Info(port);
            l = info ? info->line : -1;
        }
        break;
    default:
        Scm_Panic("Scm_PortLine: something screwed up");
    }
    return l;
}

int Scm_PortPosition(ScmPort *port)
{
    int pos;
    
    switch (SCM_PORT_TYPE(port)) {
    case SCM_PORT_FILE:
        pos = port->src.file.column;
        break;
    case SCM_PORT_ISTR:;
        pos = port->src.istr.current - port->src.istr.start;
    case SCM_PORT_OSTR:
        pos = SCM_DSTRING_SIZE(&port->src.ostr);
        break;
    case SCM_PORT_CLOSED:;
        pos = -1;
        break;
    case SCM_PORT_PROC:
        {
            ScmProcPortInfo *info = port->src.proc.vtable->Info(port);
            pos = info ? info->position : -1;
        }
        break;
    default:
        Scm_Panic("Scm_PortLine: something screwed up");
    }
    return pos;
}

static int port_print(ScmObj obj, ScmPort *port, int mode)
{
    return Scm_Printf(port, "#<%s%sport %A %p>",
                      (SCM_PORT_DIR(obj)&SCM_PORT_INPUT)? "i" : "",
                      (SCM_PORT_DIR(obj)&SCM_PORT_OUTPUT)? "o" : "",
                      Scm_PortName(SCM_PORT(obj)),
                      obj);
}

/* Returns port's associated file descriptor number, if any.
   Returns -1 otherwise. */
int Scm_PortFileNo(ScmPort *port)
{
    if (SCM_PORT_TYPE(port) == SCM_PORT_FILE) {
        return fileno(port->src.file.fp);
    } else if (SCM_PORT_TYPE(port) == SCM_PORT_PROC) {
        ScmProcPortInfo *info = port->src.proc.vtable->Info(port);
        if (info) {
            if (info->fd >= 0) return info->fd;
            if (info->fp) return fileno(info->fp);
        }
        return -1;
    } else {
        return -1;
    }
}

/*===============================================================
 * File Port
 */

ScmObj Scm_MakeFilePort(FILE *fp, ScmObj name, const char *mode, int ownerp)
{
    int dir;
    ScmPort *p;
    if (*mode == 'r') dir = SCM_PORT_INPUT;
    else              dir = SCM_PORT_OUTPUT;
    p = make_port(dir, SCM_PORT_FILE, ownerp);
    p->src.file.fp = fp;
    p->src.file.line = 1;
    p->src.file.column = 1;
    p->src.file.name = name;
    return SCM_OBJ(p);
}

ScmObj Scm_OpenFilePort(const char *path, const char *mode)
{
    int dir;
    FILE *fp;
    
    fp = fopen(path, mode);
    if (fp == NULL) return SCM_FALSE;
    return Scm_MakeFilePort(fp, Scm_MakeString(path, -1, -1), mode, TRUE);
}

/* Auxiliary function for macros */

/* Called from SCM_FILE_GETC, when it finds the char is multibyte. 
   Assuming ungotten and incomplete buffer is empty.
   If EOF is found in the middle of the character, keep the read bytes
   in the incomplete buffer and return EOF. */

int Scm__PortFileGetc(int prefetch, ScmPort *port)
{
    unsigned char b;
    char *p;
    int next, nfollows, i, ch;

    port->bufcnt = 0;
    port->buf[0] = prefetch;
    nfollows = SCM_CHAR_NFOLLOWS(prefetch);
    for (i=1; i<= nfollows; i++) {
        next = getc(port->src.file.fp);
        if (next == EOF) return EOF;
        if (next == '\n') port->src.file.line++;
        port->buf[i] = next;
        port->bufcnt++;
    }
    p = port->buf;
    SCM_STR_GETC(p, ch);
    port->bufcnt = 0;
    return ch;
}

/* Called from SCM_GETB, when there's an ungotten char or buffered
   incomplete char. */
int Scm__PortGetbInternal(ScmPort *port)
{
    int ch;
    
    if ((ch = port->ungotten) != SCM_CHAR_INVALID) {
        char *p = port->buf;
        port->bufcnt = SCM_CHAR_NBYTES(ch);
        SCM_STR_PUTC(p, ch);
        port->ungotten = SCM_CHAR_INVALID;
    }
    if (!port->bufcnt) {
        /* This shouldn't happen, but just in case ... */
        SCM_GETB(ch, port);
    } else {
        int i;
        ch = port->buf[0];
        for (i=1; i<port->bufcnt; i++) {
            port->buf[i-1] = port->buf[i];
        }
        port->bufcnt--;
    }
    return ch;
}

/* Called from SCM_GETC, when there's a buffered incomplete char. */
int Scm__PortGetcInternal(ScmPort *port)
{
    int ch, i, nfollows;
    char *p;
    
    if (!port->bufcnt) {
        /* this shouldn't happen, but just in case ... */
        SCM_GETC(ch, port);
    } else {
        /* fill the buffer */
        nfollows = SCM_CHAR_NFOLLOWS(port->buf[0]);
        for (; port->bufcnt <= nfollows; port->bufcnt++) {
            int b;
            switch (SCM_PORT_TYPE(port)) {
              case SCM_PORT_FILE: SCM__FILE_GETB(b, port); break;
              case SCM_PORT_ISTR: SCM__ISTR_GETB(b, port); break;
              case SCM_PORT_PROC: SCM__PROC_GETB(b, port); break;
              default: Scm_Panic("getc: something screwed up");
            }
            if (b == EOF) return EOF;
            port->buf[port->bufcnt] = b;
        }
        p = port->buf;
        SCM_STR_GETC(p, ch);
    }
    return ch;
}

/*===============================================================
 * String port
 */

ScmObj Scm_MakeInputStringPort(ScmString *str)
{
    ScmPort *z = make_port(SCM_PORT_INPUT, SCM_PORT_ISTR, FALSE);
    z->src.istr.start = SCM_STRING_START(str);
    z->src.istr.rest  = SCM_STRING_SIZE(str);
    z->src.istr.current = z->src.istr.start;
    return SCM_OBJ(z);
}

ScmObj Scm_MakeOutputStringPort(void)
{
    ScmPort *z = make_port(SCM_PORT_OUTPUT, SCM_PORT_OSTR, FALSE);
    Scm_DStringInit(&z->src.ostr);
    return SCM_OBJ(z);
}

ScmObj Scm_GetOutputString(ScmPort *port)
{
    SCM_ASSERT(SCM_PORT_TYPE(port) == SCM_PORT_OSTR);
    return Scm_DStringGet(&SCM_PORT(port)->src.ostr);
}

/*===============================================================
 * Generic procedures
 */

void Scm_Putb(ScmByte b, ScmPort *port)
{
    SCM_PUTB(b, port);
}

void Scm_Putc(ScmChar c, ScmPort *port)
{
    SCM_PUTC(c, port);
}

void Scm_Puts(ScmString *s, ScmPort *port)
{
    SCM_PUTS(s, port);
}

void Scm_PutCStr(const char *s, ScmPort *port)
{
    SCM_PUTCSTR(s, port);
}

void Scm_Putnl(ScmPort *port)
{
    SCM_PUTNL(port);
}

void Scm_Flush(ScmPort *port)
{
    SCM_FLUSH(port);
}

void Scm_Ungetc(ScmChar ch, ScmPort *port)
{
    SCM_UNGETC(ch, port);
}

int Scm_Getb(ScmPort *port)
{
    int b;
    SCM_GETB(b, port);
    return b;
}

int Scm_Getc(ScmPort *port)
{
    int c;
    SCM_GETC(c, port);
    return c;
}

/*
 * ReadLine
 */

static inline ScmObj readline_int(ScmPort *port)
{
    ScmDString ds;
    int ch;
    SCM_GETC(ch, port);
    if (ch == SCM_CHAR_INVALID) return SCM_EOF;
    Scm_DStringInit(&ds);
    for (;;) {
        if (ch == '\n' || ch == SCM_CHAR_INVALID)
            return Scm_DStringGet(&ds);
        SCM_DSTRING_PUTC(&ds, ch);
        SCM_GETC(ch, port);
    }
}

ScmObj Scm_ReadLine(ScmPort *port)
{
    if (SCM_PORT_DIR(port) != SCM_PORT_INPUT)
        Scm_Error("input port required: %S\n", port);
    if (SCM_PORT_TYPE(port) == SCM_PORT_PROC) {
        /* procedure port may have optimized method */
        return port->src.proc.vtable->Getline(port);
    } else {
        return readline_int(port);
    }
}

/*===============================================================
 * Procedural port
 */

/* default dummy procedures */
static int null_getb(ScmPort *dummy)
    /*ARGSUSED*/
{
    return SCM_CHAR_INVALID;
}

static int null_getc(ScmPort *dummy)
    /*ARGSUSED*/
{
    return SCM_CHAR_INVALID;
}

static ScmObj null_getline(ScmPort *port)
{
    return readline_int(port);
}

static int null_ready(ScmPort *dummy)
    /*ARGSUSED*/
{
    return TRUE;
}

static int null_putb(ScmPort *dummy, ScmByte b)
    /*ARGSUSED*/
{
    return 0;
}

static int null_putc(ScmPort *dummy, ScmChar c)
    /*ARGSUSED*/
{
    return 0;
}

static int null_puts(ScmPort *dummy, const char *buf, int size, int len)
    /*ARGSUSED*/
{
    return 0;
}

static int null_flush(ScmPort *dummy)
    /*ARGSUSED*/
{
    return 0;
}

static int null_close(ScmPort *dummy)
    /*ARGSUSED*/
{
    return 0;
}

static ScmProcPortInfo *null_info(ScmPort *dummy)
    /*ARGSUSED*/
{
    return NULL;
}

ScmObj Scm_MakeVirtualPort(int direction, ScmPortVTable *vtable, void *data,
                           int ownerp)
{
    ScmPortVTable *vt = SCM_NEW_ATOMIC(ScmPortVTable);
    ScmPort *port = make_port(direction, SCM_PORT_PROC, ownerp);
    
    /* Copy vtable, and ensure all entries contain some ptr */
    *vt = *vtable;
    if (!vt->Getb) vt->Getb = null_getb;
    if (!vt->Getc) vt->Getc = null_getc;
    if (!vt->Getline) vt->Getline = null_getline;
    if (!vt->Ready) vt->Ready = null_ready;
    if (!vt->Putb) vt->Putb = null_putb;
    if (!vt->Putc) vt->Putc = null_putc;
    if (!vt->Puts) vt->Puts = null_puts;
    if (!vt->Flush) vt->Flush = null_flush;
    if (!vt->Close) vt->Close = null_close;
    if (!vt->Info) vt->Info = null_info;

    port->src.proc.vtable = vt;
    port->src.proc.clientData = data;
    return SCM_OBJ(port);
}

/*===============================================================
 * Port over file descriptor
 */

/* A port can be constructed over given file descriptor.  It may be
   either buffered or unbuffered. */
/* TODO: need to fix how to handle the error condition */

struct fdport {
    ScmProcPortInfo info;       /* includes file descriptor */
    char *buf;                  /* if !NULL, points the buffer */
    char *cur;                  /* if buf!=NULL, points the current char */
    int bufsiz;                 /* if buf!=NULL, alocated buffer size */
    int cursiz;                 /* if buf!=NULL, size of valid data in buf */

    char eofread;               /* TRUE if EOF is read. */
    char err;                   /* TRUE if error has occurred. */
};

#define DECL_FDPORT(pdata, port) \
    struct fdport *pdata = (struct fdport *)port->src.proc.clientData
#define CHECK_EOF(pdata, eofcode) \
    if (pdata->eofread) return EOF
#define CHECK_RESULT(result, pdata, eofcode)                            \
    do {                                                                \
        if (result < 0)  { pdata->err = errno; return eofcode; }        \
        if (result == 0) { pdata->eofread = TRUE; return eofcode; }     \
    } while (0)
#define CHECK_ERROR(result, pdata)                              \
    do {                                                        \
        if (result < 0)  { pdata->err = errno; return -1; }     \
    } while (0)

/* Unbuffered I/O */
static int fdport_getb_unbuffered(ScmPort *port)
{
    char c;
    int nread;
    DECL_FDPORT(pdata, port);
    CHECK_EOF(pdata, EOF);
    nread = read(pdata->info.fd, &c, 1);
    CHECK_RESULT(nread, pdata, EOF);
    return c;
}

static int fdport_getc_unbuffered(ScmPort *port)
{
    char chbuf[SCM_CHAR_MAX_BYTES], c;
    int chcnt, chmax, ch, nread;
    DECL_FDPORT(pdata, port);
    CHECK_EOF(pdata, SCM_CHAR_INVALID);

    nread = read(pdata->info.fd, &c, 1);
    CHECK_RESULT(nread, pdata, SCM_CHAR_INVALID);
    if ((chmax = SCM_CHAR_NFOLLOWS(c)) == 0) {
        return c;
    }
    chmax++;
    chbuf[0] = c;
    for (chcnt = 1; chcnt < chmax; chcnt++) {
        nread = read(pdata->info.fd, &chbuf[chcnt], 1);
        CHECK_RESULT(nread, pdata, SCM_CHAR_INVALID);
    }
    SCM_STR_GETC(chbuf, ch);
    return ch;
}

static int fdport_ready_unbuffered(ScmPort *port)
{
    /* TODO: write me */
    return TRUE;
}

static int fdport_putb_unbuffered(ScmPort *port, ScmByte b)
{
    DECL_FDPORT(pdata, port);
    int nwrote;
    do {
        nwrote = write(pdata->info.fd, &b, 1);
        CHECK_ERROR(nwrote, pdata);
    } while (nwrote == 0);
    return 1;
}

static int fdport_putc_unbuffered(ScmPort *port, ScmChar ch)
{
    DECL_FDPORT(pdata, port);
    int nwrote, nbytes, count;
    char chbuf[SCM_CHAR_MAX_BYTES], *bufp;
    count = nbytes = SCM_CHAR_NBYTES(ch);
    SCM_STR_PUTC(chbuf, ch);
    bufp = chbuf;
    do {
        nwrote = write(pdata->info.fd, bufp, count);
        CHECK_ERROR(nwrote, pdata);
        count -= nwrote;
        bufp += nwrote;
    } while (count > 0);
    return nbytes;
}

static int fdport_puts_unbuffered(ScmPort *port, const char *buf, int size,
                                  int len)
{
    DECL_FDPORT(pdata, port);
    int nwrote, count = size;
    do {
        nwrote = write(pdata->info.fd, buf, count);
        CHECK_ERROR(nwrote, pdata);
        count -= nwrote;
        buf += nwrote;
    } while (count > 0);
    return size;
}

static int fdport_close_unbuffered(ScmPort *port)
{
    DECL_FDPORT(pdata, port);
    if (port->ownerp && pdata->info.fd >= 0)
        return close(pdata->info.fd);
}

/* Buffered I/O */
static inline int fdport_fill_buffer(struct fdport *pdata)
{
    int nread = read(pdata->info.fd, pdata->buf, pdata->bufsiz);
    if (nread < 0) {
        pdata->err = errno;
        return -1;
    }
    pdata->cur = pdata->buf;
    pdata->cursiz = nread;
    return nread;
}

static ScmProcPortInfo *fdport_info(ScmPort *port)
{
    DECL_FDPORT(pdata, port);
    return &pdata->info;
}

/* Create a port on specified file descriptor.
      NAME  - used for the name of the port.
      DIRECTION - either SCM_PORT_INPUT or SCM_PORT_OUTPUT
      FD - the opened file descriptor.
      BUFSIZ - if >0, the port will be buffered, and BUFSIZ becomes
            the buffer size.
      OWNERP - if TRUE, fd will be closed when this port is closed.
 */
ScmObj Scm_MakePortWithFd(ScmObj name, int direction,
                          int fd, int bufsiz, int ownerp)
{
    ScmPortVTable vt;
    struct fdport *pdata;
    
    pdata = SCM_NEW(struct fdport);
    pdata->info.name = name;
    pdata->info.line = 1;
    pdata->info.position = 1;
    pdata->info.fd = fd;
    pdata->info.fp = NULL;
    pdata->eofread = pdata->err = FALSE;
    pdata->cursiz = 0;

    if (bufsiz > 0) {
        pdata->buf = SCM_NEW_ATOMIC2(char *, bufsiz);
        pdata->bufsiz = bufsiz;
        pdata->cur = pdata->buf;
        Scm_Error("buffered file port is not supported yet, sorry.");
    } else {
        pdata->buf = NULL;
        pdata->bufsiz = 0;
        pdata->cur = NULL;
        vt.Getb = fdport_getb_unbuffered;
        vt.Getc = fdport_getc_unbuffered;
        vt.Getline = NULL;      /* use default */
        vt.Ready = fdport_ready_unbuffered;
        vt.Putb = fdport_putb_unbuffered;
        vt.Putc = fdport_putc_unbuffered;
        vt.Puts = fdport_puts_unbuffered;
        vt.Flush = NULL;        /* use default */
        vt.Close = fdport_close_unbuffered;
        vt.Info = fdport_info;
    }
    return Scm_MakeVirtualPort(direction, &vt, pdata, ownerp);
}

/*===============================================================
 * Predefined ports and initialization
 */

static ScmObj scm_stdin;
static ScmObj scm_stdout;
static ScmObj scm_stderr;

ScmObj Scm_Stdin(void)
{
    return scm_stdin;
}

ScmObj Scm_Stdout(void)
{
    return scm_stdout;
}

ScmObj Scm_Stderr(void)
{
    return scm_stderr;
}

void Scm__InitPort(void)
{
    scm_stdin  = Scm_MakeFilePort(stdin, SCM_MAKE_STR("(stdin)"), "r", FALSE);
    scm_stdout = Scm_MakeFilePort(stdout,SCM_MAKE_STR("(stdout)"), "w",FALSE);
    scm_stderr = Scm_MakeFilePort(stderr,SCM_MAKE_STR("(stderr)"), "w",FALSE);
}
