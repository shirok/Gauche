/*
 * port.c - port implementation
 *
 *  Copyright(C) 2000-2002 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: port.c,v 1.53 2002-04-25 14:02:34 shirok Exp $
 */

#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#define LIBGAUCHE_BODY
#include "gauche.h"

/*================================================================
 * Common
 */

static void port_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx);
static void port_finalize(GC_PTR obj, GC_PTR data);
static void register_buffered_port(ScmPort *port);
static void unregister_buffered_port(ScmPort *port);
static void bufport_flush(ScmPort*, int);
static int file_closer(ScmPort *p);

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_PortClass, port_print);

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
    if (SCM_PORT_CLOSED_P(port)) return 0;
    switch (SCM_PORT_TYPE(port)) {
    case SCM_PORT_FILE:
        if (SCM_PORT_DIR(port) == SCM_PORT_OUTPUT) bufport_flush(port, 0);
        if (port->src.buf.closer) port->src.buf.closer(port);
        break;
    case SCM_PORT_PROC:
        if (port->src.vt.Close) port->src.vt.Close(port);
        break;
    default:
        break;
    }
    SCM_PORT_CLOSED_P(port) = TRUE;
    return 0;
}

/* called by GC */
static void port_finalize(GC_PTR obj, GC_PTR data)
{
    port_cleanup(SCM_PORT(obj));
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
    port->scrcnt = 0;
    port->ungotten = SCM_CHAR_INVALID;
    port->closed = FALSE;
    port->ownerp = FALSE;
    port->name = SCM_FALSE;
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
    if (SCM_PORT_TYPE(port) == SCM_PORT_FILE
        && SCM_PORT_DIR(port) == SCM_PORT_OUTPUT) {
        unregister_buffered_port(port);
    }
    return result? SCM_FALSE : SCM_TRUE;
}

#define CLOSE_CHECK(port)                                               \
    (SCM_PORT_CLOSED_P(port)                                            \
     && (Scm_Error("I/O attempted on closed port: %S", (port)), 0))

/*===============================================================
 * Getting information
 */
ScmObj Scm_PortName(ScmPort *port)
{
    return port->name;
}

int Scm_PortLine(ScmPort *port)
{
    switch (SCM_PORT_TYPE(port)) {
    case SCM_PORT_FILE:
        return port->src.buf.line;
    default:
        /* TODO: proc port to customize */
        return -1;
    }
}

int Scm_PortPosition(ScmPort *port)
{
    /* TODO: WRITEME */
    return -1;
}

static void port_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    Scm_Printf(port, "#<%s%sport%s %A %p>",
               (SCM_PORT_DIR(obj)&SCM_PORT_INPUT)? "i" : "",
               (SCM_PORT_DIR(obj)&SCM_PORT_OUTPUT)? "o" : "",
               SCM_PORT_CLOSED_P(obj)? "(closed)" : "",
               Scm_PortName(SCM_PORT(obj)),
               obj);
}

/* Returns port's associated file descriptor number, if any.
   Returns -1 otherwise. */
int Scm_PortFileNo(ScmPort *port)
{
    if (SCM_PORT_TYPE(port) == SCM_PORT_FILE) {
        /* TODO: this is ugly, and not extensible.  rewrite. */
        if (port->src.buf.closer == file_closer) {
            return (int)port->src.buf.data;
        } else {
            return -1;
        }
    } else {
        /* TODO: proc port */
        return -1;
    }
}

/* Low-level function to find if the file descriptor is ready or not.
   DIR specifies SCM_PORT_INPUT or SCM_PORT_OUTPUT.
   If the system doesn't have select(), this function returns
   SCM_FD_UNKNOWN. */
int Scm_FdReady(int fd, int dir)
{
#ifdef HAVE_SELECT
    fd_set fds;
    int r;
    struct timeval tm;

    /* In case if this is called on non-file ports.*/
    if (fd < 0) return SCM_FD_READY;

    FD_ZERO(&fds);
    FD_SET(fd, &fds);
    tm.tv_sec = tm.tv_usec = 0;
    if (dir == SCM_PORT_OUTPUT) {
        r = Scm_SysCall(select(fd+1, NULL, &fds, NULL, &tm));
    } else {
        r = Scm_SysCall(select(fd+1, &fds, NULL, NULL, &tm));
    }
    if (r < 0) Scm_SysError("select failed");
    if (r > 0) return SCM_FD_READY;
    else       return SCM_FD_WOULDBLOCK;
#else  /*!HAVE_SELECT*/
    return SCM_FD_UNKNOWN;
#endif /*!HAVE_SELECT*/
}

int Scm_CharReady(ScmPort *p)
{
    if (!SCM_IPORTP(p)) Scm_Error("input port required, but got %S", p);
    switch (SCM_PORT_TYPE(p)) {
    case SCM_PORT_FILE:
        if (p->src.buf.current < p->src.buf.end) return TRUE;
        if (p->src.buf.ready == NULL) return TRUE;
        return (p->src.buf.ready(p) != SCM_FD_WOULDBLOCK);
    case SCM_PORT_PROC:
        return p->src.vt.Ready(p);
    default:
        return TRUE;
    }
}

/*===============================================================
 * buffered Port
 *  - mainly used for buffered file I/O, but can also be used
 *    for other purpose, like character-code conversion port.
 */

/* [Buffered port protocol]
 *
 *  Legends
 *    b = port->src.buf.buffer
 *    c = port->src.buf.current
 *    e = port->src.buf.end
 *    '*' = valid data
 *    '-' = invalid data
 *
 *  Output
 *
 *    When used as output, the end pointer always points one byte past
 *    the buffer.  Initially, the buffer is empty and the current pointer
 *    is the same as the beginning of the buffer.
 *
 *    port->src.buf.flusher(ScmPort* p, int mincnt) is called when the
 *    buffer doesn't have enough space.   When the flusher is called,
 *    the buffer is like this:
 *
 *        <--------------- size ---------------->
 *       |*********************************-----|
 *        ^                                ^     ^
 *        b                                c     e
 *
 *    The flusher is supposed to output the whole valid data between
 *    buffer and current to the underlying device.  The flusher MUST
 *    output at least mincnt bytes of data; however, it may return
 *    before entire data is output, in case like underlying device is
 *    busy.  The flusher returns the number of bytes actually written out.
 *    If an error occurs, the flusher must throw an error.
 *
 *    After flusher returns, bufport_flush shifts the unflushed data
 *    (if any), so the buffer becomes like this:
 *
 *        <--------------- size ---------------->
 *       |****----------------------------------|
 *        ^   ^                                  ^
 *        b   c                                  e
 *
 *  Input
 *
 *    When used as input, the end pointer points to one byte past the
 *    end of the valid data, which may be before the end of the buffer.
 *
 *    port->src.buf.filler(ScmPort *p, int mincnt) is called when the buffer
 *    doesn't have enough data to read.   Suppose the input routine detects
 *    the buffer doesn't have enough data when it looks like this:
 *
 *        <--------------- size ---------------->
 *       |-----------------------------****-----|
 *        ^                            ^   ^
 *        b                            c   e
 *
 *    First, bufport_fill shifts the unread data (if any) to the beginning
 *    of the buffer, so it becomes like this:
 *
 *        <--------------- size ---------------->
 *       |****----------------------------------|
 *        ^   ^ 
 *        bc  e
 *
 *    Then port->src.buf.filler is called.  It is supposed to read as many
 *    bytes as (b + size - e), putting them after the end pointer.
 *    It must read at least mincnt bytes before it returns.   The filler
 *    returns the number of bytes actually read in.
 *    The filler may return 0 if it reaches the end of the data source.
 *
 *    bufport_fill then adjust the end pointer, so the buffer becomes like
 *    this.
 *
 *        <--------------- size ---------------->
 *       |************************************--|
 *        ^                                   ^ 
 *        bc                                  e
 *
 */

#define SCM_PORT_DEFAULT_BUFSIZ 8192

ScmObj Scm_MakeBufferedPort(int dir,     /* direction */
                            int mode,    /* buffering mode */
                            int bufsiz,  /* size of the buffer. */
                            char *buffer,  /* the buffer.  can be NULL
                                              to be autoallocated */
                            int ownerp,  /* owner flag*/
                            int (*filler)(ScmPort *p, int cnt),
                            int (*flusher)(ScmPort *p, int cnt),
                            int (*closer)(ScmPort *p),
                            int (*ready)(ScmPort *p),
                            void *data)
{
    ScmPort *p;
    
    if (bufsiz <= 0) bufsiz = SCM_PORT_DEFAULT_BUFSIZ;
    if (buffer == NULL) buffer = SCM_NEW_ATOMIC2(char*, bufsiz);
    p = make_port(dir, SCM_PORT_FILE, ownerp);
    p->src.buf.buffer = buffer;
    if (dir == SCM_PORT_INPUT) {
        p->src.buf.current = p->src.buf.buffer;
        p->src.buf.end = p->src.buf.buffer;
    } else {
        p->src.buf.current = p->src.buf.buffer;
        p->src.buf.end = p->src.buf.buffer + bufsiz;
    }
    p->src.buf.size = bufsiz;
    p->src.buf.mode = mode;
    p->src.buf.filler = filler;
    p->src.buf.flusher = flusher;
    p->src.buf.closer = closer;
    p->src.buf.ready = ready;
    p->src.buf.data = data;
    p->src.buf.line = 1;
    if (dir == SCM_PORT_OUTPUT) register_buffered_port(p);
    return SCM_OBJ(p);
}

/* flushes the buffer, to make a room of at least mincnt bytes. */
static void bufport_flush(ScmPort *p, int mincnt)
{
    int cursiz = SCM_PORT_BUFFER_AVAIL(p);
    int nwrote;
    if (mincnt <= 0) mincnt = cursiz;
    nwrote = p->src.buf.flusher(p, mincnt);
    if (nwrote >= 0 && nwrote < cursiz) {
        memmove(p->src.buf.buffer, p->src.buf.buffer+nwrote, cursiz-nwrote);
        p->src.buf.current -= nwrote;
    } else {
        p->src.buf.current = p->src.buf.buffer;
    }
}

/* writes siz bytes in src to the buffered port.  siz may be larger than
   the port's buffer. */
static void bufport_write(ScmPort *p, const char *src, int siz)
{
    do {
        int room = (int)(p->src.buf.end - p->src.buf.current);
        if (room >= siz) {
            memcpy(p->src.buf.current, src, siz);
            p->src.buf.current += siz;
            siz = 0;
        } else {
            memcpy(p->src.buf.current, src, room);
            p->src.buf.current += room;
            siz -= room;
            src += room;
            bufport_flush(p, 0);
        }
    } while (siz > 0);
}

/* fills the buffer to make at least mincnt bytes. */
static void bufport_fill(ScmPort *p, int mincnt)
{
    int cursiz = (int)(p->src.buf.end - p->src.buf.current);
    int nread;
    if (cursiz > 0) {
        memmove(p->src.buf.buffer, p->src.buf.current, cursiz);
        p->src.buf.current = p->src.buf.buffer;
        p->src.buf.end = p->src.buf.current + cursiz;
    } else {
        p->src.buf.current = p->src.buf.end = p->src.buf.buffer;
    }
    if (mincnt < 0) {
        mincnt = SCM_PORT_BUFFER_ROOM(p);
    }
    nread = p->src.buf.filler(p, mincnt);
    if (nread > 0) p->src.buf.end += nread; /* safety net */
}

/* reads siz bytes to dst from the buffered port.  siz may be larger
   than the port's buffer. */
static int bufport_read(ScmPort *p, char *dst, int siz)
{
    int nread = 0;
    do {
        int avail = (int)(p->src.buf.end - p->src.buf.current);
        if (avail >= siz) {
            memcpy(dst, p->src.buf.current, siz);
            p->src.buf.current += siz;
            nread += siz;
            siz = 0;
        } else {
            memcpy(dst, p->src.buf.current, avail);
            p->src.buf.current += avail;
            nread += avail;
            siz -= avail;
            dst += avail;
            bufport_fill(p, (siz > p->src.buf.size)? 0 : siz);
            if (p->src.buf.current == p->src.buf.end) break;
        }
    } while (siz > 0);
    return nread;
}

/* Tracking buffered ports:
 *   The system doesn't automatically flush the buffered output port,
 *   as it does on FILE* structure.  So Gauche keeps track of active
 *   output buffered ports, in a weak vector.
 *   When the port is no longer used, it is collected by GC and removed
 *   from the vector.   Scm_FlushAllPorts() flushes the active ports.
 */

/*TODO: allow to extend the port vector. */
/*TODO: if we make at most one port owns one fd, the vector can be directly
  indexed by fd. */

#define PORT_VECTOR_SIZE 256    /* need to be 2^n */

static struct {
    ScmInternalMutex mutex;
    ScmWeakVector   *ports;
} active_buffered_ports;

#define PORT_HASH(port)  \
    ((((SCM_WORD(port)>>3) * 2654435761UL)>>16) % PORT_VECTOR_SIZE)

static void register_buffered_port(ScmPort *port)
{
    int i, h, c;
    h = i = PORT_HASH(port);
    c = 0;
    /* search the available entry by quadratic hash */
    (void)SCM_INTERNAL_MUTEX_LOCK(active_buffered_ports.mutex);
    while (!SCM_FALSEP(Scm_WeakVectorRef(active_buffered_ports.ports, i, SCM_FALSE))) {
        i -= ++c; if (i<0) i+=PORT_VECTOR_SIZE;
        if (i == h) Scm_Panic("active buffered port table overflow");
    }
    Scm_WeakVectorSet(active_buffered_ports.ports, i, SCM_OBJ(port));
    (void)SCM_INTERNAL_MUTEX_UNLOCK(active_buffered_ports.mutex);
}

/* This should be called when the output buffered port is explicitly closed.
   The ports collected by GC are automatically unregistered. */
static void unregister_buffered_port(ScmPort *port)
{
    int i, h, c;
    ScmObj p;
    
    h = i = PORT_HASH(port);
    c = 0;
    (void)SCM_INTERNAL_MUTEX_LOCK(active_buffered_ports.mutex);
    do {
        p = Scm_WeakVectorRef(active_buffered_ports.ports, i, SCM_FALSE);
        if (!SCM_FALSEP(p) && SCM_EQ(SCM_OBJ(port), p)) {
            Scm_WeakVectorSet(active_buffered_ports.ports, i, SCM_FALSE);
            break;
        }
        i -= ++c; if (i<0) i+=PORT_VECTOR_SIZE;
    } while (i != h);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(active_buffered_ports.mutex);
}

/* Flush all ports.  Note that it is possible that this routine can be
   called recursively if one of the flushing routine calls Scm_Exit.
   In order to avoid infinite loop, I have to delete the entries of already
   flushed port before calling flush, then recover them before return
   (unless exitting is true, in that case we know nobody cares the active
   port vector anymore).
   Even if more than one thread calls Scm_FlushAllPorts simultaneously,
   the flush method is called only once, from one of the calling thread.
 */
void Scm_FlushAllPorts(int exitting)
{
    ScmWeakVector *save, *ports;
    ScmObj p = SCM_FALSE;
    int i, saved = 0;

    save = SCM_WEAKVECTOR(Scm_MakeWeakVector(PORT_VECTOR_SIZE));
    ports = active_buffered_ports.ports;
    
    for (i=0; i<PORT_VECTOR_SIZE;) {
        (void)SCM_INTERNAL_MUTEX_LOCK(active_buffered_ports.mutex);
        for (; i<PORT_VECTOR_SIZE; i++) {
            p = Scm_WeakVectorRef(ports, i, SCM_FALSE);
            if (!SCM_FALSEP(p)) {
                Scm_WeakVectorSet(save, i, p);
                Scm_WeakVectorSet(ports, i, SCM_FALSE);
                saved++;
                break;
            }
        }
        (void)SCM_INTERNAL_MUTEX_UNLOCK(active_buffered_ports.mutex);
        if (!SCM_FALSEP(p)) {
            SCM_ASSERT(SCM_PORTP(p) && SCM_PORT_TYPE(p)==SCM_PORT_FILE);
            bufport_flush(SCM_PORT(p), 0);
        }
    }
    if (!exitting && saved) {
        (void)SCM_INTERNAL_MUTEX_LOCK(active_buffered_ports.mutex);
        for (i=0; i<PORT_VECTOR_SIZE; i++) {
            p = Scm_WeakVectorRef(save, i, SCM_FALSE);
            if (!SCM_FALSEP(p)) Scm_WeakVectorSet(ports, i, p);
        }
        (void)SCM_INTERNAL_MUTEX_UNLOCK(active_buffered_ports.mutex);
    }
}

/*===============================================================
 * Generic procedures
 */

void Scm_Putb(ScmByte b, ScmPort *p)
{
    CLOSE_CHECK(p);
    switch (SCM_PORT_TYPE(p)) {
    case SCM_PORT_FILE:
        if (p->src.buf.current >= p->src.buf.end) bufport_flush(p, 1);
        SCM_ASSERT(p->src.buf.current < p->src.buf.end);
        *p->src.buf.current++ = b;
        if (p->src.buf.mode == SCM_PORT_BUFFER_NEVER) bufport_flush(p, 1);
        break;
    case SCM_PORT_OSTR:
        SCM_DSTRING_PUTB(&p->src.ostr, b);
        break;
    case SCM_PORT_PROC:
        p->src.vt.Putb(b, p);
        break;
    default:
        Scm_Error("bad port type for output: %S", p);
    }
}

void Scm_Putc(ScmChar c, ScmPort *p)
{
    int nb;
    
    CLOSE_CHECK(p);
    switch (SCM_PORT_TYPE(p)) {
    case SCM_PORT_FILE:
        nb = SCM_CHAR_NBYTES(c);
        if (p->src.buf.current+nb > p->src.buf.end) bufport_flush(p, nb);
        SCM_ASSERT(p->src.buf.current+nb <= p->src.buf.end);
        SCM_CHAR_PUT(p->src.buf.current, c);
        p->src.buf.current += nb;
        if (p->src.buf.mode == SCM_PORT_BUFFER_LINE) {
            if (c == '\n') bufport_flush(p, nb);
        } else if (p->src.buf.mode == SCM_PORT_BUFFER_NEVER) {
            bufport_flush(p, nb);
        }
        break;
    case SCM_PORT_OSTR:
        SCM_DSTRING_PUTC(&p->src.ostr, c);
        break;
    case SCM_PORT_PROC:
        p->src.vt.Putc(c, p);
        break;
    default:
        Scm_Error("bad port type for output: %S", p);
    }
}

void Scm_Puts(ScmString *s, ScmPort *p)
{
    CLOSE_CHECK(p);
    switch (SCM_PORT_TYPE(p)) {
    case SCM_PORT_FILE:
        bufport_write(p, SCM_STRING_START(s), SCM_STRING_SIZE(s));
        if (p->src.buf.mode == SCM_PORT_BUFFER_LINE) {
            const char *cp = p->src.buf.current;
            while (cp-- > p->src.buf.buffer) {
                if (*cp == '\n') {
                    bufport_flush(p, (int)(cp - p->src.buf.current));
                    break;
                }
            }
        } else if (p->src.buf.mode == SCM_PORT_BUFFER_NEVER) {
            bufport_flush(p, 0);
        }
        break;
    case SCM_PORT_OSTR:
        Scm_DStringAdd(&p->src.ostr, s);
        break;
    case SCM_PORT_PROC:
        p->src.vt.Puts(s, p);
        break;
    default:
        Scm_Error("bad port type for output: %S", p);
    }
}

void Scm_Putz(const char *s, int siz, ScmPort *p)
{
    CLOSE_CHECK(p);
    if (siz < 0) siz = strlen(s);
    switch (SCM_PORT_TYPE(p)) {
    case SCM_PORT_FILE:
        bufport_write(p, s, siz);
        if (p->src.buf.mode == SCM_PORT_BUFFER_LINE) {
            const char *cp = p->src.buf.current;
            while (cp-- > p->src.buf.buffer) {
                if (*cp == '\n') {
                    bufport_flush(p, (int)(cp - p->src.buf.current));
                    break;
                }
            }
        } else if (p->src.buf.mode == SCM_PORT_BUFFER_NEVER) {
            bufport_flush(p, 0);
        }
        break;
        break;
    case SCM_PORT_OSTR:
        Scm_DStringPutz(&p->src.ostr, s, siz);
        break;
    case SCM_PORT_PROC:
        p->src.vt.Putz(s, siz, p);
        break;
    default:
        Scm_Error("bad port type for output: %S", p);
    }
}

void Scm_Flush(ScmPort *p)
{
    CLOSE_CHECK(p);
    switch (SCM_PORT_TYPE(p)) {
    case SCM_PORT_FILE:
        bufport_flush(p, 0);
        break;
    case SCM_PORT_OSTR:
        break;
    case SCM_PORT_PROC:
        p->src.vt.Flush(p);
        break;
    default:
        Scm_Error("bad port type for output: %S", p);
    }
}

void Scm_Ungetc(ScmChar c, ScmPort *port)
{
    SCM_UNGETC(c, port);
}

int Scm_Getb(ScmPort *p)
{
    int b = 0;
    CLOSE_CHECK(p);
    /* TODO: ungotten char */
    switch (SCM_PORT_TYPE(p)) {
    case SCM_PORT_FILE:
        if (p->src.buf.current >= p->src.buf.end) {
            bufport_fill(p, 1);
            if (p->src.buf.current >= p->src.buf.end) return EOF;
        }
        b = (unsigned char)*p->src.buf.current++;
        break;
    case SCM_PORT_ISTR:
        if (p->src.istr.current >= p->src.istr.end) return EOF;
        else b = (unsigned char)*p->src.istr.current++;
        break;
    case SCM_PORT_PROC:
        b = p->src.vt.Getb(p);
        break;
    default:
        Scm_Error("bad port type for output: %S", p);
    }
    return b;
}

int Scm_Getc(ScmPort *p)
{
    int first, nb, c = 0;

    CLOSE_CHECK(p);
    if (p->ungotten != SCM_CHAR_INVALID) {
        c = p->ungotten;
        p->ungotten = SCM_CHAR_INVALID;
        return c;
    }

    if (p->scrcnt > 0) {
        Scm_Error("binary/character mixed I/O is not supported yet, sorry (port %S)", p);
    }

    switch (SCM_PORT_TYPE(p)) {
    case SCM_PORT_FILE:
        if (p->src.buf.current >= p->src.buf.end) {
            bufport_fill(p, 1);
            if (p->src.buf.current >= p->src.buf.end) return EOF;
        }
        first = (unsigned char)*p->src.buf.current++;
        nb = SCM_CHAR_NFOLLOWS(first);
        if (nb > 0) {
            if (p->src.buf.current + nb > p->src.buf.end) {
                /* The buffer doesn't have enough bytes to consist a char.
                   move the incomplete char to the scratch buffer and try
                   to fetch the rest of the char. */
                int rest;
                p->scrcnt = (unsigned char)(p->src.buf.end - p->src.buf.current + 1);
                memcpy(p->scratch, p->src.buf.current-1, p->scrcnt);
                p->src.buf.current = p->src.buf.end;
                rest = nb + 1 - p->scrcnt;
                bufport_fill(p, rest);
                if (p->src.buf.current + rest > p->src.buf.end) {
                    /* TODO: make this behavior customizable */
                    Scm_Error("encountered EOF in middle of a multibyte character from port %S", p);
                }
                memcpy(p->scratch+p->scrcnt, p->src.buf.current, rest);
                SCM_CHAR_GET(p->scratch, c);
                p->scrcnt = 0;
                p->src.buf.current += rest;
            } else {
                SCM_CHAR_GET(p->src.buf.current-1, c);
                p->src.buf.current += nb;
            }
        } else {
            c = first;
            if (c == '\n') p->src.buf.line++;
        }
        return c;
    case SCM_PORT_ISTR:
        if (p->src.istr.current >= p->src.istr.end) return EOF;
        first = (unsigned char)*p->src.istr.current++;
        nb = SCM_CHAR_NFOLLOWS(first);
        if (nb > 0) {
            if (p->src.istr.current + nb > p->src.istr.end) {
                /* TODO: make this behavior customizable */
                Scm_Error("encountered EOF in middle of a multibyte character from port %S", p);
            }
            SCM_CHAR_GET(p->src.istr.current-1, c);
            p->src.istr.current += nb;
        } else {
            c = first;
        }
        break;
    case SCM_PORT_PROC:
        c = p->src.vt.Getc(p);
        break;
    default:
        Scm_Error("bad port type for output: %S", p);
    }
    return c;
}

/*
 * Getz - block read.
 */
int Scm_Getz(char *buf, int buflen, ScmPort *p)
{
    int siz;
    CLOSE_CHECK(p);

    /* TODO: ungotten char */

    switch (SCM_PORT_TYPE(p)) {
    case SCM_PORT_FILE:
        siz = bufport_read(p, buf, buflen);
        if (siz == 0) return EOF;
        else return siz;
    case SCM_PORT_ISTR:
        if (p->src.istr.current + buflen >= p->src.istr.end) {
            if (p->src.istr.current >= p->src.istr.end) return EOF;
            siz = (int)(p->src.istr.end - p->src.istr.current);
            memcpy(buf, p->src.istr.current, siz);
            p->src.istr.current = p->src.istr.end;
            return siz;
        } else {
            memcpy(buf, p->src.istr.current, buflen);
            p->src.istr.current += buflen;
            return buflen;
        }
    case SCM_PORT_PROC:
        return p->src.vt.Getz(buf, buflen, p);
        break;
    default:
        Scm_Error("bad port type for output: %S", p);
    }
    return -1;                  /* dummy */
}

/*
 * ReadLine
 */

/* TODO: this can be optimized by scanning buffer directly, instead of
   using Scm_Getc. */

ScmObj Scm_ReadLine(ScmPort *p)
{
    int c1, c2;
    ScmDString ds;

    Scm_DStringInit(&ds);
    SCM_GETC(c1, p);
    if (c1 == EOF) return SCM_EOF;
    for (;;) {
        if (c1 == EOF || c1 == '\n') break;
        if (c1 == '\r') {
            SCM_GETC(c2, p);
            if (c2 == EOF || c2 == '\n') break;
            SCM_UNGETC(c2, p);
            break;
        }
        SCM_DSTRING_PUTC(&ds, c1);
        SCM_GETC(c1, p);
    }
    return Scm_DStringGet(&ds);
}

/*===============================================================
 * File Port
 */

static int file_filler(ScmPort *p, int mincnt)
{
    int nread = 0, r;
    int room = SCM_PORT_BUFFER_ROOM(p);
    int fd = (int)p->src.buf.data;
    char *datptr = p->src.buf.end;
    SCM_ASSERT(fd >= 0);
    while (nread < mincnt) {
        errno = 0;
        r = read(fd, datptr, room-nread);
        if (r < 0) {
            if (errno == EINTR) {
                Scm_SigCheck(Scm_VM());
                continue;
            } else {
                Scm_SysError("read failed on %S", p);
            }
        } else if (r == 0) {
            /* EOF is read */
            break;
        } else {
            datptr += r;
            nread += r;
        }
    }
    return nread;
}

static int file_flusher(ScmPort *p, int mincnt)
{
    int nwrote = 0, r;
    int datsiz = SCM_PORT_BUFFER_AVAIL(p);
    int fd = (int)p->src.buf.data;
    char *datptr = p->src.buf.buffer;
    
    SCM_ASSERT(fd >= 0);
    while (nwrote < mincnt) {
        errno = 0;
        r = write(fd, datptr, datsiz-nwrote);
        if (r < 0) {
            if (errno == EINTR) {
                Scm_SigCheck(Scm_VM());
                continue;
            } else {
                Scm_SysError("write failed on %S", p);
            }
        } else {
            datptr += r;
            nwrote += r;
        }
    }
    return nwrote;
}

static int file_closer(ScmPort *p)
{
    int fd = (int)p->src.buf.data;
    SCM_ASSERT(fd >= 0);
    if (SCM_PORT_DIR(p) == SCM_PORT_OUTPUT) bufport_flush(p, 0);
    return close(fd);
}

static int file_ready(ScmPort *p)
{
    int fd = (int)p->src.buf.data;
    SCM_ASSERT(fd >= 0);
    return Scm_FdReady(fd, SCM_PORT_DIR(p));
}

ScmObj Scm_OpenFilePort(const char *path, int flags, int mode)
{
    int fd, dir = 0;
    ScmObj p;
    
    if ((flags & O_ACCMODE) == O_RDONLY) dir = SCM_PORT_INPUT;
    else if ((flags & O_ACCMODE) == O_WRONLY) dir = SCM_PORT_OUTPUT;
    else Scm_Error("unsupported file access mode %d to open %s", flags&O_ACCMODE, path);
    fd = open(path, flags, mode);
    if (fd < 0) return SCM_FALSE;
    p = Scm_MakeBufferedPort(dir, SCM_PORT_BUFFER_ALWAYS, 0, NULL, TRUE,
                             file_filler, file_flusher,
                             file_closer, file_ready, (void*)fd);
    SCM_PORT(p)->name = SCM_MAKE_STR_COPYING(path);
    return p;
}

/* Create a port on specified file descriptor.
      NAME  - used for the name of the port.
      DIRECTION - either SCM_PORT_INPUT or SCM_PORT_OUTPUT
      FD - the opened file descriptor.
      BUFFERED - if TRUE, the port will be buffered (using fdopen).
      OWNERP - if TRUE, fd will be closed when this port is closed.
 */
ScmObj Scm_MakePortWithFd(ScmObj name, int direction,
                          int fd, int buffered, int ownerp)
{
    ScmObj p;
    int bmode = buffered? SCM_PORT_BUFFER_ALWAYS : SCM_PORT_BUFFER_NEVER;
    
    p = Scm_MakeBufferedPort(direction, bmode, 0, NULL, ownerp,
                             file_filler, file_flusher,
                             file_closer, file_ready, (void*)fd);
    SCM_PORT(p)->name = name;
    return p;
}

/*===============================================================
 * String port
 */

ScmObj Scm_MakeInputStringPort(ScmString *str)
{
    ScmPort *p = make_port(SCM_PORT_INPUT, SCM_PORT_ISTR, FALSE);
    p->src.istr.current = SCM_STRING_START(str);
    p->src.istr.end = SCM_STRING_START(str) + SCM_STRING_SIZE(str);
    SCM_PORT(p)->name = SCM_MAKE_STR("(input string port)");
    return SCM_OBJ(p);
}

ScmObj Scm_MakeOutputStringPort(void)
{
    ScmPort *p = make_port(SCM_PORT_OUTPUT, SCM_PORT_OSTR, FALSE);
    Scm_DStringInit(&p->src.ostr);
    SCM_PORT(p)->name = SCM_MAKE_STR("(output string port)");
    return SCM_OBJ(p);
}

ScmObj Scm_GetOutputString(ScmPort *port)
{
    if (SCM_PORT_TYPE(port) != SCM_PORT_OSTR)
        Scm_Error("output string port required, but got %S", port);
    return Scm_DStringGet(&SCM_PORT(port)->src.ostr);
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

static int null_getz(char *buf, int buflen, ScmPort *dummy)
    /*ARGSUSED*/
{
    return 0;
}

static ScmObj null_getline(ScmPort *port)
{
    return SCM_EOF;
}

static int null_ready(ScmPort *dummy)
    /*ARGSUSED*/
{
    return TRUE;
}

static int null_putb(ScmByte b, ScmPort *dummy)
    /*ARGSUSED*/
{
    return 0;
}

static int null_putc(ScmChar c, ScmPort *dummy)
    /*ARGSUSED*/
{
    return 0;
}

static int null_putz(const char *str, int len, ScmPort *dummy)
    /*ARGSUSED*/
{
    return 0;
}

static int null_puts(ScmString *s, ScmPort *dummy)
    /*ARGSUSED*/
{
    return 0;
}

static int null_flush(ScmPort *dummy)
    /*ARGSUSED*/
{
    return 0;
}

ScmObj Scm_MakeVirtualPort(int direction, ScmPortVTable *vtable, void *data)
{
    ScmPort *p = make_port(direction, SCM_PORT_PROC, FALSE);
    
    /* Copy vtable, and ensure all entries contain some ptr */
    p->src.vt = *vtable;
    if (!p->src.vt.Getb) p->src.vt.Getb = null_getb;
    if (!p->src.vt.Getc) p->src.vt.Getc = null_getc;
    if (!p->src.vt.Getz) p->src.vt.Getz = null_getz;
    if (!p->src.vt.Getline) p->src.vt.Getline = null_getline;
    if (!p->src.vt.Ready) p->src.vt.Ready = null_ready;
    if (!p->src.vt.Putb) p->src.vt.Putb = null_putb;
    if (!p->src.vt.Putc) p->src.vt.Putc = null_putc;
    if (!p->src.vt.Putz) p->src.vt.Putz = null_putz;
    if (!p->src.vt.Puts) p->src.vt.Puts = null_puts;
    if (!p->src.vt.Flush) p->src.vt.Flush = null_flush;

    p->data = data;
    return SCM_OBJ(p);
}

/*===============================================================
 * with-port
 */
struct with_port_packet {
    ScmPort *origport[3];
    int mask;
    int closep;
};

static ScmObj port_restorer(ScmObj *args, int nargs, void *data)
{
    struct with_port_packet *p = (struct with_port_packet*)data;
    int pcnt = 0;
    ScmPort *curport;

    if (p->mask & SCM_PORT_CURIN) {
        curport = SCM_CURIN;
        SCM_CURIN = p->origport[pcnt++];
        if (p->closep) Scm_ClosePort(curport);
    }
    if (p->mask & SCM_PORT_CUROUT) {
        curport = SCM_CUROUT;
        SCM_CUROUT = p->origport[pcnt++];
        if (p->closep) Scm_ClosePort(curport);
    }
    if (p->mask & SCM_PORT_CURERR) {
        curport = SCM_CURERR;
        SCM_CURERR = p->origport[pcnt++];
        if (p->closep) Scm_ClosePort(curport);
    }
    return SCM_UNDEFINED;
}

ScmObj Scm_WithPort(ScmPort *port[], ScmProcedure *thunk, int mask, int closep)
{
    ScmObj finalizer;
    struct with_port_packet *packet;
    int pcnt = 0;
    
    if (SCM_PROCEDURE_REQUIRED(thunk) != 0) {
        Scm_Error("thunk required: %S", thunk);
    }
    packet = SCM_NEW(struct with_port_packet);
    if (mask & SCM_PORT_CURIN) {
        packet->origport[pcnt] = SCM_CURIN;
        SCM_CURIN = port[pcnt++];
    }
    if (mask & SCM_PORT_CUROUT) {
        packet->origport[pcnt] = SCM_CUROUT;
        SCM_CUROUT = port[pcnt++];
    }
    if (mask & SCM_PORT_CURERR) {
        packet->origport[pcnt] = SCM_CURERR;
        SCM_CURERR = port[pcnt++];
    }
    packet->mask = mask;
    packet->closep = closep;
    finalizer = Scm_MakeSubr(port_restorer, (void*)packet,
                             0, 0, SCM_FALSE);
    return Scm_VMDynamicWind(Scm_NullProc(), SCM_OBJ(thunk), finalizer);
}

/*===============================================================
 * Standard ports
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
    (void)SCM_INTERNAL_MUTEX_INIT(active_buffered_ports.mutex);
    active_buffered_ports.ports = SCM_WEAKVECTOR(Scm_MakeWeakVector(PORT_VECTOR_SIZE));

    scm_stdin  = Scm_MakePortWithFd(SCM_MAKE_STR("(stdin)"),
                                    SCM_PORT_INPUT, 0, TRUE, TRUE);
    scm_stdout = Scm_MakePortWithFd(SCM_MAKE_STR("(stdout)"),
                                    SCM_PORT_OUTPUT, 1, TRUE, TRUE);
    SCM_PORT(scm_stdout)->src.buf.mode = SCM_PORT_BUFFER_LINE;
    scm_stderr = Scm_MakePortWithFd(SCM_MAKE_STR("(stderr)"),
                                    SCM_PORT_OUTPUT, 2, TRUE, TRUE);
    SCM_PORT(scm_stderr)->src.buf.mode = SCM_PORT_BUFFER_NEVER;
}
