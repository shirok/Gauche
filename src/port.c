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
 *  $Id: port.c,v 1.66 2002-06-14 02:34:35 shirok Exp $
 */

#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#define LIBGAUCHE_BODY
#include "gauche.h"

#define MAX(a, b) ((a)>(b)? (a) : (b))
#define MIN(a, b) ((a)<(b)? (a) : (b))

/*================================================================
 * Class stuff
 */

static void port_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx);
static void port_finalize(GC_PTR obj, GC_PTR data);
static void register_buffered_port(ScmPort *port);
static void unregister_buffered_port(ScmPort *port);
static void bufport_flush(ScmPort*, int);
static int file_closer(ScmPort *p);

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_PortClass, port_print);

/*================================================================
 * Common
 */

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
        if (port->ownerp && port->src.buf.closer) port->src.buf.closer(port);
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
    port->ownerp = ownerp;
    port->name = SCM_FALSE;
    switch (type) {
    case SCM_PORT_FILE: /*FALLTHROUGH*/;
    case SCM_PORT_PROC:
        GC_REGISTER_FINALIZER(port, port_finalize, NULL, &ofn, &ocd);
        break;
    default:
        break;
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
        if (port->src.buf.filenum) return port->src.buf.filenum(port);
        else return -1;
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
    if (SCM_PORT_UNGOTTEN(p) != SCM_CHAR_INVALID) return TRUE;
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
 *    the buffer.  Initially the buffer is empty and the current pointer
 *    is the same as the beginning of the buffer.
 *
 *    port->src.buf.flusher(ScmPort* p, int cnt) is called when the port
 *    needs to create some room in the buffer.   When the flusher is called,
 *    the buffer is like this:
 *
 *        <--------------- size ---------------->
 *       |*********************************-----|
 *        ^                                ^     ^
 *        b                                c     e
 *
 *    The flusher is supposed to output as much data as from the beginning
 *    of the buffer up to the cnt bytes, which is usually up to the current
 *    pointer.  The flusher may return before entire data is output, in
 *    case like underlying device is busy.  The flusher must output at least
 *    one byte.  The flusher returns the number of bytes actually written out.
 *    If an error occurs, the flusher must return -1.
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
 *    port->src.buf.filler(ScmPort *p, int cnt) is called when the buffer
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
 *    bytes as cnt, putting them after the end pointer.   It may read
 *    less if all cnt bytes of data is not available immediately.
 *    The filler returns the number of bytes actually read in.
 *    The filler should return 0 if it reaches the end of the data source.
 *    If an error occurs, the filler must return -1.
 *
 *    bufport_fill then adjust the end pointer, so the buffer becomes like
 *    this.
 *
 *        <--------------- size ---------------->
 *       |************************************--|
 *        ^                                   ^ 
 *        bc                                  e
 *
 *  Close
 *    Port is closed either explicitly (via close-port etc) or implicity
 *    (via GC -> finalizer).   In either case, the flusher is called first
 *    if there's any data remaining in the buffer.   Then, if the closer
 *    procedure (port->src.buf.closer) is not NULL, and port->owner is TRUE,
 *    the closer procedure is called which has to take care of any system-
 *    level cleanup.   The closer can assume the buffer is already flushed.
 *
 *  Ready
 *    When char-ready? is called on a buffered port, it first checks if
 *    there's any data available in the buffer.  If so, it returns true.
 *    If not, it calls port->src.buf.ready if it is not NULL to query
 *    the character is ready.   If port->src.buf.ready is NULL, bufport
 *    assumes the input is always ready.
 *
 *  Filenum
 *    Port->src.buf.filenum is a query procedure that should return the
 *    underlying integer file descriptor of the port, or -1 if there's
 *    no associated one.   If it is NULL, the port is assumed not to
 *    be associated to any file descriptor.
 *
 *  Buffering mode
 *    {For Output}
 *      SCM_PORT_BUFFER_FULL : Full buffering.  The buffer is flushed
 *         only when the buffer gets full, explicitly requested, or
 *         closed.   This is the default, and suitable for file I/O.
 *
 *      SCM_PORT_BUFFER_LINE : Line buffering.  The buffer is flushed
 *         when a newline character is put, other than the normal
 *         circumstances as in BUFFER_ALWAYS.   Unlike C stdio, the
 *         buffer isn't flushed when an input is called on the same
 *         terminal device.
 *         This is natural for output of interactive communication.
 *         This is the default of stdout.
 *
 *      SCM_PORT_BUFFER_NONE : data is always passed to the flusher
 *         procedure.  The buffer is used just as a temporary storage.
 *         This slows down port operation significantly.  Should only
 *         be used when you want to guarantee what you write is always
 *         passed to the lower layer.   This is the default of stderr.
 *
 *    {For Input}
 *      SCM_PORT_BUFFER_FULL : Full buffering.  The filler procedure
 *         is called only if the buffer doesn't have enough data to
 *         satisfy the read request.   Read-block or read-string won't
 *         return until the specified bytes/characters are read from
 *         the port, except the port reaches EOF.
 *
 *      SCM_PORT_BUFFER_LINE : For input ports, this is almost the same
 *         as BUFFER_FULL, except that read-block and read-string may
 *         return shorter data than requested, if only that amount of
 *         data is immediately available.   Usually this mode is suitable
 *         for the ports that is attached to a pipe or network.
 *
 *      SCM_PORT_BUFFER_NONE : No buffering.  Every time the data is
 *         requested, the filler procedure is called with exact amount
 *         of the requested data.
 */

#define SCM_PORT_DEFAULT_BUFSIZ 8192

ScmObj Scm_MakeBufferedPort(ScmObj name,
                            int dir,     /* direction */
                            int ownerp,  /* owner flag*/
                            ScmPortBuffer *bufrec)
{
    ScmPort *p;
    int size = bufrec->size;
    char *buf = bufrec->buffer;
    
    if (size <= 0) size = SCM_PORT_DEFAULT_BUFSIZ;
    if (buf == NULL) buf = SCM_NEW_ATOMIC2(char*, size);
    p = make_port(dir, SCM_PORT_FILE, ownerp);
    p->name = name;
    p->src.buf.buffer = buf;
    if (dir == SCM_PORT_INPUT) {
        p->src.buf.current = p->src.buf.buffer;
        p->src.buf.end = p->src.buf.buffer;
    } else {
        p->src.buf.current = p->src.buf.buffer;
        p->src.buf.end = p->src.buf.buffer + size;
    }
    p->src.buf.size = size;
    p->src.buf.mode = bufrec->mode;
    p->src.buf.filler = bufrec->filler;
    p->src.buf.flusher = bufrec->flusher;
    p->src.buf.closer = bufrec->closer;
    p->src.buf.ready = bufrec->ready;
    p->src.buf.filenum = bufrec->filenum;
    p->src.buf.data = bufrec->data;
    p->src.buf.line = 1;
    if (dir == SCM_PORT_OUTPUT) register_buffered_port(p);
    return SCM_OBJ(p);
}

/* flushes the buffer, to make a room of cnt bytes.  cnt == 0 means
   all the available data. */
static void bufport_flush(ScmPort *p, int cnt)
{
    int cursiz = SCM_PORT_BUFFER_AVAIL(p);
    int nwrote;
    if (cursiz == 0) return;
    if (cnt <= 0) cnt = cursiz;
    nwrote = p->src.buf.flusher(p, cnt);
    if (nwrote >= 0 && nwrote < cursiz) {
        memmove(p->src.buf.buffer, p->src.buf.buffer+nwrote, cursiz-nwrote);
        p->src.buf.current -= nwrote;
    } else {
        p->src.buf.current = p->src.buf.buffer;
    }
}

/* Writes siz bytes in src to the buffered port.  siz may be larger than
   the port's buffer.  Won't return until entire siz bytes are written. */
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

/* Fills the buffer.  Reads at least MIN bytes (unless it reaches EOF).
 * If ALLOW_LESS is true, however, we allow to return before the full
 * data is read.
 * Returns the number of bytes actually read, or 0 if EOF, or -1 if error.
 */
static int bufport_fill(ScmPort *p, int min, int allow_less)
{
    int cursiz = (int)(p->src.buf.end - p->src.buf.current);
    int nread = 0, toread;
    if (cursiz > 0) {
        memmove(p->src.buf.buffer, p->src.buf.current, cursiz);
        p->src.buf.current = p->src.buf.buffer;
        p->src.buf.end = p->src.buf.current + cursiz;
    } else {
        p->src.buf.current = p->src.buf.end = p->src.buf.buffer;
    }
    if (min <= 0) min = SCM_PORT_BUFFER_ROOM(p);
    if (p->src.buf.mode == SCM_PORT_BUFFER_FULL) {
        toread = SCM_PORT_BUFFER_ROOM(p);
    } else {
        toread = min;
    }

    do {
        int r = p->src.buf.filler(p, toread-nread);
        if (r <= 0) break;
        nread += r;
        p->src.buf.end += r;
    } while (!allow_less && nread < min);
    return nread;
}

/* Reads siz bytes to dst from the buffered port.  siz may be larger
 * than the port's buffer, in which case the filler procedure is called
 * more than once.  Unless the port buffering mode is BUFFER_FULL,
 * this may read less than SIZ bytes if only that amount of data is
 * immediately available.
 * Caveat: if the filler procedure returns N where 0 < N < requested size,
 * we know less data is available; non-greedy read can return at that point.
 * However, if the filler procedure returns exactly the requested size,
 * and we need more bytes, we gotta be careful -- next call to the filler
 * procedure may or may not hang.  So we need to check the ready procedure.
 */
static int bufport_read(ScmPort *p, char *dst, int siz)
{
    int nread = 0, r, req;
    int avail = (int)(p->src.buf.end - p->src.buf.current);

    req = MIN(siz, avail);
    if (req > 0) {
        memcpy(dst, p->src.buf.current, req);
        p->src.buf.current += req;
        nread += req;
        siz -= req;
        dst += req;
    }
    while (siz > 0) {
        req = MIN(siz, p->src.buf.size);
        r = bufport_fill(p, req, TRUE);
        if (r <= 0) break; /* EOF or an error*/
        if (r >= siz) {
            memcpy(dst, p->src.buf.current, siz);
            p->src.buf.current += siz;
            nread += siz;
            break;
        } else {
            memcpy(dst, p->src.buf.current, r);
            p->src.buf.current += r;
            nread += r;
            siz -= r;
            dst += r;
        }
        if (p->src.buf.mode != SCM_PORT_BUFFER_FULL) {
            if (r < req) break;
            if (p->src.buf.ready
                && p->src.buf.ready(p) == SCM_FD_WOULDBLOCK) {
                break;
            }
        }
    }
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

/* Utility procedure to translate Scheme arg into buffering mode */
static ScmObj key_full, key_modest, key_line, key_none;

int Scm_BufferingMode(ScmObj flag, int direction, int fallback)
{
    if (SCM_EQ(flag, key_full)) return SCM_PORT_BUFFER_FULL;
    if (SCM_EQ(flag, key_none)) return SCM_PORT_BUFFER_NONE;
    if (fallback >= 0 && (SCM_UNBOUNDP(flag) || SCM_FALSEP(flag)))
        return fallback;
    if (direction == SCM_PORT_INPUT) {
        if (SCM_EQ(flag, key_modest)) return SCM_PORT_BUFFER_LINE;
        else Scm_Error("buffering mode must be one of :full, :modest or :none, but got %S", flag);
    }
    if (direction == SCM_PORT_OUTPUT) {
        if (SCM_EQ(flag, key_line)) return SCM_PORT_BUFFER_LINE;
        else Scm_Error("buffering mode must be one of :full, :line or :none, but got %S", flag);
    }
    /* if direction is none of input or output, allow both. */
    if (SCM_EQ(flag, key_line) || SCM_EQ(flag, key_modest)) {
        return SCM_PORT_BUFFER_LINE;
    }
    else Scm_Error("buffering mode must be one of :full, :modest, :line or :none, but got %S", flag);
    return -1;                  /* dummy */
}

ScmObj Scm_GetBufferingMode(ScmPort *port)
{
    if (SCM_PORT_TYPE(port) == SCM_PORT_FILE) {
        switch (port->src.buf.mode) {
        case SCM_PORT_BUFFER_FULL: return key_full;
        case SCM_PORT_BUFFER_NONE: return key_none;
        default:
            if (SCM_IPORTP(port)) return key_modest;
            else return key_line;
        }
    }
    return SCM_FALSE;
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
        if (p->src.buf.mode == SCM_PORT_BUFFER_NONE) bufport_flush(p, 1);
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
        } else if (p->src.buf.mode == SCM_PORT_BUFFER_NONE) {
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
        } else if (p->src.buf.mode == SCM_PORT_BUFFER_NONE) {
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
        } else if (p->src.buf.mode == SCM_PORT_BUFFER_NONE) {
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

/*---------------------------------------------------
 * Getb
 */

/* shift scratch buffer content */
#define SHIFT_SCRATCH(p, off) \
   do { int i_; for (i_=0; i_ < (p)->scrcnt; i_++) (p)->scratch[i_]=(p)->scratch[i_+(off)]; } while (0)

/* handle the case that there's remaining data in the scratch buffer */
static int getb_scratch(ScmPort *p)
{
    int b = (unsigned char)p->scratch[0];
    p->scrcnt--;
    SHIFT_SCRATCH(p, 1);
    return b;
}

/* handle the case that there's an ungotten char */
static int getb_ungotten(ScmPort *p)
{
    SCM_CHAR_PUT(p->scratch, p->ungotten);
    p->scrcnt = SCM_CHAR_NBYTES(p->ungotten);
    p->ungotten = SCM_CHAR_INVALID;
    return getb_scratch(p);
}

/* getb body */
int Scm_Getb(ScmPort *p)
{
    int b = 0;
    CLOSE_CHECK(p);

    /* check if there's "pushded back" stuff */
    if (p->scrcnt) return getb_scratch(p);
    if (p->ungotten != SCM_CHAR_INVALID) return getb_ungotten(p);
    
    /* TODO: ungotten char */
    switch (SCM_PORT_TYPE(p)) {
    case SCM_PORT_FILE:
        if (p->src.buf.current >= p->src.buf.end) {
            if (bufport_fill(p, 1, FALSE) == 0) return EOF;
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

/*--------------------------------------------------- 
 * Getc 
 */

/* handle the case that there's data in scratch area */
static int getc_scratch(ScmPort *p)
{
    char tbuf[SCM_CHAR_MAX_BYTES];
    int nb = SCM_CHAR_NFOLLOWS(p->scratch[0]), ch, i, curr = p->scrcnt;
    memcpy(tbuf, p->scratch, curr);
    p->scrcnt = 0;
    for (i=curr; i<=nb; i++) {
        int r = Scm_Getb(p);
        if (r == EOF) {
            Scm_Error("encountered EOF in middle of a multibyte character from port %S", p);
        }
        tbuf[i] = (char)r;
    }
    SCM_CHAR_GET(tbuf, ch);
    return ch;
}

/* getc main body */
int Scm_Getc(ScmPort *p)
{
    int first, nb, c = 0;

    CLOSE_CHECK(p);
    if (p->scrcnt > 0) return getc_scratch(p);
    if (p->ungotten != SCM_CHAR_INVALID) {
        c = p->ungotten;
        p->ungotten = SCM_CHAR_INVALID;
        return c;
    }

    switch (SCM_PORT_TYPE(p)) {
    case SCM_PORT_FILE:
        if (p->src.buf.current >= p->src.buf.end) {
            if (bufport_fill(p, 1, FALSE) == 0) return EOF;
        }
        first = (unsigned char)*p->src.buf.current++;
        nb = SCM_CHAR_NFOLLOWS(first);
        if (nb > 0) {
            if (p->src.buf.current + nb > p->src.buf.end) {
                /* The buffer doesn't have enough bytes to consist a char.
                   move the incomplete char to the scratch buffer and try
                   to fetch the rest of the char. */
                int rest, filled = 0; 
                p->scrcnt = (unsigned char)(p->src.buf.end - p->src.buf.current + 1);
                memcpy(p->scratch, p->src.buf.current-1, p->scrcnt);
                p->src.buf.current = p->src.buf.end;
                rest = nb + 1 - p->scrcnt;
                for (;;) {
                    filled = bufport_fill(p, rest, FALSE);
                    if (filled <= 0) {
                        /* TODO: make this behavior customizable */
                        Scm_Error("encountered EOF in middle of a multibyte character from port %S", p);
                    }
                    if (filled >= rest) {
                        memcpy(p->scratch+p->scrcnt, p->src.buf.current, rest);
                        p->scrcnt += rest;
                        p->src.buf.current += rest;
                        break;
                    } else {
                        memcpy(p->scratch+p->scrcnt, p->src.buf.current, filled);
                        p->scrcnt += filled;
                        p->src.buf.current = p->src.buf.end;
                        rest -= filled;
                    }
                }
                SCM_CHAR_GET(p->scratch, c);
                p->scrcnt = 0;
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

/*--------------------------------------------------- 
 * Getz - block read.
 *   If the buffering mode is BUFFER_FULL, this reads BUFLEN bytes
 *   unless it reaches EOF.  Otherwise, this reads less than BUFLEN
 *   if the data is not immediately available.
 */
static int getz_scratch(char *buf, int buflen, ScmPort *p)
{
    int i;
    if (p->scrcnt >= buflen) {
        memcpy(buf, p->scratch, buflen);
        p->scrcnt -= buflen;
        SHIFT_SCRATCH(p, buflen);
        return buflen;
    } else {
        memcpy(buf, p->scratch, p->scrcnt);
        i = p->scrcnt;
        p->scrcnt = 0;
        return i + Scm_Getz(buf+i, buflen-i, p);
    }
}

static int getz_ungotten(char *buf, int buflen, ScmPort *p)
{
    p->scrcnt = SCM_CHAR_NBYTES(p->ungotten);
    SCM_CHAR_PUT(p->scratch, p->ungotten);
    p->ungotten = SCM_CHAR_INVALID;
    return getz_scratch(buf, buflen, p);
}

int Scm_Getz(char *buf, int buflen, ScmPort *p)
{
    int siz;
    CLOSE_CHECK(p);

    if (p->scrcnt) return getz_scratch(buf, buflen, p);
    if (p->ungotten != SCM_CHAR_INVALID) return getz_ungotten(buf, buflen, p);

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

/*--------------------------------------------------- 
 * ReadLine
 *   Reads up to EOL or EOF.  
 */

#if 0
/* NB: An attempt to implement "optimized" version of readline.
   This avoids unnecessary DString creation and mb->char->mb conversion,
   and potentially boost the performance of readline.
   However, it complicates the code considerably, and I'm wondering if
   it's worth to do.  I stop implmenenting it now, until I really hit
   the performance bottleneck of read-line. [SK] */

/* Common routine to scan buffer to find EOL between cur to end.  If EOL
   is found, ScmString is created from the line and returned (If dsused
   is true, the string is added to ds and the entire string is returned).

   If EOL is not found until end, but eof is TRUE, the end of buffer
   is regarded as the end of line.  For istr port, eof is always TRUE.

   If EOL is not found until end and eof is FALSE, the entire buffer is
   added to ds, and SCM_FALSE is returned to indicate that the buffer is
   filled again.  The special exception is when the last byte of the
   buffer is '\r'---in that case we should see if the next byte is '\n'
   or not---and scanbuf returns SCM_TRUE to indicate that.
*/
static ScmObj readline_scanbuf(const char *cur, const char *end,
                               const char **next, ScmDString *ds,
                               int dsused, int eof)
{
    int eol = eof;
    const char *cp;
    *next = end;
    for (cp = cur; cp < end; cp++) {
        if (*cp == '\n') { *next = cp+1; eol = TRUE; break; }
        if (*cp == '\r') {
            if (cp < end-1 && cp[1] == '\n') {
                *next = cp+2; eol = TRUE; break;
            } else if (cp == end-1 && !eof) {
                Scm_DStringPutz(ds, cur, (int)(cp-cur));
                return SCM_TRUE;
            } else {
                *next = cp+1; eol = TRUE; break;
            }
        }
    }
    if (dsused) {
        Scm_DStringPutz(ds, cur, (int)(cp-cur));
        if (eol) return Scm_DStringGet(ds);
        else return SCM_FALSE;
    } else {
        if (eol) {
            return Scm_MakeString(cur, (int)(cp-cur), -1, SCM_MAKSTR_COPYING);
        } else {
            Scm_DStringPutz(ds, cur, (int)(cp-cur));
            return SCM_FALSE;
        }
    }
}

static ScmObj readline_scratch(ScmPort *p, ScmDString *ds,
                               int *dsused, int *crread)
{
    int i;
    ScmObj r;
    for (i=0; i<p->scrcnt; i++) {
        if (p->scratch[i] == '\n') {
            r = Scm_MakeString(p->scratch, i-1, -1, SCM_MAKSTR_COPYING);
            SHIFT_SCRATCH(p, i);
            return r;
        } else if (p->scratch[i] == '\r') {
            if (i < p->scrcnt-1) {
                r = Scm_MakeString(p->scratch, i-1, -1, SCM_MAKSTR_COPYING);
                if (p->scratch[i+1] == '\n') SHIFT_SCRATCH(p, i+1);
                else                         SHIFT_SCRATCH(p, i);
                return r;
            } else {
                Scm_DStringPutz(ds, p->scratch, i-1);
                p->scrcnt = 0;
                *crread = TRUE;
                return SCM_FALSE;
            }
        }
    }
    *dsused = TRUE; Scm_DStringPutz(ds, p->scratch, p->scrcnt);
    return SCM_FALSE;
}

ScmObj Scm_ReadLine(ScmPort *p)
{
    int dsused = FALSE, crread = FALSE;
    int c1;
    const char *cp1;
    ScmDString ds;
    ScmObj r;

    if (!SCM_IPORTP(p)) Scm_Error("input port required, but got %S", p);
    Scm_DStringInit(&ds);

    /* check ungotten stuff */
    if (p->scrcnt) {
        r = readline_scratch(p, &ds, &dsused, &crread);
        if (SCM_STRINGP(r)) return r;
    }
    if (p->ungotten != SCM_CHAR_INVALID) {
        c1 = p->ungotten; p->ungotten = SCM_CHAR_INVALID;
        if (c1 == '\n') return Scm_DStringGet(&ds);
        dsused = TRUE; Scm_DStringPutc(&ds, c1);
    }
    
    switch (SCM_PORT_TYPE(p)) {
    case SCM_PORT_FILE:
        if (p->src.buf.current == p->src.buf.end) {
            if (bufport_fill(p, 1, FALSE) == 0) goto eofdeal;
        }
        for (;;) {
            r = readline_scanbuf(p->src.buf.current, p->src.buf.end,
                                 &cp1, &ds, dsused, FALSE);
            if (SCM_STRINGP(r)) {
                p->src.buf.current = (char*)cp1;
                break;
            } else {
                dsused = TRUE;
                if (bufport_fill(p, 1, FALSE) == 0) goto eofdeal;
                if (SCM_TRUEP(r) && *p->src.buf.current == '\n') {
                    p->src.buf.current++;
                    goto eofdeal;
                }
            }
        }
        return r;
    case SCM_PORT_ISTR:
        if (p->src.istr.current == p->src.istr.end) goto eofdeal;
        r = readline_scanbuf(p->src.istr.current, p->src.istr.end,
                             &cp1, &ds, dsused, TRUE);
        p->src.istr.current = cp1;
        return r;
    case SCM_PORT_PROC:
        r = p->src.vt.Getline(p);
        if (dsused) {
            if (!SCM_STRINGP(r)) goto eofdeal;
            Scm_DStringAdd(&ds, SCM_STRING(r));
            return Scm_DStringGet(&ds);
        } else {
            return r;
        }
    default:
        Scm_Error("bad port type for output: %S", p);
        /*NOTREACHED*/
    }
  eofdeal:
    if (dsused) return Scm_DStringGet(&ds);
    else return SCM_EOF;
}
#else
/* This is much simpler, non-optimized version. */
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
#endif

/*===============================================================
 * File Port
 */

static int file_filler(ScmPort *p, int cnt)
{
    int nread = 0, r;
    int fd = (int)p->src.buf.data;
    char *datptr = p->src.buf.end;
    SCM_ASSERT(fd >= 0);
    do {
        errno = 0;
        r = read(fd, datptr, cnt-nread);
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
    } while (0);
    return nread;
}

static int file_flusher(ScmPort *p, int cnt)
{
    int nwrote = 0, r;
    int datsiz = SCM_PORT_BUFFER_AVAIL(p);
    int fd = (int)p->src.buf.data;
    char *datptr = p->src.buf.buffer;
    
    SCM_ASSERT(fd >= 0);
    while (nwrote == 0) {
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
    return close(fd);
}

static int file_ready(ScmPort *p)
{
    int fd = (int)p->src.buf.data;
    SCM_ASSERT(fd >= 0);
    return Scm_FdReady(fd, SCM_PORT_DIR(p));
}

static int file_filenum(ScmPort *p)
{
    return (int)p->src.buf.data;
}

ScmObj Scm_OpenFilePort(const char *path, int flags, int buffering, int perm)
{
    int fd, dir = 0;
    ScmObj p;
    ScmPortBuffer bufrec;
    
    if ((flags & O_ACCMODE) == O_RDONLY) dir = SCM_PORT_INPUT;
    else if ((flags & O_ACCMODE) == O_WRONLY) dir = SCM_PORT_OUTPUT;
    else Scm_Error("unsupported file access mode %d to open %s", flags&O_ACCMODE, path);
    if (buffering < SCM_PORT_BUFFER_FULL || buffering > SCM_PORT_BUFFER_NONE)
        Scm_Error("bad buffering flag: %d", buffering);
    fd = open(path, flags, perm);
    if (fd < 0) return SCM_FALSE;
    bufrec.mode = buffering;
    bufrec.buffer = NULL;
    bufrec.size = 0;
    bufrec.filler = file_filler;
    bufrec.flusher = file_flusher;
    bufrec.closer = file_closer;
    bufrec.ready = file_ready;
    bufrec.filenum = file_filenum;
    bufrec.data = (void*)fd;
    p = Scm_MakeBufferedPort(SCM_MAKE_STR_COPYING(path), dir, TRUE, &bufrec);
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
                          int fd, int bufmode, int ownerp)
{
    ScmObj p;
    ScmPortBuffer bufrec;
    
    bufrec.buffer = NULL;
    bufrec.size = 0;
    bufrec.mode = bufmode;
    bufrec.filler = file_filler;
    bufrec.flusher =file_flusher;
    bufrec.closer = file_closer;
    bufrec.ready = file_ready;
    bufrec.filenum = file_filenum;
    bufrec.data = (void*)fd;
    
    p = Scm_MakeBufferedPort(name, direction, ownerp, &bufrec);
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

ScmObj Scm_MakeVirtualPort(int direction, ScmPortVTable *vtable)
{
    ScmPort *p = make_port(direction, SCM_PORT_PROC, TRUE);
    
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
                                    SCM_PORT_INPUT, 0,
                                    SCM_PORT_BUFFER_FULL, TRUE);
    scm_stdout = Scm_MakePortWithFd(SCM_MAKE_STR("(stdout)"),
                                    SCM_PORT_OUTPUT, 1,
                                    SCM_PORT_BUFFER_LINE, TRUE);
    scm_stderr = Scm_MakePortWithFd(SCM_MAKE_STR("(stderr)"),
                                    SCM_PORT_OUTPUT, 2,
                                    SCM_PORT_BUFFER_NONE, TRUE);
    key_full   = Scm_MakeKeyword(SCM_STRING(SCM_MAKE_STR("full")));
    key_modest = Scm_MakeKeyword(SCM_STRING(SCM_MAKE_STR("modest")));
    key_line   = Scm_MakeKeyword(SCM_STRING(SCM_MAKE_STR("line")));
    key_none   = Scm_MakeKeyword(SCM_STRING(SCM_MAKE_STR("none")));
}
