/*
 * port.c - port implementation
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

#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/class.h"
#include "gauche/priv/portP.h"

#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <ctype.h>

#undef MAX
#undef MIN
#define MAX(a, b) ((a)>(b)? (a) : (b))
#define MIN(a, b) ((a)<(b)? (a) : (b))

#define SCM_PORT_BUFFER_MODE(obj) \
    (SCM_PORT(obj)->src.buf.mode & SCM_PORT_BUFFER_MODE_MASK)
#define SCM_PORT_BUFFER_SIGPIPE_SENSITIVE_P(obj) \
    (SCM_PORT(obj)->src.buf.mode & SCM_PORT_BUFFER_SIGPIPE_SENSITIVE)

/*================================================================
 * Class stuff
 */

static void port_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx);
static void port_finalize(ScmObj obj, void* data);
static void register_buffered_port(ScmPort *port);
static void unregister_buffered_port(ScmPort *port);
static void bufport_flush(ScmPort*, int, int);
static void file_closer(ScmPort *p);

static ScmObj get_port_name(ScmPort *port)
{
    return Scm_PortName(port);
}

static ScmObj get_port_current_line(ScmPort *port)
{
    return SCM_MAKE_INT(Scm_PortLine(port));
}

static ScmObj get_port_buffering(ScmPort *port)
{
    return Scm_GetPortBufferingModeAsKeyword(port);
}

static void set_port_buffering(ScmPort *port, ScmObj val)
{
    if (SCM_PORT_TYPE(port) != SCM_PORT_FILE) {
        Scm_Error("can't set buffering mode to non-buffered port: %S", port);
    }
    Scm_SetPortBufferingMode(port,Scm_BufferingMode(val,port->direction,-1));
}

static ScmObj get_port_sigpipe_sensitive(ScmPort *port)
{
    return SCM_MAKE_BOOL(Scm_GetPortBufferSigpipeSensitive(port));
}

static void set_port_sigpipe_sensitive(ScmPort *port, ScmObj val)
{
    Scm_SetPortBufferSigpipeSensitive(port, SCM_BOOL_VALUE(val));
}

static ScmClassStaticSlotSpec port_slots[] = {
    SCM_CLASS_SLOT_SPEC("name", get_port_name, NULL),
    SCM_CLASS_SLOT_SPEC("buffering", get_port_buffering,
                        set_port_buffering),
    SCM_CLASS_SLOT_SPEC("sigpipe-sensitive?", get_port_sigpipe_sensitive,
                        set_port_sigpipe_sensitive),
    SCM_CLASS_SLOT_SPEC("current-line", get_port_current_line, NULL),
    SCM_CLASS_SLOT_SPEC_END()
};

SCM_DEFINE_BASE_CLASS(Scm_PortClass,
                      ScmPort, /* instance type */
                      port_print, NULL, NULL, NULL, NULL);

static ScmClass *port_cpl[] = {
    SCM_CLASS_STATIC_PTR(Scm_PortClass),
    SCM_CLASS_STATIC_PTR(Scm_TopClass),
    NULL
};

SCM_DEFINE_BASE_CLASS(Scm_CodingAwarePortClass,
                      ScmPort, /* instance type */
                      port_print, NULL, NULL, NULL, port_cpl);

SCM_DEFINE_BASE_CLASS(Scm_LimitedLengthPortClass,
                      ScmPort, /* instance type */
                      port_print, NULL, NULL, NULL, port_cpl);

/*================================================================
 * Common
 */

/* Cleaning up:
 *   The underlying file descriptor/stream may be closed when the port
 *   is explicitly closed by close-port, or implicitly destroyed by the
 *   garbage collector.  To keep consistency, Scheme ports should never
 *   share the same file descriptor.  However, C code and Scheme port
 *   may share the same file descriptor for efficiency (e.g. stdios).
 *   In such cases, it is C code's responsibility to destroy the port.
 */
static void port_cleanup(ScmPort *port)
{
    if (SCM_PORT_CLOSED_P(port)) return;
    switch (SCM_PORT_TYPE(port)) {
    case SCM_PORT_FILE:
        if (SCM_PORT_DIR(port) == SCM_PORT_OUTPUT) {
            if (!SCM_PORT_ERROR_OCCURRED_P(port)) {
                bufport_flush(port, 0, TRUE);
            }
            unregister_buffered_port(port);
        }
        if (port->ownerp && port->src.buf.closer) port->src.buf.closer(port);
        break;
    case SCM_PORT_PROC:
        if (port->src.vt.Close) port->src.vt.Close(port);
        break;
    default:
        break;
    }
    (void)SCM_INTERNAL_FASTLOCK_DESTROY(port->lock);

    SCM_PORT_CLOSED_P(port) = TRUE;
    /* avoid unnecessary finalization */
    Scm_UnregisterFinalizer(SCM_OBJ(port));
}

/* called by GC */
static void port_finalize(ScmObj obj, void* data)
{
    port_cleanup(SCM_PORT(obj));
}

/*
 * Internal Constructor.
 *   If this port owns the underlying file descriptor/stream,
 *   ownerp must be TRUE.
 */
static ScmPort *make_port(ScmClass *klass, int dir, int type)
{
    ScmPort *port = SCM_ALLOCATE(ScmPort, klass);
    SCM_SET_CLASS(port, klass);
    port->direction = dir;
    port->type = type;
    port->scrcnt = 0;
    port->ungotten = SCM_CHAR_INVALID;
    port->closed = FALSE;
    port->error = FALSE;
    port->ownerp = FALSE;
    port->flags =
        SCM_VM_RUNTIME_FLAG_IS_SET(Scm_VM(), SCM_CASE_FOLD)
        ? SCM_PORT_CASE_FOLD
        : 0;
    port->name = SCM_FALSE;
    (void)SCM_INTERNAL_FASTLOCK_INIT(port->lock);
    port->lockOwner = NULL;
    port->lockCount = 0;
    port->recursiveContext = SCM_FALSE;
    port->attrs = SCM_NIL;
    port->line = 1;

    Scm_RegisterFinalizer(SCM_OBJ(port), port_finalize, NULL);

    return port;
}

/*
 * Close
 */
void Scm_ClosePort(ScmPort *port)
{
    ScmVM *vm = Scm_VM();
    PORT_LOCK(port, vm);
    PORT_SAFE_CALL(port,
                   do {
                       if (!SCM_PORT_CLOSED_P(port)) {
                           port_cleanup(port);
                       }
                   } while (0), /*no cleanup*/);
    PORT_UNLOCK(port);
}

/*===============================================================
 * Locking ports
 */

/* OBSOLETED */
/* C routines can use PORT_SAFE_CALL, so we reimplemented this in libio.scm.
   Kept here for ABI compatibility; will be gone by 1.0.  */
ScmObj Scm_VMWithPortLocking(ScmPort *port, ScmObj closure)
{
    static ScmObj with_port_locking_proc = SCM_UNDEFINED;
    SCM_BIND_PROC(with_port_locking_proc, "with-port-locking",
                  Scm_GaucheModule());
    return Scm_ApplyRec1(with_port_locking_proc, closure);
}

/*===============================================================
 * Getting information
 * NB: Port attribute access API is in portapi.c
 */

ScmObj Scm_PortName(ScmPort *port)
{
    return port->name;
}

int Scm_PortLine(ScmPort *port)
{
    return port->line;
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

/* Duplicates the file descriptor of the source port, and set it to
   the destination port.  Both source and destination port must be
   file ports. */
void Scm_PortFdDup(ScmPort *dst, ScmPort *src)
{
    int r;

    if (SCM_PORT_TYPE(dst) != SCM_PORT_FILE)
        Scm_Error("file port required, but got %S", dst);
    if (SCM_PORT_TYPE(src) != SCM_PORT_FILE)
        Scm_Error("file port required, but got %S", src);
    if (src->direction != dst->direction)
        Scm_Error("port direction mismatch: got %S and %S",
                  src, dst);

    int srcfd = (int)(intptr_t)src->src.buf.data;
    int dstfd = (int)(intptr_t)dst->src.buf.data;

    if (dst->direction == SCM_PORT_INPUT) {
        /* discard the current buffer */
        ScmVM *vm = Scm_VM();
        PORT_LOCK(dst, vm);
        dst->src.buf.current = dst->src.buf.buffer;
        dst->src.buf.end = dst->src.buf.buffer;
        PORT_UNLOCK(dst);
    } else {
        /* flush the current buffer */
        Scm_Flush(dst);
    }
#if defined(GAUCHE_WINDOWS)
    SCM_SYSCALL(r, _dup2(srcfd, dstfd));
#else  /*!GAUCHE_WINDOWS*/
    SCM_SYSCALL(r, dup2(srcfd, dstfd));
#endif /*!GAUCHE_WINDOWS*/
    if (r < 0) Scm_SysError("dup2 failed");
    dst->src.buf.data = (void*)(intptr_t)r;
}

/* Low-level function to find if the file descriptor is ready or not.
   DIR specifies SCM_PORT_INPUT or SCM_PORT_OUTPUT.
   If the system doesn't have select(), this function returns
   SCM_FD_UNKNOWN. */
int Scm_FdReady(int fd, int dir)
{
#if defined(HAVE_SELECT) && !defined(GAUCHE_WINDOWS)
    fd_set fds;
    int r;
    struct timeval tm;

    /* In case if this is called on non-file ports.*/
    if (fd < 0) return SCM_FD_READY;
    if (fd >= FD_SETSIZE) Scm_Error("Scm_FdReady: fd out of range: %d", fd);

    FD_ZERO(&fds);
    FD_SET(fd, &fds);
    tm.tv_sec = tm.tv_usec = 0;
    if (dir == SCM_PORT_OUTPUT) {
        SCM_SYSCALL(r, select(fd+1, NULL, &fds, NULL, &tm));
    } else {
        SCM_SYSCALL(r, select(fd+1, &fds, NULL, NULL, &tm));
    }
    if (r < 0) Scm_SysError("select failed");
    if (r > 0) return SCM_FD_READY;
    else       return SCM_FD_WOULDBLOCK;
#elif  defined(GAUCHE_WINDOWS)
    /* Windows have select(), but it can only be used on sockets.*/
    if (dir == SCM_PORT_OUTPUT) {
        /* We assume it is always ok */
        return SCM_FD_READY;
    } else {
        HANDLE h = (HANDLE)_get_osfhandle(fd);
        DWORD avail;
        if (h == INVALID_HANDLE_VALUE) return SCM_FD_READY;
        if (PeekNamedPipe(h, NULL, 0, NULL, &avail, NULL) == 0) {
            /* We assume the port isn't an end of a pipe. */
            return SCM_FD_UNKNOWN;
        }
        if (avail == 0) return SCM_FD_WOULDBLOCK;
        else return SCM_FD_READY;
    }
#else  /*!HAVE_SELECT && !GAUCHE_WINDOWS */
    return SCM_FD_UNKNOWN;
#endif /*!HAVE_SELECT && !GAUCHE_WINDOWS */
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
 *    port->src.buf.flusher(ScmPort* p, int cnt, int forcep) is called when
 *    the port needs to create some room in the buffer.   When the flusher
 *    is called, the buffer is like this:
 *
 *        <--------------- size ---------------->
 *       |*********************************-----|
 *        ^                                ^     ^
 *        b                                c     e
 *
 *    The flusher is supposed to output the cnt bytes of data beginning from
 *    the buffer, which is usually up to the current pointer (but the flusher
 *    doesn't need to check the current pointer; it is taken care of by the
 *    caller of the flusher).
 *
 *    If the third argument forcep is false, the flusher may return before
 *    entire data is output, in case like underlying device is busy.
 *    The flusher must output at least one byte even in that case.
 *    On the other hand, if the forcep argument is true, the flusher must
 *    write cnt bytes; if it is not possible, the flusher must return -1 to
 *    indicate an error(*1).
 *
 *    The flusher returns the number of bytes actually written out.
 *    If an error occurs, the flusher must return -1.
 *
 *    The flusher must be aware that the port p is locked by the current
 *    thread when called.
 *
 *    The flusher shouldn't change the buffer's internal state.
 *
 *    After the flusher returns, bufport_flush shifts the unflushed data
 *    (if any), so the buffer becomes like this:
 *
 *        <--------------- size ---------------->
 *       |****----------------------------------|
 *        ^   ^                                  ^
 *        b   c                                  e
 *
 *    (*1) Why should these two modes need to be distinguished?  Suppose
 *    you implement a buffered port that does character encoding conversion.
 *    The flusher converts the content of the buffer to different character
 *    encoding and feed it to some specified port.  It is often the case
 *    that you find a few bytes at the end of the buffer which you can't
 *    convert into a whole character but have to wait for next byte(s).
 *    It is valid that you leave them in the buffer if you can expect
 *    more data to come.  However, if you know it is really the end of
 *    the stream, you can't leave any data in the buffer and you should
 *    take appropriate action, for example, raising an error.
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
 *    bytes as cnt, putting them after the end pointer.   The filler doesn't
 *    need to modify the end pointer; it is taken care of after the filler
 *    returns.
 *
 *    The filler may read less than cnt bytes if all bytes of data is not
 *    available immediately.   The filler returns the number of bytes
 *    actually read in.  The filler should return 0 if it reaches the end
 *    of the data source.  If an error occurs, the filler must return -1.
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
 *    port->src.buf.ready should return either SCM_FD_READY, SCM_FD_WOULDBLOCK
 *    or SCM_FD_UNKNOWN.
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
 *         circumstances as in SCM_PORT_BUFFER_FULL.   Unlike C stdio,
 *         the buffer isn't flushed when an input is called on the same
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

ScmObj Scm_MakeBufferedPort(ScmClass *klass,
                            ScmObj name,
                            int dir,     /* direction */
                            int ownerp,  /* owner flag*/
                            ScmPortBuffer *bufrec)
{
    int size = bufrec->size;
    char *buf = bufrec->buffer;

    if (size <= 0) size = SCM_PORT_DEFAULT_BUFSIZ;
    if (buf == NULL) buf = SCM_NEW_ATOMIC2(char*, size);
    ScmPort *p = make_port(klass, dir, SCM_PORT_FILE);
    p->name = name;
    p->ownerp = ownerp;
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
    p->src.buf.seeker = bufrec->seeker;
    p->src.buf.data = bufrec->data;
    if (dir == SCM_PORT_OUTPUT) register_buffered_port(p);
    return SCM_OBJ(p);
}

/* some accessor APIs */
int Scm_GetPortBufferingMode(ScmPort *port)
{
    return SCM_PORT_BUFFER_MODE(port);
}

void Scm_SetPortBufferingMode(ScmPort *port, int mode)
{
    port->src.buf.mode =
        (port->src.buf.mode & ~SCM_PORT_BUFFER_MODE_MASK)
        | (mode & SCM_PORT_BUFFER_MODE_MASK);
}

int Scm_GetPortBufferSigpipeSensitive(ScmPort *port)
{
    return (SCM_PORT_BUFFER_SIGPIPE_SENSITIVE_P(port) != FALSE);
}

void Scm_SetPortBufferSigpipeSensitive(ScmPort *port, int sensitive)
{
    if (sensitive) {
        port->src.buf.mode |=  SCM_PORT_BUFFER_SIGPIPE_SENSITIVE;
    } else {
        port->src.buf.mode &= ~SCM_PORT_BUFFER_SIGPIPE_SENSITIVE;
    }
}

/* Port case folding mode is usually set at port creation, according
   to the VM's case folding mode.   In rare occasion we need to switch
   it (but it's not generally recommended). */
int Scm_GetPortCaseFolding(ScmPort *port)
{
    return (SCM_PORT_CASE_FOLDING(port) != FALSE);
}

void Scm_SetPortCaseFolding(ScmPort *port, int folding)
{
    if (folding) {
        SCM_PORT_FLAGS(port) |=  SCM_PORT_CASE_FOLD;
    } else {
        SCM_PORT_FLAGS(port) &= ~SCM_PORT_CASE_FOLD;
    }
}

/* flushes the buffer, to make a room of cnt bytes.
   cnt == 0 means all the available data.   Note that, unless forcep == TRUE,
   this function only does "best effort" to make room, but doesn't
   guarantee to output cnt bytes.  */
static void bufport_flush(ScmPort *p, int cnt, int forcep)
{
    int cursiz = SCM_PORT_BUFFER_AVAIL(p);

    if (cursiz == 0) return;
    if (cnt <= 0)  { cnt = cursiz; }
    int nwrote = p->src.buf.flusher(p, cnt, forcep);
    if (nwrote < 0) {
        p->src.buf.current = p->src.buf.buffer; /* for safety */
        p->error = TRUE;
        /* TODO: can we raise an error here, or should we propagate
           it to the caller? */
        Scm_PortError(p, SCM_PORT_ERROR_OUTPUT,
                      "Couldn't flush port %S due to an error", p);
    }
    if (nwrote >= 0 && nwrote < cursiz) {
        memmove(p->src.buf.buffer, p->src.buf.buffer+nwrote,
                cursiz-nwrote);
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
            bufport_flush(p, 0, FALSE);
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
    if (SCM_PORT_BUFFER_MODE(p) != SCM_PORT_BUFFER_NONE) {
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
 * procedure may or may not block.  So we need to check the ready procedure.
 */
static int bufport_read(ScmPort *p, char *dst, int siz)
{
    int nread = 0;
    int avail = (int)(p->src.buf.end - p->src.buf.current);

    int req = MIN(siz, avail);
    if (req > 0) {
        memcpy(dst, p->src.buf.current, req);
        p->src.buf.current += req;
        nread += req;
        siz -= req;
        dst += req;
    }
    while (siz > 0) {
        /* We check data availability first, since we might already get
           some data from the remanings in the buffer, and it is enough
           if buffering mode is not full. */
        if (nread && (SCM_PORT_BUFFER_MODE(p) != SCM_PORT_BUFFER_FULL)) {
            if (p->src.buf.ready
                && p->src.buf.ready(p) == SCM_FD_WOULDBLOCK) {
                break;
            }
        }

        int req = MIN(siz, p->src.buf.size);
        int r = bufport_fill(p, req, TRUE);
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
    }
    return nread;
}

/* Tracking buffered ports:
 *
 *   The OS doesn't automatically flush the buffered output port,
 *   as it does on FILE* structure.  So Gauche keeps track of active
 *   output buffered ports, in a weak vector.
 *   When the port is no longer used, it is collected by GC and removed
 *   from the vector.   Scm_FlushAllPorts() flushes the active ports.
 *
 *   Note that we don't remove entry from the weak vector explicitly.
 *   We used to do that in the port finalizer; however, the finalizer
 *   is called _after_ GC has run and determined the port is a garbage,
 *   and at that moment GC has already cleared the vector entry.  So we
 *   can rather let GC remove the entries.
 *
 *   When we find the weak vector is full, we trigger a global GC once.
 *   It may collect garbaged ports and make some room in the vector,
 *   even though the ports are not finalized (GC_gcollect doesn't call
 *   finalizers; they are called at the next checkpoint in VM).
 */

/*TODO: allow to extend the port vector. */

#define PORT_VECTOR_SIZE 256    /* need to be 2^n */

static struct {
    int dummy;
    ScmWeakVector   *ports;
    ScmInternalMutex mutex;
} active_buffered_ports = { 1, NULL }; /* magic to put this in .data area */

#define PORT_HASH(port)  \
    ((((SCM_WORD(port)>>3) * 2654435761UL)>>16) % PORT_VECTOR_SIZE)

static void register_buffered_port(ScmPort *port)
{
    int i, h, c;
    int tried_gc = FALSE;
    int need_gc  = FALSE;

  retry:
    h = i = (int)PORT_HASH(port);
    c = 0;
    /* search an available entry by quadratic hash
       used entry may have #<port> or #t.  #t is for transient state
       during Scm_FlushAllPorts()---see below. */
    (void)SCM_INTERNAL_MUTEX_LOCK(active_buffered_ports.mutex);
    while (!SCM_FALSEP(Scm_WeakVectorRef(active_buffered_ports.ports,
                                         i, SCM_FALSE))) {
        i -= ++c; while (i<0) i+=PORT_VECTOR_SIZE;
        if (i == h) {
            /* Vector entry is full.  We run global GC to try to collect
               unused entry. */
            need_gc = TRUE;
            break;
        }
    }
    if (!need_gc) {
        Scm_WeakVectorSet(active_buffered_ports.ports, i, SCM_OBJ(port));
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(active_buffered_ports.mutex);

    if (need_gc) {
        if (tried_gc) {
            /* We should probably try to extend the weak vector.
               But for the time being... */
            Scm_Panic("active buffered port table overflow");
        } else {
            GC_gcollect();
            tried_gc = TRUE;
            need_gc = FALSE;
            goto retry;
        }
    }
}

/* This should be called when the output buffered port is explicitly closed.
   The ports collected by GC are automatically unregistered. */
static void unregister_buffered_port(ScmPort *port)
{
    int h = (int)PORT_HASH(port);
    int i = h;
    int c = 0;
    (void)SCM_INTERNAL_MUTEX_LOCK(active_buffered_ports.mutex);
    do {
        ScmObj p = Scm_WeakVectorRef(active_buffered_ports.ports, i, SCM_FALSE);
        if (!SCM_FALSEP(p) && SCM_EQ(SCM_OBJ(port), p)) {
            Scm_WeakVectorSet(active_buffered_ports.ports, i, SCM_FALSE);
            break;
        }
        i -= ++c; while (i<0) i+=PORT_VECTOR_SIZE;
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
   the flush method is called only once for each vector.
 */
void Scm_FlushAllPorts(int exitting)
{
    ScmObj p = SCM_FALSE;
    int saved = 0;

    ScmVector *save = SCM_VECTOR(Scm_MakeVector(PORT_VECTOR_SIZE, SCM_FALSE));
    ScmWeakVector *ports = active_buffered_ports.ports;

    for (int i=0; i<PORT_VECTOR_SIZE;) {
        (void)SCM_INTERNAL_MUTEX_LOCK(active_buffered_ports.mutex);
        for (; i<PORT_VECTOR_SIZE; i++) {
            p = Scm_WeakVectorRef(ports, i, SCM_FALSE);
            if (SCM_PORTP(p)) {
                Scm_VectorSet(save, i, p);
                /* Set #t so that the slot won't be reused. */
                Scm_WeakVectorSet(ports, i, SCM_TRUE);
                saved++;
                break;
            }
        }
        (void)SCM_INTERNAL_MUTEX_UNLOCK(active_buffered_ports.mutex);
        if (SCM_PORTP(p)) {
            SCM_ASSERT(SCM_PORT_TYPE(p)==SCM_PORT_FILE);
            if (!SCM_PORT_ERROR_OCCURRED_P(SCM_PORT(p))) {
                bufport_flush(SCM_PORT(p), 0, TRUE);
            }
        }
    }
    if (!exitting && saved) {
        (void)SCM_INTERNAL_MUTEX_LOCK(active_buffered_ports.mutex);
        for (int i=0; i<PORT_VECTOR_SIZE; i++) {
            p = Scm_VectorRef(save, i, SCM_FALSE);
            if (SCM_PORTP(p)) Scm_WeakVectorSet(ports, i, p);
        }
        (void)SCM_INTERNAL_MUTEX_UNLOCK(active_buffered_ports.mutex);
    }
}

/* Utility procedure to translate Scheme arg into buffering mode */
static ScmObj key_full   = SCM_UNBOUND;
static ScmObj key_modest = SCM_UNBOUND;
static ScmObj key_line   = SCM_UNBOUND;
static ScmObj key_none   = SCM_UNBOUND;

int Scm_KeywordToBufferingMode(ScmObj flag, int direction, int fallback)
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

ScmObj Scm_GetPortBufferingModeAsKeyword(ScmPort *port)
{
    if (SCM_PORT_TYPE(port) == SCM_PORT_FILE) {
        switch (SCM_PORT_BUFFER_MODE(port)) {
        case SCM_PORT_BUFFER_FULL: return key_full;
        case SCM_PORT_BUFFER_NONE: return key_none;
        default:
            if (SCM_IPORTP(port)) return key_modest;
            else return key_line;
        }
    }
    return SCM_FALSE;
}

/* For the backward compatibility until release 1.0 */
int Scm_BufferingMode(ScmObj flag, int direction, int fallback)
{
    return Scm_KeywordToBufferingMode(flag, direction, fallback);
}

ScmObj Scm_GetBufferingMode(ScmPort *port)
{
    return Scm_GetPortBufferingModeAsKeyword(port);
}

/*===============================================================
 * Generic procedures
 */

#define SAFE_PORT_OP
#include "portapi.c"
#undef SAFE_PORT_OP
#include "portapi.c"

/*===============================================================
 * File Port
 */

static int file_filler(ScmPort *p, int cnt)
{
    int nread = 0;
    int fd = (int)(intptr_t)p->src.buf.data;
    char *datptr = p->src.buf.end;
    SCM_ASSERT(fd >= 0);
    while (nread == 0) {
        int r;
        errno = 0;
        SCM_SYSCALL(r, read(fd, datptr, cnt-nread));
        if (r < 0) {
            p->error = TRUE;
            Scm_SysError("read failed on %S", p);
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

static int file_flusher(ScmPort *p, int cnt, int forcep)
{
    int nwrote = 0;
    int datsiz = SCM_PORT_BUFFER_AVAIL(p);
    int fd = (int)(intptr_t)p->src.buf.data;
    char *datptr = p->src.buf.buffer;

    SCM_ASSERT(fd >= 0);
    while ((!forcep && nwrote == 0)
           || (forcep && nwrote < cnt)) {
        int r;
        errno = 0;
        SCM_SYSCALL(r, write(fd, datptr, datsiz-nwrote));
        if (r < 0) {
            if (SCM_PORT_BUFFER_SIGPIPE_SENSITIVE_P(p)) {
                /* (sort of) emulate termination by SIGPIPE.
                   NB: The difference is visible from the outside world
                   as the process exit status differ (WIFEXITED
                   instead of WIFSIGNALED).  If it becomes a problem,
                   we can reset the signal handler to SIG_DFL and
                   send SIGPIPE to self. */
                Scm_Exit(1);    /* exit code is somewhat arbitrary */
            }
            p->error = TRUE;
            Scm_SysError("write failed on %S", p);
        } else {
            datptr += r;
            nwrote += r;
        }
    }
    return nwrote;
}

static void file_closer(ScmPort *p)
{
    int fd = (int)(intptr_t)p->src.buf.data;
    SCM_ASSERT(fd >= 0);
    close(fd);
}

static int file_ready(ScmPort *p)
{
    int fd = (int)(intptr_t)p->src.buf.data;
    SCM_ASSERT(fd >= 0);
    return Scm_FdReady(fd, SCM_PORT_DIR(p));
}

static int file_filenum(ScmPort *p)
{
    return (int)(intptr_t)p->src.buf.data;
}

static off_t file_seeker(ScmPort *p, off_t offset, int whence)
{
    return lseek((int)(intptr_t)p->src.buf.data, offset, whence);
}

ScmObj Scm_OpenFilePort(const char *path, int flags, int buffering, int perm)
{
    int dir = 0;

    if ((flags & O_ACCMODE) == O_RDONLY) dir = SCM_PORT_INPUT;
    else if ((flags & O_ACCMODE) == O_WRONLY) dir = SCM_PORT_OUTPUT;
    else Scm_Error("unsupported file access mode %d to open %s", flags&O_ACCMODE, path);
    if (buffering < SCM_PORT_BUFFER_FULL || buffering > SCM_PORT_BUFFER_NONE) {
        Scm_Error("bad buffering flag: %d", buffering);
    }
#if defined(GAUCHE_WINDOWS)
    /* Force binary mode if not specified */
    if (!(flags & (O_TEXT|O_BINARY))) {
        flags |= O_BINARY;
    }
#endif /*GAUCHE_WINDOWS*/
    int fd = open(path, flags, perm);
    if (fd < 0) return SCM_FALSE;
    ScmPortBuffer bufrec;
    bufrec.mode = buffering;
    bufrec.buffer = NULL;
    bufrec.size = 0;
    bufrec.filler = file_filler;
    bufrec.flusher = file_flusher;
    bufrec.closer = file_closer;
    bufrec.ready = file_ready;
    bufrec.filenum = file_filenum;
    bufrec.seeker = file_seeker;
    bufrec.data = (void*)(intptr_t)fd;
    ScmObj p = Scm_MakeBufferedPort(SCM_CLASS_PORT, SCM_MAKE_STR_COPYING(path),
                                    dir, TRUE, &bufrec);
    return p;
}

/* Create a port on specified file descriptor.
      NAME  - used for the name of the port.
      DIRECTION - either SCM_PORT_INPUT or SCM_PORT_OUTPUT
      FD - the opened file descriptor.
      BUFMODE - buffering mode (ScmPortBufferMode)
      OWNERP - if TRUE, fd will be closed when this port is closed.
 */
ScmObj Scm_MakePortWithFd(ScmObj name, int direction,
                          int fd, int bufmode, int ownerp)
{
    ScmPortBuffer bufrec;

    bufrec.buffer = NULL;
    bufrec.size = 0;
    bufrec.mode = bufmode;
    bufrec.filler = file_filler;
    bufrec.flusher =file_flusher;
    bufrec.closer = file_closer;
    bufrec.ready = file_ready;
    bufrec.filenum = file_filenum;
    bufrec.data = (void*)(intptr_t)fd;

    /* Check if the given fd is seekable, and set seeker if so. */
    if (lseek(fd, 0, SEEK_CUR) < 0) {
        bufrec.seeker = NULL;
    } else {
        bufrec.seeker = file_seeker;
    }

    ScmObj p = Scm_MakeBufferedPort(SCM_CLASS_PORT, name, direction, ownerp,
                                    &bufrec);
    return p;
}

/*===============================================================
 * String port
 */

ScmObj Scm_MakeInputStringPort(ScmString *str, int privatep)
{
    ScmPort *p = make_port(SCM_CLASS_PORT, SCM_PORT_INPUT, SCM_PORT_ISTR);
    u_int size;
    const char *s = Scm_GetStringContent(str, &size, NULL, NULL);
    p->src.istr.start = s;
    p->src.istr.current = s;
    p->src.istr.end = s + size;
    SCM_PORT(p)->name = SCM_MAKE_STR("(input string port)");
    if (privatep) PORT_PRELOCK(p, Scm_VM());
    return SCM_OBJ(p);
}

ScmObj Scm_MakeOutputStringPort(int privatep)
{
    ScmPort *p = make_port(SCM_CLASS_PORT, SCM_PORT_OUTPUT, SCM_PORT_OSTR);
    Scm_DStringInit(&p->src.ostr);
    SCM_PORT(p)->name = SCM_MAKE_STR("(output string port)");
    if (privatep) PORT_PRELOCK(p, Scm_VM());
    return SCM_OBJ(p);
}

ScmObj Scm_GetOutputString(ScmPort *port, int flags)
{
    if (SCM_PORT_TYPE(port) != SCM_PORT_OSTR)
        Scm_Error("output string port required, but got %S", port);
    ScmVM *vm = Scm_VM();
    PORT_LOCK(port, vm);
    ScmObj r = Scm_DStringGet(&SCM_PORT(port)->src.ostr, flags);
    PORT_UNLOCK(port);
    return r;
}

ScmObj Scm_GetOutputStringUnsafe(ScmPort *port, int flags)
{
    if (SCM_PORT_TYPE(port) != SCM_PORT_OSTR)
        Scm_Error("output string port required, but got %S", port);
    return Scm_DStringGet(&SCM_PORT(port)->src.ostr, flags);
}

/* TRANSIENT: Pre-0.9 Compatibility routine.  Kept for the binary compatibility.
   Will be removed on 1.0 */
ScmObj Scm__GetOutputStringCompat(ScmPort *port)
{
    return Scm_GetOutputString(port, 0);
}

/* TRANSIENT: Pre-0.9 Compatibility routine.  Kept for the binary compatibility.
   Will be removed on 1.0 */
ScmObj Scm__GetOutputStringUnsafeCompat(ScmPort *port)
{
    return Scm_GetOutputStringUnsafe(port, 0);
}


static ScmObj get_remaining_input_string_aux(const char *s, int ssiz,
                                             const char *p, int psiz,
                                             int flags);

ScmObj Scm_GetRemainingInputString(ScmPort *port, int flags)
{
    if (SCM_PORT_TYPE(port) != SCM_PORT_ISTR)
        Scm_Error("input string port required, but got %S", port);
    /* NB: we don't need to lock the port, since the string body
       the port is pointing won't be changed. */
    const char *ep = port->src.istr.end;
    const char *cp = port->src.istr.current;
    /* Things gets complicated if there's an ungotten char or bytes.
       We want to share the string body whenever possible, so we
       first check the ungotten stuff matches the content of the
       buffer. */
    if (port->ungotten != SCM_CHAR_INVALID) {
        char cbuf[SCM_CHAR_MAX_BYTES];
        int nbytes = SCM_CHAR_NBYTES(port->ungotten);
        SCM_CHAR_PUT(cbuf, port->ungotten);
        const char *sp = port->src.istr.start;
        if (cp - sp >= nbytes
            && memcmp(cp - nbytes, cbuf, nbytes) == 0) {
            cp -= nbytes;       /* we can reuse buffer */
            return Scm_MakeString(cp, (int)(ep-cp), -1, flags);
        } else {
            /* we need to copy */
            return get_remaining_input_string_aux(cp, (int)(ep-cp), cbuf,
                                                  nbytes, flags);
        }
    } else if (port->scrcnt > 0) {
        const char *sp = port->src.istr.start;
        if (cp - sp >= (int)port->scrcnt
            && memcmp(cp - port->scrcnt, port->scratch, port->scrcnt) == 0) {
            cp -= port->scrcnt; /* we can reuse buffer */
            return Scm_MakeString(cp, (int)(ep-cp), -1, flags);
        } else {
            /* we need to copy */
            return get_remaining_input_string_aux(cp, (int)(ep-cp),
                                                  port->scratch,
                                                  port->scrcnt, flags);
        }
    } else {
        return Scm_MakeString(cp, (int)(ep-cp), -1, flags);
    }
}

static ScmObj get_remaining_input_string_aux(const char *s, int ssiz,
                                             const char *p, int psiz,
                                             int flags)
{
    char *b = SCM_NEW_ATOMIC2(char *, psiz+ssiz+1);
    memcpy(b, p, psiz);
    memcpy(b+psiz, s, ssiz);
    b[psiz+ssiz] = '\0';
    return Scm_MakeString(b, psiz+ssiz, -1, flags);
}

/* TRANSIENT: Pre-0.9 Compatibility routine.  Kept for the binary compatibility.
   Will be removed on 1.0 */
ScmObj Scm__GetRemainingInputStringCompat(ScmPort *port)
{
    return Scm_GetRemainingInputString(port, 0);
}

/*===============================================================
 * Procedural port
 */

/* To create a procedural port, fill in the ScmPortVTable function
   pointers and pass it to Scm_MakeVirtualPort.  You don't need to
   provide all the functions; put NULL if you think you don't
   provide the functionality.
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

static int null_ready(ScmPort *dummy, int charp)
    /*ARGSUSED*/
{
    return TRUE;
}

static void null_putb(ScmByte b, ScmPort *dummy)
    /*ARGSUSED*/
{
}

static void null_putc(ScmChar c, ScmPort *dummy)
    /*ARGSUSED*/
{
}

static void null_putz(const char *str, int len, ScmPort *dummy)
    /*ARGSUSED*/
{
}

static void null_puts(ScmString *s, ScmPort *dummy)
    /*ARGSUSED*/
{
}

static void null_flush(ScmPort *dummy)
    /*ARGSUSED*/
{
}

ScmObj Scm_MakeVirtualPort(ScmClass *klass, int direction,
                           const ScmPortVTable *vtable)
{
    ScmPort *p = make_port(klass, direction, SCM_PORT_PROC);

    /* Copy vtable, and ensure all entries contain some ptr */
    p->src.vt = *vtable;
    if (!p->src.vt.Getb)  p->src.vt.Getb = null_getb;
    if (!p->src.vt.Getc)  p->src.vt.Getc = null_getc;
    if (!p->src.vt.Getz)  p->src.vt.Getz = null_getz;
    if (!p->src.vt.Ready) p->src.vt.Ready = null_ready;
    if (!p->src.vt.Putb)  p->src.vt.Putb = null_putb;
    if (!p->src.vt.Putc)  p->src.vt.Putc = null_putc;
    if (!p->src.vt.Putz)  p->src.vt.Putz = null_putz;
    if (!p->src.vt.Puts)  p->src.vt.Puts = null_puts;
    if (!p->src.vt.Flush) p->src.vt.Flush = null_flush;
    /* Close and Seek can be left NULL */
    return SCM_OBJ(p);
}

/*===============================================================
 * Coding-aware port
 */

/* Coding-aware port wraps an input port, and specifically recognizes
   'coding' magic comment.   It is primarily used when loading source
   code, but can be used separately. */

static ScmPort *(*coding_aware_port_hook)(ScmPort *src,
                                          const char *srcencoding) = NULL;

/* gauche.charconv sets the pointer */
void Scm__InstallCodingAwarePortHook(ScmPort *(*f)(ScmPort*, const char*))
{
    coding_aware_port_hook = f;
}

#define CODING_MAGIC_COMMENT_LINES 2 /* maximum number of lines to be
                                        looked at for the 'encoding' magic
                                        comment. */

typedef struct coding_port_data_rec {
    ScmPort *source;            /* source port */
    int state;                  /* port state; see below */
    const char *pbuf;           /* prefetched buffer.  NUL terminated.
                                   contains at most CODING_MAGIC_COMMENT_LINES
                                   newlines. */
    int pbufsize;               /* # of bytes in pbuf */
} coding_port_data;

enum {
    CODING_PORT_INIT,           /* initial state */
    CODING_PORT_RECOGNIZED,     /* prefetched up to two lines, and
                                   conversion port is set if necessary.
                                   there are buffered data in lines[]. */
    CODING_PORT_FLUSHED         /* prefetched lines are flushed. */
};

/* A hardcoded DFA to recognize #/;.*coding[:=]\s*([\w.-]+)/ */
static const char *look_for_encoding(const char *buf)
{
    const char *s;

  init:
    for (;;) {
        switch (*buf++) {
        case '\0': return NULL;
        case ';':  goto comment;
        }
    }
  comment:
    for (;;) {
        switch (*buf++) {
        case '\0': return NULL;
        case '\n': goto init;
        case '\r': if (*buf != '\n') goto init; break;
        case 'c' : goto coding;
        }
    }
  coding:
    if (strncmp(buf, "oding", 5) != 0) goto comment;
    buf+=5;
    if (*buf != ':' && *buf != '=') goto comment;
    for (buf++;;buf++) {
        if (*buf != ' ' && *buf != '\t') break;
    }
    if (*buf == '\0') return NULL;

    for (s = buf;*buf;buf++) {
        if (!isalnum(*buf) && *buf != '_' && *buf != '-' && *buf != '.') {
            break;
        }
    }
    if (s == buf) goto comment;

    /* Here we found a matching string, starting from s and ends at buf. */

    /* kludge: Emacs uses special suffix #/-(unix|dos|mac)$/ to distinguish
       EOL variants.  For compatibility, drop such suffix if we have one. */
    if (buf-s > 5 && (strncmp(buf-5, "-unix", 5) == 0)) {
        buf -= 5;
    } else if (buf-s > 4 && (strncmp(buf-4, "-dos", 4) == 0
                             || strncmp(buf-4, "-mac", 4) == 0)) {
        buf -= 4;
    }

    /* Copy and return the encoding string */
    return SCM_STRDUP_PARTIAL(s, buf-s);
}

static void coding_port_recognize_encoding(ScmPort *port,
                                           coding_port_data *data)
{
    int num_newlines = 0;
    int cr_seen = FALSE;

    SCM_ASSERT(data->source != NULL);

    /* Prefetch up to CODING_MAGIC_COMMENT_LINES lines or the first NUL
       character.   data->pbuf ends up holding NUL terminated string. */
    ScmDString ds;
    Scm_DStringInit(&ds);
    for (;num_newlines < CODING_MAGIC_COMMENT_LINES;) {
        int c = Scm_GetbUnsafe(data->source);
        if (c == EOF) break;
        if (c == 0) {
            /* take extra care not to lose '\0' */
            Scm_UngetbUnsafe(c, data->source);
            break;
        }
        SCM_DSTRING_PUTB(&ds, c);
        if (c == '\r') {   /* for the source that only uses '\r' */
            if (cr_seen) num_newlines++;
            cr_seen = TRUE;
        } else if (c == '\n' || cr_seen) {
            num_newlines++;
            cr_seen = FALSE;
        } else {
            cr_seen = FALSE;
        }
    }
    data->pbuf = Scm_DStringGetz(&ds);
    data->pbufsize = (int)strlen(data->pbuf);

    /* Look for the magic comment */
    const char *encoding = NULL;
    encoding = look_for_encoding(data->pbuf);

    /* Wrap the source port by conversion port, if necessary. */
    if (encoding == NULL || Scm_SupportedCharacterEncodingP(encoding)) {
        return;
    }

    if (coding_aware_port_hook == NULL) {
        /* Require gauche.charconv.
           NB: we don't need mutex here, for loading the module is
           serialized in Scm_Require. */
        Scm_Require(SCM_MAKE_STR("gauche/charconv"),
                    SCM_LOAD_PROPAGATE_ERROR, NULL);
        if (coding_aware_port_hook == NULL) {
            Scm_PortError(port, SCM_PORT_ERROR_OTHER,
                          "couldn't load gauche.charconv module");
        }
    }
    data->source = coding_aware_port_hook(data->source, encoding);
}

static int coding_filler(ScmPort *p, int cnt)
{
    int nread = 0;
    coding_port_data *data = (coding_port_data*)p->src.buf.data;
    char *datptr = p->src.buf.end;

    SCM_ASSERT(data->source);

    /* deals with the most frequent case */
    if (data->state == CODING_PORT_FLUSHED) {
        return Scm_GetzUnsafe(datptr, cnt, data->source);
    }

    if (data->state == CODING_PORT_INIT) {
        coding_port_recognize_encoding(p, data);
        data->state = CODING_PORT_RECOGNIZED;
    }

    /* Here, we have data->state == CODING_PORT_RECOGNIZED */
    if (data->pbufsize > 0) {
        if (data->pbufsize <= cnt) {
            memcpy(datptr, data->pbuf, data->pbufsize);
            nread = data->pbufsize;
            data->pbuf = NULL;
            data->pbufsize = 0;
            data->state = CODING_PORT_FLUSHED;
        } else {
            memcpy(datptr, data->pbuf, cnt);
            nread = cnt;
            data->pbuf += cnt;
            data->pbufsize -= cnt;
        }
        return nread;
    } else {
        data->state = CODING_PORT_FLUSHED;
        return Scm_GetzUnsafe(datptr, cnt, data->source);
    }
}

static void coding_closer(ScmPort *p)
{
    coding_port_data *data = (coding_port_data*)p->src.buf.data;
    if (data->source) {
        Scm_ClosePort(data->source);
        data->source = NULL;
    }
}

static int coding_ready(ScmPort *p)
{
    coding_port_data *data = (coding_port_data*)p->src.buf.data;
    if (data->source == NULL) return TRUE;
    if (data->state == CODING_PORT_RECOGNIZED) {
        return SCM_FD_READY;
    } else {
        return Scm_ByteReadyUnsafe(p);
    }
}

static int coding_filenum(ScmPort *p)
{
    coding_port_data *data = (coding_port_data*)p->src.buf.data;
    if (data->source == NULL) return -1;
    return Scm_PortFileNo(data->source);
}

ScmObj Scm_MakeCodingAwarePort(ScmPort *iport)
{
    if (!SCM_IPORTP(iport)) {
        Scm_Error("open-coding-aware-port requires an input port, but got %S", iport);
    }
    coding_port_data *data = SCM_NEW(coding_port_data);
    data->source = iport;
    data->state = CODING_PORT_INIT;
    data->pbuf = NULL;
    data->pbufsize = 0;

    ScmPortBuffer bufrec;
    bufrec.mode = SCM_PORT_BUFFER_FULL;
    bufrec.buffer = NULL;
    bufrec.size = 0;
    bufrec.filler = coding_filler;
    bufrec.flusher = NULL;
    bufrec.closer = coding_closer;
    bufrec.ready = coding_ready;
    bufrec.filenum = coding_filenum;
    bufrec.seeker = NULL;
    bufrec.data = (void*)data;
    ScmObj p = Scm_MakeBufferedPort(SCM_CLASS_CODING_AWARE_PORT,
                                    Scm_PortName(iport), SCM_PORT_INPUT,
                                    TRUE, &bufrec);
    return p;
}

/*===============================================================
 * Limited Length Port
 */

#if 0
/* Limited-length port transfers information up to specified
   characters/bytes.  Input limited-length port returns EOF after
   the specified amount of data is read, and output limited-length port
   discards data written past the specified amount.  The port is not
   seekable. */

typedef struct limited_port_data_rec {
    ScmPort *source;            /* source/sink port */
    int max_bytes;              /* -1 if byte count doesn't matter */
    int max_chars;              /* -1 if char count doesn't matter */
    int byte_count;
    int char_count;
    int limit_reached;          /* TRUE once the limit is reached. */
} limited_port_data;

#define BYTE_LIMITED(data) ((data)->max_bytes >= 0)
#define CHAR_LIMITED(data) ((data)->max_chars >= 0)

static int limit_getb(ScmPort *p)
{
    limited_port_data *data = (limited_port_data*)SCM_PORT_VIRTUAL_DATA(p);
    if (data->limit_reached) return EOF;
    if (BYTE_LIMITED(data) && data->byte_count++ >= data->max_bytes) {
        data->limit_reached = TRUE;
        return EOF;
    }
    return Scm_Getb(data->source);
}

static int limit_getc(ScmPort *c)
{
    limited_port_data *data = (limited_port_data*)SCM_PORT_VIRTUAL_DATA(p);
    if (data->limit_reached) return SCM_CHAR_INVALID;
    if (CHAR_LIMITED(data) && data->char_count++ >= data->max_chars) {
        data->limit_reached = TRUE;
        return SCM_CHAR_INVALID;
    }
    return Scm_Getc(data->source);
}

static int limit_getz(char *buf, int buflen, ScmPort *p)
{
    int rest = 0, size, nread;
    limited_port_data *data = (limited_port_data*)SCM_PORT_VIRTUAL_DATA(p);

    if (data->limit_reached) return 0;
    if (BYTE_LIMITED(data)) {
        rest = data->max_bytes - data->byte_count;
        size = MIN(buflen, rest);
    } else {
        size = buflen;
    }
    nread = Scm_Getz(buf, size, p);
    if (nread == 0) return 0;

    if (nread == rest) {
        data->limit_reached = TRUE;
    }
    data->byte_count += nread;
    return nread;
}

static void limit_putb(ScmByte b, ScmPort *p)
{
    limited_port_data *data = (limited_port_data*)SCM_PORT_VIRTUAL_DATA(p);
    if (data->limit_reached) return;
    Scm_Putb(b, data->source);
    if (BYTE_LIMITED(data) && ++data->byte_count >= data->max_bytes) {
        data->limit_reached = TRUE;
    }
}

static void limit_putc(ScmChar c, ScmPort *p)
{
    limited_port_data *data = (limited_port_data*)SCM_PORT_VIRTUAL_DATA(p);
    if (data->limit_reached) return;
    Scm_Putc(c, data->source);
    if (CHAR_LIMITED(data) && ++data->char_count >= data->max_chars) {
        data->limit_reached = TRUE;
    }
}

static void limit_putz(const char *buf, int size, ScmPort *p)
{
    int realsize, nwritten;
    limited_port_data *data = (limited_port_data*)SCM_PORT_VIRTUAL_DATA(p);
    if (data->limit_reached) return;
    if (BYTE_LIMITED(data)) {

    }
}


ScmObj Scm_MakeLimitedLengthPort(ScmPort *source,
                                 int max_bytes,
                                 int max_chars)
{
    ScmPortVTable vtable;
    limited_port_data *data;

    data = SCM_NEW(limited_port_data);
    data->source = source;
    data->max_bytes = max_bytes;
    data->max_chars = max_chars;
    data->byte_count = 0;
    data->char_count = 0;
    data->limit_reached = FALSE;


}

#endif

/*===============================================================
 * Standard ports
 */

static ScmObj scm_stdin  = SCM_UNBOUND;
static ScmObj scm_stdout = SCM_UNBOUND;
static ScmObj scm_stderr = SCM_UNBOUND;

#define DEFSTDPORT(Name, var)                           \
    ScmObj SCM_CPP_CAT(Scm_, Name)(void)                \
    {                                                   \
        return var;                                     \
    }                                                   \
    ScmObj SCM_CPP_CAT(Scm_Set, Name)(ScmPort *port)    \
    {                                                   \
        ScmObj oldp = var;                              \
        var = SCM_OBJ(port);                            \
        return oldp;                                    \
    }

DEFSTDPORT(Stdin, scm_stdin)
DEFSTDPORT(Stdout, scm_stdout)
DEFSTDPORT(Stderr, scm_stderr)

ScmObj Scm_SetCurrentInputPort(ScmPort *port)
{
    ScmVM *vm = Scm_VM();
    ScmObj oldp = SCM_OBJ(SCM_VM_CURRENT_INPUT_PORT(vm));
    SCM_VM_CURRENT_INPUT_PORT(vm) = port;
    return oldp;
}

ScmObj Scm_SetCurrentOutputPort(ScmPort *port)
{
    ScmVM *vm = Scm_VM();
    ScmObj oldp = SCM_OBJ(SCM_VM_CURRENT_OUTPUT_PORT(vm));
    SCM_VM_CURRENT_OUTPUT_PORT(vm) = port;
    return oldp;
}

ScmObj Scm_SetCurrentErrorPort(ScmPort *port)
{
    ScmVM *vm = Scm_VM();
    ScmObj oldp = SCM_OBJ(SCM_VM_CURRENT_ERROR_PORT(vm));
    SCM_VM_CURRENT_ERROR_PORT(vm) = port;
    return oldp;
}

/*===============================================================
 * Initialization
 */

void Scm__InitPort(void)
{
    (void)SCM_INTERNAL_MUTEX_INIT(active_buffered_ports.mutex);
    active_buffered_ports.ports = SCM_WEAK_VECTOR(Scm_MakeWeakVector(PORT_VECTOR_SIZE));

    Scm_InitStaticClass(&Scm_PortClass, "<port>",
                        Scm_GaucheModule(), port_slots, 0);
    Scm_InitStaticClass(&Scm_CodingAwarePortClass, "<coding-aware-port>",
                        Scm_GaucheModule(), port_slots, 0);

    scm_stdin  = Scm_MakePortWithFd(SCM_MAKE_STR("(standard input)"),
                                    SCM_PORT_INPUT, 0,
                                    SCM_PORT_BUFFER_FULL, TRUE);
    /* By default, stdout and stderr are SIGPIPE sensitive */
    scm_stdout = Scm_MakePortWithFd(SCM_MAKE_STR("(standard output)"),
                                    SCM_PORT_OUTPUT, 1,
                                    ((isatty(1)
                                      ? SCM_PORT_BUFFER_LINE
                                      : SCM_PORT_BUFFER_FULL)
                                     | SCM_PORT_BUFFER_SIGPIPE_SENSITIVE),
                                    TRUE);
    scm_stderr = Scm_MakePortWithFd(SCM_MAKE_STR("(standard error output)"),
                                    SCM_PORT_OUTPUT, 2,
                                    (SCM_PORT_BUFFER_NONE
                                     | SCM_PORT_BUFFER_SIGPIPE_SENSITIVE),
                                    TRUE);

    /* The root VM is initialized with bogus standard ports; we need to
       reset them. */
    Scm_VM()->curin  = SCM_PORT(scm_stdin);
    Scm_VM()->curout = SCM_PORT(scm_stdout);
    Scm_VM()->curerr = SCM_PORT(scm_stderr);

    key_full   = Scm_MakeKeyword(SCM_STRING(SCM_MAKE_STR("full")));
    key_modest = Scm_MakeKeyword(SCM_STRING(SCM_MAKE_STR("modest")));
    key_line   = Scm_MakeKeyword(SCM_STRING(SCM_MAKE_STR("line")));
    key_none   = Scm_MakeKeyword(SCM_STRING(SCM_MAKE_STR("none")));
}

/* Windows specific:

   When we run Windows no-console mode, stdios are bogus (gosh-noconsole
   wires them to NUL device, but when libgauche is called from other
   applications, we can't assume that.)   It is too painful to be so,
   since when Scheme program tries to write to stdout or stderr it just
   crashes without any information at all.

   So, this function re-wires Scheme standard output and error output
   to a special port; when an output is made for the first time
   the port opens up a console by AllocConsole(), and redirects further
   output to it.

   Unfortunately we can't do this at initialization time, since
   Scm_Init() doesn't have a way to know whether we're in console mode
   or not.  Only the application knows, thus it needs to call this API
   after Scheme system is initialized.
 */
#if defined(GAUCHE_WINDOWS)

static ScmInternalMutex win_console_mutex;
static int win_console_created = FALSE;

static int trapper_flusher(ScmPort *p, int cnt, int forcep)
{
    size_t nwrote = 0;
    int size = SCM_PORT_BUFFER_AVAIL(p);
    char *buf = p->src.buf.buffer;

    SCM_INTERNAL_MUTEX_LOCK(win_console_mutex);
    if (!win_console_created) {
        AllocConsole();
        freopen("CONOUT$", "w", stdout);
        win_console_created = TRUE;
    }
    SCM_INTERNAL_MUTEX_UNLOCK(win_console_mutex);

    while ((!forcep && nwrote == 0) || (forcep && nwrote < cnt)) {
        size_t r = fwrite(buf, 1, size, stdout);
        if (ferror(stdout)) {
            /* NB: Double fault will be caught in the error handling
               mechanism, so we don't need to worry it here. */
            Scm_Error("output to CONOUT$ failed");
        }
        nwrote += r;
        buf += r;
    }
    fflush(stdout);
    return nwrote;
}

static ScmObj make_trapper_port()
{
    ScmPortBuffer bufrec;

    bufrec.mode = SCM_PORT_BUFFER_LINE;
    bufrec.buffer = NULL;
    bufrec.size = 0;
    bufrec.filler = NULL;
    bufrec.flusher = trapper_flusher;
    bufrec.closer = NULL;
    bufrec.ready = NULL;
    bufrec.filenum = NULL;
    bufrec.seeker = NULL;
    bufrec.data = NULL;
    return Scm_MakeBufferedPort(SCM_CLASS_PORT,
                                SCM_MAKE_STR("(console output)"),
                                SCM_PORT_OUTPUT, TRUE, &bufrec);
}

/* This is supposed to be called from application main(), before any
   threads are created.  We don't mutex here. */
void Scm__SetupPortsForWindows(int has_console)
{
    if (!has_console) {
        static int initialized = FALSE;
        static ScmObj orig_stdout = SCM_FALSE;
        static ScmObj orig_stderr = SCM_FALSE;
        if (!initialized) {
            ScmObj trapperPort = make_trapper_port();
            initialized = TRUE;
            SCM_INTERNAL_MUTEX_INIT(win_console_mutex);
            /* Original scm_stdout and scm_stderr holds ports that are
               connected to fd=0 and fd=1, respectively.  Losing reference
               to those ports will eventually lead to close those fds (when
               those ports are GC-ed), causing complications in the code
               that assumes fds 0, 1 and 2 are reserved.  To make things
               easier, we just save the original ports. */
            orig_stdout = scm_stdout;
            orig_stderr = scm_stderr;
            scm_stdout = trapperPort;
            scm_stderr = trapperPort;
            Scm_VM()->curout = SCM_PORT(scm_stdout);
            Scm_VM()->curerr = SCM_PORT(scm_stderr);
        }
    }
}
#endif /*defined(GAUCHE_WINDOWS)*/
