/*
 * port.c - port implementation
 *
 *   Copyright (c) 2000-2020  Shiro Kawai  <shiro@acm.org>
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
#include "gauche/priv/builtin-syms.h"

#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <ctype.h>

#undef MAX
#undef MIN
#define MAX(a, b) ((a)>(b)? (a) : (b))
#define MIN(a, b) ((a)<(b)? (a) : (b))

#define PORT_BUFFER_MODE(p) \
    (PORT_BUF(p)->mode & SCM_PORT_BUFFER_MODE_MASK)
#define PORT_BUFFER_SIGPIPE_SENSITIVE_P(p) \
    (PORT_BUF(p)->mode & SCM_PORT_BUFFER_SIGPIPE_SENSITIVE)
#define PORT_BUFFER_ROOM(p) \
    (PORT_BUF(p)->buffer + PORT_BUF(p)->size - PORT_BUF(p)->end)
#define PORT_BUFFER_AVAIL(p) \
    (PORT_BUF(p)->current - PORT_BUF(p)->buffer)

#define PORT_UNGOTTEN(p)  (P_(p)->ungotten)
#define PORT_SCRATCH(p)   (P_(p)->scratch)
#define PORT_LINE(p)      (P_(p)->line)
#define PORT_BYTES(p)     (P_(p)->bytes)
#define PORT_ATTRS(p)     (P_(p)->attrs)
#define PORT_SAVED_POS(p) (P_(p)->savedPos)

/* Parameter location for the global reader lexical mode, from which
   ports inherit. */
static ScmPrimitiveParameter *readerLexicalMode;

/*================================================================
 * Class stuff
 */

static void port_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx);
static void port_finalize(ScmObj obj, void* data);
static void register_buffered_port(ScmPort *port);
static void unregister_buffered_port(ScmPort *port);
static void bufport_flush(ScmPort*, ScmSize, int);
static void file_closer(ScmPort *p);
static int  file_buffered_port_p(ScmPort *p);       /* for Scm_PortFdDup */
static void file_buffered_port_set_fd(ScmPort *p, int fd); /* ditto */

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

    /* NB: Flush or close subroutine may raise an error and leave the port
       not fully cleaned up.  For now, we leave the port 'non-closed' state,
       so this part may be called again---it's up to the close routine to
       handle the situation gracefully.
    */
    switch (SCM_PORT_TYPE(port)) {
    case SCM_PORT_FILE:
        if (SCM_PORT_DIR(port) == SCM_PORT_OUTPUT) {
            if (!SCM_PORT_ERROR_OCCURRED_P(port)) {
                bufport_flush(port, 0, TRUE);
            }
            if (!(SCM_PORT_FLAGS(port) & SCM_PORT_TRANSIENT)) {
                unregister_buffered_port(port);
            }
        }
        ScmPortBuffer *buf = Scm_PortBufferStruct(port);
        if (port->ownerp && buf->closer) buf->closer(port);
        break;
    case SCM_PORT_PROC:
        if (PORT_VT(port)->Close) PORT_VT(port)->Close(port);
        break;
    default:
        break;
    }
    (void)SCM_INTERNAL_FASTLOCK_DESTROY(P_(port)->lock);

    SCM_PORT_CLOSED_P(port) = TRUE;
    /* avoid unnecessary finalization */
    Scm_UnregisterFinalizer(SCM_OBJ(port));
}

/* called by GC */
static void port_finalize(ScmObj obj, void* data SCM_UNUSED)
{
    port_cleanup(SCM_PORT(obj));
}

/*
 * Internal Constructor.
 *   If this port owns the underlying file descriptor/stream,
 *   ownerp must be TRUE.
 */
static ScmPort *make_port(ScmClass *klass, ScmObj name, int dir, int type)
{
    ScmPortImpl *port = (ScmPortImpl*)SCM_NEW_INSTANCE(ScmPort, klass);

    port->direction = dir & SCM_PORT_IOMASK;
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
    port->savedPos = SCM_UNBOUND;
    (void)SCM_INTERNAL_FASTLOCK_INIT(port->lock);
    port->lockOwner = NULL;
    port->lockCount = 0;
    port->writeState = NULL;
    port->line = 1;
    /* We set name attribute as read-only attribute.  See portapi.c
       for the format of attrs. */
    port->attrs = SCM_LIST1(Scm_Cons(SCM_SYM_NAME, Scm_Cons(name, SCM_FALSE)));

    Scm_RegisterFinalizer(SCM_OBJ(port), port_finalize, NULL);

    /* Default reader lexical mode */
    Scm_PortAttrSetUnsafe(SCM_PORT(port),
                          SCM_SYM_READER_LEXICAL_MODE,
                          Scm_ReaderLexicalMode());

    return SCM_PORT(port);
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
ScmObj Scm_VMWithPortLocking(ScmPort *port SCM_UNUSED, ScmObj closure)
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
    return Scm_PortAttrGet(port, SCM_SYM_NAME, SCM_FALSE);
}

ScmSize Scm_PortLine(ScmPort *port)
{
    return PORT_LINE(port);
}

ScmSize Scm_PortBytes(ScmPort *port)
{
    return PORT_BYTES(port);
}

static void port_print(ScmObj obj, ScmPort *port,
                       ScmWriteContext *ctx SCM_UNUSED)
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
        ScmPortBuffer *buf = Scm_PortBufferStruct(port);
        if (buf->filenum) return buf->filenum(port);
        else return -1;
    } else {
        /* TODO: proc port */
        return -1;
    }
}

/* Returns a pointer to the 'src' strcuture.  User code should use this
 * instead of directly referring to 'src' member.
 */
ScmPortBuffer *Scm_PortBufferStruct(ScmPort *port)
{
    SCM_ASSERT(port->type == SCM_PORT_FILE);
    return PORT_BUF(port);
}

ScmPortInputString *Scm_PortInputStringStruct(ScmPort *port)
{
    SCM_ASSERT(port->type == SCM_PORT_ISTR);
    return PORT_ISTR(port);
}

ScmDString *Scm_PortOutputDString(ScmPort *port)
{
    SCM_ASSERT(port->type == SCM_PORT_OSTR);
    return PORT_OSTR(port);
}

ScmPortVTable *Scm_PortVTableStruct(ScmPort *port)
{
    SCM_ASSERT(port->type == SCM_PORT_PROC);
    return PORT_VT(port);
}

/* For input buffered port, returns the size of room that can be filled
   by the filler */
ScmSize Scm_PortBufferRoom(ScmPort *port)
{
    SCM_ASSERT(port->type == SCM_PORT_FILE);
    return PORT_BUFFER_ROOM(port);
}

/* For output buffered port, returns the size of available data that can
   be flushed by the flusher */
ScmSize Scm_PortBufferAvail(ScmPort *port)
{
    SCM_ASSERT(port->type == SCM_PORT_FILE);
    return PORT_BUFFER_AVAIL(port);
}

ScmWriteState *Scm_PortWriteState(ScmPort *port)
{
    return P_(port)->writeState;
}

void Scm_PortWriteStateSet(ScmPort *port, ScmWriteState *ws)
{
    P_(port)->writeState = ws;
}

int Scm_GetPortBufferingMode(ScmPort *port)
{
    if (port->type == SCM_PORT_FILE) return PORT_BUFFER_MODE(port);
    else return SCM_PORT_BUFFER_NONE;
}

void Scm_SetPortBufferingMode(ScmPort *port, int mode)
{
    if (port->type != SCM_PORT_FILE) {
        Scm_Error("Can't set buffering mode to non-buffered port: %S", port);
    }
    PORT_BUF(port)->mode =
        (PORT_BUF(port)->mode & ~SCM_PORT_BUFFER_MODE_MASK)
        | (mode & SCM_PORT_BUFFER_MODE_MASK);
}

int Scm_GetPortBufferSigpipeSensitive(ScmPort *port)
{
    if (port->type == SCM_PORT_FILE) {
        return (PORT_BUFFER_SIGPIPE_SENSITIVE_P(port) != FALSE);
    } else {
        return FALSE;
    }
}

void Scm_SetPortBufferSigpipeSensitive(ScmPort *port, int sensitive)
{
    if (port->type != SCM_PORT_FILE) {
        Scm_Error("Can't set sigpipe sensitivity to non-buffered port: %S",
                  port);
    }
    if (sensitive) {
        PORT_BUF(port)->mode |=  SCM_PORT_BUFFER_SIGPIPE_SENSITIVE;
    } else {
        PORT_BUF(port)->mode &= ~SCM_PORT_BUFFER_SIGPIPE_SENSITIVE;
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

/* Port's reader lexical mode is set at port creation, taken from
   readerLexicalMode parameter.  It may be altered by reader directive
   such as #!r7rs.
   The possible value is the same as the global reader lexical mode,
   i.e.  one of the symbols legacy, warn-legacy, permissive or strict-r7.
*/
ScmObj Scm_GetPortReaderLexicalMode(ScmPort *port)
{
    /* We let it throw an error if there's no reader-lexical-mode attr.
       It must be set in the constructor. */
    return Scm_PortAttrGet(port, SCM_SYM_READER_LEXICAL_MODE, SCM_UNBOUND);
}

void Scm_SetPortReaderLexicalMode(ScmPort *port, ScmObj mode)
{
    /*The check is duplicatd in Scm_SetReaderLexicalMode; refactoring needed.*/
    if (!(SCM_EQ(mode, SCM_SYM_LEGACY)
          || SCM_EQ(mode, SCM_SYM_WARN_LEGACY)
          || SCM_EQ(mode, SCM_SYM_PERMISSIVE)
          || SCM_EQ(mode, SCM_SYM_STRICT_R7))) {
        Scm_Error("reader-lexical-mode must be one of the following symbols:"
                  " legacy, warn-legacy, permissive, strict-r7, but got %S",
                  mode);
    }
    Scm_PortAttrSet(port, SCM_SYM_READER_LEXICAL_MODE, mode);
}

/* global reader lexical mode. */
ScmObj Scm_SetReaderLexicalMode(ScmObj mode)
{
    if (!(SCM_EQ(mode, SCM_SYM_LEGACY)
          || SCM_EQ(mode, SCM_SYM_WARN_LEGACY)
          || SCM_EQ(mode, SCM_SYM_PERMISSIVE)
          || SCM_EQ(mode, SCM_SYM_STRICT_R7))) {
        Scm_Error("reader-lexical-mode must be one of the following symbols:"
                  " legacy, warn-legacy, permissive, strict-r7, but got %S",
                  mode);
    }
    return Scm_PrimitiveParameterSet(Scm_VM(), readerLexicalMode, mode);
}

ScmObj Scm_ReaderLexicalMode()
{
    return Scm_PrimitiveParameterRef(Scm_VM(), readerLexicalMode);
}

/* flag can be checked with SCM_PORT_ERROR_OCCURRED_P()  */
void Scm_SetPortErrorOccurred(ScmPort *port, int flag)
{
    port->error = flag;
}

/* Query whether the port is positoinable.  If setp is false, returns
   if port can get current pos.  If setp is true, returns if port
   can set pos.
   Note: For the buffering and procedural ports, if the user used old
   protocol (using seeker), we can't exactly know if get/set position
   is possible or not.
 */
int Scm_PortPositionable(ScmPort *port, int setp)
{
    switch (SCM_PORT_TYPE(port)) {
    case SCM_PORT_FILE:
        if (setp) {
            return (PORT_BUF(port)->setpos || PORT_BUF(port)->seeker);
        } else {
            return (PORT_BUF(port)->getpos || PORT_BUF(port)->seeker);
        }
    case SCM_PORT_PROC:
        if (setp) {
            return (PORT_VT(port)->SetPos || PORT_VT(port)->Seek);
        } else {
            return (PORT_VT(port)->GetPos || PORT_VT(port)->Seek);
        }
    case SCM_PORT_ISTR:
        return TRUE;
    case SCM_PORT_OSTR:
        if (setp) return FALSE; /* we haven't supported setpos for ostr */
        else      return TRUE;
    }
    return FALSE;		/* dummy */
}

/* Duplicates the file descriptor of the source port, and set it to
   the destination port.  Both source and destination port must be
   file ports.
   DST also must be a file buffered port, for we rewrite the fd slot
   in its private data structure. */
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

    int srcfd = Scm_PortFileNo(src);
    int dstfd = Scm_PortFileNo(dst);

    if (srcfd < 0) Scm_Error("port isn't associated to fd: %S", src);
    if (dstfd < 0) Scm_Error("port isn't associated to fd: %S", dst);

    if (!file_buffered_port_p(dst)) {
        Scm_Error("port isn't directly associated to file: %S", dst);
    }

    if (dst->direction == SCM_PORT_INPUT) {
        /* discard the current buffer */
        ScmVM *vm = Scm_VM();
        PORT_LOCK(dst, vm);
        PORT_BUF(dst)->current = PORT_BUF(dst)->buffer;
        PORT_BUF(dst)->end = PORT_BUF(dst)->buffer;
        PORT_UNLOCK(dst);
    } else {
        /* flush the current buffer */
        Scm_Flush(dst);
    }
    /*  NB: We don't retry dup2().  By the time it returns EINTR, the
        dstfd has actually been closed, and if other thread happens to
        grab the same fd, retrying dup2() inadvertently closes that one.
    */

#if defined(GAUCHE_WINDOWS)
    r = _dup2(srcfd, dstfd);
#else  /*!GAUCHE_WINDOWS*/
    r = dup2(srcfd, dstfd);
#endif /*!GAUCHE_WINDOWS*/
    if (r < 0) Scm_SysError("dup2 failed");
    file_buffered_port_set_fd(dst, r);
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
        if (h == INVALID_HANDLE_VALUE) return SCM_FD_READY;

        /* pipe */
        DWORD avail;
        if (PeekNamedPipe(h, NULL, 0, NULL, &avail, NULL) != 0) {
            if (avail == 0) return SCM_FD_WOULDBLOCK;
            else return SCM_FD_READY;
        }

        /* socket */
        int optval;
        int optlen;
        optlen = sizeof(optval);
        if (getsockopt((SOCKET)h, SOL_SOCKET, SO_TYPE, (char*)&optval, &optlen) != SOCKET_ERROR) {
            fd_set fds;
            int r;
            struct timeval tm;
            FD_ZERO(&fds);
            FD_SET((SOCKET)h, &fds);
            tm.tv_sec = tm.tv_usec = 0;
            /* NB: The first argument of select() is ignored on Windows */
            SCM_SYSCALL(r, select(0, &fds, NULL, NULL, &tm));
            if (r < 0) Scm_SysError("select failed");
            if (r > 0) return SCM_FD_READY;
            else       return SCM_FD_WOULDBLOCK;
        }

        /* other */
        return SCM_FD_UNKNOWN;
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
 *    Port is closed either explicitly (via close-port etc) or implicitly
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

ScmObj Scm_MakeBufferedPortFull(ScmClass *klass,
                                ScmObj name,
                                int dir,     /* direction */
                                ScmPortBuffer *bufrec,
                                u_long flags)
{
    ScmSize size = bufrec->size;
    char *buf = bufrec->buffer;

    if (size == 0) size = SCM_PORT_DEFAULT_BUFSIZ;
    if (buf == NULL) buf = SCM_NEW_ATOMIC2(char*, size);
    ScmPort *p = make_port(klass, name, dir, SCM_PORT_FILE);
    p->ownerp = flags & SCM_PORT_OWNER;
    PORT_BUF(p)->buffer = buf;
    if ((dir & SCM_PORT_IOMASK) == SCM_PORT_INPUT) {
        PORT_BUF(p)->current = PORT_BUF(p)->buffer;
        PORT_BUF(p)->end = PORT_BUF(p)->buffer;
    } else {
        PORT_BUF(p)->current = PORT_BUF(p)->buffer;
        PORT_BUF(p)->end = PORT_BUF(p)->buffer + size;
    }
    if (dir == SCM_PORT_OUTPUT_TRANSIENT) {
        SCM_PORT_FLAGS(p) |= SCM_PORT_TRANSIENT;
    }
    PORT_BUF(p)->size = size;
    PORT_BUF(p)->mode = bufrec->mode;
    PORT_BUF(p)->filler = bufrec->filler;
    PORT_BUF(p)->flusher = bufrec->flusher;
    PORT_BUF(p)->closer = bufrec->closer;
    PORT_BUF(p)->ready = bufrec->ready;
    PORT_BUF(p)->filenum = bufrec->filenum;
    PORT_BUF(p)->seeker = bufrec->seeker;
    PORT_BUF(p)->data = bufrec->data;

    if (flags & SCM_PORT_WITH_POSITION) {
        PORT_BUF(p)->getpos = bufrec->getpos;
        PORT_BUF(p)->setpos = bufrec->setpos;
        PORT_BUF(p)->flags  = bufrec->flags;
    } else {
        PORT_BUF(p)->getpos = NULL;
        PORT_BUF(p)->setpos = NULL;
        PORT_BUF(p)->flags = 0;
    }

    /* NB: DIR may be SCM_PORT_OUTPUT_TRANSIENT; in that case we don't
       register the buffer. */
    if (dir == SCM_PORT_OUTPUT) register_buffered_port(p);
    return SCM_OBJ(p);
}

/* deprecated */
ScmObj Scm_MakeBufferedPort(ScmClass *klass,
                            ScmObj name,
                            int dir,     /* direction */
                            int ownerp,
                            ScmPortBuffer *bufrec)
{
    return Scm_MakeBufferedPortFull(klass, name, dir, bufrec,
                                    (ownerp? SCM_PORT_OWNER : 0));
}

/* flushes the buffer, to make a room of cnt bytes.
   cnt == 0 means all the available data.   Note that, unless forcep == TRUE,
   this function only does "best effort" to make room, but doesn't
   guarantee to output cnt bytes.  */
static void bufport_flush(ScmPort *p, ScmSize cnt, int forcep)
{
    ScmSize cursiz = PORT_BUFFER_AVAIL(p);

    if (cursiz == 0) return;
    if (cnt <= 0)  { cnt = cursiz; }
    ScmSize nwrote = PORT_BUF(p)->flusher(p, cnt, forcep);
    if (nwrote < 0) {
        PORT_BUF(p)->current = PORT_BUF(p)->buffer; /* for safety */
        p->error = TRUE;
        /* TODO: can we raise an error here, or should we propagate
           it to the caller? */
        Scm_PortError(p, SCM_PORT_ERROR_OUTPUT,
                      "Couldn't flush port %S due to an error", p);
    }
    if (nwrote >= 0 && nwrote < cursiz) {
        memmove(PORT_BUF(p)->buffer, PORT_BUF(p)->buffer+nwrote,
                cursiz-nwrote);
        PORT_BUF(p)->current -= nwrote;
    } else {
        PORT_BUF(p)->current = PORT_BUF(p)->buffer;
    }
}

/* Writes siz bytes in src to the buffered port.  siz may be larger than
   the port's buffer.  Won't return until entire siz bytes are written. */
static void bufport_write(ScmPort *p, const char *src, ScmSize siz)
{
    do {
        ScmSize room = PORT_BUF(p)->end - PORT_BUF(p)->current;
        if (room >= siz) {
            memcpy(PORT_BUF(p)->current, src, siz);
            PORT_BUF(p)->current += siz;
            siz = 0;
        } else {
            memcpy(PORT_BUF(p)->current, src, room);
            PORT_BUF(p)->current += room;
            siz -= room;
            src += room;
            bufport_flush(p, 0, FALSE);
        }
    } while (siz != 0);
}

/* Fills the buffer.  Reads at least MIN bytes (unless it reaches EOF).
 * If ALLOW_LESS is true, however, we allow to return before the full
 * data is read.
 * Returns the number of bytes actually read, or 0 if EOF, or -1 if error.
 */
static ScmSize bufport_fill(ScmPort *p, ScmSize min, int allow_less)
{
    ScmSize cursiz = PORT_BUF(p)->end - PORT_BUF(p)->current;
    ScmSize nread = 0, toread;
    if (cursiz > 0) {
        memmove(PORT_BUF(p)->buffer, PORT_BUF(p)->current, cursiz);
        PORT_BUF(p)->current = PORT_BUF(p)->buffer;
        PORT_BUF(p)->end = PORT_BUF(p)->current + cursiz;
    } else {
        PORT_BUF(p)->current = PORT_BUF(p)->end = PORT_BUF(p)->buffer;
    }
    if (min <= 0) min = PORT_BUFFER_ROOM(p);
    if (PORT_BUFFER_MODE(p) != SCM_PORT_BUFFER_NONE) {
        toread = PORT_BUFFER_ROOM(p);
    } else {
        toread = min;
    }

    do {
        ScmSize r = PORT_BUF(p)->filler(p, toread-nread);
        if (r <= 0) break;
        nread += r;
        PORT_BUF(p)->end += r;
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
static ScmSize bufport_read(ScmPort *p, char *dst, ScmSize siz)
{
    ScmSize nread = 0;
    ScmSize avail = PORT_BUF(p)->end - PORT_BUF(p)->current;

    ScmSize req = MIN(siz, avail);
    if (req > 0) {
        memcpy(dst, PORT_BUF(p)->current, req);
        PORT_BUF(p)->current += req;
        nread += req;
        siz -= req;
        dst += req;
    }
    while (siz > 0) {
        /* We check data availability first, since we might already get
           some data from the remanings in the buffer, and it is enough
           if buffering mode is not full. */
        if (nread && (PORT_BUFFER_MODE(p) != SCM_PORT_BUFFER_FULL)) {
            if (PORT_BUF(p)->ready
                && PORT_BUF(p)->ready(p) == SCM_FD_WOULDBLOCK) {
                break;
            }
        }

        ScmSize req = MIN(siz, PORT_BUF(p)->size);
        ScmSize r = bufport_fill(p, req, TRUE);
        if (r <= 0) break; /* EOF or an error*/
        if (r >= siz) {
            memcpy(dst, PORT_BUF(p)->current, siz);
            PORT_BUF(p)->current += siz;
            nread += siz;
            break;
        } else {
            memcpy(dst, PORT_BUF(p)->current, r);
            PORT_BUF(p)->current += r;
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
    ScmWeakVector   *ports;
    ScmInternalMutex mutex;
} active_buffered_ports;

#define PORT_HASH(port)  \
    ((u_long)(((SCM_WORD(port)>>3) * 2654435761UL)>>16) % PORT_VECTOR_SIZE)

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
   (unless exiting is true, in that case we know nobody cares the active
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
        switch (PORT_BUFFER_MODE(port)) {
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

/* This small piece of data is kept in port->src.buf.data. */
typedef struct file_port_data_rec {
    int fd;
} file_port_data;

#define FILE_PORT_DATA(p) ((file_port_data*)(PORT_BUF(p)->data))

static ScmSize file_filler(ScmPort *p, ScmSize cnt)
{
    ScmSize nread = 0;
    int fd = FILE_PORT_DATA(p)->fd;
    char *datptr = PORT_BUF(p)->end;
    SCM_ASSERT(fd >= 0);
    while (nread == 0) {
        ScmSize r;
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

static ScmSize file_flusher(ScmPort *p, ScmSize cnt, int forcep)
{
    ScmSize nwrote = 0;
    ScmSize datsiz = PORT_BUFFER_AVAIL(p);
    int fd = FILE_PORT_DATA(p)->fd;
    char *datptr = PORT_BUF(p)->buffer;

    SCM_ASSERT(fd >= 0);
    while ((!forcep && nwrote == 0)
           || (forcep && nwrote < cnt)) {
        ScmSize r;
        errno = 0;
        SCM_SYSCALL(r, write(fd, datptr, datsiz-nwrote));
        if (r < 0) {
            if (PORT_BUFFER_SIGPIPE_SENSITIVE_P(p)) {
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
    int fd = FILE_PORT_DATA(p)->fd;
    if (fd >= 0) {
        /* If close() fails, the port's CLOSED flag isn't set and file_closer
           may be called again (probably via finalizer).  We don't want to call
           close() again and raise an error. */
        FILE_PORT_DATA(p)->fd = -1;
        if (close(fd) < 0) {
            Scm_SysError("close() failed on %S", SCM_OBJ(p));
        }
    }
}

static int file_ready(ScmPort *p)
{
    int fd = FILE_PORT_DATA(p)->fd;
    SCM_ASSERT(fd >= 0);
    return Scm_FdReady(fd, SCM_PORT_DIR(p));
}

static int file_filenum(ScmPort *p)
{
    return FILE_PORT_DATA(p)->fd;
}

static off_t file_seeker(ScmPort *p, off_t offset, int whence)
{
    return lseek(FILE_PORT_DATA(p)->fd, offset, whence);
}

/* Kludge: We should have better way */
static int file_buffered_port_p(ScmPort *p)
{
    return (PORT_BUF(p)->filenum == file_filenum);
}

static void file_buffered_port_set_fd(ScmPort *p, int fd)
{
    if (!file_buffered_port_p(p)) {
        Scm_Error("port is not directly conntect to fd: %S", p);
    }
    FILE_PORT_DATA(p)->fd = fd;
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
    /* In appending mode, we need to seek explicitly to the end to make
       port-tell returns the size of the file.
       We ignore the result of lseek here--it can fail if the opened file
       is a character device, for example, and it's ok.   Any other serious
       errors would be caught by later operations anyway.
    */
    if (flags & O_APPEND) (void)lseek(fd, 0, SEEK_END);

    file_port_data *data = SCM_NEW(file_port_data);
    data->fd = fd;

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
    bufrec.data = data;
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
    file_port_data *data = SCM_NEW(file_port_data);
    data->fd = fd;

    ScmPortBuffer bufrec;
    bufrec.buffer = NULL;
    bufrec.size = 0;
    bufrec.mode = bufmode;
    bufrec.filler = file_filler;
    bufrec.flusher =file_flusher;
    bufrec.closer = file_closer;
    bufrec.ready = file_ready;
    bufrec.filenum = file_filenum;
    bufrec.data = data;

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

ScmObj Scm_MakeInputStringPortFull(ScmString *str, ScmObj name,
                                   u_long flags)
{
    ScmPort *p = make_port(SCM_CLASS_PORT, name, SCM_PORT_INPUT, SCM_PORT_ISTR);
    ScmSmallInt size;
    const char *s = Scm_GetStringContent(str, &size, NULL, NULL);
    PORT_ISTR(p)->start = s;
    PORT_ISTR(p)->current = s;
    PORT_ISTR(p)->end = s + size;
    if (flags&SCM_PORT_STRING_PRIVATE) PORT_PRELOCK(p, Scm_VM());
    return SCM_OBJ(p);
}

/* deprecated */
ScmObj Scm_MakeInputStringPort(ScmString *str, int privatep)
{
    return Scm_MakeInputStringPortFull(str,
                                       SCM_MAKE_STR("(input string port)"),
                                       (privatep?SCM_PORT_STRING_PRIVATE:0));
}

ScmObj Scm_MakeOutputStringPortFull(ScmObj name, u_long flags)
{
    ScmPort *p = make_port(SCM_CLASS_PORT, name, SCM_PORT_OUTPUT, SCM_PORT_OSTR);
    Scm_DStringInit(PORT_OSTR(p));
    if (flags&SCM_PORT_STRING_PRIVATE) PORT_PRELOCK(p, Scm_VM());
    return SCM_OBJ(p);
}

/* deprecated */
ScmObj Scm_MakeOutputStringPort(int privatep)
{
    return Scm_MakeOutputStringPortFull(SCM_MAKE_STR("(output string port)"),
                                        (privatep?SCM_PORT_STRING_PRIVATE:0));
}

ScmObj Scm_GetOutputString(ScmPort *port, int flags)
{
    if (SCM_PORT_TYPE(port) != SCM_PORT_OSTR)
        Scm_Error("output string port required, but got %S", port);
    ScmVM *vm = Scm_VM();
    PORT_LOCK(port, vm);
    ScmObj r = Scm_DStringGet(PORT_OSTR(port), flags);
    PORT_UNLOCK(port);
    return r;
}

ScmObj Scm_GetOutputStringUnsafe(ScmPort *port, int flags)
{
    if (SCM_PORT_TYPE(port) != SCM_PORT_OSTR)
        Scm_Error("output string port required, but got %S", port);
    return Scm_DStringGet(PORT_OSTR(port), flags);
}

#if GAUCHE_API_VERSION < 1000
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
#endif /*GAUCHE_API_VERSION < 1000*/

static ScmObj get_remaining_input_string_aux(const char *s, ScmSize ssiz,
                                             const char *p, ScmSize psiz,
                                             int flags);

ScmObj Scm_GetRemainingInputString(ScmPort *port, int flags)
{
    if (SCM_PORT_TYPE(port) != SCM_PORT_ISTR)
        Scm_Error("input string port required, but got %S", port);
    /* NB: we don't need to lock the port, since the string body
       the port is pointing won't be changed. */
    const char *ep = PORT_ISTR(port)->end;
    const char *cp = PORT_ISTR(port)->current;
    /* Things gets complicated if there's an ungotten char or bytes.
       We want to share the string body whenever possible, so we
       first check the ungotten stuff matches the content of the
       buffer. */
    if (PORT_UNGOTTEN(port) != SCM_CHAR_INVALID) {
        char cbuf[SCM_CHAR_MAX_BYTES];
        int nbytes = SCM_CHAR_NBYTES(PORT_UNGOTTEN(port));
        SCM_CHAR_PUT(cbuf, PORT_UNGOTTEN(port));
        const char *sp = PORT_ISTR(port)->start;
        if (cp - sp >= nbytes
            && memcmp(cp - nbytes, cbuf, nbytes) == 0) {
            cp -= nbytes;       /* we can reuse buffer */
            return Scm_MakeString(cp, (int)(ep-cp), -1, flags);
        } else {
            /* we need to copy */
            return get_remaining_input_string_aux(cp, ep-cp,
                                                  cbuf, nbytes, flags);
        }
    } else if (port->scrcnt > 0) {
        const char *sp = PORT_ISTR(port)->start;
        if (cp - sp >= (int)port->scrcnt
            && memcmp(cp - port->scrcnt, PORT_SCRATCH(port), port->scrcnt) == 0) {
            cp -= port->scrcnt; /* we can reuse buffer */
            return Scm_MakeString(cp, (int)(ep-cp), -1, flags);
        } else {
            /* we need to copy */
            return get_remaining_input_string_aux(cp, ep-cp,
                                                  PORT_SCRATCH(port),
                                                  port->scrcnt, flags);
        }
    } else {
        return Scm_MakeString(cp, (int)(ep-cp), -1, flags);
    }
}

static ScmObj get_remaining_input_string_aux(const char *s, ScmSize ssiz,
                                             const char *p, ScmSize psiz,
                                             int flags)
{
    char *b = SCM_NEW_ATOMIC2(char *, psiz+ssiz+1);
    memcpy(b, p, psiz);
    memcpy(b+psiz, s, ssiz);
    b[psiz+ssiz] = '\0';
    return Scm_MakeString(b, psiz+ssiz, -1, flags);
}

#if GAUCHE_API_VERSION < 1000
/* TRANSIENT: Pre-0.9 Compatibility routine.  Kept for the binary compatibility.
   Will be removed on 1.0 */
ScmObj Scm__GetRemainingInputStringCompat(ScmPort *port)
{
    return Scm_GetRemainingInputString(port, 0);
}
#endif /*GAUCHE_API_VERSION < 1000*/

/*===============================================================
 * Procedural port
 */

/* To create a procedural port, fill in the ScmPortVTable function
   pointers and pass it to Scm_MakeVirtualPort.  You don't need to
   provide all the functions; put NULL if you think you don't
   provide the functionality.
*/

/* default dummy procedures */
static int null_getb(ScmPort *dummy SCM_UNUSED)
{
    return SCM_CHAR_INVALID;
}

static int null_getc(ScmPort *dummy SCM_UNUSED)
{
    return SCM_CHAR_INVALID;
}

static ScmSize null_getz(char *buf SCM_UNUSED,
                         ScmSize buflen SCM_UNUSED,
                         ScmPort *dummy SCM_UNUSED)
{
    return 0;
}

static int null_ready(ScmPort *dummy SCM_UNUSED, int charp SCM_UNUSED)
{
    return TRUE;
}

static void null_putb(ScmByte b SCM_UNUSED, ScmPort *dummy SCM_UNUSED)
{
}

static void null_putc(ScmChar c SCM_UNUSED, ScmPort *dummy SCM_UNUSED)
{
}

static void null_putz(const char *str SCM_UNUSED,
                      ScmSize len SCM_UNUSED,
                      ScmPort *dummy SCM_UNUSED)
{
}

static void null_puts(ScmString *s SCM_UNUSED, ScmPort *dummy SCM_UNUSED)
{
}

static void null_flush(ScmPort *dummy SCM_UNUSED)
{
}

ScmObj Scm_MakeVirtualPortFull(ScmClass *klass, ScmObj name,
                               int direction,
                               const ScmPortVTable *vtable,
                               u_long flags SCM_UNUSED)
{
    ScmPort *p = make_port(klass, name, direction, SCM_PORT_PROC);

    /* Initialize default values */
    PORT_VT(p)->Getb = null_getb;
    PORT_VT(p)->Getc = null_getc;
    PORT_VT(p)->Getz = null_getz;
    PORT_VT(p)->Ready = null_ready;
    PORT_VT(p)->Putb = null_putb;
    PORT_VT(p)->Putc = null_putc;
    PORT_VT(p)->Putz = null_putz;
    PORT_VT(p)->Puts = null_puts;
    PORT_VT(p)->Flush = null_flush;
    PORT_VT(p)->Close = NULL;
    PORT_VT(p)->Seek = NULL;
    PORT_VT(p)->data = NULL;
    PORT_VT(p)->GetPos = NULL;
    PORT_VT(p)->SetPos = NULL;
    PORT_VT(p)->flags = 0;

    if (vtable->Getb)  PORT_VT(p)->Getb   = vtable->Getb;
    if (vtable->Getc)  PORT_VT(p)->Getc   = vtable->Getc;
    if (vtable->Getz)  PORT_VT(p)->Getz   = vtable->Getz;
    if (vtable->Ready) PORT_VT(p)->Ready  = vtable->Ready;
    if (vtable->Putb)  PORT_VT(p)->Putb   = vtable->Putb;
    if (vtable->Putc)  PORT_VT(p)->Putc   = vtable->Putc;
    if (vtable->Putz)  PORT_VT(p)->Putz   = vtable->Putz;
    if (vtable->Puts)  PORT_VT(p)->Puts   = vtable->Puts;
    if (vtable->Flush) PORT_VT(p)->Flush  = vtable->Flush;
    if (vtable->Close) PORT_VT(p)->Close  = vtable->Close;
    if (vtable->Seek)  PORT_VT(p)->Seek   = vtable->Seek;
    PORT_VT(p)->data = vtable->data;

    if (flags & SCM_PORT_WITH_POSITION) {
        if (vtable->GetPos) PORT_VT(p)->GetPos = vtable->GetPos;
        if (vtable->SetPos) PORT_VT(p)->SetPos = vtable->SetPos;
        PORT_VT(p)->flags = vtable->flags;
    }

    return SCM_OBJ(p);
}

/* deprecated */
ScmObj Scm_MakeVirtualPort(ScmClass *klass,
                           int direction,
                           const ScmPortVTable *vtable)
{
    return Scm_MakeVirtualPortFull(klass, SCM_FALSE, direction, vtable, 0);
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

static ScmSize coding_filler(ScmPort *p, ScmSize cnt)
{
    ScmSize nread = 0;
    coding_port_data *data = (coding_port_data*)PORT_BUF(p)->data;
    char *datptr = PORT_BUF(p)->end;

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
    coding_port_data *data = (coding_port_data*)PORT_BUF(p)->data;
    if (data->source) {
        Scm_ClosePort(data->source);
        data->source = NULL;
    }
}

static int coding_ready(ScmPort *p)
{
    coding_port_data *data = (coding_port_data*)PORT_BUF(p)->data;
    if (data->source == NULL) return TRUE;
    if (data->state == CODING_PORT_RECOGNIZED) {
        return SCM_FD_READY;
    } else {
        return Scm_ByteReadyUnsafe(p);
    }
}

static int coding_filenum(ScmPort *p)
{
    coding_port_data *data = (coding_port_data*)PORT_BUF(p)->data;
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
    if (sizeof(ScmPort) < sizeof(ScmPortImpl)) {
        fprintf(stderr,
                "sizeof(ScmPort) [%"PRIdPTR"] is smaller than "
                "sizeof(ScmPortImpl) [%"PRIdPTR"]\n",
                SCM_WORD(sizeof(ScmPort)), SCM_WORD(sizeof(ScmPortImpl)));
        Scm_Panic("Implementation error.  Exitting.");
    }

    (void)SCM_INTERNAL_MUTEX_INIT(active_buffered_ports.mutex);
    active_buffered_ports.ports = SCM_WEAK_VECTOR(Scm_MakeWeakVector(PORT_VECTOR_SIZE));

    Scm_InitStaticClass(&Scm_PortClass, "<port>",
                        Scm_GaucheModule(), port_slots, 0);
    Scm_InitStaticClass(&Scm_CodingAwarePortClass, "<coding-aware-port>",
                        Scm_GaucheModule(), port_slots, 0);

    /* This must be done before *any* port is created. */
    readerLexicalMode =
        Scm_BindPrimitiveParameter(Scm_GaucheModule(), "reader-lexical-mode",
                                   SCM_OBJ(SCM_SYM_PERMISSIVE), 0);

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

static void prepare_console_and_stdio(const char *devname, int flags,
                                      DWORD nStdHandle, int fd,
                                      int *initialized)
{
    HANDLE h;
    SECURITY_ATTRIBUTES sa;
    int temp_fd = -1;
    int err = 0;
#define ERR_CREATEFILE 1
#define ERR_OPEN_OSFHANDLE 2
#define ERR_DUP2 3
#define ERR_SETSTDHANDLE 4

    SCM_INTERNAL_MUTEX_LOCK(win_console_mutex);
    if (!win_console_created) {
        win_console_created = TRUE;
        AllocConsole();
    }
    if (!(*initialized)) {
        /* NB: Double fault will be caught in the error handling
           mechanism, so we don't need to worry it here. */
        sa.nLength = sizeof(SECURITY_ATTRIBUTES);
        sa.lpSecurityDescriptor = NULL;
        sa.bInheritHandle = TRUE;
        h = CreateFile(SCM_MBS2WCS(devname),
                       GENERIC_READ | GENERIC_WRITE,
                       FILE_SHARE_READ | FILE_SHARE_WRITE,
                       &sa, OPEN_EXISTING, 0, NULL);
        if (h == INVALID_HANDLE_VALUE) {
            err = ERR_CREATEFILE;
        } else if ((temp_fd = _open_osfhandle((intptr_t)h, flags)) < 0) {
            err = ERR_OPEN_OSFHANDLE;
        } else if (_dup2(temp_fd, fd) < 0) {
            err = ERR_DUP2;
        } else if (SetStdHandle(nStdHandle, (HANDLE)_get_osfhandle(fd)) == 0) {
            err = ERR_SETSTDHANDLE;
        } else {
            *initialized = TRUE;
        }
    }
    SCM_INTERNAL_MUTEX_UNLOCK(win_console_mutex);

    if (temp_fd >= 0) close(temp_fd);
    switch (err) {
    case ERR_CREATEFILE:
        Scm_SysError("CreateFile(%s) failed", devname);
        break;                  /* Dummy */
    case ERR_OPEN_OSFHANDLE:
        CloseHandle(h);
        Scm_SysError("_open_osfhandle failed (fd = %d)", fd);
        break;                  /* Dummy */
    case ERR_DUP2:
        CloseHandle(h);
        Scm_SysError("dup2(%d) failed (osf_handle)", fd);
        break;                  /* Dummy */
    case ERR_SETSTDHANDLE:
        CloseHandle(h);
        Scm_SysError("SetStdHandle(%d) failed (fd = %d)", (int)nStdHandle, fd);
        break;                  /* Dummy */
    }

#undef ERR_CREATEFILE
#undef ERR_OPEN_OSFHANDLE
#undef ERR_DUP2
#undef ERR_SETSTDHANDLE
}

static ScmSize trapper_filler(ScmPort *p, ScmSize cnt)
{
    static int initialized = FALSE;
    prepare_console_and_stdio("CONIN$",  _O_RDONLY | _O_BINARY,
                              STD_INPUT_HANDLE,  0, &initialized);
    return file_filler(p, cnt);
}

static ScmSize trapper_flusher1(ScmPort *p, ScmSize cnt, int forcep)
{
    static int initialized = FALSE;
    prepare_console_and_stdio("CONOUT$", _O_WRONLY | _O_BINARY,
                              STD_OUTPUT_HANDLE, 1, &initialized);
    return file_flusher(p, cnt, forcep);
}

static ScmSize trapper_flusher2(ScmPort *p, ScmSize cnt, int forcep)
{
    static int initialized = FALSE;
    prepare_console_and_stdio("CONOUT$", _O_WRONLY | _O_BINARY,
                              STD_ERROR_HANDLE,  2, &initialized);
    return file_flusher(p, cnt, forcep);
}

static ScmObj make_trapper_port(ScmObj name, int direction,
                                int fd, int bufmode)
{
    ScmPortBuffer bufrec;

    bufrec.buffer = NULL;
    bufrec.size = 0;
    bufrec.mode = bufmode;
    if (fd == 0) {
        bufrec.filler = trapper_filler;
    } else {
        bufrec.filler = NULL;
    }
    if (fd == 1) {
        bufrec.flusher = trapper_flusher1;
    } else if (fd == 2) {
        bufrec.flusher = trapper_flusher2;
    } else {
        bufrec.flusher = NULL;
    }
    bufrec.closer = file_closer;
    bufrec.ready = file_ready;
    bufrec.filenum = file_filenum;
    file_port_data *data = SCM_NEW(file_port_data);
    data->fd = fd;
    bufrec.data = data;
    bufrec.seeker = NULL;
    ScmObj p = Scm_MakeBufferedPort(SCM_CLASS_PORT, name, direction, TRUE,
                                    &bufrec);
    return p;
}

/* This is supposed to be called from application main(), before any
   threads are created.  We don't mutex here. */
void Scm__SetupPortsForWindows(int has_console)
{
    if (!has_console) {
        static int initialized = FALSE;
        static volatile ScmObj orig_stdin  = SCM_FALSE;
        static volatile ScmObj orig_stdout = SCM_FALSE;
        static volatile ScmObj orig_stderr = SCM_FALSE;
        if (!initialized) {
            initialized = TRUE;
            SCM_INTERNAL_MUTEX_INIT(win_console_mutex);
            /* Original scm_stdout and scm_stderr holds ports that are
               connected to fd=0 and fd=1, respectively.  Losing reference
               to those ports will eventually lead to close those fds (when
               those ports are GC-ed), causing complications in the code
               that assumes fds 0, 1 and 2 are reserved.  To make things
               easier, we just save the original ports. */
            orig_stdin  = scm_stdin;
            orig_stdout = scm_stdout;
            orig_stderr = scm_stderr;
            /* We know the destination is allocated Windows Console, so we
               just use fixed buffered modes. */
            scm_stdin  = make_trapper_port(SCM_MAKE_STR("(standard input)"),
                                           SCM_PORT_INPUT, 0,
                                           SCM_PORT_BUFFER_FULL);
            scm_stdout = make_trapper_port(SCM_MAKE_STR("(standard output)"),
                                           SCM_PORT_OUTPUT, 1,
                                           SCM_PORT_BUFFER_LINE);
            scm_stderr = make_trapper_port(SCM_MAKE_STR("(standard error output)"),
                                           SCM_PORT_OUTPUT, 2,
                                           SCM_PORT_BUFFER_NONE);
            Scm_VM()->curin  = SCM_PORT(scm_stdin);
            Scm_VM()->curout = SCM_PORT(scm_stdout);
            Scm_VM()->curerr = SCM_PORT(scm_stderr);
        }
        (void)orig_stdin;  /* suppress unused var warning */
        (void)orig_stdout; /* suppress unused var warning */
        (void)orig_stderr; /* suppress unused var warning */
    }
}
#endif /*defined(GAUCHE_WINDOWS)*/
