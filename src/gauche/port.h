/*
 * gauche/port.h - Port API
 *
 *   Copyright (c) 2000-2014  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_PORT_H
#define GAUCHE_PORT_H

/* Port is the Scheme way of I/O abstraction.  R5RS's definition of
 * of the port is very simple and straightforward.   Practical
 * applications, however, require far more detailed control over
 * the I/O channel, as well as the reasonable performance.
 *
 * Current implementation is a bit messy, trying to achieve both
 * performance and feature requirements.  In the core API level,
 * ports are categorized in one of three types: file ports, string
 * ports and procedural ports.   A port may be an input port or
 * an output port.   A port may handle byte (binary) streams, as
 * well as character streams.  Some port may interchange byte (binary)
 * I/O versus character I/O, while some may signal an error if you
 * mix those operations.
 *
 * You shouldn't rely on the underlying port implementation, for
 * it is likely to be changed in future.  There are enough macros
 * and API functions provided to use and extend the port mechanism.
 * See also ext/vport for the way to extend the port from Scheme.
 *
 * Most public port APIs locks the given port to ensure it won't
 * interfere with other threads.  Some basic APIs have corresponding
 * "Unsafe" version (e.g. Scm_Putc() vs Scm_PutcUnsafe()), which
 * assumes the caller already has the lock.
 */

/*================================================================
 * Port structures & flags
 */

/* Substructures */

/* The alternative of FILE* structure, used by buffered (file) port.
   The members are owned by the port, and client shouldn't change the
   elements.  You can create your own custom buffered port by using
   Scm_MakeBufferedPort() --- with it, you pass ScmPortBuffer with
   the function pointers filled in, which is copied to the port's
   internal ScmPortBuffer structure.
   See port.c for the details of function pointers. */

typedef struct ScmPortBufferRec {
    char *buffer;       /* ptr to the buffer area */
    char *current;      /* current buffer position */
    char *end;          /* the end of the current valid data */
    int  size;          /* buffer size */
    int  mode;          /* buffering mode (ScmPortBufferMode) & SIGPIPE flag */
    int  (*filler)(ScmPort *p, int min);
    int  (*flusher)(ScmPort *p, int cnt, int forcep);
    void (*closer)(ScmPort *p);
    int  (*ready)(ScmPort *p);
    int  (*filenum)(ScmPort *p);
    off_t (*seeker)(ScmPort *p, off_t offset, int whence);
    void *data;
} ScmPortBuffer;

/* For input buffered port, returns the size of room that can be filled
   by the filler */
#define SCM_PORT_BUFFER_ROOM(p) \
    (int)((p)->src.buf.buffer+(p)->src.buf.size-(p)->src.buf.end)

/* For output buffered port, returns the size of available data that can
   be flushed by the flusher */
#define SCM_PORT_BUFFER_AVAIL(p) \
    (int)((p)->src.buf.current-(p)->src.buf.buffer)

/* The funtion table of procedural port. */

typedef struct ScmPortVTableRec {
    int    (*Getb)(ScmPort *p);
    int    (*Getc)(ScmPort *p);
    int    (*Getz)(char *buf, int buflen, ScmPort *p);
    int    (*Ready)(ScmPort *p, int charp);
    void   (*Putb)(ScmByte b, ScmPort *p);
    void   (*Putc)(ScmChar c, ScmPort *p);
    void   (*Putz)(const char *buf, int size, ScmPort *p);
    void   (*Puts)(ScmString *s, ScmPort *p);
    void   (*Flush)(ScmPort *p);
    void   (*Close)(ScmPort *p);
    off_t  (*Seek)(ScmPort *p, off_t off, int whence);
    void    *data;
} ScmPortVTable;

/* The main port structure.
 * Regardless of the port type, the port structure caches at most
 * one character, in order to realize `peek-char' (Scheme) or `Ungetc' (C)
 * operation.   'scratch', 'scrcnt', and 'ungotten' fields are used for
 * that purpose, and outside routine shouldn't touch these fields.
 * See portapi.c for the detailed semantics.
 */

struct ScmPortRec {
    SCM_INSTANCE_HEADER;
    u_int direction : 2;        /* SCM_PORT_INPUT or SCM_PORT_OUTPUT.
                                   There may be I/O port in future. */
    u_int type      : 2;        /* SCM_PORT_{FILE|ISTR|OSTR|PROC} */
    u_int scrcnt    : 3;        /* # of bytes in the scratch buffer */

    u_int ownerp    : 1;        /* TRUE if this port owns underlying
                                   file pointer */
    u_int closed    : 1;        /* TRUE if this port is closed */
    u_int error     : 1;        /* Error has been occurred */

    u_int flags     : 5;        /* see ScmPortFlags below */

    char scratch[SCM_CHAR_MAX_BYTES]; /* incomplete buffer */

    ScmChar ungotten;           /* ungotten character.
                                   SCM_CHAR_INVALID if empty. */
    ScmObj name;                /* port's name.  Can be any Scheme object. */

    ScmInternalFastlock lock;   /* for port mutex */
    ScmVM *lockOwner;           /* for port mutex; owner of the lock */
    int lockCount;              /* for port mutex; # of recursive locks */

    ScmObj recursiveContext;    /* used internally */

    /* Input counters.  these doesn't take account of ungetting and
       seeking: Ungetting doesn't affect those counters (you can think
       that ungetting are handled above the counting layer).
       Seeking invalidates counters; if you seek, the values of the counters
       become bogus.
       We don't have character counter, since it is difficult to track
       (read-line uses byte read; see Scm_ReadLine in portapi.c).
     */
    u_long line;                /* line counter */
    u_long bytes;               /* byte counter */

    /* The source or the sink of the port. */
    union {
        ScmPortBuffer buf;      /* buffered port */
        struct {
            const char *start;
            const char *current;
            const char *end;
        } istr;                 /* input string port */
        ScmDString ostr;        /* output string port */
        ScmPortVTable vt;       /* virtual port */
    } src;

    /* Port attibutes.
     * NB: Before we release 0.9.4, we might merge this into port->data and/or
     * port->name.
     */
    ScmObj attrs;               /* port attibutes.  use Scm_PortAttr* API to
                                   access. */
};

/* Port direction.  Bidirectional port is not supported yet. */
enum ScmPortDirection {
    SCM_PORT_INPUT = 1,
    SCM_PORT_OUTPUT = 2
};

/* Port buffering mode */
enum ScmPortBufferMode {
    SCM_PORT_BUFFER_FULL = 0,       /* full buffering */
    SCM_PORT_BUFFER_LINE = 1,       /* flush the buffer for each line */
    SCM_PORT_BUFFER_NONE = 2,       /* flush the buffer for every output */

    SCM_PORT_BUFFER_MODE_MASK = 0x07 /* for future extension */
};

/* If this flag is set in `mode' member of ScmPortBuffer, SIGPIPE causes
   the process to terminate.  By default this flag is off except stdin and
   stdout ports; it is to emulate default Unix behavior.  On Windows platform
   this flag is ignored, for we don't have SIGPIPE.  */
#define SCM_PORT_BUFFER_SIGPIPE_SENSITIVE  (1L<<8)

/* Port types.  The type is also represented by a port's class, but
   C routine can dispatch quicker using these flags.  User code
   doesn't need to care about these. */
enum ScmPortType {
    SCM_PORT_FILE,              /* file (buffered) port */
    SCM_PORT_ISTR,              /* input string port */
    SCM_PORT_OSTR,              /* output string port */
    SCM_PORT_PROC               /* virtual port */
};

/* Return value from Scm_FdReady */
enum ScmFdReadyResult {
    SCM_FD_WOULDBLOCK,
    SCM_FD_READY,
    SCM_FD_UNKNOWN
};

/* Other flags used internally */
/* NB: The first two flags only matter when port->recursiveContext is set,
   and they're transient by nature.  See write.c for the details. */
enum ScmPortFlags {
    SCM_PORT_WRITESS = (1L<<0), /* we're write/ss mode.  */
    SCM_PORT_WALKING = (1L<<1), /* indicates we're currently in 'walk' pass
                                   of two-pass writing. */
    SCM_PORT_PRIVATE = (1L<<2), /* this port is for 'private' use within
                                   a thread, so never need to be locked. */
    SCM_PORT_CASE_FOLD = (1L<<3) /* read from or write to this port should
                                    be case folding. */
};

#if 0 /* not implemented */
/* Incomplete character handling policy.
   When Scm_Getc encounters a byte sequence that doesn't consist a valid
   multibyte character, it may take one of the following actions,
   according to the port's icpolicy field. */
enum ScmPortICPolicy {
    SCM_PORT_IC_ERROR,          /* signal an error */
    SCM_PORT_IC_IGNORE,         /* ignore bytes until Getc finds a
                                   valid multibyte character */
    SCM_PORT_IC_REPLACE,        /* replace invalid byte to a designated
                                   character. */
};
#endif

/*================================================================
 * Generic operations
 */

/* Predicates & accessors */
#define SCM_PORTP(obj)          (SCM_ISA(obj, SCM_CLASS_PORT))

#define SCM_PORT(obj)           ((ScmPort *)(obj))
#define SCM_PORT_TYPE(obj)      (SCM_PORT(obj)->type)
#define SCM_PORT_DIR(obj)       (SCM_PORT(obj)->direction)
#define SCM_PORT_FLAGS(obj)     (SCM_PORT(obj)->flags)
#define SCM_PORT_ICPOLICY(obj)  (SCM_PORT(obj)->icpolicy)

#define SCM_PORT_CASE_FOLD(obj) (SCM_PORT_FLAGS(obj)&SCM_PORT_CASE_FOLD)

#define SCM_PORT_CLOSED_P(obj)  (SCM_PORT(obj)->closed)
#define SCM_PORT_OWNER_P(obj)   (SCM_PORT(obj)->ownerp)
#define SCM_PORT_ERROR_OCCURRED_P(obj) (SCM_PORT(obj)->error)

#define SCM_PORT_BUFFER_DATA(obj)  (SCM_PORT(obj)->src.buf.data)
#define SCM_PORT_VIRTUAL_DATA(obj) (SCM_PORT(obj)->src.vt.data)

#define SCM_IPORTP(obj)  (SCM_PORTP(obj)&&(SCM_PORT_DIR(obj)&SCM_PORT_INPUT))
#define SCM_OPORTP(obj)  (SCM_PORTP(obj)&&(SCM_PORT_DIR(obj)&SCM_PORT_OUTPUT))

#define SCM_PUTB(b, p)     Scm_Putb(b, SCM_PORT(p))
#define SCM_PUTC(c, p)     Scm_Putc(c, SCM_PORT(p))
#define SCM_PUTZ(s, l, p)  Scm_Putz(s, l, SCM_PORT(p))
#define SCM_PUTS(s, p)     Scm_Puts(SCM_STRING(s), SCM_PORT(p))
#define SCM_FLUSH(p)       Scm_Flush(SCM_PORT(p))
#define SCM_PUTNL(p)       SCM_PUTC('\n', p)

#define SCM_UNGETC(c, port) Scm_Ungetc(c, SCM_PORT(port))
#define SCM_GETB(b, p)     (b = Scm_Getb(SCM_PORT(p)))
#define SCM_GETC(c, p)     (c = Scm_Getc(SCM_PORT(p)))

SCM_CLASS_DECL(Scm_PortClass);
#define SCM_CLASS_PORT                (&Scm_PortClass)

SCM_CLASS_DECL(Scm_CodingAwarePortClass);
#define SCM_CLASS_CODING_AWARE_PORT   (&Scm_CodingAwarePortClass)

SCM_CLASS_DECL(Scm_LimitedLengthPortClass);
#define SCM_CLASS_LIMITED_LENGTH_PORT (&Scm_LimitedLengthPortClass)

SCM_CLASS_DECL(Scm_WriterPortClass);
#define SCM_CLASS_WRITER_PORT         (&Scm_WriterPortClass)


/* Conversion between Scheme keyword and ScmPortBufferMode enums */
SCM_EXTERN ScmObj Scm_GetPortBufferingModeAsKeyword(ScmPort *port);
SCM_EXTERN int    Scm_BufferingModeAsKeyword(ScmObj flag,
                                             int direction,
                                             int fallback);

SCM_EXTERN ScmObj Scm_GetBufferingMode(ScmPort *port); /* obsoleted */
SCM_EXTERN int    Scm_BufferingMode(ScmObj flag,       /* obsoleted */
                                    int direction,
                                    int fallback);

SCM_EXTERN int    Scm_GetPortBufferingMode(ScmPort *port);
SCM_EXTERN void   Scm_SetPortBufferingMode(ScmPort *port, int mode);
SCM_EXTERN int    Scm_GetPortBufferSigpipeSensitive(ScmPort *port);
SCM_EXTERN void   Scm_SetPortBufferSigpipeSensitive(ScmPort *port, int sensitive);

SCM_EXTERN void   Scm_FlushAllPorts(int exitting);

SCM_EXTERN ScmObj Scm_PortName(ScmPort *port);
SCM_EXTERN int    Scm_PortLine(ScmPort *port);
SCM_EXTERN ScmObj Scm_PortSeek(ScmPort *port, ScmObj off, int whence);
SCM_EXTERN ScmObj Scm_PortSeekUnsafe(ScmPort *port, ScmObj off, int whence);
SCM_EXTERN int    Scm_PortFileNo(ScmPort *port);
SCM_EXTERN void   Scm_PortFdDup(ScmPort *dst, ScmPort *src);
SCM_EXTERN int    Scm_FdReady(int fd, int dir);
SCM_EXTERN int    Scm_ByteReady(ScmPort *port);
SCM_EXTERN int    Scm_ByteReadyUnsafe(ScmPort *port);
SCM_EXTERN int    Scm_CharReady(ScmPort *port);
SCM_EXTERN int    Scm_CharReadyUnsafe(ScmPort *port);

SCM_EXTERN void   Scm_ClosePort(ScmPort *port);

SCM_EXTERN ScmObj Scm_VMWithPortLocking(ScmPort *port,
                                        ScmObj closure);

SCM_EXTERN ScmObj Scm_PortAttrGet(ScmPort *port, ScmObj key,
                                  ScmObj fallback);
SCM_EXTERN ScmObj Scm_PortAttrGetUnsafe(ScmPort *port, ScmObj key,
                                        ScmObj fallback);
SCM_EXTERN ScmObj Scm_PortAttrSet(ScmPort *port, ScmObj key, ScmObj val);
SCM_EXTERN ScmObj Scm_PortAttrSetUnsafe(ScmPort *port, ScmObj key, ScmObj val);
SCM_EXTERN ScmObj Scm_PortAttrCreate(ScmPort *port, ScmObj key, ScmObj get, ScmObj set);
SCM_EXTERN ScmObj Scm_PortAttrCreateUnsafe(ScmPort *port, ScmObj key, ScmObj get, ScmObj set);
SCM_EXTERN ScmObj Scm_PortAttrDelete(ScmPort *port, ScmObj key);
SCM_EXTERN ScmObj Scm_PortAttrDeleteUnsafe(ScmPort *port, ScmObj key);
SCM_EXTERN ScmObj Scm_PortAttrs(ScmPort *port);
SCM_EXTERN ScmObj Scm_PortAttrsUnsafe(ScmPort *port);

SCM_EXTERN void   Scm_Putb(ScmByte b, ScmPort *port);
SCM_EXTERN void   Scm_Putc(ScmChar c, ScmPort *port);
SCM_EXTERN void   Scm_Puts(ScmString *s, ScmPort *port);
SCM_EXTERN void   Scm_Putz(const char *s, int len, ScmPort *port);
SCM_EXTERN void   Scm_Flush(ScmPort *port);

SCM_EXTERN void   Scm_PutbUnsafe(ScmByte b, ScmPort *port);
SCM_EXTERN void   Scm_PutcUnsafe(ScmChar c, ScmPort *port);
SCM_EXTERN void   Scm_PutsUnsafe(ScmString *s, ScmPort *port);
SCM_EXTERN void   Scm_PutzUnsafe(const char *s, int len, ScmPort *port);
SCM_EXTERN void   Scm_FlushUnsafe(ScmPort *port);

SCM_EXTERN void   Scm_Ungetc(ScmChar ch, ScmPort *port);
SCM_EXTERN void   Scm_Ungetb(int b, ScmPort *port);
SCM_EXTERN int    Scm_Getb(ScmPort *port);
SCM_EXTERN int    Scm_Getc(ScmPort *port);
SCM_EXTERN int    Scm_Getz(char *buf, int buflen, ScmPort *port);
SCM_EXTERN ScmChar Scm_Peekc(ScmPort *port);
SCM_EXTERN int    Scm_Peekb(ScmPort *port);

SCM_EXTERN void   Scm_UngetcUnsafe(ScmChar ch, ScmPort *port);
SCM_EXTERN void   Scm_UngetbUnsafe(int b, ScmPort *port);
SCM_EXTERN int    Scm_GetbUnsafe(ScmPort *port);
SCM_EXTERN int    Scm_GetcUnsafe(ScmPort *port);
SCM_EXTERN int    Scm_GetzUnsafe(char *buf, int buflen, ScmPort *port);
SCM_EXTERN ScmChar Scm_PeekcUnsafe(ScmPort *port);
SCM_EXTERN int    Scm_PeekbUnsafe(ScmPort *port);

SCM_EXTERN ScmObj Scm_ReadLine(ScmPort *port);
SCM_EXTERN ScmObj Scm_ReadLineUnsafe(ScmPort *port);

#if 0
#define SCM_PORT_CURIN  (1<<0)
#define SCM_PORT_CUROUT (1<<1)
#define SCM_PORT_CURERR (1<<2)

SCM_EXTERN ScmObj Scm_WithPort(ScmPort *port[], ScmObj thunk,
                               int mask, int closep);
#endif

/*================================================================
 * File ports
 */

SCM_EXTERN ScmObj Scm_OpenFilePort(const char *path, int flags,
                                   int buffering, int perm);

SCM_EXTERN ScmObj Scm_Stdin(void);
SCM_EXTERN ScmObj Scm_Stdout(void);
SCM_EXTERN ScmObj Scm_Stderr(void);

SCM_EXTERN ScmObj Scm_SetStdin(ScmPort *port);
SCM_EXTERN ScmObj Scm_SetStdout(ScmPort *port);
SCM_EXTERN ScmObj Scm_SetStderr(ScmPort *port);

#define SCM_CURIN    SCM_VM_CURRENT_INPUT_PORT(Scm_VM())
#define SCM_CUROUT   SCM_VM_CURRENT_OUTPUT_PORT(Scm_VM())
#define SCM_CURERR   SCM_VM_CURRENT_ERROR_PORT(Scm_VM())

SCM_EXTERN ScmObj Scm_SetCurrentInputPort(ScmPort *port);
SCM_EXTERN ScmObj Scm_SetCurrentOutputPort(ScmPort *port);
SCM_EXTERN ScmObj Scm_SetCurrentErrorPort(ScmPort *port);

/*================================================================
 * String ports
 */

SCM_EXTERN ScmObj Scm_MakeInputStringPort(ScmString *str, int privatep);
SCM_EXTERN ScmObj Scm_MakeOutputStringPort(int privatep);

#if !defined(GAUCHE_API_PRE_0_9)
SCM_EXTERN ScmObj Scm_GetOutputString(ScmPort *port, int flags);
SCM_EXTERN ScmObj Scm_GetOutputStringUnsafe(ScmPort *port, int flags);
SCM_EXTERN ScmObj Scm_GetRemainingInputString(ScmPort *port, int flags);

#else  /* GAUCHE_API_PRE_0_9 */
#define Scm_GetOutputString(p) Scm__GetOutputStringCompat(p)
#define Scm_GetOutputStringUnsafe(p) Scm__GetOutputStringUnsafeCompat(p)
#define Scm_GetRemainingInputString(p) Scm__GetRemainingInputStringCompat(p)

/* For backward compatibility */
SCM_EXTERN ScmObj Scm__GetOutputStringCompat(ScmPort *port);
SCM_EXTERN ScmObj Scm__GetOutputStringUnsafeCompat(ScmPort *port);
SCM_EXTERN ScmObj Scm__GetRemainingInputStringCompat(ScmPort *port);
#endif /* GAUCHE_API_PRE_0_9 */

/*================================================================
 * Other type of ports
 */

SCM_EXTERN ScmObj Scm_MakeVirtualPort(ScmClass *klass,
                                      int direction,
                                      const ScmPortVTable *vtable);
SCM_EXTERN ScmObj Scm_MakeBufferedPort(ScmClass *klass,
                                       ScmObj name, int direction,
                                       int ownerp,
                                       ScmPortBuffer *bufrec);
SCM_EXTERN ScmObj Scm_MakePortWithFd(ScmObj name,
                                     int direction,
                                     int fd,
                                     int bufmode,
                                     int ownerp);
SCM_EXTERN ScmObj Scm_MakeCodingAwarePort(ScmPort *iport);
SCM_EXTERN ScmObj Scm_MakeWriterPort(ScmPort *port, ScmObj context);

#endif /*GAUCHE_PORT_H*/

