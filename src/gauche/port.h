/*
 * gauche/port.h - Port API
 *
 *   Copyright (c) 2000-2022  Shiro Kawai  <shiro@acm.org>
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
 * an output port.   A port may handle byte (binary) streams,
 * as well as character streams.  Some port may
 * interchange byte (binary) I/O versus character I/O, while some
 * may signal an error if you mix those operations.
 *
 * You shouldn't rely on the underlying port implementation, for
 * it is likely to be changed in future.  There are enough macros
 * and API functions provided to use and extend the port mechanism.
 * See also ext/vport for the way to extend the port from Scheme.
 *
 * Most public port APIs locks the given port to ensure it won't
 * interfere with other threads.  Some basic APIs have corresponding
 * "Unsafe" version (e.g. Scm_Putc() vs Scm_PutcUnsafe()), which
 * assumes the caller already has the lock; but the base version
 * won't do extra locking when the calling thread alreay hold the lock,
 * so there's no reason for general code to call unsafe verson directly.
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
    ScmSize size;       /* buffer size */
    int  mode;          /* buffering mode (ScmPortBufferMode) & SIGPIPE flag */
    ScmSize (*filler)(ScmPort *p, ScmSize min);
    ScmSize (*flusher)(ScmPort *p, ScmSize cnt, int forcep);
    void (*closer)(ScmPort *p);
    int  (*ready)(ScmPort *p);
    int  (*filenum)(ScmPort *p);
    off_t (*seeker)(ScmPort *p, off_t offset, int whence);
    void *data;

    /* The following fields are added in 0.9.10.  When you pass ScmPortBuffer
       to the constructor, those fields are looked at only when you specify
       SCM_PORT_WITH_POSITION.  They are needed to fully support srfi-192-style
       port positioning.   See the Port Positioning Interface below. */
    ScmObj (*getpos)(ScmPort *p);
    void   (*setpos)(ScmPort *p, ScmObj pos);
    u_long flags;               /* reserved */
} ScmPortBuffer;

/* The function table of procedural port. */
typedef struct ScmPortVTableRec {
    int     (*Getb)(ScmPort *p);
    int     (*Getc)(ScmPort *p);
    ScmSize (*Getz)(char *buf, ScmSize buflen, ScmPort *p);
    int     (*Ready)(ScmPort *p, int charp);
    void    (*Putb)(ScmByte b, ScmPort *p);
    void    (*Putc)(ScmChar c, ScmPort *p);
    void    (*Putz)(const char *buf, ScmSize size, ScmPort *p);
    void    (*Puts)(ScmString *s, ScmPort *p);
    void    (*Flush)(ScmPort *p);
    void    (*Close)(ScmPort *p);
    off_t   (*Seek)(ScmPort *p, off_t off, int whence);
    void    *data;

    /* The following fields are added in 0.9.10.  When you pass ScmPortBuffer
       to the constructor, those fields are looked at only when you specify
       SCM_PORT_WITH_POSITION.  They are needed to fully support srfi-192-style
       port positioning.   See the Port Positioning Interface below. */
    ScmObj (*GetPos)(ScmPort *p);
    void   (*SetPos)(ScmPort *p, ScmObj pos);
    u_long flags;               /* reserved */
} ScmPortVTable;

/* Input string port */
typedef struct ScmPortInputStringRec {
    const char *start;
    const char *current;
    const char *end;
} ScmPortInputString;

/* Note on Port Positioning Interface

   Gauche historically supported lseek(2) style interface, which can handle
   both getting and setting the position.  R6RS introduced
   port-position and set-port-position! interface, which then adopted as
   SRFI-192.  It requires different strategy than lseek interface.

   1) SRFI-192 allows port position can be gotten but not be set, and that
      needs to be queried by port-has-set-port-position!?.  With lseek API,
      port position is either totally unavaiable or can be gotten/set.
      (seek callback may raise an error when setting is not supported, but
      it can't be queried.)

   2) SRFI-192 allows arbitrary Scheme object as the position, while lseek
      API assumes integer byte offset.  Indeed, for the textual port, meaning
      of byte offset is vague.

   3) Lseek allows position to be set relative to the current position or
      to the end of the file.  SRFI-192 set-port-position! is always the
      absolute position.

  To support both interface, we have these hooks in SCM_PORT_FILE and
  SCM_PORT_PROC type of ports (the field names differ for the historical
  reasons):

     off_t SEEK(ScmPort*, off_t, int)    ;; seeker, Seek
     ScmObj GETPOS(ScmPort*)             ;; getpos, GetPos
     void SETPOS(ScmPort*, ScmObj)       ;; setpos, SetPos

  GETPOS/SETPOS takes precedence to SEEK.

  Port is port-has-port-position? if it has either GETPOS or SEEK handler,
  and port-has-set-port-position!? if it has either SETPOS or SEEK handler.

  For SCM_PORT_BUF, the position returned by GETPOS and that SETPOS takes
  must be an exact Scheme integer, representing the integer byte offset.
  Since the input is buffered, we need to calculate the actual position
  in the buffer, from the source position returned by GETPOS.

  For SCM_PORT_PROC, we treat the position as an opaque object.  Note that
  input port has a small buffer for peeked byte/character, that makes
  things complicated.  Since we can't adjust the opaque position, we instead
  call GETPOS before peek and remember the position.
 */

/*
 * We don't expose much about ScmPort struct to the user.  A bunch of
 * flags are public, but most of the stuff are hidden.  See priv/portP.h
 * for the 'real' definition.
 *
 * ScmPort should never be allocated directly, or statically.  Port
 * constructors properly initializes hidden fields.
 */

#define SCM_PORT_HEADER                                                 \
    SCM_INSTANCE_HEADER;                                                \
    u_int direction : 2;        /* SCM_PORT_INPUT or SCM_PORT_OUTPUT.   \
                                   There may be I/O port in future. */  \
    u_int type      : 2;        /* SCM_PORT_{FILE|ISTR|OSTR|PROC} */    \
    u_int scrcnt    : 3;        /* # of bytes in the scratch buffer */  \
                                                                        \
    u_int ownerp    : 1;        /* TRUE if this port owns underlying    \
                                   file pointer */                      \
    u_int closed    : 1;        /* TRUE if this port is closed */       \
    u_int error     : 1;        /* Error has been occurred */           \
                                                                        \
    u_int flags     : 5         /* see ScmPortFlags below */

struct ScmPortRec {
    SCM_PORT_HEADER;
    /* See portP.h for the real definition.  The magic number is chosen
       so that ScmPort has enough size to contain the real struct.

       We tested to use a pointer to the real struct, but I/O intensive
       benchmark showed 2-3% reduction of speed.  So we avoid indirection.
     */
    char opaque[29*sizeof(ScmWord) + 2*sizeof(ScmInternalFastlock)];
};

/* Port direction.
   Bidirectional port have both SCM_PORT_INPUT and SCM_PORT_OUTPUT bits set.
   SCM_PORT_OUTPUT_TRANSIENT is only used for constructor to indicate
   that the output buffer doesn't need to be flushed when the process
   exits.  It is same as SCM_PORT_OUTPUT for non-buffered ports.
   Only the lower two bits are stored in port->direction.
 */
enum ScmPortDirection {
    SCM_PORT_INPUT = 1,
    SCM_PORT_OUTPUT = 2,
    SCM_PORT_IOMASK = 3,
    SCM_PORT_OUTPUT_TRANSIENT = 4+SCM_PORT_OUTPUT
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

/* The flag bits passed to 'flags' argument of various port constructors */
enum ScmPortConstructorFlag {
    SCM_PORT_OWNER = (1L<<0),        /* Set the ownerp flag of the port */
    SCM_PORT_WITH_POSITION = (1L<<1) /* The additional getpos/setpos fields
                                        are looked at. */
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
    SCM_PORT_CASE_FOLD = (1L<<3),/* read from or write to this port should
                                    be case folding. */
    SCM_PORT_TRANSIENT = (1L<<4) /* a buffered output port that's used
                                    transiently and doesn't need to be
                                    registered for flushing. */
};

/*================================================================
 * Generic operations
 */

/* Predicates & accessors */
#define SCM_PORTP(obj)          (SCM_ISA(obj, SCM_CLASS_PORT))

#define SCM_PORT(obj)           ((ScmPort *)(obj))
#define SCM_PORT_TYPE(obj)      (SCM_PORT(obj)->type)
#define SCM_PORT_DIR(obj)       (SCM_PORT(obj)->direction)
#define SCM_PORT_FLAGS(obj)     (SCM_PORT(obj)->flags)

#define SCM_PORT_CASE_FOLDING(obj) (SCM_PORT_FLAGS(obj)&SCM_PORT_CASE_FOLD)

#define SCM_PORT_CLOSED_P(obj)  (SCM_PORT(obj)->closed)
#define SCM_PORT_OWNER_P(obj)   (SCM_PORT(obj)->ownerp)
#define SCM_PORT_ERROR_OCCURRED_P(obj) (SCM_PORT(obj)->error)

#define SCM_IPORTP(obj)  (SCM_PORTP(obj)&&(SCM_PORT_DIR(obj)&SCM_PORT_INPUT))
#define SCM_OPORTP(obj)  (SCM_PORTP(obj)&&(SCM_PORT_DIR(obj)&SCM_PORT_OUTPUT))

/* The following macros are for the backward compatibility; their use is
   deprecated.  (We used to have these macros directly access ScmPort
   structure to eliminate function call overhead.  It turned out
   it matters little.   Use function APIs.) */
#define SCM_PUTB(b, p)     Scm_Putb(b, SCM_PORT(p))
#define SCM_PUTC(c, p)     Scm_Putc(c, SCM_PORT(p))
#define SCM_PUTZ(s, l, p)  Scm_Putz(s, l, SCM_PORT(p))
#define SCM_PUTS(s, p)     Scm_Puts(SCM_STRING(s), SCM_PORT(p))
#define SCM_FLUSH(p)       Scm_Flush(SCM_PORT(p))
#define SCM_PUTNL(p)       SCM_PUTC('\n', p)
#define SCM_UNGETC(c, port) Scm_Ungetc(c, SCM_PORT(port))
#define SCM_GETB(b, p)     (b = Scm_Getb(SCM_PORT(p)))
#define SCM_GETC(c, p)     (c = Scm_Getc(SCM_PORT(p)))
#define SCM_PORT_BUFFER_ROOM(p)  Scm_PortBufferRoom(p)
#define SCM_PORT_BUFFER_AVAIL(p) Scm_PortBufferAvail(p)


SCM_CLASS_DECL(Scm_PortClass);
#define SCM_CLASS_PORT                (&Scm_PortClass)

SCM_CLASS_DECL(Scm_CodingAwarePortClass);
#define SCM_CLASS_CODING_AWARE_PORT   (&Scm_CodingAwarePortClass)

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
SCM_EXTERN int    Scm_GetPortCaseFolding(ScmPort *port);
SCM_EXTERN void   Scm_SetPortCaseFolding(ScmPort *port, int flag);
SCM_EXTERN ScmObj Scm_GetPortReaderLexicalMode(ScmPort *port);
SCM_EXTERN void   Scm_SetPortReaderLexicalMode(ScmPort *port, ScmObj obj);

SCM_EXTERN void   Scm_SetPortErrorOccurred(ScmPort *port, int val);

SCM_EXTERN int    Scm_PortPositionable(ScmPort *port, int setp);

SCM_EXTERN void   Scm_FlushAllPorts(int exitting);

SCM_EXTERN ScmObj Scm_PortName(ScmPort *port);
SCM_EXTERN ScmSize Scm_PortLine(ScmPort *port);
SCM_EXTERN ScmSize Scm_PortBytes(ScmPort *port);
SCM_EXTERN ScmObj Scm_GetPortPosition(ScmPort *port);
SCM_EXTERN ScmObj Scm_GetPortPositionUnsafe(ScmPort *port);
SCM_EXTERN ScmObj Scm_SetPortPosition(ScmPort *port, ScmObj pos);
SCM_EXTERN ScmObj Scm_PortSeek(ScmPort *port, ScmObj off, int whence);
SCM_EXTERN ScmObj Scm_PortSeekUnsafe(ScmPort *port, ScmObj off, int whence);
SCM_EXTERN int    Scm_PortFileNo(ScmPort *port);
SCM_EXTERN void   Scm_PortFdDup(ScmPort *dst, ScmPort *src);
SCM_EXTERN int    Scm_FdReady(int fd, int dir);
SCM_EXTERN int    Scm_ByteReady(ScmPort *port);
SCM_EXTERN int    Scm_ByteReadyUnsafe(ScmPort *port);
SCM_EXTERN int    Scm_CharReady(ScmPort *port);
SCM_EXTERN int    Scm_CharReadyUnsafe(ScmPort *port);

SCM_EXTERN ScmPortBuffer      *Scm_PortBufferStruct(ScmPort *port);
SCM_EXTERN ScmPortInputString *Scm_PortInputStringStruct(ScmPort *port);
SCM_EXTERN ScmDString         *Scm_PortOutputDString(ScmPort *port);
SCM_EXTERN ScmPortVTable      *Scm_PortVTableStruct(ScmPort *port);

SCM_EXTERN ScmSize  Scm_PortBufferRoom(ScmPort *port);
SCM_EXTERN ScmSize  Scm_PortBufferAvail(ScmPort *port);

SCM_EXTERN ScmWriteState *Scm_PortWriteState(ScmPort *);
SCM_EXTERN void           Scm_PortWriteStateSet(ScmPort *, ScmWriteState*);

SCM_EXTERN void   Scm_ClosePort(ScmPort *port);

SCM_EXTERN ScmObj Scm_VMWithPortLocking(ScmPort *port,
                                        ScmObj closure);

SCM_EXTERN ScmObj Scm_PortAttrGet(ScmPort *port, ScmObj key,
                                  ScmObj fallback);
SCM_EXTERN ScmObj Scm_PortAttrSet(ScmPort *port, ScmObj key, ScmObj val);
SCM_EXTERN ScmObj Scm_PortAttrDelete(ScmPort *port, ScmObj key);
SCM_EXTERN ScmObj Scm_PortAttrs(ScmPort *port);

SCM_EXTERN void   Scm_Putb(ScmByte b, ScmPort *port);
SCM_EXTERN void   Scm_Putc(ScmChar c, ScmPort *port);
SCM_EXTERN void   Scm_Puts(ScmString *s, ScmPort *port);
SCM_EXTERN void   Scm_Putz(const char *s, ScmSize len, ScmPort *port);
SCM_EXTERN void   Scm_Flush(ScmPort *port);

SCM_EXTERN void   Scm_PutbUnsafe(ScmByte b, ScmPort *port);
SCM_EXTERN void   Scm_PutcUnsafe(ScmChar c, ScmPort *port);
SCM_EXTERN void   Scm_PutsUnsafe(ScmString *s, ScmPort *port);
SCM_EXTERN void   Scm_PutzUnsafe(const char *s, ScmSize len, ScmPort *port);
SCM_EXTERN void   Scm_FlushUnsafe(ScmPort *port);

SCM_EXTERN void   Scm_Ungetc(ScmChar ch, ScmPort *port);
SCM_EXTERN void   Scm_Ungetb(int b, ScmPort *port);
SCM_EXTERN int    Scm_Getb(ScmPort *port);
SCM_EXTERN int    Scm_Getc(ScmPort *port);
SCM_EXTERN ScmSize Scm_Getz(char *buf, ScmSize buflen, ScmPort *port);
SCM_EXTERN ScmChar Scm_Peekc(ScmPort *port);
SCM_EXTERN int    Scm_Peekb(ScmPort *port);
SCM_EXTERN ScmObj Scm_UngottenChars(ScmPort *port);
SCM_EXTERN ScmObj Scm_UngottenBytes(ScmPort *port);

SCM_EXTERN void   Scm_UngetcUnsafe(ScmChar ch, ScmPort *port);
SCM_EXTERN void   Scm_UngetbUnsafe(int b, ScmPort *port);
SCM_EXTERN int    Scm_GetbUnsafe(ScmPort *port);
SCM_EXTERN int    Scm_GetcUnsafe(ScmPort *port);
SCM_EXTERN ScmSize Scm_GetzUnsafe(char *buf, ScmSize buflen, ScmPort *port);
SCM_EXTERN ScmChar Scm_PeekcUnsafe(ScmPort *port);
SCM_EXTERN int    Scm_PeekbUnsafe(ScmPort *port);
SCM_EXTERN ScmObj Scm_UngottenCharsUnsafe(ScmPort *port);
SCM_EXTERN ScmObj Scm_UngottenBytesUnsafe(ScmPort *port);

SCM_EXTERN ScmObj Scm_ReadLine(ScmPort *port);
SCM_EXTERN ScmObj Scm_ReadLineUnsafe(ScmPort *port);

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

enum {
    SCM_PORT_STRING_PRIVATE = 1 /* for flags arg */
};

SCM_EXTERN ScmObj Scm_MakeInputStringPortFull(ScmString *str,
                                              ScmObj name,
                                              u_long flags);
SCM_EXTERN ScmObj Scm_MakeOutputStringPortFull(ScmObj name,
                                               u_long flags);

/* these two are deprecated */
SCM_EXTERN ScmObj Scm_MakeInputStringPort(ScmString *str, int privatep);
SCM_EXTERN ScmObj Scm_MakeOutputStringPort(int privatep);

SCM_EXTERN ScmObj Scm_GetOutputString(ScmPort *port, int flags);
SCM_EXTERN ScmObj Scm_GetOutputStringUnsafe(ScmPort *port, int flags);
SCM_EXTERN ScmObj Scm_GetRemainingInputString(ScmPort *port, int flags);

/*================================================================
 * Other type of ports
 */

SCM_EXTERN ScmObj Scm_MakeVirtualPortFull(ScmClass *klass,
                                          ScmObj name, int direction,
                                          const ScmPortVTable *vtable,
                                          u_long flags);

/* deprecated */
SCM_EXTERN ScmObj Scm_MakeVirtualPort(ScmClass *klass,
                                      int direction,
                                      const ScmPortVTable *vtable);


SCM_EXTERN ScmObj Scm_MakeBufferedPortFull(ScmClass *klass,
                                           ScmObj name, int direction,
                                           ScmPortBuffer *bufrec,
                                           u_long flags);

/* deprecated */
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

#endif /*GAUCHE_PORT_H*/
