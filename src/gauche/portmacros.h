/*
 * portmacros.h - auxiliary macros for port
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
 *  $Id: portmacros.h,v 1.3 2001-05-21 09:14:54 shirok Exp $
 */

#ifndef GAUCHE_PORT_MACROS_H
#define GAUCHE_PORT_MACROS_H

/*
 * Inlined operation for better performance.
 * Assuming the port is confirmed as input/output port.
 */

/*====================================================================
 * Output Port
 */

/*--------------------------------------------------------------------
 * File output
 */
#define SCM__FILE_PUTB(b, port) \
    putc(b, SCM_PORT(port)->src.file.fp)

#define SCM__FILE_PUTC(c, port)                         \
    do { char buf_PORT[SCM_CHAR_MAX_BYTES];             \
         SCM_CHAR_PUT(buf_PORT, c);                     \
         fwrite(buf_PORT, 1, SCM_CHAR_NBYTES(c),        \
                SCM_PORT(port)->src.file.fp);           \
    } while(0)

#define SCM__FILE_PUTZ(s, port) \
    fputs(s, SCM_PORT(port)->src.file.fp)

#define SCM__FILE_PUTS(s, port)                 \
    fwrite(SCM_STRING_START(s), 1,              \
           SCM_STRING_SIZE(s),                  \
           SCM_PORT(port)->src.file.fp)

#define SCM__FILE_FLUSH(port) \
    fflush(SCM_PORT(port)->src.file.fp)

/*---------------------------------------------------------------------
 * String output
 */

#define SCM__OSTR_PUTB(b, port)                         \
    SCM_DSTRING_PUTB(&SCM_PORT(port)->src.ostr, b)

#define SCM__OSTR_PUTC(c, port)                         \
    SCM_DSTRING_PUTC(&SCM_PORT(port)->src.ostr, c)

#define SCM__OSTR_PUTZ(s, port)                         \
    Scm_DStringPutz(&SCM_PORT(port)->src.ostr, s)

#define SCM__OSTR_PUTS(s, port)                         \
    Scm_DStringAdd(&SCM_PORT(port)->src.ostr, SCM_STRING(s))

/*---------------------------------------------------------------------
 * Procedural output
 */

#define SCM__PROC_PUTB(b, port)                         \
    SCM_PORT(port)->src.proc.vtable->Putb(SCM_PORT(port), b)

#define SCM__PROC_PUTC(c, port)                         \
    SCM_PORT(port)->src.proc.vtable->Putc(SCM_PORT(port), c)

#define SCM__PROC_PUTZ(s, port)                         \
    SCM_PORT(port)->src.proc.vtable->Putz(SCM_PORT(port), s)

#define SCM__PROC_PUTS(s, port)                                 \
    SCM_PORT(port)->src.proc.vtable->Puts(SCM_PORT(port),       \
                                          SCM_STRING(s))

#define SCM__PROC_FLUSH(port) \
    SCM_PORT(port)->src.proc.vtable->Flush(SCM_PORT(port))

/*---------------------------------------------------------------------
 * Generic output
 */

#define SCM_PUTB(byte, port)                                            \
    do {                                                                \
        switch (SCM_PORT_TYPE(port)) {                                  \
          case SCM_PORT_FILE: SCM__FILE_PUTB(byte, port); break;        \
          case SCM_PORT_OSTR: SCM__OSTR_PUTB(byte, port); break;        \
          case SCM_PORT_PROC: SCM__PROC_PUTB(byte, port); break;        \
          case SCM_PORT_CLOSED:                                         \
            Scm_Error("port already closed: %S", SCM_OBJ(port));        \
          default: Scm_Panic("SCM_PUTB: something screwed up");         \
            /*NOTREACHED*/                                              \
        }                                                               \
    } while (0)

#define SCM_PUTC(ch, port)                                              \
    do {                                                                \
        switch (SCM_PORT_TYPE(port)) {                                  \
          case SCM_PORT_FILE: SCM__FILE_PUTC(ch, port); break;          \
          case SCM_PORT_OSTR: SCM__OSTR_PUTC(ch, port); break;          \
          case SCM_PORT_PROC: SCM__PROC_PUTC(ch, port); break;          \
          case SCM_PORT_CLOSED:                                         \
            Scm_Error("port already closed: %S", SCM_OBJ(port));        \
          default: Scm_Panic("SCM_PUTC: something screwed up");         \
            /*NOTREACHED*/                                              \
        }                                                               \
    } while (0)

#define SCM_PUTZ(str, port)                                             \
    do {                                                                \
        switch (SCM_PORT_TYPE(port)) {                                  \
          case SCM_PORT_FILE: SCM__FILE_PUTZ(str, port); break;         \
          case SCM_PORT_OSTR: SCM__OSTR_PUTZ(str, port); break;         \
          case SCM_PORT_PROC: SCM__PROC_PUTZ(str, port); break;         \
          case SCM_PORT_CLOSED:                                         \
            Scm_Error("port already closed: %S", SCM_OBJ(port));        \
          default: Scm_Panic("SCM_PUTZ: something screwed up");         \
        }                                                               \
    } while (0)

#define SCM_PUTS(str, port)                                             \
    do {                                                                \
        switch (SCM_PORT_TYPE(port)) {                                  \
          case SCM_PORT_FILE: SCM__FILE_PUTS(str, port); break;         \
          case SCM_PORT_OSTR: SCM__OSTR_PUTS(str, port); break;         \
          case SCM_PORT_PROC: SCM__PROC_PUTS(str, port); break;         \
          case SCM_PORT_CLOSED:                                         \
            Scm_Error("port already closed: %S", SCM_OBJ(port));        \
          default: Scm_Panic("SCM_PUTS: something screwed up");         \
            /*NOTREACHED*/                                              \
        }                                                               \
    } while (0)

#define SCM_FLUSH(port)                                                 \
    do {                                                                \
        switch (SCM_PORT_TYPE(port)) {                                  \
          case SCM_PORT_FILE: SCM__FILE_FLUSH(port); break;             \
          case SCM_PORT_OSTR: break;                                    \
          case SCM_PORT_PROC: SCM__PROC_FLUSH(port); break;             \
          case SCM_PORT_CLOSED: break;                                  \
          default: Scm_Panic("SCM_FLUSH: something screwed up");        \
            /*NOTREACHED*/                                              \
        }                                                               \
    } while (0)

#define SCM_PUTNL(port)      SCM_PUTC('\n', port)

/*====================================================================
 * Input Port
 */

/* only one-char unget is supported */
#define SCM_UNGETC(c, port)      (SCM_PORT(port)->ungotten = (c))

/*--------------------------------------------------------------------
 * File input
 */
#define SCM__FILE_GETB(b, port)                                 \
    do {                                                        \
        if ((b = getc(SCM_PORT(port)->src.file.fp)) == '\n') {  \
            SCM_PORT(port)->src.file.line++;                    \
            SCM_PORT(port)->src.file.column = 0;                \
        } else {                                                \
            SCM_PORT(port)->src.file.column++;                  \
        }                                                       \
    } while(0)
    
#define SCM__FILE_GETC(c, port)                                         \
    do {                                                                \
        int nbytes_SCM_GETC;                                            \
        c = getc(SCM_PORT(port)->src.file.fp);                          \
        SCM_PORT(port)->src.file.column++;                              \
        if (c != EOF && (nbytes_SCM_GETC = SCM_CHAR_NFOLLOWS(c))) {     \
            c = Scm__PortFileGetc(c, SCM_PORT(port));                   \
            SCM_PORT(port)->src.file.column += nbytes_SCM_GETC;         \
        } else if (c == '\n') {                                         \
            SCM_PORT(port)->src.file.line++;                            \
            SCM_PORT(port)->src.file.column = 0;                        \
        }                                                               \
    } while (0)

extern int Scm__PortFileGetc(int prefetch, ScmPort *port);

/*--------------------------------------------------------------------
 * String input
 */

#define SCM__ISTR_GETB(b, port)                         \
    do {                                                \
        if (SCM_PORT(port)->src.istr.rest <= 0) {       \
            (b) = Scm__PortIstrGetb(port);              \
        } else {                                        \
            SCM_PORT(port)->src.istr.rest--;            \
            (b) = *SCM_PORT(port)->src.istr.current++;  \
        }                                               \
    } while (0)

#define SCM__ISTR_GETC(c, port)                                         \
    do {                                                                \
       if (SCM_PORT(port)->src.istr.rest <= 0) {                        \
           (c) = Scm__PortIstrGetc(-1, SCM_PORT(port));                 \
       } else {                                                         \
           const char *cp__ISTR = SCM_PORT(port)->src.istr.current;     \
           unsigned char uc__ISTR = (unsigned char)*cp__ISTR;           \
           int siz__ISTR = SCM_CHAR_NFOLLOWS(uc__ISTR);                 \
           if (SCM_PORT(port)->src.istr.rest < siz__ISTR) {             \
               (c) = Scm__PortIstrGetc(uc__ISTR, SCM_PORT(port));       \
           } else {                                                     \
               SCM_CHAR_GET(cp__ISTR, c);                               \
           }                                                            \
           SCM_PORT(port)->src.istr.current += siz__ISTR + 1;           \
           SCM_PORT(port)->src.istr.rest -= siz__ISTR + 1;              \
       }                                                                \
    } while (0)

extern int Scm__PortIstrGetb(ScmPort *port);
extern ScmChar Scm__PortIstrGetc(int prefetch, ScmPort *port);

/*--------------------------------------------------------------------
 * Procedural input
 */

#define SCM__PROC_GETB(b, port) \
    ((b) = SCM_PORT(port)->src.proc.vtable->Getb(SCM_PORT(port)))

#define SCM__PROC_GETC(c, port) \
    ((c) = SCM_PORT(port)->src.proc.vtable->Getc(SCM_PORT(port)))

/*--------------------------------------------------------------------
 * Generic input
 */

#define SCM_GETB(var, port)                                             \
    do {                                                                \
        if (SCM_PORT(port)->ungotten != SCM_CHAR_INVALID                \
            || SCM_PORT(port)->bufcnt != 0) {                           \
            (var) = Scm__PortGetbInternal(SCM_PORT(port));              \
        } else {                                                        \
            switch (SCM_PORT_TYPE(port)) {                              \
              case SCM_PORT_FILE: SCM__FILE_GETB(var, port); break;     \
              case SCM_PORT_ISTR: SCM__ISTR_GETB(var, port); break;     \
              case SCM_PORT_PROC: SCM__PROC_GETB(var, port); break;     \
              case SCM_PORT_CLOSED:                                     \
                Scm_Error("port already closed: %S", SCM_OBJ(port));    \
              default: Scm_Panic("SCM_GETB: something screwed up");     \
                 /*NOTREACHED*/                                         \
            }                                                           \
        }                                                               \
    } while (0)

extern int Scm__PortGetbInternal(ScmPort *port);

#define SCM_GETC(var, port)                                             \
    do {                                                                \
        if (SCM_PORT(port)->ungotten != SCM_CHAR_INVALID) {             \
            var = SCM_PORT(port)->ungotten;                             \
            SCM_PORT(port)->ungotten = SCM_CHAR_INVALID;                \
        } else if (SCM_PORT(port)->bufcnt != 0) {                       \
            var = Scm__PortGetcInternal(SCM_PORT(port));                \
        } else {                                                        \
            switch (SCM_PORT_TYPE(port)) {                              \
              case SCM_PORT_FILE: SCM__FILE_GETC(var, port); break;     \
              case SCM_PORT_ISTR: SCM__ISTR_GETC(var, port); break;     \
              case SCM_PORT_PROC: SCM__PROC_GETC(var, port); break;     \
              case SCM_PORT_CLOSED:                                     \
                Scm_Error("port already closed: %S", SCM_OBJ(port));    \
              default: Scm_Panic("SCM_GETC: something screwed up");     \
                 /*NOTREACHED*/                                         \
            }                                                           \
        }                                                               \
    } while (0)

extern int Scm__PortGetcInternal(ScmPort *port);



#endif /* GAUCHE_PORT_MACROS_H */

