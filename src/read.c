/*
 * read.c - reader
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
 *  $Id: read.c,v 1.32 2001-12-15 10:58:49 shirok Exp $
 */

#include <stdio.h>
#include <ctype.h>
#include <math.h>
#include "gauche.h"

/*
 * READ
 */

static ScmObj read_internal(ScmPort *port, ScmReadContext *ctx);
static ScmObj read_list(ScmPort *port, ScmChar closer, ScmReadContext *ctx);
static ScmObj read_string(ScmPort *port, int incompletep);
static ScmObj read_quoted(ScmPort *port, ScmObj quoter, ScmReadContext *ctx);
static ScmObj read_char(ScmPort *port);
static ScmObj read_word(ScmPort *port, ScmChar initial);
static ScmObj read_symbol(ScmPort *port, ScmChar initial);
static ScmObj read_number(ScmPort *port, ScmChar initial);
static ScmObj read_symbol_or_number(ScmPort *port, ScmChar initial);
static ScmObj read_escaped_symbol(ScmPort *port, ScmChar delim);
static ScmObj read_keyword(ScmPort *port);
static ScmObj read_regexp(ScmPort *port);
static ScmObj read_charset(ScmPort *port);
static ScmObj read_sharp_comma(ScmPort *port, ScmObj form);
static ScmObj maybe_uvector(ScmPort *port, char c, ScmReadContext *ctx);

/* Special hook for SRFI-4 syntax */
ScmObj (*Scm_ReadUvectorHook)(ScmPort *port, const char *tag);

/* Table of 'read-time constructor' in SRFI-10 */
/* TODO: MT Safeness */
static ScmHashTable *read_ctor_table;

/*----------------------------------------------------------------
 * Entry point
 */
ScmObj Scm_Read(ScmObj port)
{
    ScmReadContext ctx;
    ctx.flags = SCM_READ_SOURCE_INFO;
    ctx.table = NULL;
    if (!SCM_PORTP(port) || SCM_PORT_DIR(port) != SCM_PORT_INPUT) {
        Scm_Error("input port required: %S", port);
    }
    return read_internal(SCM_PORT(port), &ctx);
}

ScmObj Scm_ReadList(ScmObj port, ScmChar closer)
{
    ScmReadContext ctx;
    ctx.flags = SCM_READ_SOURCE_INFO;
    ctx.table = NULL;
    if (!SCM_PORTP(port) || SCM_PORT_DIR(port) != SCM_PORT_INPUT) {
        Scm_Error("input port required: %S", port);
    }
    return read_list(SCM_PORT(port), closer, &ctx);
}

/*----------------------------------------------------------------
 * Error
 */

static void read_error(ScmPort *port, const char *msg, ...)
{
    ScmObj ostr = Scm_MakeOutputStringPort();
    ScmObj name = Scm_PortName(port);
    int line = Scm_PortLine(port);
    va_list ap;

    Scm_Printf(SCM_PORT(ostr), "Read error at %S:",
               SCM_STRINGP(name)? name : SCM_OBJ(SCM_MAKE_STR("??")));
    if (line >= 0) {
        Scm_Printf(SCM_PORT(ostr), "line %d: ", line);
    }
    va_start(ap, msg);
    Scm_Vprintf(SCM_PORT(ostr), msg, ap);
    va_end(ap);
    Scm_Error("%A", Scm_GetOutputString(SCM_PORT(ostr)));
}

/*----------------------------------------------------------------
 * Miscellaneous routines
 */

#ifdef __GNUC__
inline
#endif
static int skipws(ScmPort *port)
{
    int c = 0;
    for (;;) {
        SCM_GETC(c, port);
        if (c == EOF) return c;
        if (c <= 256 && isspace(c)) continue;
        if (c == ';') {
            for (;;) {
                SCM_GETC(c, port);
                if (c == '\n') break;
                if (c == EOF) return EOF;
            }
            continue;
        }
        return c;
    }
}

#define PUNCT_CASE \
    case '(':; case ')':; case '[':; case ']':;       \
    case '{':; case '}':; case '"':; case ';':;       \
    case ' ':; case '\n':; case '\t':; case '\r'

ScmObj read_internal(ScmPort *port, ScmReadContext *ctx)
{
    int c;

    c = skipws(port);
    switch (c) {
    case '(':
        return read_list(port, ')', ctx);
    case '"':
        return read_string(port, FALSE);
    case '#':
        {
            int c1 = 0;
            SCM_GETC(c1, port);
            switch (c1) {
            case EOF:
                read_error(port, "premature #-sequence at EOF");
            case 't':; case 'T': return SCM_TRUE;
            case 'f':; case 'F': return maybe_uvector(port, 'f', ctx);
            case 's':; case 'S': return maybe_uvector(port, 's', ctx);
            case 'u':; case 'U': return maybe_uvector(port, 'u', ctx);
            case '(':
                {
                    ScmObj v = read_list(port, ')', ctx);
                    return Scm_ListToVector(v);
                }
            case '\\':
                return read_char(port);
            case 'x':; case 'X':; case 'o':; case 'O':;
            case 'b':; case 'B':; case 'd':; case 'D':;
            case 'e':; case 'E':; case 'i':; case 'I':;
                SCM_UNGETC(c1, port);
                return read_number(port, c);
            case '!':
                /* allow `#!' magic of executable */
                for (;;) {
                    SCM_GETC(c, port);
                    if (c == '\n') return read_internal(port, ctx);
                    if (c == EOF) return SCM_EOF;
                }
            case '/':
                /* #/.../ literal regexp */
                return read_regexp(port);
            case '[':
                /* #[...] literal charset */
                return read_charset(port);
            case '"':
                /* #"..." explicit incomplete string */
                return read_string(port, TRUE);
            case ',':
                /* #,(form) - SRFI-10 read-time macro */
                {
                    ScmObj form = read_internal(port, ctx);
                    return read_sharp_comma(port, form);
                }
            case '?':
                /* #?form - debug print (for now) */
                {
                    ScmObj form = read_internal(port, ctx);
                    return SCM_LIST2(SCM_INTERN("debug-print"), form);
                }
            default:
                read_error(port, "unsupported #-syntax: #%C", c1);
            }
        }
    case '\'': return read_quoted(port, SCM_SYM_QUOTE, ctx);
    case '`': return read_quoted(port, SCM_SYM_QUASIQUOTE, ctx);
    case ':':
        return read_keyword(port);
    case ',':
        {
            int c1 = 0;
            SCM_GETC(c1, port);
            if (c1 == EOF) {
                read_error(port, "unterminated unquote");
            } else if (c1 == '@') {
                return read_quoted(port, SCM_SYM_UNQUOTE_SPLICING, ctx);
            } else {
                SCM_UNGETC(c1, port);
                return read_quoted(port, SCM_SYM_UNQUOTE, ctx);
            }
        }
    case '|':
        return read_escaped_symbol(port, '|');
    case '[':
        /* TODO: make it customizable */
        return read_list(port, ']', ctx);
    case '{':
        return read_list(port, '}', ctx);
    case '+':; case '-':
        /* Note: R5RS doesn't permit identifiers beginning with '+' or '-',
           but some Scheme programs use such identifiers. */
        return read_symbol_or_number(port, c);
    case '.':;
        {
            int c1 = 0;
            SCM_GETC(c1, port);
            switch (c1) {
            PUNCT_CASE:
                read_error(port, "dot in wrong context");
            default:
                SCM_UNGETC(c1, port);
                return read_symbol_or_number(port, c);
            }
        }
    case '0':; case '1':; case '2':; case '3':; case '4':;
    case '5':; case '6':; case '7':; case '8':; case '9':;
        /* Note: R5RS doesn't permit identifiers beginning with digits,
           but some Scheme programs use such identifiers. */
        return read_symbol_or_number(port, c);
    case ')':; case ']':; case '}':;
        read_error(port, "extra close parenthesis");
    case EOF:
        return SCM_EOF;
    default:
        return read_symbol(port, c);
    }
}

/*----------------------------------------------------------------
 * List
 */

static ScmObj read_list(ScmPort *port, ScmChar closer, ScmReadContext *ctx)
{
    ScmObj start = SCM_NIL, last = SCM_NIL, item;
    int c, dot_seen = 0;
    int line = -1;

    if (ctx->flags & SCM_READ_SOURCE_INFO) line = Scm_PortLine(port);
    
    for (;;) {
        c = skipws(port);
        if (c == EOF) read_error(port, "EOF inside a list");
        if (c == closer) return start;

        if (dot_seen) read_error(port, "bad dot syntax");

        if (c == '.') {
            int c2 = 0;
            SCM_GETC(c2, port);
            if (c2 == closer || c2 == EOF) {
                read_error(port, "bad dot syntax");
            } else if (isspace(c2)) {
                /* dot pair at the end */
                if (start == SCM_NIL) {
                    read_error(port, "bad dot syntax");
                }
                item = read_internal(port, ctx);
                SCM_SET_CDR(last, item);
                dot_seen++;
                continue;
            }
            SCM_UNGETC(c2, port);
            item = read_symbol_or_number(port, c);
        } else {
            SCM_UNGETC(c, port);
            item = read_internal(port, ctx);
        }
        SCM_APPEND1(start, last, item);
        if (start==last && (ctx->flags & SCM_READ_SOURCE_INFO) && line >= 0) {
            /* add source information to the top of the list */
            Scm_PairAttrSet(SCM_PAIR(start), SCM_SYM_SOURCE_INFO,
                            SCM_LIST2(Scm_PortName(port),
                                      SCM_MAKE_INT(line)));
        }
    }
}

static ScmObj read_quoted(ScmPort *port, ScmObj quoter, ScmReadContext *ctx)
{
    ScmObj item = read_internal(port, ctx);
    if (SCM_EOFP(item)) read_error(port, "unterminated quote");
    return Scm_Cons(quoter, Scm_Cons(item, SCM_NIL));
}

/*----------------------------------------------------------------
 * String
 */

static ScmObj read_string(ScmPort *port, int incompletep)
{
    int c = 0;
    ScmDString ds;
    Scm_DStringInit(&ds);

#define FETCH(var)                              \
    if (incompletep) { SCM_GETB(var, port); }   \
    else             { SCM_GETC(var, port); }
#define ACCUMULATE(var)                                 \
    if (incompletep) { SCM_DSTRING_PUTB(&ds, var); }    \
    else             { SCM_DSTRING_PUTC(&ds, var); }

    for (;;) {
        FETCH(c);
        switch (c) {
          case EOF: goto eof_exit;
          case '"': {
              ScmString *s = SCM_STRING(Scm_DStringGet(&ds));
              if (incompletep) {
                  s = SCM_STRING(Scm_StringCompleteToIncompleteX(s));
              }
              return Scm_StringMakeImmutable(s);
          }
          case '\\': {
            int c1 = 0;
            SCM_GETC(c1, port);
            switch (c1) {
              case EOF: goto eof_exit;
              case 'n': ACCUMULATE('\n'); break;
              case 'r': ACCUMULATE('\r'); break;
              case 'f': ACCUMULATE('\f'); break;
              case 't': ACCUMULATE('\t'); break;
              case '\\': ACCUMULATE('\\'); break;
              case '0': ACCUMULATE(0); break;
              case 'x': {
                  int c2 = 0, c3 = 0;
                  SCM_GETC(c2, port);
                  if (c2 == EOF) goto eof_exit;
                  if (SCM_CHAR_ASCII_P(c2) && isxdigit(c2)) {
                      SCM_GETC(c3, port);
                      if (c3 == EOF) goto eof_exit;
                      if (SCM_CHAR_ASCII_P(c2) && isxdigit(c3)) {
                          int cc = ((Scm_DigitToInt(c2, 16)<<4)
                                    + Scm_DigitToInt(c3, 16));
                          ACCUMULATE(cc);
                      } else {
                          ACCUMULATE('\\');
                          ACCUMULATE('x');
                          ACCUMULATE(c2);
                          ACCUMULATE(c3);
                      }
                  } else {
                      ACCUMULATE('\\');
                      ACCUMULATE('x');
                      ACCUMULATE(c2);
                  }
                  break;
              }
              default:
                ACCUMULATE(c1); break;
            }
            break;
          }
          default: ACCUMULATE(c); break;
        }
    }
 eof_exit:
    read_error(port, "EOF encountered in a string literal: %S", Scm_DStringGet(&ds));
    /* NOTREACHED */
    return SCM_FALSE; 
}

/*----------------------------------------------------------------
 * Character 
 */

static struct char_name {
    const char *name;
    ScmObj ch;
} char_names[] = {
    { "space",        SCM_MAKE_CHAR(' ')  },
    { "newline",      SCM_MAKE_CHAR('\n') },
    { "nl",           SCM_MAKE_CHAR('\n') },
    { "lf",           SCM_MAKE_CHAR('\n') },
    { "return",       SCM_MAKE_CHAR('\r') },
    { "cr",           SCM_MAKE_CHAR('\r') },
    { "tab",          SCM_MAKE_CHAR('\t') },
    { "ht",           SCM_MAKE_CHAR('\t') },
    { "page",         SCM_MAKE_CHAR('\f') },
    { "escape",       SCM_MAKE_CHAR(0x1b) },
    { "esc",          SCM_MAKE_CHAR(0x1b) },
    { "delete",       SCM_MAKE_CHAR(0x7f) },
    { "del",          SCM_MAKE_CHAR(0x7f) },
    { "null",         SCM_MAKE_CHAR(0)    },
    { NULL, 0 }
};

static ScmObj read_char(ScmPort *port)
{
    int c = 0;
    ScmString *name;
    const char *cname;
    struct char_name *cntab = char_names;
    
    SCM_GETC(c, port);
    switch (c) {
    case EOF: read_error(port, "EOF encountered in character literal");
    case '(':; case ')':; case '[':; case ']':; case '{':; case '}':;
    case '"':; case ' ':; case '\\':; case '|':; case ';':;
    case '#':;
        return SCM_MAKE_CHAR(c);
    default:
        /* need to read word to see if it is a character name */
        name = SCM_STRING(read_word(port, c));
        if (SCM_STRING_LENGTH(name) == 1) {
            return SCM_MAKE_CHAR(c);
        }
        cname = Scm_GetStringConst(name);

        /* handle #\x1f etc. */
        if (cname[0] == 'x' && isxdigit(cname[1])) {
            int i = 1, cc;
            unsigned long code = 0;
            do {
                cc = Scm_DigitToInt(cname[i], 16);
                code = code * 16 + cc; /* TODO: check overflow */
                if (cname[++i] == '\0') return SCM_MAKE_CHAR(code);
            } while (isxdigit(cname[i]));
        }
        
        while (cntab->name) {
            if (strcmp(cntab->name, cname) == 0) return cntab->ch;
            cntab++;
        }
        read_error(port, "Unknown character name: #\\%s", cname);
    }
    return SCM_UNDEFINED;       /* dummy */
}

/*----------------------------------------------------------------
 * Symbols and Numbers
 */

static ScmObj read_word(ScmPort *port, ScmChar initial)
{
    int c = 0;
    ScmDString ds;
    Scm_DStringInit(&ds);
    if (initial != SCM_CHAR_INVALID) c = initial;
    else SCM_GETC(c, port);
    
    for (;;) {
        switch (c) {
        case EOF:;
        case '(':; case ')':; case '[':; case ']':; case '{':; case '}':;
        case '"':; case ' ':; case '\n':; case '\r':; case '\t':;
        case ';':;
            SCM_UNGETC(c, port);
            return Scm_DStringGet(&ds);
        default:
            if (c >= 'A' && c <= 'Z') c += ('a' - 'A');
            SCM_DSTRING_PUTC(&ds, c);
            SCM_GETC(c, port);
        }
    }
}

static ScmObj read_symbol(ScmPort *port, ScmChar initial)
{
    ScmString *s = SCM_STRING(read_word(port, initial));
    return Scm_Intern(s);
}

static ScmObj read_number(ScmPort *port, ScmChar initial)
{
    ScmString *s = SCM_STRING(read_word(port, initial));
    ScmObj num = Scm_StringToNumber(s, 10);
    if (num == SCM_FALSE)
        read_error(port, "bad numeric format: %S", s);
    return num;
}

static ScmObj read_symbol_or_number(ScmPort *port, ScmChar initial)
{
    ScmString *s = SCM_STRING(read_word(port, initial));
    ScmObj num = Scm_StringToNumber(s, 10);
    if (num == SCM_FALSE)
        return Scm_Intern(s);
    else
        return num;
}

static ScmObj read_keyword(ScmPort *port)
{
    ScmString *s = SCM_STRING(read_word(port, SCM_CHAR_INVALID));
    return Scm_MakeKeyword(s);
}

static ScmObj read_escaped_symbol(ScmPort *port, ScmChar delim)
{
    int c = 0;
    ScmDString ds;
    Scm_DStringInit(&ds);
    
    for (;;) {
        SCM_GETC(c, port);
        if (c == EOF) {
            read_error(port, "unterminated escaped symbol: |%s ...",
                       Scm_DStringGetz(&ds));
        } else if (c == delim) {
            ScmString *s = SCM_STRING(Scm_DStringGet(&ds));
            return Scm_Intern(s);
        } else {
            SCM_DSTRING_PUTC(&ds, c);
        }
    }
}

/*----------------------------------------------------------------
 * Regexp & charset
 */

/* gauche extension :  #/regexp/ */
static ScmObj read_regexp(ScmPort *port)
{
    ScmChar c = 0;
    ScmDString ds;
    Scm_DStringInit(&ds);
    for (;;) {
        SCM_GETC(c, port);
        if (c == SCM_CHAR_INVALID) {
            read_error(port, "unterminated literal regexp");
        }
        if (c == '\\') {
            SCM_DSTRING_PUTC(&ds, c);
            SCM_GETC(c, port);
            if (c == SCM_CHAR_INVALID) {
                read_error(port, "unterminated literal regexp");
            }
            SCM_DSTRING_PUTC(&ds, c);
        } else if (c == '/') {
            return Scm_RegComp(SCM_STRING(Scm_DStringGet(&ds)));
        } else {
            SCM_DSTRING_PUTC(&ds, c);
        }
    }
}

/* gauche extension :  #[charset] */
static ScmObj read_charset(ScmPort *port)
{
    return Scm_CharSetRead(port, NULL, TRUE, FALSE);
}

/*----------------------------------------------------------------
 * SRFI-10 support
 */

ScmObj Scm_DefineReaderCtor(ScmObj symbol, ScmObj proc)
{
    ScmHashEntry *e;
    if (!SCM_PROCEDUREP(proc)) {
        Scm_Error("procedure required, but got %S\n", proc);
    }
    /* TODO: MT Safeness */
    Scm_HashTablePut(read_ctor_table, symbol, proc);
    return SCM_UNDEFINED;
}

static ScmObj read_sharp_comma(ScmPort *port, ScmObj form)
{
    int len = Scm_Length(form);
    ScmHashEntry *e;

    if (len <= 0) {
        read_error(port, "bad #,-form: #,%S", form);
    }

    /* TODO: MT Safeness */
    e = Scm_HashTableGet(read_ctor_table, SCM_CAR(form));
    if (e == NULL) {
        read_error(port, "unknown #,-key: %S", SCM_CAR(form));
    }
    SCM_ASSERT(SCM_PROCEDUREP(e->value));
    return Scm_Apply(e->value, SCM_CDR(form));
}

static ScmObj reader_ctor(ScmObj *args, int nargs, void *data)
{
    return Scm_DefineReaderCtor(args[0], args[1]);
}

/*----------------------------------------------------------------
 * Uvector
 */

/* Uvector support is implemented by extention.  When the extention
   is loaded, it sets up the pointer Scm_ReadUvectorHook. */

static ScmObj maybe_uvector(ScmPort *port, char ch, ScmReadContext *ctx)
{
    ScmChar c1 = 0, c2 = SCM_CHAR_INVALID;
    char *tag = NULL;

    SCM_GETC(c1, port);
    if (ch == 'f') {
        if (c1 != '3' && c1 != '6') {
            SCM_UNGETC(c1, port);
            return SCM_FALSE;
        }
        SCM_GETC(c2, port);
        if (c1 == '3' && c2 == '2') tag = "f32";
        else if (c1 == '6' && c2 == '4') tag = "f64";
    } else {
        if (c1 == '8') tag = (ch == 's')? "s8" : "u8";
        else if (c1 == '1') {
            SCM_GETC(c2, port);
            if (c2 == '6') tag = (ch == 's')? "s16" : "u16";
        }
        else if (c1 == '3') {
            SCM_GETC(c2, port);
            if (c2 == '2') tag = (ch == 's')? "s32" : "u32";
        }
        else if (c1 == '6') {
            SCM_GETC(c2, port);
            if (c2 == '4') tag = (ch == 's')? "s64" : "u64";
        }
    }
    if (tag == NULL) {
        char buf[SCM_CHAR_MAX_BYTES*4], *bufp = buf;
        *bufp++ = ch;
        SCM_CHAR_PUT(bufp, c1);
        bufp += SCM_CHAR_NBYTES(c1);
        if (c2 != SCM_CHAR_INVALID) {
            SCM_CHAR_PUT(bufp, c2);
            bufp += SCM_CHAR_NBYTES(c2);
        }
        *bufp = '\0';
        read_error(port, "invalid uniform vector tag: %s", buf);
    }
    if (Scm_ReadUvectorHook == NULL) {
        /* load srfi-4. */
        Scm_Load("srfi-4", FALSE);
        if (Scm_ReadUvectorHook == NULL)
            Scm_Error("couldn't load srfi-4 module");
    }
    return Scm_ReadUvectorHook(port, tag);
}

/*----------------------------------------------------------------
 * Initialization
 */

void Scm__InitRead(void)
{
    ScmObj sym_reader_ctor = SCM_INTERN("define-reader-ctor");
    read_ctor_table = SCM_HASHTABLE(Scm_MakeHashTable(SCM_HASH_ADDRESS,
                                                      NULL, 0));
    Scm_DefineReaderCtor(sym_reader_ctor,
                         Scm_MakeSubr(reader_ctor, NULL, 2, 0,
                                      sym_reader_ctor));
}

