/*
 * symbol.c - symbol implementation
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
 *  $Id: symbol.c,v 1.16 2001-04-22 07:51:29 shiro Exp $
 */

#include "gauche.h"

/*-----------------------------------------------------------
 * Symbols
 */

static void symbol_print(ScmObj obj, ScmPort *port, ScmWriteContext *);
SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_SymbolClass, symbol_print);

#define INITSYM(sym, nam)                       \
    sym = SCM_NEW(ScmSymbol);                   \
    SCM_SET_CLASS(sym, SCM_CLASS_SYMBOL);       \
    sym->name = SCM_STRING(nam)

/* These two are global resource.  Must be protected in MT environment. */
static ScmHashTable *obtable;   /* name -> symbol mapper */
static int gensym_count = 0;

/* Intern */

ScmObj Scm_Intern(ScmString *name)
{
    ScmHashEntry *e = Scm_HashTableGet(obtable, SCM_OBJ(name));
    if (e) return e->value;
    else {
        ScmObj n = Scm_CopyString(name);
        ScmSymbol *sym;
        INITSYM(sym, n);
        Scm_HashTablePut(obtable, n, SCM_OBJ(sym));
        return SCM_OBJ(sym);
    }
}

/* Returns a list of symbols whose name contains substr */
ScmObj Scm_Apropos(ScmString *substr)
{
    ScmHashIter iter;
    ScmHashEntry *e;
    ScmObj h = SCM_NIL, t = SCM_NIL;
    Scm_HashIterInit(obtable, &iter);
    while ((e = Scm_HashIterNext(&iter)) != NULL) {
        if (!SCM_FALSEP(Scm_StringContains(SCM_STRING(e->key), substr)))
            SCM_APPEND1(h, t, e->value);
    }
    return h;
}

/* Default prefix string. */
static SCM_DEFINE_STRING_CONST(default_prefix, "G", 1, 1);

/* Returns uninterned symbol.
   PREFIX can be NULL*/
ScmObj Scm_Gensym(ScmString *prefix)
{
    ScmString *name;
    ScmSymbol *sym;
    char numbuf[50];
    int nc;

    if (prefix == NULL) prefix = &default_prefix;
    nc = snprintf(numbuf, 50, "%d", gensym_count++);
    name = SCM_STRING(Scm_StringAppendC(prefix, numbuf, nc, nc));
    INITSYM(sym, name);
    return SCM_OBJ(sym);
}

/* Print */

/* table of special chars.
   bit 0: bad char for symbol to begin with
   bit 1: bad char for symbol to contain
   bit 2: bad char for symbol, and should be written as \nnn
   bit 3: bad char for symbol, and should be written as \c */
static char special[] = {
 /* NUL .... */
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
 /* .... */
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
 /*    !  "  #  $  %  &  '  (  )  *  +  ,  -  .  /  */
    3, 0, 3, 3, 0, 0, 0, 3, 3, 3, 0, 1, 3, 1, 1, 0,
 /* 0  1  2  3  4  5  6  7  8  9  :  ;  <  =  >  ?  */
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 0, 0, 0, 0,
 /* @  A  B  C  D  E  F  G  H  I  J  K  L  M  N  O  */
    1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
 /* P  Q  R  S  T  U  V  W  X  Y  Z  [  \  ]  ^  _  */
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 11,3, 0, 0,
 /* `  a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  */
    3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
 /* p  q  r  s  t  u  v  w  x  y  z  {  |  }  ~  ^? */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 11,3, 0, 7
};

static void symbol_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    if (ctx->mode == SCM_WRITE_DISPLAY) {
        SCM_PUTS(SCM_SYMBOL(obj)->name, port);
    } else {
        /* See if we have special characters, and use |-escape if necessary. */
        /* TODO: For now, we regard chars over 0x80 is all "printable".
           Need a more consistent mechanism. */
        ScmString *snam = SCM_SYMBOL(obj)->name;
        const char *p = SCM_STRING_START(snam), *q;
        int siz = SCM_STRING_SIZE(snam), i;
        int escape = FALSE;
        
        if (siz == 0) {         /* special case */
            SCM_PUTCSTR("||", port);
            return;
        }
        if (siz == 1 && (*p == '+' || *p == '-')) {
            SCM_PUTC((unsigned)*p, port);
            return;
        }
        if ((unsigned int)*p < 128 && (special[(unsigned int)*p]&1)) {
            escape = TRUE;
        } else {
            for (i=0, q=p; i<siz; i++, q++) {
                if ((unsigned int)*q < 128 && (special[(unsigned int)*q]&2)) {
                    escape = TRUE;
                    break;
                }
            }
        }
        if (escape) {
            SCM_PUTC('|', port);
            for (q=p; q<p+siz; ) {
                unsigned int ch;
                SCM_STR_GETC(q, ch);
                q += SCM_CHAR_NBYTES(ch);
                if (ch < 128) {
                    if (special[ch] & 8) {
                        SCM_PUTC('\\', port);
                        SCM_PUTC(ch, port);
                    } else if (special[ch] & 4) {
                        Scm_Printf(port, "\\x%02x", ch);
                    } else {
                        SCM_PUTC(ch, port);
                    }
                } else {
                    SCM_PUTC(ch, port);
                }
            }
            SCM_PUTC('|', port);
            return;
        } else {
            SCM_PUTS(snam, port);
        }
    }
}

/*---------------------------------------------------------------
 * GLOCs
 */

static void gloc_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    ScmGloc *g = SCM_GLOC(obj);
    Scm_Printf(port, "#<gloc %A::%S>", g->module->name, g->name);
}

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_GlocClass, gloc_print);

ScmObj Scm_MakeGloc(ScmSymbol *sym, ScmModule *module)
{
    ScmGloc *g = SCM_NEW(ScmGloc);
    SCM_SET_CLASS(g, &Scm_GlocClass);
    g->name = sym;
    g->module = module;
    g->value = SCM_UNBOUND;
    return SCM_OBJ(g);
}

/*
 * Initialization
 */

#define SYMREG(var, nam)                                                \
    do {                                                                \
        ScmObj snam = Scm_MakeStringConst(nam, -1, -1);                 \
        var.name = SCM_STRING(snam);                                    \
        Scm_HashTablePut(obtable, SCM_OBJ(snam), SCM_OBJ(&var));        \
    } while (0)

#define DEFSYM(cname, sname) \
    ScmSymbol cname = { { SCM_CLASS_SYMBOL }, NULL }
#include "gauche/predef-syms.h"
#undef DEFSYM

void Scm__InitSymbol(void)
{
    obtable = SCM_HASHTABLE(Scm_MakeHashTable(SCM_HASH_STRING, NULL, 2000));

#define DEFSYM(cname, sname) SYMREG(cname, sname)
#include "gauche/predef-syms.h"
#undef DEFSYM
}
