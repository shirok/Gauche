/*
 * symbol.c - symbol implementation
 *
 *  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, ditribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: symbol.c,v 1.6 2001-02-05 09:46:26 shiro Exp $
 */

#include "gauche.h"

/*
 * Symbols
 */

static int symbol_print(ScmObj obj, ScmPort *port, int mode)
{
    SCM_PUTS(SCM_SYMBOL(obj)->name, port);
    return SCM_STRING_LENGTH(SCM_SYMBOL(obj)->name);
}

SCM_DEFCLASS(Scm_SymbolClass, "<symbol>", symbol_print, SCM_CLASS_DEFAULT_CPL);

#define INITSYM(sym, nam)                       \
    sym = SCM_NEW(ScmSymbol);                   \
    SCM_SET_CLASS(sym, SCM_CLASS_SYMBOL);       \
    sym->name = SCM_STRING(nam)

/* These two are global resource.  Must be protected in MT environment. */
static ScmHashTable *obtable;   /* name -> symbol mapper */
static int gensym_count = 0;

ScmObj Scm_Intern(ScmString *name)
{
    ScmHashEntry *e = Scm_HashTableGet(obtable, SCM_OBJ(name));
    if (e) return e->value;
    else {
        ScmSymbol *sym;
        INITSYM(sym, Scm_CopyString(name));
        Scm_HashTablePut(obtable, SCM_OBJ(name), SCM_OBJ(sym));
        return SCM_OBJ(sym);
    }
}

/* Default prefix string. */
SCM_DEFINE_STRING_CONST(default_prefix, "G", 1, 1);

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

/*
 * GLOCs
 */

static int gloc_print(ScmObj obj, ScmPort *port, int mode)
{
    ScmGloc *g = SCM_GLOC(obj);
    return Scm_Printf(port, "#<gloc %A::%S>",
                      g->module->name, g->name);
}

SCM_DEFCLASS(Scm_GlocClass, "<gloc>", gloc_print, SCM_CLASS_DEFAULT_CPL);

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

#define SYMINIT   { SCM_CLASS_SYMBOL, NULL }
#define SYMREG(var, nam)                                                \
    do {                                                                \
        ScmObj snam = Scm_MakeStringConst(nam, -1, -1);                 \
        var.name = SCM_STRING(snam);                                    \
        Scm_HashTablePut(obtable, SCM_OBJ(snam), SCM_OBJ(&var));        \
    } while (0)

ScmSymbol ScmQquote            = SYMINIT;
ScmSymbol ScmQquasiquote       = SYMINIT;
ScmSymbol ScmQunquote          = SYMINIT;
ScmSymbol ScmQunquoteSplicing  = SYMINIT;
ScmSymbol ScmQdefine           = SYMINIT;
ScmSymbol ScmQlambda           = SYMINIT;
ScmSymbol ScmQif               = SYMINIT;
ScmSymbol ScmQset              = SYMINIT;
ScmSymbol ScmQlet              = SYMINIT;
ScmSymbol ScmQletStar          = SYMINIT;
ScmSymbol ScmQletrec           = SYMINIT;
ScmSymbol ScmQbegin            = SYMINIT;
ScmSymbol ScmQwhen             = SYMINIT;
ScmSymbol ScmQunless           = SYMINIT;
ScmSymbol ScmQand              = SYMINIT;
ScmSymbol ScmQor               = SYMINIT;
ScmSymbol ScmQcond             = SYMINIT;
ScmSymbol ScmQcase             = SYMINIT;
ScmSymbol ScmQelse             = SYMINIT;
ScmSymbol ScmQyields           = SYMINIT;
ScmSymbol ScmQdo               = SYMINIT;
ScmSymbol ScmQdelay            = SYMINIT;
ScmSymbol ScmQmacroExpand      = SYMINIT;

ScmSymbol ScmQcons             = SYMINIT;
ScmSymbol ScmQcar              = SYMINIT;
ScmSymbol ScmQcdr              = SYMINIT;
ScmSymbol ScmQlist             = SYMINIT;
ScmSymbol ScmQeq               = SYMINIT;
ScmSymbol ScmQeqv              = SYMINIT;
ScmSymbol ScmQequal            = SYMINIT;
ScmSymbol ScmQmemv             = SYMINIT;

void Scm__InitSymbol(void)
{
    obtable = SCM_HASHTABLE(Scm_MakeHashTable(SCM_HASH_STRING, NULL, 2000));

    SYMREG(ScmQquote, "quote");
    SYMREG(ScmQquasiquote, "quasiquote");
    SYMREG(ScmQunquote, "unquote");
    SYMREG(ScmQunquoteSplicing, "unquote-splicing");
    SYMREG(ScmQdefine, "define");
    SYMREG(ScmQlambda, "lambda");
    SYMREG(ScmQif, "if");
    SYMREG(ScmQset, "set!");
    SYMREG(ScmQlet, "let");
    SYMREG(ScmQletStar, "let*");
    SYMREG(ScmQletrec, "letrec");
    SYMREG(ScmQbegin, "begin");
    SYMREG(ScmQwhen, "when");
    SYMREG(ScmQunless, "unless");
    SYMREG(ScmQand,    "and");
    SYMREG(ScmQor,     "or");
    SYMREG(ScmQcond,   "cond");
    SYMREG(ScmQcase,   "case");
    SYMREG(ScmQelse,   "else");
    SYMREG(ScmQyields, "=>");
    SYMREG(ScmQdo,     "do");
    SYMREG(ScmQdelay,  "delay");
    SYMREG(ScmQmacroExpand, "%macro-expand");

    SYMREG(ScmQcons,   "cons");
    SYMREG(ScmQcar,    "car");
    SYMREG(ScmQcdr,    "cdr");
    SYMREG(ScmQlist,   "list");
    SYMREG(ScmQeq,     "eq?");
    SYMREG(ScmQeqv,    "eqv?");
    SYMREG(ScmQequal,  "equal");
    SYMREG(ScmQmemv,   "memv");
}
