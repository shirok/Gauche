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
 *  $Id: symbol.c,v 1.10 2001-03-17 08:22:11 shiro Exp $
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
    ScmObj h = SCM_NIL, t;
    Scm_HashIterInit(obtable, &iter);
    while ((e = Scm_HashIterNext(&iter)) != NULL) {
        if (!SCM_FALSEP(Scm_StringContains(SCM_STRING(e->key), substr)))
            SCM_APPEND1(h, t, e->value);
    }
    return h;
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

#define SYMREG(var, nam)                                                \
    do {                                                                \
        ScmObj snam = Scm_MakeStringConst(nam, -1, -1);                 \
        var.name = SCM_STRING(snam);                                    \
        Scm_HashTablePut(obtable, SCM_OBJ(snam), SCM_OBJ(&var));        \
    } while (0)

#define DEFSYM(cname, sname) \
    ScmSymbol cname = { SCM_CLASS_SYMBOL, NULL }
#include "gauche/predef-syms.h"
#undef DEFSYM

void Scm__InitSymbol(void)
{
    obtable = SCM_HASHTABLE(Scm_MakeHashTable(SCM_HASH_STRING, NULL, 2000));

#define DEFSYM(cname, sname) SYMREG(cname, sname)
#include "gauche/predef-syms.h"
#undef DEFSYM
}
