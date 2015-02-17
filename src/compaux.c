/*
 * compaux.c - C API bridge for the compiler
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

/* This file serves as a bridge to the compiler, which is implemented
   in Scheme (see compile.scm) */

#include <stdlib.h>
#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/vm.h"
#include "gauche/vminsn.h"
#include "gauche/class.h"
#include "gauche/code.h"
#include "gauche/priv/builtin-syms.h"

/*
 * Syntax
 */


/*
 * Compiler Entry
 */

static ScmGloc *compile_gloc = NULL;
static ScmGloc *compile_partial_gloc = NULL;
static ScmGloc *compile_finish_gloc = NULL;
static ScmGloc *init_compiler_gloc = NULL;

static ScmInternalMutex compile_finish_mutex;

ScmObj Scm_Compile(ScmObj program, ScmObj env)
{
    return Scm_ApplyRec2(SCM_GLOC_GET(compile_gloc), program, env);
}

ScmObj Scm_CompilePartial(ScmObj program, ScmObj env)
{
    return Scm_ApplyRec2(SCM_GLOC_GET(compile_partial_gloc), program, env);
}

void Scm_CompileFinish(ScmCompiledCode *cc)
{
    if (cc->code == NULL) {
        SCM_INTERNAL_MUTEX_LOCK(compile_finish_mutex);
        SCM_UNWIND_PROTECT {
            if (cc->code == NULL) {
                Scm_ApplyRec1(SCM_GLOC_GET(compile_finish_gloc), SCM_OBJ(cc));
            }
        }
        SCM_WHEN_ERROR {
            SCM_INTERNAL_MUTEX_UNLOCK(compile_finish_mutex);
            SCM_NEXT_HANDLER;
        }
        SCM_END_PROTECT;
        SCM_INTERNAL_MUTEX_UNLOCK(compile_finish_mutex);
    }
}

/*-------------------------------------------------------------
 * Syntactic closure object
 */

static void synclo_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    Scm_Printf(port, "#<syntactic-closure %S>",
               SCM_SYNTACTIC_CLOSURE(obj)->expr);
}

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_SyntacticClosureClass, synclo_print);

ScmObj Scm_MakeSyntacticClosure(ScmObj env, ScmObj literals, ScmObj expr)
{
    ScmSyntacticClosure *s = SCM_NEW(ScmSyntacticClosure);
    SCM_SET_CLASS(s, SCM_CLASS_SYNTACTIC_CLOSURE);
    s->env = env;
    s->literals = literals;
    s->expr = expr;
    return SCM_OBJ(s);
}

static ScmObj synclo_env_get(ScmObj obj)
{
    return SCM_SYNTACTIC_CLOSURE(obj)->env;
}

static ScmObj synclo_literals_get(ScmObj obj)
{
    return SCM_SYNTACTIC_CLOSURE(obj)->literals;
}

static ScmObj synclo_expr_get(ScmObj obj)
{
    return SCM_SYNTACTIC_CLOSURE(obj)->expr;
}

static ScmClassStaticSlotSpec synclo_slots[] = {
    SCM_CLASS_SLOT_SPEC("env", synclo_env_get, NULL),
    SCM_CLASS_SLOT_SPEC("literals", synclo_literals_get, NULL),
    SCM_CLASS_SLOT_SPEC("expr", synclo_expr_get, NULL),
    SCM_CLASS_SLOT_SPEC_END()
};

/*-------------------------------------------------------------
 * Identifier object
 */

static void identifier_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    ScmIdentifier *id = SCM_IDENTIFIER(obj);
    /* We may want to have an external identifier syntax, so that an
       identifier can be written out and then read back.  It will be
       convenient if we can embed a reference to other module's global
       binding directly in the program.  However, it can also breaches
       module-based sandbox implementation, so further consideration is
       required.
    */
    Scm_Printf(port, "#<identifier %S#%S.%x>",
               id->module->name, id->name, SCM_WORD(id));
}

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_IdentifierClass, identifier_print);

static ScmObj get_binding_frame(ScmObj var, ScmObj env)
{
    ScmObj frame, fp;
    SCM_FOR_EACH(frame, env) {
        if (!SCM_PAIRP(SCM_CAR(frame))) continue;
        SCM_FOR_EACH(fp, SCM_CDAR(frame)) {
            if (SCM_CAAR(fp) == var) return frame;
        }
    }
    return SCM_NIL;
}

ScmObj Scm_MakeIdentifier(ScmObj name, ScmModule *mod, ScmObj env)
{
    ScmIdentifier *id = SCM_NEW(ScmIdentifier);
    SCM_SET_CLASS(id, SCM_CLASS_IDENTIFIER);
    id->name = name;
    id->module = mod? mod : SCM_CURRENT_MODULE();
    id->env = (env == SCM_NIL)? SCM_NIL : get_binding_frame(SCM_OBJ(name), env);
    return SCM_OBJ(id);
}

ScmIdentifier *Scm_OutermostIdentifier(ScmIdentifier *id)
{
    while (SCM_IDENTIFIERP(id->name)) {
        id = SCM_IDENTIFIER(SCM_IDENTIFIER(id)->name);
    }
    return id;
}

ScmSymbol *Scm_UnwrapIdentifier(ScmIdentifier *id)
{
    ScmObj z = Scm_OutermostIdentifier(id)->name;
    SCM_ASSERT(SCM_SYMBOLP(z));
    return SCM_SYMBOL(z);
}

/* returns global binding of the identifier */
ScmGloc *Scm_IdentifierGlobalBinding(ScmIdentifier *id)
{
    ScmIdentifier *z = Scm_OutermostIdentifier(id);
    return Scm_FindBinding(z->module, SCM_SYMBOL(z->name), 0);
}

/* returns true if SYM has the same binding with ID in ENV. */
int Scm_IdentifierBindingEqv(ScmIdentifier *id, ScmSymbol *sym, ScmObj env)
{
    ScmObj bf = get_binding_frame(SCM_OBJ(sym), env);
    return (bf == id->env);
}

ScmObj Scm_CopyIdentifier(ScmIdentifier *orig)
{
    ScmIdentifier *id = SCM_NEW(ScmIdentifier);
    SCM_SET_CLASS(id, SCM_CLASS_IDENTIFIER);
    id->name = orig->name;
    id->module = orig->module;
    id->env = orig->env;
    return SCM_OBJ(id);
}

static ScmObj identifier_name_get(ScmObj obj)
{
    return SCM_OBJ(SCM_IDENTIFIER(obj)->name);
}

static void   identifier_name_set(ScmObj obj, ScmObj val)
{
    if (!SCM_SYMBOLP(val) && !SCM_IDENTIFIERP(val)) {
        Scm_Error("symbol or identifier required, but got %S", val);
    }
    SCM_IDENTIFIER(obj)->name = val;
}

static ScmObj identifier_module_get(ScmObj obj)
{
    return SCM_OBJ(SCM_IDENTIFIER(obj)->module);
}

static void   identifier_module_set(ScmObj obj, ScmObj val)
{
    if (!SCM_MODULEP(val)) {
        Scm_Error("module required, but got %S", val);
    }
    SCM_IDENTIFIER(obj)->module = SCM_MODULE(val);
}

static ScmObj identifier_env_get(ScmObj obj)
{
    return SCM_IDENTIFIER(obj)->env;
}

static void   identifier_env_set(ScmObj obj, ScmObj val)
{
    if (!SCM_LISTP(val)) {
        Scm_Error("list required, but got %S", val);
    }
    SCM_IDENTIFIER(obj)->env = val;
}

static ScmClassStaticSlotSpec identifier_slots[] = {
    SCM_CLASS_SLOT_SPEC("name", identifier_name_get, identifier_name_set),
    SCM_CLASS_SLOT_SPEC("module", identifier_module_get, identifier_module_set),
    SCM_CLASS_SLOT_SPEC("env", identifier_env_get, identifier_env_set),
    SCM_CLASS_SLOT_SPEC_END()
};

/*------------------------------------------------------------------
 * Utility functions
 */

/* Convert all identifiers in form into a symbol.
   This keeps linear history to avoid entering infinite loop if
   the given form is circular; but it doens't recover the shared
   substricture. */
static ScmObj unwrap_rec(ScmObj form, ScmObj history)
{
    if (!SCM_PTRP(form)) return form;
    if (!SCM_FALSEP(Scm_Memq(form, history))) return form;

    ScmObj newh;

    if (SCM_PAIRP(form)) {
        ScmObj ca, cd;
        newh = Scm_Cons(form, history);
        ca = unwrap_rec(SCM_CAR(form), newh);
        cd = unwrap_rec(SCM_CDR(form), newh);
        if (ca == SCM_CAR(form) && cd == SCM_CDR(form)) {
            return form;
        } else {
            return Scm_Cons(ca, cd);
        }
    }
    if (SCM_IDENTIFIERP(form)) {
        return SCM_OBJ(Scm_UnwrapIdentifier(SCM_IDENTIFIER(form)));
    }
    if (SCM_VECTORP(form)) {
        int len = SCM_VECTOR_SIZE(form);
        ScmObj *pelt = SCM_VECTOR_ELEMENTS(form);
        newh = Scm_Cons(form, history);
        for (int i=0; i<len; i++, pelt++) {
            ScmObj elt = unwrap_rec(*pelt, newh);
            if (elt != *pelt) {
                ScmObj newvec = Scm_MakeVector(len, SCM_FALSE);
                pelt = SCM_VECTOR_ELEMENTS(form);
                int j;
                for (j=0; j<i; j++, pelt++) {
                    SCM_VECTOR_ELEMENT(newvec, j) = *pelt;
                }
                SCM_VECTOR_ELEMENT(newvec, i) = elt;
                for (; j<len; j++, pelt++) {
                    SCM_VECTOR_ELEMENT(newvec, j) = unwrap_rec(*pelt, newh);
                }
                return newvec;
            }
        }
        return form;
    }
    return form;
}

ScmObj Scm_UnwrapSyntax(ScmObj form)
{
    return unwrap_rec(form, SCM_NIL);
}

/*===================================================================
 * Initializer
 */

#define INIT_GLOC(gloc, name, mod)                                      \
    do {                                                                \
        gloc = Scm_FindBinding(mod, SCM_SYMBOL(SCM_INTERN(name)),       \
                               SCM_BINDING_STAY_IN_MODULE);             \
        if (gloc == NULL) {                                             \
            Scm_Panic("no " name " procedure in gauche.internal");      \
        }                                                               \
    } while (0)

void Scm__InitCompaux(void)
{
    ScmModule *g = Scm_GaucheModule();
    ScmModule *gi = Scm_GaucheInternalModule();

    Scm_InitStaticClass(SCM_CLASS_SYNTACTIC_CLOSURE, "<syntactic-closure>", g,
                        synclo_slots, 0);
    Scm_InitStaticClass(SCM_CLASS_IDENTIFIER, "<identifier>", g,
                        identifier_slots, 0);

    SCM_INTERNAL_MUTEX_INIT(compile_finish_mutex);

    /* Grab the entry points of compile.scm */
    INIT_GLOC(init_compiler_gloc,   "init-compiler", gi);
    INIT_GLOC(compile_gloc,         "compile",       gi);
    INIT_GLOC(compile_partial_gloc, "compile-partial", gi);
    INIT_GLOC(compile_finish_gloc,  "compile-finish",  gi);

    Scm_ApplyRec0(SCM_GLOC_GET(init_compiler_gloc));
}
