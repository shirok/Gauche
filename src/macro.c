/*
 * macro.c - macro implementation
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
 *  $Id: macro.c,v 1.16 2001-03-04 08:17:28 shiro Exp $
 */

#include "gauche.h"
#include "gauche/macro.h"

/*===================================================================
 * Syntax object
 */

static int syntax_print(ScmObj obj, ScmPort *port, int mode)
{
    return Scm_Printf(port, "#<syntax %A>", SCM_SYNTAX(obj)->name);
}

SCM_DEFCLASS(Scm_SyntaxClass, "<syntax>", syntax_print, SCM_CLASS_DEFAULT_CPL);

ScmObj Scm_MakeSyntax(ScmSymbol *name, ScmCompileProc compiler, void *data)
{
    ScmSyntax *s = SCM_NEW(ScmSyntax);
    SCM_SET_CLASS(s, SCM_CLASS_SYNTAX);
    s->name = name;
    s->compiler = compiler;
    s->data = data;
    return SCM_OBJ(s);
}

/*===================================================================
 * SyntaxPattern object
 *   Internal object to construct pattern matcher
 */

static int pattern_print(ScmObj obj, ScmPort *port, int mode)
{
    return Scm_Printf(port, "#<pattern:%d%S %S%s>",
                      SCM_SYNTAX_PATTERN(obj)->level,
                      SCM_SYNTAX_PATTERN(obj)->vars,
                      SCM_SYNTAX_PATTERN(obj)->pattern,
                      SCM_SYNTAX_PATTERN(obj)->repeat? " ..." : "");
}

SCM_DEFCLASS(Scm_SyntaxPatternClass, "<syntax-pattern>",
             pattern_print, SCM_CLASS_DEFAULT_CPL);

ScmSyntaxPattern *make_syntax_pattern(int level, int repeat)
{
    ScmSyntaxPattern *p = SCM_NEW(ScmSyntaxPattern);
    SCM_SET_CLASS(p, SCM_CLASS_SYNTAX_PATTERN);
    p->pattern = SCM_NIL;
    p->vars = SCM_NIL;
    p->level = level;
    p->repeat = repeat;
    return p;
}

/*===================================================================
 * SyntaxRules object
 *   Internal object to construct pattern matcher
 */

static int synrule_print(ScmObj obj, ScmPort *port, int mode)
{
    int nc = 0, i;
    ScmSyntaxRules *r = SCM_SYNTAX_RULES(obj);
    ScmSyntaxRuleBranch *br;

    nc += Scm_Printf(port, "#<syntax-rules(%d)\n", r->numRules);
    for (i = 0; i < r->numRules; i++) {
        nc += Scm_Printf(port, "%2d: (numPvars=%d, maxLevel=%d)\n",
                         i, r->rules[i].numPvars, r->rules[i].maxLevel);
        nc += Scm_Printf(port, "   pattern  = %S\n", r->rules[i].pattern);
        nc += Scm_Printf(port, "   template = %S\n", r->rules[i].template);
    }
    nc += Scm_Printf(port, ">");
    return nc;
}

SCM_DEFCLASS(Scm_SyntaxRulesClass, "<syntax-rules>", synrule_print,
             SCM_CLASS_DEFAULT_CPL);

ScmSyntaxRules *make_syntax_rules(int nr) 
{
    ScmSyntaxRules *r = SCM_NEW2(ScmSyntaxRules *,
                                 sizeof(ScmSyntaxRules)+(nr-1)*sizeof(ScmSyntaxRuleBranch));
    SCM_SET_CLASS(r, SCM_CLASS_SYNTAX_RULES);
    r->numRules = nr;
    return r;
}

/*===================================================================
 * Traditional Macro
 */

/* TODO: how to retain debug info? */
/* TODO: better error message on syntax error (macro invocation with
   bad number of arguments) */

static ScmObj macro_transform(ScmObj form, ScmObj env, int ctx, void *data)
{
    ScmObj proc = SCM_OBJ(data);
    ScmObj newform = Scm_Apply(proc, form);
    return Scm_Compile(newform, env, ctx);
}

ScmObj Scm_MakeMacroTransformer(ScmSymbol *name, ScmProcedure *proc)
{
    return Scm_MakeSyntax(name, macro_transform, (void*)proc);
}

/*===================================================================
 * R5RS Macro
 */

/* Keeping hygienic reference
 *
 *  - symbols which a template inserts into the expanded form are
 *    converted to identifiers at the macro definition time, encapsulating
 *    the defining environment of the macro.   So it doesn't interfere
 *    with the macro call environment.
 *
 *  - literal symbols provided to the syntax-rules are also converted
 *    to identifiers encapsulating the defining environment, and the
 *    environment information is used when comparing with the symbols
 *    in the macro call.
 *
 *  - symbols in the macro call is treated as they are.  Since the result
 *    of macro expansion is immediately compiled in the macro call 
 *    environment, those symbols can refer proper bindings.
 */

/*-------------------------------------------------------------------
 * pattern language compiler
 *   - convert literals into identifiers
 *   - recognize repeatable subpatterns and replace it to SyntaxPattern node.
 *   - convert free symbols in the template into identifiers
 *   - convert pattern variables into LREF object.
 */
/* TODO: avoid unnecessary consing as much as possible */

/* context of pattern traversal */
typedef struct {                
    ScmObj name;                /* name of this macro (for error msg)*/
    ScmObj form;                /* form being compiled (for error msg) */
    ScmObj literals;            /* list of literal identifiers */
    ScmObj pvars;               /* list of (pvar . pvref) */
    int pvcnt;                  /* counter of pattern variables */
    int maxlev;                 /* maximum level */
    ScmObj tvars;               /* list of identifies inserted in template */
    ScmObj env;                 /* compiler env of this macro definition */
} PatternContext;

#define PVREF_P(pvref) \
    (SCM_VM_INSNP(pvref)&&(SCM_VM_INSN_CODE(pvref)==SCM_VM_LREF))
#define PVREF_LEVEL(pvref)     SCM_VM_INSN_ARG0(pvref)
#define PVREF_COUNT(pvref)     SCM_VM_INSN_ARG1(pvref)

/* add pattern variable pvar.  called when compiling a pattern */
static inline ScmObj add_pvar(PatternContext *ctx,
                              ScmSyntaxPattern *pat,
                              ScmObj pvar)
{
    ScmObj pvref = SCM_VM_INSN2(SCM_VM_LREF, pat->level, ctx->pvcnt);
    if (!SCM_FALSEP(Scm_Memq(pvar, ctx->pvars))) {
        Scm_Error("pattern variable %S appears more than once in the macro definition of %S: %S", 
                  pvar, ctx->name, ctx->form);
    }
    ctx->pvcnt++;
    ctx->pvars = Scm_Acons(pvar, pvref, ctx->pvars);
    pat->vars = Scm_Cons(pvref, pat->vars);
    return pvref;
}

/* returns pvref corresponds to the given pvar in template compilation.
   if pvar is not a valid pvar, returns pvar itself. */
static inline ScmObj pvar_to_pvref(PatternContext *ctx,
                                   ScmSyntaxPattern *pat,
                                   ScmObj pvar)
{
    ScmObj q = Scm_Assq(pvar, ctx->pvars), pvref;
    if (!SCM_PAIRP(q)) return pvar;
    pvref = SCM_CDR(q);
    if (PVREF_LEVEL(pvref) != pat->level) {
        Scm_Error("%S: Pattern variable %S is used in wrong level: %S",
                  ctx->name, pvar, ctx->form);
    }
    return pvref;
}

static inline ScmObj pvref_to_pvar(PatternContext *ctx, ScmObj pvref)
{
    int count = PVREF_COUNT(pvref);
    ScmObj q = Scm_ListRef(ctx->pvars, count);
    SCM_ASSERT(SCM_PAIRP(q));
    return SCM_CAR(q);
}

/* search an identifier with name NAME from a list of identifiers */
static inline ScmObj id_memq(ScmObj name, ScmObj list)
{
    ScmObj lp;
    SCM_FOR_EACH(lp, list) {
        if (SCM_OBJ(SCM_IDENTIFIER(SCM_CAR(lp))->name) == name)
            return SCM_CAR(lp);
    }
    return SCM_FALSE;
}

#define ELLIPSIS_FOLLOWING(Pat) \
    (SCM_PAIRP(SCM_CDR(Pat)) && SCM_CADR(Pat)==SCM_SYM_ELLIPSIS)

#define BAD_ELLIPSIS(Ctx)                                               \
    Scm_Error("Bad ellipsis usage in macro definition of %S: %S",       \
               Ctx->name, Ctx->form)

/* convert literal symbols into identifiers */
static ScmObj preprocess_literals(ScmObj literals, ScmObj env)
{
    ScmObj lp, h = SCM_NIL, t;
    SCM_FOR_EACH(lp, literals) {
        ScmObj lit = SCM_CAR(lp);
        if (SCM_IDENTIFIERP(lit))
            SCM_APPEND1(h, t, lit);
        else if (SCM_SYMBOLP(lit))
            SCM_APPEND1(h, t, Scm_MakeIdentifier(SCM_SYMBOL(lit), env));
        else
            Scm_Error("literal list contains non-symbol: %S", literals);
    }
    if (!SCM_NULLP(lp))
        Scm_Error("bad literal list in syntax-rules: %S", literals);
    return h;
}

/* compile a pattern or a template.
   In a pattern, replace literal symbols for identifiers; leave
   non-literal symbols (i.e. pattern variables) as they are, but
   records it's presence in the context.   Also, when encounters
   a repeatable subpattern, replace it with SyntaxPattern node.
   In a template, replace symbols for identifiers except pattern variables.
*/

static ScmObj compile_rule1(ScmObj form,
                            ScmSyntaxPattern *spat,
                            PatternContext *ctx,
                            int patternp)
{
    if (SCM_PAIRP(form)) {
        ScmObj pp, h = SCM_NIL, t;
        SCM_FOR_EACH(pp, form) {
            if (ELLIPSIS_FOLLOWING(pp)) {
                ScmSyntaxPattern *nspat;
                if (patternp && !SCM_NULLP(SCM_CDDR(pp))) BAD_ELLIPSIS(ctx);
                nspat = make_syntax_pattern(spat->level+1, TRUE);
                if (ctx->maxlev <= spat->level) ctx->maxlev++;
                nspat->pattern = compile_rule1(SCM_CAR(pp), nspat, ctx,
                                               patternp);
                SCM_APPEND1(h, t, SCM_OBJ(nspat));
                if (!patternp && SCM_NULLP(nspat->vars)) {
                    Scm_Error("in definition of macro %S: "
                              "template contains repetition of constant form: %S",
                              ctx->name, form);
                }
                spat->vars = Scm_Append2(spat->vars, nspat->vars);
                pp = SCM_CDR(pp);
            } else {
                SCM_APPEND1(h, t,
                            compile_rule1(SCM_CAR(pp), spat, ctx, patternp));
            }
        }
        if (!SCM_NULLP(pp))
            SCM_APPEND1(h, t, compile_rule1(pp, spat, ctx, patternp));
        return h;
    }
    else if (SCM_VECTORP(form)) {
        /* TODO: this is a sloppy implementation.
           Eliminate intermediate list structure! */
        ScmObj l = Scm_VectorToList(SCM_VECTOR(form));
        return Scm_ListToVector(compile_rule1(l, spat, ctx, patternp));
    }
    else if (patternp && SCM_IDENTIFIERP(form)) {
        /* this happens in a macro produced by another macro */
        form = SCM_OBJ(SCM_IDENTIFIER(form)->name);
    }
    if (SCM_SYMBOLP(form)) {
        ScmObj q;
        if (form == SCM_SYM_ELLIPSIS) BAD_ELLIPSIS(ctx);
        if (!SCM_FALSEP(q = id_memq(form, ctx->literals))) return q;

        if (patternp) {
            return add_pvar(ctx, spat, form);
        } else {
            ScmObj id, pvref = pvar_to_pvref(ctx, spat, form);
            if (pvref == form) {
                /* form is not a pattern variable.  make it an identifier. */
                if (!SCM_FALSEP(q = id_memq(form, ctx->tvars))) return q;
                id = Scm_MakeIdentifier(SCM_SYMBOL(form), ctx->env);
                ctx->tvars = Scm_Cons(id, ctx->tvars);
                return id;
            } else {
                spat->vars = Scm_Cons(pvref, spat->vars);
            }
            return pvref;
        }
    }
    return form;
}

/* compile rules into ScmSyntaxRules structure */
static ScmSyntaxRules *compile_rules(ScmObj name,
                                     ScmObj literals,
                                     ScmObj rules,
                                     ScmObj env) /* compiler env */
{
    PatternContext ctx;
    ScmSyntaxPattern *pat, *tmpl;
    ScmSyntaxRules *sr;
    ScmSyntaxRuleBranch *branch;
    ScmObj rp;
    int numRules = Scm_Length(rules), i;

    if (numRules < 1) goto badform;
    if (Scm_Length(literals) < 0) goto badform;

    ctx.name = name;
    ctx.literals = preprocess_literals(literals, env);
    ctx.env = env;

    sr = make_syntax_rules(numRules);
    sr->name = name;
    sr->numRules = numRules;
    sr->maxNumPvars = 0;
    for (i=0, rp = rules; i < numRules; i++, rp = SCM_CDR(rp)) {
        ScmObj rule = SCM_CAR(rp);
        if (Scm_Length(rule) != 2) goto badform;

        pat  = make_syntax_pattern(0, FALSE);
        tmpl = make_syntax_pattern(0, FALSE);
        ctx.pvars = SCM_NIL;
        ctx.tvars = SCM_NIL;
        ctx.pvcnt = 0;
        ctx.maxlev = 0;

        ctx.form = SCM_CAR(rule);
        if (!SCM_PAIRP(ctx.form)) goto badform;
        pat->pattern = compile_rule1(SCM_CDR(ctx.form), pat, &ctx, TRUE);

        ctx.form = SCM_CADR(rule);
        tmpl->pattern = compile_rule1(ctx.form, tmpl, &ctx, FALSE);

        sr->rules[i].pattern  = SCM_OBJ(pat->pattern);
        sr->rules[i].template = SCM_OBJ(tmpl->pattern);
        sr->rules[i].numPvars = ctx.pvcnt;
        sr->rules[i].maxLevel = ctx.maxlev;
        if (ctx.pvcnt > sr->maxNumPvars) sr->maxNumPvars = ctx.pvcnt;
    }
    return sr;

  badform:
    Scm_Error("malformed macro %S: %S", name,
              Scm_Cons(SCM_INTERN("syntax-rules"), Scm_Cons(literals, rules)));
    return NULL;       /* dummy */
}

/*-------------------------------------------------------------------
 * pattern language matcher
 */

/* Matchvec
 *   A sort of shallow binding technique is used to bind pattern
 *   variables with matched patterns.
 *
 *   Matchlist itself is an assoc list whose key is a pattern variable.
 *   It's value is a tree of the same depth of the pattern variable.
 *
 *   Suppose you have a pattern
 *      (?a (?b (?c ?d ...) ...) ...)
 *   In it, pattern variable ?a is level 0, ?b is 1, ?c is 2 and ?d is 3.
 *   When the pattern matches the following form:
 *      (1 (2 (3 4 5) (6)) (7 (8 9) (10 11 12)))
 *   trees bound to each pattern variables are like this:
 *
 *      ?a => 1
 *      ?b => (2 7)
 *      ?c => ((3 6) (8 10))
 *      ?d => (((4 5) ()) ((9) (11 12)))
 */

typedef struct {
    ScmObj branch;              /* current level match */
    ScmObj sprout;              /* current sprout */
    ScmObj root;                /* root of the tree */
} MatchVar;

static MatchVar *alloc_matchvec(int numPvars)
{
    return SCM_NEW2(MatchVar*, sizeof(MatchVar)*numPvars);
}

static void init_matchvec(MatchVar *mvec, int numPvars)
{
    int i;
    for (i=0; i<numPvars; i++) {
        mvec[i].branch = mvec[i].sprout = mvec[i].root = SCM_NIL;
    }
}

/* get value associated to the pvref.  if exhausted, return SCM_UNBOUND
   and set exhaust level in *exlev. */
static ScmObj get_pvref_value(ScmObj pvref, MatchVar *mvec,
                              int *indices, int *exlev)
{
    int level = PVREF_LEVEL(pvref), count = PVREF_COUNT(pvref);
    int i, j;
    ScmObj tree = mvec[count].root;
    for (i=1; i<=level; i++) {
        for (j=0; j<indices[i]; j++) {
            if (!SCM_PAIRP(tree)) {
                *exlev = i;
                return SCM_UNBOUND;
            }
            tree = SCM_CDR(tree);
        }
        if (!SCM_PAIRP(tree)) {
            *exlev = i;
            return SCM_UNBOUND;
        }
        tree = SCM_CAR(tree);
    }
    return tree;
}

/* for debug */
static void print_matchvec(MatchVar *mvec, int numPvars, ScmPort *port)
{
    int i;
    for (i=0; i<numPvars; i++) {
        Scm_Printf(port, "[%S %S %S]\n",
                   mvec[i].branch, mvec[i].sprout, mvec[i].root);
    }
}

static int match_synrule(ScmObj form, ScmObj pattern, ScmObj env,
                         MatchVar *mvec);

#define SPROUT  Scm_Cons(SCM_NIL, SCM_NIL)

/* add a new "sprout" to the given tree at the given level. */
static void grow_branch(MatchVar *rec, int level)
{
    ScmObj trunc;
    int i;
    if (level <= 1) return;
    if (rec->root == SCM_NIL) {
        rec->sprout = rec->root = SPROUT;
        if (level == 2) return;
    }
    
    trunc = rec->root;
    for (i=1; i<level-1; i++, trunc = SCM_CAR(trunc)) {
        SCM_FOR_EACH(trunc, trunc) {
            if (SCM_NULLP(SCM_CDR(trunc))) break;
        }
        if (SCM_NULLP(SCM_CAR(trunc))) {
            for (i++; i<level-1; i++, trunc = SCM_CAR(trunc)) {
                SCM_SET_CAR(trunc, SPROUT);
            }
            rec->sprout = SPROUT;
            SCM_SET_CAR(trunc, rec->sprout);
            return;
        }
    }
    SCM_FOR_EACH(trunc, trunc) {
        if (SCM_NULLP(SCM_CDR(trunc))) {
            rec->sprout = SPROUT;
            SCM_SET_CDR(trunc, rec->sprout);
            break;
        }
    }
}

static void enter_subpattern(ScmSyntaxPattern *subpat, MatchVar *mvec)
{
    ScmObj pp;
    SCM_FOR_EACH(pp, subpat->vars) {
        ScmObj pvref = SCM_CAR(pp);
        int count = PVREF_COUNT(pvref);
        grow_branch(mvec+count, subpat->level);
    }
}

static void exit_subpattern(ScmSyntaxPattern *subpat, MatchVar *mvec)
{
    ScmObj pp;
    SCM_FOR_EACH(pp, subpat->vars) {
        ScmObj pvref = SCM_CAR(pp);
        int count = PVREF_COUNT(pvref);
        if (PVREF_LEVEL(pvref) == subpat->level) {
            if (subpat->level == 1) {
                mvec[count].root = Scm_ReverseX(mvec[count].branch);
            } else {
                SCM_SET_CAR(mvec[count].sprout,
                            Scm_ReverseX(mvec[count].branch));
                mvec[count].branch = SCM_NIL;
            }
        }
    }
}

/* add pattern variable PVREF and its matched object MATCHED into MVEC */
static inline void match_insert(ScmObj pvref, ScmObj matched, MatchVar *mvec)
{
    int count = PVREF_COUNT(pvref);
    if (PVREF_LEVEL(pvref) == 0) {
        mvec[count].root = matched;
    } else {
        mvec[count].branch = Scm_Cons(matched, mvec[count].branch);
    }
}

/* see if literal identifier ID in the pattern matches the given object */
static inline int match_identifier(ScmIdentifier *id, ScmObj obj, ScmObj env)
{
    if (SCM_SYMBOLP(obj)) {
        return (id->name == SCM_SYMBOL(obj)
                && Scm_IdentifierBindingEqv(id, SCM_SYMBOL(obj), env));
    }
    if (SCM_IDENTIFIERP(obj)) {
        /*TODO: module?*/
        return (id->name == SCM_IDENTIFIER(obj)->name
                && id->env == SCM_IDENTIFIER(obj)->env);
    }
    return FALSE;
}

static inline int match_subpattern(ScmObj form, ScmSyntaxPattern *pat,
                                   ScmObj env, MatchVar *mvec)
{
    enter_subpattern(pat, mvec);
    while (SCM_PAIRP(form)) {
        if (!match_synrule(SCM_CAR(form), pat->pattern, env, mvec))
            return FALSE;
        form = SCM_CDR(form);
    }
    if (!SCM_NULLP(form)) return FALSE;
    exit_subpattern(pat, mvec);
    return TRUE;
}

/* See if form matches pattern.  If match, add matched syntax variable
   bindings to match vector and return TRUE; otherwise, return FALSE
*/
static int match_synrule(ScmObj form, ScmObj pattern, ScmObj env,
                         MatchVar *mvec)
{
    if (PVREF_P(pattern)) {
        match_insert(pattern, form, mvec);
        return TRUE;
    }
    if (SCM_IDENTIFIERP(pattern)) {
        return match_identifier(SCM_IDENTIFIER(pattern), form, env);
    }
    if (SCM_SYNTAX_PATTERN_P(pattern)) {
        return match_subpattern(form, SCM_SYNTAX_PATTERN(pattern), env, mvec);
    }
    if (SCM_PAIRP(pattern)) {
        while (SCM_PAIRP(pattern)) {
            ScmObj elt = SCM_CAR(pattern);
            if (SCM_SYNTAX_PATTERN_P(elt)) {
                return match_subpattern(form, SCM_SYNTAX_PATTERN(elt),
                                        env, mvec);
            } else if (!SCM_PAIRP(form)) {
                return FALSE;
            } else {
                if (!match_synrule(SCM_CAR(form), elt, env, mvec))
                    return FALSE;
                pattern = SCM_CDR(pattern);
                form = SCM_CDR(form);
            }
        }
        if (!SCM_NULLP(pattern))
            return match_synrule(form, pattern, env, mvec);
        else 
            return SCM_NULLP(form);
    }
    if (SCM_VECTORP(pattern)) {
        int i, plen, flen, elli;
        if (!SCM_VECTORP(form)) return FALSE;
        plen = SCM_VECTOR_SIZE(pattern);
        flen = SCM_VECTOR_SIZE(form);
        if (plen == 0) return (flen == 0);
        elli = SCM_SYNTAX_PATTERN_P(SCM_VECTOR_ELEMENT(pattern, plen-1));
        if ((!elli && plen!=flen) || (elli && plen-1>flen)) return FALSE;
        for (i=0; i < plen-elli; i++) {
            if (!match_synrule(SCM_VECTOR_ELEMENT(form, i),
                               SCM_VECTOR_ELEMENT(pattern, i),
                               env, mvec))
                return FALSE;
        }
        if (elli) {
            ScmObj h = SCM_NIL, t;
            ScmObj pat = SCM_VECTOR_ELEMENT(pattern, plen-1);
            for (i=plen-1; i<flen; i++) {
                SCM_APPEND1(h, t, SCM_VECTOR_ELEMENT(form, i));
            }
            return match_subpattern(h, SCM_SYNTAX_PATTERN(pat), env, mvec);
        }
        return TRUE;
    }

    /* literal */
    return !SCM_FALSEP(Scm_EqualP(pattern, form));
}

/*-------------------------------------------------------------------
 * pattern language transformer
 */

/* If a pattern variable is exhausted, SCM_UNDEFINED is returned. */
static ScmObj realize_template_rec(ScmObj template,
                                   MatchVar *mvec,
                                   int level,
                                   int *indices,
                                   ScmObj *idlist,
                                   int *exlev)
{
    if (SCM_PAIRP(template)) {
        ScmObj h = SCM_NIL, t, r, e;
        while (SCM_PAIRP(template)) {
            e = SCM_CAR(template);
            if (SCM_SYNTAX_PATTERN_P(e)) {
                r = realize_template_rec(e, mvec, level, indices, idlist, exlev);
                if (SCM_UNBOUNDP(r)) return r;
                SCM_APPEND(h, t, r);
            } else {
                r = realize_template_rec(e, mvec, level, indices, idlist, exlev);
                if (SCM_UNBOUNDP(r)) return r;
                SCM_APPEND1(h, t, r);
            }
            template = SCM_CDR(template);
        }
        if (!SCM_NULLP(template)) {
            r = realize_template_rec(template, mvec, level, indices, idlist, exlev);
            if (SCM_UNBOUNDP(r)) return r;
            SCM_APPEND(h, t, r);
        }
        return h;
    }
    if (PVREF_P(template)) {
        return get_pvref_value(template, mvec, indices, exlev);
    }
    if (SCM_SYNTAX_PATTERN_P(template)) {
        ScmSyntaxPattern *pat = SCM_SYNTAX_PATTERN(template);
        ScmObj h = SCM_NIL, t, r;
        indices[level+1] = 0;
        for (;;) {
            r = realize_template_rec(pat->pattern, mvec, level+1, indices, idlist, exlev);
            if (SCM_UNBOUNDP(r)) return (*exlev < pat->level)? r : h;
            SCM_APPEND1(h, t, r);
            indices[level+1]++;
        }
    }
    if (SCM_VECTORP(template)) {
        ScmObj h = SCM_NIL, t, r, *pe;
        int len = SCM_VECTOR_SIZE(template), i;
        pe = SCM_VECTOR_ELEMENTS(template);
        
        for (i=0; i<len; i++, pe++) {
            if (SCM_SYNTAX_PATTERN_P(*pe)) {
                r = realize_template_rec(*pe, mvec, level, indices, idlist, exlev);
                if (SCM_UNBOUNDP(r)) return r;
                SCM_APPEND(h, t, r);
            } else {
                r = realize_template_rec(*pe, mvec, level, indices, idlist, exlev);
                if (SCM_UNBOUNDP(r)) return r;
                SCM_APPEND1(h, t, r);
            }
        }
        return Scm_ListToVector(h);
    }
    if (SCM_IDENTIFIERP(template)) {
        /* we copy the identifier, so that the symbol bindings introduced
           by recursive macro call won't interfere each other.
           (e.g. the macro definitions of "letrec" and "do" shown in R5RS
           use the fact that the symbol "newtemp" introduced in each
           iteration of macro expansion are distinct. */
        ScmObj p = Scm_Assq(template, *idlist);
        if (SCM_PAIRP(p)) return SCM_CDR(p);
        else {
            ScmObj id = Scm_CopyIdentifier(SCM_IDENTIFIER(template));
            *idlist = Scm_Acons(template, id, *idlist);
            return id;
        }
    }
    return template;
}

#define DEFAULT_MAX_LEVEL  10

static ScmObj realize_template(ScmSyntaxRuleBranch *branch,
                               MatchVar *mvec)
{
    int index[DEFAULT_MAX_LEVEL], *indices = index, i, lev;
    int exlev = 0;
    ScmObj idlist = SCM_NIL;
    
    if (branch->maxLevel > DEFAULT_MAX_LEVEL)
        indices = SCM_NEW_ATOMIC2(int*, (branch->maxLevel+1) * sizeof(int));
    for (i=0; i<=branch->maxLevel; i++) indices[i] = 0;
    return realize_template_rec(branch->template, mvec, 0, indices, &idlist, &exlev);
}

static ScmObj synrule_expand(ScmObj form, ScmObj env, ScmSyntaxRules *sr)
{
    MatchVar *mvec = alloc_matchvec(sr->maxNumPvars);
    ScmObj expanded;
    int i;

#ifdef DEBUG_SYNRULE    
    Scm_Printf(SCM_CUROUT, "**** synrule_transform: %S\n", form);
#endif
    for (i=0; i<sr->numRules; i++) {
#ifdef DEBUG_SYNRULE    
        Scm_Printf(SCM_CUROUT, "pattern #%d: %S\n", i, sr->rules[i].pattern);
#endif
        init_matchvec(mvec, sr->rules[i].numPvars);
        if (match_synrule(SCM_CDR(form), sr->rules[i].pattern, env, mvec)) {
#ifdef DEBUG_SYNRULE    
            Scm_Printf(SCM_CUROUT, "success #%d:\n", i);
            print_matchvec(mvec, sr->rules[i].numPvars, SCM_CUROUT);
#endif
            expanded = realize_template(&sr->rules[i], mvec);
#ifdef DEBUG_SYNRULE    
            Scm_Printf(SCM_CUROUT, "result: %S\n", expanded);
#endif
            return expanded;
        }
    }
    Scm_Error("malformed %S: %S", SCM_CAR(form), form);
    return SCM_NIL;
}

static ScmObj synrule_transform(ScmObj form, ScmObj env,
                                int ctx, void *data)
{
    ScmSyntaxRules *sr = (ScmSyntaxRules *)data;
    ScmObj expanded = synrule_expand(form, env, sr);
    return Scm_Compile(expanded, env, ctx);
}

/*-------------------------------------------------------------------
 * %syntax-rules
 *    Internal macro of syntax-rules.  Taking macro name as the first arg.
 */
static ScmObj compile_syntax_rules(ScmObj form, ScmObj env,
                                   int ctx, void *data)
{
    ScmObj name, literals, rules;
    ScmSyntaxPattern *pat, *tmpl;
    ScmSyntaxRules *sr;

    if (Scm_Length(form) < 4) {
        SCM_ASSERT(SCM_PAIRP(SCM_CDR(form)));
        goto badform;
    }
    name = SCM_CADR(form);
    if (SCM_IDENTIFIERP(name)) name = SCM_OBJ(SCM_IDENTIFIER(name)->name);
    SCM_ASSERT(SCM_SYMBOLP(name));
    literals = SCM_CAR(SCM_CDDR(form));
    rules = SCM_CDR(SCM_CDDR(form));

    sr = compile_rules(name, literals, rules, env);
#ifdef DEBUG_SYNRULE
    Scm_Printf(SCM_CUROUT, "%S\n", sr);
#endif
    return SCM_LIST1(Scm_MakeSyntax(SCM_SYMBOL(name),
                                    synrule_transform,
                                    (void*)sr));
        
  badform:
    Scm_Error("malformed syntax-rules: ",
              Scm_Cons(SCM_INTERN("syntax-rules"), SCM_CDDR(form)));
    return SCM_NIL;
}

static ScmSyntax syntax_syntax_rules = {
    SCM_CLASS_SYNTAX,
    SCM_SYMBOL(SCM_SYM_SYNTAX_RULES_INT),
    compile_syntax_rules,
    NULL
};

/*-------------------------------------------------------------------
 * let-syntax, letrec-syntax
 */

static ScmObj compile_let_syntax(ScmObj form, ScmObj env, int ctx, void *data)
{
    int letrecp = (data != NULL);
    ScmObj var, rule, vars = SCM_NIL, vars_t;
    ScmObj frame = SCM_NIL, frame_t;
    ScmObj syntax, body, synbinds, sp, newenv;

    syntax = SCM_CAR(form);
    if (Scm_Length(form) < 2) Scm_Error("malformed %S: %S", syntax, form);
    synbinds = SCM_CADR(form);
    body = SCM_CDDR(form);
    SCM_APPEND1(frame, frame_t, SCM_TRUE);

    if (!SCM_PAIRP(synbinds))
        Scm_Error("%S: malformed syntactic bindings: %S", syntax, form);
    
    SCM_FOR_EACH(sp, synbinds) {
        ScmObj synbind = SCM_CAR(sp);
        if (Scm_Length(synbind) != 2)
            Scm_Error("%S: malformed syntactic binding: %S", syntax, synbind);
        var = SCM_CAR(synbind);
        rule = SCM_CADR(synbind);
        if (!SCM_SYMBOLP(var) && !SCM_IDENTIFIERP(var))
            Scm_Error("%S: symbol required in syntactic binding: %S",
                      syntax, synbind);
        if (!SCM_PAIRP(rule) ||
            !Scm_FreeVariableEqv(SCM_CAR(rule), SCM_SYM_SYNTAX_RULES, env))
            Scm_Error("%S: needs syntax-rules form, but got: %S",
                      syntax, rule);
        if (!SCM_FALSEP(Scm_Memq(var, vars)))
            Scm_Error("%S: duplicate names in syntactic bindings: %S",
                      syntax, var);

        SCM_APPEND1(vars, vars_t, var);
        SCM_APPEND1(frame, frame_t, Scm_Cons(var, rule));
    }
    newenv = letrecp? Scm_Cons(frame, env) : env;

    /* compile rules */
    SCM_FOR_EACH(sp, SCM_CDR(frame)) {
        ScmObj synrule;
        
        var = SCM_CAAR(sp);
        rule = SCM_CDAR(sp);
        synrule = compile_syntax_rules(Scm_Cons(SCM_SYM_SYNTAX_RULES_INT,
                                                Scm_Cons(var, SCM_CDR(rule))),
                                       newenv, ctx, NULL);
        SCM_ASSERT(SCM_PAIRP(synrule));
        SCM_SET_CDR(SCM_CAR(sp), SCM_CAR(synrule));
    }
    
    if (!letrecp) newenv = Scm_Cons(frame, env);

    return Scm_CompileBody(body, newenv, ctx);
}

static ScmSyntax syntax_let_syntax = {
    SCM_CLASS_SYNTAX,
    SCM_SYMBOL(SCM_SYM_LET_SYNTAX),
    compile_let_syntax,
    (void*)0
};

static ScmSyntax syntax_letrec_syntax = {
    SCM_CLASS_SYNTAX,
    SCM_SYMBOL(SCM_SYM_LETREC_SYNTAX),
    compile_let_syntax,
    (void*)1
};

/*===================================================================
 * macro-expand
 */

/*
 * To capture locally-bound macros, macro-expand needs to be a syntax.
 * From scheme, this syntax is visible as %macro-expand.
 * The procedure version, which works only for globally defined macros,
 * can be defined as
 *  (define (macro-expand form) (%macro-expand form))
 */
static ScmObj compile_macro_expand(ScmObj form, ScmObj env,
                                   int ctx, void *data)
{
    ScmObj expr, sym;
    ScmSyntax *syn;
    ScmGloc *gloc;
    int oncep = (int)data;

    if (!SCM_PAIRP(SCM_CDR(form)) || !SCM_NULLP(SCM_CDDR(form)))
        Scm_Error("syntax error: %S", form);
    expr = SCM_CADR(form);

    for (;;) {
        if (!SCM_PAIRP(expr)) return SCM_LIST1(expr);
        if (!SCM_SYMBOLP(SCM_CAR(expr)) && !SCM_IDENTIFIERP(SCM_CAR(expr)))
            return SCM_LIST1(expr);

        syn = NULL;
        sym = Scm_CompileLookupEnv(SCM_CAR(expr), env, TRUE);
        if (SCM_SYNTAXP(sym)) {
            /* local syntactic binding */
            syn = SCM_SYNTAX(sym);
        } else {
            if (SCM_IDENTIFIERP(sym)) {
                sym = SCM_OBJ(SCM_IDENTIFIER(sym)->name);
            }
            if (SCM_SYMBOLP(sym)) {
                ScmGloc *g = Scm_FindBinding(Scm_VM()->module, SCM_SYMBOL(sym),
                                             FALSE);
                if (g && SCM_SYNTAXP(g->value)) {
                    syn = SCM_SYNTAX(g->value);
                }
            }
        }
        if (syn) {
            if (syn->compiler == macro_transform) {
                ScmObj proc = SCM_OBJ(syn->data);
                expr = Scm_Apply(proc, expr);
                if (!oncep) continue;
            }
            if (syn->compiler == synrule_transform) {
                ScmSyntaxRules *sr = (ScmSyntaxRules *)syn->data;
                expr = synrule_expand(expr, env, sr);
                if (!oncep) continue;
            }
        }
        break;
    }
    return SCM_LIST1(expr);
}

static ScmSyntax syntax_macro_expand = {
    SCM_CLASS_SYNTAX,
    SCM_SYMBOL(SCM_SYM_MACRO_EXPAND),
    compile_macro_expand,
    (void*)0
};

static ScmSyntax syntax_macro_expand_1 = {
    SCM_CLASS_SYNTAX,
    SCM_SYMBOL(SCM_SYM_MACRO_EXPAND_1),
    compile_macro_expand,
    (void*)1
};

/*===================================================================
 * Initializer
 */

void Scm__InitMacro(void)
{
    ScmModule *m = SCM_MODULE(Scm_SchemeModule());

#define DEFSYN(symbol, syntax) \
    Scm_Define(m, SCM_SYMBOL(symbol), SCM_OBJ(&syntax))
    
    DEFSYN(SCM_SYM_SYNTAX_RULES_INT, syntax_syntax_rules);
    DEFSYN(SCM_SYM_LET_SYNTAX, syntax_let_syntax);
    DEFSYN(SCM_SYM_LETREC_SYNTAX, syntax_letrec_syntax);
    DEFSYN(SCM_SYM_MACRO_EXPAND, syntax_macro_expand);
    DEFSYN(SCM_SYM_MACRO_EXPAND_1, syntax_macro_expand_1);
}
