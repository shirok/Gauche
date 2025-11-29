/*
 * gauche/procedure.h - Procedures
 *
 *   Copyright (c) 2000-2025  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_PROCEDURE_H
#define GAUCHE_PROCEDURE_H

/*=============================================================
 * Procedures
 */

/* ScmProcedure can be statically allocated, so do changing its layout
 * can break binary compatibility.
 * The current definition exposes too much internal info, though.  We may
 * hide internal slots in future.
 */

/* Base structure */
struct ScmProcedureRec {
    SCM_INSTANCE_HEADER;
    unsigned int required : 16;    /* # of required args */
    unsigned int optional : 8;     /* >=1 if it takes opt args. see below.*/
    unsigned int type     : 3;     /* ScmProcedureType */
    unsigned int locked   : 1;     /* setter locked? (see below) */
    unsigned int currying : 1;     /* autocurrying */
    unsigned int constant : 1;     /* constant procedure. see below. */
    unsigned int leaf     : 1;     /* leaf procedure/method */
    unsigned int placeholder : 1;  /* placeholder method. */
#if GAUCHE_API_VERSION >= 98
    unsigned int reserved32 : 32;  /* unused yet. */
#endif /*GAUCHE_API_VERSION >= 98*/
    ScmObj info;                   /* source code info (see below) */
    ScmObj setter;                 /* setter, if exists. */
    ScmObj inliner;                /* inliner information (see below) */
#if GAUCHE_API_VERSION >= 98
    ScmObj typeHint;               /* info to be used for type checking.
                                      shouldn't be accessed directly, for
                                      we do some tricky stuff here.
                                      Use Scheme API procedure-type
                                      to get the type info.
                                   */
    ScmObj tagsAlist;              /* Alist of procedure tags.  See below. */
#endif /*GAUCHE_API_VERSION >= 98*/
};

/* About locked slot:
   For <procedure> and <generic>, it shows whether the setter is locked.
   For <method>, it shows whether the alteration of the method is disallowed,
   i.e. one can't redefine a method with matching signature.
   (These two roles are reflected to the two macros,
   SCM_PROCEDURE_SETTER_LOCKED and SCM_PROCEDURE_METHOD_LOCKED)
   TODO: When we change ABI, maybe split these roles to different flags.
 */

/* About optional slot:
   If this slot is non-zero, the procedure takes optional arguments.
   For Standard Scheme procedures with 'rest' arguments, this slot is 1
   and all excessive arguments are 'folded' in a list.

   This slot may have a value more than 1.  If it is N (>1), then up to N-1
   optional arguments are passed without being folded (that is, passed
   'on the stack'.  Only when the given argument is more than or equal to
   N + reqargs, the excessive arguments are folded and passed in a list.
   Thus, such procedure may get between reqargs values and N+reqargs values
   after folding (NB: Fixed argument procedure always get regargs values,
   and standard Scheme variable argument procedure always get reqargs+1 values
   after argument folding).

   This special treatment is to avoid unnecessary consing of argumets;
   if we know the callee immediately unfolds the rest argument, it's no
   use to fold excessive arguments anyway.
 */

/* About 'constant' flag:

   For a <procedure> and <method>, this flag being TRUE means it returns
   the same constant value if given same constant arguments, and it does
   not have any other external effects.   The compiler may use this info
   to replace a call of this proc with the resulting value,
   if all the arguments are known at compile-time.
   The resulting value must be serializable to the
   precompiled file.  The result shouldn't be affected
   by the timing of the compile, architecture on which the compiler runs,
   or the compiler configuration (e.g. internal encoding).

   If <generic> has this flag, it tells the compiler that it can calculate
   applicable method at the compile time.  It is independent from method's
   constantness---the selected method may or may not be used as a compile-time
   calculation; but it is safe to pre-select that method, given that
   enough information is available at the compile time.
   We warn if a new method is added to a 'constant' generic.
 */

/* About 'leaf' flag:
   For METHOD, this flag indicates the method doesn't refer to next-method
   argument at all, so we can skip creating next-method instance when
   making a call.
   For CLOSURE, we *plan* to use this to indicate the closure body doesn't
   make a call to another procedures, to allow certain optimizations.
 */

/* About 'info' slot:
   This is a sort of the kitchen sink slot, keeping whatever miscellaneous
   information as our implementation evolves.  Since this can be a part of
   statically allocated structure, we can't change its format in a way
   that breaks the backward compatibility.

   SUBR, CLOSURE:
           This slot may contain one of this:
           - Signature: For example, the subr `cons' has (cons obj1 obj2)
             in it.  The first pair may have the following pair attributes.

               `source-info'   (<filename> <lineno>)
                   The source location the procedure is defined, if known.
                   This info can be retrieved with (source-location PROC).
               `bind-info'     (<module-name> <var-name>)
                   The proc is bound to <var-name> in a module named
                   <module-name>, and it's inlinable binding.  When the
                   compiler can pre-calculate the proc to be called in a
                   code, it can replace the original code with a global
                   variable reference to <var-name>.  (We can't directly
                   insert reference to the proc, for it may not be
                   serializable for AOT compilation).

           - A <primitive-parameter> or <parameter> object.  R7RS requires
             parameters to be a procedure, responding #t to procedure?.
             We need to adapt Gauche parameter into that, saving the
             actual parameter instance here.

           - Subr's name, as a string or a symbol.  This is the old format.
             It may also the case that subr is created from C function
             Scm_MakeSubr(), for it's cumbersome in C routine to construct
             the signature list.  Accept it, but not recommended to use
             this format in the new code.
           - #f.  Indicates there's no useful info.

   GENERIC:
           This slot contains the "name" of the gf, which is a symbol.
           A kludge: For setter gf, which can be created indirectly
           via (define-method (setter GF) ...), we use a weird name
           |setter of GF|.  This is a quick hack to make it work, but ideally
           we should accept a list (setter GF) as the name.  Anticipate
           this change in future.
           Furthermore, in order to hold source-info, we might just make
           it a pair, e.g. (NAME) or ((setter NAME)).

   METHOD:
           This slot contains (<name> <specializer> ...),
           where <name> is the name of the generic function, and
           <specializer>s are the name of classes.

   NEXT_METHOD:
           This slot isn't used.
 */

/* About procedure inliner:
   This slot holds information to inline procedures.  The value of this slot
   can be one of the following kinds:

   #f: No inliner associated to this procedure.  (For historical
      reasons, the code that access to this slot expects this slot can be
      NULL and treats it as SCM_FALSE in that case)

   <integer>: Only appears in some built-in procedures, and specifies
      the VM instruction number.  This should be considered as a special
      hack.   The set of procedures that can have this type of inliner
      is tied to the VM definition.

   <vector>: Procedures defined with define-inline have this.  The vector
      encodes intermediate form (IForm) of the procedure code, which will be
      expanded into the caller.

   <macro>:  A compiler macro.  The macro expander is invoked with the
      original source and macro-use environment, just like the ordinary macro
      call.  The expander must return an Sexpr.  If the expander returns
      the input as is, it indicates expansion is not possible and the form
      is compiled as the ordinary procedure call.

   <procedure>: A procedural inliner.  It has signature Sexpr,[IForm] -> IForm,
      where Sexpr is the original source of call site (just for debug info) and
      input [IForm] is the IForm for list of arguments.  See compiler-1.scm.
      It returns the modified IForm.  It can return #<undef>, to indicate
      inlining isn't possible.
 */

/* About procedure tags alist
   Procedure tags can carry extra info about the procedure.  They
   should be treated as immutable property, for procedures can be
   duplicated/consolidated for optimizations and other runtime handlings.
   (The value of a tag can have a mutable structure).

   This alist itself should be kept for internal use.  Some values
   associated with specific keys are exposed with public API.

   For now, it is used for the following features

    SRFI-229 procedure tag - kept with the key 'srfi-229-tag
    SRFI-259 procedure tag - kept with a unique gensym-ed symbol as a key
    Custom procedure printer - kept iwth tag #:print. (Not utilized yet, but
                               infrastructure is in place).
 */

/* procedure type */
enum ScmProcedureType {
    SCM_PROC_SUBR,
    SCM_PROC_CLOSURE,
    SCM_PROC_GENERIC,
    SCM_PROC_METHOD,
    SCM_PROC_NEXT_METHOD
};

#define SCM_PROCEDURE(obj)          ((ScmProcedure*)(obj))
#define SCM_PROCEDURE_REQUIRED(obj) SCM_PROCEDURE(obj)->required
#define SCM_PROCEDURE_OPTIONAL(obj) SCM_PROCEDURE(obj)->optional
#define SCM_PROCEDURE_TYPE(obj)     SCM_PROCEDURE(obj)->type
#define SCM_PROCEDURE_CONSTANT(obj) SCM_PROCEDURE(obj)->constant
#define SCM_PROCEDURE_CURRYING(obj) SCM_PROCEDURE(obj)->currying
#define SCM_PROCEDURE_INFO(obj)     SCM_PROCEDURE(obj)->info
#define SCM_PROCEDURE_SETTER(obj)   SCM_PROCEDURE(obj)->setter
#define SCM_PROCEDURE_INLINER(obj)  SCM_PROCEDURE(obj)->inliner
#define SCM_PROCEDURE_SETTER_LOCKED(obj) SCM_PROCEDURE(obj)->locked
#define SCM_PROCEDURE_LEAF(obj)     SCM_PROCEDURE(obj)->leaf

SCM_CLASS_DECL(Scm_ProcedureClass);
#define SCM_CLASS_PROCEDURE    (&Scm_ProcedureClass)
#define SCM_PROCEDUREP(obj) \
    (SCM_HOBJP(obj) && SCM_CLASS_APPLICABLE_P(SCM_CLASS_OF(obj)))
#define SCM_PROCEDURE_TAKE_NARG_P(obj, narg) \
    (SCM_PROCEDUREP(obj)&& \
     (  (!SCM_PROCEDURE_OPTIONAL(obj)&&SCM_PROCEDURE_REQUIRED(obj)==(narg)) \
      ||(SCM_PROCEDURE_OPTIONAL(obj)&&SCM_PROCEDURE_REQUIRED(obj)<=(narg))))
#define SCM_PROCEDURE_THUNK_P(obj) \
    (SCM_PROCEDUREP(obj)&& \
     (  (!SCM_PROCEDURE_OPTIONAL(obj)&&SCM_PROCEDURE_REQUIRED(obj)==0) \
      ||(SCM_PROCEDURE_OPTIONAL(obj))))

/* This is internal - should never be used directly */
#if GAUCHE_API_VERSION >= 98
#define SCM__PROCEDURE_INITIALIZER(klass, req, opt, typ, cst, lef, inf, inl) \
    { { klass, NULL }, (req), (opt), (typ), FALSE, FALSE, cst, lef, 0, 0,    \
      (inf), SCM_FALSE, (inl), SCM_FALSE, SCM_NIL }
#else  /* GAUCHE_API_VERSION < 98 */
#define SCM__PROCEDURE_INITIALIZER(klass, req, opt, typ, cst, lef, inf, inl) \
    { { klass, NULL }, (req), (opt), (typ), FALSE, FALSE, cst, lef, 0,       \
      (inf), SCM_FALSE, (inl) }
#endif /* GAUCHE_API_VERSION < 98 */

/* Closure - Scheme defined procedure */
struct ScmClosureRec {
    ScmProcedure common;
    ScmObj code;                /* compiled code */
    ScmEnvFrame *env;           /* environment */
};

#define SCM_CLOSUREP(obj) \
    (SCM_PROCEDUREP(obj)&&(SCM_PROCEDURE_TYPE(obj)==SCM_PROC_CLOSURE))
#define SCM_CLOSURE(obj)           ((ScmClosure*)(obj))
#define SCM_CLOSURE_CODE(obj)      SCM_CLOSURE(obj)->code
#define SCM_CLOSURE_ENV(obj)       SCM_CLOSURE(obj)->env

SCM_EXTERN ScmObj Scm_MakeClosure(ScmObj code, ScmEnvFrame *env);

/* Subr - C defined procedure */
struct ScmSubrRec {
    ScmProcedure common;
    int flags;
    ScmSubrProc *func;
    void *data;
};

#define SCM_SUBRP(obj) \
    (SCM_PROCEDUREP(obj)&&(SCM_PROCEDURE_TYPE(obj)==SCM_PROC_SUBR))
#define SCM_SUBR(obj)              ((ScmSubr*)(obj))
#define SCM_SUBR_FLAGS(obj)        SCM_SUBR(obj)->flags
#define SCM_SUBR_FUNC(obj)         SCM_SUBR(obj)->func
#define SCM_SUBR_DATA(obj)         SCM_SUBR(obj)->data

/* flags */
#define SCM_SUBR_IMMEDIATE_ARG  (1L<<0) /* This subr will not retain a reference
                                           to the flonums given to args.  VM
                                           can safely pass the register flonums
                                           to the subr.  This is added when
                                           the :fast-flonum flag is given to
                                           define-cproc. */

#define SCM__DEFINE_SUBR_INT(cvar, req, opt, cst, inf, flags, func, inliner, data) \
    ScmSubr cvar = {                                                        \
        SCM__PROCEDURE_INITIALIZER(SCM_CLASS_STATIC_TAG(Scm_ProcedureClass),\
             req, opt, SCM_PROC_SUBR, cst, 0, inf, inliner),                \
        flags, (func), (data)                                               \
    }

#define SCM_DEFINE_SUBR(cvar, req, opt, inf, func, inliner, data) \
    SCM__DEFINE_SUBR_INT(cvar, req, opt, 0, inf, 0, func, inliner, data)
#define SCM_DEFINE_SUBRX(cvar, req, opt, cst, inf, flags, func, inliner, data) \
    SCM__DEFINE_SUBR_INT(cvar, req, opt, cst, inf, flags, func, inliner, data)

SCM_EXTERN ScmObj Scm_MakeSubr(ScmSubrProc *func,
                               void *data,
                               int required, int optional,
                               ScmObj info);
SCM_EXTERN ScmObj Scm_NullProc(void);

SCM_EXTERN ScmObj Scm_SetterSet(ScmProcedure *proc, ScmProcedure *setter,
                                int lock);
SCM_EXTERN ScmObj Scm_Setter(ScmObj proc);
SCM_EXTERN int    Scm_HasSetter(ScmObj proc);

/* Generic - Generic function */
struct ScmGenericRec {
    ScmProcedure common;
    ScmObj methods;             /* list of methods */
    int   maxReqargs;           /* maximum # of args required to select
                                   applicable methods */
    ScmObj (*fallback)(ScmObj *argv, int argc, ScmGeneric *gf);
    void *dispatcher;
    void *data;
    ScmInternalMutex lock;
};

SCM_CLASS_DECL(Scm_GenericClass);
#define SCM_CLASS_GENERIC          (&Scm_GenericClass)
#define SCM_GENERICP(obj)          SCM_XTYPEP(obj, SCM_CLASS_GENERIC)
#define SCM_GENERIC(obj)           ((ScmGeneric*)obj)
#define SCM_GENERIC_DATA(obj)      (SCM_GENERIC(obj)->data)

/* we share 'constant' flag for sealed generic */
#define SCM_GENERIC_SEALED_P(obj)  SCM_PROCEDURE_CONSTANT(obj)

#define SCM_DEFINE_GENERIC(cvar, cfunc, data)                           \
    ScmGeneric cvar = {                                                 \
        SCM__PROCEDURE_INITIALIZER(SCM_CLASS_STATIC_TAG(Scm_GenericClass),\
                                   0, 0, SCM_PROC_GENERIC, 0, 0,        \
                                   SCM_FALSE, NULL),                    \
        SCM_NIL, 0, cfunc, NULL, data,                                  \
        SCM_INTERNAL_MUTEX_INITIALIZER                                  \
    }

SCM_EXTERN void Scm_InitBuiltinGeneric(ScmGeneric *gf, const char *name,
                                       ScmModule *mod);
SCM_EXTERN ScmObj Scm_MakeBaseGeneric(ScmObj name,
                                      ScmObj (*fallback)(ScmObj *, int, ScmGeneric*),
                                      void *data);
SCM_EXTERN ScmObj Scm_NoNextMethod(ScmObj *argv, int argc, ScmGeneric *gf);
SCM_EXTERN ScmObj Scm_NoOperation(ScmObj *argv, int argc, ScmGeneric *gf);
SCM_EXTERN ScmObj Scm_InvalidApply(ScmObj *argv, int argc, ScmGeneric *gf);

/* Method - method
   A method can be defined either by C or by Scheme.  C-defined method
   have func ptr, with optional data.   Scheme-define method has NULL
   in func, code in data, and optional environment in env. */
struct ScmMethodRec {
    ScmProcedure common;
    ScmGeneric *generic;
    ScmClass **specializers;    /* array of specializers, size==required */
    ScmObj (*func)(ScmNextMethod *nm, ScmObj *argv, int argc, void * data);
    void *data;                 /* closure, or code */
    ScmEnvFrame *env;           /* environment (for Scheme created method) */
};

SCM_CLASS_DECL(Scm_MethodClass);
#define SCM_CLASS_METHOD           (&Scm_MethodClass)
#define SCM_METHODP(obj)           SCM_ISA(obj, SCM_CLASS_METHOD)
#define SCM_METHOD(obj)            ((ScmMethod*)obj)
#define SCM_METHOD_LOCKED(obj)     SCM_METHOD(obj)->common.locked
#define SCM_METHOD_LEAF_P(obj)     SCM_METHOD(obj)->common.leaf
#define SCM_METHOD_ENV(obj)        SCM_METHOD(obj)->env

#define SCM_DEFINE_METHOD(cvar, gf, req, opt, specs, func, data)        \
    ScmMethod cvar = {                                                  \
        SCM__PROCEDURE_INITIALIZER(SCM_CLASS_STATIC_TAG(Scm_MethodClass),\
                                   req, opt, SCM_PROC_METHOD, 0, 0,     \
                                   SCM_FALSE, NULL),                    \
        gf, specs, func, data, NULL                                     \
    }

SCM_EXTERN void Scm_InitBuiltinMethod(ScmMethod *m);

/* Next method object
   Next method is just another callable entity, with memoizing
   the arguments. */
struct ScmNextMethodRec {
    ScmProcedure common;
    ScmGeneric *generic;
    ScmObj methods;          /* list of applicable methods */
    ScmObj *argv;            /* original arguments */
    int argc;                /* # of original arguments */
    int applyargs;           /* if TRUE, argv[argc-1] has a list of rest args */
};

SCM_CLASS_DECL(Scm_NextMethodClass);
#define SCM_CLASS_NEXT_METHOD      (&Scm_NextMethodClass)
#define SCM_NEXT_METHODP(obj)      SCM_XTYPEP(obj, SCM_CLASS_NEXT_METHOD)
#define SCM_NEXT_METHOD(obj)       ((ScmNextMethod*)obj)

/* Calling a Scheme function from C
 *
 *  static ScmObj proc = SCM_UNDEFINED;
 *
 *  SCM_BIND_PROC(proc, "scheme-proc-name", module);
 *
 *  Scm_ApplyRec(proc, args);
 *   or
 *  Scm_Apply(proc, args, &result);
 *
 * SCM_BIND_PROC macro initializes the C variable proc to the value of
 * the global Scheme variable scheme-proc-name in the module.
 * It is idempotent operation, so it's MT-safe.
 */
#define SCM_BIND_PROC(var, name, module)                                \
    do {                                                                \
        if (SCM_UNDEFINEDP(var)) {                                      \
            ScmObj v__ =                                                \
                Scm_GlobalVariableRef(module,                           \
                                      SCM_SYMBOL(SCM_INTERN(name)),     \
                                      0);                               \
            if (SCM_UNBOUNDP(v__)) {                                    \
                Scm_Error("Procedure %s is unbound", name);             \
            }                                                           \
            var = v__;                                                  \
        }                                                               \
    } while (0)



#endif //GAUCHE_PROCEDURE_H
