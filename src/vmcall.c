/*
 * This code fragment implements the core of procedure and generic function
 * calling sequence in Gauche VM.  It is included by vm.c twice, with slight
 * difference switched by preprocessor macros.
 */

/* ADJUST_ARGUMENT_FRAME
 *
 * Checks the argument count is OK for call to PROC.  if PROC takes &rest
 * args, fold those arguments to the list.  Modifies ARGC to hold the
 * adjusted size of the argument frame.
 *
 * There's two cases in how the arguments are pushed in the VM stack
 * (normal call and apply call), and there's two cases in how the arguments
 * should be prepared (fixed-argument call and variable-argument call), so
 * there are four cases in total.
 *
 * Input:
 *  In normal call, arguments at the call site are pushed in the stack
 *  in left-to-right order.  For example, if the call site is (f x y z),
 *  the top 3 stack entries of the stack are like this ('|' indicates
 *  stack top; stack grows to right):
 *
 *      ... x  y  z|
 *
 *  in this case, input ARGC is 3.
 *
 *  In apply call, the stack top is a list of tail of argument list.
 *  If the call site is (apply f x (list y z)), input ARGC is 2 and the
 *  stack top has these two entries:
 *
 *      ... x  (y z)|
 *
 * Output:
 *  Suppose proc->required == N and proc->optional == M.
 *  If M == 0, it is a fixed-argument procedure.  The stack must contain
 *  N values, and ARGC must be N.
 *
 *  If M > 0, the stack must contain between N+1 to N+M values, where
 *  the last value is the tail of the argument list.  For standard Scheme
 *  variable argument procedure, M is always 1 and the stack contains
 *  N required arguments plus one list of 'rest' argument.
 */

#undef ADJUST_ARGUMENT_FRAME
#if !defined(APPLY_CALL)
#define ADJUST_ARGUMENT_FRAME(proc, argc)                               \
    do {                                                                \
        int reqargs = SCM_PROCEDURE_REQUIRED(proc);                     \
        int optargs = SCM_PROCEDURE_OPTIONAL(proc);                     \
        if (optargs) {                                                  \
            ScmObj p = SCM_NIL;                                         \
            if (argc < reqargs) {                                       \
                wna(vm, VAL0, argc, -1); RETURN_OP(); NEXT;             \
            }                                                           \
            /* fold &rest args */                                       \
            while (argc > reqargs+optargs-1) {                          \
                ScmObj a;                                               \
                POP_ARG(a);                                             \
                p = Scm_Cons(a, p);                                     \
                argc--;                                                 \
            }                                                           \
            PUSH_ARG(p);                                                \
            argc++;                                                     \
        } else {                                                        \
            if (argc != reqargs) {                                      \
                wna(vm, VAL0, argc, -1); RETURN_OP(); NEXT;             \
            }                                                           \
        }                                                               \
    } while (0)
#else /*APPLY_CALL*/
#define ADJUST_ARGUMENT_FRAME(proc, argc)                               \
    do {                                                                \
        int rargc = check_arglist_tail_for_apply(vm, *(vm->sp - 1));    \
        ScmObj p, a;                                                    \
        int reqargs = SCM_PROCEDURE_REQUIRED(proc);                     \
        int optargs = SCM_PROCEDURE_OPTIONAL(proc);                     \
        if (optargs) {                                                  \
            if ((rargc+argc-1) < reqargs) {                             \
                wna(vm, VAL0, rargc+argc-1, rargc); RETURN_OP(); NEXT;  \
            }                                                           \
            POP_ARG(p);  /* tail of arglist */                          \
            if (argc > reqargs+optargs) {                               \
                /* fold rest args. */                                   \
                p = Scm_CopyList(p);                                    \
                for (int c=argc; c>reqargs+optargs; c--) {              \
                    POP_ARG(a);                                         \
                    p = Scm_Cons(a, p);                                 \
                }                                                       \
                PUSH_ARG(p);                                            \
            } else {                                                    \
                /* 'unfold' rest arg */                                 \
                CHECK_STACK(reqargs + optargs - argc + 1);              \
                for (int c=argc;                                        \
                     SCM_PAIRP(p) && c<reqargs+optargs;                 \
                     c++) {                                             \
                    PUSH_ARG(SCM_CAR(p));                               \
                    p = SCM_CDR(p);                                     \
                }                                                       \
                p = Scm_CopyList(p);                                    \
                PUSH_ARG(p);                                            \
            }                                                           \
        } else {                                                        \
            /* no optargs */                                            \
            if ((rargc+argc-1) != reqargs) {                            \
                wna(vm, VAL0, rargc+argc-1, rargc); RETURN_OP(); NEXT;  \
            }                                                           \
            POP_ARG(p);  /* tail of arglist */                          \
            if (rargc > 0) {                                            \
                /* 'unfold' rest arg */                                 \
                CHECK_STACK(rargc);                                     \
                do {                                                    \
                    PUSH_ARG(SCM_CAR(p));                               \
                    p = SCM_CDR(p);                                     \
                } while (--rargc > 0);                                  \
            }                                                           \
        }                                                               \
        argc = SP-ARGP;                                                 \
    } while (0)
#endif /*APPLY_CALL*/

/* avoid duplication of labels */
#undef GENERIC_ENTRY
#undef DO_METHOD_CALL
#undef APP
#if !defined(APPLY_CALL)
#define GENERIC_ENTRY  generic_entry
#define DO_METHOD_CALL do_method_call
#define APP            FALSE
#else
#define GENERIC_ENTRY  generic_entry_app
#define DO_METHOD_CALL do_method_call_app
#define APP            TRUE
#endif

/*
 * The code fragment begins here.  It is included in run_loop,
 * so the VM registers are fully accessible via macros (SP, ARGP, etc.)
 *
 * NB: argc, proctype and nm are defined in outer scope.
 */

{
    ScmObj mm, *fp;

    argc = (int)(SP - ARGP);
    vm->numVals = 1; /* default */

    /* object-apply hook.  shift args, and insert val0 into
       the fist arg slot, then call GenericObjectApply. */
    if (MOSTLY_FALSE(!SCM_PROCEDUREP(VAL0))) {
        int i;
        CHECK_STACK_PARANOIA(1);
        for (i=0; i<argc; i++) {
            *(SP-i) = *(SP-i-1);
        }
        *(SP-argc) = VAL0;
        SP++; argc++;
        VAL0 = SCM_OBJ(&Scm_GenericObjectApply);
        proctype = SCM_PROC_GENERIC;
        nm = SCM_FALSE;
        goto GENERIC_ENTRY;
    }

    /*
     * We process the common cases first
     */
    proctype = SCM_PROCEDURE_TYPE(VAL0);
    if (proctype == SCM_PROC_SUBR) {
        /* We don't need to complete environment frame.  Just need to
           adjust sp, so that stack-operating procs called from subr
           won't be confused. */
        ADJUST_ARGUMENT_FRAME(VAL0, argc);
        SP = ARGP;
        PC = PC_TO_RETURN;
#if GAUCHE_FFX
        /* We can't pass FLONUM_REGs unless the subr is flagged as
           IMMEDIATE_ARG. */
        if (!(SCM_SUBR_FLAGS(VAL0)&SCM_SUBR_IMMEDIATE_ARG)) {
            ScmObj *ap = SP;
            int i = 0;
            for (; i<argc; i++, ap++) SCM_FLONUM_ENSURE_MEM(*ap);
        }
#endif

        SCM_PROF_COUNT_CALL(vm, VAL0);
        VAL0 = SCM_SUBR(VAL0)->func(ARGP, argc, SCM_SUBR(VAL0)->data);
        /* the subr may have substituted pc, so we need to check
           if we can pop the continuation immediately. */
        if (TAIL_POS()) RETURN_OP();
        CHECK_INTR;
        NEXT;
    }
    if (proctype == SCM_PROC_CLOSURE) {
        ADJUST_ARGUMENT_FRAME(VAL0, argc);
        if (argc) {
            FINISH_ENV(SCM_PROCEDURE_INFO(VAL0), SCM_CLOSURE(VAL0)->env);
        } else {
            ENV = SCM_CLOSURE(VAL0)->env;
            ARGP = SP;
        }
        vm->base = SCM_COMPILED_CODE(SCM_CLOSURE(VAL0)->code);
        PC = vm->base->code;
        CHECK_STACK(vm->base->maxstack);
        SCM_PROF_COUNT_CALL(vm, SCM_OBJ(vm->base));
        VAL0 = SCM_MAKE_INT(argc); /* keep argc to VAL0. */
        NEXT;
    }

    /*
     * Generic function application
     */

    /* First, compute methods */
    nm = SCM_FALSE;
    if (proctype == SCM_PROC_GENERIC) {
        if (!SCM_GENERICP(VAL0)) {
            /* use scheme-defined MOP.  we modify the stack frame so
               that it is converted to an application of pure generic
               fn apply-generic. */
            ScmObj args, arg;
#if !defined(APPLY_CALL)
            if (argc < 2) CHECK_STACK(2);
            args = SCM_NIL;
            for (int i=0; i<argc; i++) {
                POP_ARG(arg);
                args = Scm_Cons(arg, args);
            }
            ARGP = SP;
            argc = 2;
            PUSH_ARG(VAL0);
            PUSH_ARG(args);
#else  /* APPLY_CALL */
            if (argc < 3) CHECK_STACK(3);
            POP_ARG(args);
            argc--;
            for (int i=0; i<argc; i++) {
                POP_ARG(arg);
                args = Scm_Cons(arg, args);
            }
            ARGP = SP;
            argc = 3;
            PUSH_ARG(VAL0);
            PUSH_ARG(args);
            PUSH_ARG(SCM_NIL);  /* applyargs */
#endif /* APPLY_CALL */
            VAL0 = SCM_OBJ(&Scm_GenericApplyGeneric);
        }
      GENERIC_ENTRY:
        /* pure generic application.  we implement MOP in C. */
        mm = Scm_ComputeApplicableMethods(SCM_GENERIC(VAL0), ARGP, argc, APP);
        if (!SCM_NULLP(mm)) {
            /* sort methods.  we only need as many args as
               gf->maxReqargs to order methods, so we only unfold that
               many args if applyargs.
            */
#if defined(APPLY_CALL)
            if (argc-1<SCM_GENERIC(VAL0)->maxReqargs) {
                ScmObj args;
                POP_ARG(args);
                CHECK_STACK(SCM_GENERIC(VAL0)->maxReqargs - argc);
                while (argc <= SCM_GENERIC(VAL0)->maxReqargs
                       && SCM_PAIRP(args)) {
                    PUSH_ARG(SCM_CAR(args));
                    args = SCM_CDR(args);
                    argc++;
                }
                PUSH_ARG(args);
            }
#endif /*APPLY_CALL*/
#if GAUCHE_FFX
            {
                ScmObj *ap = ARGP;
                for (int i=0;i<argc; i++, ap++) SCM_FLONUM_ENSURE_MEM(*ap);
            }
#endif /*GAUCHE_FFX*/
            mm = Scm_SortMethods(mm, ARGP, argc);
            nm = Scm_MakeNextMethod(SCM_GENERIC(VAL0), SCM_CDR(mm),
                                    ARGP, argc, TRUE, APP);
            VAL0 = SCM_CAR(mm);
            proctype = SCM_PROC_METHOD;
        }
    } else if (proctype == SCM_PROC_NEXT_METHOD) {
        ScmNextMethod *n = SCM_NEXT_METHOD(VAL0);
        int use_saved_args = FALSE;
        int apply_call_p = APP;
        /* If no arguments are given to next-method, we use the args
           saved in the next-method.  */
#if !defined(APPLY_CALL)
        use_saved_args = (argc == 0);
#else  /*APPLY_CALL*/
        use_saved_args = (argc == 1 && SCM_NULLP(*(SP-1)));
#endif /*APPLY_CALL*/
        if (use_saved_args) {
            CHECK_STACK(n->argc+1);
            memcpy(SP, n->argv, sizeof(ScmObj)*n->argc);
            SP += n->argc;
            argc = n->argc;
            apply_call_p = n->applyargs;
        }
        if (SCM_NULLP(n->methods)) {
            VAL0 = SCM_OBJ(n->generic);
            proctype = SCM_PROC_GENERIC;
        } else {
            nm = Scm_MakeNextMethod(n->generic, SCM_CDR(n->methods),
                                    ARGP, argc, TRUE, apply_call_p);
            VAL0 = SCM_CAR(n->methods);
            proctype = SCM_PROC_METHOD;
        }
        if (use_saved_args) {
#if !defined(APPLY_CALL)
            if (apply_call_p)  goto do_method_call_app;
#else  /*APPLY_CALL*/
            if (!apply_call_p) goto do_method_call;
#endif /*APPLY_CALL*/
        }
    } else if (proctype == SCM_PROC_METHOD) {
        VM_ERR(("Attempt to call a method %S without using a generic function.",
                VAL0));
    } else {
        Scm_Panic("something's wrong.");
    }

  DO_METHOD_CALL:
    fp = ARGP;
    if (proctype == SCM_PROC_GENERIC) {
        /* we have no applicable methods.  call fallback fn. */
#if defined(APPLY_CALL)
        /* TEMPORARY - we need to fix the protocol of calling fallback
           generic, so that it can accept large number of arguments
           without unfolding all of them on the VM stack. */
        ScmObj args;

        POP_ARG(args);
        argc--;
        while (SCM_PAIRP(args)) {
            PUSH_ARG(SCM_CAR(args));
            args = SCM_CDR(args);
            argc++;
        }
#endif /*APPLY_CALL*/
        FINISH_ENV(SCM_PROCEDURE_INFO(VAL0), NULL);
        PC = PC_TO_RETURN;
        SCM_PROF_COUNT_CALL(vm, VAL0);
        VAL0 = SCM_GENERIC(VAL0)->fallback(fp, argc, SCM_GENERIC(VAL0));
        /* The fallback may substituted pc, so we need to check if we
           can pop the continuation immediately. */
        if (TAIL_POS()) RETURN_OP();
        NEXT;
    }

    /*
     * Now, apply method
     */
    ADJUST_ARGUMENT_FRAME(VAL0, argc);

    VM_ASSERT(proctype == SCM_PROC_METHOD);
    VM_ASSERT(!SCM_FALSEP(nm));
    if (SCM_METHOD(VAL0)->func) {
        /* C-defined method */
        FINISH_ENV(SCM_PROCEDURE_INFO(VAL0), NULL);
        PC = PC_TO_RETURN;
        SCM_PROF_COUNT_CALL(vm, VAL0);
        VAL0 = SCM_METHOD(VAL0)->func(SCM_NEXT_METHOD(nm), fp, argc,
                                      SCM_METHOD(VAL0)->data);
        /* the func may substituted pc, so we need to check if we can
           pop the continuation immediately. */
        if (TAIL_POS()) RETURN_OP();
    } else {
        /* Scheme-defined method.  next-method arg is passed as the
           last arg (note that rest arg is already folded). */
        PUSH_ARG(SCM_OBJ(nm));
        FINISH_ENV(SCM_PROCEDURE_INFO(VAL0),
                   SCM_METHOD(VAL0)->env);
        VM_ASSERT(SCM_COMPILED_CODE_P(SCM_METHOD(VAL0)->data));
        vm->base = SCM_COMPILED_CODE(SCM_METHOD(VAL0)->data);
        PC = vm->base->code;
        CHECK_STACK(vm->base->maxstack);
        SCM_PROF_COUNT_CALL(vm, SCM_OBJ(vm->base));
        VAL0 = SCM_MAKE_INT(argc); /* keep argc to VAL0. */
    }
    NEXT;
}
