;;;
;;; Small repl to benchmark C continuation overhead.
;;; Use 'make bench-pushcc' to build.
;;;

(define-cfn pushcc-cc (r d::void**)
  (let* ([n (cast ScmObj (aref d 0))]
         [m (Scm_Sub n (SCM_MAKE_INT 1))])
    (if (Scm_NumEq n (SCM_MAKE_INT 0))
      (return r)
      (begin
        (Scm_VMPushCC pushcc-cc (cast void** (& m)) 1)
        (return (Scm_Add r n))))))

(define-cproc pushcc-bench (n)
  (if (Scm_NumEq n (SCM_MAKE_INT 0))
    (return n)
    (let* ([m (Scm_Sub n (SCM_MAKE_INT 1))])
      (Scm_VMPushCC pushcc-cc (cast void** (& m)) 1)
      (return n))))

(inline-stub
 (.include "gauche/precomp.h"))

(define-cfn pushpc-cc (vm::ScmVM* r d::ScmObj*)
  (let* ([n (cast ScmObj (aref d 0))]
         [m (Scm_Sub n (SCM_MAKE_INT 1))])
    (if (Scm_NumEq n (SCM_MAKE_INT 0))
      (return r)
      (let* ([dd::ScmObj* (Scm_pc_PushCC vm pushpc-cc 1)])
        (set! (aref dd 0) m)
        (return (Scm_Add r n))))))

(define-cproc pushpc-bench (n)
  (if (Scm_NumEq n (SCM_MAKE_INT 0))
    (return n)
    (let* ([d::ScmObj* (Scm_pc_PushCC (Scm_VM) pushpc-cc 1)])
      (set! (aref d 0) (Scm_Sub n (SCM_MAKE_INT 1)))
      (return n))))

;; (time (pushcc-bench 100000000))
;; (time (pushpc-bench 100000000))

(inline-stub
 (declare-cfn Scm_Init_bench_pushcc () ::void :static)
 (define-cfn main (_::int _::char**) ::int
   (Scm_Init GAUCHE_SIGNATURE)
   (Scm_Init_bench_pushcc)
   (Scm_EvalCString "(read-eval-print-loop)"
                    (SCM_OBJ (Scm_CurrentModule))
                    NULL)
   (return 0)))
