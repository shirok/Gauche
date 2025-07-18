;;;
;;;  This is to test features that aren't accessible from Scheme by default.
;;;  We build a special executable that exposes those features.
;;;

(select-module user)

;; Foreign pointer features

(inline-stub
 (.include "gauche/priv/configP.h")

 (define-cvar fptr_simple::ScmClass* :static)
 (define-cvar fptr_unique::ScmClass* :static)
 (define-cvar fptr_maybe::ScmClass* :static)

 (define-cfn print-fptr (obj sink::ScmPort* _::ScmWriteContext*) ::void
   (let* ([k::ScmClass* (Scm_ClassOf obj)])
     (Scm_Printf sink "#<fptr-%s %p>"
                 (?: (== k fptr_simple) "simple"
                     (?: (== k fptr_unique) "unique" "maybe"))
                 (SCM_FOREIGN_POINTER_REF (void*) obj))))

 (define-cproc make-fptr-simple (value :optional (attr '()))
   (return (Scm_MakeForeignPointerWithAttr fptr_simple value attr)))
 (define-cproc make-fptr-unique (value :optional (attr '()))
   (return (Scm_MakeForeignPointerWithAttr fptr_unique value attr)))
 (define-cproc make-fptr-maybe (value :optional (attr '()))
   (return (Scm_MakeForeignPointerWithAttr fptr_maybe
                                           (?: (SCM_FALSEP value) 0 value)
                                           attr)))
 (define-cproc fptr-value (fptr)
   (let* ([k::ScmClass* (Scm_ClassOf fptr)])
     (unless (or (== k fptr_simple)
             (== k fptr_unique)
             (== k fptr_maybe))
       (Scm_Error "fptr required, but got: %S" fptr))
     (return (SCM_OBJ_SAFE (SCM_FOREIGN_POINTER_REF ScmObj fptr)))))

 (initcode
  (set! fptr_simple (Scm_MakeForeignPointerClass
                     (Scm_UserModule) "<fptr-simple>" print-fptr NULL 0))
  (set! fptr_unique (Scm_MakeForeignPointerClass
                     (Scm_UserModule) "<fptr-simple>" print-fptr NULL
                     SCM_FOREIGN_POINTER_KEEP_IDENTITY))
  (set! fptr_maybe  (Scm_MakeForeignPointerClass
                     (Scm_UserModule) "<fptr-simple>" print-fptr NULL
                     SCM_FOREIGN_POINTER_MAP_NULL))
  ))

;; Pathname replacement
(inline-stub
 (declcode   ;we need declcode to ensure those .defines comes before .include
  (.define PATH_ALLOC (n) (SCM_MALLOC_ATOMIC n))
  (.define PATH_ERROR (...) (Scm_Error __VA_ARGS__))
  (.include "paths.c"))

 (define-cproc substitute-all (input::<const-cstring>
                               mark::<const-cstring>
                               subst::<const-cstring>)
   ::<const-cstring>
   (cast void replace_install_dir) ; to avoid unused warning
   (cast void get_libgauche_path)  ; ditto
   (return (substitute_all input mark subst))))

;; Busy loop to test thread termination.
;;   thread-terminate! first flags target VM with a termination request,
;;   but if thread doesn't terminate with it (either busy executing a subr
;;   or blocked), it sends a signal.
;;   The test (tests/thread-termination.scm) kicks busy loop in a thread,
;;   and call thread-terminate!.  If thread is really terminated,
;;   it stops incrementing busy-loop-counter.
(inline-stub
 (define-cvar busy-loop-counter ::ScmSmallInt :static 0)
 (define-cproc busy-loop (max-count::<fixnum>)
   (let* ([ts::ScmTimeSpec])
     (while (< (post++ busy-loop-counter) max-count)
       (when (% busy-loop-counter 100000)
         (set! (ref ts tv_sec) 0)
         (set! (ref ts tv_nsec) 100000)
         (Scm_NanoSleep (& ts) NULL)))
     (return SCM_TRUE)))
 (define-cproc get-busy-loop-counter () ::<fixnum>
   (return busy-loop-counter))
 )

;; Entry point

(define (main args)
  (unless (pair? (cdr args))
    (exit 1 "usage: testgosh <test-script>"))
  (load (cadr args))
  0)
