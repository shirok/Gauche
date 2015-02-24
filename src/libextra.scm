;;;
;;;  This is to test features that aren't accessible from Scheme by default.
;;;  We build a special executable that exposes those features.
;;;

(select-module user)

;; Foreign pointer features

(inline-stub
 "ScmClass *fptr_simple, *fptr_unique, *fptr_maybe;"

 (define-cfn print-fptr (obj sink::ScmPort* mode::ScmWriteContext*) ::void
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

;; Entry point

(define (main args)
  (unless (pair? (cdr args))
    (exit 1 "usage: testgosh <test-script>"))
  (load (cadr args))
  0)

