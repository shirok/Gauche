;;;
;;; libalpha.scm - Procedures needed by other lib*.scm
;;;
;;;   Copyright (c) 2000-2024  Shiro Kawai  <shiro@acm.org>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

;; This file is initialized first among lib-*.scm.  Define procedures
;; here that are required to run initialization of other lib-*.scm files.

(select-module gauche.internal)

(inline-stub
 (.include "gauche/priv/configP.h"
           "gauche/vminsn.h"
           "gauche/vm.h"
           "gauche/priv/glocP.h"
           "gauche/priv/macroP.h"
           "gauche/priv/procP.h"
           "gauche/priv/vmP.h"))

;; This is referred from the compiled code of toplevel definitions, so
;; we need it before anything else.
(select-module gauche.internal)
(define-cproc %expression-name-mark-key ()
  (return (Scm__GetDenvKey SCM_DENV_KEY_EXPRESSION_NAME)))
(define-cproc %exception-handler-mark-key ()
  (return (Scm__GetDenvKey SCM_DENV_KEY_EXCEPTION_HANDLER)))

;;;
;;;  errors
;;;

(select-module gauche.internal)
(define (%error-scan-keys args)
  (let loop ([args args] [keys '()])
    (if (and (not (null? args))
             (keyword? (car args))
             (not (null? (cdr args))))
      (loop (cddr args) (list* (cadr args) (car args) keys))
      (values (reverse! keys) args))))

(define (%make-error-message msg args) ; SRFI-23 style message
  (let1 p (open-output-string)
    (display msg p)
    (dolist [obj args] (display " " p) (write/ss obj p))
    (get-output-string p)))

;; Handy when you want to create <error> object without immediately raising it.
(define-in-module gauche (make-error msg . args)
  (make <error> :message (%make-error-message msg args)
        :message-prefix msg :message-args args))

;; error and errorf.  A bit messy to allow optional condition class argument
;; as the first arg.
(define-in-module gauche (error msg . args)
  (raise
   (cond
    [(is-a? msg <condition-meta>)
     (receive (keys msgs) (%error-scan-keys args)
       (if (null? msgs)
         (apply make msg keys)
         (apply make msg
                :message (%make-error-message (car msgs) (cdr msgs))
                :message-prefix (car msgs)
                :message-args (cdr msgs)
                keys)))]
    [else (apply make-error msg args)])))

(define-in-module gauche (errorf fmt . args)
  (raise
   (cond
    [(is-a? fmt <condition-meta>)
     (receive (keys msgs) (%error-scan-keys args)
       (if (null? msgs)
         (apply make fmt keys)
         (apply make fmt :message (apply format/ss #f msgs) keys)))]
    [else (make <error> :message (apply format/ss #f fmt args))])))

;;;
;;;  make-vector
;;;

;; make-vector needs to be defined here, for it is called
;; in other initalization routines.
(select-module scheme)
(define-cproc make-vector (k::<fixnum> :optional fill) Scm_MakeVector)

;;;
;;;  SRFI-17 support
;;;

;; some libraries needs to set the setter procedures during initialization,
;; so we should have it before them.

(select-module gauche)
(define-cproc getter-with-setter (proc::<procedure> set::<procedure>) ;SRFI-17
  (case (SCM_PROCEDURE_TYPE proc)
    [(SCM_PROC_SUBR SCM_PROC_CLOSURE)]  ;ok
    [(SCM_PROC_GENERIC SCM_PROC_METHOD)
     (Scm_Error "You can't attach a setter to a generic function or a method \
                 using getter-with-setter.  Instead, you can define a setter \
                 method using the name (setter %S)."
                (SCM_PROCEDURE_INFO proc))]
    [else (Scm_Error "You can't attach a setter to %S." (SCM_OBJ proc))])
  (let* ([p (Scm__CopyProcedure proc SCM_FALSE)])
    ;; NB: We override p->locked, for p is a copy.
    (set! (SCM_PROCEDURE_SETTER_LOCKED p) FALSE)
    (return (Scm_SetterSet (SCM_PROCEDURE p) set TRUE))))

(define-cproc setter (proc) ;SRFI-17
  (inliner SETTER)
  (setter (proc::<procedure> setter::<procedure>) ::<void>
          (Scm_SetterSet proc setter FALSE))
  Scm_Setter)

;;;
;;;  case-lambda support
;;;

(select-module gauche.internal)
(inline-stub
 ;; Support procedure for case-lambda
 ;; Dispatch-vector maps # of required arguments to the procedure body.
 ;; The last entry of dispatch vector is to cover more arguments; it should
 ;; be #f if no cases take arbitrary number of arguments.
 ;; Examples:
 ;;  (case-lambda [() a] [(x) (b)] [(x y) c] [(x y . z) d])
 ;;  =>
 ;;  #( (lambda () a)              min-reqargs == 0
 ;;     (lambda (x) b)
 ;;     (lambda (x y) c)
 ;;     (lambda (x y . z) d) )
 ;;
 ;;  (case-lambda [() a] [(x) (b)] [(x y) c])
 ;;  =>
 ;;  #( (lambda () a)              min-reqargs == 0
 ;;     (lambda (x) b)
 ;;     (lambda (x y) c)
 ;;     #f )
 ;;
 ;;  (case-lambda [(x y) a] [(x y z) b] [(x y z . w) c]
 ;;  =>
 ;;  #( (lambda (x y) a)           min-reqargs == 2
 ;;     (lambda (x y z) b)
 ;;     (lambda (x y z . w) c))
 ;;
 ;;  (case-lambda [(x y) a] [(x . y) b]
 ;;  =>
 ;;  #( (lambda (x . y) b)         min-reqargs == 1
 ;;     (lambda (x y) a)
 ;;     (lambda (x . y) b))
 ;;
 ;; In the last example, 2 argument case matches the first procedure,
 ;; for it has precedence.

 (define-ctype case_lambda_packet
   :: (.struct case_lambda_packet_rec
               (dispatch_vector::ScmVector*
                max_optargs::int
                min_reqargs::int)))

 (define-cfn case-lambda-dispatch (args::ScmObj* nargs::int data::void*):static
   (let* ([d::case_lambda_packet* (cast case_lambda_packet* data)]
          [rarg (aref args (- nargs 1))]
          [_ (SCM_ASSERT (> nargs (-> d min_reqargs)))]
          [_ (SCM_ASSERT (<= nargs (+ (-> d min_reqargs) (-> d max_optargs) 1)))]
          ;; NB: Since this is variable-arg subr, rarg is always a list.
          ;; If nargs <= d->max_optargs + d->min_reqargs, rargs should always
          ;; be '().  See vmcall.c for how the arguments are folded.
          [p (SCM_VECTOR_ELEMENT (-> d dispatch_vector)
                                 (- nargs (-> d min_reqargs) 1))])
     (when (SCM_FALSEP p)
       (Scm_Error "wrong number of arguments to case lambda: %S"
                  (Scm_ArrayToListWithTail args (- nargs 1)
                                           (aref args (- nargs 1)))))
     (if (SCM_NULLP rarg)
       (case (- nargs 1)
         [(0) (return (Scm_VMApply0 p))]
         [(1) (return (Scm_VMApply1 p (aref args 0)))]
         [(2) (return (Scm_VMApply2 p (aref args 0) (aref args 1)))]
         [(3) (return (Scm_VMApply3 p (aref args 0) (aref args 1)
                                    (aref args 2)))]
         [(4) (return (Scm_VMApply4 p (aref args 0) (aref args 1)
                                    (aref args 2) (aref args 3)))]
         [else (return (Scm_VMApply p (Scm_ArrayToList args (- nargs 1))))])
       (return
        (Scm_VMApply p (Scm_ArrayToListWithTail args (- nargs 1) rarg))))
     ))

 ;; A case-lambda procedure has special inliner procedure in its 'inliner' slot,
 ;; which is made by pass1/make-case-labmda-inliner.   There's a catch,
 ;; though.  Case-lambda procedures are made and called during booting,
 ;; but pass1/make-case-labmda-inliner won't be available until the late
 ;; stage of booting when the compiler is initialized.  So, if we don't
 ;; have pass1/make-case-labmda-inliner yet, we assign
 ;; delayed-case-lambda-inliner to delay actual calling.
 (define-cfn delayed-case-lambda-inliner (args::ScmObj* nargs::int data::void*)
   :static
   (let* ([clambda-proc (SCM_OBJ data)]
          [make-inliner
           (Scm_GlobalVariableRef (Scm_GaucheInternalModule)
                                  (SCM_SYMBOL 'pass1/make-case-lambda-inliner)
                                  0)])
     (when (SCM_UNBOUNDP make-inliner)
       (return SCM_UNDEFINED))          ;not fully booted
     (SCM_ASSERT (== nargs 2))
     (let* ([src (aref args 0)]
            [arg-iforms (aref args 1)]
            [inliner (Scm_ApplyRec1 make-inliner clambda-proc)])
       (SCM_ASSERT (SCM_PROCEDUREP inliner))
       (set! (SCM_PROCEDURE_INLINER clambda-proc) inliner)
       (return (Scm_VMApply2 inliner src arg-iforms)))))

 ;; The compiler expands case-lambda to a call of make-case-lambda.
 ;; It may be called during initialization, so we need it here.
 ;; TRANSIENT: The third argument used to be a list of formals of the closures.
 ;; We no longer need it, for we extract info from closures.  It is left for
 ;; the backward compatibility until we switch ABI.
 (define-cproc make-case-lambda (minarg::<int>
                                 maxarg::<int>
                                 _
                                 closures
                                 :optional (name #f))
   (let* ([max-optargs::int (+ (- maxarg minarg) 2)]
          [v-closures (Scm_MakeVector max-optargs SCM_FALSE)]
          [num-closures::int 0]
          [v-formals]
          [packet::case_lambda_packet* (SCM_NEW case_lambda_packet)])
     (for-each
      (lambda (c)
        (let* ([req::int (SCM_PROCEDURE_REQUIRED c)]
               [opt::int (SCM_PROCEDURE_OPTIONAL c)]
               [n::int minarg])
          (for (() (<= n (+ maxarg 1)) (post++ n))
            (when (and (or (== n req)
                           (and (> n req) (> opt 0)))
                       (SCM_FALSEP (SCM_VECTOR_ELEMENT v-closures (- n minarg))))
              (set! (SCM_VECTOR_ELEMENT v-closures (- n minarg)) c)
              (pre++ num-closures)))))
      closures)
     ;; v-formals is #(<formals> ...)).  it is kept in the procedure-info
     ;; slot.  It is only for information, the program should not count on
     ;; its content.
     ;; NB: We need a standard way to extract formals from a closure. Here
     ;; I take cdr of each closure's procedure-info, but it's not guaranteed
     ;; that they always have the formals.
     (set! v-formals (Scm_MakeVector num-closures SCM_FALSE))
     (let* ([i::int 0])
       (dotimes [n max-optargs]
         (let* ([c (SCM_VECTOR_ELEMENT v-closures n)])
           (when (and (SCM_PROCEDUREP c)
                      (SCM_PAIRP (SCM_PROCEDURE_INFO c)))
             (set! (SCM_VECTOR_ELEMENT v-formals i)
                   (SCM_CDR (SCM_PROCEDURE_INFO c)))
             (post++ i)))))
     (set! (-> packet min_reqargs) minarg
           (-> packet max_optargs) max-optargs
           (-> packet dispatch_vector) (SCM_VECTOR v-closures))
     (let* ([r (Scm_MakeSubr case_lambda_dispatch packet
                             minarg max_optargs
                             (SCM_LIST2 (?: (SCM_FALSEP name)
                                            'case-lambda-dispatcher
                                            name)
                                        v-formals))]
            [make-inliner
             (Scm_GlobalVariableRef (Scm_GaucheInternalModule)
                                    (SCM_SYMBOL 'pass1/make-case-lambda-inliner)
                                    0)])
       (if (SCM_UNBOUNDP make-inliner)
         (set! (SCM_PROCEDURE_INLINER r)
               (Scm_MakeSubr delayed-case-lambda-inliner
                             (cast void* r) 2 0
                             'delayed-case-lambda-inliner))
         (set! (SCM_PROCEDURE_INLINER r)
               (Scm_ApplyRec1 make-inliner r)))
       (return r))))

 ;; Returns a plist of case-lambda info if OBJ is a case lambda closure,
 ;; otherwise #f.
 (define-cproc %case-lambda-info (obj)
   (if (and (SCM_SUBRP obj)
            (== (SCM_SUBR_FUNC obj) case_lambda_dispatch))
     (let* ([packet::case_lambda_packet* (cast case_lambda_packet*
                                               (SCM_SUBR_DATA obj))])
       (return (list ':min-reqargs
                     (SCM_MAKE_INT (-> packet min_reqargs))
                     ':max-optargs
                     (SCM_MAKE_INT (-> packet max_optargs))
                     ':dispatch-vector
                     (SCM_OBJ (-> packet dispatch_vector)))))
     (return SCM_FALSE)))
 )

;; Returns ((<required args> <optional arg> <procedure>) ...)
;; for easier introspection.
(define-in-module gauche (case-lambda-decompose proc)
  (and-let1 info (%case-lambda-info proc)
    (let ([min-args (get-keyword :min-reqargs info)]
          [len (get-keyword :max-optargs info)]
          [dispatch-vec (get-keyword :dispatch-vector info)])
      (let loop ([r '()] [i 0])
        (if (= i (- len 1))
          (if-let1 optproc (vector-ref dispatch-vec i)
            (reverse r `((,(+ min-args i -1) #t ,optproc)))
            (reverse r))
          (if-let1 proc (vector-ref dispatch-vec i)
            (loop (cons `(,(+ min-args i) #f ,proc) r) (+ i 1))
            (loop r (+ i 1))))))))

;;;
;;; Transfer binding to other module
;;;

;; Some components (e.g. libobj.scm) has its own module, and transfer
;; external API to 'gauche' module.
;; NB: For more general (internal) API, we have %insert-binding in libmod.
;; %transfer-bindings is purely for initialization, and it is overwritten
;; in libomega.

(with-module gauche.internal)

;; Take binding of each symbol in SYMBOLS in the module FROM and inject
;; it in the module TO.  Binding flags (const, inlinable) are carried over,
;; unless a list given to FLAGS argument.  FLAGS list may include
;; 'const and 'inlinable.
(define-cproc %transfer-bindings (from::<module> to::<module> symbols
                                                 :optional (flags #f))
  ::<void>
  (for-each (lambda (sym)
              (SCM_ASSERT (SCM_SYMBOLP sym))
              (let* ([g::ScmGloc* (Scm_FindBinding from (SCM_SYMBOL sym) 0)]
                     [lflags::u_long 0])
                (SCM_ASSERT (!= g NULL))
                (cond [(SCM_PAIRP flags)
                       (for-each (lambda (fl)
                                   (cond [(SCM_EQ fl 'const)
                                          (logior= lflags SCM_BINDING_CONST)]
                                         [(SCM_EQ fl 'inlinable)
                                          (logior= lflags SCM_BINDING_INLINABLE)]
                                         [else
                                          (Scm_Error "unknown flag: %S" fl)]))
                                 flags)]
                      [else
                       (when (Scm_GlocInlinableP g)
                         (logior= lflags SCM_BINDING_INLINABLE))
                       (when (Scm_GlocConstP g)
                         (logior= lflags SCM_BINDING_CONST))])
                (Scm_MakeBinding to (SCM_SYMBOL sym) (SCM_GLOC_GET g) lflags)))
            symbols))

;;;
;;; This is needed before we use define-macro.
;;;

(with-module gauche.internal)

;; transformer :: Sexpr, Cenv -> Sexpr
;; flags is a list of flag symbols (we avoid using numeric values,
;; to avoid inter-version build.
(define-cproc %make-macro-transformer (name transformer
                                       :optional (info ())
                                                 (flags ()))
  (let* ([flag-bits::u_long 0])
    (for-each (lambda (flag)
                (cond
                 [(SCM_EQ flag 'identifier-macro)
                  (logior= flag-bits SCM_MACRO_IDENTIFIER)]
                 [(SCM_EQ flag 'parameterizable)
                  (logior= flag-bits SCM_MACRO_PARAMETERIZABLE)]
                 [else
                  (Scm_Error "Unknown flag symbol: %S" flag)]))
              flags)
    (return (Scm_MakeMacro name transformer info flag-bits))))
