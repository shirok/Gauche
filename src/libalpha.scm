;;;
;;; libalpha.scm - Procedures needed by other lib*.scm
;;;
;;;   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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
 (declcode (.include <gauche/vminsn.h>)))

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

(define-in-module gauche (error msg . args)
  (define (mkmsg msg args) ;; srfi-23 style message
    (let1 p (open-output-string)
      (display msg p)
      (dolist [obj args] (display " " p) (write/ss obj p))
      (get-output-string p)))
  (raise
   (cond
    [(is-a? msg <condition-meta>)
     (receive (keys msgs) (%error-scan-keys args)
       (if (null? msgs)
         (apply make msg keys)
         (apply make msg :message (mkmsg (car msgs) (cdr msgs)) keys)))]
    [else (make <error>
            :message (mkmsg msg args)
            :message-prefix msg
            :message-args args)])))

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
;;;  srfi-17 support
;;;

;; some libraries needs to set the setter procedures during initialization,
;; so we should have it before them.

(select-module gauche)
(define-cproc setter (proc) ;SRFI-17
  (inliner SETTER)
  (setter (proc::<procedure> setter::<procedure>) ::<void>
          (Scm_SetterSet proc setter FALSE))
  Scm_Setter)

(define (getter-with-setter get set)
  (rlet1 proc (^ x (apply get x))
    (set! (setter proc) set)))

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
 ;;  #( (lambda (x . y) c)         min-reqargs == 1
 ;;     (lambda (x y) a)
 ;;     (lambda (x . y) c))
 ;;
 ;; In the last example, 2 argument case matches the first procedure,
 ;; for it has precedence.

 "typedef struct case_lambda_packet_rec {
    ScmVector *dispatch_vector;
    int max_optargs;
    int min_reqargs;
 } case_lambda_packet;"

 (define-cfn case-lambda-dispatch (args::ScmObj* nargs::int data::void*):static
   (let* ([d::case_lambda_packet* (cast case_lambda_packet* data)]
          [rarg (aref args (- nargs 1))])
     (SCM_ASSERT (> nargs (-> d min_reqargs)))
     (SCM_ASSERT (<= nargs (+ (-> d min_reqargs) (-> d max_optargs) 1)))
     ;; NB: Since this is variable-arg subr, rarg is always a list.
     ;; If nargs <= d->max_optargs + d->min_reqargs, rargs should always
     ;; be '().  See vmcall.c for how the arguments are folded.
     (let* ([p (SCM_VECTOR_ELEMENT (-> d dispatch_vector)
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
       )))

 ;; NB: We keep dispatch-vector in the procedure info, so that disasm
 ;; and other inspection routines can find it.  The structure of
 ;; the procedure info here is tentative; it may be changed later.
 ;; Routines that depends on this info must be aware of that.
 (define-cproc make-case-lambda-dispatcher (dispatch-vector::<vector>
                                            min-reqargs::<int>
                                            name)
   (let* ([max-optargs::int (SCM_VECTOR_SIZE dispatch-vector)]
          [packet::case_lambda_packet* (SCM_NEW case_lambda_packet)])
     (set! (-> packet min_reqargs) min-reqargs
           (-> packet max_optargs) max-optargs
           (-> packet dispatch_vector) dispatch-vector)
     (return (Scm_MakeSubr case_lambda_dispatch packet
                           min_reqargs max_optargs
                           (SCM_LIST3 (?: (SCM_FALSEP name)
                                         'case-lambda-dispatcher
                                         name)
                                      (SCM_MAKE_INT min-reqargs)
                                      (SCM_OBJ dispatch-vector))))))
 )

;; The compiler expands case-lambda to a call of make-case-lambda.
;; It may be called during initialization, so we need it here.
(define-in-module gauche.internal (make-case-lambda minarg maxarg
                                                    formals closures
                                                    :optional (name #f))
  (define (fill-dispatch-vector! v formals closure)
    (define (%set n)
      (let1 i (- n minarg)
        (unless (vector-ref v i) (vector-set! v i closure))))
    (let loop ([formals formals] [n 0])
      (cond [(> n (+ maxarg 1))]
            [(null? formals) (%set n)]
            [(pair? formals) (loop (cdr formals) (+ n 1))]
            [else (%set n) (loop formals (+ n 1))])))

  (let1 v (make-vector (+ (- maxarg minarg) 2) #f)
    ;; Avoid using for-each, since the definition of for-each itself
    ;; nees to call make-case-lambda.
    (let loop ([fs formals] [cs closures])
      (unless (null? fs)
        (fill-dispatch-vector! v (car fs) (car cs))
        (loop (cdr fs) (cdr cs))))
    ((with-module gauche.internal make-case-lambda-dispatcher) v minarg name)))

;; Returns ((<required args> <optional arg> <procedure>) ...)
;; This also depends on the structure of dispatch vector info.
;; Programs that needs to deal with case-lambda should use this procedure
;; instead of directly interpret dispatch vector info.
(define-in-module gauche (case-lambda-info proc)
  (and (subr? proc)
       (let1 info (procedure-info proc)
         (and (pair? info)
              (pair? (cdr info))
              (integer? (cadr info))
              (pair? (cddr info))
              (vector? (caddr info))
              (let* ([min-args (cadr info)]
                     [dispatch-vec (caddr info)]
                     [len (vector-length dispatch-vec)])
                (let loop ([r '()] [i 0])
                  (if (= i (- len 1))
                    (if-let1 optproc (vector-ref dispatch-vec i)
                      (reverse r `((,(+ min-args i -1) #t ,optproc)))
                      (reverse r))
                    (if-let1 proc (vector-ref dispatch-vec i)
                      (loop (cons `(,(+ min-args i) #f ,proc) r) (+ i 1))
                      (loop r (+ i 1))))))))))
