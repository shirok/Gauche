;;;
;;; common-macros.scm - common macros
;;;  
;;;   Copyright (c) 2000-2010  Shiro Kawai  <shiro@acm.org>
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

;;; Defines number of useful macros.  This file is to be autoloaded.

;; Note: This file is so fundamental that most other autoloaded files
;; depend on it.  If you modify this file, be very careful not to depend
;; on other autoloaded files, since it is easy to create circular
;; dependency.

(select-module gauche)

;;; syntax-error
;;; syntax-errorf
;;;   Signals an error at compile time.

(define-macro (syntax-error . args)
  (apply error (map unwrap-syntax args)))

(define-macro (syntax-errorf . args)
  (apply errorf (map unwrap-syntax args)))

;;;-------------------------------------------------------------
;;; generalized set! family

(define-syntax update!
  (syntax-rules ()
    [(_ "vars" ((var arg) ...) () proc updater val ...)
     (let ((getter proc)
           (var arg) ...)
       ((setter getter) var ... (updater val ... (getter var ...))))]
    [(_ "vars" ((var arg) ...) (arg0 arg1 ...) proc updater val ...)
     (update! "vars"
              ((var arg) ... (newvar arg0))
              (arg1 ...)
              proc updater val ...)]
    [(_ (proc arg ...) updater val ...)
     (update! "vars"
              ()
              (arg ...)
              proc updater val ...)]
    [(_ loc updater val ...)
     (set! loc (updater val ... loc))]
    [(_ . other)
     (syntax-error "malformed update!" (update! . other))]))

(define-syntax push!
  (syntax-rules ()
    [(_ "vars" ((var arg) ...) () proc val)
     (let ((getter proc)
           (var arg) ...)
       ((setter getter) var ... (cons val (getter var ...))))]
    [(_ "vars" ((var arg) ...) (arg0 arg1 ...) proc val)
     (push! "vars" ((var arg) ... (newvar arg0)) (arg1 ...) proc val)]
    [(_ (proc arg ...) val)
     (push! "vars" () (arg ...) proc val)]
    [(_ loc val)
     (set! loc (cons val loc))]
    [(_ . other)
     (syntax-error "malformed push!" (push! . other))]))

(define-syntax pop!
  (syntax-rules ()
    [(_ "vars" ((var arg) ...) () proc)
     (let ((getter proc)
           (var arg) ...)
       (let ((val (getter var ...)))
         ((setter getter) var ... (cdr val))
         (car val)))]
    [(_ "vars" ((var arg) ...) (arg0 arg1 ...) proc)
     (pop! "vars" ((var arg) ... (newvar arg0)) (arg1 ...) proc)]
    [(_ (proc arg ...))
     (pop! "vars" () (arg ...) proc)]
    [(_ loc)
     (let ((val loc))
       (set! loc (cdr val))
       (car val))]
    [(_ . other)
     (syntax-error "malformed pop!" (pop! . other))]))

(define-syntax inc!
  (syntax-rules ()
    [(_ "vars" ((var arg) ...) () proc num)
     (let ((getter proc)
           (delta num)
           (var arg) ...)
       (let ((val (getter var ...)))
         ((setter getter) var ... (+ val delta))))]
    [(_ "vars" ((var arg) ...) (arg0 arg1 ...) proc num)
     (inc! "vars" ((var arg) ... (newvar arg0)) (arg1 ...) proc num)]
    [(_ (proc arg ...) num)
     (inc! "vars" () (arg ...) proc num)]
    [(_ (proc arg ...))
     (inc! "vars" () (arg ...) proc 1)]
    [(_ loc num)
     (let ((val loc))
       (set! loc (+ val num)))]
    [(_ loc)
     (inc! loc 1)]
    [(_ . other)
     (syntax-error "malformed inc!" (inc! . other))]))

(define-syntax dec!
  (syntax-rules ()
    [(_ "vars" ((var arg) ...) () proc num)
     (let ((getter proc)
           (delta num)
           (var arg) ...)
       (let ((val (getter var ...)))
         ((setter getter) var ... (- val delta))))]
    [(_ "vars" ((var arg) ...) (arg0 arg1 ...) proc num)
     (dec! "vars" ((var arg) ... (newvar arg0)) (arg1 ...) proc num)]
    [(_ (proc arg ...) num)
     (dec! "vars" () (arg ...) proc num)]
    [(_ (proc arg ...))
     (dec! "vars" () (arg ...) proc 1)]
    [(_ loc num)
     (let ((val loc))
       (set! loc (- val num)))]
    [(_ loc)
     (dec! loc 1)]
    [(_ . other)
     (syntax-error "malformed dec!" (dec! . other))]))

;;;-------------------------------------------------------------
;;; bind construct

;; ^ == lambda
(define-syntax ^
  (syntax-rules ()
    [(^ formals . body) (lambda formals . body)]))

;; (^x . body) == (lambda (x) . body) where x in #[a-z_]
;; TODO: need to make 'lambda's hygineic!
(define-macro (^-generator var)
  (let ([name (string->symbol (string-append "^" (symbol->string var)))])
    `(define-macro (,name . body)
       `(lambda (,',var) ,@body))))
(define-macro (define-^x . vars)
  `(begin ,@(map (lambda (x) `(^-generator ,x)) vars)))
(define-^x _ a b c d e f g h i j k l m n o p q r s t u v w x y z)

(define-syntax let1                     ;single variable bind
  (syntax-rules ()
    [(let1 var exp . body)
     (let ((var exp)) . body)]))

(define-syntax if-let1                  ;like aif in On Lisp, but explicit var
  (syntax-rules ()
    [(if-let1 var exp then . else)
     (let ((var exp)) (if var then . else))]))

(define-syntax let/cc                   ;as in PLT
  (syntax-rules ()
    [(let/cc var . body)
     (call/cc (lambda (var) . body))]))

(define-syntax begin0                   ;prog1 in Lisp.
  (syntax-rules ()
    [(begin0 exp exp2 ...)
     (receive r exp exp2 ... (apply values r))]))

(define-syntax rlet1                    ;begin0 + let1.  name is arguable.
  (syntax-rules ()
    [(rlet1 var exp body ...)
     (let ((var exp)) body ... var)]))

(define-syntax values-ref               ;nth-value in Lisp
  (syntax-rules ()
    ;; provide shortcut for common cases
    [(_ mv-expr 0)
     (receive (v . ignore) mv-expr v)]
    [(_ mv-expr 1)
     (receive (v0 v1 . ignore) mv-expr v1)]
    [(_ mv-expr 2)
     (receive (v0 v1 v2 . ignore) mv-expr v2)]
    [(_ mv-expr n)
     (receive v mv-expr (list-ref v n))]
    ;; The following extension is experimental; do not rely on them.
    [(_ mv-expr n m)
     (receive v mv-expr (values (list-ref v n) (list-ref v m)))]
    [(_ mv-expr n m l)
     (receive v mv-expr (values (list-ref v n) (list-ref v m) (list-ref v l)))]
    [(_ mv-expr n m l k ...)
     (receive v mv-expr
       (apply values
              (map (lambda (i) (list-ref v i)) (list n m l k ...))))]
    ))

;; fluid-let written by Dorai Sitaram
;; NB: all threads shares the state of fluid global vers.
;; this is mainly for the comatibility of existing third-party code.
(define-macro fluid-let 
  (lambda (varvals . body)
    (let ((vars (map car varvals))
          (vars-twins (map (lambda (ig) (gensym)) varvals))
          (swap (gensym))
          (temp (gensym)))
      `(let (,@(map list vars-twins (map cadr varvals)))
         (let ((,swap
                (lambda ()
                  ,@(map (lambda (var twin)
                           `(let ((,temp ,var))
                              (set! ,var ,twin)
                              (set! ,twin ,temp)))
                         vars vars-twins))))
           (dynamic-wind
               ,swap
               (lambda () ,@body)
               ,swap))))))

;;;-------------------------------------------------------------
;;; useful argument utility

(define-syntax check-arg
  (syntax-rules ()
    [(_ test arg)
     (let ((tmp arg))
       (unless (test tmp)
         (errorf "bad type of argument for ~s: ~s" 'arg tmp)))]
    ))

(define-syntax get-keyword*
  (syntax-rules ()
    [(_ key lis default)
     (let ((li lis))
       (let loop ((l li))
         (cond ((null? l) default)
               ((null? (cdr l)) (error "keyword list not even" li))
               ((eq? key (car l)) (cadr l))
               (else (loop (cddr l))))))]
    [(_ key lis) (get-keyword key lis)]))

(define-syntax get-optional
  (syntax-rules ()
    [(_ args default)
     (let ((a args))
       (if (pair? a) (car a) default))]
    [(_ . other)
     (syntax-error "badly formed get-optional" (get-optional . other))]
    ))

;;;-------------------------------------------------------------
;;; repeat construct

(define-syntax dotimes
  (syntax-rules ()
    [(_ (var n res) . body)
     (do ((limit n)
          (var 0 (+ var 1)))
         ((>= var limit) res)
       . body)]
    [(_ (var n) . body)
     (do ((limit n)
          (var 0 (+ var 1)))
         ((>= var limit))
       . body)]
    [(_ . other)
     (syntax-error "malformed dotimes" (dotimes . other))]))

(define-syntax dolist
  (syntax-rules ()
    [(_ (var lis res) . body)
     (begin (for-each (lambda (var) . body) lis)
            (let ((var '())) res))      ;bound var for CL compatibility
     ]
    [(_ (var lis) . body)
     (begin (for-each (lambda (var) . body) lis) '())]
    [(_ . other)
     (syntax-error "malformed dolist" (dolist . other))]))

(define-syntax while
  (syntax-rules (=>)
    [(_ expr guard => var . body)
     (do ((var expr expr))
         ((not (guard var)))
       . body)]
    [(_ expr => var . body)
     (do ((var expr expr))
         ((not var))
       . body)]
    [(_ expr . body)
     (do ()
         ((not expr))
       . body)]
    [(_ . other)
     (syntax-error "malformed while" (while . other))]))

(define-syntax until
  (syntax-rules (=>)
    [(_ expr guard => var . body)
     (do ((var expr expr))
         ((guard var))
       . body)]
    [(_ expr => var . body)
     (do ((var expr expr))
         (var)
       . body)]
    [(_ expr . body)
     (do ()
         (expr)
       . body)]
    [(_ . other)
     (syntax-error "malformed until" (until . other))]))

;;;-------------------------------------------------------------
;;; guard (srfi-34)

(define-syntax guard
  (syntax-rules ()
    [(guard (var . clauses) . body)
     (with-error-handler
         (lambda (e)
           (let ((var e))
             (%guard-rec var e . clauses)))
       (lambda () . body)
       :rewind-before #t)]))

(define-syntax %guard-rec
  (syntax-rules (else =>)
    [(%guard-rec var exc)
     (raise exc)]
    [(%guard-rec var exc (else . exprs))
     (begin . exprs)]
    [(%guard-rec var exc (test => proc) . more)
     (let ((tmp test))
       (if tmp
         (proc tmp)
         (%guard-rec var exc . more)))]
    [(%guard-rec var exc (test . exprs) . more)
     (if test
       (begin . exprs)
       (%guard-rec var exc . more))]
    [(%guard-rec var exc other . more)
     (syntax-error "malformed guard clause" other)]))

;;;-------------------------------------------------------------
;;; unwind-protect

(define-syntax unwind-protect
  (syntax-rules ()
    [(unwind-protect body handler ...)
     (let ((h (lambda () handler ...)))
       (receive r (guard (e (else (h) (raise e))) body)
         (h)
         (apply values r)))]
    [(unwind-protect . other)
     (syntax-error "malformed unwind-protect" (unwind-protect . other))]))

