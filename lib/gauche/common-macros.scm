;;;
;;; common-macros.scm - common macros
;;;  
;;;   Copyright (c) 2000-2007 Shiro Kawai  (shiro@acm.org)
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
;;;  $Id: common-macros.scm,v 1.26 2007-01-21 14:21:51 rui314159 Exp $
;;;

;;; Defines number of useful macros.  This file is to be autoloaded.

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
    ((_ "vars" ((var arg) ...) () proc updater val ...)
     (let ((getter proc)
           (var arg) ...)
       ((setter getter) var ... (updater val ... (getter var ...)))))
    ((_ "vars" ((var arg) ...) (arg0 arg1 ...) proc updater val ...)
     (update! "vars"
              ((var arg) ... (newvar arg0))
              (arg1 ...)
              proc updater val ...))
    ((_ (proc arg ...) updater val ...)
     (update! "vars"
              ()
              (arg ...)
              proc updater val ...))
    ((_ loc updater val ...)
     (set! loc (updater val ... loc)))
    ((_ . other)
     (syntax-error "malformed update!" (update! . other)))))

(define-syntax push!
  (syntax-rules ()
    ((_ "vars" ((var arg) ...) () proc val)
     (let ((getter proc)
           (var arg) ...)
       ((setter getter) var ... (cons val (getter var ...)))))
    ((_ "vars" ((var arg) ...) (arg0 arg1 ...) proc val)
     (push! "vars" ((var arg) ... (newvar arg0)) (arg1 ...) proc val))
    ((_ (proc arg ...) val)
     (push! "vars" () (arg ...) proc val))
    ((_ loc val)
     (set! loc (cons val loc)))
    ((_ . other)
     (syntax-error "malformed push!" (push! . other)))))

(define-syntax pop!
  (syntax-rules ()
    ((_ "vars" ((var arg) ...) () proc)
     (let ((getter proc)
           (var arg) ...)
       (let ((val (getter var ...)))
         ((setter getter) var ... (cdr val))
         (car val))))
    ((_ "vars" ((var arg) ...) (arg0 arg1 ...) proc)
     (pop! "vars" ((var arg) ... (newvar arg0)) (arg1 ...) proc))
    ((_ (proc arg ...))
     (pop! "vars" () (arg ...) proc))
    ((_ loc)
     (let ((val loc))
       (set! loc (cdr val))
       (car val)))
    ((_ . other)
     (syntax-error "malformed pop!" (pop! . other)))))

(define-syntax inc!
  (syntax-rules ()
    ((_ "vars" ((var arg) ...) () proc num)
     (let ((getter proc)
           (delta num)
           (var arg) ...)
       (let ((val (getter var ...)))
         ((setter getter) var ... (+ val delta)))))
    ((_ "vars" ((var arg) ...) (arg0 arg1 ...) proc num)
     (inc! "vars" ((var arg) ... (newvar arg0)) (arg1 ...) proc num))
    ((_ (proc arg ...) num)
     (inc! "vars" () (arg ...) proc num))
    ((_ (proc arg ...))
     (inc! "vars" () (arg ...) proc 1))
    ((_ loc num)
     (let ((val loc))
       (set! loc (+ val num))))
    ((_ loc)
     (inc! loc 1))
    ((_ . other)
     (syntax-error "malformed inc!" (inc! . other)))))

(define-syntax dec!
  (syntax-rules ()
    ((_ "vars" ((var arg) ...) () proc num)
     (let ((getter proc)
           (delta num)
           (var arg) ...)
       (let ((val (getter var ...)))
         ((setter getter) var ... (- val delta)))))
    ((_ "vars" ((var arg) ...) (arg0 arg1 ...) proc num)
     (dec! "vars" ((var arg) ... (newvar arg0)) (arg1 ...) proc num))
    ((_ (proc arg ...) num)
     (dec! "vars" () (arg ...) proc num))
    ((_ (proc arg ...))
     (dec! "vars" () (arg ...) proc 1))
    ((_ loc num)
     (let ((val loc))
       (set! loc (- val num))))
    ((_ loc)
     (dec! loc 1))
    ((_ . other)
     (syntax-error "malformed dec!" (dec! . other)))))

;;;-------------------------------------------------------------
;;; bind construct

(define-syntax let1                     ;single variable bind
  (syntax-rules ()
    ((let1 var exp . body)
     (let ((var exp)) . body))))

(define-syntax let/cc                   ;as in PLT
  (syntax-rules ()
    ((let/cc var . body)
     (call/cc (lambda (var) . body)))))

(define-syntax begin0                   ;prog1 in Lisp.
  (syntax-rules ()
    ((begin0 exp exp2 ...)
     (receive r exp exp2 ... (apply values r)))))

(define-syntax values-ref               ;nth-value in Lisp
  (syntax-rules ()
    ;; provide shortcut for common cases
    ((_ mv-expr 0)
     (receive (v . ignore) mv-expr v))
    ((_ mv-expr 1)
     (receive (v0 v1 . ignore) mv-expr v1))
    ((_ mv-expr 2)
     (receive (v0 v1 v2 . ignore) mv-expr v2))
    ((_ mv-expr n)
     (receive v mv-expr (list-ref v n)))
    ;; The following extension is experimental; do not rely on them.
    ((_ mv-expr n m)
     (receive v mv-expr (values (list-ref v n) (list-ref v m))))
    ((_ mv-expr n m l)
     (receive v mv-expr (values (list-ref v n) (list-ref v m) (list-ref v l))))
    ((_ mv-expr n m l k ...)
     (receive v mv-expr
       (apply values
              (map (lambda (i) (list-ref v i)) (list n m l k ...)))))
    ))

;; fluid-let written by Dorai Sitaram
;; NB: all threads shares the state of fluid global vers.
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
    ((_ test arg)
     (let ((tmp arg))
       (unless (test tmp)
         (errorf "bad type of argument for ~s: ~s" 'arg tmp))))
    ))

(define-syntax get-keyword*
  (syntax-rules ()
    ((_ key lis default)
     (let ((li lis))
       (let loop ((l li))
         (cond ((null? l) default)
               ((null? (cdr l)) (error "keyword list not even" li))
               ((eq? key (car l)) (cadr l))
               (else (loop (cddr l)))))))
    ((_ key lis) (get-keyword key lis))))

(define-syntax get-optional
  (syntax-rules ()
    ((_ args default)
     (let ((a args))
       (if (pair? a) (car a) default)))
    ((_ . other)
     (syntax-error "badly formed get-optional" (get-optional . other)))
    ))

(define-syntax let-optionals*
  (syntax-rules ()
    ((_ "binds" arg binds () body) (let* binds . body))
    ((_ "binds" arg (binds ...) ((var default) . more) body)
     (let-optionals* "binds"
         (if (null? tmp) tmp (cdr tmp))
       (binds ...
              (tmp arg)
              (var (if (null? tmp) default (car tmp))))
       more
       body))
    ((_ "binds" arg (binds ...) (var . more) body)
     (let-optionals* "binds"
         (if (null? tmp) tmp (cdr tmp))
       (binds ...
              (tmp arg)
              (var (if (null? tmp) (undefined) (car tmp))))
       more
       body))
    ((_ "binds" arg (binds ...) var body)
     (let-optionals* "binds"
         arg
       (binds ... (var arg))
       ()
       body))
    ((_ arg vars . body)
     (let-optionals* "binds" arg () vars body))
    ((_ . other)
     (syntax-error "badly formed let-optionals*" (let-optionals* . other)))
    ))

;; We want to generate corresponding keyword for each variable
;; beforehand, so I use a traditional macro.

(define (%let-keywords-rec arg specs body %let %error/warn)
  (define (triplet var&default)
    (or (and-let* (((list? var&default))
                   (var (unwrap-syntax (car var&default)))
                   ((symbol? var)))
          (case (length var&default)
            ((2) (values (car var&default)
                         (make-keyword var)
                         (cadr var&default)))
            ((3) (values (car var&default)
                         (unwrap-syntax (cadr var&default))
                         (caddr var&default)))
            (else #f)))
        (error "bad binding form in let-keywords" var&default)))
  (define (process-specs specs)
    (let loop ((specs specs)
               (vars '()) (keys '()) (defaults '()) (tmps '()))
      (define (finish restvar)
        (values (reverse! vars)
                (reverse! keys)
                (reverse! defaults)
                (reverse! tmps)
                restvar))
      (cond ((null? specs) (finish #f))
            ((pair? specs)
             (receive (var key default) (triplet (car specs))
               (loop (cdr specs)
                     (cons var vars)
                     (cons key keys)
                     (cons default defaults)
                     (cons (gensym) tmps))))
            (else (finish (or specs #t))))))

  (let ((argvar (gensym "args")) (loop (gensym "loop")))
    (receive (vars keys defaults tmps restvar)
        (process-specs specs)
      `(let ,loop ((,argvar ,arg)
                   ,@(if (boolean? restvar) '() `((,restvar '())))
                   ,@(map (cut list <> (undefined)) tmps))
            (cond
             ((null? ,argvar)
              (,%let ,(map (lambda (var tmp default)
                             `(,var (if (undefined? ,tmp) ,default ,tmp)))
                           vars tmps defaults)
                     ,@body))
             ((null? (cdr ,argvar))
              (error "keyword list not even" ,argvar))
             (else
              (case (car ,argvar)
                ,@(map (lambda (key)
                         `((,key)
                           (,loop (cddr ,argvar)
                                  ,@(if (boolean? restvar) '() `(,restvar))
                                  ,@(map (lambda (k t)
                                           (if (eq? key k) `(cadr ,argvar) t))
                                         keys tmps))))
                       keys)
                (else
                 ,(cond ((eq? restvar #t)
                         `(,loop (cddr ,argvar) ,@tmps))
                        ((eq? restvar #f)
                         `(begin (,%error/warn "unknown keyword ~S" (car ,argvar))
                                 (,loop (cddr ,argvar) ,@tmps)))
                        (else
                         `(,loop (cddr ,argvar)
                                 (cons (car ,argvar) (cons (cadr ,argvar) ,restvar))
                                 ,@tmps)))))))))))

(define-macro (let-keywords arg specs . body)
  (%let-keywords-rec arg specs body 'let 'errorf))

(define-macro (let-keywords* arg specs . body)
  (%let-keywords-rec arg specs body 'let* 'warn))

;;;-------------------------------------------------------------
;;; repeat construct

(define-syntax dotimes
  (syntax-rules ()
    ((_ (var n res) . body)
     (do ((limit n)
          (var 0 (+ var 1)))
         ((>= var limit) res)
       . body))
    ((_ (var n) . body)
     (do ((limit n)
          (var 0 (+ var 1)))
         ((>= var limit))
       . body))
    ((_ . other)
     (syntax-error "malformed dotimes" (dotimes . other)))))

(define-syntax dolist
  (syntax-rules ()
    ((_ (var lis res) . body)
     (begin (for-each (lambda (var) . body) lis)
            (let ((var '())) res))      ;bound var for CL compatibility
     )
    ((_ (var lis) . body)
     (begin (for-each (lambda (var) . body) lis) '()))
    ((_ . other)
     (syntax-error "malformed dolist" (dolist . other)))))

(define-syntax while
  (syntax-rules (=>)
    ((_ expr guard => var . body)
     (do ((var expr expr))
         ((not (guard var)))
       . body))
    ((_ expr => var . body)
     (do ((var expr expr))
         ((not var))
       . body))    
    ((_ expr . body)
     (do ()
         ((not expr))
       . body))
    ((_ . other)
     (syntax-error "malformed while" (while . other)))))

(define-syntax until
  (syntax-rules (=>)
    ((_ expr guard => var . body)
     (do ((var expr expr))
         ((guard var))
       . body))
    ((_ expr => var . body)
     (do ((var expr expr))
         (var)
       . body))
    ((_ expr . body)
     (do ()
         (expr)
       . body))
    ((_ . other)
     (syntax-error "malformed until" (until . other)))))

;;;-------------------------------------------------------------
;;; guard (srfi-34)

(define-syntax guard
  (syntax-rules ()
    ((guard (var . clauses) . body)
     (with-error-handler
         (lambda (e)
           (let ((var e))
             (%guard-rec var e . clauses)))
       (lambda () . body)
       :rewind-before #t))
    ))

(define-syntax %guard-rec
  (syntax-rules (else =>)
    ((%guard-rec var exc)
     (raise exc))
    ((%guard-rec var exc (else . exprs))
     (begin . exprs))
    ((%guard-rec var exc (test => proc) . more)
     (let ((tmp test))
       (if tmp
         (proc tmp)
         (%guard-rec var exc . more))))
    ((%guard-rec var exc (test . exprs) . more)
     (if test
       (begin . exprs)
       (%guard-rec var exc . more)))
    ((%guard-rec var exc other . more)
     (syntax-error "malformed guard clause" other))))

;;;-------------------------------------------------------------
;;; unwind-protect

(define-syntax unwind-protect
  (syntax-rules ()
    ((unwind-protect body handler)
     (letrec ((h (lambda () handler)))
       (receive r (guard (e (else (h) (raise e))) body)
         (h)
         (apply values r))))
    ((unwind-protect . other)
     (syntax-error "malformed unwind-protect" (unwind-protect . other)))))

(provide "gauche/common-macros")
