;;;
;;; common-macros.scm - common macros
;;;
;;;  Copyright(C) 2001 by Shiro Kawai (shiro@acm.org)
;;;
;;;  Permission to use, copy, modify, distribute this software and
;;;  accompanying documentation for any purpose is hereby granted,
;;;  provided that existing copyright notices are retained in all
;;;  copies and that this notice is included verbatim in all
;;;  distributions.
;;;  This software is provided as is, without express or implied
;;;  warranty.  In no circumstances the author(s) shall be liable
;;;  for any damages arising out of the use of this software.
;;;
;;;  $Id: common-macros.scm,v 1.4 2001-12-01 10:58:14 shirok Exp $
;;;

;;; Defines number of useful macros.  This file is loaded by
;;; gauche-init.scm

(select-module gauche)

;;; syntax-error
;;; syntax-errorf
;;;   Signals an error at compile time.

(define-macro (syntax-error . args)
  (apply error (map unwrap-syntax args)))

(define-macro (syntax-errorf . args)
  (apply errorf (map unwrap-syntax args)))

;; strip off syntactic information from identifiers in the macro output.
(define (unwrap-syntax form)
  (cond
   ((identifier? form) (identifier->symbol form))
   ((pair? form) (cons (unwrap-syntax (car form))
                       (unwrap-syntax (cdr form))))
   ((vector? form)
    (list->vector (map unwrap-syntax (vector->list form))))
   (else form)))

;;;-------------------------------------------------------------
;;; generalized set! family

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
;;; repeat construct

(define-syntax dotimes
  (syntax-rules ()
    ((_ (var n res) . body)
     (do ((limit n)
          (var 0 (+ var 1)))
         ((>= var limit) res)
       . body))
    ((_ (var n) body . body)
     (do ((limit n)
          (var 0 (+ var 1)))
         ((>= var limit))
       . body))
    ((_ . other)
     (syntax-error "malformed dotimes" (dotimes . other)))))

(define-syntax while
  (syntax-rules ()
    ((_ expr . body)
     (do ()
         ((not expr))
       . body))
    ((_ . other)
     (syntax-error "malformed while" (while . other)))))

(define-syntax until
  (syntax-rules ()
    ((_ expr . body)
     (do ()
         (expr)
       . body))
    ((_ . other)
     (syntax-error "malformed until" (until . other)))))

(provide "gauche/common-macros")
