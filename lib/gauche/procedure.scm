;;;
;;; procedure.scm - auxiliary procedure utilities.  to be autoloaded.
;;;
;;;  Copyright(C) 2002 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: procedure.scm,v 1.8 2002-12-04 04:47:55 shirok Exp $
;;;

(define-module gauche.procedure
  (use srfi-1)
  (use srfi-2)
  (export compose pa$ map$ for-each$ apply$
          any-pred every-pred
          let-optionals* let-keywords* get-optional
          arity procedure-arity-includes?
          <arity-at-least> arity-at-least? arity-at-least-value
          ))

(select-module gauche.procedure)

;; Combinator utilities -----------------------------------------

(define (pa$ fn . args)                  ;partial apply
  (lambda more-args (apply fn (append args more-args))))

(define (compose f g . more)
  (if (null? more)
      (lambda args
        (call-with-values (lambda () (apply g args)) f))
      (compose f (apply compose g more))))

(define (compose$ f) (pa$ compose f))

(define (map$ proc)      (pa$ map proc))
(define (for-each$ proc) (pa$ for-each proc))
(define (apply$ proc)    (pa$ apply proc))

(define (any-pred . preds)
  (lambda args
    (let loop ((preds preds))
      (cond ((null? preds) #f)
            ((apply (car preds) args))
            (else (loop (cdr preds)))))))

(define (every-pred . preds)
  (if (null? preds)
      (lambda args #t)
      (lambda args
        (let loop ((preds preds))
          (cond ((null? (cdr preds))
                 (apply (car preds) args))
                ((apply (car preds) args)
                 (loop (cdr preds)))
                (else #f))))))

;; Macros for optional arguments ---------------------------

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
;; beforehand, so I use a traditional macro as a wrapper.

(define-macro (let-keywords* arg vars . body)
  (let* ((tmp (gensym))
         (triplets
          (map (lambda (var&default)
                 (or (and-let* (((list? var&default))
                                (var (unwrap-syntax (car var&default)))
                                ((symbol? var)))
                       (case (length var&default)
                         ((2) `(,var
                                ,(make-keyword var)
                                ,(cadr var&default)))
                         ((3) `(,var ,(unwrap-syntax (cadr var&default))
                                     ,(caddr var&default)))
                         (else #f)))
                     (error "bad binding form in let-keywords*" var&default)))
               vars)))
    `(let* ((,tmp ,arg)
            ,@(map (lambda (binds)
                     `(,(car binds)
                       (get-keyword* ,(cadr binds) ,tmp ,(caddr binds))))
                   triplets))
       ,@body)))

;; Procedure arity -----------------------------------------

(define-class <arity-at-least> ()
  ((value :init-keyword :value :init-value 0)))

(define-method write-object ((obj <arity-at-least>) port)
  (format port "#<arity-at-least ~a>" (ref obj 'value)))

(define (arity-at-least? x) (is-a? x <arity-at-least>))

(define (arity-at-least-value x)
  (check-arg arity-at-least? x)
  (ref x 'value))

(define (arity proc)
  (cond
   ((or (is-a? proc <procedure>) (is-a? proc <method>))
    (if (ref proc 'optional)
        (make <arity-at-least> :value (ref proc 'required))
        (ref proc 'required)))
   ((is-a? proc <generic>)
    (map arity (ref proc 'methods)))
   (else
    (errorf "cannot get arity of ~s" proc))))

(define (procedure-arity-includes? proc k)
  (let1 a (arity proc)
    (define (check a)
      (cond ((integer? a) (= a k))
            ((arity-at-least? a) (>= k (arity-at-least-value a)))
            (else (errorf "implementation error in (procedure-arity-includes? ~s ~s)" proc k))))
    (if (list? a)
        (any check a)
        (check a))))

(provide "gauche/procedure")
