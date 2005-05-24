;;;
;;; SRFI-55   require-extension
;;;
;;; Written by Shiro Kawai
;;;
;;; $Id: srfi-55.scm,v 1.1 2005-05-24 09:52:56 shirok Exp $
;;;

;; This file is to be autoloaded

(define-module srfi-55
  (export require-extension))
(select-module srfi-55)

;; We expand require-extension into cond-expand, which will
;; load and imports required features if necessary.
;; (This depends on the fact that Gauche's cond-expand has an effect
;; after the form).

(define-macro (require-extension . clauses)
  (define (rec clauses)
    (cond ((null? clauses) #t)
          ((pair? (car clauses))
           (if (eq? (unwrap-syntax (caar clauses)) 'srfi)
             (require-srfi (cdar clauses) (cdr clauses))
             (error "require-extension: unknown extension identifier:"
                    (car claues))))
          (else
           (error "require-extension: bad clause:" (car clauses)))))
  (define (require-srfi ids rest)
    (if (null? ids)
      (rec rest)
      (let ((id (string->symbol #`"srfi-,(car ids)")))
        `(cond-expand (,id ,(require-srfi (cdr ids) rest))
                      (else (syntax-error "unsupported srfi:" ,id))))))
  (rec clauses))

(provide "srfi-55")



     

