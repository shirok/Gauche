;;;
;;; Handy macro expander
;;;

;;; $Id: macroutil.scm,v 1.1 2001-03-24 09:50:16 shiro Exp $

;;
;; These routines are handy to check macro expansion process interactively.
;;

(define-module gauche.macroutil
  (export unwrap-syntax xmac xmac1))
(select-module gauche.macroutil)

;; strip off syntactic information from identifiers in the macro output.
(define (unwrap-syntax form)
  (cond
   ((identifier? form) (identifier->symbol form))
   ((pair? form) (cons (unwrap-syntax (car form))
                       (unwrap-syntax (cdr form))))
   ((vector? form)
    (list->vector (map unwrap-syntax (vector->list form))))
   (else form)))

(define-syntax xmac
  (syntax-rules ()
    ((_ ?form)
     (unwrap-syntax (%macro-expand ?form)))))

(define-syntax xmac1
  (syntax-rules ()
    ((_ ?form)
     (unwrap-syntax (%macro-expand-1 ?form)))))

(provide "gauche/macroutil")
