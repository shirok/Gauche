;;;
;;; Handy macro expander
;;;

;;; $Id: macroutil.scm,v 1.2 2001-10-03 09:17:38 shirok Exp $

;;
;; These routines are handy to check macro expansion process interactively.
;;

(define-module gauche.macroutil
  (export xmac xmac1))
(select-module gauche.macroutil)

(define-syntax xmac
  (syntax-rules ()
    ((_ ?form)
     (unwrap-syntax (%macro-expand ?form)))))

(define-syntax xmac1
  (syntax-rules ()
    ((_ ?form)
     (unwrap-syntax (%macro-expand-1 ?form)))))

(provide "gauche/macroutil")
