;;;
;;; Handy macro expander
;;;

;;
;; These routines are handy to check macro expansion process interactively.
;;

(define-module gauche.macroutil
  (export xmac xmac1))
(select-module gauche.macroutil)

(define-syntax xmac
  (syntax-rules ()
    ((_ ?form)
     (unwrap-syntax (%macroexpand ?form)))))

(define-syntax xmac1
  (syntax-rules ()
    ((_ ?form)
     (unwrap-syntax (%macroexpand-1 ?form)))))

