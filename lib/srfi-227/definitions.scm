;;;
;;; srfi-227 sublibrary
;;;

(define-module srfi-227.definitions
  (use srfi-227)
  (export define-optionals define-optionals*))
(select-module srfi-227.definitions)

(define-syntax define-optionals
  (syntax-rules ()
    [(_ (name . opt-formals) . body)
     (define name (opt-lambda opt-formals . body))]))

(define-syntax define-optionals*
  (syntax-rules ()
    [(_ (name . opt-formals) . body)
     (define name (opt*-lambda opt-formals . body))]))
