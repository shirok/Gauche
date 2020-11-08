(define-module srfi-192
  (export make-i/o-invalid-position-error
          i/o-invalid-position-error?
          port-has-port-position?       ;built-in
          port-position                 ;built-in
          port-has-set-port-position!?  ;built-in
          set-port-position!))          ;built-in
(select-module srfi-192)

;; For the portability.
;; Gauche-specific code should use srfi-34 condition to create
;; a compound condition of <io-invalid-position-error-mixin> and
;; <port-error>.
(define (make-i/o-invalid-position-error position)
  (make <io-invalid-position-error-mixin> :position position))

(define (i/o-invalid-position-error? obj)
  (condition-has-type? obj <io-invalid-position-error-mixin>))
