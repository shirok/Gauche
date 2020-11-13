(define-module srfi-192
  (export make-i/o-invalid-position-error
          i/o-invalid-position-error?
          port-has-port-position?       ;built-in
          port-position                 ;built-in
          port-has-set-port-position!?  ;built-in
          set-port-position!))          ;built-in
(select-module srfi-192)

;; <io-invalid-position-error> is a subclass of <port-error>.  For
;; Gauche-specific code, we recommend using Gauche 'error' to raise
;; this condition, adding port information.
;; If you raise this error inside set-position! callback of srfi-181
;; custom ports, it takes care of the port slot.
(define (make-i/o-invalid-position-error position)
  (make <io-invalid-position-error> :position position))

(define (i/o-invalid-position-error? obj)
  (condition-has-type? obj <io-invalid-position-error>))
