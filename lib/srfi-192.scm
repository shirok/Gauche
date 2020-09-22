;;;
;;; From srfi-102 reference implementation by Shiro Kawai
;;; MIT License
;;;

;; At this moment, it isn't possible to faithfully implment srfi-192
;; on top of Gauche's port protocol.  We'll revise this as the port
;; protocol evolves.

(define-module srfi-192
  (export make-i/o-invalid-position-error
          port-has-port-position?       ;built-in
          port-position                 ;built-in
          port-has-set-port-position!?  ;built-in
          set-port-position!))          ;built-in
(select-module srfi-192)

;; It'd be natural for this condition to inherit <port-error>, but
;; the public constructor doens't take the port.  So we implement it
;; as a mixin condition.  The recommended use is to make a compound
;; condition with a <port-error>.
(define-condition-type <i/o-invalid-position-error-mixin> <condition>
  i/o-invalid-position-error?
  (position))

(define (make-i/o-invalid-position-error position)
  (make <i/o-invalid-position-error-mixin> :position position))
