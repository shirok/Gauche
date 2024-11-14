;;;
;;; srfi-230 Atomic Operations
;;;

;; This is a thin wrapper of gauche.atomic

(define-module srfi.230
  (use gauche.atomic)
  (use util.match)
  (export memory-order memory-order?
          atomic-fence

          ;; Following procedures are defined in gauche.atomic
          make-atomic-box atomic-box?
          atomic-box-ref atomic-box-set!
          atomic-box-swap! atomic-box-compare-and-swap!

          make-atomic-flag atomic-flag?
          atomic-flag-test-and-set! atomic-flag-clear!

          make-atomic-fxbox atomic-fxbox?
          atomic-fxbox-ref atomic-fxbox-set!
          atomic-fxbox-swap! atomic-fxbox-compare-and-swap!
          atomic-fxbox+/fetch! atomic-fxbox-/fetch!
          atomic-fxbox-and/fetch!
          atomic-fxbox-ior/fetch! atomic-fxbox-xor/fetch!

          make-atomic-pair atomic-pair?
          atomic-pair-ref atomic-pair-set!
          atomic-pair-swap! atomic-pair-compare-and-swap!)
  )

(select-module srfi.230)

(define-constant *memory-orders*
  '(relaxed acquire release acquire-release sequentially-consistent))

(define-syntax memory-order
  (er-macro-transformer
   (^[f r c]
     (match (cdr f)
       [(order)
        (if (memq (unwrap-syntax order) *memory-orders*)
          (quasirename r `',order)
          (errorf "Memory order must be one of ~s, but got: ~s"
                  *memory-orders* ',order))]
       [_ (error "Malformed memory-order:" f)]))))

(define (memory-order? obj)
  (boolean (memq obj *memory-orders*)))

(define (atomic-fence memory-order)
  (undefined))
