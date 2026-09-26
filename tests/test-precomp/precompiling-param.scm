;; Test the parameter to distinguish whether we're precompiling or not

(define-module precompiling-param
  (export call-inside call-outside precompiling-discriminator))
(select-module precompiling-param)

(define-macro (precompiling-discriminator)
  ((with-module gauche.internal precompiling?)))

(define (call-inside)
  (precompiling-discriminator))

(without-precompiling
 (define (call-outside)
   (precompiling-discriminator)))
