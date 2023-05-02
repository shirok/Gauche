;;;
;;; compat.real-elementary-functions
;;;

;; Gauche 0.9.12 and earlier had real-only trigonometric functions
;; under undocumented names.
;; 0.9.13 renamed them as real-sin etc. and made them official.  This
;; module is to run code that used old undocumented API.

(define-module compat.real-elementary-functions
  (export %exp %sin %cos %tan %log %sinpi %cospi %tanpi %asin %acos %atan
          %sinh %cosh %tanh %asinh %acosh %atanh %sqrt %expt))
(select-module compat.real-elementary-functions)

(define %exp real-exp)
(define %sin real-sin)
(define %cos real-cos)
(define %tan real-tan)
(define %log real-ln)
(define %sinpi real-sinpi)
(define %cospi real-cospi)
(define %tanpi real-tanpi)
(define %asin real-asin)
(define %acos real-acos)
(define %atan real-atan)
(define %sinh real-sinh)
(define %cosh real-cosh)
(define %tanh real-tanh)
(define %asinh real-asinh)
(define %acosh real-acosh)
(define %atanh real-atanh)
(define %sqrt real-sqrt)
(define %expt real-expt)
