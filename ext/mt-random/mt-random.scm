;;;
;;; mt-random - Mersenne Twister interface
;;;
;;;  Copyright(C) 2002 by Shiro Kawai (shiro@acm.org)
;;;
;;;  Permission to use, copy, modify, distribute this software and
;;;  accompanying documentation for any purpose is hereby granted,
;;;  provided that existing copyright notices are retained in all
;;;  copies and that this notice is included verbatim in all
;;;  distributions.
;;;  This software is provided as is, without express or implied
;;;  warranty.  In no circumstances the author(s) shall be liable
;;;  for any damages arising out of the use of this software.
;;;
;;;  $Id: mt-random.scm,v 1.8 2002-09-18 22:12:27 shirok Exp $
;;;

(define-module math.mt-random
  (use gauche.uvector)
  (export <mersenne-twister>
          mt-random-set-seed!
          mt-random-get-state
          mt-random-set-state!
          mt-random-real
          mt-random-real0
          mt-random-integer
          mt-random-fill-u32vector!
          mt-random-fill-f32vector!
          mt-random-fill-f64vector!)
  )
(select-module math.mt-random)

(dynamic-load "mt-random" :export-symbols #t)

(define (%get-nword-random-int mt n)
  (let loop ((i 0) (r (%mt-random-uint32 mt)))
    (if (= i n)
        r
        (loop (+ i 1)
              (+ (ash r 32) (%mt-random-uint32 mt))))))

(define (mt-random-integer mt n)
  (when (not (positive? n)) (error "invalid range" n))
  (if (<= n #x100000000)
      (%mt-random-integer mt n)
      (let* ((siz (ash (integer-length n) -5))
             (q   (quotient (ash 1 (* 32 (+ siz 1))) n))
             (qn  (* q n)))
        (let loop ((r (%get-nword-random-int mt siz)))
          (if (< r qn)
              (quotient r q)
              (loop #(%get-nword-random-int mt siz)))))))



(provide "math/mt-random")
