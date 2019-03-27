;;
;; This code measures the effect of TAILP-APPLY 
;;

(use gauche.time)

(define Apply #f)                       ; prevent apply inlining
  
(define (foo a b c d e f g h i j . args)
  (+ a b c d e f g h i j))

(define (main args)
  (set! Apply apply)
  (time (dotimes [10000000]
          (Apply foo 1 2 3 4 5 6 7 8 9 10 '(a b c))))
  0)

