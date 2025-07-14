;;
;; run-once macro inserts literal mutex (and box)
;; make sure it is compilable.
;;

(define-module literal-mutex
  (export foo)
  (use gauche.threads))
(select-module literal-mutex)

(define *v* 0)

(define (foo)
  (run-once
   (set! *v* (+ *v* 1))
   *v*))
