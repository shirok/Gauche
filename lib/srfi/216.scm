;;
;; srfi.216 - SICP prerequisites
;;

(define-module srfi.216
  (use control.pmap)
  (use gauche.threads)
  (use util.stream)
  (use srfi.27)
  (export false true nil runtime random
          parallel-execute test-and-set!
          cons-stream the-empty-stream stream-null?)
  )
(select-module srfi.216)

(define-constant false #f)
(define-constant true #t)
(define-constant nil '())

(define (runtime)
  (round->exact (* (expt 10 6)
                   (time->seconds (current-time)))))

(define (random n)
  (assume-type n <real>)
  (assume (> n 0))
  (if (exact-integer? n)
    (random-integer n)
    (inexact (random-integer (ceiling->exact n)))))

(define (pararallel-execute thunk1 . thunks)
  (pmap (^p (p)) (cons thunk1 thunks) (make-fully-concurrent-mapper)))
