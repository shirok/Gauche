;;
;; srfi.216 - SICP prerequisites
;;

;; NB: It is better to use Gauche-compat-sicp, which is more comprehensive
;; than this SRFI.  We have this for the compatibility.

(define-module srfi.216
  (use control.pmap)
  (use gauche.threads)
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

(define (parallel-execute thunk1 . thunks)
  (pmap (^p (p)) (cons thunk1 thunks) (make-fully-concurrent-mapper))
  (undefined))

(define *global-lock* (make-mutex))     ;for test-and-set!

(define (test-and-set! cell)
  (assume cell <pair>)
  (with-locking-mutex *global-lock*
    (^[] (if (car cell)
           #t
           (begin (set! (car cell) #t) #f)))))

(define-syntax cons-stream
  (syntax-rules ()
    ([_ a b] (cons a (delay b)))))

(define-constant the-empty-stream '#:the-empty-stream)
(define (stream-null? obj) (eq? obj the-empty-stream))
