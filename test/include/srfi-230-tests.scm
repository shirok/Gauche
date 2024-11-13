(import (scheme base)
        (scheme list)
        (srfi 78)
        (srfi 230))

(cond-expand
 ((library (srfi 18)) (import (srfi 18)))
 (else))

;; To test collision situation, this needs to be not too small,
;; though too many threads may slow down the tests.
(define *num-test-threads* 1000)

;;;
;;; Atomic flags
;;;

(let ()
  (define f (make-atomic-flag))
  (check (atomic-flag? f) => #t)
  (check (atomic-flag-test-and-set! f) => #f)
  (check (atomic-flag-test-and-set! f) => #t)
  (atomic-flag-clear! f)
  (check (atomic-flag-test-and-set! f) => #f)
  )

(cond-expand
 ((library (srfi 18))
  (let ()
    (define flag (make-atomic-flag))
    (define counter 0)
    (define (increment! delta)
      (cond ((atomic-flag-test-and-set! flag)
             ;; somebody's working
             (thread-sleep! 0.000001)
             (increment! delta))        ; retry
            (else                       ; we got lock
             (set! counter (+ counter delta))
             (atomic-flag-clear! flag))))
    (define threads (unfold (lambda (x) (= x *num-test-threads*))
                            (lambda (x) (make-thread
                                         (lambda ()
                                           (thread-sleep! 0.000001)
                                           (increment! x))))
                            (lambda (x) (+ x 1))
                            0))
    (for-each thread-start! threads)
    (for-each thread-join! threads)
    (check counter => (fold + 0 (iota *num-test-threads*)))
    )))

;;;
;;; Atomic box
;;;

(let ()
  (define b (make-atomic-box 'foo))
  (check (atomic-box? b) => #t)
  (check (atomic-box-ref b) => 'foo)
  (atomic-box-set! b 'bar)
  (check (atomic-box-ref b) => 'bar)
  (check (atomic-box-swap! b 'baz) => 'bar)
  (check (atomic-box-ref b) => 'baz)
  (check (atomic-box-compare-and-swap! b 'foo 'boo) => 'baz)
  (check (atomic-box-ref b) => 'baz)
  (check (atomic-box-compare-and-swap! b 'baz 'boo) => 'baz)
  (check (atomic-box-ref b) => 'boo)
  )

(cond-expand
 ((library (srfi 18))
  (let ()
    (define box (make-atomic-box #f))
    (define counter 0)
    (define (increment! delta)
      (cond ((atomic-box-compare-and-swap! box #f #t)
             ;; somebody's working
             (thread-sleep! 0.000001)
             (increment! delta))        ; retry
            (else                       ; we got lock
             (set! counter (+ counter delta))
             (atomic-box-set! box #f))))
    (define threads (unfold (lambda (x) (= x *num-test-threads*))
                            (lambda (x) (make-thread
                                         (lambda ()
                                           (thread-sleep! 0.000001)
                                           (increment! x))))
                            (lambda (x) (+ x 1))
                            0))
    (for-each thread-start! threads)
    (for-each thread-join! threads)
    (check counter => (fold + 0 (iota *num-test-threads*)))
    )))

;;;
;;; Atomic fxbox
;;;

(let ()
  (define x (make-atomic-fxbox 0))
  (check (atomic-fxbox? x) => #t)
  (check (atomic-fxbox-ref x) => 0)
  (atomic-fxbox-set! x 1)
  (check (atomic-fxbox-ref x) => 1)
  (check (atomic-fxbox-swap! x 3) => 1)
  (check (atomic-fxbox-ref x) => 3)
  (check (atomic-fxbox-compare-and-swap! x 1 5) => 3)
  (check (atomic-fxbox-ref x) => 3)
  (check (atomic-fxbox-compare-and-swap! x 3 5) => 3)
  (check (atomic-fxbox-ref x) => 5)

  (check (atomic-fxbox+/fetch! x 3) => 5)
  (check (atomic-fxbox-ref x) => 8)
  (check (atomic-fxbox-/fetch! x 1) => 8)
  (check (atomic-fxbox-ref x) => 7)

  (check (atomic-fxbox-and/fetch! x #x55) => 7)
  (check (atomic-fxbox-ref x) => 5)
  (check (atomic-fxbox-ior/fetch! x #xaa) => 5)
  (check (atomic-fxbox-ref x) => #xaf)
  (check (atomic-fxbox-xor/fetch! x #x15) => #xaf)
  (check (atomic-fxbox-ref x) => #xba)
  )

(cond-expand
 ((library (srfi 18))
  (let ()
    (define counter (make-atomic-fxbox 0))
    (define (increment! delta)
      (atomic-fxbox-/fetch! counter delta))
    (define threads (unfold (lambda (x) (= x *num-test-threads*))
                            (lambda (x) (make-thread
                                         (lambda ()
                                           (thread-sleep! 0.000001)
                                           (increment! x))))
                            (lambda (x) (+ x 1))
                            0))
    (for-each thread-start! threads)
    (for-each thread-join! threads)
    (check (atomic-fxbox-ref counter) =>
           (- (fold + 0 (iota *num-test-threads*))))
    )))
