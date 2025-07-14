;; SRFI-230 tests
;;
;;  Gauche:  gosh -Ilib srfi-230-tests.scm
;;  Chibi:   chibi-scheme -I lib srfi-230-tests.scm
;;

(import (scheme base)
        (scheme list)
        (srfi 64)
        (srfi 230))

(cond-expand
 ((library (srfi 18)) (import (srfi 18)))
 (else))

(cond-expand
 ((library (srfi 210)) (import (only (srfi 210) list/mv)))
 (else
  ;; not a complete replacement, but this suffices for our purpose.
  (define-syntax list/mv
    (syntax-rules ()
      ((_ mv-expr) (call-with-values (lambda () mv-expr) list))))))

;; To test collision situations, this needs to be not too small,
;; though too many threads may slow down the tests.
(define *num-test-threads* 500)

(test-begin "SRFI 230 Atomic operations")

;;;
;;; Memory order
;;;;

(test-group "Memory order"
  (test-assert (memory-order? (memory-order relaxed)))
  (test-assert (memory-order? (memory-order acquire)))
  (test-assert (memory-order? (memory-order release)))
  (test-assert (memory-order? (memory-order acquire-release)))
  (test-assert (memory-order? (memory-order sequentially-consistent)))
  (test-eqv #f (memory-order? 'invalid-memory-order))
  )

;;;
;;; Atomic flags
;;;

(test-group "Atomic flags"
  (let ()
    (define f (make-atomic-flag))
    (test-assert (atomic-flag? f))
    (test-eqv #f (atomic-flag-test-and-set! f))
    (test-eqv #t (atomic-flag-test-and-set! f))
    (atomic-flag-clear! f)
    (test-eqv #f (atomic-flag-test-and-set! f))
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
               (atomic-flag-clear! flag)
               (atomic-fence (memory-order sequentially-consistent)))))
      (define threads (map (lambda (x)
                             (make-thread
                              (lambda ()
                                (thread-sleep! 0.000001)
                                (increment! x))))
                           (iota *num-test-threads*)))
      (for-each thread-start! threads)
      (for-each thread-join! threads)
      (test-eqv (apply + (iota *num-test-threads*)) counter)
      ))
   (else))
  )

;;;
;;; Atomic box
;;;

(test-group "Atomic box"
  (let ()
    (define b (make-atomic-box 'foo))
    (test-assert (atomic-box? b))
    (test-eq 'foo (atomic-box-ref b))
    (atomic-box-set! b 'bar)
    (test-eq 'bar (atomic-box-ref b))
    (test-eq 'bar (atomic-box-swap! b 'baz))
    (test-eq 'baz (atomic-box-ref b))
    (test-eq 'baz (atomic-box-compare-and-swap! b 'foo 'boo))
    (test-eq 'baz (atomic-box-ref b))
    (test-eq 'baz (atomic-box-compare-and-swap! b 'baz 'boo))
    (test-eq 'boo (atomic-box-ref b))
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
               (atomic-box-set! box #f)
               (atomic-fence (memory-order sequentially-consistent)))))
      (define threads (map (lambda (x) (make-thread
                                        (lambda ()
                                          (thread-sleep! 0.000001)
                                          (increment! x))))
                           (iota *num-test-threads*)))
      (for-each thread-start! threads)
      (for-each thread-join! threads)
      (test-eqv (apply + (iota *num-test-threads*)) counter)
      ))
   (else))
  )

;;;
;;; Atomic fxbox
;;;

(test-group "Atomic fbox"
  (let ()
    (define x (make-atomic-fxbox 0))
    (test-assert (atomic-fxbox? x))
    (test-eqv 0  (atomic-fxbox-ref x))
    (atomic-fxbox-set! x 1)
    (test-eqv 1  (atomic-fxbox-ref x))
    (test-eqv 1  (atomic-fxbox-swap! x 3))
    (test-eqv 3 (atomic-fxbox-ref x))
    (test-eqv 3 (atomic-fxbox-compare-and-swap! x 1 5))
    (test-eqv 3 (atomic-fxbox-ref x))
    (test-eqv 3 (atomic-fxbox-compare-and-swap! x 3 5))
    (test-eqv 5 (atomic-fxbox-ref x))

    (test-eqv 5 (atomic-fxbox+/fetch! x 3))
    (test-eqv 8 (atomic-fxbox-ref x))
    (test-eqv 8 (atomic-fxbox-/fetch! x 1))
    (test-eqv 7 (atomic-fxbox-ref x))

    (test-eqv 7 (atomic-fxbox-and/fetch! x #x55))
    (test-eqv 5 (atomic-fxbox-ref x))
    (test-eqv 5 (atomic-fxbox-ior/fetch! x #xaa))
    (test-eqv #xaf (atomic-fxbox-ref x))
    (test-eqv #xaf  (atomic-fxbox-xor/fetch! x #x15))
    (test-eqv #xba (atomic-fxbox-ref x))
    )

  (cond-expand
   ((library (srfi 18))
    (let ()
      (define counter (make-atomic-fxbox 0))
      (define (increment! delta)
        (atomic-fxbox-/fetch! counter delta
                              (memory-order sequentially-consistent)))
      (define threads (map (lambda (x) (make-thread
                                        (lambda ()
                                          (thread-sleep! 0.000001)
                                          (increment! x))))
                           (iota *num-test-threads*)))
      (for-each thread-start! threads)
      (for-each thread-join! threads)
      (test-eqv (- (apply + (iota *num-test-threads*)))
                (atomic-fxbox-ref counter))
      ))
   (else))
  )

;;;
;;; Atomic pair
;;;

(test-group "Atomic pair"
  (let ()
    (define x (make-atomic-pair 'p 'q))
    (test-assert (atomic-pair? x))
    (test-equal '(p q) (list/mv (atomic-pair-ref x)))
    (atomic-pair-set! x 'r 's)
    (test-equal '(r s) (list/mv (atomic-pair-ref x)))
    (test-equal '(r s) (list/mv (atomic-pair-swap! x 't 'u)))
    (test-equal '(t u) (list/mv (atomic-pair-ref x)))
    (test-equal '(t u) (list/mv (atomic-pair-compare-and-swap! x 'p 'q 'v 'w)))
    (test-equal '(t u) (list/mv (atomic-pair-ref x)))
    (test-equal '(t u) (list/mv (atomic-pair-compare-and-swap! x 't 'q 'v 'w)))
    (test-equal '(t u) (list/mv (atomic-pair-ref x)))
    (test-equal '(t u) (list/mv (atomic-pair-compare-and-swap! x 'p 'u 'v 'w)))
    (test-equal '(t u)  (list/mv (atomic-pair-ref x)))
    (test-equal '(t u) (list/mv (atomic-pair-compare-and-swap! x 't 'u 'v 'w)))
    (test-equal '(v w) (list/mv (atomic-pair-ref x)))
    )

  (cond-expand
   ((library (srfi 18))
    (let ()
      (define accumulator (make-atomic-pair 0 0))
      (define (accumulate! delta)
        (define (try curr-sum curr-val)
          (let-values (((prev-sum prev-val)
                        (atomic-pair-compare-and-swap! accumulator
                                                       curr-sum curr-val
                                                       (+ curr-sum curr-val) delta)))
            (unless (and (eq? prev-sum curr-sum)
                         (eq? prev-val curr-val))
              (try prev-sum prev-val))))
        (call-with-values (lambda () (atomic-pair-ref accumulator)) try))
      (define threads (map (lambda (x) (make-thread
                                        (lambda ()
                                          (thread-sleep! 0.001)
                                          (accumulate! x))))
                           (iota *num-test-threads*)))
      (for-each thread-start! threads)
      (for-each thread-join! threads)
      (test-eqv (apply + (iota *num-test-threads*))
                (apply + (list/mv (atomic-pair-ref accumulator))))
      ))
   (else))
  )

(test-end "SRFI 230 Atomic operations")
