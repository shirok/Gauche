/*
 * Test for threads
 */

(use gauche.test)
(use gauche.sequence)

(test-start "thread")

(unless (eq? (gauche-thread-type) 'pthread)
  (format #t "thread not supported")
  (test-end))

;; calculate fibonacchi in awful way
(define (mt-fib n)
  (let ((threads (make-vector n)))
    (dotimes (i n)
      (set! (ref threads i)
            (make-thread
             (case i
               ((0) (lambda () 1))
               ((1) (lambda () 1))
               (else (lambda () (+ (thread-join! (ref threads (- i 1)))
                                   (thread-join! (ref threads (- i 2)))))))
             i)))
    ;(print threads)
    (dotimes (i n)
      (thread-start! (ref threads (- n i 1))))
    ;(print threads)
    (thread-join! (ref threads (- n 1)))))

(test-end)

