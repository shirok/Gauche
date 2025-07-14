;;
;; Test thread-terminate! while the target thread is busy-looping.
;;
;; This must be run with test-extra

(use gauche.test)
(use gauche.threads)

(test-start "thread termination edge cases")

(cond-expand
 [gauche.sys.pthreads
  ;; See src/libextra.scm for busy-loop and get-busy-loop-counter
  (define t (thread-start! (make-thread (^[] (busy-loop 100_0000_0000)))))
  (define (running?)
    (let1 a (get-busy-loop-counter)
      (sys-nanosleep 1_000_000)
      (or (not (= a (get-busy-loop-counter)))
          (begin (sys-nanosleep 1_000_000)
                 (not (= a (get-busy-loop-counter)))))))
  (test* "thread-termiate! to busy loop (precondition)" #t
         (running?))
  (test* "thread-termiate! to busy loop (termination)" #f
         (begin (thread-terminate! t) (running?)))
  (test* "thread-termiate! to busy loop (join)"
         (test-error <terminated-thread-exception>)
         (thread-join! t))
  ]
 [else])

(test-end)
