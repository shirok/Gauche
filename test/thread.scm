;;
;; Test for threads
;;

(use gauche.test)
(use gauche.sequence)

(test-start "thread")

(unless (eq? (gauche-thread-type) 'pthread)
  (format #t "thread not supported\n")
  (test-end)
  (exit 0))

;;---------------------------------------------------------------------
(test-section "basic thread API")

(test "current-thread" #t
      (lambda () (eq? (current-thread) (current-thread)) ))
(test "thread?" '(#t #f)
      (lambda ()
        (list (thread? (current-thread))
              (thread? 'foo))))
(test "make-thread" #t
      (lambda ()
        (thread? (make-thread (lambda () #f)))))
(test "thread-name" 'foo
      (lambda ()
        (thread-name (make-thread (lambda () #f) 'foo))))
(test "thread-specific" "hello"
      (lambda ()
        (thread-specific-set! (current-thread) "hello")
        (thread-specific (current-thread))))
(test "thread-start!" "hello"
      (lambda ()
        (call-with-output-string
          (lambda (p)
            (let1 t (thread-start! (make-thread (lambda () (display "hello" p))))
              (thread-join! t))))))

;;---------------------------------------------------------------------
(test-section "basic mutex API")

(test "make-mutex" #t
      (lambda () (mutex? (make-mutex))))

(test "mutex-name" 'foo
      (lambda () (mutex-name (make-mutex 'foo))))

(test "mutex-specific" "hoge"
      (lambda ()
        (let ((m (make-mutex 'bar)))
          (mutex-specific-set! m "hoge")
          (mutex-specific m))))

(test "lock and unlock - no blocking" #t
      (lambda ()
        (let ((m (make-mutex)))
          (mutex-lock! m)
          (mutex-unlock! m))))

(test "mutex-state" (list 'not-abandoned (current-thread) 'not-owned 'not-abandoned)
      (lambda ()
        (let ((m (make-mutex))
              (r '()))
          (push! r (mutex-state m))
          (mutex-lock! m)
          (push! r (mutex-state m))
          (mutex-unlock! m)
          (mutex-lock! m #f #f)
          (push! r (mutex-state m))
          (mutex-unlock! m)
          (push! r (mutex-state m))
          (reverse r))))

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

