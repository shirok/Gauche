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
    (dotimes (i n)
      (thread-start! (ref threads (- n i 1))))
    (thread-join! (ref threads (- n 1)))))
(test "thread-join!" 1346269 (lambda () (mt-fib 31)))

;; NB: the result of the following test is not guaranteed; theoretically,
;; there can be indefinite delay between thread-start! and the execution
;; of the thunk, so the execution of t1 may be delayed and the result can
;; be '(a b c). 
(test "thread-sleep!" '(b a c)
      (lambda ()
        (let* ((l '())
               (t0 (make-thread (lambda ()
                                  (thread-sleep! 0.1)
                                  (push! l 'a))))
               (t1 (make-thread (lambda ()
                                  (push! l 'b)
                                  (thread-sleep! 0.15)
                                  (push! l 'c)))))
          (thread-start! t0)
          (thread-start! t1)
          (thread-join! t0)
          (thread-join! t1)
          (reverse l))))

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

;; This test uses simple-minded spin lock, without using mutex timeouts
;; nor condition variables.   Not recommended way for real code.
(test "lock and unlock - blocking (simple spin-lock)" 
      '((put a) (get a) (put b) (get b) (put c) (get c))
      (lambda ()
        (let ((log '())
              (cell #f)
              (m (make-mutex)))
          (define (put! msg)
            (mutex-lock! m)
            (if cell
                (begin (mutex-unlock! m) (put! msg))
                (begin (set! cell msg)
                       (push! log `(put ,msg))
                       (mutex-unlock! m))))
          (define (get!)
            (mutex-lock! m)
            (if cell
                (let1 r cell
                  (set! cell #f)
                  (push! log `(get ,r))
                  (mutex-unlock! m)
                  r)
                (begin (mutex-unlock! m) (get!))))
          (define (producer)
            (put! 'a)
            (put! 'b)
            (put! 'c))
          (define (consumer)
            (get!)
            (get!)
            (get!))
          (let ((tp (thread-start! (make-thread producer 'producer)))
                (tc (thread-start! (make-thread consumer 'consumer))))
            (thread-join! tp)
            (thread-join! tc)
            (reverse log)))))

(test "lock with timeout"
      '(#t #f #f #f #f #t #t)
      (lambda ()
        (let ((m (make-mutex)))
          (let* ((r0 (mutex-lock! m))
                 (r1 (mutex-lock! m 0))
                 (r2 (mutex-lock! m 0.05))
                 (r3 (mutex-lock! m (seconds->time (+ (time->seconds (current-time)) 0.05))))
                 (r4 (mutex-lock! m (seconds->time (- (time->seconds (current-time)) 0.05))))
                 (r5 (mutex-unlock! m))
                 (r6 (mutex-lock! m 0)))
            (mutex-unlock! m)
            (list r0 r1 r2 r3 r4 r5 r6)))))

;; recursive mutex code taken from an example in SRFI-18
(test "recursive mutex"
      (list (current-thread) 0 'not-abandoned)
      (lambda ()
        (define (mutex-lock-recursively! mutex)
          (if (eq? (mutex-state mutex) (current-thread))
              (let ((n (mutex-specific mutex)))
                (mutex-specific-set! mutex (+ n 1)))
              (begin
                (mutex-lock! mutex)
                (mutex-specific-set! mutex 0))))
        (define (mutex-unlock-recursively! mutex)
          (let ((n (mutex-specific mutex)))
            (if (= n 0)
                (mutex-unlock! mutex)
                (mutex-specific-set! mutex (- n 1)))))
        (let1 m (make-mutex)
          (mutex-specific-set! m 0)
          (mutex-lock-recursively! m)
          (mutex-lock-recursively! m)
          (mutex-lock-recursively! m)
          (let1 r0 (mutex-state m)
            (mutex-unlock-recursively! m)
            (mutex-unlock-recursively! m)
            (let1 r1 (mutex-specific m)
              (mutex-unlock-recursively! m)
              (list r0 r1 (mutex-state m)))))
        ))

;;---------------------------------------------------------------------
(test-section "condition variables")

(test "make-condition-variable" #t
      (lambda ()
        (condition-variable? (make-condition-variable))))

(test "condition-varaible-name" 'foo
      (lambda ()
        (condition-variable-name (make-condition-variable 'foo))))

(test "condition-variable-specific" "hello"
      (lambda ()
        (let1 c (make-condition-variable 'foo)
          (condition-variable-specific-set! c "hello")
          (condition-variable-specific c))))

;; Producer-consumer model using condition variable.
(test "condition-variable-signal!"
      '((put a) (get a) (put b) (get b) (put c) (get c))
      (lambda ()
        (let ((log '())
              (cell #f)
              (m  (make-mutex))
              (put-cv (make-condition-variable))
              (get-cv (make-condition-variable)))
          (define (put! msg)
            (mutex-lock! m)
            (if cell
                (begin (mutex-unlock! m put-cv) (put! msg))
                (begin (set! cell msg)
                       (push! log `(put ,msg))
                       (condition-variable-signal! get-cv)
                       (mutex-unlock! m))))
          (define (get!)
            (mutex-lock! m)
            (if cell
                (let1 r cell
                  (set! cell #f)
                  (push! log `(get ,r))
                  (condition-variable-signal! put-cv)
                  (mutex-unlock! m)
                  r)
                (begin
                  (mutex-unlock! m get-cv) (get!))))
          (define (producer)
            (put! 'a)
            (put! 'b)
            (put! 'c))
          (define (consumer)
            (get!)
            (get!)
            (get!))
          (let ((tp (thread-start! (make-thread producer 'producer)))
                (tc (thread-start! (make-thread consumer 'consumer))))
            (thread-join! tp)
            (thread-join! tc)
            (reverse log)))))


(test-end)

