;;
;; Test for threads
;;

(use gauche.test)
(use gauche.sequence)
(add-load-path ".")

(test-start "threads")

(use gauche.threads)
(test-module 'gauche.threads)

(unless (eq? (gauche-thread-type) 'pthread)
  (format #t "thread not supported\n")
  (test-end)
  (exit 0))

;;---------------------------------------------------------------------
(test-section "basic thread API")

(test* "current-thread" #t
       (eq? (current-thread) (current-thread)))
(test* "thread?" '(#t #f)
       (list (thread? (current-thread))
             (thread? 'foo)))
(test* "make-thread" #t
       (thread? (make-thread (lambda () #f))))
(test* "thread-name" 'foo
       (thread-name (make-thread (lambda () #f) 'foo)))
(test* "thread-specific" "hello"
       (begin
         (thread-specific-set! (current-thread) "hello")
         (thread-specific (current-thread))))
(test* "thread-start!" "hello"
       (call-with-output-string
         (lambda (p)
           (let1 t
               (thread-start! (make-thread (lambda () (display "hello" p))))
             (thread-join! t)))))

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
(test* "thread-join!" 1346269 (mt-fib 31))

;; NB: the result of the following test is not guaranteed.
;; There can be indefinite delay between thread-start! and the execution
;; of the thunk, so the execution of t1 may be delayed and the result can
;; be '(a b c). 
'(test "thread-sleep!" '(b a c)
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

;; thread stop and cont
(let1 t1 (make-thread (lambda () (while #t (sys-nanosleep #e5e8))))
  (test* "thread-status" 'new (thread-state t1))
  (thread-start! t1)
  (test* "thread-status" 'runnable (thread-state t1))
  (thread-stop! t1)
  (test* "thread-status" 'stopped (thread-state t1))
  (thread-stop! t1) ; duplicate stop test
  (test* "thread-status" 'stopped (thread-state t1))
  (thread-cont! t1)
  (test* "thread-status" 'runnable (thread-state t1))
  (thread-terminate! t1)
  (test* "thread-status" 'terminated 
         (guard (e [(<terminated-thread-exception> e)
                    (thread-state t1)])
           (thread-join! t1))))

;;---------------------------------------------------------------------
(test-section "thread and error")

(test* "uncaught-exception" #t
       (let ((t (make-thread (lambda () (error "foo")))))
         (thread-start! t)
         (with-error-handler
             (lambda (e)
               (and (uncaught-exception? e)
                    (is-a? (uncaught-exception-reason e) <error>)))
           (lambda () (thread-join! t)))))

(test* "uncaught-exception" #t
       (let ((t (make-thread (lambda () (raise 4)))))
         (thread-start! t)
         (with-error-handler
             (lambda (e)
               (and (uncaught-exception? e)
                    (eqv? (uncaught-exception-reason e) 4)))
           (lambda () (thread-join! t)))))

(test* "uncaught-exception" #t
       (let ((t (make-thread (lambda ()
                               (with-error-handler
                                   (lambda (e) e)
                                 (lambda () (error "foo")))))))
         (thread-start! t)
         (with-error-handler
             (lambda (e) e)
           (lambda () (is-a? (thread-join! t) <error>)))))

;;---------------------------------------------------------------------
(test-section "basic mutex API")

(test* "make-mutex" #t (mutex? (make-mutex)))
(test* "mutex-name" 'foo (mutex-name (make-mutex 'foo)))

(test* "mutex-specific" "hoge"
       (let ((m (make-mutex 'bar)))
         (mutex-specific-set! m "hoge")
         (mutex-specific m)))

(test* "lock and unlock - no blocking" #t
       (let ((m (make-mutex)))
         (mutex-lock! m)
         (mutex-unlock! m)))

(test* "mutex-state"
       (list 'not-abandoned (current-thread) 'not-owned 'not-abandoned)
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
         (reverse r)))

;; This test uses simple-minded spin lock, without using mutex timeouts
;; nor condition variables.   Not recommended way for real code.
(test* "lock and unlock - blocking (simple spin-lock)" 
       '((put a) (get a) (put b) (get b) (put c) (get c))
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
           (reverse log))))

(test* "lock with timeout"
      '(#t #f #f #f #f #t #t)
      (let ((m (make-mutex)))
        (let* ((r0 (mutex-lock! m))
               (r1 (mutex-lock! m 0))
               (r2 (mutex-lock! m 0.05))
               (r3 (mutex-lock! m (seconds->time (+ (time->seconds (current-time)) 0.05))))
               (r4 (mutex-lock! m (seconds->time (- (time->seconds (current-time)) 0.05))))
               (r5 (mutex-unlock! m))
               (r6 (mutex-lock! m 0)))
          (mutex-unlock! m)
          (list r0 r1 r2 r3 r4 r5 r6))))

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

(test* "make-condition-variable" #t
       (condition-variable? (make-condition-variable)))

(test* "condition-varaible-name" 'foo
       (condition-variable-name (make-condition-variable 'foo)))

(test* "condition-variable-specific" "hello"
       (let1 c (make-condition-variable 'foo)
         (condition-variable-specific-set! c "hello")
         (condition-variable-specific c)))

;; Producer-consumer model using condition variable.
(test* "condition-variable-signal!"
       '((put a) (get a) (put b) (get b) (put c) (get c))
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
           (reverse log))))

;;---------------------------------------------------------------------
(test-section "port access serialization")

(use srfi-1)

(define (port-test-chunk-generator nchars c)
  (lambda () (make-string nchars c)))

(define (port-test-read-string nchars port)
  (let loop ((i 1) (c (read-char port)) (r '()))
    (cond ((eof-object? c)
           (if (null? r) c (list->string (reverse r))))
          ((= i nchars) (list->string (reverse (cons c r))))
          (else (loop (+ i 1) (read-char port) (cons c r))))))

(define (port-test-testers nchars nthread nrepeat line?)
  (let* ((strgen     (map (lambda (i)
                            (port-test-chunk-generator nchars
                                                       (integer->char
                                                        (+ (char->integer #\a)
                                                           i))))
                          (iota nthread)))
         (generators (map (lambda (gen)
                            (let ((i 0))
                              (lambda ()
                                (if (= i nrepeat)
                                    #f
                                    (begin
                                      (inc! i)
                                      (if line?
                                          (string-append (gen) "\n")
                                          (gen)))))))
                          strgen))
         (getter     (if line?
                         read-line
                         (lambda (port)
                           (port-test-read-string nchars port))))
         (confirmer  (lambda (inp)
                       (let1 strs (map (cut <>) strgen)
                         (let loop ((chunk (getter inp)))
                           (cond ((eof-object? chunk) #t)
                                 ((member chunk strs) (loop (getter inp)))
                                 (else #f))))))
         )
    (values confirmer generators)))

(define (port-test-kick-threads generators outp)
  (let* ((thunks  (map (lambda (gen)
                         (lambda ()
                           (let loop ((s (gen)))
                             (when s
                               (display s outp)
                               (thread-sleep! 0.001)
                               (loop (gen))))))
                       generators))
         (threads (map (lambda (thunk) (make-thread thunk)) thunks))
         )
    (for-each thread-start! threads)
    (for-each thread-join! threads)))

(sys-system "rm -rf test.out")

(test* "write to file, buffered" #t
       (receive (confirmer generators)
           (port-test-testers 160 8 20 #f)
         (call-with-output-file "test.out"
           (lambda (outp) (port-test-kick-threads generators outp)))
         (call-with-input-file "test.out" confirmer)))

(sys-system "rm -rf test.out")

(test* "write to file, line-buffered" #t
       (receive (confirmer generators)
           (port-test-testers 160 8 20 #t)
         (call-with-output-file "test.out"
           (lambda (outp) (port-test-kick-threads generators outp))
           :buffering :line)
         (call-with-input-file "test.out" confirmer)))


(sys-system "rm -rf test.out")

(test* "write to string" #t
       (receive (confirmer generators)
           (port-test-testers 160 8 20 #f)
         (let1 s (call-with-output-string
                   (lambda (outp) (port-test-kick-threads generators outp)))
           (call-with-input-string s confirmer))))

;; Check if port is properly unlocked when an error is signalled
;; inside the port processing routine.

(define *port-test-error* #f)

(define (make-error-test-port outp flush?)
  (open-output-buffered-port
   (lambda (str)
     (cond ((not str) (flush outp))
           ((string-scan str "Z" 'before)
            => (lambda (s)
                 (display s outp)
                 (if flush? (flush outp))
                 (unless *port-test-error*
                   (set! *port-test-error* #t)
                   (error "error"))))
           (else (display str outp) (if flush? (flush outp)))))
   5))

(define (port-test-on-error port use-flush?)
  (set! *port-test-error* #f)
  (let* ((p   (make-error-test-port port use-flush?))
         (th1 (make-thread
               (lambda ()
                 (with-error-handler
                     (lambda (e) #f)
                   (lambda () (display "aaaaaAAAZAa" p))))
               'th1))
         (th2 (make-thread
               (lambda ()
                 (display "bbbbbbbb" p))
               'th2)))
    (thread-start! th1)
    (thread-join! th1)
    (thread-start! th2)
    (thread-join! th2)
    (close-output-port p)))

(test* "check if port is unlocked on error" "aaaaaAAAAAAbbbbbbbb"
       (call-with-output-string (cut port-test-on-error <> #f)))
(test* "check if port is unlocked on error" "aaaaaAAAAAAbbbbbbbb"
       (call-with-output-string (cut port-test-on-error <> #t)))

(sys-system "rm -f test.out")
(test* "check if port is unlocked on error (use file)" "aaaaaAAAAAAbbbbbbbb"
       (begin
         (call-with-output-file "test.out"
           (cut port-test-on-error <> #f))
         (call-with-input-file "test.out" port->string)))

(sys-system "rm -f test.out")
(test* "check if port is unlocked on error (use file)" "aaaaaAAAAAAbbbbbbbb"
       (begin
         (call-with-output-file "test.out"
           (cut port-test-on-error <> #t))
         (call-with-input-file "test.out" port->string)))

;;---------------------------------------------------------------------
;(test-section "thread and signal")

;(test "catching signal by primordial thread" (make-list 10 'int)

;;---------------------------------------------------------------------
(test-section "thread-local parameters")

(use gauche.parameter)

(define *thr1-val* #f)
(define *thr2-val* #f)

(define p (make-parameter 3))

(test* "check locality of parameters" '(3 4 5)
       (let ((th1 (make-thread
                   (lambda ()
                     (p 4)
                     (set! *thr1-val* (p)))))
             (th2 (make-thread
                   (lambda ()
                     (p 5)
                     (set! *thr2-val* (p))))))
         (thread-start! th1)
         (thread-start! th2)
         (thread-join! th1)
         (thread-join! th2)
         (list (p) *thr1-val* *thr2-val*)))

(test* "check parameter identification"
       (test-error)
       (let* ((local #f))
         (thread-join!
          (thread-start!
           (make-thread
            (lambda ()
              (set! local (make-parameter 1))))))
         (thread-join!
          (thread-start!
           (make-thread
            (lambda ()
              (local)))))))

;;---------------------------------------------------------------------
(test-section "atoms")

(test* "atom" #t (atom? (atom 0 1 2)))

(test* "atomic counting" 1000
       (let ([a (atom 0)] [ts '()])
         (dotimes [n 100]
           (push! ts
                  (thread-start! (make-thread 
                                  (^()
                                    (dotimes [m 10]
                                      (atomic-update! a (pa$ + 1))))))))
         (for-each thread-join! ts)
         (atom-ref a)))

;;---------------------------------------------------------------------
(test-section "synchrnization by queues")

;; These are actually for testing mtqueue, but put here since they
;; require threads to work.

(use util.queue)

(define (test-producer-consumer name qs ndata nthreads)
  (define qr (make-mtqueue))
  (define data (iota ndata))
  (define (producer)
    (dolist [n data] (enqueue/wait! qs n))
    (dotimes [k nthreads] (enqueue/wait! qs #f)))
  (define (consumer)
    (let loop ([x (dequeue/wait! qs)])
      (when x
        (enqueue! qr x)
        (sys-nanosleep #e1e7)
        (loop (dequeue/wait! qs)))))

  (test* #`"synchronized queue ,name" data
         (let* ([cs (map (^_ (thread-start! (make-thread consumer)))
                         (iota nthreads))]
                [p1 (make-thread producer)])
           (sys-nanosleep #e1e8)
           (thread-start! p1)
           (for-each thread-join! cs)
           (thread-join! p1)
           (queue->list qr))
         (cut lset= eqv? <> <>)))

(test-producer-consumer "(unbound queue length)"
                        (make-mtqueue)
                        100 3)

(test-producer-consumer "(bound queue length)"
                        (make-mtqueue :max-length 5)
                        100 3)


(test* "dequeue/wait! timeout" "timed out!"
       (dequeue/wait! (make-mtqueue) 0.01 "timed out!"))
(test* "enqueue/wait! timeout" "timed out!"
       (let1 q (make-mtqueue :max-length 1)
         (enqueue! q 'a)
         (enqueue/wait! q 'b 0.01 "timed out!")))
(test* "queue-push/wait! timeout" "timed out!"
       (let1 q (make-mtqueue :max-length 1)
         (enqueue! q 'a)
         (queue-push/wait! q 'b 0.01 "timed out!")))

(test-end)

