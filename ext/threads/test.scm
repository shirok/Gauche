;;
;; Test for threads
;;

(use gauche.test)
(use gauche.sequence)
(use gauche.time)
(use srfi-19)
(add-load-path ".")

(test-start "threads")

(use gauche.threads)
(test-module 'gauche.threads)

(cond-expand
 [(not gauche.sys.threads)
  (format #t "thread not supported\n")
  (test-end)
  (exit 0)]
 [else])

;;---------------------------------------------------------------------
(test-section "basic thread API")

(test* "current-thread" #t
       (eq? (current-thread) (current-thread)))
(test* "thread?" '(#t #f)
       (list (thread? (current-thread))
             (thread? 'foo)))
(test* "make-thread" #t
       (thread? (make-thread (^[] #f))))
(test* "thread-name" 'foo
       (thread-name (make-thread (^[] #f) 'foo)))
(test* "thread-specific" "hello"
       (begin
         (thread-specific-set! (current-thread) "hello")
         (thread-specific (current-thread))))
(test* "thread-start!" "hello"
       (call-with-output-string
         (^p (let1 t (thread-start! (make-thread (^[] (display "hello" p))))
               (thread-join! t)))))

;; calculate fibonacchi in awful way
(define (mt-fib n)
  (let1 threads (make-vector n)
    (dotimes [i n]
      (set! (ref threads i)
            (make-thread
             (case i
               [(0) (^[] 1)]
               [(1) (^[] 1)]
               [else (^[] (+ (thread-join! (ref threads (- i 1)))
                             (thread-join! (ref threads (- i 2)))))])
             i)))
    (dotimes [i n]
      (thread-start! (ref threads (- n i 1))))
    (thread-join! (ref threads (- n 1)))))
(test* "thread-join!" 1346269 (mt-fib 31))

(let ()
  (define (thread-sleep-test x)
    (test* (format "thread-sleep! ~s" x) #t
           (let1 c (make <real-time-counter>)
             (with-time-counter c (thread-sleep! x))
             (> (time-counter-value c)
                (- (if (is-a? x <time>)
                     (time->seconds (time-difference (current-time) x))
                     x)
                   0.01)))))

  (thread-sleep-test 0.02)
  (thread-sleep-test 1/50)
  (thread-sleep-test (add-duration (current-time)
                                   (make-time time-duration #e2e7 0)))
  )

;; thread stop and cont
(let1 t1 (make-thread (^[] (while #t (sys-nanosleep #e5e8))))
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

;; thread termination via stopRequest
(let1 t1 (thread-start! (make-thread (^[] (let loop () (loop))))) ; busy loop
  (test* "thread termination via stopRequest" 'terminated
         (guard (e [(<terminated-thread-exception> e)
                    (thread-state t1)])
           (thread-terminate! t1)
           (thread-join! t1))))

;; this SEGVs on 0.9.3.3. test code from @cryks.
(test* "thread termination before running" 'terminated
       (let1 t1 (make-thread (^[] #f))
         (thread-terminate! t1)
         (thread-state t1)))

(test* "thread termination while being stopped" 'terminated
       (let1 t1 (thread-start! (make-thread (^[] (let loop () (loop)))))
         (thread-stop! t1)
         (thread-terminate! t1)
         (thread-state t1)))

;;---------------------------------------------------------------------
(test-section "thread and error")

(test* "uncaught-exception" #t
       (let1 t (make-thread (^[] (error "foo")))
         (thread-start! t)
         (with-error-handler
             (^e (and (uncaught-exception? e)
                      (is-a? (uncaught-exception-reason e) <error>)))
           (^[] (thread-join! t)))))

(test* "uncaught-exception" #t
       (let1 t (make-thread (^[] (raise 4)))
         (thread-start! t)
         (with-error-handler
             (^e (and (uncaught-exception? e)
                      (eqv? (uncaught-exception-reason e) 4)))
           (^[] (thread-join! t)))))

(test* "uncaught-exception" #t
       (let1 t (make-thread (^[] (with-error-handler identity
                                   (^[] (error "foo")))))
         (thread-start! t)
         (with-error-handler identity
           (^[] (is-a? (thread-join! t) <error>)))))

;;---------------------------------------------------------------------
(test-section "basic mutex API")

(test* "make-mutex" #t (mutex? (make-mutex)))
(test* "mutex-name" 'foo (mutex-name (make-mutex 'foo)))

(test* "mutex-specific" "hoge"
       (let1 m (make-mutex 'bar)
         (mutex-specific-set! m "hoge")
         (mutex-specific m)))

(test* "lock and unlock - no blocking" #t
       (let1 m (make-mutex)
         (mutex-lock! m)
         (mutex-unlock! m)))

(test* "mutex-state"
       (list 'not-abandoned (current-thread) 'not-owned 'not-abandoned)
       (let ([m (make-mutex)]
             [r '()])
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
       (let ([log '()]
             [cell #f]
             [m (make-mutex)])
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
             (rlet1 r cell
               (set! cell #f)
               (push! log `(get ,r))
               (mutex-unlock! m))
             (begin (mutex-unlock! m) (get!))))
         (define (producer)
           (put! 'a)
           (put! 'b)
           (put! 'c))
         (define (consumer)
           (get!)
           (get!)
           (get!))
         (let ([tp (thread-start! (make-thread producer 'producer))]
               [tc (thread-start! (make-thread consumer 'consumer))])
           (thread-join! tp)
           (thread-join! tc)
           (reverse log))))

(test* "lock with timeout"
      '(#t #f #f #f #f #t #t)
      (let1 m (make-mutex)
        (let* ([r0 (mutex-lock! m)]
               [r1 (mutex-lock! m 0)]
               [r2 (mutex-lock! m 0.05)]
               [r3 (mutex-lock! m (seconds->time (+ (time->seconds (current-time)) 0.05)))]
               [r4 (mutex-lock! m (seconds->time (- (time->seconds (current-time)) 0.05)))]
               [r5 (mutex-unlock! m)]
               [r6 (mutex-lock! m 0)])
          (mutex-unlock! m)
          (list r0 r1 r2 r3 r4 r5 r6))))

;; recursive mutex code taken from an example in SRFI-18
(test "recursive mutex"
      (list (current-thread) 0 'not-abandoned)
      (^[]
        (define (mutex-lock-recursively! mutex)
          (if (eq? (mutex-state mutex) (current-thread))
            (let1 n (mutex-specific mutex)
              (mutex-specific-set! mutex (+ n 1)))
            (begin
              (mutex-lock! mutex)
              (mutex-specific-set! mutex 0))))
        (define (mutex-unlock-recursively! mutex)
          (let1 n (mutex-specific mutex)
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
       (let ([log '()]
             [cell #f]
             [m  (make-mutex)]
             [put-cv (make-condition-variable)]
             [get-cv (make-condition-variable)])
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
             (rlet1 r cell
               (set! cell #f)
               (push! log `(get ,r))
               (condition-variable-signal! put-cv)
               (mutex-unlock! m))
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
         (let ([tp (thread-start! (make-thread producer 'producer))]
               [tc (thread-start! (make-thread consumer 'consumer))])
           (thread-join! tp)
           (thread-join! tc)
           (reverse log))))

;;---------------------------------------------------------------------
(test-section "port access serialization")

(use srfi-1)

(define (port-test-chunk-generator nchars c)
  (^[] (make-string nchars c)))

(define (port-test-read-string nchars port)
  (let loop ([i 1] [c (read-char port)] [r '()])
    (cond [(eof-object? c)
           (if (null? r) c (list->string (reverse r)))]
          [(= i nchars) (list->string (reverse (cons c r)))]
          [else (loop (+ i 1) (read-char port) (cons c r))])))

(define (port-test-testers nchars nthread nrepeat line?)
  (let* ([strgen (map (^i ($ port-test-chunk-generator nchars
                             $ integer->char (+ (char->integer #\a) i)))
                      (iota nthread))]
         [generators (map (^[gen]
                            (let ((i 0))
                              (^[] (if (= i nrepeat)
                                     #f
                                     (begin
                                       (inc! i)
                                       (if line?
                                         (string-append (gen) "\n")
                                         (gen)))))))
                          strgen)]
         [getter (if line?
                   read-line
                   (cut port-test-read-string nchars <>))]
         [confirmer  (^[inp]
                       (let1 strs (map (cut <>) strgen)
                         (let loop ([chunk (getter inp)])
                           (cond [(eof-object? chunk) #t]
                                 [(member chunk strs) (loop (getter inp))]
                                 [else #f]))))])
    (values confirmer generators)))

(define (port-test-kick-threads generators outp)
  (let* ([thunks  (map (^[gen] (^[] (let loop ([s (gen)])
                                      (when s
                                        (display s outp)
                                        (thread-sleep! 0.001)
                                        (loop (gen))))))
                       generators)]
         [threads (map make-thread thunks)])
    (for-each thread-start! threads)
    (for-each thread-join! threads)))

(sys-system "rm -rf test.out")

(test* "write to file, buffered" #t
       (receive (confirmer generators)
           (port-test-testers 160 8 20 #f)
         (call-with-output-file "test.out"
           (^[outp] (port-test-kick-threads generators outp)))
         (call-with-input-file "test.out" confirmer)))

(sys-system "rm -rf test.out")

(test* "write to file, line-buffered" #t
       (receive (confirmer generators)
           (port-test-testers 160 8 20 #t)
         (call-with-output-file "test.out"
           (^[outp] (port-test-kick-threads generators outp))
           :buffering :line)
         (call-with-input-file "test.out" confirmer)))


(sys-system "rm -rf test.out")

(test* "write to string" #t
       (receive (confirmer generators)
           (port-test-testers 160 8 20 #f)
         (let1 s (call-with-output-string
                   (^[outp] (port-test-kick-threads generators outp)))
           (call-with-input-string s confirmer))))

;; Check if port is properly unlocked when an error is signalled
;; inside the port processing routine.

(define *port-test-error* #f)

(define (make-error-test-port outp flush?)
  (open-output-buffered-port
   (^[str]
     (cond [(not str) (flush outp)]
           [(string-scan str "Z" 'before)
            => (^s (display s outp)
                   (if flush? (flush outp))
                   (unless *port-test-error*
                     (set! *port-test-error* #t)
                     (error "error")))]
           [else (display str outp) (if flush? (flush outp))]))
   5))

(define (port-test-on-error port use-flush?)
  (set! *port-test-error* #f)
  (let* ([p   (make-error-test-port port use-flush?)]
         [th1 (make-thread
               (^[] (with-error-handler (^e #f)
                      (^[] (display "aaaaaAAAZAa" p))))
               'th1)]
         [th2 (make-thread
               (^[] (display "bbbbbbbb" p))
               'th2)])
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
       (let ([th1 (make-thread (^[] (p 4) (set! *thr1-val* (p))))]
             [th2 (make-thread (^[] (p 5) (set! *thr2-val* (p))))])
         (thread-start! th1)
         (thread-start! th2)
         (thread-join! th1)
         (thread-join! th2)
         (list (p) *thr1-val* *thr2-val*)))

;; Parameters that are created in a different thread
;; We used to prohibit accessing parameters that are created by different
;; threads (up to 0.9.2).  But it caused annoyance when a module creating
;; parameters is loaded after some threads are already started.

(test* "parameters created by different thread" '(2 1 3)
       (let ([p #f]
             [handshake #f]
             [val1 #f]
             [val2 #f]
             [val3 #f])
         (let ([th1 (make-thread (^[]
                                   (set! p (make-parameter 1))
                                   (p 2)
                                   (until handshake (sys-nanosleep 10000))
                                   (set! val1 (p))))]
               [th2 (make-thread (^[]
                                   (until p (sys-nanosleep 10000))
                                   (set! val2 (p))))]
               [th3 (make-thread (^[]
                                   (until p (sys-nanosleep 10000))
                                   (p 3)
                                   (set! handshake #t)
                                   (set! val3 (p))))])
           (thread-start! th1)
           (thread-start! th2)
           (thread-start! th3)
           (thread-join! th1)
           (thread-join! th2)
           (thread-join! th3)
           (list val1 val2 val3))))

;;---------------------------------------------------------------------
(test-section "atoms")

(test* "atom" #t (atom? (atom 0 1 2)))

(test* "atomic counting" 300
       (let ([a (atom 0)] [ts '()])
         (dotimes [n 30]
           (push! ts
                  (thread-start! (make-thread
                                  (^[] (dotimes [m 10]
                                         (atomic-update! a (pa$ + 1))))))))
         (for-each thread-join! ts)
         (atom-ref a)))

;;---------------------------------------------------------------------
(test-section "threads and promise")

(use srfi-1)

(letrec ([x 0]
         [z (delay (begin (sys-nanosleep 100000) (inc! x) 'ok))])
  (test* "concurrent forcing" 1
         (let ([ts (map (^_ (make-thread (^[] (force z)))) (iota 10))])
           (for-each thread-start! ts)
           (for-each thread-join! ts)
           x)))

(letrec ([count 0]
         [x 5]
         [z (delay (begin (set! count (+ count 1))
                          (if (> count x)
                            count
                            (force p))))])
  (test* "concurrent forcing w/ recursive force" 1
         (let ([ts (map (^_ (make-thread (^[] (force z)))) (iota 10))])
           (for-each thread-start! ts)
           (for-each thread-join! ts)
           count)))

;;---------------------------------------------------------------------
(test-section "threads and lazy sequences")

(let1 k 0
  (define (weird-generator)
    (inc! k)
    (case k
      [(1) 0]
      [(2) (sys-nanosleep #e1e7) (raise "boo")]
      [(3) 1]
      [else (eof-object)]))
  (define (task seq)
    (guard (e [else e])
      (let1 k (length seq) seq)))
  (let* ([seq (generator->lseq weird-generator)]
         [t0  (make-thread (^[] (task seq)))]
         [t1  (make-thread (^[] (task seq)))])
    (test* "accessing the same lazy sequence"
           (test-one-of '("boo" (0 1)) '((0 1) "boo"))
           (begin (thread-start! t0)
                  (thread-start! t1)
                  (list (thread-join! t0) (thread-join! t1))))))

;;---------------------------------------------------------------------
(test-section "synchrnization by queues")

;; These are actually for testing mtqueue, but put here since they
;; require threads to work.

(use data.queue)

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

  (test* #"synchronized queue ~name" data
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

(test-producer-consumer "(zero-length queue)"
                        (make-mtqueue :max-length 0)
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

(test* "zero-length-queue handshaking" '(5 4 3 2 1 0)
       (let ([r '()]
             [q0 (make-mtqueue :max-length 0)]
             [q1 (make-mtqueue :max-length 0)])
         (let1 t (thread-start!
                  (make-thread (^[]
                                 (push! r 0)
                                 (dequeue/wait! q0)
                                 (enqueue/wait! q1 'b)
                                 (push! r 2)
                                 (dequeue/wait! q0)
                                 (enqueue/wait! q1 'd)
                                 (push! r 4)
                                 (dequeue/wait! q0))))
           (enqueue/wait! q0 'a)
           (push! r 1)
           (dequeue/wait! q1)
           (enqueue/wait! q0 'c)
           (push! r 3)
           (dequeue/wait! q1)
           (enqueue/wait! q0 'e)
           (push! r 5))
         r))

(test* "zero-length-queue multiple reader" '(a a b b)
       (let ([r0 #f] [r1 #f]
             [q  (make-mtqueue :max-length 0)]
             [qq (make-mtqueue :max-length 0)])
         (let ([t0 (thread-start!
                    (make-thread (^[] (enqueue/wait! qq #t)
                                      (set! r0 (dequeue/wait! q))
                                      (enqueue/wait! qq 'b))))]
               [t1 (thread-start!
                    (make-thread (^[] (enqueue/wait! qq #t)
                                      (set! r1 (dequeue/wait! q))
                                      (enqueue/wait! qq 'b))))])
           (dequeue/wait! qq)
           (dequeue/wait! qq)
           (enqueue/wait! q 'a)
           (enqueue/wait! q 'a)
           (let1 r (list (dequeue/wait! qq) (dequeue/wait! qq))
             (list* r0 r1 r)))))

(test-end)

