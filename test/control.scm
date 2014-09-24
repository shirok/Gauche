;; Tests for control.* modules

(use gauche.test)
(use srfi-1)
(use srfi-19)
(use data.queue)

(test-start "control")

;;--------------------------------------------------------------------
;; control.job
;;
(test-section "control.job")
(use control.job)
(test-module 'control.job)

(let ((j #f)
      (z #f))
  (test* "make-job" #t
         (begin (set! j (make-job (^[] #f)))
                (job? j)))

  (dolist [when `((:acknowledge ,job-acknowledge-time)
                  (:start       ,job-start-time)
                  (:finish      ,job-finish-time))]
    (test* (format "job-touch! ~a (pre)" (car when)) #f
           ((cadr when) j))
    (test* (format "job-touch! ~a" (car when)) #t
           (let1 time (job-touch! j (car when))
             (time=? time ((cadr when) j)))))
    
  (set! j (make-job (^[] (set! z #t) 'ok)))
  (test* "job-run! precondition" '(#f #f) (list (job-status j) z))
  (test* "job-run! postcondition" '(done ok #t)
         (begin (job-run! j)
                (list (job-status j) (job-result j) z)))

  (set! j (make-job (^[] (raise 'gosh) 'ok)))
  (test* "job-run! error condition" '(error gosh)
         (begin (job-run! j)
                (list (job-status j) (job-result j))))

  (set! j (make-job (^[] #f) :cancellable #t))
  (test* "job-run! killed" '(killed test)
         (begin (job-mark-killed! j 'test)
                (list (job-status j) (job-result j))))
  )

(cond-expand
 [gauche.sys.pthreads
  (test* "job-wait, job-kill" '(killed foo)
         (let* ([gate (make-mtqueue :max-length 0)]
                [job (make-job (^[] (enqueue/wait! gate #t) (sys-sleep 10))
                               :waitable #t)]
                [t1 (thread-start! (make-thread (^[] (job-run! job))))]
                [t2 (thread-start! (make-thread (^[] (job-wait job))))])
           (dequeue/wait! gate)
           (job-mark-killed! job 'foo)
           (list (thread-join! t2) (job-result job))))

  (test* "job-wait and error" '(error "bang")
         (let* ([job (make-job (^[] (error "bang")) :waitable #t)]
                [t1 (thread-start! (make-thread (^[] (job-wait job))))])
           (job-run! job)
           (list (thread-join! t1) (and (<error> (job-result job))
                                        (~ (job-result job)'message)))))
  ]
 [else])

;;--------------------------------------------------------------------
;; control.thread-pool
;;

(cond-expand
 [gauche.sys.threads
  (test-section "control.thread-pool")
  (use control.thread-pool)
  (test-module 'control.thread-pool)

  (let ([pool (make-thread-pool 5)]
        [rvec (make-vector 10 #f)])
    (test* "pool" '(5 #t 5 #f)
           (list (length (~ pool'pool))
                 (every thread? (~ pool'pool))
                 (~ pool'size)
                 (~ pool'max-backlog)))

    (test* "doit" '#(0 1 2 3 4 5 6 7 8 9)
           (begin (dotimes [k 10]
                    (add-job! pool (^[]
                                     (sys-nanosleep 1e7)
                                     (vector-set! rvec k k))))
                  (and (wait-all pool #f 1e7) rvec)))

    (test* "error results" '(ng ng ng ng ng)
           (begin (dotimes [k 5]
                    (add-job! pool (^[]
                                     (sys-nanosleep 1e7)
                                     (raise 'ng)
                                     (vector-set! rvec k k))
                              #t))
                  (and (wait-all pool #f 1e7)
                       (map (cut job-result <>)
                            (queue->list (~ pool'result-queue))))))
    )

  ;; Testing max backlog and timeout
  (let ([pool (make-thread-pool 1 :max-backlog 1)]
        [gate #f])
    (define (work) (do [] [gate] (sys-nanosleep #e1e8)))

    (add-job! pool work)

    (test* "add-job! backlog" #t
           (job? (add-job! pool work)))

    (test* "add-job! timeout" #f
           (add-job! pool work #f 0.1))

    (set! gate #t)
    (test* "add-job! backlog" #t
           (job? (add-job! pool work #t)))

    ;; synchronize
    (dequeue/wait! (thread-pool-results pool))

    (set! gate #f)
    (test* "wait-all timeout" #f
           (begin (add-job! pool work)
                  (wait-all pool 0.1 #e1e7)))

    ;; fill the queue.  
    (add-job! pool work #t)
    
    (test* "shutdown - raising <thread-pool-shutting-down>"
           (test-error <thread-pool-shut-down>)
           ;; subtle arrangement: The timing of thread execution isn't
           ;; guaranteed, but we hope to run the following events in
           ;; sequence:
           ;;  - main: add-job!  - this will block because queue is full
           ;;  - t1: enqueue/wait! triggers q1.
           ;;  - t1: terminate-all! - this causes the pending add-job! to
           ;;                raise an exception.  this call blocks.
           ;;  - main: add-job! raises an exception, and triggers q2.
           ;;  - t2: triggered by q1, this sets gate to #t, causing the
           ;;                suspended jobs to resume.
           ;;  - t1: terminate-all! returns once all the suspended jobs
           ;;                are finished.  making sure q2 is already triggered,
           ;;                we set gate to 'finished for the later check.
           (let* ([q1 (make-mtqueue :max-length 0)]
                  [q2 (make-mtqueue :max-length 0)]
                  [t1 (make-thread (^[]
                                     (enqueue/wait! q1 #t)
                                     (terminate-all! pool
                                                     :cancel-queued-jobs #t)
                                     (dequeue/wait! q2)
                                     (set! gate 'finished)))]
                  [t2 (make-thread (^[]
                                     (dequeue/wait! q1)
                                     (sys-nanosleep #e2e8)
                                     (set! gate #t)))])
             (thread-start! t1)
             (thread-start! t2)
             (unwind-protect (add-job! pool work)
               (enqueue/wait! q2 #t))))

    (test* "shutdown - killing a job in the queue"
           'killed
           (job-status (dequeue/wait! (thread-pool-results pool))))
    (test* "shutdown check" 'finished
           (let retry ([n 0])
             (cond [(= n 10) #f]
                   [(symbol? gate) gate]
                   [else (sys-nanosleep #e1e8) (retry (+ n 1))])))
    )

  ;; now, test forcible termination
  (let ([pool (make-thread-pool 1)]
        [gate #f])
    (define (work) (do [] [gate] (sys-nanosleep #e1e8)))
    (test* "forced shutdown" 'killed
           (let1 xjob (add-job! pool work)
             (terminate-all! pool :force-timeout 0.05)
             (job-status xjob))))

  ;; This SEGVs on 0.9.3.3 (test code by @cryks)
  (test* "thread pool termination" 'terminated
         (let ([t (thread-start! (make-thread (cut undefined)))]
               [pool (make-thread-pool 10)])
           (terminate-all! pool)
           (thread-terminate! t)
           (thread-state t)))
  ] ; gauche.sys.pthreads
 [else])

(test-end)


