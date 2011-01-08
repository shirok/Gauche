;; Tests for control.* modules

(use gauche.test)
(use srfi-1)
(use srfi-19)
(use util.queue)

(test-start "control")

(test-section "control.job")
(use control.job)
(test-module 'control.job)

(let ((j #f)
      (z #f))
  (test* "make-job" #t
         (begin (set! j (make-job (lambda () #f)))
                (job? j)))

  (dolist [when `((:acknowledge ,job-acknowledge-time)
                  (:start       ,job-start-time)
                  (:finish      ,job-finish-time))]
    (test* (format "job-touch! ~a (pre)" (car when)) #f
           ((cadr when) j))
    (test* (format "job-touch! ~a" (car when)) #t
           (let1 time (job-touch! j (car when))
             (time=? time ((cadr when) j)))))
    
  (set! j (make-job (lambda () (set! z #t) 'ok)))
  (test* "job-run! precondition" '(#f #f) (list (job-status j) z))
  (test* "job-run! postcondition" '(done ok #t)
         (begin (job-run! j)
                (list (job-status j) (job-result j) z)))

  (set! j (make-job (lambda () (raise 'gosh) 'ok)))
  (test* "job-run! error condition" '(error gosh)
         (begin (job-run! j)
                (list (job-status j) (job-result j))))

  (set! j (make-job (lambda () #f)))
  (test* "job-run! killed" '(killed test)
         (begin (job-mark-killed! j 'test)
                (list (job-status j) (job-result j))))
  )

(cond-expand
 [gauche.sys.pthreads
  (test-section "control.thread-pool")
  (use control.thread-pool)
  (test-module 'control.thread-pool)

  (let ([pool (make-thread-pool 5)]
        [rvec (make-vector 10 #f)])
    (test* "pool" '(5 #t 5 0)
           (list (length (~ pool'pool))
                 (every thread? (~ pool'pool))
                 (~ pool'size)
                 (~ pool'max-backlog)))

    (test* "doit" '#(0 1 2 3 4 5 6 7 8 9)
           (begin (dotimes [k 10]
                    (add-job! pool (lambda ()
                                     (sys-nanosleep 1e7)
                                     (vector-set! rvec k k))))
                  (and (wait-all pool #f 1e7) rvec)))

    (test* "error results" '(ng ng ng ng ng)
           (begin (dotimes [k 5]
                    (add-job! pool (lambda ()
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
           ;;  - add-job!  - this will block because queue is full
           ;;  - terminate-all! - this causes the pending add-job! to raise
           ;;                     an exception.  this call blocks.
           ;;  - setting gate to #f - this causes the remaining thread to
           ;;                     run, and the blocking terminate-all! to
           ;;                     unblock.
           (let ([t1 (make-thread (lambda ()
                                    (sys-nanosleep #e1e8)
                                    (terminate-all! pool
                                                    :cancel-queued-jobs #t)
                                    (set! gate 'finished)))]
                 [t2 (make-thread (lambda ()
                                    (sys-nanosleep #e2e8)
                                    (set! gate #t)))])
             (thread-start! t1)
             (thread-start! t2)
             (add-job! pool work)))

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
  ]
 [else])

(test-end)


