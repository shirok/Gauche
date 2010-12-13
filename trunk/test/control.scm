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

  (let ((pool (make-thread-pool 5))
        (rvec (make-vector 10 #f)))
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
                  (wait-all pool 1e7)
                  rvec))

    (test* "error results" '(ng ng ng ng ng)
           (begin (dotimes [k 5]
                    (add-job! pool (lambda ()
                                     (sys-nanosleep 1e7)
                                     (raise 'ng)
                                     (vector-set! rvec k k))
                              #t))
                  (wait-all pool 1e7)
                  (map (cut job-result <>)
                       (queue->list (~ pool'result-queue)))))
    )]
 [else])

(test-end)


