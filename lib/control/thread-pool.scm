;;;
;;; control.thread-pool - thread pool
;;;
;;;  Copyright (c) 2003-2007 Time Intermedia Corporation, All rights reserved.
;;;  Copyright (c) 2010-2019  Shiro Kawai  <shiro@acm.org>
;;;
;;;  Redistribution and use in source and binary forms, with or without
;;;  modification, are permitted provided that the following conditions
;;;  are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; Originally written as a part of Kahua project  http://www.kahua.org/
;; Heavily modified by Shiro Kawai to generalize, and to use new Gauche
;; features.

(define-module control.thread-pool
  (use srfi-1)
  (use srfi-19)
  (use data.queue)
  (use util.match)
  (use gauche.threads)
  (use gauche.record)
  (use gauche.mop.propagate)
  (use control.job)
  (export <thread-pool>
          <thread-pool-shut-down>
          make-thread-pool thread-pool-results thread-pool-shut-down?
          add-job! wait-all terminate-all!))
(select-module control.thread-pool)

;; - Thread job is queued in job queue.
;; - add-job! returns a job record, with which the client can track results.
;; - optionally, the client can ask to queue the finished job to result-queue.
;; - while exeuting the job, thread keeps job record in its 'specific' slot.
;; - graceful termination is requested by 'over in the job queue.

(define-class <thread-pool> ()
  ((result-queue :init-form (make-mtqueue)) ; Queue Job
   ;; the rest of slots are private
   (pool         :init-keyword :pool :init-value '()) ; [Thread]
   (size         :init-keyword :size :init-value 2)
   (job-queue    :init-form (make-mtqueue)) ; Queue (Bool . Job)
   (max-backlog  :allocation :propagated
                 :propagate '(job-queue max-length)
                 :init-keyword :max-backlog)
   (shut-down    :init-value #f)       ; #t if the pool is shut down
   )
  :metaclass <propagate-meta>)

(define (make-thread-pool size :key (max-backlog #f))
  (make <thread-pool> :size size :max-backlog max-backlog))

(define-method initialize ((pool <thread-pool>) initargs)
  (next-method)
  (set! (~ pool'pool)
        (list-tabulate (~ pool'size)
                       (lambda (_)
                         (thread-start! (make-thread (cut worker pool)))))))

(define (thread-pool-results pool)    (~ pool'result-queue))
(define (thread-pool-shut-down? pool) (~ pool'shut-down))

;; This condition is raised by add-job! when the pool is
;; shutting down.
(define-condition-type <thread-pool-shut-down> <error> (pool #f))
(define (%shut-down pool)
  (error <thread-pool-shut-down> :pool pool "Thread pool has shut down"))

(define (worker pool)
  (define self (current-thread))
  (match (dequeue/wait! (~ pool'job-queue))
    [(need-result . job)
     (thread-specific-set! self job)
     (job-run! job)                     ; captures errors
     (when need-result (enqueue! (~ pool'result-queue) job))
     (thread-specific-set! self #f)
     (worker pool)]
    [_ #t]))                            ; no more jobs

;; Returns job if queued, #f if job queue is full
(define (add-job! pool thunk :optional (need-result #f) (timeout #f))
  (when (~ pool'shut-down) (%shut-down pool))
  (let1 job (make-job thunk :cancellable #t)
    (job-acknowledge! job)
    (and (enqueue/wait! (~ pool'job-queue) (cons need-result job) timeout #f)
         (if (~ pool'shut-down)
           (%shut-down pool)
           job))))

;; Note: The signature has been changed from 0.9.1, in which wait-all
;; only takes check-interval optional argument.  It is impossible to detect
;; the old usage, since integer is a valid argument as timeout.  However,
;; I hope it does little harm; for example, the old code (wait-all pool #e2e9)
;; to expect check-interval in 2 seconds is now interpreted as the default
;; check-interval and extremely long timeout---about 60 years---which virtually
;; the same as saying "forever", so all you get is slighly off check-interval.
;; Smaller check-interval may be a bit serious, since it may delay response
;; in some situation.  But the default 0.5 seconds isn't really bad, I guess.
(define (wait-all pool :optional (timeout #f) (check-interval #e5e8))
  (define abstime
    (cond [(is-a? timeout <time>) timeout]
          [(real? timeout)
           (receive (subsec sec) (modf timeout)
             (add-duration (current-time)
                           (make-time time-duration
                                      (round->exact (* subsec 1e9))
                                      sec)))]
          [(not timeout) #f]
          [else (error "timeout must be either a real number, a <time> object, \
                        or #f, but got:" timeout)]))
  (let loop ([now (and abstime (current-time))])
    (cond [(and (queue-empty? (~ pool'job-queue))
                (every (^t (not (thread-specific t))) (~ pool'pool)))]
          [(and abstime (time>=? now abstime)) #f] ;timeout
          [else (sys-nanosleep check-interval)
                (loop (and abstime (current-time)))])))

;; For backward compatibility, allow (terminate-all! pool force-timeout)
;; the proper API is (terminate-all! pool :force-timeout force-timeout)
;; (as far as I know, only Kahua is affected by this API change).
;; Rewrite to simplified version once Kahua switches to the new API.
(define (terminate-all! pool . args)
  (match args
    [(val) (%terminate-all! pool :force-timeout val)]
    [_     (apply %terminate-all! pool args)]))

(define (%terminate-all! pool :key (force-timeout #f) (cancel-queued-jobs #f))
  (define size (~ pool'size))

  ;; First, make sure no more jobs are put into the queue.
  (set! (~ pool'shut-down) #t)

  ;; If requested, cancel jobs already queued but not being executing.
  (when cancel-queued-jobs
    (dolist [job (dequeue-all! (~ pool'job-queue))]
      (job-mark-killed! (cdr job) "thread pool has shut down")
      (enqueue! (~ pool'result-queue) (cdr job))))

  ;; Sends threads termination message
  (dotimes [count size]
    (enqueue/wait! (~ pool'job-queue) 'over))

  ;; Wait for termination of threads.
  (dolist [t (~ pool'pool)]
    (unless (thread-join! t force-timeout #f)
      (and-let* ([job (thread-specific t)])
        (job-mark-killed! job "thread pool has shut down"))
      (thread-terminate! t))))
