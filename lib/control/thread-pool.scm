;;;
;;; control.thread-pool - thread pool
;;;
;;;  Copyright (c) 2003-2007 Time Intermedia Corporation, All rights reserved.
;;;  Copyright (c) 2010 Shiro Kawai
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
  (use util.queue)
  (use util.match)
  (use gauche.threads)
  (use gauche.record)
  (use gauche.mop.propagate)
  (use control.job)
  (export <thread-pool>
	  make-thread-pool
          add-job!
	  wait-all
	  terminate-all!))
(select-module control.thread-pool)

;; - Thread job is queued in job queue.
;; - add-job! returns a job record, with which the client can track results.
;; - optionally, the client can ask to queue the finished job to result-queue.
;; - while exeuting the job, thread keeps job record in its specific slot.
;; - graceful termination is requested by 'over in the job queue.

(define-class <thread-pool> ()
  ((pool         :init-keyword :pool :init-value '()) ; [Thread]
   (size         :init-keyword :size :init-value 2)
   (job-queue    :init-form (make-mtqueue)) ; Queue Job
   (result-queue :init-form (make-mtqueue)) ; Queue Job
   (max-backlog  :allocation :propagated
                 :propagate '(job-queue max-length)
                 :init-keyword :max-backlog)
   )
  :metaclass <propagate-meta>)

(define (make-thread-pool size :key (max-backlog 0))
  (make <thread-pool> :size size :max-backlog max-backlog))

(define-method initialize ((pool <thread-pool>) initargs)
  (next-method)
  (set! (~ pool'pool)
        (list-tabulate (~ pool'size)
                       (lambda (_)
                         (thread-start! (make-thread (cut worker pool)))))))

(define (worker pool)
  (define self (current-thread))
  (match (dequeue/wait! (~ pool'job-queue))
    [(need-result . job)
     (thread-specific-set! self job)
     (job-run! job)                     ; captures errors
     (thread-specific-set! self #f)
     (when need-result (enqueue! (~ pool'result-queue) job))
     (worker pool)]
    [_ #t]))                            ; no more jobs

;; Returns job if queued, #f if job queue is full
(define (add-job! pool thunk :optional (need-result #f))
  (let1 job (make-job thunk)
    (job-acknowledge! job)
    (and (enqueue/wait! (~ pool'job-queue) (cons need-result job) 0 #f)
         job)))

(define (wait-all pool :optional (check-interval #e5e8))
  (do []
      [(and (queue-empty? (~ pool'job-queue))
            (every (^t (not (thread-specific t))) (~ pool'pool)))]
    (sys-nanosleep check-interval)))

(define (terminate-all! pool :optional (force-timeout #f))
  (define size (~ pool'size))
  ;; Sends threads termination message
  (let loop ((count 0))
    (cond [(>= count size)]
          [(> (mtqueue-room (~ pool'job-queue)) 0)
           (enqueue! (~ pool'job-queue) 'over)
           (loop (+ count 1))]
          [else (sys-nanosleep 5e8) (loop count)]))
  ;;
  (dolist [t (~ pool'pool)]
    (unless (thread-join! t force-timeout #f)
      (thread-terminate! t))))
