;;;
;;; control.scheduler - scheduler
;;;
;;;   Copyright (c) 2021-2022  Shiro Kawai  <shiro@acm.org>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
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
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(define-module control.scheduler
  (use gauche.threads)
  (use gauche.dictionary)
  (use data.queue)
  (use data.priority-map)               ;id -> (thunk time)
  (use srfi-19)
  (use control.job)
  (export <scheduler>
          scheduler-schedule!
          scheduler-reschedule!
          scheduler-remove!
          scheduler-exists?
          scheduler-running?
          scheduler-terminate!))
(select-module control.scheduler)

(define-class <scheduler> ()
  ((error-handler :init-keyword :error-handler :init-value #f)
   ;; The following slots are private.  Only the scheduler's thread
   ;; modifies those data structures.  The client thread (the caller of
   ;; API just inserts request to the queue.
   (request-queue :init-form (make-mtqueue :max-length 0))
   (task-queue :init-form (make-priority-map :value-comparator task-comparator))
   (next-task-id :init-value 0)
   (exception :init-value (undefined))
   (thread)))

(define-method initialize ((s <scheduler>) initargs)
  (next-method)
  (set! (~ s'thread) (make-scheduler-thread s)))

;; Request queue is a queue of jobs (control.job).
;; Public API actually sends a request to the scheduler's thread,
;; and retrieves the result.
(define (request-response scheduler thunk :optional (end? #f))
  (define job (make-job thunk :waitable #t))
  (enqueue/wait! (~ scheduler'request-queue) job #f #f end?)
  (job-wait job)
  (let1 r (job-result job)
    (if (is-a? r <condition>)
      (raise r)
      r)))

;; Task queue is a queue of <task>s.
(define-class <task> ()
  ((id :init-keyword :id)
   (thunk :init-keyword :thunk)
   (time :init-keyword :time)           ;absolute time to invoke thunk
   (interval :init-keyword :interval))) ;#f for one-shot

(define (task-equal a b)
  (and (eqv? (~ a'id) (~ b'id))
       (eq? (~ a'thunk) (~ b'thunk))
       (time=? (~ a'time) (~ b'time))
       (equal? (~ a'time) (~ b'time))))

(define (task-compare a b)
  (time<? (~ a'time) (~ b'time)))

(define task-comparator
  (make-comparator (cut is-a? <> <task>) task-equal task-compare #f))

(define (make-task scheduler thunk time interval)
  (rlet1 t (make <task> :id (~ scheduler'next-task-id)
                 :thunk thunk :time time :interval interval)
    (inc! (~ scheduler'next-task-id))))

;; Scheduler thread.
;; Each scheduler runs an event processing loop with this thread.
;; When a task throws an error and not handled by error-handler, we
;; still keep running thread but no longer runs ready tasks.
(define (scheduler-thread-proc s)
  (define (cancel-by-error e)
    (set! (~ s'exception) e))
  (define end? #f)
  (^[]
    (do () (end?)
      (guard (e [(eq? e 'end) (set! end? #t)]
                [else
                 (if-let1 eh (~ s'error-handler)
                   (guard (e [else (cancel-by-error e)])
                     (eh e))
                   (cancel-by-error e))])
        (unless (slot-bound? s 'exception)
          (run-ready-tasks! (~ s'task-queue)))
        (and-let1 req (dequeue/wait! (~ s'request-queue)
                                     (next-check-time (~ s'task-queue)))
          (job-run! req))))))

(define (make-scheduler-thread s)
  (thread-start! (make-thread (scheduler-thread-proc s))))

;; Run tasks ready to execute
(define (run-ready-tasks! task-queue)
  (and-let* ([p (priority-map-min task-queue)]
             [id (car p)]
             [task (cdr p)])
    (when (time<=? (~ task'time) (current-time))
      (dict-delete! task-queue id)
      ((~ task'thunk))
      (when (and (~ task'interval) (not (eqv? (~ task'interval) 0)))
        (set! (~ task'time) (absolute-time (~ task'interval)))
        (dict-put! task-queue id task))
      (run-ready-tasks! task-queue))))

;; Return next absolute time to check the task queue.  #f if no need to check.
(define (next-check-time task-queue)
  (and-let1 p (priority-map-min task-queue)
    (~ (cdr p)'time)))

(define (validate-time when)
  (or (and (real? when) (>= when 0))
      (and (time? when) (memq (time-type when) '(time-duration time-utc)))
      (error "Nonnegative real number or <time> is expected, but got:" when)))

(define (absolute-time when)
  (cond [(real? when)
         (receive (dfrac dsec) (modf when)
           (let* ([now (current-time)]
                  [sec  (+ (time-second now) (exact dsec))]
                  [nsec (+ (time-nanosecond now)
                           (round->exact (* dfrac 1e9)))])
             (receive (sec nsec)
                 (if (>= nsec #e1e9)
                   (values (+ sec (exact (quotient nsec #e1e9)))
                           (modulo nsec #e1e9))
                   (values sec nsec))
               (make-time time-utc nsec sec))))]
        [(time? when)
         (case (time-type when)
           [(time-duration) (add-duration (current-time) when)]
           [(time-utc time-tai) when]
           [else (error "bad time object for 'when':" when)])]
        [else (error "bad object for 'when':" when)]))

;; API
;; Returns task id
(define (scheduler-schedule! s thunk when :optional (interval #f))
  (validate-time when)
  (and interval (validate-time interval))
  ($ request-response s
     (^[]
       (let1 task (make-task s thunk (absolute-time when) interval)
         (dict-put! (~ s'task-queue) (~ task'id) task)
         (~ task'id)))))

;; API
(define (scheduler-reschedule! s task-id when :optional (interval #f))
  (validate-time when)
  (and interval (validate-time interval))
  ($ request-response s
     (^[]
       (if-let1 task (dict-get (~ s'task-queue) task-id #f)
         (begin
           (dict-delete! (~ s'task-queue) task-id)
           (set! (~ task'time) (absolute-time when))
           (set! (~ task'interval) 0)
           (dict-put! (~ s'task-queue) task-id task)
           task-id)
         (condition
          (<error> (message (format "No task with id: ~s" task-id))))))))

;; API
;;  Returns #t if task is removed, #f if not, for srfi-120 specifies so.
;;  Relies on such behaivor of dict-delete!
(define (scheduler-remove! s task-id)
  ($ request-response s
     (^[] (dict-delete! (~ s'task-queue) task-id))))

;; API
(define (scheduler-exists? s task-id)
  ($ request-response s
     (^[] (dict-exists? (~ s'task-queue) task-id))))

;; API
(define (scheduler-running? s)
  (not (~ s'request-queue'closed)))

;; API
(define (scheduler-terminate! s :key (on-error :reraise))
  (assume (memq on-error '(:reraise :return)))
  ($ request-response s (^[] (raise 'end)) #t)
  (if (and (slot-bound? s 'exception)
           (eq? on-error :reraise))
    (raise (~ s'exception))
    #t))
