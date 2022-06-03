;;;
;;; srfi-120 - Timer
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

;; SRFI-120's Timer is just a <scheduler>

(define-module srfi-120
  (use control.scheduler)
  (use srfi-19)
  (export make-timer timer? timer-cancel!
          timer-schedule! timer-reschedule!
          timer-task-remove! timer-task-exists?
          make-timer-delta timer-delta?))
(select-module srfi-120)

(define (make-timer :optional (error-handler #f))
  (make <scheduler> :error-handler error-handler))

(define (%when when)
  (cond [(integer? when) (/ when 1e3)] ;ms
        [(is-a? when <time>) when]
        [else (error "integer (ms) or <time> required, but got:" when)]))

(define (timer? obj) (is-a? obj <scheduler>))

(define (timer-cancel! timer)
  (scheduler-terminate! timer))

(define (timer-schedule! timer thunk when :optional (period 0))
  (scheduler-schedule! timer thunk (%when when) period))

(define (timer-reschedule! timer task-id when :optional (period 0))
  (scheduler-reschedule! timer task-id (%when when) period))

(define (timer-task-remove! timer task-id)
  (scheduler-remove! timer task-id))

(define (timer-task-exists? timer task-id)
  (scheduler-exists? timer task-id))

(define (make-timer-delta n unit)
  (define (make-duration nanosecs secs)
    (make-time time-duration
               (remainder nanosecs #e1e9)
               (+ secs (quotient nanosecs #e1e9))))
  (case unit
    [(h)  (make-duration 0 (* n 3600))]
    [(m)  (make-duration 0 (* n 60))]
    [(s)  (make-duration 0 n)]
    [(ms) (make-duration (* n #e1e6) 0)]
    [(us) (make-duration (* n #e1e3) 0)]
    [(ns) (make-duration n 0)]
    [else (error "Unknown time unit.  Either one of (h m s ms us ns) \
                  expected, but got:" unit)]))

(define (timer-delta? obj)
  (and (is-a? obj <time>)
       (eq? (time-type obj) time-duration)))
