;;;
;;; text.progress - shows progress bar
;;;
;;;   Copyright (c) 2007-2019  Shiro Kawai  <shiro@acm.org>
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

(define-module text.progress
  (use srfi-19)
  (export make-text-progress-bar))
(select-module text.progress)


;; Progress bar sample:
;;
;; <-- header---> <------- bar -------> <- num -><-time->     <-- info -->
;; foo           |##############       |  123/211   01:21 ETA   sending...
;;               ^
;;             separator

;; The 'ETA' calculation is derived from the scmail's progress bar code
;; written by Satoru Takabayashi.

(define (make-text-progress-bar :key
                                (header  "progress")
                                (header-width 14)
                                (bar-char #\#)
                                (bar-width 40)
                                (num-width 9)
                                (num-format (^[cur max] 
                                              (format "~d/~d" cur max)))
                                (time-width 7)
                                (info-width 0)
                                (info  "")
                                (separator-char #\|)
                                (max-value 100)
                                (port (current-output-port)))
  (assume (positive? max-value))
  (let ([current-value 0]
        [start-time (current-time)]
        [finish-time  #f])
    (define (show)
      (format port "~v,,,,va~a~v,,,,va~a~v,,,,v@a~v,,,,v@a~a~v,,,,va~a"
              header-width header-width header
              (or separator-char "")
              bar-width bar-width (make-bar)
              (or separator-char "")
              num-width num-width
              (if (> num-width 0) (num-format current-value max-value) "")
              time-width time-width
              (if (> time-width 0) (make-time) "")
              (if (and (not finish-time) (> time-width 0)) " ETA" "    ")
              info-width info-width
              (if (> info-width 0) info "")
              (if finish-time "\n" "\r"))
      (flush port))
    (define (make-bar)
      (make-string (floor->exact (* (/. current-value max-value) bar-width))
                   bar-char))
    (define (fmt-time seconds)
      (cond
       [(not seconds) "--:--"]
       [(>= seconds 3600)
        (format "~d:~2,'0d:~2,'0d"
                (quotient seconds 3600)
                (quotient (remainder seconds 3600) 60)
                (remainder seconds 60))]
       [else
        (format "~2,'0d:~2,'0d"
                (quotient seconds 60)
                (remainder seconds 60))]))
    (define (time- s t)
      (time->seconds (time-difference s t)))
    (define (make-time)
      (cond
       [finish-time
        (fmt-time (round->exact (time- finish-time start-time)))]
       [(zero? current-value)
        (fmt-time #f)]
       [else
        (let* ([used (time- (current-time) start-time)]
               [eta  (- (/. (* used max-value) current-value) used)])
          (fmt-time (round->exact eta)))]))

    (^[msg . args]
      (case msg
        [(show) (show)]
        [(set)
         (when (null? args)
           (error "text-progress-bar: message 'set requires an argument"))
         (set! current-value (car args))
         (show)]
        [(inc)
         (when (null? args)
           (error "text-progress-bar: message 'inc requires an argument"))
         (set! current-value (min (+ (car args) current-value) max-value))
         (show)]
        [(finish)
         (set! finish-time (current-time))
         (show)]
        [(set-header)
         (when (null? args)
           (error "text-progress-bar: message 'set-header requires an argument"))
         (set! header (car args))
         (show)]
        [(set-info)
         (when (null? args)
           (error "text-progress-bar: message 'set-info requires an argument"))
         (set! info (car args))
         (show)]
        [else
         (error "text-progress-bar: unrecognized message:" msg)]))
    ))

