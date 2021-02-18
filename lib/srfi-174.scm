;;;
;;; srfi-174 - POSIX Timespecs
;;;
;;;   Shiro Kawai
;;;   This is simply a wrapper of srfi-19 time type.
;;;   I put this in Public Domain.
;;;

(define-module srfi-174
  (use srfi-19)
  (export timespec timespec?
          timespec-seconds timespec-nanoseconds
          inexact->timespec timespec->inexact
          timespec=? timespec<? timespec-hash))
(select-module srfi-174)

(define (timespec secs nsecs)
  (make-time time-utc nsecs secs))

(define (timespec? obj) (is-a? obj <time>))

(define-inline (timespec-seconds obj) (time-second obj))
(define-inline (timespec-nanoseconds obj) (time-nanosecond obj))

(define (inexact->timespec v)
  (receive (frac secs) (modf v)
    ;; be careful that scaling and rounding frac may cause nanos to be 10e9.
    (let ([nanos (round->exact (* (abs frac) #e1_000_000_000))]
          [secs  (exact secs)])
      (timespec (if (>= nanos #e1_000_000_000)
                  (if (>= secs 0)
                    (+ secs 1)
                    (- secs 1))
                  secs)
                (modulo nanos #e1_000_000_000)))))

(define (timespec->inexact v)
  (if (>= (timespec-seconds v) 0)
    (+ (inexact (timespec-seconds v))
       (* (inexact (timespec-nanoseconds v)) 1e-9))
    (- (inexact (timespec-seconds v))
       (* (inexact (timespec-nanoseconds v)) 1e-9))))

(define (timespec=? ts1 ts2) (time=? ts1 ts2))
(define (timespec<? ts1 ts2) (time<? ts1 ts2))

(define (timespec-hash ts) (default-hash ts))
