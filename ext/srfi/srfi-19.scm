;; SRFI-19: Time Data Types and Procedures.
;;
;; Copyright (C) Neodesic Corporation (2000). All Rights Reserved.
;;
;; This document and translations of it may be copied and furnished to others,
;; and derivative works that comment on or otherwise explain it or assist in its
;; implementation may be prepared, copied, published and distributed, in whole or
;; in part, without restriction of any kind, provided that the above copyright
;; notice and this paragraph are included on all such copies and derivative works.
;; However, this document itself may not be modified in any way, such as by
;; removing the copyright notice or references to the Scheme Request For
;; Implementation process or editors, except as needed for the purpose of
;; developing SRFIs in which case the procedures for copyrights defined in the SRFI
;; process must be followed, or as required to translate it into languages other
;; than English.
;;
;; The limited permissions granted above are perpetual and will not be revoked
;; by the authors or their successors or assigns.
;;
;; This document and the information contained herein is provided on an "AS IS"
;; basis and THE AUTHOR AND THE SRFI EDITORS DISCLAIM ALL WARRANTIES, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO ANY WARRANTY THAT THE USE OF THE
;; INFORMATION HEREIN WILL NOT INFRINGE ANY RIGHTS OR ANY IMPLIED WARRANTIES OF
;; MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.

;;; Modified for Gauche by Shiro Kawai, shiro@acm.org

(define-module srfi-19
  (use srfi-1)
  (use gauche.sequence)
  (use srfi-13)
  (export time-tai time-utc time-monotonic time-thread
          time-process time-duration current-time time-resolution
          make-time time? time-type time-second time-nanosecond
          set-time-type! set-time-second! set-time-nanosecond! copy-time
          time=? time<? time<=? time>? time>=?
          time-difference time-difference! add-duration add-duration!
          subtract-duration subtract-duration!
          make-date date? date-nanosecond date-second date-minute
          date-hour date-day date-month date-year date-zone-offset
          date-year-day date-week-day date-week-number current-date
          current-julian-day current-modified-julian-day
          date->julian-day date->modified-julian-day date->time-monotonic
          date->time-tai date->time-utc
          julian-day->date julian-day->time-monotonic
          julian-day->time-tai julian-day->time-utc
          modified-julian-day->date modified-julian-day->time-monotonic
          modified-julian-day->time-tai modified-julian-day->time-utc
          time-monotonic->date time-monotonic->julian-day
          time-monotonic->modified-julian-day
          time-monotonic->time-tai time-monotonic->time-tai!
          time-monotonic->time-utc time-monotonic->time-utc!
          time-utc->date time-utc->julian-day
          time-utc->modified-julian-day
          time-utc->time-monotonic time-utc->time-monotonic!
          time-utc->time-tai time-utc->time-tai!
          time-tai->date time-tai->julian-day
          time-tai->modified-julian-day
          time-tai->time-monotonic time-tai->time-monotonic!
          time-tai->time-utc time-tai->time-utc!
          date->string string->date <date>
          )
  )
(select-module srfi-19)

;;;----------------------------------------------------------
;;; Constants
;;;

(define-constant time-tai 'time-tai)
(define-constant time-utc 'time-utc)
(define-constant time-monotonic 'time-monotonic)
(define-constant time-thread 'time-thread)
(define-constant time-process 'time-process)
(define-constant time-duration 'time-duration)

;; example of extension (MZScheme specific)
;(define time-gc 'time-gc)

;;-- Miscellaneous Constants.
;;-- only the tm:tai-epoch-in-jd might need changing if
;;   a different epoch is used.

;; NB: If the constant TM_EXACT_CALC is true, the calculation of julian-day
;; and modified-julian-day is done in exact calculation.  It would have some
;; penalty.  Here's some small benchmark results of round-trip conversion from
;; time-utc -> julian-day -> time-utc, repeating 100000 times on 0.8.12.
;;
;;            current-time     day-boundary     half-day-boundary
;; exact          4.67             3.17                2.76
;; inexact        1.67             1.95                1.94
;;
;;   'day-boundary' uses (make-time time-utc 0 86400)
;;   'half-day-boundary' uses (make-time time-utc 0 43200)
;;
;; It does slow down the calculation, but I'd say it's not so bad.  I'll
;; go for exact calculation for now.
(define-constant TM_EXACT_CALC #t)

(define-constant tm:nano 1000000000)
(define-constant tm:nano/i (if TM_EXACT_CALC tm:nano (exact->inexact tm:nano)))
(define-constant tm:sid  86400)    ; seconds in a day
(define-constant tm:sihd 43200)    ; seconds in a half day
(define-constant tm:tai-epoch-in-jd  ; julian day number for 'the epoch'
  (let1 off 4881175/2
    (if TM_EXACT_CALC off (exact->inexact off))))

;; each entry is ( tai seconds since epoch . # seconds to subtract for utc )
;; note they go higher to lower, and end in 1972.
;; See srfi-19/read-tai.scm to update this list.
(define-constant tm:leap-second-table
  '((1435708800 . 36)
    (1341100800 . 35)
    (1230768000 . 34)
    (1136073600 . 33)
    (915148800 . 32)
    (867715200 . 31)
    (820454400 . 30)
    (773020800 . 29)
    (741484800 . 28)
    (709948800 . 27)
    (662688000 . 26)
    (631152000 . 25)
    (567993600 . 24)
    (489024000 . 23)
    (425865600 . 22)
    (394329600 . 21)
    (362793600 . 20)
    (315532800 . 19)
    (283996800 . 18)
    (252460800 . 17)
    (220924800 . 16)
    (189302400 . 15)
    (157766400 . 14)
    (126230400 . 13)
    (94694400  . 12)
    (78796800  . 11)
    (63072000  . 10)))

(define-constant tm:leap-second-base
  (* (- 1972 1970) 365 tm:sid))

(define (tm:leap-second-delta utc-seconds)
  (if (< utc-seconds tm:leap-second-base)
      0
      (let loop ((table tm:leap-second-table))
        (if (>= utc-seconds (caar table))
            (cdar table)
            (loop (cdr table))))))

;;;----------------------------------------------------------
;;; TIME strcture interface
;;;  The <time> class is built-in.  We just define some APIs.

(define-method time-type       ((t <time>)) (slot-ref t 'type))
(define-method time-second     ((t <time>)) (slot-ref t 'second))
(define-method time-nanosecond ((t <time>)) (slot-ref t 'nanosecond))

(define-method set-time-type!  ((t <time>) s)
  (slot-set! t 'type s))
(define-method set-time-second! ((t <time>) s)
  (slot-set! t 'second s))
(define-method set-time-nanosecond! ((t <time>) s)
  (slot-set! t 'nanosecond s))

(define (make-time type nanosecond second)
  (make <time> :type type :second second :nanosecond nanosecond))

(define (copy-time time)
  (make <time>
    :type       (time-type time)
    :second     (time-second time)
    :nanosecond (time-nanosecond time)))

;;;----------------------------------------------------------
;;; Error check routine
;;;

(define-syntax tm:check-time-type
  (syntax-rules ()
    [(_ time type caller)
     (unless (eq? (time-type time) type)
       (errorf "~a: incompatible time type: ~a type required, but got ~a"
               caller type time))]))

;;;----------------------------------------------------------
;;; Current-time
;;;

(define (tm:make-time-usec type sec usec)
  (make-time type (* usec 1000) sec))

(define (tm:current-time-process type)
  (let* ([times (sys-times)]
         [cpu   (+ (car times) (cadr times))]
         [tick  (list-ref times 4)]
         [sec   (quotient cpu tick)]
         [nsec  (* (/. tm:nano tick) (remainder cpu tick))])
    (make-time type nsec sec)))

(define (tm:current-time-tai type)
  (let* ([now (with-module gauche (current-time))]
         [sec (slot-ref now 'second)])
    (make <time> :type type :second (+ sec (tm:leap-second-delta sec))
          :nanosecond (slot-ref now 'nanosecond))))


;; redefine built-in current-time
(define (current-time :optional (clock-type 'time-utc))
  (case clock-type
    [(time-tai) (tm:current-time-tai clock-type)]
    [(time-utc) (with-module gauche (current-time))]
    [(time-monotonic) (tm:current-time-tai clock-type)]
    [(time-thread)  (tm:current-time-process 'time-thread)]
    [(time-process) (tm:current-time-process 'time-process)]
    [else (error "current-time: invalid-clock-type" clock-type)]))

;; -- Time Resolution
;; This is the resolution of the clock in nanoseconds.

;; We don't really know ...  for now, just return 10ms.
(define (time-resolution . args)
  10000000)

;; -- Time comparisons
;; [SK] we can use builtin comparison.
(define-syntax define-tm:cmp
  (syntax-rules ()
    [(_ (name time1 time2) expr)
     (define (name time1 time2)
       (unless (time? time1)
         (errorf "~a: time objects are required, but got ~s" 'name time1))
       (unless (time? time2)
         (errorf "~a: time objects are required, but got ~s" 'name time2))
       expr)]))

(define-tm:cmp (time=? time1 time2) (equal? time1 time2))
(define-tm:cmp (time>? time1 time2) (> (compare time1 time2) 0))
(define-tm:cmp (time<? time1 time2) (< (compare time1 time2) 0))
(define-tm:cmp (time>=? time1 time2) (>= (compare time1 time2) 0))
(define-tm:cmp (time<=? time1 time2) (<= (compare time1 time2) 0))

;; -- Time arithmetic

(define (tm:time-difference time1 time2 time3)
  (if (or (not (and (time? time1) (time? time2)))
          (not (eq? (time-type time1) (time-type time2))))
    (errorf "time-difference: incompatible time types: ~s, ~s" time1 time2)
    (let ([sec-diff  (- (time-second time1) (time-second time2))]
          [nsec-diff (- (time-nanosecond time1) (time-nanosecond time2))])
      (set-time-type! time3 time-duration)
      (if (negative? nsec-diff)
        (begin
          (set-time-second! time3 (- sec-diff 1))
          (set-time-nanosecond! time3 (+ tm:nano nsec-diff)))
        (begin
          (set-time-second! time3 sec-diff)
          (set-time-nanosecond! time3 nsec-diff)))
      time3)))

(define (time-difference time1 time2)
  (tm:time-difference time1 time2 (make <time>)))

(define (time-difference! time1 time2)
  (tm:time-difference time1 time2 time1))

(define (tm:add-duration time1 duration time3)
  (when (not (and (time? time1) (time? duration)))
    (errorf "add-duration: incompatible type types: ~a ~a"
            time1 duration))
  (tm:check-time-type duration 'time-duration 'add-duration)
  (let ([sec-plus (+ (time-second time1) (time-second duration))]
        [nsec-plus (+ (time-nanosecond time1) (time-nanosecond duration))])
    (let ([r (remainder nsec-plus tm:nano)]
          [q (quotient nsec-plus tm:nano)])
      (if (negative? r)
        (begin
          (set-time-second! time3 (+ sec-plus q -1))
          (set-time-nanosecond! time3 (+ tm:nano r)))
        (begin
          (set-time-second! time3 (+ sec-plus q))
          (set-time-nanosecond! time3 r)))
      time3)))

(define (add-duration time1 duration)
  (tm:add-duration time1 duration (make <time> :type (time-type time1))))

(define (add-duration! time1 duration)
  (tm:add-duration time1 duration time1))

(define (tm:subtract-duration time1 duration time3)
  (when (not (and (time? time1) (time? duration)))
    (errorf "subtract-duration: incompatible type types: ~a ~a"
            time1 duration))
  (tm:check-time-type duration 'time-duration 'subtract-duration)
  (let ([sec-minus  (- (time-second time1) (time-second duration))]
        [nsec-minus (- (time-nanosecond time1) (time-nanosecond duration))])
    (let ([r (remainder nsec-minus tm:nano)]
          [q (quotient nsec-minus tm:nano)])
      (if (negative? r)
        (begin
          (set-time-second! time3 (- sec-minus q 1))
          (set-time-nanosecond! time3 (+ tm:nano r)))
        (begin
          (set-time-second! time3 (- sec-minus q))
          (set-time-nanosecond! time3 r)))
      time3)))

(define (subtract-duration time1 duration)
  (tm:subtract-duration time1 duration (make <time> :type (time-type time1))))

(define (subtract-duration! time1 duration)
  (tm:subtract-duration time1 duration time1))

;; -- Converters between types.

(define (tm:time-tai->time-utc! time-in time-out caller)
  (tm:check-time-type time-in 'time-tai caller)
  (set-time-type! time-out time-utc)
  (set-time-nanosecond! time-out (time-nanosecond time-in))
  (set-time-second!     time-out (- (time-second time-in)
                                    (tm:leap-second-delta
                                     (time-second time-in))))
  time-out)

(define (time-tai->time-utc time-in)
  (tm:time-tai->time-utc! time-in (make <time>) 'time-tai->time-utc))

(define (time-tai->time-utc! time-in)
  (tm:time-tai->time-utc! time-in time-in 'time-tai->time-utc!))


(define (tm:time-utc->time-tai! time-in time-out caller)
  (tm:check-time-type time-in 'time-utc caller)
  (set-time-type! time-out time-tai)
  (set-time-nanosecond! time-out (time-nanosecond time-in))
  (set-time-second!     time-out (+ (time-second time-in)
                                    (tm:leap-second-delta
                                     (time-second time-in))))
  time-out)

(define (time-utc->time-tai time-in)
  (tm:time-utc->time-tai! time-in (make <time>) 'time-utc->time-tai))

(define (time-utc->time-tai! time-in)
  (tm:time-utc->time-tai! time-in time-in 'time-utc->time-tai!))

;; -- these depend on time-monotonic having the same definition as time-tai!
(define (time-monotonic->time-utc time-in)
  (tm:check-time-type time-in 'time-monotonic 'time-monotonic->time-utc)
  (let1 ntime (copy-time time-in)
    (set-time-type! ntime time-tai)
  (tm:time-tai->time-utc! ntime ntime 'time-monotonic->time-utc)))

(define (time-monotonic->time-utc! time-in)
  (tm:check-time-type time-in 'time-monotonic 'time-monotonic->time-utc!)
  (set-time-type! time-in time-tai)
  (tm:time-tai->time-utc! time-in time-in 'time-monotonic->time-utc))

(define (time-monotonic->time-tai time-in)
  (tm:check-time-type time-in 'time-monotonic 'time-monotonic->time-tai)
  (rlet1 ntime (copy-time time-in)
    (set-time-type! ntime time-tai)))

(define (time-monotonic->time-tai! time-in)
  (tm:check-time-type time-in 'time-monotonic 'time-monotonic->time-tai!)
  (set-time-type! time-in time-tai)
  time-in)

(define (time-utc->time-monotonic time-in)
  (tm:check-time-type time-in 'time-utc 'time-utc->time-monotonic)
  (rlet1 ntime (tm:time-utc->time-tai! time-in (make <time>)
                                       'time-utc->time-monotonic)
    (set-time-type! ntime time-monotonic)))

(define (time-utc->time-monotonic! time-in)
  (tm:check-time-type time-in 'time-utc 'time-utc->time-monotonic!)
  (rlet1 ntime (tm:time-utc->time-tai! time-in time-in
                                       'time-utc->time-monotonic!)
    (set-time-type! ntime time-monotonic)))

(define (time-tai->time-monotonic time-in)
  (tm:check-time-type time-in 'time-tai 'time-tai->time-monotonic)
  (rlet1 ntime (copy-time time-in)
    (set-time-type! ntime time-monotonic)))

(define (time-tai->time-monotonic! time-in)
  (tm:check-time-type time-in 'time-tai 'time-tai->time-monotonic!)
  (set-time-type! time-in time-monotonic)
  time-in)

;; -- Date Structures

(define-class <date> ()
  ((nanosecond :init-keyword :nanosecond :getter date-nanosecond)
   (second     :init-keyword :second     :getter date-second)
   (minute     :init-keyword :minute     :getter date-minute)
   (hour       :init-keyword :hour       :getter date-hour)
   (day        :init-keyword :day        :getter date-day)
   (month      :init-keyword :month      :getter date-month)
   (year       :init-keyword :year       :getter date-year)
   (zone-offset :init-keyword :zone-offset :getter date-zone-offset)))

(define (date? obj) (is-a? obj <date>))

(define (make-date nanosecond second minute hour day month year zone-offset)
  (make <date>
    :nanosecond nanosecond :second second :minute minute :hour hour
    :day day :month month :year year :zone-offset zone-offset))

(define-method write-object ((obj <date>) port)
  (format port "#<date ~d/~2,'0d/~2,'0d ~2,'0d:~2,'0d:~2,'0d.~9,'0d (~a)>"
          (date-year obj) (date-month obj) (date-day obj)
          (date-hour obj) (date-minute obj) (date-second obj)
          (date-nanosecond obj) (date-zone-offset obj)))

;; gives the julian day which starts at noon.
(define (tm:encode-julian-day-number day month year)
  (let* ([a (quotient (- 14 month) 12)]
         [y (- (+ year 4800) a (if (negative? year) -1  0))]
         [m (- (+ month (* 12 a)) 3)])
    (+ day
       (quotient (+ (* 153 m) 2) 5)
       (* 365 y)
       (quotient y 4)
       (- (quotient y 100))
       (quotient y 400)
       -32045)))

;; Gives the seconds/date/month/year
(define (tm:decode-julian-day-number jdn)
  (let* ([days (truncate->exact jdn)]
         [a (+ days 32044)]
         [b (quotient (+ (* 4 a) 3) 146097)]
         [c (- a (quotient (* 146097 b) 4))]
         [d (quotient (+ (* 4 c) 3) 1461)]
         [e (- c (quotient (* 1461 d) 4))]
         [m (quotient (+ (* 5 e) 2) 153)]
         [y (+ (* 100 b) d -4800 (quotient m 10))])
    (values ; seconds date month year
     (round->exact (* (- jdn days) tm:sid))
     (+ e (- (quotient (+ (* 153 m) 2) 5)) 1)
     (+ m 3 (* -12 (quotient m 10)))
     (if (>= 0 y) (- y 1) y))
    ))

;; Offset of local timezone in seconds.
;; System-dependent.

(define (tm:local-tz-offset)
  (define (tm->seconds-in-year tm)
    (+ (cond [(assv (+ (slot-ref tm 'mon) 1) tm:month-assoc)
              => (^p (* (+ (cdr p)
                           (slot-ref tm 'mday)
                           (if (and (> (car p) 2)
                                    (tm:leap-year? (slot-ref tm 'year)))
                             1 0))
                        3600 24))]
             [else (error "something wrong")])
       (* (slot-ref tm 'hour) 3600)
       (* (slot-ref tm 'min) 60)))
  (let* ([now   (sys-time)]
         [local (sys-localtime now)]
         [local-sec (tm->seconds-in-year local)]
         [local-yr  (slot-ref local 'year)]
         [gm    (sys-gmtime now)]
         [gm-sec (tm->seconds-in-year gm)]
         [gm-yr  (slot-ref gm 'year)])
    (cond [(= local-yr gm-yr)
           (- local-sec gm-sec)]
          ;; The following two cases are very rare, when this function is
          ;; called very close to the year boundary.
          [(< local-yr gm-yr)
           (- (- local-sec
                 (if (tm:leap-year? (slot-ref local 'year)) 31622400 31536000))
              gm-sec)]
          [else
           (- local-sec
              (- gm-sec
                 (if (tm:leap-year? (slot-ref gm 'year)) 31622400 31536000)))]
          )))

;; special thing -- ignores nanos
(define (tm:time->julian-day-number seconds tz-offset)
  (/ (+ (* tm:tai-epoch-in-jd tm:sid)
        seconds tz-offset tm:sihd)
     tm:sid))

(define (tm:leap-second? second)
  (and (assoc second tm:leap-second-table) #t))

(define (time-utc->date time :optional (offset (tm:local-tz-offset)))
  (tm:check-time-type time 'time-utc 'time-utc->date)
  (let1 is-leap-second (tm:leap-second? (+ offset (time-second time)))
    (receive (secs date month year)
        (if is-leap-second
          (tm:decode-julian-day-number (tm:time->julian-day-number (- (time-second time) 1) offset))
          (tm:decode-julian-day-number (tm:time->julian-day-number (time-second time) offset)))
      (let* ([hours    (quotient secs (* 60 60))]
             [rem      (remainder secs (* 60 60))]
             [minutes  (quotient rem 60)]
             [seconds  (remainder rem 60)])
        (make-date (time-nanosecond time)
                   (if is-leap-second (+ seconds 1) seconds)
                   minutes
                   hours
                   date
                   month
                   year
                   offset)))))

(define (time-tai->date time :optional (offset (tm:local-tz-offset)))
  (tm:check-time-type time 'time-tai 'time-tai->date)
  (let* ([seconds (- (time-second time) (tm:leap-second-delta (time-second time)))]
         [is-leap-second (tm:leap-second? (+ offset seconds))])
    (receive (secs date month year)
        (if is-leap-second
          (tm:decode-julian-day-number (tm:time->julian-day-number (- seconds 1) offset))
          (tm:decode-julian-day-number (tm:time->julian-day-number seconds offset)))
      ;; adjust for leap seconds if necessary ...
      (let* ([hours    (quotient secs (* 60 60))]
             [rem      (remainder secs (* 60 60))]
             [minutes  (quotient rem 60)]
             [seconds  (remainder rem 60)])
        (make-date (time-nanosecond time)
                   (if is-leap-second (+ seconds 1) seconds)
                   minutes
                   hours
                   date
                   month
                   year
                   offset)))))

;; this is the same as time-tai->date.
(define (time-monotonic->date time :optional (offset (tm:local-tz-offset)))
  (tm:check-time-type time 'time-monotonic 'time-monotonic->date)
  (let* ([seconds (- (time-second time) (tm:leap-second-delta (time-second time)))]
         [is-leap-second (tm:leap-second? (+ offset seconds))])
    (receive (secs date month year)
        (if is-leap-second
          (tm:decode-julian-day-number (tm:time->julian-day-number (- seconds 1) offset))
          (tm:decode-julian-day-number (tm:time->julian-day-number seconds offset)))
      ;; adjust for leap seconds if necessary ...
      (let* ([hours    (quotient secs (* 60 60))]
             [rem      (remainder secs (* 60 60))]
             [minutes  (quotient rem 60)]
             [seconds  (remainder rem 60)])
        (make-date (time-nanosecond time)
                   (if is-leap-second (+ seconds 1) seconds)
                   minutes
                   hours
                   date
                   month
                   year
                   offset)))))

(define (date->time-utc date)
  (let ([nanosecond (date-nanosecond date)]
        [second (date-second date)]
        [minute (date-minute date)]
        [hour (date-hour date)]
        [day (date-day date)]
        [month (date-month date)]
        [year (date-year date)]
        [offset (date-zone-offset date)])
    (let1 jdays-1/2 (- (tm:encode-julian-day-number day month year)
                       (+ tm:tai-epoch-in-jd 1/2))
      (make-time time-utc
                 nanosecond
                 (+ (* 24 60 60 jdays-1/2)
                    (* 60 60 hour)
                    (* 60 minute)
                    second
                    (- offset))))))

(define (date->time-tai date)
  (time-utc->time-tai! (date->time-utc date)))

(define (date->time-monotonic date)
  (time-utc->time-monotonic! (date->time-utc date)))

(define (tm:leap-year? year)
  (or (= (modulo year 400) 0)
      (and (= (modulo year 4) 0) (not (= (modulo year 100) 0)))))

(define (leap-year? date)
  (tm:leap-year? (date-year date)))

(define  tm:month-assoc '((1 . 0)  (2 . 31)  (3 . 59)   (4 . 90)   (5 . 120)
                          (6 . 151) (7 . 181)  (8 . 212)  (9 . 243)
                          (10 . 273) (11 . 304) (12 . 334)))

(define (tm:year-day day month year)
  (let1 days-pr (assoc month tm:month-assoc)
    (when (not days-pr) (errorf "date-year-day: invalid month: ~a" month))
    (if (and (tm:leap-year? year) (> month 2))
        (+ day (cdr days-pr) 1)
        (+ day (cdr days-pr)))))

(define (date-year-day date)
  (tm:year-day (date-day date) (date-month date) (date-year date)))

;; from calendar faq
(define (tm:week-day day month year)
  (let* ([a (quotient (- 14 month) 12)]
         [y (- year a)]
         [m (+ month (* 12 a) -2)])
    (modulo (+ day y (quotient y 4) (- (quotient y 100))
               (quotient y 400) (quotient (* 31 m) 12))
            7)))

(define (date-week-day date)
  (tm:week-day (date-day date) (date-month date) (date-year date)))

(define (tm:days-before-first-week date day-of-week-starting-week)
  (let* ([first-day (make-date 0 0 0 0
                               1
                               1
                               (date-year date)
                               #f)]
         [fdweek-day (date-week-day first-day)])
    (modulo (- day-of-week-starting-week fdweek-day)
            7)))

(define (date-week-number date day-of-week-starting-week)
  (quotient (- (date-year-day date)
               (tm:days-before-first-week  date day-of-week-starting-week))
            7))

(define (current-date :optional (off (tm:local-tz-offset)))
  (time-utc->date (current-time time-utc) off))

;; given a 'two digit' number, find the year within 50 years +/-
(define (tm:natural-year n)
  (let* ([current-year (date-year (current-date))]
         [current-century (* (quotient current-year 100) 100)])
    (cond [(>= n 100) n]
          [(<  n 0) n]
          [(<=  (- (+ current-century n) current-year) 50)
           (+ current-century n)]
          [else (+ (- current-century 100) n)])))

(define (date->julian-day date)
  ;; NB: we scale everything execpt nanoseconds first, so that we can
  ;; avoid having intermediate results in ratnum.
  (/ (+ (* tm:sid tm:nano/i
           (tm:encode-julian-day-number (date-day date)
                                        (date-month date)
                                        (date-year date)))
        (+ (* tm:nano/i
              (+ (* 60 60 (date-hour date))
                 (* (date-minute date) 60)
                 (date-second date)
                 (- (date-zone-offset date))
                 (- tm:sihd)))
           (date-nanosecond date)))
     (* tm:sid tm:nano/i)))

(define (date->modified-julian-day date)
  (- (date->julian-day date) 4800001/2))

(define (time-utc->julian-day time)
  (tm:check-time-type time 'time-utc 'time-utc->julian-day)
  ;; NB: we scale first to avoid intermediate ratnum calculation.
  (/ (+ (time-nanosecond time)
        (* (time-second time) tm:nano/i)
        (* tm:tai-epoch-in-jd tm:sid tm:nano/i))
     (* tm:sid tm:nano/i)))

(define (time-utc->modified-julian-day time)
  (- (time-utc->julian-day time) 4800001/2))

(define (time-tai->julian-day time)
  (tm:check-time-type time 'time-tai 'time-tai->julian-day)
  ;; NB: we scale first to avoid intermediate ratnum calculation.
  (/ (+ (time-nanosecond time)
        (* (- (time-second time) (tm:leap-second-delta (time-second time)))
           tm:nano/i)
        (* tm:tai-epoch-in-jd tm:sid tm:nano/i))
     (* tm:sid tm:nano/i)))

(define (time-tai->modified-julian-day time)
  (- (time-tai->julian-day time) 4800001/2))

;; this is the same as time-tai->julian-day
(define (time-monotonic->julian-day time)
  (tm:check-time-type time 'time-monotonic 'time-monotonic->julian-day)
  ;; NB: we scale first to avoid intermediate ratnum calculation.
  (/ (+ (time-nanosecond time)
        (* (- (time-second time) (tm:leap-second-delta (time-second time)))
           tm:nano/i)
        (* tm:tai-epoch-in-jd tm:sid tm:nano/i))
     (* tm:sid tm:nano/i)))

(define (time-monotonic->modified-julian-day time)
  (- (time-monotonic->julian-day time) 4800001/2))

(define (julian-day->time-utc jdn)
  (let1 nanosecs (- (* tm:nano/i tm:sid jdn)
                    (* tm:nano/i tm:sid tm:tai-epoch-in-jd))
    (receive (q r) (quotient&remainder nanosecs tm:nano)
      (make-time time-utc r q))))

(define (julian-day->time-tai jdn)
  (time-utc->time-tai! (julian-day->time-utc jdn)))

(define (julian-day->time-monotonic jdn)
  (time-utc->time-monotonic! (julian-day->time-utc jdn)))

(define (julian-day->date jdn :optional (offset (tm:local-tz-offset)))
  (time-utc->date (julian-day->time-utc jdn) offset))

(define (modified-julian-day->date jdn :optional (offset (tm:local-tz-offset)))
  (julian-day->date (+ jdn 4800001/2) offset))

(define (modified-julian-day->time-utc jdn)
  (julian-day->time-utc (+ jdn 4800001/2)))

(define (modified-julian-day->time-tai jdn)
  (julian-day->time-tai (+ jdn 4800001/2)))

(define (modified-julian-day->time-monotonic jdn)
  (julian-day->time-monotonic (+ jdn 4800001/2)))

(define (current-julian-day)
  (time-utc->julian-day (current-time time-utc)))

(define (current-modified-julian-day)
  (time-utc->modified-julian-day (current-time time-utc)))

;; formatting stuff

(define tm:locale-number-separator ".")

(define tm:locale-abbr-weekday-vector
  '#("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
(define tm:locale-long-weekday-vector
  '#("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"))
;; note empty string in 0th place.
(define tm:locale-abbr-month-vector
  '#("" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul"
     "Aug" "Sep" "Oct" "Nov" "Dec"))
(define tm:locale-long-month-vector
  '#("" "January" "February" "March" "April" "May"
     "June" "July" "August" "September" "October" "November" "December"))

(define tm:locale-pm "PM")
(define tm:locale-am "AM")

;; See date->string
(define tm:locale-date-time-format "~a ~b ~d ~H:~M:~S~z ~Y")
(define tm:locale-short-date-format "~m/~d/~y")
(define tm:locale-time-format "~H:~M:~S")
(define tm:iso-8601-date-time-format "~Y-~m-~dT~H:~M:~S~z")

;; returns a string rep. of number N, of minimum LENGTH,
;; padded with character PAD-WITH. If PAD-WITH is #f,
;; no padding is done, and it's as if number->string was used.
;; if string is longer than LENGTH, it's as if number->string was used.

(define (tm:padding n pad-with length)
  (if pad-with
    (format #f "~v,vd" length pad-with n)
    (number->string n)))

(define (tm:last-n-digits i n)
  (abs (remainder i (expt 10 n))))

(define (tm:locale-abbr-weekday n)
  (vector-ref tm:locale-abbr-weekday-vector n))

(define (tm:locale-long-weekday n)
  (vector-ref tm:locale-long-weekday-vector n))

(define (tm:locale-abbr-month n)
  (vector-ref tm:locale-abbr-month-vector n))

(define (tm:locale-long-month n)
  (vector-ref tm:locale-long-month-vector n))

(define (tm:locale-abbr-weekday->index string)
  (find-index (cut string=? string <>) tm:locale-abbr-weekday-vector))

(define (tm:locale-long-weekday->index string)
  (find-index (cut string=? string <>) tm:locale-long-weekday-vector))

(define (tm:locale-abbr-month->index string)
  (find-index (cut string=? string <>) tm:locale-abbr-month-vector))

(define (tm:locale-long-month->index string)
  (find-index (cut string=? string <>) tm:locale-long-month-vector))

;; do nothing.
;; Your implementation might want to do something...
;;
(define (tm:locale-print-time-zone date)
  (values))

;; Again, locale specific.
(define (tm:locale-am/pm hr)
  (if (> hr 11) tm:locale-pm tm:locale-am))

(define (tm:tz-printer offset)
  (cond [(= offset 0) (display "Z")]
        [(negative? offset) (display "-")]
        [else (display "+")])
  (unless (zero? offset)
    (let ([hours   (abs (quotient offset (* 60 60)))]
          [minutes (abs (quotient (remainder offset (* 60 60)) 60))])
      (format #t "~2,'0d~2,'0d" hours minutes))))

;; A table of output formatting directives.
;; the first time is the format char.
;; the second is a procedure that takes the date, a padding character
;; (which might be #f), and the output port.
;;
(define tm:directives
  `((#\~ . ,(^[date pad-with] (display #\~)))
    (#\a . ,(^[date pad-with]
              (display (tm:locale-abbr-weekday (date-week-day date)))))
    (#\A . ,(^[date pad-with]
              (display (tm:locale-long-weekday (date-week-day date)))))
    (#\b . ,(^[date pad-with]
              (display (tm:locale-abbr-month (date-month date)))))
    (#\B . ,(^[date pad-with]
              (display (tm:locale-long-month (date-month date)))))
    (#\c . ,(^[date pad-with]
              (display (date->string date tm:locale-date-time-format))))
    (#\d . ,(^[date pad-with]
              (format #t "~2,'0d" (date-day date))))
    (#\D . ,(^[date pad-with]
              (display (date->string date "~m/~d/~y"))))
    (#\e . ,(^[date pad-with]
              (format #t "~2,' d" (date-day date))))
    (#\f . ,(^[date pad-with]
              (display (tm:padding (date-second date) pad-with 2))
              (display tm:locale-number-separator)
              (let1 nanostr (number->string (/. (date-nanosecond date) tm:nano/i))
                (cond [(string-index nanostr #\.)
                       => (^i (display (string-drop nanostr (+ i 1))))]))))
    (#\h . ,(^[date pad-with]
              (display (date->string date "~b"))))
    (#\H . ,(^[date pad-with]
              (display (tm:padding (date-hour date) pad-with 2))))
    (#\I . ,(^[date pad-with]
              (let1 hr (date-hour date)
                (if (> hr 12)
                  (display (tm:padding (- hr 12) pad-with 2))
                  (display (tm:padding hr pad-with 2))))))
    (#\j . ,(^[date pad-with]
              (display (tm:padding (date-year-day date) pad-with 3))))
    (#\k . ,(^[date pad-with]
              (format #t "~2,' d" (date-hour date))))
    (#\l . ,(^[date pad-with]
              (let1 hr (if (> (date-hour date) 12)
                         (- (date-hour date) 12)
                         (date-hour date))
                (format #t "~2,' d" hr))))
    (#\m . ,(^[date pad-with]
              (display (tm:padding (date-month date) pad-with 2))))
    (#\M . ,(^[date pad-with]
              (display (tm:padding (date-minute date) pad-with 2))))
    (#\n . ,(^[date pad-with] (newline)))
    (#\N . ,(^[date pad-with]
              (display (tm:padding (date-nanosecond date) pad-with 9))))
    (#\p . ,(^[date pad-with]
              (display (tm:locale-am/pm (date-hour date)))))
    (#\r . ,(^[date pad-with]
              (display (date->string date "~I:~M:~S ~p"))))
    (#\s . ,(^[date pad-with]
              (display (time-second (date->time-utc date)))))
    (#\S . ,(^[date pad-with]
              (display (tm:padding (date-second date) pad-with 2))))
    (#\t . ,(^[date pad-with]
              (display #\tab)))
    (#\T . ,(^[date pad-with]
              (display (date->string date "~H:~M:~S"))))
    (#\U . ,(^[date pad-with]
              (format #t "~2,'0d"
                      (if (> (tm:days-before-first-week date 0) 0)
                        (+ (date-week-number date 0) 1)
                        (date-week-number date 0)))))
    (#\V . ,(^[date pad-with]
              (format #t "~2,'0d" (date-week-number date 1))))
    (#\w . ,(^[date pad-with]
              (display (date-week-day date))))
    (#\x . ,(^[date pad-with]
              (display (date->string date tm:locale-short-date-format))))
    (#\X . ,(^[date pad-with]
              (display (date->string date tm:locale-time-format))))
    (#\W . ,(^[date pad-with]
              (format #t "~2,'0d"
                      (if (> (tm:days-before-first-week date 1) 0)
                        (+ (date-week-number date 1) 1)
                        (date-week-number date 1)))))
    (#\y . ,(^[date pad-with]
              (display (tm:padding (tm:last-n-digits (date-year date) 2) pad-with 2))))
    (#\Y . ,(^[date pad-with]
              (display (date-year date))))
    (#\z . ,(^[date pad-with]
              (tm:tz-printer (date-zone-offset date))))
    (#\Z . ,(^[date pad-with]
              (tm:locale-print-time-zone date)))
    (#\1 . ,(^[date pad-with]
              (display (date->string date "~Y-~m-~d"))))
    (#\2 . ,(^[date pad-with]
              (display (date->string date "~H:~M:~S~z"))))
    (#\3 . ,(^[date pad-with]
              (display (date->string date "~H:~M:~S"))))
    (#\4 . ,(^[date pad-with]
              (display (date->string date "~Y-~m-~dT~H:~M:~S~z"))))
    (#\5 . ,(^[date pad-with]
              (display (date->string date "~Y-~m-~dT~H:~M:~S"))))
    ))

(define (date->string date . maybe-fmtstr)
  (let1 format-string (get-optional maybe-fmtstr "~c")
    (with-input-from-string format-string
      (^[]
        (define (bad i)
          (errorf "date->string: bad date format string: \"~a >>>~a<<< ~a\""
                  (string-take format-string i)
                  (substring format-string i (+ i 1))
                  (string-drop format-string (+ i 1))))

        (define (call-formatter ch pad ind)
          (if-let1 fn (assv ch tm:directives)
            (begin ((cdr fn) date pad) (rec (read-char) (+ ind 1)))
            (bad ind)))

        (define (rec ch ind)
          (cond
           [(eof-object? ch)]
           [(not (char=? ch #\~))
            (write-char ch) (rec (read-char) (+ ind 1))]
           [else
            (let1 ch2 (read-char)
              (cond
               [(eof-object? ch2) (write-char ch)]
               ;; Gauche extension: ~@x calls the directive 'x' with locale
               ;; set to C, so the caller can guarantee the output.  Currently
               ;; the library only supports the default locale, so we can simply
               ;; ignore '@'.  In future we'll add locale-sensitive stuff.
               [(char=? ch2 #\@)
                (call-formatter (read-char) #f (+ ind 2))]
               [else (call-formatter ch2 #\0 (+ ind 1))]))]))

        ;; body
        (with-output-to-string
          (cut rec (read-char) 0))))))

(define (tm:char->int ch)
  (or (digit->integer ch)
      (errorf "bad date template string: non integer character: ~a" ch)))

;; read an integer upto n characters long on port; upto -> #f if any length
(define (tm:integer-reader upto port)
  (define (accum-int port accum nchars)
    (let1 ch (peek-char port)
      (if (or (eof-object? ch)
              (not (char-numeric? ch))
              (and upto (>= nchars  upto )))
        accum
        (accum-int port
                   (+ (* accum 10) (tm:char->int (read-char port)))
                   (+ nchars 1)))))
  (accum-int port 0 0))

(define (tm:make-integer-reader upto)
  (^[port] (tm:integer-reader upto port)))

;; read *exactly* n characters and convert to integer; could be padded
(define (tm:integer-reader-exact n port)
  (let1 padding-ok #t
    (define (accum-int port accum nchars)
      (let1 ch (peek-char port)
        (cond
         [(>= nchars n) accum]
         [(eof-object? ch)
          (error "string->date: premature ending of integer read")]
         [(char-numeric? ch)
          (set! padding-ok #f)
          (accum-int port
                     (+ (* accum 10) (tm:char->int (read-char port)))
                     (+ nchars 1))]
         [padding-ok
          (read-char port) ; consume padding
          (accum-int port accum (+ nchars 1))]
         [else ; padding where it shouldn't be
          (error "string->date: Non-numeric characters in integer read.")]
         )))
    (accum-int port 0 0)))

(define (tm:make-integer-exact-reader n)
  (^[port] (tm:integer-reader-exact n port)))

(define (tm:zone-reader port)
  (let ([offset 0]
        [positive? #f])
    (let1 ch (read-char port)
      (when (eof-object? ch)
        (errorf "string->date: invalid time zone +/-: ~s" ch))
      (if (or (char=? ch #\Z) (char=? ch #\z))
        0
        (begin
          (cond
           [(char=? ch #\+) (set! positive? #t)]
           [(char=? ch #\-) (set! positive? #f)]
           [else (errorf "string->date: invalid time zone +/-: ~s" ch)])
          (let1 ch (read-char port)
            (when (eof-object? ch)
              (error "string->date: premature end of time zone number"))
            (set! offset (* (tm:char->int ch)
                            10 60 60)))
          (let1 ch (read-char port)
            (when (eof-object? ch)
              (error "string->date: premature end of time zone number"))
            (set! offset (+ offset (* (tm:char->int ch)
                                      60 60))))
          (let1 ch (read-char port)
            (when (eof-object? ch)
              (error "string->date: premature end of time zone number"))
            (set! offset (+ offset (* (tm:char->int ch)
                                      10 60))))
          (let1 ch (read-char port)
            (when (eof-object? ch)
              (error "string->date: premature end of time zone number"))
            (set! offset (+ offset (* (tm:char->int ch)
                                      60))))
          (if positive? offset (- offset)))))))

;; looking at a char, read the char string, run thru indexer, return index
(define (tm:locale-reader port indexer)
  (let1 string-port (open-output-string)
    (define (read-char-string)
      (let1 ch (peek-char port)
        (if (char-alphabetic? ch)
          (begin (write-char (read-char port) string-port)
                 (read-char-string))
          (get-output-string string-port))))
    (let* ([str (read-char-string)]
           [index (indexer str)])
      (or index
          (errorf "string->date: invalid string for ~s" indexer)))))

(define (tm:make-locale-reader indexer)
  (^[port] (tm:locale-reader port indexer)))

(define (tm:make-char-id-reader char)
  (^[port] (if (char=? char (read-char port))
             char
             (error "string->date: invalid character match"))))

;; A List of formatted read directives.
;; Each entry is a list.
;; 1. the character directive;
;; a procedure, which takes a character as input & returns
;; 2. #t as soon as a character on the input port is acceptable
;; for input,
;; 3. a port reader procedure that knows how to read the current port
;; for a value. Its one parameter is the port.
;; 4. a action procedure, that takes the value (from 3.) and some
;; object (here, always the date) and (probably) side-effects it.
;; In some cases (e.g., ~A) the action is to do nothing

(define tm:read-directives
  (let ([ireader4 (tm:make-integer-reader 4)]
        [ireader2 (tm:make-integer-reader 2)]
        [ireaderf (tm:make-integer-reader #f)]
        [eireader2 (tm:make-integer-exact-reader 2)]
        [eireader4 (tm:make-integer-exact-reader 4)]
        [locale-reader-abbr-weekday (tm:make-locale-reader
                                     tm:locale-abbr-weekday->index)]
        [locale-reader-long-weekday (tm:make-locale-reader
                                     tm:locale-long-weekday->index)]
        [locale-reader-abbr-month   (tm:make-locale-reader
                                     tm:locale-abbr-month->index)]
        [locale-reader-long-month   (tm:make-locale-reader
                                     tm:locale-long-month->index)]
        [char-fail (^_ #t)]
        [do-nothing (^[val object] (values))])

  (list
   (list #\~ char-fail (tm:make-char-id-reader #\~) do-nothing)
   (list #\a char-alphabetic? locale-reader-abbr-weekday do-nothing)
   (list #\A char-alphabetic? locale-reader-long-weekday do-nothing)
   (list #\b char-alphabetic? locale-reader-abbr-month
         (^[val object] (slot-set! object 'month val)))
   (list #\B char-alphabetic? locale-reader-long-month
         (^[val object] (slot-set! object 'month val)))
   (list #\d char-numeric? ireader2
         (^[val object] (slot-set! object 'day val)))
   (list #\e char-fail eireader2
         (^[val object] (slot-set! object 'day val)))
   (list #\h char-alphabetic? locale-reader-abbr-month
         (^[val object] (slot-set! object 'month val)))
   (list #\H char-numeric? ireader2
         (^[val object] (slot-set! object 'hour val)))
   (list #\k char-fail eireader2
         (^[val object] (slot-set! object 'hour val)))
   (list #\m char-numeric? ireader2
         (^[val object] (slot-set! object 'month val)))
   (list #\M char-numeric? ireader2
         (^[val object] (slot-set! object 'minute val)))
   (list #\S char-numeric? ireader2
         (^[val object] (slot-set! object 'second val)))
   (list #\y char-fail eireader2
         (^[val object] (slot-set! object 'year (tm:natural-year val))))
   (list #\Y char-numeric? ireader4
         (^[val object] (slot-set! object 'year val)))
   (list #\z (^c (or (char=? c #\Z)
                     (char=? c #\z)
                     (char=? c #\+)
                     (char=? c #\-)))
         tm:zone-reader (^[val object] (slot-set! object 'zone-offset val)))
   )))

(define (tm:string->date date index format-string str-len port template-string)
  (define (bad)
    (errorf "string->date: bad date format string: \"~a >>>~a<<< ~a\""
            (string-take template-string index)
            (substring template-string index (+ index 1))
            (string-drop template-string (+ index 1))))
  (define (skip-until port skipper)
    (let1 ch (peek-char port)
      (when (eof-object? port) (bad))
      (unless (skipper ch)
        (read-char port) (skip-until port skipper))))
  (if (>= index str-len)
    (values)
    (let1 current-char (string-ref format-string index)
      (if (not (char=? current-char #\~))
        (let1 port-char (read-char port)
          (when (or (eof-object? port-char)
                  (not (char=? current-char port-char)))
            (bad))
          (tm:string->date date (+ index 1) format-string str-len port template-string))
        ;; otherwise, it's an escape, we hope
        (if (> (+ index 1) str-len)
          (bad)
          (let* ([format-char (string-ref format-string (+ index 1))]
                 [format-info (assoc format-char tm:read-directives)])
            (when (not format-info) (bad))
            (let ([skipper (cadr format-info)]
                  [reader  (caddr format-info)]
                  [actor   (cadddr format-info)])
              (skip-until port skipper)
              (let1 val (reader port)
                (when (eof-object? val) (bad))
                (actor val date))
              (tm:string->date date (+ index 2) format-string  str-len port template-string))))))))

(define (string->date input-string template-string)
  (define (tm:date-ok? date)
    (and (date-nanosecond date)
         (date-second date)
         (date-minute date)
         (date-hour date)
         (date-day date)
         (date-month date)
         (date-year date)
         (date-zone-offset date)))
  (let1 newdate (make-date 0 0 0 0 #f #f #f (tm:local-tz-offset))
    (tm:string->date newdate
                     0
                     template-string
                     (string-length template-string)
                     (open-input-string input-string)
                     template-string)
    (if (tm:date-ok? newdate)
      newdate
      (errorf "string->date: incomplete date read: ~s for ~s"
              newdate template-string))))

;; A table of leap seconds
;; See ftp://maia.usno.navy.mil/ser7/tai-utc.dat
;; and update as necessary.
;; this procedures reads the file in the abover
;; format and creates the leap second table
;; it also calls the almost standard, but not R5 procedures read-line
;; & open-input-string
;; ie (set! tm:leap-second-table (tm:read-tai-utc-data "tai-utc.dat"))

(define (tm:read-tai-utc-data filename)
  (define (convert-jd jd)
    (* (- (inexact->exact jd) tm:tai-epoch-in-jd) tm:sid))
  (define (convert-sec sec)
    (inexact->exact sec))
  (let ([port (open-input-file filename)]
        [table '()])
    (let loop ([line (read-line port)])
      (unless (eof-object? line)
        (let* ([data (read (open-input-string (string-append "(" line ")")))]
               [year (car data)]
               [jd   (cadddr (cdr data))]
               [secs (cadddr (cdddr data))])
          (when (>= year 1972)
            (set! table (cons (cons (convert-jd jd) (convert-sec secs)) table)))
          (loop (read-line port)))))
    table))

(define (read-leap-second-table filename)
  (set! tm:leap-second-table (tm:read-tai-utc-data filename))
  (values))

