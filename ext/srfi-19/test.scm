;;
;; testing srfi-19
;;

(use gauche.test)

(test-start "srfi-19")
(use srfi-19)
(test-module 'srfi-19)

(test* "make-time" '(#t time-utc 100 5555)
       (let1 t (make-time time-utc 5555 100)
         (list (time? t) (time-type t) (time-second t)
               (time-nanosecond t))))
(test* "copy-time" '(#t time-utc 100 5555)
       (let1 t (copy-time (make-time time-utc 5555 100))
         (list (time? t) (time-type t) (time-second t)
               (time-nanosecond t))))
(test* "current-time" '(time-utc time-tai time-monotonic)
       (map time-type (list (current-time)
                            (current-time time-tai)
                            (current-time time-monotonic))))
(test* "time comparison"
       '(#t #f #f
            #t #f #t #f #f 
            #t #f #t #f #t
            #f #t #f #t #f
            #f #t #f #t #t)
       (let ((t0 (make-time time-tai 345676543 23456))
             (t1 (make-time time-tai 293892851 93853))
             (t2 (make-time time-tai 893892851 93853)))
         (list (time=?  t0 t0)
               (time=?  t0 t1)
               (time=?  t1 t2)
               (time<?  t0 t1)
               (time<?  t1 t0)
               (time<?  t1 t2)
               (time<?  t2 t1)
               (time<?  t1 t1)
               (time<=? t0 t1)
               (time<=? t1 t0)
               (time<=? t1 t2)
               (time<=? t2 t1)
               (time<=? t0 t0)
               (time>?  t0 t1)
               (time>?  t1 t0)
               (time>?  t1 t2)
               (time>?  t2 t1)
               (time>?  t0 t0)
               (time>=? t0 t1)
               (time>=? t1 t0)
               (time>=? t1 t2)
               (time>=? t2 t1)
               (time>=? t0 t0))))
(test* "time difference" '(#t #t #t #t #t #t)
       (let* ((t0 (current-time))
              (t1 (make-time time-utc 333333333 1000000000))
              (dt (time-difference t1 t0))
              (r0 (eq? (time-type dt) time-duration))
              (t2 (add-duration t0 dt))
              (r1 (eq? (time-type t2) (time-type t0)))
              (r2 (time=? t1 t2))
              (r3 (begin (subtract-duration! t2 dt)
                         (time=? t2 t0)))
              (r4 (begin (add-duration! t0 dt)
                         (time=? t1 t0)))
              (r5 (begin (time-difference! t0 t2)
                         (time=? t0 dt))))
         (list r0 r1 r2 r3 r4 r5)))
(test* "time conversion" '(#t #t)
       (let* ((t0 (current-time))
              (ta (time-utc->time-tai t0))
              (tb (time-tai->time-utc ta))
              (r0 (time=? t0 tb))
              (r1 (time=? ta (begin (time-utc->time-tai! t0) t0))))
         (list r0 r1)))
(test* "time conversion" '(#t #t)
       (let* ((t0 (current-time))
              (ta (time-utc->time-monotonic t0))
              (tb (time-monotonic->time-utc ta))
              (r0 (time=? t0 tb))
              (r1 (time=? ta (begin (time-utc->time-monotonic! t0) t0))))
         (list r0 r1)))
(let ((now (current-time)))
  (test* "make-date"
         (let1 d1 (sys-localtime (time-second now))
           (list (+ (slot-ref d1 'year) 1900)
                 (+ (slot-ref d1 'mon) 1)
                 (slot-ref d1 'mday)
                 (slot-ref d1 'hour)
                 (slot-ref d1 'min)
                 (slot-ref d1 'sec)
                 (time-nanosecond now)
                 (+ (slot-ref d1 'yday) 1)
                 (slot-ref d1 'wday)))
         (let1 d0  (time-utc->date now)
           (list (date-year d0) 
                 (date-month d0)
                 (date-day d0)
                 (date-hour d0)
                 (date-minute d0)
                 (date-second d0)
                 (date-nanosecond d0)
                 (date-year-day d0)
                 (date-week-day d0)))
         ))
(test* "date conversion"
       '(#t #t #t #t)
       (let* ((t0 (make-time 'time-utc 0 0))
              (t1 (make-time 'time-utc 48375295 1022191954))
              (t2 (make-time 'time-tai 0 0))
              (t3 (make-time 'time-tai 48375295 1022191954)))
         (list (time=? t0 (date->time-utc (time-utc->date t0)))
               (time=? t1 (date->time-utc (time-utc->date t1)))
               (time=? t2 (date->time-tai (time-tai->date t2)))
               (time=? t3 (date->time-tai (time-tai->date t3)))
               )))

;; NB: in Gauche, the round-trip conversion from time -> julian-day -> time
;; can't be guaranteed because of the limited precision of julian-day
;; calcularion.   We round the nanosecond range.
(define (round-to-seconds time)
  (let ((n (time-nanosecond time)))
    (set! (ref time 'nanosecond) 0)
    (when (> n 500000000)
      (add-duration! time (make-time 'time-duration 0 1)))
    time))
  
(let1 t0 (make-time time-utc 0 1022191954)
  (test "julian day number, via time-utc"
        t0
        (lambda ()
          (round-to-seconds (julian-day->time-utc (time-utc->julian-day t0))))
        time=?))
(let1 jd 2453311.0
  (test "julian day number, via date"
        jd
        (lambda ()
          (date->julian-day (julian-day->date jd)))))
(let1 t0 (make-time time-utc 0 1022191954)
  (test "modified julian day number"
        t0
        (lambda ()
          (round-to-seconds
           (modified-julian-day->time-utc (time-utc->modified-julian-day t0))))
        time=?))


(test* "date->string"
       "2002/05/15 01:23:34.001234567 -1000 3"
       (date->string (make-date 1234567 34 23 1 15 5 2002 -36000)
                     "~Y/~m/~d ~H:~M:~S.~N ~z ~w"))

(test* "date->string"
       "02/05/ 1| 2|14"
       (date->string (make-date 1234567 4 3 14 1 5 2002 -36000)
                     "~y/~m/~e|~l|~k"))

(test* "string->date"
       '(2002 5 15 12 34 56 -36000)
       (let1 d (string->date "2002/5/15 12:34:56 (-1000)"
                             "~Y/~m/~d ~H:~M:~S (~z)")
         (map (lambda (s) (slot-ref d s))
              '(year month day hour minute second zone-offset))))

;; NB: this test will fail when locale-dependent date name is supported.
(test* "string->date"
       '(2002 11 2 7 14 11 32400)
       (let1 d (string->date "02/Nov/2002:07:14:11 +0900"
                             "~d~b~Y~H~M~S~z")
         (map (lambda (s) (slot-ref d s))
              '(year month day hour minute second zone-offset))))

(test-end)
