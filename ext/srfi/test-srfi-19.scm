;;;
;;; testing SRFI-19
;;;

(use gauche.test)
(test-start "SRFI-19")
(test-section "SRFI-19")

(define-module srfi-19-tests
  (use gauche.test)
  (use srfi.19)
  (test-module 'srfi.19)

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
         (let ([t0 (make-time time-tai 345676543 23456)]
               [t1 (make-time time-tai 293892851 93853)]
               [t2 (make-time time-tai 893892851 93853)])
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
         (let* ([t0 (current-time)]
                [t1 (make-time time-utc 333333333 1000000000)]
                [dt (time-difference t1 t0)]
                [r0 (eq? (time-type dt) time-duration)]
                [t2 (add-duration t0 dt)]
                [r1 (eq? (time-type t2) (time-type t0))]
                [r2 (time=? t1 t2)]
                [r3 (begin (subtract-duration! t2 dt)
                           (time=? t2 t0))]
                [r4 (begin (add-duration! t0 dt)
                           (time=? t1 t0))]
                [r5 (begin (time-difference! t0 t2)
                           (time=? t0 dt))])
           (list r0 r1 r2 r3 r4 r5)))
  (test* "time conversion" '(#t #t)
         (let* ([t0 (current-time)]
                [ta (time-utc->time-tai t0)]
                [tb (time-tai->time-utc ta)]
                [r0 (time=? t0 tb)]
                [r1 (time=? ta (begin (time-utc->time-tai! t0) t0))])
           (list r0 r1)))
  (test* "time conversion" '(#t #t)
         (let* ([t0 (current-time)]
                [ta (time-utc->time-monotonic t0)]
                [tb (time-monotonic->time-utc ta)]
                [r0 (time=? t0 tb)]
                [r1 (time=? ta (begin (time-utc->time-monotonic! t0) t0))])
           (list r0 r1)))
  (let ([now (current-time)])
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
         (let* ([t0 (make-time 'time-utc 0 0)]
                [t1 (make-time 'time-utc 48375295 1022191954)]
                [t2 (make-time 'time-tai 0 0)]
                [t3 (make-time 'time-tai 48375295 1022191954)])
           (list (time=? t0 (date->time-utc (time-utc->date t0)))
                 (time=? t1 (date->time-utc (time-utc->date t1)))
                 (time=? t2 (date->time-tai (time-tai->date t2)))
                 (time=? t3 (date->time-tai (time-tai->date t3)))
                 )))

  (test* "date past 2038/1/19" (date->time-utc (make-date 0 0 0 0 1 1 3000 0))
         (julian-day->time-utc
          (+ (date->julian-day (make-date 0 0 0 0 1 1 2000 0))
             (+ (* 365 1000) 243))))

  ;; NB: in Gauche, the round-trip conversion from time -> julian-day -> time
  ;; can't be guaranteed because of the limited precision of julian-day
  ;; calculation.   We round the nanosecond range.
  (define (round-to-seconds time)
    (let1 n (time-nanosecond time)
      (set! (ref time 'nanosecond) 0)
      (when (> n 500000000)
        (add-duration! time (make-time 'time-duration 0 1)))
      time))

  (let1 t0 (make-time time-utc 0 1022191954)
    (test "julian day number, via time-utc"
          t0
          (^[] ($ round-to-seconds
                  $ julian-day->time-utc $ time-utc->julian-day t0))
          time=?))
  (let1 jd 2453311
    (test "julian day number, via date"
          jd
          (^[] (date->julian-day (julian-day->date jd)))
          =))
  (let1 t0 (make-time time-utc 0 1022191954)
    (test "modified julian day number"
          t0
          (^[] ($ round-to-seconds $ modified-julian-day->time-utc
                  $ time-utc->modified-julian-day t0))
          time=?))


  (test* "date->string"
         "2002/05/15 01:23:34.001234567 -1000 3"
         (date->string (make-date 1234567 34 23 1 15 5 2002 -36000)
                       "~Y/~m/~d ~H:~M:~S.~N ~z ~w"))
  (test* "date->string (past 2038)"
         "2100/05/15 01:23:34.001234567 -1000 6"
         (date->string
          (time-utc->date
           (date->time-utc
            (make-date 1234567 34 23 1 15 5 2100 -36000)) -36000)
          "~Y/~m/~d ~H:~M:~S.~N ~z ~w"))

  (test* "date->string"
         "02/05/ 1| 2|14"
         (date->string (make-date 1234567 4 3 14 1 5 2002 -36000)
                       "~y/~m/~e|~l|~k"))

  (test* "string->date"
         '(2002 5 15 12 34 56 -36000)
         (let1 d (string->date "2002/5/15 12:34:56 (-1000)"
                               "~Y/~m/~d ~H:~M:~S (~z)")
           (map (cut slot-ref d <>)
                '(year month day hour minute second zone-offset))))

  ;; NB: this test will fail when locale-dependent date name is supported.
  (test* "string->date"
         '(2002 11 2 7 14 11 32400)
         (let1 d (string->date "02/Nov/2002:07:14:11 +0900"
                               "~d~b~Y~H~M~S~z")
           (map (cut slot-ref d <>)
                '(year month day hour minute second zone-offset))))

  ;; ISO week number ~V should be the same as strftime %V
  ;; NB: MinGW's strftime doesn't support %V yet.  Since the Scheme code is
  ;; portable, we just skip the tests for now.
  ;; Cf. https://sourceforge.net/p/mingw-w64/bugs/793/
  (cond-expand
   [gauche.os.windows]
   [else
    (let ((dates '((2021 1 1) ; Friday, week 53
                   (2021 1 3) ; Sunday, week 53
                   (2021 1 4) ; Monday, week 1
                   (2020 12 31) ; Thursday, week 53

                   (2020 1 1) ; Wednesday, week 1
                   (2019 12 31) ; Tuesday, week 1
                   (2019 12 30) ; Monday, week 1
                   (2019 12 29) ; Sunday, week 52

                   (2017 1 1) ; Sunday, week 52
                   (2017 1 2) ; Monday, week 1
                   (2017 1 8) ; Sunday, week 1
                   (2017 1 9) ; Monday, week 2
                   (2016 12 31) ; Saturday, week 52

                   (2015 1 1) ; Thursday, week 1
                   (2015 1 2) ; Friday, week 1
                   (2015 1 3) ; Saturday, week 1
                   (2015 1 4) ; Sunday, week 1
                   (2015 1 5) ; Monday, week 2
                   (2014 12 31) ; Wednesday, week 1
                   (2014 12 30) ; Tuesday, week 1
                   (2014 12 29) ; Monday, week 1
                   (2014 12 28) ; Sunday, week 52
                   )))
      (dolist [date dates]
        (let1 d (make-date 0 0 0 0 (caddr date) (cadr date) (car date) 0)
          (test* (apply format "~d/~2,'0d/~2,'0d" date)
                 (sys-strftime "%V" (sys-gmtime (date->time-utc d)))
                 (date->string d "~V")))))
    ]) ; cond-expand

  ;; object-compare for dates.
  (let ((d1 (make-date 0 0 0 0 1 1 2022 0))
        (d2 (make-date 0 0 0 0 2 1 2022 0))
        (d3 (make-date 0 0 0 0 2 1 2022 -36000)))
    (define data
      `([,d1 ,d2 -1]
        [,d2 ,d2 0]
        [,d2 ,d1 1]
        [,d1 ,d3 -1]
        [,d2 ,d3 -1]))
    (dolist [dd data]
      (test* #"comparing dates ~(car dd) ~(cadr dd)" (caddr dd)
             (compare (car dd) (cadr dd))))
    )
  )

(test-end)
