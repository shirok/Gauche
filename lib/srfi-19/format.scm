;;; srfi-19/format.scm - excerpt from SRFI-19 for date formatting routine.
;;; $Id: format.scm,v 1.7 2003-02-26 22:09:31 shirok Exp $

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

(select-module srfi-19)
(use gauche.sequence)
(use srfi-13)
(use gauche.collection)
(use util.list)

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
  (find-index (cut string=? string <>) tm:locale-long-month-vector string=?))

;; do nothing. 
;; Your implementation might want to do something...
;; 
(define (tm:locale-print-time-zone date)
  (values))

;; Again, locale specific.
(define (tm:locale-am/pm hr)
  (if (> hr 11) tm:locale-pm tm:locale-am))

(define (tm:tz-printer offset)
  (cond
   ((= offset 0) (display "Z"))
   ((negative? offset) (display "-"))
   (else (display "+")))
  (unless (zero? offset)
    (let ((hours   (abs (quotient offset (* 60 60))))
          (minutes (abs (quotient (remainder offset (* 60 60)) 60))) )
      (format #t "~2,'0d~2,'0d" hours minutes))))

;; A table of output formatting directives.
;; the first time is the format char.
;; the second is a procedure that takes the date, a padding character
;; (which might be #f), and the output port.
;;
(define tm:directives 
  `((#\~ . ,(lambda (date pad-with) (display #\~)))
    (#\a . ,(lambda (date pad-with)
              (display (tm:locale-abbr-weekday (date-week-day date)))))
    (#\A . ,(lambda (date pad-with)
              (display (tm:locale-long-weekday (date-week-day date)))))
    (#\b . ,(lambda (date pad-with)
              (display (tm:locale-abbr-month (date-month date)))))
    (#\B . ,(lambda (date pad-with)
              (display (tm:locale-long-month (date-month date)))))
    (#\c . ,(lambda (date pad-with)
              (display (date->string date tm:locale-date-time-format))))
    (#\d . ,(lambda (date pad-with)
              (format #t "~2,'0d" (date-day date))))
    (#\D . ,(lambda (date pad-with)
              (display (date->string date "~m/~d/~y"))))
    (#\e . ,(lambda (date pad-with)
              (format #t "~2,' d" (date-day date))))
    (#\f . ,(lambda (date pad-with)
              (display (tm:padding (date-second date) pad-with 2))
              (display tm:locale-number-separator)
              (let1 nanostr (number->string (/ (date-nanosecond date) tm:nano))
                (cond ((string-index nanostr #\.)
                       => (lambda (i) (display (string-drop nanostr (+ i 1)))))
                      ))))
    (#\h . ,(lambda (date pad-with)
              (display (date->string date "~b"))))
    (#\H . ,(lambda (date pad-with)
              (display (tm:padding (date-hour date) pad-with 2))))
    (#\I . ,(lambda (date pad-with)
              (let ((hr (date-hour date)))
                (if (> hr 12)
                    (display (tm:padding (- hr 12) pad-with 2))
                    (display (tm:padding hr pad-with 2))))))
    (#\j . ,(lambda (date pad-with)
              (display (tm:padding (date-year-day date) pad-with 3))))
    (#\k . ,(lambda (date pad-with)
              (format #t "~2,' d" (date-hour date))))
    (#\l . ,(lambda (date pad-with)
              (let ((hr (if (> (date-hour date) 12)
                            (- (date-hour date) 12)
                            (date-hour date))))
                (format #t "~2,' d" hr))))
    (#\m . ,(lambda (date pad-with)
              (display (tm:padding (date-month date) pad-with 2))))
    (#\M . ,(lambda (date pad-with)
              (display (tm:padding (date-minute date) pad-with 2))))
    (#\n . ,(lambda (date pad-with) (newline)))
    (#\N . ,(lambda (date pad-with)
              (display (tm:padding (date-nanosecond date) pad-with 9))))
    (#\p . ,(lambda (date pad-with)
              (display (tm:locale-am/pm (date-hour date)))))
    (#\r . ,(lambda (date pad-with)
              (display (date->string date "~I:~M:~S ~p"))))
    (#\s . ,(lambda (date pad-with)
              (display (time-second (date->time-utc date)))))
    (#\S . ,(lambda (date pad-with)
              (display (tm:padding (date-second date) pad-with 2))))
    (#\t . ,(lambda (date pad-with)
              (display #\tab)))
    (#\T . ,(lambda (date pad-with)
              (display (date->string date "~H:~M:~S"))))
    (#\U . ,(lambda (date pad-with)
              (format #t "~2,'0d"
                      (if (> (tm:days-before-first-week date 0) 0)
                          (+ (date-week-number date 0) 1)
                          (date-week-number date 0)))))
    (#\V . ,(lambda (date pad-with)
              (format #t "~2,'0d" (date-week-number date 1))))
    (#\w . ,(lambda (date pad-with)
              (display (date-week-day date))))
    (#\x . ,(lambda (date pad-with)
              (display (date->string date tm:locale-short-date-format))))
    (#\X . ,(lambda (date pad-with)
              (display (date->string date tm:locale-time-format))))
    (#\W . ,(lambda (date pad-with)
              (format #t "~2,'0d"
                      (if (> (tm:days-before-first-week date 1) 0)
                          (+ (date-week-number date 1) 1)
                          (date-week-number date 1)))))
    (#\y . ,(lambda (date pad-with)
              (display (tm:padding (tm:last-n-digits (date-year date) 2) pad-with 2))))
    (#\Y . ,(lambda (date pad-with)
              (display (date-year date))))
    (#\z . ,(lambda (date pad-with)
              (tm:tz-printer (date-zone-offset date))))
    (#\Z . ,(lambda (date pad-with)
              (tm:locale-print-time-zone date)))
    (#\1 . ,(lambda (date pad-with)
              (display (date->string date "~Y-~m-~d"))))
    (#\2 . ,(lambda (date pad-with)
              (display (date->string date "~k:~M:~S~z"))))
    (#\3 . ,(lambda (date pad-with)
              (display (date->string date "~k:~M:~S"))))
    (#\4 . ,(lambda (date pad-with)
              (display (date->string date "~Y-~m-~dT~k:~M:~S~z"))))
    (#\5 . ,(lambda (date pad-with)
              (display (date->string date "~Y-~m-~dT~k:~M:~S"))))
    ))

(define (tm:get-formatter char)
  (let ( (associated (assoc char tm:directives)) )
    (if associated (cdr associated) #f)))

(define (date->string date . maybe-fmtstr)

  (define (bad i)
    (errorf "date->string: bad date format string: \"~a >>>~a<<< ~a\""
            (string-take format-string i)
            (substring format-string i (+ i 1))
            (string-drop format-string (+ i 1))))

  (define (call-formatter ch pad ind)
    (cond ((assv ch tm:directives) =>
           (lambda (fn) ((cdr fn) date pad) (rec (read-char) (+ ind 1))))
          (else (bad ind))))
  
  (define (rec ch ind)
    (cond
     ((eof-object? ch))
     ((not (char=? ch #\~))
      (write-char ch) (rec (read-char) (+ ind 1)))
     (else
      (let1 ch2 (read-char)
        (cond
         ((eof-object? ch2) (write-char ch))
         ((char=? ch2 #\-)
          (call-formatter (read-char) #f (+ ind 2)))
         ((char=? ch2 #\_)
          (call-formatter (read-char) #\space (+ ind 2)))
         (else
          (call-formatter ch2 #\0 (+ ind 1))))))
     ))

  ;; body
  (with-input-from-string (get-optional maybe-fmtstr "~c")
    (lambda ()
      (with-output-to-string
        (cut rec (read-char) 0))))
  )

(define (tm:char->int ch)
  (or (digit->integer ch) 
      (errorf "bad date template string: non integer character: ~a" ch)))

;; read an integer upto n characters long on port; upto -> #f if any length
(define (tm:integer-reader upto port)
  (define (accum-int port accum nchars)
    (let ((ch (peek-char port)))
      (if (or (eof-object? ch)
              (not (char-numeric? ch))
              (and upto (>= nchars  upto )))
          accum
          (accum-int port
                     (+ (* accum 10) (tm:char->int (read-char port)))
                     (+ nchars 1)))))
  (accum-int port 0 0))

(define (tm:make-integer-reader upto)
  (lambda (port)
    (tm:integer-reader upto port)))

;; read *exactly* n characters and convert to integer; could be padded
(define (tm:integer-reader-exact n port)
  (let ( (padding-ok #t) )
    (define (accum-int port accum nchars)
      (let ((ch (peek-char port)))
	(cond
	 ((>= nchars n) accum)
	 ((eof-object? ch)
          (errorf "string->date: premature ending of integer read"))
	 ((char-numeric? ch)
	  (set! padding-ok #f)
	  (accum-int port
                     (+ (* accum 10) (tm:char->int (read-char port)))
		     (+ nchars 1)))
	 (padding-ok
	  (read-ch port) ; consume padding
	  (accum-int prot accum (+ nchars 1)))
	 (else ; padding where it shouldn't be
          (errorf "string->date: Non-numeric characters in integer read."))
         )))
    (accum-int port 0 0)))

(define (tm:make-integer-exact-reader n)
  (lambda (port)
    (tm:integer-reader-exact n port)))

(define (tm:zone-reader port) 
  (let ( (offset 0) 
	 (positive? #f) )
    (let ( (ch (read-char port)) )
      (if (eof-object? ch)
          (errorf "string->date: invalid time zone +/-: ~s" ch))
      (if (or (char=? ch #\Z) (char=? ch #\z))
	  0
	  (begin
	    (cond
	     ((char=? ch #\+) (set! positive? #t))
	     ((char=? ch #\-) (set! positive? #f))
	     (else
              (errorf "string->date: invalid time zone +/-: ~s" ch)))
	    (let ((ch (read-char port)))
	      (if (eof-object? ch)
                  (errorf "string->date: premature end of time zone number"))
	      (set! offset (* (tm:char->int ch)
			      10 60 60)))
	    (let ((ch (read-char port)))
	      (if (eof-object? ch)
                  (errorf "string->date: premature end of time zone number"))
	      (set! offset (+ offset (* (tm:char->int ch)
					60 60))))
	    (let ((ch (read-char port)))
	      (if (eof-object? ch)
                  (errorf "string->date: premature end of time zone number"))
	      (set! offset (+ offset (* (tm:char->int ch)
					10 60))))
	    (let ((ch (read-char port)))
	      (if (eof-object? ch)
                  (errorf "string->date: premature end of time zone number"))
	      (set! offset (+ offset (* (tm:char->int ch)
					60))))
	    (if positive? offset (- offset)))))))
    
;; looking at a char, read the char string, run thru indexer, return index
(define (tm:locale-reader port indexer)
  (let ( (string-port (open-output-string)) )
    (define (read-char-string)
      (let ((ch (peek-char port)))
	(if (char-alphabetic? ch)
	    (begin (write-char (read-char port) string-port) 
		   (read-char-string))
	    (get-output-string string-port))))
    (let* ( (str (read-char-string)) 
	    (index (indexer str)) )
      (or index
          (errorf "string->date: invalid string for ~s" indexer)))))

(define (tm:make-locale-reader indexer)
  (lambda (port)
    (tm:locale-reader port indexer)))
      
(define (tm:make-char-id-reader char)
  (lambda (port)
    (if (char=? char (read-char port))
	char
        (errorf "string->date: invalid character match"))))

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
  (let ( (ireader4 (tm:make-integer-reader 4))
	 (ireader2 (tm:make-integer-reader 2))
	 (ireaderf (tm:make-integer-reader #f))
	 (eireader2 (tm:make-integer-exact-reader 2))
	 (eireader4 (tm:make-integer-exact-reader 4))
	 (locale-reader-abbr-weekday (tm:make-locale-reader
				      tm:locale-abbr-weekday->index))
	 (locale-reader-long-weekday (tm:make-locale-reader
				      tm:locale-long-weekday->index))
	 (locale-reader-abbr-month   (tm:make-locale-reader
				      tm:locale-abbr-month->index))
	 (locale-reader-long-month   (tm:make-locale-reader
				      tm:locale-long-month->index))
	 (char-fail (lambda (ch) #t))
	 (do-nothing (lambda (val object) (values)))
	 )
		    
  (list
   (list #\~ char-fail (tm:make-char-id-reader #\~) do-nothing)
   (list #\a char-alphabetic? locale-reader-abbr-weekday do-nothing)
   (list #\A char-alphabetic? locale-reader-long-weekday do-nothing)
   (list #\b char-alphabetic? locale-reader-abbr-month
	 (lambda (val object)
	   (slot-set! object 'month val)))
   (list #\B char-alphabetic? locale-reader-long-month
	 (lambda (val object)
	   (slot-set! object 'month val)))
   (list #\d char-numeric? ireader2 (lambda (val object)
                                      (slot-set! object 'day val)))
   (list #\e char-fail eireader2 (lambda (val object)
                                   (slot-set! object 'day val)))
   (list #\h char-alphabetic? locale-reader-abbr-month
	 (lambda (val object)
	   (slot-set! object 'month val)))
   (list #\H char-numeric? ireader2 (lambda (val object)
                                      (slot-set! object 'hour val)))
   (list #\k char-fail eireader2 (lambda (val object)
                                   (slot-set! object 'hour val)))
   (list #\m char-numeric? ireader2 (lambda (val object)
                                      (slot-set! object 'month val)))
   (list #\M char-numeric? ireader2 (lambda (val object)
                                      (slot-set! object 'minute val)))
   (list #\S char-numeric? ireader2 (lambda (val object)
                                      (slot-set! object 'second val)))
   (list #\y char-fail eireader2 
	 (lambda (val object)
	   (slot-set! object 'year (tm:natural-year val))))
   (list #\Y char-numeric? ireader4 (lambda (val object)
                                      (slot-set! object 'year val)))
   (list #\z (lambda (c)
	       (or (char=? c #\Z)
		   (char=? c #\z)
		   (char=? c #\+)
		   (char=? c #\-)))
	 tm:zone-reader (lambda (val object)
			  (slot-set! object 'zone-offset val)))
   )))

(define (tm:string->date date index format-string str-len port template-string)
  (define (bad) 
    (errorf "string->date: bad date format string: \"~a >>>~a<<< ~a\""
            (string-take template-string index)
            (substring template-string index (+ index 1))
            (string-drop template-string (+ index 1))))
  (define (skip-until port skipper)
    (let ((ch (peek-char port)))
      (if (eof-object? port)
          (bad)
	  (if (not (skipper ch))
	      (begin (read-char port) (skip-until port skipper))))))
  (if (>= index str-len)
      (begin 
	(values))
      (let ( (current-char (string-ref format-string index)) )
	(if (not (char=? current-char #\~))
	    (let ((port-char (read-char port)))
	      (if (or (eof-object? port-char)
		      (not (char=? current-char port-char)))
                  (bad))
	      (tm:string->date date (+ index 1) format-string str-len port template-string))
	    ;; otherwise, it's an escape, we hope
	    (if (> (+ index 1) str-len)
                (bad)
		(let* ( (format-char (string-ref format-string (+ index 1)))
			(format-info (assoc format-char tm:read-directives)) )
		  (if (not format-info)
                      (bad)
		      (begin
			(let ((skipper (cadr format-info))
			      (reader  (caddr format-info))
			      (actor   (cadddr format-info)))
			  (skip-until port skipper)
			  (let ((val (reader port)))
			    (if (eof-object? val)
                                (bad)
				(actor val date)))
			  (tm:string->date date (+ index 2) format-string  str-len port template-string))))))))))

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
  (let ( (newdate (make-date 0 0 0 0 #f #f #f (tm:local-tz-offset))) )
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

(provide "srfi-19/format")
