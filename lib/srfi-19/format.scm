;;; srfi-19/format.scm - excerpt from SRFI-19 for date formatting routine.
;;; $Id: format.scm,v 1.2 2002-03-09 20:25:44 shirok Exp $

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

(define tm:locale-number-separator ".")

(define tm:locale-abbr-weekday-vector (vector "Sun" "Mon" "Tue" "Wed"
					     "Thu" "Fri" "Sat")) 
(define tm:locale-long-weekday-vector (vector "Sunday" "Monday"
					     "Tuesday" "Wednesday"
					     "Thursday" "Friday"
					     "Saturday"))
;; note empty string in 0th place. 
(define tm:locale-abbr-month-vector   (vector "" "Jan" "Feb" "Mar"
					     "Apr" "May" "Jun" "Jul"
					     "Aug" "Sep" "Oct" "Nov"
					     "Dec")) 
(define tm:locale-long-month-vector   (vector "" "January" "February"
					     "March" "April" "May"
					     "June" "July" "August"
					     "September" "October"
					     "November" "December")) 

(define tm:locale-pm "PM")
(define tm:locale-am "AM")

;; See date->string
(define tm:locale-date-time-format "~a ~b ~d ~H:~M:~S~z ~Y")
(define tm:locale-short-date-format "~m/~d/~y")
(define tm:locale-time-format "~H:~M:~S")
(define tm:iso-8601-date-time-format "~Y-~m-~dT~H:~M:~S~z")

;; returns a string rep. of number N, of minimum LENGTH,
;; padded with character PAD-WITH. If PAD-WITH if #f, 
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

(define (tm:vector-find needle haystack comparator)
  (let ((len (vector-length haystack)))
    (define (tm:vector-find-int index)
      (cond
       ((>= index len) #f)
       ((comparator needle (vector-ref haystack index)) index)
       (else (tm:vector-find-int (+ index 1)))))
    (tm:vector-find-int 0)))

(define (tm:locale-abbr-weekday->index string)
  (tm:vector-find string tm:locale-abbr-weekday-vector string=?))

(define (tm:locale-long-weekday->index string)
  (tm:vector-find string tm:locale-long-weekday-vector string=?))

(define (tm:locale-abbr-month->index string)
  (tm:vector-find string tm:locale-abbr-month-vector string=?))

(define (tm:locale-long-month->index string)
  (tm:vector-find string tm:locale-long-month-vector string=?))



;; do nothing. 
;; Your implementation might want to do something...
;; 
(define (tm:locale-print-time-zone date port)
  (values))

;; Again, locale specific.
(define (tm:locale-am/pm hr)
  (if (> hr 11) tm:locale-pm tm:locale-am))

(define (tm:tz-printer offset port)
  (cond
   ((= offset 0) (display "Z" port))
   ((negative? offset) (display "-" port))
   (else (display "+" port)))
  (if (not (= offset 0))
      (let ( (hours   (abs (quotient offset (* 60 60))))
	     (minutes (abs (quotient (remainder offset (* 60 60)) 60))) )
	(display (tm:padding hours #\0 2) port)
	(display (tm:padding minutes #\0 2) port))))

;; A table of output formatting directives.
;; the first time is the format char.
;; the second is a procedure that takes the date, a padding character
;; (which might be #f), and the output port.
;;
(define tm:directives 
  (list
   (cons #\~ (lambda (date pad-with port) (display #\~ port)))

   (cons #\a (lambda (date pad-with port)
	       (display (tm:locale-abbr-weekday (date-week-day date))
			port)))
   (cons #\A (lambda (date pad-with port)
	       (display (tm:locale-long-weekday (date-week-day date))
			port)))
   (cons #\b (lambda (date pad-with port)
	       (display (tm:locale-abbr-month (date-month date))
			port)))
   (cons #\B (lambda (date pad-with port)
	       (display (tm:locale-long-month (date-month date))
			port)))
   (cons #\c (lambda (date pad-with port)
	       (display (date->string date tm:locale-date-time-format) port)))
   (cons #\d (lambda (date pad-with port)
	       (display (tm:padding (date-day date)
				    #\0 2)
			    port)))
   (cons #\D (lambda (date pad-with port)
	       (display (date->string date "~m/~d/~y") port)))
   (cons #\e (lambda (date pad-with port)
	       (display (tm:padding (date-day date)
				    #\Space 2)
			port)))
   (cons #\f (lambda (date pad-with port)
	       (if (> (date-nanosecond date)
		      tm:nano)
		   (display (tm:padding (+ (date-second date) 1)
					pad-with 2)
			    port)
		   (display (tm:padding (date-second date)
					pad-with 2)
			    port))
	       (receive (i f) 
			(tm:split-real (/ 
					(date-nanosecond date)
					tm:nano 1.0))
			(let* ((ns (number->string f))
			       (le (string-length ns)))
			  (if (> le 2)
			      (begin
				(display tm:locale-number-separator port)
				(display (substring ns 2 le) port)))))))
   (cons #\h (lambda (date pad-with port)
	       (display (date->string date "~b") port)))
   (cons #\H (lambda (date pad-with port)
	       (display (tm:padding (date-hour date)
				    pad-with 2)
			port)))
   (cons #\I (lambda (date pad-with port)
	       (let ((hr (date-hour date)))
		 (if (> hr 12)
		     (display (tm:padding (- hr 12)
					  pad-with 2)
			      port)
		     (display (tm:padding hr
					  pad-with 2)
			      port)))))
   (cons #\j (lambda (date pad-with port)
	       (display (tm:padding (date-year-day date)
				    pad-with 3)
			port)))
   (cons #\k (lambda (date pad-with port)
	       (display (tm:padding (date-hour date)
				    #\Space 2)
			    port)))
   (cons #\l (lambda (date pad-with port)
	       (let ((hr (if (> (date-hour date) 12)
			     (- (date-hour date) 12) (date-hour date))))
		 (display (tm:padding hr  #\Space 2)
			  port))))
   (cons #\m (lambda (date pad-with port)
	       (display (tm:padding (date-month date)
				    pad-with 2)
			port)))
   (cons #\M (lambda (date pad-with port)
	       (display (tm:padding (date-minute date)
				    pad-with 2)
			port)))
   (cons #\n (lambda (date pad-with port)
	       (newline port)))
   (cons #\N (lambda (date pad-with port)
	       (display (tm:padding (date-nanosecond date)
				    pad-with 7)
			port)))
   (cons #\p (lambda (date pad-with port)
	       (display (tm:locale-am/pm (date-hour date)) port)))
   (cons #\r (lambda (date pad-with port)
	       (display (date->string date "~I:~M:~S ~p") port)))
   (cons #\s (lambda (date pad-with port)
	       (display (time-second (date->time-utc date)) port)))
   (cons #\S (lambda (date pad-with port)
	       (if (> (date-nanosecond date)
		      tm:nano)
	       (display (tm:padding (+ (date-second date) 1)
				    pad-with 2)
			port)
	       (display (tm:padding (date-second date)
				    pad-with 2)
			port))))
   (cons #\t (lambda (date pad-with port)
	       (display #\Tab port)))
   (cons #\T (lambda (date pad-with port)
	       (display (date->string date "~H:~M:~S") port)))
   (cons #\U (lambda (date pad-with port)
	       (if (> (tm:days-before-first-week date 0) 0)
		   (display (tm:padding (+ (date-week-number date 0) 1)
					#\0 2) port)
		   (display (tm:padding (date-week-number date 0)
					#\0 2) port))))
   (cons #\V (lambda (date pad-with port)
	       (display (tm:padding (date-week-number date 1)
				    #\0 2) port)))
   (cons #\w (lambda (date pad-with port)
	       (display (date-week-day date) port)))
   (cons #\x (lambda (date pad-with port)
	       (display (date->string date tm:locale-short-date-format) port)))
   (cons #\X (lambda (date pad-with port)
	       (display (date->string date tm:locale-time-format) port)))
   (cons #\W (lambda (date pad-with port)
	       (if (> (tm:days-before-first-week date 1) 0)
		   (display (tm:padding (+ (date-week-number date 1) 1)
					#\0 2) port)
		   (display (tm:padding (date-week-number date 1)
					#\0 2) port))))
   (cons #\y (lambda (date pad-with port)
	       (display (tm:padding (tm:last-n-digits 
				     (date-year date) 2)
				    pad-with
				    2)
			port)))
   (cons #\Y (lambda (date pad-with port)
	       (display (date-year date) port)))
   (cons #\z (lambda (date pad-with port)
	       (tm:tz-printer (date-zone-offset date) port)))
   (cons #\Z (lambda (date pad-with port)
	       (tm:locale-print-time-zone date port)))
   (cons #\1 (lambda (date pad-with port)
	       (display (date->string date "~Y-~m-~d") port)))
   (cons #\2 (lambda (date pad-with port)
	       (display (date->string date "~k:~M:~S~z") port)))
   (cons #\3 (lambda (date pad-with port)
	       (display (date->string date "~k:~M:~S") port)))
   (cons #\4 (lambda (date pad-with port)
	       (display (date->string date "~Y-~m-~dT~k:~M:~S~z") port)))
   (cons #\5 (lambda (date pad-with port)
	       (display (date->string date "~Y-~m-~dT~k:~M:~S") port)))
   ))


(define (tm:get-formatter char)
  (let ( (associated (assoc char tm:directives)) )
    (if associated (cdr associated) #f)))

(define (tm:date-printer date index format-string str-len port)
  (define (bad) 
    (errorf "tm:date-printer: bad date format string: ~s" format-string))
  (if (>= index str-len)
      (values)
      (let ( (current-char (string-ref format-string index)) )
	(if (not (char=? current-char #\~))
	    (begin
	      (display current-char port)
	      (tm:date-printer date (+ index 1) format-string str-len port))
	    (if (= (+ index 1) str-len) ; bad format string.
                (bad)
                (let ( (pad-char? (string-ref format-string (+ index 1))) )
                  (cond
                   ((char=? pad-char? #\-)
                    (when (= (+ index 2) str-len) ; bad format string.
                      (bad))
                    (let ( (formatter (tm:get-formatter 
                                       (string-ref format-string
                                                   (+ index 2)))) )
                      (unless formatter (bad))
                      (formatter date #f port)
                      (tm:date-printer date (+ index 3)
                                       format-string str-len port)))

                   ((char=? pad-char? #\_)
                    (when (= (+ index 2) str-len) ; bad format string.
                      (bad))
                    (let ( (formatter (tm:get-formatter 
                                       (string-ref format-string
                                                   (+ index 2)))) )
                      (unless formatter (bad))
                      (formatter date #\Space port)
                      (tm:date-printer date (+ index 3)
                                       format-string str-len port)))

                   (else
                    (let ( (formatter (tm:get-formatter 
                                       (string-ref format-string
                                                   (+ index 1)))) )
                      (unless formatter (bad))
                      (formatter date #\0 port)
                      (tm:date-printer date (+ index 2)
                                       format-string str-len port))))))))))


(define (date->string date .  format-string)
  (let-optionals* format-string ((fmt-str "~c"))
    (let ( (str-port (open-output-string)))
      (tm:date-printer date 0 fmt-str (string-length fmt-str) str-port)
      (get-output-string str-port))))
	
(define (tm:char->int ch)
  (or (digit->integer ch) 
      (errorf "bad date template string: non integer character: ~a" ch)))

;; read an integer upto n characters long on port; upto -> #f if any length
(define (tm:integer-reader upto port)
  (define (accum-int port accum nchars)
    (let ((ch (peek-char port)))
      (if (or (eof-object? ch)
              n(not (char-numeric? ch))
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
	   (tm:set-date-month! object val)))
   (list #\B char-alphabetic? locale-reader-long-month
	 (lambda (val object)
	   (tm:set-date-month! object val)))
   (list #\d char-numeric? ireader2 (lambda (val object)
					       (tm:set-date-day!
						object val)))
   (list #\e char-fail eireader2 (lambda (val object)
					       (tm:set-date-day! object val)))
   (list #\h char-alphabetic? locale-reader-abbr-month
	 (lambda (val object)
	   (tm:set-date-month! object val)))
   (list #\H char-numeric? ireader2 (lambda (val object)
							(tm:set-date-hour! object val)))
   (list #\k char-fail eireader2 (lambda (val object)
					       (tm:set-date-hour! object val)))
   (list #\m char-numeric? ireader2 (lambda (val object)
					       (tm:set-date-month! object val)))
   (list #\M char-numeric? ireader2 (lambda (val object)
					       (tm:set-date-minute!
						object val)))
   (list #\S char-numeric? ireader2 (lambda (val object)
							(tm:set-date-second! object val)))
   (list #\y char-fail eireader2 
	 (lambda (val object)
	   (tm:set-date-year! object (tm:natural-year val))))
   (list #\Y char-numeric? ireader4 (lambda (val object)
					       (tm:set-date-year! object val)))
   (list #\z (lambda (c)
	       (or (char=? c #\Z)
		   (char=? c #\z)
		   (char=? c #\+)
		   (char=? c #\-)))
	 tm:zone-reader (lambda (val object)
			  (tm:set-date-zone-offset! object val)))
   )))

(define (tm:string->date date index format-string str-len port template-string)
  (define (bad) 
    (errorf "string->date: bad date format string: ~s" template-string))
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
