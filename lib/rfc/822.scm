;;;
;;; 822.scm - parsing RFC2822 style message
;;;
;;;  Copyright(C) 2001-2003 by Shiro Kawai (shiro@acm.org)
;;;
;;;  Permission to use, copy, modify, distribute this software and
;;;  accompanying documentation for any purpose is hereby granted,
;;;  provided that existing copyright notices are retained in all
;;;  copies and that this notice is included verbatim in all
;;;  distributions.
;;;  This software is provided as is, without express or implied
;;;  warranty.  In no circumstances the author(s) shall be liable
;;;  for any damages arising out of the use of this software.
;;;
;;;  $Id: 822.scm,v 1.8 2003-01-09 11:32:57 shirok Exp $
;;;

;; Parser and constructor of the message defined in
;; RFC2822 Internet Message Format <ftp://ftp.isi.edu/in-notes/rfc2822.txt>

(define-module rfc.822
  (use srfi-1)
  (use srfi-13)
  (use gauche.regexp)
  (export rfc822-header->list
          rfc822-skip-cfws
          rfc822-next-word
          rfc822-parse-date
          )
  )

(select-module rfc.822)

;;=================================================================
;; Parsers
;;

;;-----------------------------------------------------------------
;; Generic header parser, recognizes folded line and field names
;;
(define (rfc822-header->list iport . args)
  (let-optionals* args ((strict? #f))
    (let loop ((r '())
               (line (read-line iport)))
      (receive (head body next)
          (read-single-field iport line strict?)
        (if next
            (loop (cons (list head body) r) next)
            (reverse (cons (list head body) r))))
      )))

;; Internal routine.  read a single header field.
;;  iport - input port
;;  pline  - prefetched line.  #f if none.
;; Returns three values: field-name, field-body, and the prefetched line
;; (to process folded field, the routne need to prefetch one line ahead).
;; If it sees the end of the header, returns (values name body #f)
(define (read-single-field input line strict?)
  (rxmatch-case line
    (test eof-object? (values #t #t #f))
    (test string-null? (values #t #t #f))
    (#/^([\x21-\x39\x3b-\x7e]+):\s*(.*)$/ (#f name body)
     (let ((name (string-downcase name)))
       (let loop ((nline (read-line input))
                  (bodies (list body)))
         (cond ((eof-object? nline)
                ;; maybe premature end of the message
                (if strict?
                    (error "premature end of message header")
                    (values name (string-concatenate-reverse bodies) #f)))
               ((string-null? nline)     ;; end of the header
                (values name (string-concatenate-reverse bodies) #f))
               ((char-set-contains? #[ \t] (string-ref nline 0))
                (loop (read-line input) (cons nline bodies)))
               (else
                (values name (string-concatenate-reverse bodies) nline)))
         )
       ))
    (else
     (if strict?
         (error "bad header line:" line)
         (read-single-field input (read-line input) #f)))))

;;------------------------------------------------------------------
;; Comments, quoted pairs, atoms and quoted string.  Section 3.2
;;

(define (rfc822-skip-cfws input prefetch)
  (define (scan c)
    (cond ((eof-object? c) c)
          ((char=? c #\( ) (in-comment (read-char input)))
          ((char-whitespace? c) (scan (read-char input)))
          (else c)))
  (define (in-comment c)
    (cond ((eof-object? c) c)
          ((char=? c #\) ) (scan (read-char input)))
          ((char=? c #\\ ) (read-char input) (in-comment (read-char input)))
          ((char=? c #\( ) (in-comment (in-comment (read-char input))))
          (else (in-comment (read-char input)))))
  (scan (or prefetch (read-char input))))

(define (rfc822-next-word input prefetch)
  (let ((out (open-output-string)))
    (define (finish c) (values (get-output-string out) c))
    (define (atom c)
      (cond ((eof-object? c) (finish c))
            ((char-set-contains? #[A-Za-z0-9!#$%&'*+/=?^_`{|}~-] c)
             (write-char c out) (atom (read-char input)))
            (else (finish c))))
    (define (quoted c)
      (cond ((eof-object? c) (finish c)) ;tolerate
            ((char=? c #\") (finish #f))
            ((char=? c #\\)
             (let ((c (read-char input)))
               (cond ((eof-object? c) (finish c)) ;tolerate
                     (else (write-char c out) (quoted (read-char input))))))
            (else (write-char c out) (quoted (read-char input)))))

    (let ((c (rfc822-skip-cfws input prefetch)))
      (cond ((eof-object? c) (values "" c))
            ((char=? c #\") (quoted (read-char input)))
            (else (atom c))))
    ))

;;------------------------------------------------------------------
;; Date and time, section 3.3
;;

;; Takes RFC-822 type date string, and returns eight values:
;;   year, month, day-of-month, hour, minutes, seconds, timezone, day-of-week.
;; Timezone is an offset from UT in minutes.  Day-of-week is a day from
;; sunday, and may be #f if that information is not available.
;; If the string is not parsable, all the elements are #f.

;; NB: This function follows the new definition of date format in RFC2822,
;; but may fail to recognize "obsolete" format, which allows arbitrary
;; comments appear between words.

(define (rfc822-parse-date string)
  (define (dow->number dow)
    (- (length (member dow '("Sat" "Fri" "Thu" "Wed" "Tue" "Mon" "Sun"))) 1))
  (define (mon->number mon)
    (- (length (member mon '("Dec" "Nov" "Oct" "Sep" "Aug" "Jul"
                             "Jun" "May" "Apr" "Mar" "Feb" "Jan")))
       1))
  (define (year->number year) ;; see obs-year definition of RFC-2822
    (let ((y (string->number year)))
      (and y
           (cond ((< y 50)  (+ y 2000))
                 ((< y 100) (+ y 1900))
                 (else y)))))
  (define (tz->number tz)
    (cond ((string->number tz))
          ((assoc tz '(("UT" . 0) ("GMT" . 0) ("EDT" . -400) ("EST" . -500)
                       ("CDT" . -500) ("CST" . -600) ("MDT" . -600)
                       ("MST" . -700) ("PDT" . -700) ("PST" . -800))))
          (else #f)))

  (rxmatch-case string
    (#/((Sun|Mon|Tue|Wed|Thu|Fri|Sat)\s*,)?\s*(\d+)\s*(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\s*(\d\d\d\d)\s+(\d\d)\s*:\s*(\d\d)\s*:\s*(\d\d)\s+([+-]\d\d\d\d|[A-Z][A-Z][A-Z])/
       (#f #f dow dom mon yr hour min sec tz)
       (values (year->number yr)
               (mon->number mon)
               (string->number dom)
               (string->number hour)
               (string->number min)
               (string->number sec)
               (tz->number tz)
               (dow->number dow)))
     (else (values #f #f #f #f #f #f #f #f))))

;(define (rfc822-date->time string)
;  (receive (year month day hour min sez tz . rest)
;      (rfc822-parse-date string)
;    (and year
;         (
  

(provide "rfc/822")
