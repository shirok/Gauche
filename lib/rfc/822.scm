;;;
;;; 822.scm - parsing RFC2822 style message
;;;  
;;;   Copyright (c) 2000-2003 Shiro Kawai, All rights reserved.
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
;;;  $Id: 822.scm,v 1.11 2003-11-27 17:10:40 shirok Exp $
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

(define (rfc822-atext-char? ch)
  (and (char? ch)
       (char-set-contains? #[A-Za-z0-9!#$%&'*+/=?^_`{|}~-] ch)))

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

(define (rfc822-atom input prefetch out cont)
  (let loop ((c (or prefetch (read-char input))))
    (cond ((eof-object? c) (cont c))
          ((rfc822-atext-char? c)
           (when out (write-char c out))
           (loop (read-char input)))
          (else (cont c)))))

(define (rfc822-dot-atom input prefetch out cont)
  (let loop ((c (or prefetch (read-char input))))
    (cond ((eof-object? c) (cont c))
          ((rfc822-atext-char? c)
           (when out (write-char c out))
           (loop (read-char input)))
          ((char=? c #\.)
           (let1 c2 (read-char input)
             (if (rfc822-atext-char? c2)
               (begin
                 (when out (write-char c out) (write-char c2 out))
                 (loop (read-char input)))
               #f)))
          (else (cont c)))))

;; preceding DQUOTE has already been read
(define (rfc822-quoted-string input out cont)
  (let loop ((c (read-char input)))
    (cond ((eof-object? c) (cont c)) ;; tolerate missing closing DQUOTE
          ((char=? c #\") (cont #f))
          ((char=? c #\\)
           (let1 c (read-char input)
             (cond ((eof-object? c) (cont #f)) ;; tolerate stray backslash
                   (else (when out (write-char c out))
                         (loop (read-char input)))))
           (else (when out (write-char c out))
                 (loop (read-char input)))))))

;(define (rfc822-word input prefetch out cont)
;  (let loop ((c (or prefetch (read-char input))))
;    (cond ((eof-object? c) (cont c))
;          (

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

;(define (rfc822-next-dot-atom 

    

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
    (list-index (cut string=? <> dow)
                '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")))
  (define (mon->number mon)
    (list-index (cut string=? <> mon)
                '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                  "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))
  (define (year->number year) ;; see obs-year definition of RFC2822
    (let ((y (string->number year)))
      (and y
           (cond ((< y 50)  (+ y 2000))
                 ((< y 100) (+ y 1900))
                 (else y)))))
  (define (tz->number tz)
    (cond ((equal? tz "-0000") #f)  ;;no effective TZ info; see 3.3 of RFC2822
          ((string->number tz))
          ((assoc tz '(("UT" . 0) ("GMT" . 0) ("EDT" . -400) ("EST" . -500)
                       ("CDT" . -500) ("CST" . -600) ("MDT" . -600)
                       ("MST" . -700) ("PDT" . -700) ("PST" . -800)))
           => cdr)
          (else #f)))

  (rxmatch-case string
    (#/((Sun|Mon|Tue|Wed|Thu|Fri|Sat)\s*,)?\s*(\d+)\s*(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\s*(\d\d(\d\d)?)\s+(\d\d)\s*:\s*(\d\d)(\s*:\s*(\d\d))?(\s+([+-]\d\d\d\d|[A-Z][A-Z][A-Z]?))?/
       (#f #f dow dom mon yr #f hour min #f sec #f tz)
       (values (year->number yr)
               (mon->number mon)
               (string->number dom)
               (string->number hour)
               (string->number min)
               (and sec (string->number sec))
               (and tz (tz->number tz))
               (and dow (dow->number dow))))
     (else (values #f #f #f #f #f #f #f #f))))

;(define (rfc822-date->time string)
;  (receive (year month day hour min sez tz . rest)
;      (rfc822-parse-date string)
;    (and year
;         (
  

(provide "rfc/822")
