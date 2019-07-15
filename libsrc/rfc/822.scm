;;;
;;; 822.scm - parsing RFC2822 style message
;;;
;;;   Copyright (c) 2000-2019  Shiro Kawai  <shiro@acm.org>
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

;; Parser and constructor of the message defined in
;; RFC2822 Internet Message Format
;;         http://www.ietf.org/rfc/rfc2822.txt

(define-module rfc.822
  (use srfi-1)
  (use srfi-13)
  (use srfi-19)
  (use text.parse)
  (use gauche.regexp)
  (use util.match)
  (export <rfc822-parse-error> rfc822-parse-errorf
          rfc822-read-headers rfc822-header->list rfc822-header-ref
          rfc822-skip-cfws
          *rfc822-atext-chars* *rfc822-standard-tokenizers*
          rfc822-atom rfc822-dot-atom rfc822-quoted-string
          rfc822-next-token rfc822-field->tokens
          rfc822-parse-date rfc822-date->date date->rfc822-date

          rfc822-invalid-header-field rfc822-write-headers
          )
  )
(select-module rfc.822)

;;=================================================================
;; Parsers
;;

;;-----------------------------------------------------------------
;; A condition reported when rfc822 parser finds an error.
;;
;; For most practical applications, it is useful to handle
;; rfc822 syntax errors gracefully.  So the rfc822 and mime procedures
;; tries to return a reasonable value even if the input is not strictly
;; conforming the RFCs.  Some routines have an option to be more strict,
;; and they raise <rfc822-parse-error> when the option is given and
;; they find a violation.
;;
;; TODO: We need some consistent way to switch these 'graceful' and
;; 'strict' modes throughout the libraries; otherwise the API would be
;; pretty confusing.  Something to consider as we add more high-level
;; parsers.

(define-condition-type <rfc822-parse-error> <error> #f
  (header-field-name)
  (header-field-body))

(define (rfc822-parse-errorf name body fmt . args)
  (apply errorf <rfc822-parse-error>
         :header-field-name name
         :header-field-body body
         fmt args))

;;-----------------------------------------------------------------
;; Generic header parser.  Returns ((name body) ...)
;; Does process unfolding.
;; May throw <rfc822-parse-error> if :strict? is true.
(define (rfc822-read-headers iport :key (strict? #f)
                             (reader (cut read-line <> #t)))

  (define (accum name bodies r)
    (cons (list name (string-concatenate-reverse bodies)) r))

  (define (drop-leading-fws body)
    (if (string-incomplete? body)
      body  ;; this message is not RFC2822 compliant anyway
      (string-trim body)))

  (let loop ([r '()]
             [line (reader iport)])
    (cond
     [(eof-object? line) (reverse! r)]
     [(string-null? line) (reverse! r)]
     [else
      (receive (n body) (string-scan line #\: 'both)
        (let1 name (and-let* ([ (string? n) ]
                              [name (string-incomplete->complete n)]
                              [name (string-trim-both name)]
                              [ (string-every #[\u0021-\u0039\u003b-\u007e] name) ])
                     (string-downcase name))
          (cond
           [name
            (let loop2 ([nline (reader iport)]
                        [bodies (list (drop-leading-fws body))])
              (cond [(eof-object? nline)
                     ;; maybe premature end of the message
                     (if strict?
                       (rfc822-parse-errorf
                        #f #f "premature end of message header")
                       (reverse! (accum name bodies r)))]
                    [(string-null? nline)     ;; end of the header
                     (reverse! (accum name bodies r))]
                    [(memv (string-byte-ref nline 0) '(9 32))
                     ;; careful for byte strings
                     (loop2 (reader iport) (cons nline bodies))]
                    [else
                     (loop (accum name bodies r) nline)]))]
           [strict? (rfc822-parse-errorf #f #f "bad header line: ~s" line)]
           [else (loop r (reader iport))])))])
    ))

(define (rfc822-header-ref header field-name :optional (default #f))
  (cond [(assoc field-name header) => cadr]
        [else default]))

;; backward compatibility
(define rfc822-header->list rfc822-read-headers)

;;------------------------------------------------------------------
;; Comments, quoted pairs, atoms and quoted string.  Section 3.2
;;

;; skip comments and white spaces, then returns the head char.
(define (rfc822-skip-cfws input)
  (define (scan c)
    (cond [(eof-object? c) c]
          [(char=? c #\( ) (in-comment (peek-next-char input))]
          [(char-whitespace? c) (scan (peek-next-char input))]
          [else c]))
  (define (in-comment c)
    (cond [(eof-object? c) c]
          [(char=? c #\) ) (scan (peek-next-char input))]
          [(char=? c #\\ ) (read-char input) (in-comment (peek-next-char input))]
          [(char=? c #\( ) (in-comment (in-comment (peek-next-char input)))]
          [else (in-comment (peek-next-char input))]))
  (scan (peek-char input)))

;; Basic tokenizers.  Supposed to be used for higher-level parsers.

(define-constant *rfc822-atext-chars* #[A-Za-z0-9!#$%&'*+/=?^_`{|}~-])

(define (rfc822-atom input)
  (next-token-of *rfc822-atext-chars* input))

;; NB: this is loose, but usually OK.
(define (rfc822-dot-atom input)
  (next-token-of `(,*rfc822-atext-chars* #\.) input))

;; Assuming the first char in input is DQUOTE
(define (rfc822-quoted-string input)
  (let1 r (open-output-string :private? #t)
    (define (finish) (get-output-string r))
    (let loop ([c (peek-next-char input)])
      (cond [(eof-object? c) (finish)];; tolerate missing closing DQUOTE
            [(char=? c #\") (read-char input) (finish)] ;; discard DQUOTE
            [(char=? c #\\)
             (let1 c (peek-next-char input)
               (cond [(eof-object? c) (finish)] ;; tolerate stray backslash
                     [else (write-char c r) (loop (peek-next-char input))]))]
            [else (write-char c r) (loop (peek-next-char input))]))))

;; Default tokenizer table
(define *rfc822-standard-tokenizers*
  `((#[\"] . ,rfc822-quoted-string)
    (,*rfc822-atext-chars* . ,rfc822-dot-atom)))

;; Returns the next token or EOF
(define (rfc822-next-token input :optional (table *rfc822-standard-tokenizers*))
  (let ([toktab (map (^e (if (char-set? e)
                           (cons e (cut next-token-of e <>))
                           e))
		     table)]
        [c (rfc822-skip-cfws input)])
    (cond [(eof-object? c) c]
          [(find (^e (char-set-contains? (car e) c)) toktab)
           => (^e ((cdr e) input))]
          [else (read-char input)])))

;; returns a list of tokens, for convenience
(define (rfc822-field->tokens field . opts)
  (call-with-input-string field
    (cut port->list (cut apply rfc822-next-token <> opts) <>)))

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
    (+ 1 (list-index (cut string=? <> mon)
                     '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                       "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))))
  (define (year->number year) ;; see obs-year definition of RFC2822
    (and-let* ([y (string->number year)])
      (cond [(< y 50)  (+ y 2000)]
            [(< y 100) (+ y 1900)]
            [else y])))
  (define (tz->number tz)
    (cond [(equal? tz "-0000") #f]  ;;no effective TZ info; see 3.3 of RFC2822
          [(string->number tz)]
          [(assoc-ref '(("UT" . 0) ("GMT" . 0) ("EDT" . -400) ("EST" . -500)
                        ("CDT" . -500) ("CST" . -600) ("MDT" . -600)
                        ("MST" . -700) ("PDT" . -700) ("PST" . -800)) tz)]
          [else #f]))

  (rxmatch-case string
    (#/((Sun|Mon|Tue|Wed|Thu|Fri|Sat)\s*,)?\s*(\d+)\s*(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\s*(\d\d(\d\d)?)\s+(\d\d)\s*:\s*(\d\d)(\s*:\s*(\d\d))?(\s+([+-]\d\d\d\d|[A-Z][A-Z][A-Z]?))?/
       (#f bebe dow dom mon yr #f hour min #f sec #f tz)
       (values (year->number yr)
               (mon->number mon)
               (string->number dom)
               (string->number hour)
               (string->number min)
               (and sec (string->number sec))
               (and tz (tz->number tz))
               (and dow (dow->number dow))))
     (else (values #f #f #f #f #f #f #f #f))))

;; returns it by srfi-19 date
(define (rfc822-date->date string)
  (receive (year month day hour min sec tz . rest)
      (rfc822-parse-date string)
    (and year
         (make-date 0 sec min hour day month year
                    (receive (quot rem) (quotient&remainder tz 100)
                      (+ (* quot 3600) (* rem 60)))))))

;; inverse of rfc822-date->date.  Take srfi-19 date and returns
;; formatted string.
(define (date->rfc822-date date)
  (let1 tz (date-zone-offset date)
    (format "~a, ~2d ~a ~4d ~2,'0d:~2,'0d:~2,'0d ~a~2,'0d~2,'0d"
            (~ '#("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")
               (date-week-day date))
            (date-day date)
            (~ '#("" "Jan" "Feb" "Mar" "Apr" "May" "Jun"
                  "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
               (date-month date))
            (date-year date)
            (date-hour date) (date-minute date) (date-second date)
            (if (>= tz 0) "+" "-")
            (quotient (abs tz) 3600)
            (modulo (quotient (abs tz) 60) 60))))

;;------------------------------------------------------------------
;; Address specification (Section 3.4)
;;

;; The EBNF syntax in RFC2822 requires arbitrary lookahead,
;; so straight recursive-descent parser won't work.
;;

;; to be written

;;=================================================================
;; Constructors
;;

;; Writes out the header fields specified by HEADERS, which is
;; ((name body) ...).
(define (rfc822-write-headers headers :key
                              (output (current-output-port))
                              (check :error) ; #f, :ignore, :error or proc
                              (continue #f))
  (define (process headers)
    (dolist [field headers]
      (display (car field) output)
      (display ": " output)
      (display (cadr field) output)
      (unless (string-suffix? "\r\n" (cadr field))
        (display "\r\n" output)))
    (unless continue (display "\r\n" output)))
  (define (bad name body reason)
    (errorf "Illegal RFC2822 header field data (~a): ~a: ~,,,,80:a" reason name body))
  (if (memv check '(#f :ignore))
    (process headers)
    (let loop ([hs headers]
               [hs2 '()])
      (match hs
        [() (process (reverse hs2))]
        [((name body) . rest)
         (cond [(rfc822-invalid-header-field (string-append name ": " body))
                => (^[reason]
                     (if (eq? check :error)
                       (bad name body reason)
                       (receive (name2 body2) (check name body reason)
                         (if (and (equal? name name2) (equal? body body2))
                           (bad name body reason)
                           (loop `((,name2 ,body2) . ,rest) hs2)))))]
               [else (loop rest `((,name ,body) . ,hs2))])]
        [else
         (error "Invalid header data:" headers)]))))

(define (rfc822-invalid-header-field body)
  (cond [(string-incomplete? body) 'incomplete-string]
        [(string-index body #[^\u0001-\u007f]) 'bad-character]
        [else
         (let1 lines (string-split body "\r\n ")
           (cond [(any (^s (> (string-size s) 998)) lines) 'line-too-long]
                 [(any (^s (string-index s #[\u000d\u000a])) lines) 'stray-crlf]
                 [else #f]))]))

