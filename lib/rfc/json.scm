;;;
;;; json.scm - JSON (RFC4627) Parser
;;;
;;;   Copyright (c) 2006 Rui Ueyama (rui314@gmail.com)
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

;;; http://www.ietf.org/rfc/rfc4627.txt

;; NOTE: This module depends on parser.peg, whose API is not officially
;; fixed.  Hence do not take this code as an example of parser.peg;
;; this will likely to be rewritten once parser.peg's API is changed.

(define-module rfc.json
  (use parser.peg)
  (use srfi-13)
  (use srfi-14)
  (use srfi-43)
  (export <json-parse-error> <json-construct-error>
          parse-json parse-json-string
          construct-json construct-json-string))
(select-module rfc.json)

;; NB: We have <json-parse-error> independent from <parse-error> for
;; now, since parser.peg's interface may be changed later.
(define-condition-type <json-parse-error> <error> #f
  (position)                            ;stream position
  (objects))                            ;offending object(s) or messages

(define-condition-type <json-construct-error> <error> #f
  (object))                             ;offending object

;;;============================================================
;;; Parser
;;;
(define %ws ($skip-many ($one-of #[ \t\r\n])))

(define %begin-array     ($seq ($char #\[) %ws))
(define %begin-object    ($seq ($char #\{) %ws))
(define %end-array       ($seq ($char #\]) %ws))
(define %end-object      ($seq ($char #\}) %ws))
(define %name-separator  ($seq ($char #\:) %ws))
(define %value-separator ($seq ($char #\,) %ws))

(define %false ($do [($string "false")] ($return 'false)))
(define %null  ($do [($string "null")]  ($return 'null)))
(define %true  ($do [($string "true")]  ($return 'true)))

(define %value
  ($lazy ($do [v ($or %false %null %true %object %array %number %string)]
              %ws
              ($return v))))

(define %array
  ($do %begin-array
       [lis ($sep-by %value %value-separator)]
       %end-array
       ($return (list->vector (rope-finalize lis)))))

(define %number
  (let* ((%sign ($or ($do [($char #\-)] ($return -1))
                     ($do [($char #\+)] ($return 1))
                     ($return 1)))
         (%digits ($do [d ($many digit 1)]
                       ($return (string->number (list->string d)))))
         (%int %digits)
         (%frac ($do [($char #\.)]
                     [d ($many digit 1)]
                     ($return (string->number (apply string #\0 #\. d)))))
         (%exp ($do [($one-of #[eE])] [s %sign] [d %digits]
                    ($return (* s d)))))
    ($do (sign %sign)
         (int %int)
         (frac ($or %frac ($return 0)))
         (exp ($or %exp ($return #f)))
         ($return (let ((mantissa (+ int frac)))
                    (* sign (if exp (exact->inexact mantissa) mantissa)
                       (if exp (expt 10 exp) 1)))))))

(define %string
  (let* ((%dquote ($char #\"))
         (%escape ($char #\\))
         (%hex4 ($do [s ($many hexdigit 4 4)]
                     ($return (string->number (list->string s) 16))))
         (%special-char
          ($do %escape
               ($or ($char #\")
                    ($char #\\)
                    ($char #\/)
                    ($do [($char #\b)] ($return #\x08))
                    ($do [($char #\f)] ($return #\page))
                    ($do [($char #\n)] ($return #\newline))
                    ($do [($char #\r)] ($return #\return))
                    ($do [($char #\t)] ($return #\tab))
                    ($do [($char #\u)] (c %hex4) ($return (ucs->char c))))))
         (%unescaped ($none-of #[\"]))
         (%body-char ($or %special-char %unescaped))
         (%string-body ($->rope ($many %body-char))))
    ($between %dquote %string-body %dquote)))

(define %object
  (let1 %member ($do [k %string] %ws
                     %name-separator
                     [v %value]
                     ($return (cons k v)))
    ($between %begin-object
              ($sep-by %member %value-separator)
              %end-object)))

(define %json-text ($or %object %array))

;; entry point
(define (parse-json :optional (port (current-input-port)))
  (guard (e [(<parse-error> e)
             ;; not to expose parser.peg's <parse-error>.
             (error <json-parse-error>
                    :position (~ e'position) :objects (~ e'objects)
                    :message (~ e'message))])
    (peg-parse-port %json-text port)))

(define (parse-json-string str)
  (call-with-input-string str (cut parse-json <>)))

;;;============================================================
;;; Writer
;;;

(define (print-value obj)
  (cond [(eq? obj 'false) (display "false")]
        [(eq? obj 'null)  (display "null")]
        [(eq? obj 'true)  (display "true")]
        [(pair? obj)      (print-object obj)]
        [(vector? obj)    (print-array obj)]
        [(number? obj)    (print-number obj)]
        [(string? obj)    (print-string obj)]
        [else (error <json-construct-error> :object obj
                     "construct-json expects list or vector, but got:" obj)]))

(define (print-object obj)
  (display "{")
  (fold (lambda (attr comma)
          (unless (and (pair? attr)
                       (string? (car attr)))
            (error <json-construct-error> :object obj
                   "construct-json needs an assoc list with all keys being \
                    string, but got:" obj))
          (display comma)
          (print-string (car attr))
          (display ":")
          (print-value (cdr attr))
          ",")
        "" obj)
  (display "}"))

(define (print-array obj)
  (display "[")
  (vector-for-each (lambda (i val)
                     (unless (zero? i) (display ","))
                     (print-value val))
                   obj)
  (display "]"))

(define (print-number num)
  (cond [(or (not (real? num)) (not (finite? num)))
         (error <json-construct-error> :object num
                "json cannot represent a number" num)]
        [(and (rational? num) (not (integer? num)))
         (write (exact->inexact num))]
        [else (write num)]))

(define (print-string str)
  (define (print-char c)
    (cond [(eqv? c #\") (write-char #\\) (write-char c)]
          [(char-set-contains? char-set:ascii c) (write-char c)]
          [else (format #t "\\u~4,0x" (char->ucs c))]))
  (display "\"")
  (string-for-each print-char str)
  (display "\""))

(define (construct-json x :optional (oport (current-output-port)))
  (with-output-to-port oport
    (^()
      (cond [(pair? x)   (print-object x)]
            [(vector? x) (print-array x)]
            [else (error <json-construct-error> :object x
                         "construct-json expects a list or a vector, \
                          but got" x)]))))

(define (construct-json-string x)
  (call-with-output-string (cut construct-json x <>)))

