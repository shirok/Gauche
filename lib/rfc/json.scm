;;;
;;; json.scm - JSON (RFC7159) Parser
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

;;; http://www.ietf.org/rfc/rfc7159.txt

;; NOTE: This module depends on parser.peg, whose API is not officially
;; fixed.  Hence do not take this code as an example of parser.peg;
;; this will likely to be rewritten once parser.peg's API is changed.

(define-module rfc.json
  (use gauche.parameter)
  (use gauche.sequence)
  (use gauche.generator)
  (use parser.peg)
  (use gauche.unicode)
  (use srfi-13)
  (use srfi-14)
  (use srfi-43)
  (export <json-parse-error> <json-construct-error>
          parse-json parse-json-string
          parse-json*
          construct-json construct-json-string

          json-array-handler json-object-handler json-special-handler

          json-parser                   ;experimental
          ))
(select-module rfc.json)

;; NB: We have <json-parse-error> independent from <parse-error> for
;; now, since parser.peg's interface may be changed later.
(define-condition-type <json-parse-error> <error> #f
  (position)                            ;stream position
  (objects))                            ;offending object(s) or messages

(define-condition-type <json-construct-error> <error> #f
  (object))                             ;offending object

(define json-array-handler   (make-parameter list->vector))
(define json-object-handler  (make-parameter identity))
(define json-special-handler (make-parameter identity))

(define (build-array elts) ((json-array-handler) elts))
(define (build-object pairs) ((json-object-handler) pairs))
(define (build-special symbol) ((json-special-handler) symbol))

;;;============================================================
;;; Parser
;;;
(define %ws ($skip-many ($. #[ \t\r\n])))

(define %begin-array     ($seq ($. #\[) %ws))
(define %begin-object    ($seq ($. #\{) %ws))
(define %end-array       ($seq ($. #\]) %ws))
(define %end-object      ($seq ($. #\}) %ws))
(define %name-separator  ($seq ($. #\:) %ws))
(define %value-separator ($seq ($. #\,) %ws))

(define %special
  ($lift ($ build-special $ string->symbol $ rope-finalize $)
         ($or ($. "false") ($. "true") ($. "null"))))

(define %value
  ($lazy
   ($followed-by ($or %special %object %array %number %string) %ws)))

(define %array
  ($lift ($ build-array $ rope-finalize $)
         ($between %begin-array ($sep-by %value %value-separator) %end-array)))

(define %number
  (let* ([%sign ($optional ($->rope ($one-of #[+-])))]
         [%digits ($->rope ($many ($. #[\d]) 1))]
         [%frac ($->rope ($. #\.) ($many ($. #[\d]) 1))]
         [%exp ($->rope ($. #[eE]) %sign %digits)])
    ($lift ($ string->number $ string-concatenate
              $ map rope->string $ list $*)
           %sign %digits ($optional %frac) ($optional %exp))))

(define %unicode
  (let ([%hex4 ($lift (^s (string->number (list->string s) 16))
                      ($many ($. #[\da-fA-F]) 4 4))]
        ;; NB: If we just $fail, the higher-level parser may conceal the
        ;; direct cause of the error by backtracking.  Unpaired surrogate
        ;; is an unrecoverable error, so we throw <json-parse-error> directly.
        ;; There may be a better way to integrate this kind of error in
        ;; the combinators; let's see.
        [err (^c (errorf <json-parse-error>
                         :position #f :object c
                         "unpaired surrogate: \\u~4,'0x" c))])
    ($let ([ ($. #\u) ]
           [c %hex4])
      (cond [(<= #xd800 c #xdbff)
             ($or ($try ($let ([ ($. "\\u") ]
                               [c2 %hex4])
                          (receive (cc x)
                              (utf16->ucs4 `(,c ,c2) 'permissive)
                            (and (null? x)
                                 ($return (ucs->char cc))))))
                  ;; NB: We wrap (err c) with dummy $do to put the call
                  ;; to the err into a parser monad.  Simple ($return (err c))
                  ;; or ($fail (err c)) won't do, since (err c) is evaluated
                  ;; at the parser-construction time, not the actual parsing
                  ;; time.  We need a dummy ($return #t) clause to ensure
                  ;; (err c) is wrapped; ($do x) is expanded to just x.
                  ;; Definitely we need something better to do this kind of
                  ;; operation.
                  ($do [($return #t)] (err c)))]
            [(<= #xdc00 c #xdfff) (err c)]
            [else ($return (ucs->char c))]))))

(define %string
  (let* ([%dquote ($. #\")]
         [%escape ($. #\\)]
         [%special-char
          ($seq %escape
                ($or ($. #\")
                     ($. #\\)
                     ($. #\/)
                     ($seq ($. #\b) ($return #\x08))
                     ($seq ($. #\f) ($return #\page))
                     ($seq ($. #\n) ($return #\newline))
                     ($seq ($. #\r) ($return #\return))
                     ($seq ($. #\t) ($return #\tab))
                     %unicode))]
         [%unescaped ($none-of #[\"])]
         [%body-char ($or %special-char %unescaped)]
         [%string-body ($->rope ($many %body-char))])
    ($between %dquote %string-body %dquote)))

(define %object
  (let1 %member ($let ([k %string]
                       [ %ws ]
                       [ %name-separator ]
                       [v %value])
                  ($return (cons k v)))
    ($between %begin-object
              ($lift ($ build-object $ rope-finalize $)
                     ($sep-by %member %value-separator))
              %end-object)))

(define json-parser ($seq %ws ($or ($eos) %value)))

;; entry point
(define (parse-json :optional (port (current-input-port)))
  (guard (e [(<parse-error> e)
             ;; not to expose parser.peg's <parse-error>.
             (error <json-parse-error>
                    :position (~ e'position) :objects (~ e'objects)
                    :message (~ e'message))])
    (peg-parse-port json-parser port)))

(define (parse-json-string str)
  (call-with-input-string str (cut parse-json <>)))

(define (parse-json* :optional (port (current-input-port)))
  (guard (e [(<parse-error> e)
             ;; not to expose parser.peg's <parse-error>.
             (error <json-parse-error>
                    :position (~ e'position) :objects (~ e'objects)
                    :message (~ e'message))])
    (generator->list (peg-parser->generator json-parser port))))

;;;============================================================
;;; Writer
;;;

(define (print-value obj)
  (cond [(or (eq? obj 'false) (eq? obj #f)) (display "false")]
        [(or (eq? obj 'true) (eq? obj #t))  (display "true")]
        [(eq? obj 'null)  (display "null")]
        [(list? obj)      (print-object obj)]
        [(string? obj)    (print-string obj)]
        [(number? obj)    (print-number obj)]
        [(is-a? obj <dictionary>) (print-object obj)]
        [(is-a? obj <sequence>)   (print-array obj)]
        [else (error <json-construct-error> :object obj
                     "can't convert Scheme object to json:" obj)]))

(define (print-object obj)
  (display "{")
  (fold (^[attr comma]
          (unless (pair? attr)
            (error <json-construct-error> :object obj
                   "construct-json needs an assoc list or dictionary, \
                    but got:" obj))
          (display comma)
          (print-string (x->string (car attr)))
          (display ":")
          (print-value (cdr attr))
          ",")
        "" obj)
  (display "}"))

(define (print-array obj)
  (display "[")
  (for-each-with-index (^[i val]
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
  (define specials
    '((#\" . #\") (#\\ . #\\) (#\x08 . #\b) (#\page . #\f)
      (#\newline . #\n) (#\return . #\r) (#\tab . #\t)))
  (define (hexescape code) (format #t "\\u~4,'0x" code))
  (define (print-char c)
    (cond [(assv c specials) => (^p (write-char #\\) (write-char (cdr p)))]
          [(and (char-set-contains? char-set:ascii c)
                (not (eq? (char-general-category c) 'Cc)))
           (write-char c)]
          [else
           (let1 code (char->ucs c)
             (if (>= code #x10000)
               (for-each hexescape (ucs4->utf16 code))
               (hexescape code)))]))
  (display "\"")
  (string-for-each print-char str)
  (display "\""))

(define (construct-json x :optional (oport (current-output-port)))
  (with-output-to-port oport
    (^()
      (cond [(or (list? x) (is-a? x <dictionary>)) (print-object x)]
            [(and (is-a? x <sequence>) (not (string? x))) (print-array x)]
            [else (error <json-construct-error> :object x
                         "construct-json expects a list or a vector, \
                          but got" x)]))))

(define (construct-json-string x)
  (call-with-output-string (cut construct-json x <>)))

