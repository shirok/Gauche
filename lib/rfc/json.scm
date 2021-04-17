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

(define-module rfc.json
  (use gauche.sequence)
  (use gauche.generator)
  (use gauche.unicode)
  (use scheme.charset)
  (use parser.peg)
  (use srfi-13)
  (use srfi-113)
  (export <json-parse-error> <json-construct-error>
          parse-json parse-json-string
          parse-json*
          construct-json construct-json-string

          json-array-handler json-object-handler json-special-handler
          json-nesting-depth-limit

          json-parser json-tokenizer
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

;; This is to bail out the input nesting is too deep (see srfi-180)
(define json-nesting-depth-limit (make-parameter +inf.0))
(define json-current-nesting-depth (make-parameter 0))

;;;============================================================
;;; Parser
;;;
(define %ws ($many_ ($. #[ \t\r\n])))

(define %begin-array     ($seq ($. #\[) %ws))
(define %begin-object    ($seq ($. #\{) %ws))
(define %end-array       ($seq ($. #\])))
(define %end-object      ($seq ($. #\})))
(define %name-separator  ($seq ($. #\:) %ws))
(define %value-separator ($seq ($. #\,) %ws))

(define %special
  ($lift ($ build-special $ string->symbol $ rope-finalize $)
         ($or ($. "false") ($. "true") ($. "null"))))

(define %value
  ($lazy ($or %special %object %array %number %string)))

(define ($depth-check parser)
  (^s (let1 d (json-current-nesting-depth)
        (if (>= d (json-nesting-depth-limit))
          (error <json-parse-error>
                 "Input JSON nesting is too deep.")
          (parameterize ([json-current-nesting-depth (+ d 1)])
            (parser s))))))

(define %array
  ($depth-check
   ($lift ($ build-array $ rope-finalize $)
          ($between %begin-array
                    ($sep-by ($seq0 %value %ws) %value-separator)
                    %end-array))))

(define %sign ($optional ($. #[+-])))
(define %digits ($many1 ($. #[\d])))

(define %number
  ($binding ($: sign   %sign)
            ($: digits %digits)
            ($: frac   ($optional ($->rope ($. #\.) ($many1 ($. #[\d])))))
            ($: exp    ($optional ($->rope ($. #[eE]) %sign %digits)))
            (string->number (rope->string (list sign digits frac exp)))))

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
                  ;; NB: We wrap (err c) with dummy $let to put the call
                  ;; to the err into a parser monad.  Simple ($return (err c))
                  ;; or ($fail (err c)) won't do, since (err c) is evaluated
                  ;; at the parser-construction time, not the actual parsing
                  ;; time.  We need a dummy ($return #t) clause to ensure
                  ;; (err c) is wrapped; ($let () x) is expanded to just x.
                  ;; Definitely we need something better to do this kind of
                  ;; operation.
                  ($let ([($return #t)]) (err c)))]
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
  (let1 %member ($binding ($: k %string)
                          %ws
                          %name-separator
                          ($: v %value)
                          %ws
                          (cons k v))
    ($depth-check
     ($between %begin-object
               ($lift ($ build-object $ rope-finalize $)
                      ($sep-by %member %value-separator))
               %end-object))))

(define json-parser ($parameterize ([json-current-nesting-depth 0])
                       ($seq %ws ($or ($eos) %value))))

;; for streaming parser
(define json-tokenizer
  ($seq %ws
        ($or %special %number %string
             ($seq %begin-array  ($return 'array-start))
             ($seq %end-array    ($return 'array-end))
             ($seq %begin-object ($return 'object-start))
             ($seq %end-object   ($return 'object-end))
             ($seq %name-separator ($return #\:))
             ($seq %value-separator ($return #\,)))))

;; entry point
(define (parse-json :optional (port (current-input-port)))
  (guard (e [(<parse-error> e)
             ;; not to expose parser.peg's <parse-error>.
             (error <json-parse-error>
                    :position (~ e'position) :objects (~ e'objects)
                    :message (condition-message e e))])
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
    (^() (print-value x))))

(define (construct-json-string x)
  (call-with-output-string (cut construct-json x <>)))
