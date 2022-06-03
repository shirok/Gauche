;;;
;;; srfi-180 - JSON
;;;
;;;   Copyright (c) 2020-2022  Shiro Kawai  <shiro@acm.org>
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

;; A wrapper of rfc.json

(define-module srfi-180
  (use gauche.vport)
  (use parser.peg)
  (use scheme.list)
  (use rfc.json)
  (export json-error? json-error-reason json-null?

          json-nesting-depth-limit  ; in rfc.json
          json-number-of-character-limit

          json-generator json-fold
          json-read json-lines-read json-sequence-read
          json-accumulator json-write
          ))
(select-module srfi-180)

(define (json-error? obj)
  (or (is-a? obj <json-parse-error>)
      (is-a? obj <json-construct-error>)))

(define (json-error-reason obj)
  (~ obj'message))

(define (json-null? obj) (eq? obj 'null))

;; json-nesting-depth-limit is in rfc.json

(define json-number-of-character-limit (make-parameter +inf.0))


;; internal
;; we use peek-char to keep the last read char in the port, since
;; lseq read ahead one character and we don't want that character to
;; be missed in the subsequent read from the port.
(define (port/gen->json-lseq port)
  (define nchars 0)
  (if (port? port)
    (generator->lseq
     (^[]
       (unless (= nchars 0) (read-char port))
       (let1 c (peek-char port)
         (cond [(eof-object? c) c]
               [(>= nchars (json-number-of-character-limit))
                (error <json-parse-error>
                       "Input length exceeds json-number-of-character-limit")]
               [else (inc! nchars) c]))))
    (generator->lseq port)))            ;assume it's a char-generator

;; API: streaming parser
;; NB: srfi's json-generator doesn't take a char generator, but for
;; the upper layers, we accept it for the convenience.
(define (json-generator :optional (port (current-input-port)))
  (define inner-gen
    (peg-parser->generator json-tokenizer (port/gen->json-lseq port)))
  (define (nexttok)
    (guard (e ([<parse-error> e]
               ;; not to expose parser.peg's <parse-error>.
               (error <json-parse-error>
                      :position (~ e'position) :objects (~ e'objects)
                      :message (~ e'message))))
      (case (inner-gen)
        [(true) #t]
        [(false) #f]
        [else => identity])))
  (define nesting '())
  (define (push-nesting kind)
    (when (>= (length nesting) (json-nesting-depth-limit))
      (error <json-parse-error> "Input JSON nesting is too deep."))
    (push! nesting (cons kind gen)))
  (define (pop-nesting kind)
    (unless (pair? nesting)
      (errorf <json-parse-error> "Stray close ~a" kind))
    (unless (eq? (caar nesting) kind)
      (errorf <json-parse-error> "Unmatched open ~a" (car nesting)))
    (set! gen (cdr (pop! nesting))))
  (define (badtok tok)
    (error <json-parse-error> (format "Invalid token: ~s" tok)))

  ;; Read one value.  array-end and object-end are returned as-is, and the
  ;; caller should handle it---they can appear in value context input is empty
  ;; array and empty object.
  (define (value)
    (case (nexttok)
      [(array-start)
       (push-nesting 'array)
       (set! gen array-element)
       'array-start]
      [(object-start)
       (push-nesting 'object)
       (set! gen object-key)
       'object-start]
      [(#\: #\,) => badtok]
      [else => identity]))

  ;; State machine
  ;;  Each state is a thunk, set! to variable 'gen'.

  ;; Initial state
  (define (init)
    (set! gen fini)
    (case (value)
      [(array-end object-end) => badtok]
      [else => identity]))

  ;; We've already read a whole item.
  (define (fini) (eof-object))

  ;; Reading an array element.
  (define (array-element)
    (set! gen array-element-after)
    (case (value)
      [(array-end) (pop-nesting 'array) 'array-end]
      [(object-end) => badtok]
      [else => identity]))

  ;; Just read an array element.
  (define (array-element-after)
    (case (nexttok)
      [(#\,) (array-element)]
      [(array-end) (pop-nesting 'array) 'array-end]
      [else => badtok]))

  ;; Reading an object key
  (define (object-key)
    (let1 t (nexttok)
      (cond [(string? t) (set! gen object-key-after) t]
            [(eq? t 'object-end) (pop-nesting 'object) 'object-end]
            [else (badtok t)])))

  ;; Just read an object key
  (define (object-key-after)
    (case (nexttok)
      [(#\:)
       (set! gen object-value-after)
       (case (value)
         [(array-end object-end) => badtok]
         [else => identity])]
      [else => badtok]))

  ;; Just read an object value
  (define (object-value-after)
    (case (nexttok)
      [(#\,) (object-key)]
      [(object-end) (pop-nesting 'object) 'object-end]
      [else => badtok]))

  ;; 'gen' will be set! to the next state handler.
  (define gen init)
  ;; Entry point
  (^[] (gen)))

;; API
(define (json-fold proc array-start array-end object-start object-end seed
                   :optional (port-or-generator (current-input-port)))
  (define gen (json-generator port-or-generator))
  (define (rec seed stack)
    (let1 tok (gen)
      (cond [(eof-object? tok) seed]
            [(eq? tok 'array-start) (rec (array-start seed) (cons seed stack))]
            [(eq? tok 'array-end) (rec (proc (array-end seed) (car stack))
                                    (cdr stack))]
            [(eq? tok 'object-start) (rec (object-start seed)
                                       (cons seed stack))]
            [(eq? tok 'object-end) (rec (proc (object-end seed) (car stack))
                                     (cdr stack))]
            [else (rec (proc tok seed) stack)])))
  (rec seed '()))

;; Common utility to adapt rfc.json with srfi-180 API.  We skip
;; json-fold/json-generator stuff entirely.
(define (with-json-parser proc lseq)
  (parameterize ((json-special-handler (^x (case x
                                             [(true) #t]
                                             [(false) #f]
                                             [else x])))
                 (json-object-handler (^x (map (^p `(,(string->symbol (car p))
                                                     . ,(cdr p)))
                                               x))))
    (guard (e ([<parse-error> e]
               ;; not to expose parser.peg's <parse-error>.
               (error <json-parse-error>
                      :position (~ e'position) :objects (~ e'objects)
                      :message (~ e'message))))
      (proc lseq))))

;; API
;; We skip json-fold/json-generator stuff entirely.
(define (json-read :optional (port-or-generator (current-input-port)))
  (with-json-parser
   (^s (values-ref (peg-run-parser json-parser s) 0))
   (port/gen->json-lseq port-or-generator)))

;; API
(define (json-lines-read :optional (port-or-generator (current-input-port)))
  (let1 lseq (port/gen->json-lseq port-or-generator)
    (^[]
      (with-json-parser
       (^s (receive (r next) (peg-run-parser json-parser s)
             (set! lseq next)
             r))
       lseq))))

;; API
;; <json-sequence> : ( #x1e json-text )*
;; Skip unparsable json-text.
(define (json-sequence-read :optional (port-or-generator (current-input-port)))
  (define (skip-to-rs lseq)
    (drop-while (^c (not (eqv? c #\x1e))) lseq))
  (define (skip-rs lseq)
    (drop-while (^c (eqv? c #\x1e)) lseq))
  (define (fetch lseq)
    (let1 start (skip-rs lseq)
      (if (null? start)
        (values (eof-object) '())
        (receive (json next)
            (guard (e [(<json-parse-error> e)
                       (values 'error (skip-to-rs start))])
              (with-json-parser (cut peg-run-parser json-parser <>) start))
          (if (eq? json 'error)
            (fetch next)
            (values json next))))))
  (let1 lseq (skip-to-rs (port/gen->json-lseq port-or-generator))
    (^[] (receive (json next) (fetch lseq)
           (set! lseq next)
           json))))

;; API
(define (json-accumulator port-or-accumulator)
  (define acc (if (port? port-or-accumulator)
                (^o (if (string? o)
                      (write-string o port-or-accumulator)
                      (write-char o port-or-accumulator)))
                port-or-accumulator))
  (define (err-input obj)
    (error <json-construct-error>
           :message (format "Unexpected token ~a as json-accumulator input"
                            obj)))
  (define stack '()) ; (kind . #<proc>)

  (define (push-state!) (push! stack gen))
  (define (pop-state! tok)
    (when (null? stack) (err-input tok))
    (set! gen (pop! stack)))

  (define (value obj)
    (cond
     [(eof-object? obj)]
     [(eq? obj 'array-start) (acc #\[) (push-state!) (set! gen a0)]
     [(eq? obj 'object-start) (acc #\{) (push-state!) (set! gen k0)]
     [(or (eq? obj 'null) (boolean? obj)
          (string? obj) (number? obj))
      (acc (construct-json-string obj))]
     [else (err-input obj)]))

  (define (init obj)
    (set! gen fini)
    (value obj))

  (define (fini obj)
    (unless (eof-object? obj)
      (error <json-construct-error>
             :message "json-accumulator no longer accepting events.")))

  ;; initial element of array
  (define (a0 obj)
    (if (eq? obj 'array-end)
      (begin (acc #\]) (pop-state! obj))
      (begin (set! gen a1) (value obj))))

  ;; subsequent element of array
  (define (a1 obj)
    (if (eq? obj 'array-end)
      (begin (acc #\]) (pop-state! obj))
      (begin (acc #\,) (value obj))))

  ;; initial key of object
  (define (k0 obj)
    (cond
     [(eq? obj 'object-end) (acc #\}) (pop-state! obj)]
     [(string? obj) (acc obj) (set! gen v)]
     [else (err-input obj)]))

  ;; value of object
  (define (v obj)
    (set! gen k1)
    (acc #\:)
    (value obj))

  ;; subsequent key of object
  (define (k1 obj)
    (cond
     [(eq? obj 'object-end) (acc #\}) (pop-state! obj)]
     [(string? obj) (acc #\,) (acc obj) (set! gen v)]
     [else (err-input obj)]))

  (define gen init)
  (^[obj] (gen obj)))

;; API
(define (json-write obj :optional (port-or-accumulator (current-output-port)))
  (construct-json obj (if (port? port-or-accumulator)
                        port-or-accumulator
                        (open-output-accumulator port-or-accumulator))))
