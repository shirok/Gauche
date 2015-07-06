;;;
;;; csv.scm - read and write CSV (actually, xSV) format.
;;;
;;;   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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

(define-module text.csv
  (use srfi-1)
  (use srfi-11)
  (use srfi-13)
  (use srfi-42)
  (use gauche.sequence)
  (export make-csv-reader
          make-csv-writer
          make-csv-header-parser
          make-csv-record-parser
          csv-rows->tuples)
  )
(select-module text.csv)

;;;
;;;Low-level API - convert text into nested lists
;;;

;; API
(define (make-csv-reader separator :optional (quote-char #\"))
  (^[:optional (port (current-input-port))]
    (csv-reader separator quote-char port)))

(define (csv-reader sep quo port)
  (define (eor? ch) (or (eqv? ch #\newline) (eof-object? ch)))

  (define (start fields)
    (let1 ch (read-char port)
      (cond [(eor? ch) (reverse! (cons "" fields))]
            [(eqv? ch sep) (start (cons "" fields))]
            [(eqv? ch quo) (quoted fields)]
            [(char-whitespace? ch) (start fields)]
            [else (unquoted (list ch) fields)])))

  (define (unquoted chs fields)
    (let loop ([ch (read-char port)] [last chs] [chs chs])
      (cond [(eor? ch) (reverse! (cons (finish last) fields))]
            [(eqv? ch sep) (start (cons (finish last) fields))]
            [(char-whitespace? ch) (loop (read-char port) last (cons ch chs))]
            [else (let1 chs (cons ch chs)
                    (loop (read-char port) chs chs))])))

  (define (finish rchrs) (list->string (reverse! rchrs)))

  (define (quoted fields)
    (let loop ([ch (read-char port)] [chs '()])
      (cond [(eof-object? ch) (error "unterminated quoted field")]
            [(eqv? ch quo)
             (if (eqv? (peek-char port) quo)
               (begin (read-char port) (loop (read-char port) (cons quo chs)))
               (quoted-tail (cons (finish chs) fields)))]
            [else (loop (read-char port) (cons ch chs))])))

  (define (quoted-tail fields)
    (let loop ([ch (read-char port)])
      (cond [(eor? ch) (reverse! fields)]
            [(eqv? ch sep) (start fields)]
            [else (loop (read-char port))])))

  (if (eof-object? (peek-char port))
    (eof-object)
    (start '())))

;; API
(define (make-csv-writer separator :optional (newline "\n") (quote-char #\"))
  (let* ([quote-string (string quote-char)]
         [quote-escape (string-append quote-string quote-string)]
         [quote-rx (string->regexp (regexp-quote quote-string))]
         [separator-chars (if (string? separator)
                            (string->list separator)
                            (list separator))]
         [special-chars
          (apply char-set quote-char #\space #\newline #\return
                 separator-chars)])

    (^[port fields]
      (define (write-a-field field)
        (if (string-index field special-chars)
          (begin (display quote-char port)
                 (display (regexp-replace-all quote-rx field quote-escape) port)
                 (display quote-char port))
          (display field port)))

      (unless (null? fields)
        (write-a-field (car fields))
        (dolist [field (cdr fields)]
          (display separator port)
          (write-a-field field)))
      (display newline port))))

;;;
;;;Middle-level API
;;;

;; Occasionally, CVS files generated from spreadsheet contains
;; superfluous rows/columns and we need to make sense of them.
;; Here are some utilities to help them.

;; API
;; Create a procedure that detects a row that contains the specified
;; slots.  Returns a permuter vector, which is a vector of integers,
;; where K-th element being I to mean the K-th slot value should be
;; taken from I-th column.
;;
;; Example:
;; input data:  ("" "" "Year" "Country" "" "Population" "GDP")
;; header-slots: ("Country" "Year" "GDP" "Population")
;; result: #(3 2 6 5)
(define (make-csv-header-parser header-slots)
  (define tab (rlet1 t (make-hash-table 'equal?)
                (do-ec (: s (index k) header-slots)
                       (hash-table-put! t s k))))
  (define num-slots (hash-table-num-entries tab))
  (^[row]
    (and (= num-slots
            (fold (^[c n] (+ n (if (hash-table-exists? tab c) 1 0))) 0 row))
         (rlet1 permuter (make-vector num-slots)
           (do-ec (: c (index i) row)
                  (and-let1 k (hash-table-get tab c #f)
                    (set! (~ permuter k) i)))))))

;; API
;; Create a procedure that converts one input row into a list of slot
;; values, orderd in the same way as header-slots. 
;; The permuter is the vector returned by make-csv-header-parser.
;; Required-slots determines if the input row is valid or not.  If
;; not, #f is returned.
;;
;;   required-slots : (<spec> ...)
;;   <spec> : slot-name | (slot-name predicate)
;;
;; A single slot-name in <spec> means `(,slot-name ,(complement string-null?))
;; If required-slots is omitted or (), a row is regarded as valid if
;; there's at least one non-null slots.
(define (make-csv-record-parser header-slots permuter
                                :optional (required-slots '()))
  (define (make-requirement-checker spec)
    (let* ([slot (if (string? spec) spec (car spec))]
           [pred (if (string? spec) (complement string-null?) (cadr spec))]
           [ind (find-index (cut equal? slot <>) header-slots)])
      (unless ind
        (error "make-csv-record-parser: invalid slot name in required-slots:"
               spec))
      (^[slot-values] (pred (list-ref slot-values ind)))))
  (define check
    (if (null? required-slots)
      (^[slot-values] (not (every string-null? slot-values)))
      (apply every-pred (map make-requirement-checker required-slots))))
  (^[row]
    (let1 slot-values (permute row permuter)
      (and (check slot-values) slot-values))))

;; API
;; Convert input rows to a list of tuples (A tuple is a list of slot values)
;; If no header is found, #f is returned.
;; If allow-gap? is #t, it keeps reading rows until the end, skipping
;; invalid rows.  If allow-gap? is #f, it stops reading once it sees
;; an invalid row after headers.
(define (csv-rows->tuples rows header-slots
                          :key (required-slots '())
                               (allow-gap? #f))
  (define header-parser (make-csv-header-parser header-slots))
  (let header-loop ([rows rows])
    (cond [(null? rows) #f] ; no header found
          [(header-parser (car rows))
           => (^[permuter]
                (define record-parser
                  (make-csv-record-parser header-slots
                                          permuter
                                          required-slots))
                (let record-loop ([rows (cdr rows)] [r '()])
                  (cond [(null? rows) (reverse r)]
                        [(record-parser (car rows))
                         => (^[tuple] (record-loop (cdr rows) (cons tuple r)))]
                        [allow-gap? (record-loop (cdr rows) r)]
                        [else (reverse r)])))]
          [else (header-loop (cdr rows))])))

