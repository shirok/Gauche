;;;
;;; util.stream - stream library (srfi-40, 41 & more)
;;;
;;;   [SK] This module includes code from streams-ext.scm,
;;;   written by Alejandro Forero Cuervo and released in Public Domain
;;;
;;;   The rest is written by Shiro Kawai
;;;
;;;   Copyright (c) 2019  Shiro Kawai  <shiro@acm.org>
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

(define-module util.stream
  (use srfi-1)
  (use util.match)
  (export
   ;;  srfi-40
   stream? stream-null stream-cons stream-null?
   stream-pair? stream-car stream-cdr stream-delay stream
   stream-unfoldn stream-map stream-for-each stream-filter

   ;; srfi-41 additions
   stream+ stream-lambda define-stream stream-unfold stream-unfolds
   list->stream port->stream stream->list stream-append stream-concat
   stream-constant stream-drop-while stream-take-while
   stream-range stream-fold stream-from stream-iterate
   stream-length stream-ref stream-reverse stream-scan stream-zip
   stream-match stream-of

   ;; extras
   generator->stream stream-concatenate
   
   stream-xcons stream-cons* make-stream stream-tabulate
   stream-iota stream-format stream-lines 
   string->stream stream->string
   number->stream stream->number symbol->stream stream->symbol
   iterator->stream
   stream= stream-prefix=
   stream-caar stream-cadr stream-cdar stream-cddr
   stream-caaar stream-caadr stream-cadar stream-caddr
   stream-cdaar stream-cdadr stream-cddar stream-cdddr
   stream-caaaar stream-caaadr stream-caadar stream-caaddr
   stream-cadaar stream-cadadr stream-caddar stream-cadddr
   stream-cdaaar stream-cdaadr stream-cdadar stream-cdaddr
   stream-cddaar stream-cddadr stream-cdddar stream-cddddr
   stream-first stream-second stream-third stream-fourth
   stream-fifth stream-sixth stream-seventh stream-eighth
   stream-ninth stream-tenth
   stream-take-safe stream-take stream-drop-safe stream-drop
   stream-intersperse stream-split stream-last stream-last-n
   stream-butlast stream-butlast-n stream-length>= stream-length=
   stream-count
   stream-remove stream-partition stream-find stream-find-tail
   stream-span stream-break
   stream-any stream-every stream-index
   stream-member stream-memq stream-memv
   stream-delete stream-delete-duplicates
   stream-grep ->stream-char stream-replace stream-translate
   write-stream
   ))
(select-module util.stream)

;;;
;;; <stream> type is a promise with 'stream in its kind.
;;;

(define-inline (stream? s)
  (and (promise? s) (eq? (promise-kind s) 'stream)))

(define-inline (%make-stream promise)
  (set! (promise-kind promise) 'stream)
  promise)

;;;
;;; Primitives
;;;

;; srfi-40, 41
;; A singleton instance of null stream
(define stream-null (%make-stream (delay '())))

;; srfi-40, 41
(define-syntax stream-cons
  (syntax-rules ()
    [(stream-cons obj strm)
     (%make-stream (delay (cons obj strm)))]))

;; stri-40, 41
(define-inline (stream-null? obj)
  (and (stream? obj) (null? (force obj))))
(define-inline (stream-pair? obj)
  (and (stream? obj) (pair? (force obj))))
(define-inline (stream-car s) (car (force s)))
(define-inline (stream-cdr s) (cdr (force s)))

;; srfi-40
(define-syntax stream-delay
  (syntax-rules ()
    [(stream-delay expr)
     (%make-stream (lazy expr))]))

;; srfi-41
(define-syntax stream-lambda
  (syntax-rules ()
    [(_ formals body0 body1 ...)
     (lambda formals (stream-delay (let () body0 body1 ...)))]))

;;;
;;; Derived
;;;

;; srfi-40 (objs are evaluated first)
(define (stream . objs)
  (if (null? objs)
    stream-null
    (stream-cons (car objs) (apply stream (cdr objs)))))

;; srfi-41's stream (evaluation of objs are delayed)
(define-syntax stream+
  (syntax-rules ()
    [(_) stream-null]
    [(_ x y ...) (stream-cons x (stream+ y ...))]))

;; srfi-41
(define-syntax define-stream
  (syntax-rules ()
    [(_ (name . formal) body0 body1 ...)
     (define name (stream-lambda formal body0 body1 ...))]))

;; srfi-41
;; NB: The argument order differs from srfi-1#unfold. Also, predicate is
;; to continue, as oppsed to the stop predicate in srfi-1#unfold.
(define-stream (stream-unfold f p g seed)
  (if (p seed)
    (stream-cons (f seed) (stream-unfold f p g (g seed)))
    stream-null))

;; srfi-40
(define (stream-unfoldn f seed n)
  ;; stream of N-tuples of the results
  (define rstream
    ((rec (loop seed)
       (stream-delay
        (receive (seed . rs) (f seed)
          (stream-cons rs (loop seed)))))
     seed))
  ;; create N-th stream
  (define (nth-stream rss i)
    (stream-delay
     (let1 r (~ (stream-car rss) i)
       (cond [(pair? r) (stream-cons (car r) (nth-stream (stream-cdr rss) i))]
             [(not r) (nth-stream (stream-cdr rss) i)]
             [else stream-null]))))
  (apply values (map (cute nth-stream rstream <>) (iota n))))

;; srfi-41
;; Similar to stream-unfoldn, but the number of result streams is inferred
;; from the number of returned values from f.
(define (stream-unfolds f seed)
  (receive vs (f seed)
    (stream-unfoldn f seed (- (length vs) 1))))

;; srfi-40, 41
(define (stream-map f s . ss)
  (if (null? ss)
    (let loop [(s s)]
      (stream-delay
       (if (stream-null? s)
         s
         (stream-cons (f (stream-car s)) (loop (stream-cdr s))))))
    (let loop ([ss (cons s ss)])
      (stream-delay
       (if (any stream-null? ss)
         stream-null
         (stream-cons (apply f (map stream-car ss))
                      (loop (map stream-cdr ss))))))))

;; srfi-40, 41
(define (stream-for-each f s . ss)
  (if (null? ss)
    (let loop [(s s)]
      (unless (stream-null? s)
        (f (stream-car s))
        (loop (stream-cdr s))))
    (let loop [(ss (cons s ss))]
      (unless (any stream-null? ss)
        (apply f (map stream-car ss))
        (loop (map stream-cdr ss))))))

;; srfi-40, 41
(define-stream (stream-filter p s)
  (cond [(stream-null? s) s]
        [(p (stream-car s)) (stream-cons (stream-car s) 
                                         (stream-filter p (stream-cdr s)))]
        [else (stream-filter p (stream-cdr s))]))

;; srfi-41
(define (list->stream lis)
  (stream-unfold car pair? cdr lis))

(define (generator->stream gen :optional (fini #f))
  ((rec (next)
     (stream-delay
      (let1 v (gen)
        (cond [(eof-object? v)
               (when fini (fini))
               stream-null]
              [else (stream-cons v (next))]))))))

;; srfi-41
;; reader and close-at-eof are Gauche extension.
(define (port->stream :optional (in (current-input-port))
                                (reader read-char)
                                (close-at-eof #f))
  (generator->stream (cut reader in)
                     (^[] (when close-at-eof (close-at-eof in)))))

;; srfi-41
(define stream->list
  (case-lambda
    [(n s)
     (assume n <integer>)
     (assume (stream? s))
     (let loop ([n n] [s s] [r '()])
       (cond [(<= n 0) (reverse r)]
             [(stream-null? s) (reverse r)]
             [else (loop (- n 1) (stream-cdr s) (cons (stream-car s) r))]))]
    [(s)
     (assume (stream? s))
     (let loop ([s s] [r '()])
       (if (stream-null? s)
         (reverse r)
         (loop (stream-cdr s) (cons (stream-car s) r))))]))

;; srfi-41
(define (stream-append . ss)
  (stream-delay
   (cond [(null? ss) stream-null]
         [(null? (cdr ss)) (car ss)]
         [else
          (let loop ([s (car ss)] [ss (cdr ss)])
            (stream-delay
             (if (stream-null? s)
               (apply stream-append ss)
               (stream-cons (stream-car s) (loop (stream-cdr s) ss)))))])))

;; srfi-41
(define (stream-concat ss)
  (stream-delay
   (if (stream-null? ss)
     stream-null
     (stream-append (stream-car ss)
                    (stream-concat (stream-cdr ss))))))

;; for the backward compatibility
(define stream-concatenate stream-concat)

;; srfi-41
(define (stream-constant . objs)
  (list->stream (apply circular-list objs)))

;; srfi-41
(define-stream (stream-drop-while pred s)
  (if (or (stream-null? s) (not (pred (stream-car s))))
    s
    (stream-drop-while pred (stream-cdr s))))

;; srfi-41
(define-stream (stream-take-while pred s)
  (if (or (stream-null? s) (not (pred (stream-car s))))
    stream-null
    (stream-cons (stream-car s)
                 (stream-take-while pred (stream-cdr s)))))

;; srfi-41
(define (stream-fold f seed s)
  (let loop ([seed seed] [s s])
    (if (stream-null? s)
      seed
      (loop (f seed (stream-car s)) (stream-cdr s)))))

;; srfi-41
(define (stream-from start :optional (step 1))
  (stream-range start +inf.0 step))

;; srfi-41
;; NB: End being optional is Gauche's extension
(define (stream-range start :optional (end +inf.0) step)
  (assume real? start)
  (assume real? end)
  (let* ([step (if (undefined? step)
                 (if (<= start end) 1 -1)
                 (begin (assume real? step) step))]
         [cmp (if (negative? step) > <)])
    (if (and (exact? start) 
             (or (exact? end) (infinite? end))
             (exact? step))
      (let loop ((start start))
        (stream-delay
         (if (cmp start end) 
           (stream-cons start (loop (+ start step)))
           stream-null)))
      (let loop ((k 0))
        (stream-delay
         (let1 v (+. start (* k step))
           (if (cmp v end)
             (stream-cons v (loop (+ k 1)))
             stream-null)))))))

;; srfi-41
(define-stream (stream-iterate proc base)
  (stream-cons base (stream-iterate proc (proc base))))

;; srfi-41
(define (stream-length s)
  (let loop ([n 0] [s s])
    (if (stream-null? s) n (loop (+ n 1) (stream-cdr s)))))

;; srfi-41
(define-syntax stream-let
  (syntax-rules ()
    [(_ tag ((var expr) ...) body0 body1 ...)
     ((letrec ((tag (stream-lambda (var ...) body0 body1 ...))) tag)
      expr ...)]))

;; srfi-41
(define (stream-ref s n)
  (if (zero? n)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

;; srfi-41
(define (stream-reverse s)
  (stream-fold stream-xcons stream-null s))

;; srfi-41
(define-stream (stream-scan f seed s)
  (if (stream-null? s)
    (stream-cons seed stream-null)
    (stream-cons seed 
                 (stream-scan f (f seed (stream-car s)) (stream-cdr s)))))

;; srfi-41
(define (stream-zip . ss)
  (define-stream (rec ss)
    (if (any stream-null? ss)
      stream-null
      (stream-cons (map stream-car ss) (rec (map stream-cdr ss)))))
  (rec ss))

;;;
;;; srfi-41 matcher
;;;

(define-syntax stream-match
  (er-macro-transformer
   (^[f r c]
     ;; returns length of normal patvars, and flag of rest var
     (define (patlen pat)
       (let loop ([pat pat] [n 0])
         (cond [(null? pat) (values n #f)]
               [(pair? pat) (loop (cdr pat) (+ n 1))]
               [(or (symbol? pat) (identifier? pat)) (values n #t)]
               [else (errorf "Bad pattern element ~s in stream-match: ~s" f)])))
     (define (flatpat pat)
       (map* identity (^t (if (null? t) '() (list t))) pat))
     ;; produce one cond clause
     (define (expand-clause s clause)
       (match clause
         [(pat expr)
          (receive (n rest?) (patlen pat)
            (if rest?
              (quasirename r
                `[(stream-length>= ,s ,n)
                  (apply (lambda ,(flatpat pat) ,expr) 
                         (stream->list+stream ,s ,n))])
              (quasirename r
                `[(stream-length= ,s ,n)
                  (apply (lambda ,pat ,expr) (stream->list ,n ,s))])))]
         [(pat fender expr)
          (receive (n rest?) (patlen pat)
            (if rest?
              (quasirename r
                `[(and-let* ([ (stream-length>= ,s ,n) ]
                             [args (stream->list+stream ,s ,n)]
                             [ (apply (lambda ,(flatpat pat) ,fender) args) ])
                    args)
                  => (cut apply (lambda ,(flatpat pat) ,expr) <>)])
              (quasirename r
                `[(and-let* ([ (stream-length= ,s ,n) ]
                             [args (stream->list ,n ,s)]
                             [ (apply (lambda ,par ,fender) args) ])
                    args)
                  => (cut apply (lambda ,pat ,expr) <>)])))]
         [_ (error "invalid clause ~s in stream-match: ~s" clause f)]))
          
     (match f
       [(_ stream clause ...)
        (when (null? clause)
          (error "malformed stream-match: no clauses:" f))
        (quasirename r
          `(let1 s ,stream
             (cond ,@(map (cut expand-clause (r's) <>) clause))))]
       [_ (error "malformed stream-match:" f)]))))

;; Aux fn for stream-match
;; {a b c d e ...} => (a b c {d e ...}) etc.
;; assume we alreadh check s has more than n elts.
(define (stream->list+stream s n)
  (if (= n 0)
    (list s)
    (cons (stream-car s) (stream->list+stream (stream-cdr s) (- n 1)))))

;;;
;;; srfi-41 comprehension
;;;

;; The following 'stream-of' definition is taken from srfi-41 reference
;; implementation.  This kind of code is where syntax-rules shines.
;;
;; Copyright (C) Philip L. Bewig (2007). All Rights Reserved.
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

(define-syntax stream-of
  (syntax-rules ()
    ((_ expr rest ...)
     (stream-of-aux expr stream-null rest ...))))

(define-syntax stream-of-aux
  (syntax-rules (in is)
    ((stream-of-aux expr base)
     (stream-cons expr base))
    ((stream-of-aux expr base (var in stream) rest ...)
     (stream-let loop ((strm stream))
                 (if (stream-null? strm)
                   base
                   (let ((var (stream-car strm)))
                     (stream-of-aux expr (loop (stream-cdr strm)) rest ...)))))
    ((stream-of-aux expr base (var is exp) rest ...)
     (let ((var exp)) (stream-of-aux expr base rest ...)))
    ((stream-of-aux expr base pred? rest ...)
     (if pred? (stream-of-aux expr base rest ...) base))))

;; End of excerpt from srfi-41 reference implementation

;;;
;;; Extras
;;;

;; The following procedures are taken from stream-ext.scm by
;; Alejandro Forero Cuervo <bachue@bachue.com>

;; Constructors
(define (stream-xcons a b) (stream-cons b a))

(define-stream (stream-cons* . elts)
  (if (null? (cdr elts))
    (car elts)
    (stream-cons (car elts) (apply stream-cons* (cdr elts)))))

(define (make-stream n . rest)
  (stream-tabulate n (if (null? rest)
                       (^_ #f)
                       (^_ (car rest)))))

(define (stream-tabulate n init-proc)
  (stream-let loop ((i 0))
    (if (equal? i n)
       stream-null
       (stream-cons (init-proc i) (loop (+ i 1))))))

(define (stream-iota :optional (count +inf.0) (start 0) (step 1))
  (define cnt (if (< count 0) +inf.0 count)) ;; for the backward compatibility
  (if (and (or (exact? cnt) (infinite? cnt))
           (exact? start)
           (exact? step))
    (let loop ([c cnt] [s start])
      (stream-delay
       (if (<= c 0)
         stream-null
         (stream-cons s (loop (- c 1) (+ s step))))))
    (let loop ([k 0])
      (stream-delay
       (if (>= k cnt)
         stream-null
         (stream-cons (+. start (* k step)) (loop (+ k 1))))))))

(define (stream-format fmt . rest)
  (string->stream (apply format fmt rest)))

(define stream-lines (cut stream-split <> (cut equal? <> #\newline)))

;; Conversion

(define (string->stream str :optional (tail stream-null))
  (let loop ((i 0))
    (stream-delay
     (if (equal? i (string-length str))
       tail
       (stream-cons (string-ref str i) (loop (+ i 1)))))))

(define stream->string (compose list->string stream->list))
(define number->stream (compose string->stream number->string))
(define stream->number (compose string->number stream->string))
(define stream->symbol (compose string->symbol stream->string))
(define symbol->stream (compose string->stream symbol->string))



;(define (make-output-port-char write close)
;  (make-output-port
;   (lambda (string)
;     (let loop ((i 0))
;       (when (< i (string-length string))
;         (write (string-ref string i))
;         (loop (+ i 1)))))
;   close))

(define (iterator->stream proc)
  (stream-delay
   (call-with-current-continuation
    (lambda (return)
      (proc
       (lambda (obj)
         (call-with-current-continuation
          (lambda (next)
            (return
             (stream-cons obj
                          (stream-delay
                           (call-with-current-continuation
                            (lambda (new)
                              (set! return new)
                              (next #t)))))))))
       (lambda () (return stream-null)))
      (return stream-null)))))

;(define (with-output-to-stream proc)
;  (iterator->stream
;   (lambda (write close)
;     (with-output-to-port
;         (make-output-port
;          (lambda (string)
;            (let loop ((i 0))
;              (when (< i (string-length string))
;                (write (string-ref string i))
;                (loop (+ i 1)))))
;          close)
;       proc))))

;(define (with-input-from-stream stream proc)
;  (with-input-from-port
;      (make-input-port
;       (lambda ()
;         (if (stream-null? stream)
;           (end-of-file)
;           (let ((char (stream-car stream)))
;             (set! stream (stream-cdr stream))
;             char)))
;       (lambda ()
;         (not (stream-null? stream)))
;       (lambda ()
;         (set! stream stream-null))
;       (lambda ()
;         (stream-car stream)))
;    proc))

;; Predicates

(define (stream= elt= . strs)
  (or (every stream-null? strs)
      (and (not (any stream-null? strs))
           (let loop ((es (map stream-car strs)))
             (or (null? (cdr es))
                 (and (elt= (car es) (cadr es)) (loop (cdr es)))))
           (apply stream= elt= (map stream-cdr strs)))))

(define (stream-prefix= str prefix . rest)
  (if (null? prefix)
    str
    (and (not (stream-null? str))
         ((if (null? rest) equal? (car rest)) (stream-car str) (car prefix))
         (apply stream-prefix= (stream-cdr str) (cdr prefix) rest))))

;; Selectors

(define (stream-caar   x) (stream-car (stream-car x)))
(define (stream-cadr   x) (stream-car (stream-cdr x)))
(define (stream-cdar   x) (stream-cdr (stream-car x)))
(define (stream-cddr   x) (stream-cdr (stream-cdr x)))

(define (stream-caaar  x) (stream-caar (stream-car x)))
(define (stream-caadr  x) (stream-caar (stream-cdr x)))
(define (stream-cadar  x) (stream-cadr (stream-car x)))
(define (stream-caddr  x) (stream-cadr (stream-cdr x)))
(define (stream-cdaar  x) (stream-cdar (stream-car x)))
(define (stream-cdadr  x) (stream-cdar (stream-cdr x)))
(define (stream-cddar  x) (stream-cddr (stream-car x)))
(define (stream-cdddr  x) (stream-cddr (stream-cdr x)))

(define (stream-caaaar x) (stream-caaar (stream-car x)))
(define (stream-caaadr x) (stream-caaar (stream-cdr x)))
(define (stream-caadar x) (stream-caadr (stream-car x)))
(define (stream-caaddr x) (stream-caadr (stream-cdr x)))
(define (stream-cadaar x) (stream-cadar (stream-car x)))
(define (stream-cadadr x) (stream-cadar (stream-cdr x)))
(define (stream-caddar x) (stream-caddr (stream-car x)))
(define (stream-cadddr x) (stream-caddr (stream-cdr x)))
(define (stream-cdaaar x) (stream-cdaar (stream-car x)))
(define (stream-cdaadr x) (stream-cdaar (stream-cdr x)))
(define (stream-cdadar x) (stream-cdadr (stream-car x)))
(define (stream-cdaddr x) (stream-cdadr (stream-cdr x)))
(define (stream-cddaar x) (stream-cddar (stream-car x)))
(define (stream-cddadr x) (stream-cddar (stream-cdr x)))
(define (stream-cdddar x) (stream-cdddr (stream-car x)))
(define (stream-cddddr x) (stream-cdddr (stream-cdr x)))

(define stream-first  stream-car)
(define stream-second stream-cadr)
(define stream-third  stream-caddr)
(define stream-fourth stream-cadddr)
(define (stream-fifth   x) (stream-car    (stream-cddddr x)))
(define (stream-sixth   x) (stream-cadr   (stream-cddddr x)))
(define (stream-seventh x) (stream-caddr  (stream-cddddr x)))
(define (stream-eighth  x) (stream-cadddr (stream-cddddr x)))
(define (stream-ninth   x) (stream-car  (stream-cddddr (stream-cddddr x))))
(define (stream-tenth   x) (stream-cadr (stream-cddddr (stream-cddddr x))))

(define (stream-take-safe stream count)
  (stream-delay
   (if (or (zero? count) (stream-null? stream))
     stream-null
     (stream-cons (stream-car stream)
                  (stream-take-safe (stream-cdr stream) (- count 1))))))

(define (stream-drop-safe str count)
  (stream-delay
   (if (or (zero? count) (stream-null? str))
     str
     (stream-drop-safe (stream-cdr str) (- count 1)))))

;; NB: srfi-41 has arguments reversed
(define (stream-take stream count)
  (stream-delay
   (if (zero? count)
     stream-null
     (stream-cons (stream-car stream)
                  (stream-take (stream-cdr stream) (- count 1))))))

;; NB: srfi-41 has arguments reversed
(define (stream-drop str count)
  (stream-delay
   (if (zero? count)
     str
     (stream-drop (stream-cdr str) (- count 1)))))

(define (stream-intersperse stream element)
  (stream-delay
   (if (stream-null? stream)
     stream-null
     (stream-cons (stream-car stream)
                  (let loop ((rest (stream-cdr stream)))
                    (if (stream-null? rest)
                      stream-null
                      (stream-cons element
                                   (stream-cons (stream-car rest)
                                                (loop (stream-cdr rest))))))))))

(define (stream-split in p?)
  (let loop ((current '()) (s in))
    (stream-delay
     (cond
      ((stream-null? s)
       (if (null? current)
         stream-null
         (stream-cons (list->stream (reverse current)) stream-null)))
      ((p? (stream-car s))
       (stream-cons (list->stream (reverse current)) (loop '() (stream-cdr s))))
      (else (loop (cons (stream-car s) current) (stream-cdr s)))))))

(define (stream-last str)
  (if (stream-null? (stream-cdr str))
    (stream-car str)
    (stream-last (stream-cdr str))))

(define (stream-last-n str count)
  (stream-delay
   (let ((l (list #f)))
     (set-cdr! l l)
     (let loop ((s str) (l l) (i 0))
       (cond
        ((stream-null? s)
         (if (< i count)
           str
           (stream-take (list->stream (cdr l)) i)))
        ((equal? i count)
         (set-car! l (stream-car s))
         (loop (stream-cdr s) (cdr l) i))
        (else
         (set-car! l (stream-car s))
         (set-cdr! l (cons i (cdr l)))
         (loop (stream-cdr s) (cdr l) (+ i 1))))))))

(define (stream-butlast str)
  (stream-butlast-n str 1))

(define (stream-butlast-n str count)
  (stream-delay
   (let loop ((head str) (tail (stream-drop str count)))
     (if (stream-null? tail)
       stream-null
       (stream-cons (stream-car head)
                    (loop (stream-cdr head) (stream-cdr tail)))))))

;; Miscelaneous: length, append, concatenate, reverse, zip & count

(define (stream-length>= s len)
  (or (zero? len)
      (and (not (stream-null? s))
           (stream-length>= (stream-cdr s) (- len 1)))))

(define (stream-length= s len)
  (if (zero? len)
    (stream-null? s)
    (stream-length= (stream-cdr s) (- len 1))))

(define (stream-concatenate strs)
  (stream-delay
   (if (stream-null? strs)
     stream-null
     (stream-append (stream-car strs)
                    (stream-concatenate (stream-cdr strs))))))

(define (stream-count pred . strs)
  (let loop ((times 0) (s strs))
    (if (any stream-null? s)
      times
      (loop (+ times (if (apply pred (map stream-car s)) 1 0))
            (map stream-cdr s)))))

;; Filtering & Partitioning

(define (stream-remove pred str)
  (stream-filter (complement pred) str))

; The following version is faster than the current but has the problem of
; eagerly evaluating the streams rather than create them as they are
; needed (so, for instance, it won't work in infinite streams).

;(define (stream-partition pred str)
;  (if (stream-null? str)
;    (values stream-null stream-null)
;    (receive (in out) (stream-partition pred (stream-cdr str))
;      (if (pred (stream-car str))
;        (values (stream-cons (stream-car str) in) out)
;        (values in (stream-cons (stream-car str) out))))))

(define (stream-partition pred str)
  (values (stream-filter pred str)
          (stream-remove pred str)))

;; Searching

(define (stream-find pred str)
  (let ((result (stream-find-tail pred str)))
    (and result (stream-car result))))

(define (stream-find-tail pred str)
  (and (not (stream-null? str))
       (if (pred (stream-car str))
           str
           (stream-find-tail pred (stream-cdr str)))))

(define (stream-span pred str)
  (values (stream-take-while pred str) (stream-drop-while pred str)))

(define (stream-break pred str)
  (stream-span (^x (not (pred x))) str))

(define (stream-any pred . strs)
  (and (not (find stream-null? strs))
       (or (apply pred (map stream-car strs))
           (apply stream-any pred (map stream-cdr strs)))))

(define (stream-every pred . strs)
  (or (any stream-null? strs)
      (let loop ([strs strs])
        (cond [(apply pred (map stream-car strs))
               => (^r (let1 cdrs (map stream-cdr strs)
                        (if (any stream-null? cdrs)
                          r
                          (loop cdrs))))]
              [else #f]))))

(define (stream-index pred . strs)
  (let loop ((strs strs) (pos 0))
    (and (not (find stream-null? strs))
         (if (apply pred (map stream-car strs))
             pos
             (loop (map stream-cdr strs) (+ pos 1))))))

(define (stream-member-real x str =)
  (stream-find-tail (lambda (elt) (= x elt)) str))

(define (stream-member x str . rest)
  (stream-member-real x str (if (null? rest) equal? (car rest))))

(define (stream-memq x str) (stream-member-real x str eq?))
(define (stream-memv x str) (stream-member-real x str eqv?))

;; Deletion

(define (stream-delete x str . rest)
  (stream-remove
    (let ((= (if (null? rest) equal? (car rest))))
      (lambda (elt) (= x elt)))
    str))

(define (stream-delete-duplicates str . rest)
  (stream-delete-dups str '() (if (null? rest) equal? (car rest))))

(define (stream-delete-dups str already =)
  (stream-delay
    (cond
      ((stream-null? str) stream-null)
      ((any (^x (= x (stream-car str))) already)
       (stream-delete-dups (stream-cdr str) already =))
      (else
        (stream-cons (stream-car str)
                     (stream-delete-dups (stream-cdr str) (cons (stream-car str) already) =))))))

;; Pattern Matching

(define (stream-grep re stream)
  (let ((real-re (if (string? re) (string->regexp re) re)))
    (stream-filter (cut real-re <>) stream)))

; (equal? tail stream-null) rather than (stream-null? tail) to avoid an
; off-by-one error (evaluating tail before obj is fully consumed).

(define (->stream-char obj :optional (tail stream-null))
  (stream-delay
   (cond
    ((string? obj) (string->stream obj tail))
    ((or (number? obj) (boolean? obj) (symbol? obj)) (->stream-char (x->string obj) tail))
    ((char? obj) (stream-cons obj tail))
    ((port? obj) (port->stream obj))
    ((stream? obj)
     (if (equal? tail stream-null)
       obj
       (stream-append obj tail)))
    (else (error "Unable to convert object to stream-char" obj)))))

(define (stream-replace in reps)
  (if (stream-null? in)
      stream-null
      (let ((obj (assoc (stream-car in) reps)))
        (if obj
            (->stream-char (cadr obj) (stream-replace (stream-cdr in) reps))
            (stream-cons (stream-car in) (stream-replace (stream-cdr in) reps))))))

(define (stream-translate str from to)
  (stream-map (^c (if (equal? c from) to c)) str))

(define (write-stream stream :optional
                      (port (current-output-port)) (writer write-char))
  (let loop ((s stream))
    (unless (stream-null? s)
      (writer (stream-car s) port)
      (loop (stream-cdr s)))))

;(define (stream-chomp stream . args)
;  (stream-delay
;    (let-optionals args ((lastchar #\newline))
;      (if (or (stream-null? stream)
;              (and (stream-null? (stream-cdr stream))
;                   (char=? lastchar (stream-car stream))))
;        stream-null
;        (stream-cons (stream-car stream) (stream-chomp (stream-cdr stream) lastchar))))))

