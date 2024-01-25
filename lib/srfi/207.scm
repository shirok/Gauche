;;;
;;; SRFI-207 - String-notated bytevectors
;;;
;;;   Copyright (c) 2023-2024  Shiro Kawai  <shiro@acm.org>
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

(define-module srfi.207
  (use gauche.generator)
  (use gauche.lazy)
  (use gauche.unicode)
  (use gauche.uvector)
  (use rfc.base64)
  (use srfi.42)
  (use srfi.175)
  (use util.match)
  (export bytestring make-bytestring make-bytestring!
          bytevector->hex-string hex-string->bytevector
          bytevector->base64 base64->bytevector
          bytestring->list make-bytestring-generator

          bytestring-pad bytestring-pad-right
          bytestring-trim bytestring-trim-right bytestring-trim-both
          bytestring-replace
          bytestring<? bytestring>? bytestring<=? bytestring>=?

          bytestring-index bytestring-index-right
          bytestring-break bytestring-span
          bytestring-join bytestring-split

          read-textual-bytestring
          write-textual-bytestring
          write-binary-bytestring

          bytestring-error?)
  )
(select-module srfi.207)

(define-condition-type <bytestring-error> <error> bytestring-error?)

;; TODO: We create intermediate u8vectors to concatenate.  It is debatable
;; whether filling individual elements may be faster.  U8vector-concatenate
;; uses block copy written in C, so it may compensate the overhead of
;; u8vector allocations.

(define (x->u8vector obj)
  (cond [(u8vector? obj) obj]
        [(number? obj)
         (if (and (exact-integer? obj) (<= 0 obj 255))
           (u8vector obj)
           (error <bytestring-error>
                  "Out of range number for an element of bytestring:" obj))]
        [(char? obj)
         (let1 c (char->integer obj)
           (if (<= 0 c 127)
             (u8vector c)
             (error <bytestring-error>
                    "Out of range character for an element of bytestring:" obj)))]
        [(string? obj)
         (unless (ascii-string? obj)
           (error <bytestring-error>
                  "Only ASCII string is allowed for bytestring:" obj))

         (string->u8vector obj 0 -1 #t) ; we can avoid copying
         ]
        [else
         (error <bytestring-error>
                "Ivalid object to construct a bytestring:" obj)]))

(define (bytestring . args) (make-bytestring args))

(define (make-bytestring lis)
  (u8vector-concatenate (map x->u8vector lis)))

(define (make-bytestring! bv start lis)
  (assume-type bv <u8vector>)
  ;; TODO: This may be added to gauche.uvector
  (let1 uvs (map x->u8vector lis)
    (assume (<= (+ start (fold (^[uv sum] (+ sum (u8vector-length uv))) 0 uvs))
                (u8vector-length bv))
            "Destination bytevector overflow" bv)
    (let loop ([uvs uvs] [start start])
      (match uvs
        [() (undefined)]
        [(uv . uvs)
         (u8vector-copy! bv start uv)
         (loop uvs (+ start (u8vector-length uv)))]))))

(define (bytevector->hex-string bv)
  (assume-type bv <u8vector>)
  (with-output-to-string
    (^[] (u8vector-for-each (^b (format #t "~2,'0x" b)) bv))))

(define (hex-string->bytevector str)
  (assume-type str <string>)
  (let1 slen (string-length str)
    (assume (even? slen) <bytestring-error>
            "Hex string must have an even length:" str)
    (rlet1 bv (make-u8vector (ash slen -1))
      (let1 in (open-input-string str)
        (let loop ([i 0])
          (let* ([a (read-char in)]
                 [b (read-char in)])
            (unless (eof-object? a)
              (let ([aa (digit->integer a 16)]
                    [bb (digit->integer b 16)])
                (assume aa <bytestring-error> "Invalid hexdigit char:" a)
                (assume bb <bytestring-error> "Invalid hexdigit char:" b)
                (u8vector-set! bv i (+ (* aa 16) bb))
                (loop (+ i 1))))))))))

(define (bytevector->base64 bv :optional (digits #f))
  (assume-type bv <u8vector>)
  (assume-type digits (<?> <string>))
  (base64-encode-bytevector bv :line-width #f :digits digits))

(define (base64->bytevector string :optional (digits #f))
  (assume-type string <string>)
  (assume-type digits (<?> <string>))
  (guard (e [else (raise (make-condition <bytestring-error>
                                         'message (~ e'message)))])
    (base64-decode-bytevector string :digits digits :strict #t)))

(define (%byte->elt b)
  (if (<= 32 b 127)
    (integer->char b)
    b))

(define (%elt->byte e)
  (cond [(char? e)
         (let1 b (char->integer e)
           (unless (<= 32 b 127)
             (error "Invalid char for bytestring:" e))
           b)]
        [(exact-integer? e)
         (unless (<= 32 e 127)
           (error "Integer out of range for bytestring:" e))
         e]
        [else (error "Invalid element for bytestring:" e)]))

(define (bytestring->list bv :optional (start 0) (end -1))
  (assume-type bv <u8vector>)
  (let ([end (if (and end (>= end 0)) end (u8vector-length bv))])
    (assume (<= 0 start end (u8vector-length bv)))
    (list-ec (: i start end)
             (%byte->elt (u8vector-ref bv i)))))

(define (make-bytestring-generator . objs)
  ;; We must check all args before start generating bytes
  (define gens (map ($ uvector->generator $ x->u8vector $) objs))
  (rec (gen)
    (if (null? gens)
      (eof-object)
      (let1 v ((car gens))
        (if (eof-object? v)
          (begin (pop! gens) (gen))
          v)))))

(define (%bytestring-pad bv len elt where)
  (assume-type bv <u8vector>)
  (let1 origlen (u8vector-length bv)
    (if (>= origlen len)
      (u8vector-copy bv)                  ;; required to allocate
      (rlet1 rv (make-u8vector len (%elt->byte elt))
        (case where
          [(left)  (u8vector-copy! rv (- len origlen) bv)]
          [(right) (u8vector-copy! rv 0 bv)])))))

(define (bytestring-pad bv len elt) (%bytestring-pad bv len elt 'left))
(define (bytestring-pad-right bv len elt) (%bytestring-pad bv len elt 'right))

(define (bytestring-trim bv pred)
  (define len (u8vector-length bv))
  (let loop ([i 0])
    (cond [(= i len) (u8vector)]
          [(pred (u8vector-ref bv i)) (loop (+ i 1))]
          [else (u8vector-copy bv i len)])))

(define (bytestring-trim-right bv pred)
  (define len (u8vector-length bv))
  (let loop ([i (- len 1)])
    (cond [(< i 0) (u8vector)]
          [(pred (u8vector-ref bv i)) (loop (- i 1))]
          [else (u8vector-copy bv 0 (+ i 1))])))

(define (bytestring-trim-both bv pred)
  (define len (u8vector-length bv))
  (let leading ([i 0])
    (cond [(= i len) (u8vector)]
          [(pred (u8vector-ref bv i)) (leading (+ i 1))]
          [else
           (let trailing ([j (- len 1)])
             (if (pred (u8vector-ref bv j))
               (trailing (- j 1))
               (u8vector-copy bv i (+ j 1))))])))

(define (bytestring-replace bv1 bv2 start1 end1 :optional (start2 0) (end2 #f))
  (assume-type bv1 <u8vector>)
  (assume-type bv2 <u8vector>)
  (u8vector-append-subvectors bv1 0 start1
                              bv2 start2 (or end2 (u8vector-length bv2))
                              bv1 end1 (u8vector-length bv1)))

(define (%bytestring-compare bv1 bv2)
  (assume-type bv1 <u8vector>)
  (assume-type bv2 <u8vector>)
  (let ([len1 (u8vector-length bv1)]
        [len2 (u8vector-length bv2)])
    (let loop ([k 0])
      (cond [(= k len1) (if (= k len2) 0 -1)]
            [(= k len2) 1]
            [else (let ([b1 (u8vector-ref bv1 k)]
                        [b2 (u8vector-ref bv2 k)])
                    (cond [(< b1 b2) -1]
                          [(> b1 b2) 1]
                          [else (loop (+ k 1))]))]))))

(define (bytestring<? bv1 bv2)  (< (%bytestring-compare bv1 bv2) 0))
(define (bytestring>? bv1 bv2)  (> (%bytestring-compare bv1 bv2) 0))
(define (bytestring<=? bv1 bv2) (<= (%bytestring-compare bv1 bv2) 0))
(define (bytestring>=? bv1 bv2) (>= (%bytestring-compare bv1 bv2) 0))

;; These differ from u8vector-index/u8vector-index-right, so we
;; scan by our own.
(define (bytestring-index bv pred :optional (start 0) (end #f))
  (assume-type bv <u8vector>)
  (let1 end (or end (u8vector-length bv))
    (let loop ([i start])
      (and (< i end)
           (if (pred (u8vector-ref bv i))
             i
             (loop (+ i 1)))))))

(define (bytestring-index-right bv pred :optional (start 0) (end #f))
  (assume-type bv <u8vector>)
  (let1 end (or end (u8vector-length bv))
    (let loop ([i (- end 1)])
      (and (>= i start)
           (if (pred (u8vector-ref bv i))
             i
             (loop (- i 1)))))))

(define (bytestring-break bv pred)
  (assume-type bv <u8vector>)
  (if-let1 i (bytestring-index bv pred)
    (values (u8vector-copy bv 0 i)
            (u8vector-copy bv i))
    (values bv (u8vector))))

(define (bytestring-span bv pred)
  (assume-type bv <u8vector>)
  (if-let1 i (bytestring-index bv (complement pred))
    (values (u8vector-copy bv 0 i)
            (u8vector-copy bv i))
    (values bv (u8vector))))

(define (bytestring-join bvs delim :optional (grammar 'infix))
  (case grammar
    [(infix) (u8vector-concatenate (intersperse (x->u8vector delim) bvs))]
    [(strict-infix)
     (if (null? bvs)
       (error <bytestring-error>
              "Zero bytevectors cannot be joined with strict-infix grammar.")
       (bytestring-join bvs delim 'infix))]
    [(suffix) (u8vector-concatenate
               (append-map (cute list <> (x->u8vector delim)) bvs))]
    [(prefix) (u8vector-concatenate
               (append-map (cute list (x->u8vector delim) <>) bvs))]
    [else (error <bytestring-error>
                 "Invalid grammar argument:" grammar)]))

(define (bytestring-split bv delim :optional (grammar 'infix))
  (define dbyte
    (cond [(and (char? delim) (#[\x00-\x7f] delim)) (char->integer delim)]
          [(and (exact-integer? delim) (<= 0 delim 255)) delim]
          [else (error "Delimiter out of domain:" delim)]))
  (assume-type bv <u8vector>)
  (let1 end (u8vector-length bv)
    (let loop ([i 0] [s 0] [r '()])
      (cond [(= i end)
             (let1 r (cons (u8vector-copy bv s i) r)
               (case grammar
                 [(infix strict-infix) (if (equal? r '(#u8())) '() (reverse r))]
                 [(prefix) (let1 rr (reverse r)
                             (if (and (pair? rr) (equal? #u8() (car rr)))
                               (cdr rr)
                               rr))]
                 [(suffix) (if (and (pair? r) (equal? #u8() (car r)))
                             (reverse (cdr r))
                             (reverse r))]
                 [else (error <bytestring-error>
                              "Invalid grammar argument:" grammar)]))]
            [(= (u8vector-ref bv i) dbyte)
             (loop (+ i 1) (+ i 1) (cons (u8vector-copy bv s i) r))]
            [else
             (loop (+ i 1) s r)]))))

(define (read-textual-bytestring prefix :optional (port (current-input-port)))
  (define (err msg . args) (apply error <bytestring-error> msg args))
  (define (ensure char expected)
    (unless (eqv? char expected)
      (err (format "Bad bytestring sytnax. '~s' expected, but got:" expected)
           char)))
  (define (premature)
    (error <bytestring-error> "Premature end of bytestring literal"))
  (define (badhex c)
    (error <bytestring-error>
           "Invalid hexadecimal digit in bytestring literal:" c))
  (define (skip-ws next bytes newline-seen?)
    (let1 ch (read-char port)
      (cond [(eof-object? ch) (premature)]
            [(eqv? ch #\newline) (skip-ws next bytes #t)]
            [(char-whitespace? ch) (skip-ws next bytes newline-seen?)]
            [else (if newline-seen?
                    (next ch bytes)
                    (err "Invalid \\+newline sequence"))])))
  (when prefix
    (ensure (read-char port) #\#)
    (ensure (read-char port) #\u)
    (ensure (read-char port) #\8))
  (ensure (read-char port) #\")
  (let loop ((ch (read-char port))
             (bytes '()))
    (when (eof-object? ch) (premature))
    (cond [(eqv? ch #\") (list->u8vector (reverse bytes))]
          [(eqv? ch #\\)
           (let1 ch2 (read-char port)
             (case ch2
               [(#\a) (loop (read-char port) (cons 7 bytes))]
               [(#\b) (loop (read-char port) (cons 8 bytes))]
               [(#\t) (loop (read-char port) (cons 9 bytes))]
               [(#\n) (loop (read-char port) (cons 10 bytes))]
               [(#\r) (loop (read-char port) (cons 13 bytes))]
               [(#\|) (loop (read-char port) (cons 124 bytes))]
               [(#\" #\\)
                (loop (read-char port) (cons (char->integer ch2) bytes))]
               [(#\x)
                (let* ([x0 (read-char port)]
                       [x1 (read-char port)])
                  (unless (and (char? x0) (char? x1)) (premature))
                  (ensure (read-char port) #\;)
                  (let ([d0 (digit->integer x0 16)]
                        [d1 (digit->integer x1 16)])
                    (unless d0 (badhex x0))
                    (unless d1 (badhex x1))
                    (loop (read-char port) (cons (+ (* d0 16) d1) bytes))))]
               [(#\tab #\space) (skip-ws loop bytes #f)]
               [(#\newline) (skip-ws loop bytes #t)]
               [else
                (err "Invalid escape sequence in bytestring literal:" ch2)]))]
          [(ascii-char? ch)
           (loop (read-char port) (cons (char->integer ch) bytes))]
          [else (err "Non-ascii character in bytestring literal:" ch)])))

(define (write-textual-bytestring bv :optional (port (current-output-port)))
  (assume-type bv <u8vector>)
  (display "#u8\"" port)
  (do-ec (: byte bv)
         (cond
          [(eq? byte (char->integer #\")) (display "\\\"" port)]
          [(eq? byte (char->integer #\\)) (display "\\\\" port)]
          [(eq? byte #x07) (display "\\a" port)]
          [(eq? byte #x08) (display "\\b" port)]
          [(eq? byte #x09) (display "\\t" port)]
          [(eq? byte #x0a) (display "\\n" port)]
          [(eq? byte #x0d) (display "\\r" port)]
          [(eq? byte #x7c) (display "\\|" port)]
          [(<= #x20 byte #x7e) (display (integer->char byte) port)]
          [else
           ;;(format port "\\x~2,'0x;" byte)
           (display "\\x" port)
           (display (integer->digit (ash byte -4) 16) port)
           (display (integer->digit (logand byte #x0f) 16) port)
           (display ";" port)]))
  (display "\"" port))

(define (write-binary-bytestring port . args)
  ;; TODO: We can avoid intermediate u8vectors.
  (for-each (^x (write-uvector (x->u8vector x) port)) args))
