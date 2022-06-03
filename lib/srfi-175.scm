;;;
;;; srfi-175 - ASCII character library
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

(define-module srfi-175
  (use gauche.uvector)
  (use gauche.generator)
  (use scheme.charset)
  (use srfi-13)
  (use srfi-42)
  (export ascii-codepoint? ascii-bytevector?
          ascii-char? ascii-string?
          ascii-control? ascii-non-control?
          ascii-space-or-tab? ascii-other-graphic?
          ascii-alphanumeric? ascii-alphabetic?
          ascii-numeric? ascii-whitespace?
          ascii-upper-case? ascii-lower-case?
          ascii-ci=? ascii-ci<? ascii-ci<=? ascii-ci>? ascii-ci>=?
          ascii-string-ci=? ascii-string-ci<? ascii-string-ci<=?
          ascii-string-ci>? ascii-string-ci>=?
          ascii-upcase ascii-downcase
          ascii-control->graphic ascii-graphic->control
          ascii-mirror-bracket
          ascii-nth-digit ascii-nth-upper-case ascii-nth-lower-case
          ascii-digit-value ascii-upper-case-value ascii-lower-case-value
          ))
(select-module srfi-175)

(define (ascii-codepoint? x) (and (integer? x) (<= #x00 x #x7f)))

(define (ascii-bytevector? x)
  (and (u8vector? x)
       (every?-ec [: b x]
                  (ascii-codepoint? b))))

(define (ascii-char? x) (and (char? x) (char-set-contains? #[[:ascii:]] x)))

(define (ascii-string? x)
  (and (string? x)
       (string-every ascii-char? x)))

;; some infrastructure

(define (%char x)
  (cond [(char? x) x]
        [(integer? x) (integer->char x)]
        [else (type-error 'argument "character or an integer" x)]))
(define (%code x)
  (cond [(char? x) (char->integer x)]
        [(integer? x) x]
        [else (type-error 'argument "character or an integer" x)]))

(define (ascii-control? c)
  (char-set-contains? char-set:ascii-control (%char c)))

(define *non-control*
  (char-set-intersection char-set:ascii
                         (char-set-complement char-set:ascii-control)))
(define (ascii-non-control? c)
  (char-set-contains? *non-control* (%char c)))

(define (ascii-space-or-tab? c)
  (boolean (memv (%char c) '(#\space #\tab))))

(define *other-graphic*
  (char-set-difference char-set:ascii-graphic
                       char-set:ascii-letter+digit))
(define (ascii-other-graphic? c)
  (char-set-contains? *other-graphic* (%char c)))
(define (ascii-alphanumeric? c)
  (char-set-contains? char-set:ascii-letter+digit (%char c)))
(define (ascii-alphabetic? c)
  (char-set-contains? char-set:ascii-letter (%char c)))
(define (ascii-numeric? c )
  (char-set-contains? char-set:ascii-digit (%char c)))
(define (ascii-whitespace? c)
  (char-set-contains? char-set:ascii-whitespace (%char c)))
(define (ascii-upper-case? c)
  (char-set-contains? char-set:ascii-upper-case (%char c)))
(define (ascii-lower-case? c)
  (char-set-contains? char-set:ascii-lower-case (%char c)))

(define (%foldcase-code c)
  (if (ascii-upper-case? c)
    (+ (%code c) #x20)
    (%code c)))

(define (ascii-ci=? c1 c2)  (= (%foldcase-code c1) (%foldcase-code c2)))
(define (ascii-ci<? c1 c2)  (< (%foldcase-code c1) (%foldcase-code c2)))
(define (ascii-ci<=? c1 c2) (<= (%foldcase-code c1) (%foldcase-code c2)))
(define (ascii-ci>? c1 c2)  (> (%foldcase-code c1) (%foldcase-code c2)))
(define (ascii-ci>=? c1 c2) (>= (%foldcase-code c1) (%foldcase-code c2)))

(define (ascii-string-ci=? s1 s2)
  (let ([g1 (string->generator s1)]
        [g2 (string->generator s2)])
    (let loop ([c1 (g1)] [c2 (g2)])
      (cond [(eof-object? c1) (eof-object? c2)]
            [(eof-object? c2) #f]
            [else (let ([cc1 (%foldcase-code c1)]
                        [cc2 (%foldcase-code c2)])
                    (and (= cc1 cc2)
                         (loop (g1) (g2))))]))))

(define (ascii-string-ci<? s1 s2)
  (let ([g1 (string->generator s1)]
        [g2 (string->generator s2)])
    (let loop ([c1 (g1)] [c2 (g2)])
      (cond [(eof-object? c2) #f]
            [(eof-object? c1) #t]
            [else (let ([cc1 (%foldcase-code c1)]
                        [cc2 (%foldcase-code c2)])
                    (cond [(= cc1 cc2) (loop (g1) (g2))]
                          [(< cc1 cc2)]
                          [else #f]))]))))

(define (ascii-string-ci<=? s1 s2)
  (let ([g1 (string->generator s1)]
        [g2 (string->generator s2)])
    (let loop ([c1 (g1)] [c2 (g2)])
      (cond [(eof-object? c2) (eof-object? c1)]
            [(eof-object? c1) #t]
            [else (let ([cc1 (%foldcase-code c1)]
                        [cc2 (%foldcase-code c2)])
                    (cond [(= cc1 cc2) (loop (g1) (g2))]
                          [(< cc1 cc2)]
                          [else #f]))]))))

(define (ascii-string-ci>? s1 s2)
  (let ([g1 (string->generator s1)]
        [g2 (string->generator s2)])
    (let loop ([c1 (g1)] [c2 (g2)])
      (cond [(eof-object? c1) #f]
            [(eof-object? c2) #t]
            [else
             (let ([cc1 (%foldcase-code c1)]
                   [cc2 (%foldcase-code c2)])
               (cond [(= cc1 cc2) (loop (g1) (g2))]
                     [(> cc1 cc2) #t]
                     [else #f]))]))))

(define (ascii-string-ci>=? s1 s2)
  (let ([g1 (string->generator s1)]
        [g2 (string->generator s2)])
    (let loop ([c1 (g1)] [c2 (g2)])
      (cond [(eof-object? c1) (eof-object? c2)]
            [(eof-object? c2) #t]
            [else (let ([cc1 (%foldcase-code c1)]
                        [cc2 (%foldcase-code c2)])
                    (cond [(= cc1 cc2) (loop (g1) (g2))]
                          [(> cc1 cc2)]
                          [else #f]))]))))

(define (ascii-upcase c)
  (cond [(char? c)
         (if (char-set-contains? char-set:ascii-lower-case c)
           (char-upcase c)
           c)]
        [(integer? c)
         (if (char-set-contains? char-set:ascii-lower-case (integer->char c))
           (- c #x20)
           c)]
        [else (type-error 'c "char or integer" c)]))

(define (ascii-downcase c)
  (cond [(char? c)
         (if (char-set-contains? char-set:ascii-upper-case c)
           (char-downcase c)
           c)]
        [(integer? c)
         (if (char-set-contains? char-set:ascii-upper-case (integer->char c))
           (+ c #x20)
           c)]
        [else (type-error 'c "char or integer" c)]))

(define (ascii-control->graphic c)
  (cond [(char? c)
         (and (char-set-contains? char-set:ascii-control c)
              (if (eqv? c #\x7f)
                #\?
                (integer->char (+ (char->integer c) #x40))))]
        [(integer? c)
         (and (char-set-contains? char-set:ascii-control (integer->char c))
              (if (eqv? c #x7f)
                (char->integer #\?)
                (+ c #x40)))]
        [else (type-error 'c "char or integer" c)]))

(define (ascii-graphic->control c)
  (cond [(char? c)
         (if (eqv? c #\?)
           #\x7f
           (let1 cc (char->integer c)
             (and (<= #x40 cc #x5f)
                  (integer->char (- cc #x40)))))]
        [(integer? c)
         (cond [(= c (char->integer #\?)) #x7f]
               [(<= #x40 c #x5f) (- c #x40)]
               [else #f])]
        [else (type-error 'c "char or integer" c)]))

(define (ascii-mirror-bracket c)
  (cond[(char? c)
        (and-let1 p (assv c '((#\( . #\)) (#\[ . #\]) (#\{ . #\}) (#\< . #\>)
                              (#\) . #\() (#\] . #\[) (#\} . #\{) (#\> . #\<)))
          (cdr p))]
       [(integer? c)
        (and-let1 p (assv c `((,(char->integer #\() . ,(char->integer #\)))
                              (,(char->integer #\[) . ,(char->integer #\]))
                              (,(char->integer #\{) . ,(char->integer #\}))
                              (,(char->integer #\<) . ,(char->integer #\>))
                              (,(char->integer #\)) . ,(char->integer #\())
                              (,(char->integer #\]) . ,(char->integer #\[))
                              (,(char->integer #\}) . ,(char->integer #\{))
                              (,(char->integer #\>) . ,(char->integer #\<))))
          (cdr p))]
        [else (type-error 'c "char or integer" c)]))

(define (ascii-nth-digit n)
  (and (exact-integer? n) (integer->digit n)))

(define (ascii-nth-upper-case n)
  (integer->char (+ (modulo n 26) (char->integer #\A))))
(define (ascii-nth-lower-case n)
  (integer->char (+ (modulo n 26) (char->integer #\a))))

(define (ascii-digit-value c limit)
  (assume (real? limit))
  (let1 lim (floor->exact limit)
    (cond
     [(= lim 1) (and (or (eqv? c #\0) (eqv? c (char->integer #\0))) 0)] ;special
     [(<= 2 lim) (digit->integer (%char c) (min lim 10))]
     [else #f])))

(define (ascii-upper-case-value c offset limit)
  (let1 cc (- (%code c) (char->integer #\A))
    (and (< -1 cc limit)
         (+ offset cc))))

(define (ascii-lower-case-value c offset limit)
  (let1 cc (- (%code c) (char->integer #\a))
    (and (< -1 cc limit)
         (+ offset cc))))
