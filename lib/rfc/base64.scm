;;;
;;; base64.scm - base64 encoding/decoding routine
;;;
;;;   Copyright (c) 2000-2022  Shiro Kawai  <shiro@acm.org>
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

;; Implements Base64 encoding/decoding routine
;; Ref: RFC2045 section 6.8  <http://www.rfc-editor.org/rfc/rfc2045.txt>
;; and RFC3548 <http://www.rfc-editor.org/rfc/rfc3548.txt>

(define-module rfc.base64
  (use gauche.sequence)
  (use gauche.threads)
  (use srfi.42)
  (export base64-encode base64-encode-string base64-encode-bytevector
          base64-decode base64-decode-string base64-decode-bytevector))
(select-module rfc.base64)

(autoload gauche.vport open-input-uvector open-output-uvector get-output-uvector)

;; Mapping tables
;;  Decode table is a 96-element vector, maps charcode-32 to integer 0-63.
;;  Encode table is a 65-element vector, maps 0-63 and padding to a character.

(define *standard-decode-table*
  ;;    !   "   #   $   %   &   '   (   )   *   +   ,   -   .   /
  #(#f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  62  #f  #f  #f  63
  ;;0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ?
    52  53  54  55  56  57  58  59  60  61  #f  #f  #f  #f  #f  #f
  ;;@   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O
    #f  0   1   2   3   4   5   6   7   8   9   10  11  12  13  14
  ;;P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _
    15  16  17  18  19  20  21  22  23  24  25  #f  #f  #f  #f  #f
  ;;`   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o
    #f  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40
  ;;p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~
    41  42  43  44  45  46  47  48  49  50  51  #f  #f  #f  #f  #f
  ))

(define *standard-encode-table*
  ;;0   1   2   3   4   5   6   7   8   9   10  11  12  13  14  15
  #(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P
  ;;16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31
    #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\a #\b #\c #\d #\e #\f
  ;;32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47
    #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v
  ;;48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63
    #\w #\x #\y #\z #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\+ #\/
  ;;pad
    #\=
  ))

(define *url-safe-decode-table*
  ;;    !   "   #   $   %   &   '   (   )   *   +   ,   -   .   /
  #(#f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  62  #f  #f
  ;;0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ?
    52  53  54  55  56  57  58  59  60  61  #f  #f  #f  #f  #f  #f
  ;;@   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O
    #f  0   1   2   3   4   5   6   7   8   9   10  11  12  13  14
  ;;P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _
    15  16  17  18  19  20  21  22  23  24  25  #f  #f  #f  #f  63
  ;;`   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o
    #f  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40
  ;;p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~
    41  42  43  44  45  46  47  48  49  50  51  #f  #f  #f  #f  #f
  ))

(define *url-safe-encode-table*
  ;;0   1   2   3   4   5   6   7   8   9   10  11  12  13  14  15
  #(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P
  ;;16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31
    #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\a #\b #\c #\d #\e #\f
  ;;32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47
    #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v
  ;;48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63
    #\w #\x #\y #\z #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\- #\_
  ;;pad
    #\=
  ))

(define (%standard-digits? digit0 digit1)
  (and (eqv? digit0 #\+) (eqv? digit1 #\/)))

(define (%url-safe-digits? digit0 digit1)
  (and (eqv? digit0 #\-) (eqv? digit1 #\_)))

(define *nonstd-decode-tables*
  (make-hash-table 'equal?))          ; cons -> vector

(define (%valid-digit? digit)
  (and (char-set:ascii digit)
       (not (#[A-Za-z0-9=] digit))))

(define (%digits->decode-table digits)
  (unless (and (or (string? digits) (vector? digits))
               (eqv? (size-of digits) 2))
    (error "Digits must be a string or vector of length 2, but got:" digits))
  (let ([digit0 (~ digits 0)]
        [digit1 (~ digits 1)])
    (assume (%valid-digit? digit0) "Invalid extra digit for base64:" digit0)
    (assume (%valid-digit? digit1) "Invalid extra digit for base64:" digit1)
    (cond [(%standard-digits? digit0 digit1) *standard-decode-table*]
          [(%url-safe-digits? digit0 digit1) *url-safe-decode-table*]
          [(hash-table-get *nonstd-decode-tables* (cons digit0 digit1) #f)]
          [else
           (rlet1 v (vector-copy *standard-decode-table*)
             (vector-set! v (- (char->integer digit0) 32) 62)
             (vector-set! v (- (char->integer digit1) 32) 63)
             (hash-table-put! *nonstd-decode-tables* (cons digit0 digit1) v))])))

(define *nonstd-encode-tables*
  (make-hash-table 'equal?))          ; digits -> vector

(define (%digits->encode-table digits)
  (unless (and (or (string? digits) (vector? digits))
               (eqv? (size-of digits) 2))
    (error "Digits must be a string or vector of length 2, but got:" digits))
  (let ([digit0 (~ digits 0)]
        [digit1 (~ digits 1)])
    (assume (%valid-digit? digit0) "Invalid extra digit for base64:" digit0)
    (assume (%valid-digit? digit1) "Invalid extra digit for base64:" digit1)
    (cond [(%standard-digits? digit0 digit1) *standard-encode-table*]
          [(%url-safe-digits? digit0 digit1) *url-safe-encode-table*]
          [(hash-table-get *nonstd-encode-tables* (cons digit0 digit1) #f)]
          [else
           (rlet1 v (vector-copy *standard-encode-table*)
             (vector-set! v 62 digit0)
             (vector-set! v 63 digit1)
             (hash-table-put! *nonstd-encode-tables* (cons digit0 digit1) v))])))

(define (base64-decode :key (url-safe #f) (digits #f) (strict #f))
  (define table (cond [url-safe *url-safe-decode-table*]
                      [digits (%digits->decode-table digits)]
                      [else *standard-decode-table*]))
  (define (invalid c) (error "Invalid base64 input character" c))
  (define (premature) (when strict (error "Premature end of base64 input")))
  (let-syntax ([lookup (syntax-rules ()
                         [(_ c)
                          (let1 i (char->integer c)
                            (and (< 32 i 128)
                                 (vector-ref table (- i 32))))])]
               )
    (define (d0 c)
      (cond [(eof-object? c)]
            [(eqv? c #\=)]
            [(lookup c) => (^v (d1 (read-char) v))]
            [(and strict (not (char-whitespace? c))) (invalid c)]
            [else (d0 (read-char))]))

    (define (d1 c hi)
      (cond [(eof-object? c) (premature)]
            [(eqv? c #\=)]
            [(lookup c) => (^[lo]
                             (write-byte (+ (* hi 4) (quotient lo 16)))
                             (d2 (read-char) (modulo lo 16)))]
            [(and strict (not (char-whitespace? c))) (invalid c)]
            [else (d1 (read-char) hi)]))

    (define (d2 c hi)
      (cond [(eof-object? c) (premature)]
            [(eqv? c #\=)]
            [(lookup c) => (^[lo]
                             (write-byte (+ (* hi 16) (quotient lo 4)))
                             (d3 (read-char) (modulo lo 4)))]
            [(and strict (not (char-whitespace? c))) (invalid c)]
            [else (d2 (read-char) hi)]))

    (define (d3 c hi)
      (cond [(eof-object? c) (premature)]
            [(eqv? c #\=)]
            [(lookup c) => (^[lo]
                             (write-byte (+ (* hi 64) lo))
                             (d0 (read-char)))]
            [(and strict (not (char-whitespace? c))) (invalid c)]
            [else (d3 (read-char) hi)]))

    (d0 (read-char))))

(define (base64-decode-string string . opts)
  (with-output-to-string
    (cut with-input-from-string string (cut apply base64-decode opts))))

(define (base64-decode-bytevector string . opts)
  (let1 out (open-output-uvector)
    (with-input-from-string string
      (cut with-output-to-port out (cut apply base64-decode opts)))
    (get-output-uvector out)))

(define (base64-encode :key (line-width 76) (url-safe #f) (digits #f))
  (define table (cond [url-safe *url-safe-encode-table*]
                      [digits (%digits->encode-table digits)]
                      [else *standard-encode-table*]))
  (define maxcol (and line-width (> line-width 0) (- line-width 1)))

  (letrec-syntax ([emit*
                   (syntax-rules ()
                     [(_ col) col]
                     [(_ col idx idx2 ...)
                      (begin
                        (write-char (vector-ref table idx))
                        (let1 col2 (cond [(eqv? col maxcol) (newline) 0]
                                         [else (+ col 1)])
                          (emit* col2 idx2 ...)))])])

    (define (e0 c col)
      (cond [(eof-object? c)]
            [else
             (e1 (read-byte) (modulo c 4) (emit* col (quotient c 4)))]))

    (define (e1 c hi col)
      (cond [(eof-object? c)
             (emit* col (* hi 16) 64 64)]
            [else
             (e2 (read-byte) (modulo c 16)
                 (emit* col (+ (* hi 16) (quotient c 16))))]))

    (define (e2 c hi col)
      (cond [(eof-object? c)
             (emit* col (* hi 4) 64)]
            [else
             (e0 (read-byte)
                 (emit* col (+ (* hi 4) (quotient c 64)) (modulo c 64)))]))

    (e0 (read-byte) 0)))

(define (base64-encode-string string . opts)
  (with-string-io string (cut apply base64-encode opts)))

(define (base64-encode-bytevector vec . opts)
  (assume-type vec <u8vector>)
  (with-output-to-string
    (^[] (with-input-from-port (open-input-uvector vec)
           (cut apply base64-encode opts)))))
