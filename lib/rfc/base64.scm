;;;
;;; base64.scm - base64 encoding/decoding routine
;;;
;;;   Copyright (c) 2000-2024  Shiro Kawai  <shiro@acm.org>
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
;; RFC4648 <https://datatracker.ietf.org/doc/html/rfc4648>
;; We also provide Base32 and Base16 as defined in RFC4648.

(define-module rfc.base64
  (use gauche.sequence)
  (use gauche.threads)
  (use srfi.42)
  (export base64-encode base64-encode-message
          base64-decode base64-decode-string-to

          base32-encode base32-encode-message
          base32-decode base32-decode-string-to
          base32hex-encode base32hex-encode-message
          base32hex-decode base32hex-decode-string-to

          base16-encode base16-encode-message
          base16-decode base16-decode-string-to

          ;; deprecated, kept for the backward compatibility
          base64-encode-string base64-encode-bytevector
          base64-decode-string base64-decode-bytevector
          ))
(select-module rfc.base64)

(autoload gauche.vport open-input-uvector open-output-uvector get-output-uvector)

;; Common routine to derive port->port filter to seq->seq
(define (make-string->bytevector string thunk)
  (let1 out (open-output-uvector)
    (with-input-from-string string
      (cut with-output-to-port out thunk))
    (get-output-uvector out)))
(define (make-bytevector->string bv thunk)
  (assume-type bv <u8vector>)
  (with-output-to-string
    (^[] (with-input-from-port (open-input-uvector bv) thunk))))
(define (make-decode-string-to target string thunk)
  (cond
   [(eqv? target <string>) (with-string-io string thunk)]
   [(eqv? target <u8vector>) (make-string->bytevector string thunk)]
   [else (error "invalid target:" target)]))


;;;
;;; Base 64
;;;

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
  (letrec-syntax ([lookup (syntax-rules ()
                            [(_ c)
                             (let1 i (char->integer c)
                               (and (< 32 i 128)
                                    (vector-ref table (- i 32))))])]
                  [state (syntax-rules ()
                           [(_ c end-expr handler skipper)
                            (cond [(eof-object? c) end-expr]
                                  [(eqv? c #\=)]
                                  [(lookup c) => handler]
                                  [(and strict (not (char-whitespace? c)))
                                   (invalid c)]
                                  [else skipper])])])
    ;; |00000011|11112222|22333333|
    (define (d0 c)
      (state c #f (cut d1 (read-char) <>) (d0 (read-char))))
    (define (d1 c c0)
      (state c (premature)
             (^[c1]
               (write-byte (+ (ash c0 2) (ash c1 -4)))
               (d2 (read-char) (logand c1 #x0f)))
             (d1 (read-char) c0)))
    (define (d2 c c1)
      (state c (premature)
             (^[c2]
               (write-byte (+ (ash c1 4) (ash c2 -2)))
               (d3 (read-char) (logand c2 #x03)))
             (d2 (read-char) c1)))
    (define (d3 c c2)
      (state c (premature)
             (^[c3]
               (write-byte (+ (ash c2 6) c3))
               (d0 (read-char)))
             (d3 (read-char) c2)))

    (d0 (read-char))))

(define (base64-decode-string-to target string . opts)
  (make-decode-string-to target string (cut apply base64-decode opts)))

;; DEPRECATED
(define (base64-decode-string string . opts)
  (with-string-io string (cut apply base64-decode opts)))
(define (base64-decode-bytevector string . opts)
  (make-string->bytevector string (cut apply base64-decode opts)))

(define (base64-encode :key (line-width 76) (url-safe #f) (digits #f)
                            (omit-padding #f))
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
    ;;  b0
    ;; |AAAAAA--|
    (define (e0 b0 col)
      (if (eof-object? b0)
        col
        (e1 (logand b0 #x03) (read-byte) (emit* col (ash b0 -2)))))

    ;;  b0       b1
    ;; |------BB|BBBB----|
    (define (e1 b0 b1 col)
      (if (eof-object? b1)
        (let1 col (emit* col (ash b0 4))
          (unless omit-padding
            (emit* col 64 64)))
        (e2 (logand b1 #x0f) (read-byte)
            (emit* col (+ (ash b0 4) (ash b1 -4))))))

    ;;  b0       b1       b2
    ;; |--------|----CCCC|CCDDDDDD|
    (define (e2 b1 b2 col)
      (if (eof-object? b2)
        (let1 col (emit* col (ash b1 2))
          (unless omit-padding
            (emit* col 64)))
        (e0 (read-byte)
            (emit* col (+ (ash b1 2) (ash b2 -6)) (logand b2 #x3f)))))

    (e0 (read-byte) 0)))

(define (base64-encode-message msg . opts)
  (etypecase msg
    [<string> (with-string-io msg (cut apply base64-encode opts))]
    [<u8vector> (make-bytevector->string msg (cut apply base64-encode opts))]))

;; DEPRECATED
(define (base64-encode-string string . opts)
  (with-string-io string (cut apply base64-encode opts)))
(define (base64-encode-bytevector vec . opts)
  (make-bytevector->string vec (cut apply base64-encode opts)))


;;;
;;; Base32
;;;

(define *base32-decode-table*
  ;;    !   "   #   $   %   &   '   (   )   *   +   ,   -   .   /
  #(#f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f
  ;;0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ?
    #f  #f  26  27  28  29  30  31  #f  #f  #f  #f  #f  #f  #f  #f
  ;;@   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O
    #f  0   1   2   3   4   5   6   7   8   9   10  11  12  13  14
  ;;P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _
    15  16  17  18  19  20  21  22  23  24  25  #f  #f  #f  #f  #f
  ;;`   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o
    #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f
  ;;p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~
    #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f
  ))

(define *base32-encode-table*
  ;;0   1   2   3   4   5   6   7   8   9   10  11  12  13  14  15
  #(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P
  ;;16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31
    #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\2 #\3 #\4 #\5 #\6 #\7
  ;;pad
    #\=
  ))

(define *base32hex-decode-table*
  ;;    !   "   #   $   %   &   '   (   )   *   +   ,   -   .   /
  #(#f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f
  ;;0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ?
    0   1   2   3   4   5   6   7   8   9   #f  #f  #f  #f  #f  #f
  ;;@   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O
    #f  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24
  ;;P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _
    25  26  27  28  29  30  31  #f  #f  #f  #f  #f  #f  #f  #f  #f
  ;;`   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o
    #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f
  ;;p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~
    #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f
  ))

(define *base32hex-encode-table*
  ;;0   1   2   3   4   5   6   7   8   9   10  11  12  13  14  15
  #(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F
  ;;16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31
    #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V
  ;;pad
    #\=
  ))

(define (%base32-decode table strict)
  (define (invalid c) (error "Invalid base32 input character" c))
  (define (premature) (when strict (error "Premature end of base32 input")))
  (letrec-syntax ([lookup (syntax-rules ()
                            [(_ c)
                             (let1 i (char->integer c)
                               (and (< 32 i 128)
                                    (vector-ref table (- i 32))))])]
                  [state (syntax-rules ()
                           [(_ c end-expr handler skipper)
                            (cond [(eof-object? c) end-expr]
                                  [(eqv? c #\=)]
                                  [(lookup c) => handler]
                                  [(and strict (not (char-whitespace? c)))
                                   (invalid c)]
                                  [else skipper])])])
    ;; |00000111|11222223|33334444|45555566|66677777|
    (define (d0 c)
      (state c #f (cut d1 (read-char) <>) (d0 (read-char))))
    (define (d1 c c0)
      (state c (premature)
             (^[c1]
               (write-byte (+ (ash c0 3) (ash c1 -2)))
               (d2 (read-char) (logand c1 #x03)))
             (d1 (read-char) c0)))
    (define (d2 c c1)
      (state c (premature)
             (^[c2] (d3 (read-char) c1 c2))
             (d2 (read-char) c1)))
    (define (d3 c c1 c2)
      (state c (premature)
             (^[c3]
               (write-byte (+ (ash c1 6) (ash c2 1) (ash c3 -4)))
               (d4 (read-char) (logand c3 #x0f)))
             (d3 (read-char) c1 c2)))
    (define (d4 c c3)
      (state c (premature)
             (^[c4]
               (write-byte (+ (ash c3 4) (ash c4 -1)))
               (d5 (read-char) (logand c4 #x01)))
             (d4 (read-char) c3)))
    (define (d5 c c4)
      (state c (premature)
             (^[c5] (d6 (read-char) c4 c5))
             (d5 (read-char) c4)))
    (define (d6 c c4 c5)
      (state c (premature)
             (^[c6]
               (write-byte (+ (ash c4 7) (ash c5 2) (ash c6 -3)))
               (d7 (read-char) (logand c6 #x07)))
             (d6 (read-char) c4 c5)))
    (define (d7 c c6)
      (state c (premature)
             (^[c7]
               (write-byte (+ (ash c6 5) c7))
               (d0 (read-char)))
             (d7 c c6)))

    (d0 (read-char))))

(define (base32-decode :key (strict #f))
  (%base32-decode *base32-decode-table* strict))
(define (base32hex-decode :key (strict #f))
  (%base32-decode *base32hex-decode-table* strict))

(define (base32-decode-string-to target string :key (strict #f))
  (make-decode-string-to target string (cut base32-decode :strict strict)))
(define (base32hex-decode-string-to target  string :key (strict #f))
  (make-decode-string-to target string (cut base32hex-decode :strict strict)))

(define (%base32-encode table omit-padding line-width)
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

    ;;  b0
    ;; |AAAAA---|
    (define (e0 b0 col)
      (if (eof-object? b0)
        col
        (e1 (logand b0 #x07) (read-byte) (emit* col (ash b0 -3)))))

    ;;  b0       b1
    ;; |-----BBB|BBCCCCC-|
    (define (e1 b0 b1 col)
      (if (eof-object? b1)
        (rlet1 col (emit* col (ash b0 2))
          (unless omit-padding
            (emit* col 32 32 32 32 32 32)))
        (e2 (logand b1 #x01) (read-byte)
            (emit* col
                   (+ (ash b0 2) (ash b1 -6))
                   (logand (ash b1 -1) #x1f)))))

    ;;  b0       b1       b2
    ;; |--------|-------D|DDDD----|
    (define (e2 b1 b2 col)
      (if (eof-object? b2)
        (rlet1 col (emit* col (ash b1 4))
          (unless omit-padding
            (emit* col 32 32 32 32)))
        (e3 (logand b2 #x0f) (read-byte)
            (emit* col (+ (ash b1 4) (ash b2 -4))))))

    ;;  b0       b1       b2       b3
    ;; |--------|--------|----EEEE|EFFFFF--|
    (define (e3 b2 b3 col)
      (if (eof-object? b3)
        (rlet1 col (emit* col (ash b2 1))
          (unless omit-padding
            (emit* col 32 32 32)))
        (e4 (logand b3 #x03) (read-byte)
            (emit* col (+ (ash b2 1) (ash b3 -7))
                   (logand (ash b3 -2) #x1f)))))

    ;;  b0       b1       b2       b3       b4
    ;; |--------|--------|--------|------GG|GGGHHHHH|
    (define (e4 b3 b4 col)
      (if (eof-object? b4)
        (rlet1 col (emit* col (ash b3 3))
          (unless omit-padding
            (emit* col 32)))
        (e0 (read-byte)
            (emit* col (+ (ash b3 3) (ash b4 -5)) (logand b4 #x1f)))))

    (e0 (read-byte) 0)))

(define (base32-encode :key (omit-padding #f) (line-width 76))
  (%base32-encode *base32-encode-table* omit-padding line-width))
(define (base32hex-encode :key (omit-padding #f) (line-width 76))
  (%base32-encode *base32hex-encode-table* omit-padding line-width))

(define (base32-encode-message msg . args)
  (etypecase msg
    [<string> (with-string-io msg (cut apply base32-encode args))]
    [<u8vector> (make-bytevector->string msg (cut apply base32-encode args))]))
(define (base32hex-encode-message msg . args)
  (etypecase msg
    [<string> (with-string-io msg (cut apply base32hex-encode args))]
    [<u8vector> (make-bytevector->string msg (cut apply base32hex-encode args))]))

;;;
;;; Base16
;;;

;; The RFC only uses uppercase alphabets.  For the convenience,
;; the decoder accept lowercase letters in non-strict mode.  The encoder
;; generates lowercase letetrs when :lowercase is true.

(define *base16-decode-table*
  ;;    !   "   #   $   %   &   '   (   )   *   +   ,   -   .   /
  #(#f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f
  ;;0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ?
    0   1   2   3   4   5   6   7   8   9  #f  #f  #f  #f  #f  #f
  ;;@   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O
    #f  10  11  12  13  14  15  #f  #f  #f  #f  #f  #f  #f  #f  #f
  ;;P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _
    #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f
  ;;`   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o
    #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f
  ;;p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~
    #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f
  ))
(define *base16lax-decode-table*
  ;;    !   "   #   $   %   &   '   (   )   *   +   ,   -   .   /
  #(#f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f
  ;;0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ?
    0   1   2   3   4   5   6   7   8   9  #f  #f  #f  #f  #f  #f
  ;;@   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O
    #f  10  11  12  13  14  15  #f  #f  #f  #f  #f  #f  #f  #f  #f
  ;;P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _
    #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f
  ;;`   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o
    #f  10  11  12  13  14  15  #f  #f  #f  #f  #f  #f  #f  #f  #f
  ;;p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~
    #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f  #f
  ))

(define *base16-encode-table*
  ;;0   1   2   3   4   5   6   7   8   9   10  11  12  13  14  15
  #(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F))
(define *base16lc-encode-table*
  ;;0   1   2   3   4   5   6   7   8   9   10  11  12  13  14  15
  #(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f))


(define (base16-decode :key (strict #f))
  (define (invalid c) (error "Invalid base16 input character" c))
  (define (premature) (when strict (error "Premature end of base16 input")))
  (define table (if strict *base16-decode-table* *base16lax-decode-table*))
  (letrec-syntax ([lookup (syntax-rules ()
                            [(_ c)
                             (let1 i (char->integer c)
                               (and (< 32 i 128)
                                    (vector-ref table (- i 32))))])]
                  [state (syntax-rules ()
                           [(_ c end-expr handler skipper)
                            (cond [(eof-object? c) end-expr]
                                  [(lookup c) => handler]
                                  [(and strict (not (char-whitespace? c)))
                                   (invalid c)]
                                  [else skipper])])])
    (define (d0 c)
      (state c #f (cut d1 (read-char) <>) (d0 (read-char))))
    (define (d1 c b0)
      (state c (premature)
             (^[b1] (write-byte (+ (ash b0 4) b1)) (d0 (read-char)))
             (d1 (read-char) b0)))
    (d0 (read-char))))

(define (base16-decode-string-to target string :key (strict #f))
  (make-decode-string-to target string (cut base16-decode :strict strict)))

(define (base16-encode :key (lowercase #f))
  (define table (if lowercase *base16lc-encode-table* *base16-encode-table*))
  (generator-for-each
   (^b (write-char (vector-ref table (ash b -4)))
       (write-char (vector-ref table (logand b #x0f))))
   read-byte))

(define (base16-encode-message msg :key (lowercase #f))
  (etypecase msg
    [<string> (with-string-io msg (cut base16-encode :lowercase lowercase))]
    [<u8vector> (make-bytevector->string msg (cut base16-encode :lowercase lowercase))]))
