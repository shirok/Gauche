;;;
;;; base64.scm - base64 encoding/decoding routine
;;;
;;;  Copyright(C) 2001 by Shiro Kawai (shiro@acm.org)
;;;
;;;  Permission to use, copy, modify, distribute this software and
;;;  accompanying documentation for any purpose is hereby granted,
;;;  provided that existing copyright notices are retained in all
;;;  copies and that this notice is included verbatim in all
;;;  distributions.
;;;  This software is provided as is, without express or implied
;;;  warranty.  In no circumstances the author(s) shall be liable
;;;  for any damages arising out of the use of this software.
;;;
;;;  $Id: base64.scm,v 1.5 2001-06-02 21:56:08 shirok Exp $
;;;

;; Implements Base64 encoding/decoding routine
;; Ref: RFC2045 section 6.8  <http://www.rfc-editor.org/rfc/rfc2045.txt>

(define-module rfc.base64
  (use srfi-2)
  (export base64-encode base64-encode-string
          base64-decode base64-decode-string))
(select-module rfc.base64)

(define *decode-table*
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

(define *encode-table*
  ;;0   1   2   3   4   5   6   7   8   9   10  11  12  13  14  15
  #(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P
  ;;16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31
    #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\a #\b #\c #\d #\e #\f
  ;;32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47
    #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v
  ;;48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63
    #\w #\x #\y #\z #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\+ #\/
  ))

(define (base64-decode)
  (let-syntax ((lookup (syntax-rules ()
                         ((_ c)
                          (let ((i (char->integer c)))
                            (and (< 32 i 128)
                                 (vector-ref *decode-table* (- i 32)))))))
               )
    (define (d0 c)
      (cond ((eof-object? c))
            ((eqv? c #\=))
            ((lookup c) => (lambda (v) (d1 (read-byte) v)))
            (else (d0 (read-byte)))))

    (define (d1 c hi)
      (cond ((eof-object? c))
            ((eqv? c #\=))
            ((lookup c) => (lambda (lo)
                             (write-byte (+ (* hi 4) (quotient lo 16)))
                             (d2 (read-byte) (modulo lo 16))))
            (else (d1 (read-byte) hi))))

    (define (d2 c hi)
      (cond ((eof-object? c))
            ((eqv? c #\=))
            ((lookup c) => (lambda (lo)
                             (write-byte (+ (* hi 16) (quotient lo 4)))
                             (d3 (read-byte) (modulo lo 4))))
            (else (d2 (read-byte) hi))))

    (define (d3 c hi)
      (cond ((eof-object? c))
            ((eqv? c #\=))
            ((lookup c) => (lambda (lo)
                             (write-byte (+ (* hi 64) lo))
                             (d0 (read-byte))))
            (else (d3 (read-byte) hi))))

    (d0 (read-byte))))

(define (base64-decode-string string)
  (with-output-to-string
    (lambda ()
      (with-input-from-string string base64-decode))))


(define (base64-encode)
  (let-syntax ((emit (syntax-rules ()
                       ((_ idx)
                        (write-byte (vector-ref *encode-table* idx))))))
    (define (e0 c cnt)
      (if (eof-object? c)
          #t
          (begin (emit (quotient c 4))
                 (e1 (read-byte) (modulo c 4) cnt))))

    (define (e1 c hi cnt)
      (if (eof-object? c)
          (begin (emit (* hi 16))
                 (write-byte #\=)
                 (write-byte #\=))
          (begin (emit (+ (* hi 16) (quotient c 16)))
                 (e2 (read-byte) (modulo c 16) cnt))))

    (define (e2 c hi cnt)
      (if (eof-object? c)
          (begin (emit (* hi 4))
                 (write-byte #\=))
          (begin (emit (+ (* hi 4) (quotient c 64)))
                 (emit (modulo c 64))
                 (if (= cnt 17)
                     (begin (newline)
                            (e0 (read-byte) 0))
                     (e0 (read-byte) (+ cnt 1))))))

    (e0 (read-byte) 0)))

(define (base64-encode-string string)
  (with-output-to-string
    (lambda ()
      (with-input-from-string string base64-encode))))

(provide "rfc/base64")

