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
;;;  $Id: base64.scm,v 1.2 2001-03-30 10:09:50 shiro Exp $
;;;

;; Implements Base64 encoding/decoding routine
;; Ref: RFC2045 section 6.8  <http://www.rfc-editor.org/rfc/rfc2045.txt>

;; NOT IMPLEMENTED YET

(define-module rfc.base64
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

(define *decode-table-1*  (make-vector 96 #f))
(define *decode-table-2a* (make-vector 96 #f))
(define *decode-table-2b* (make-vector 96 #f))
(define *decode-table-3a* (make-vector 96 #f))
(define *decode-table-3b* (make-vector 96 #f))
(define *decode-table-4*  *decode-table*)

(define (setup-decode-table)
  (do ((i 0 (+ i 1)))
      ((> i 96))
    (let ((v (vector-ref *decode-table-1* i)))
      (vector-set! *decode-table-1* i  (* v 4))
      (vector-set! *decode-table-2a* i (quotient v 16))
      (vector-set! *decode-table-2b* i (* (modulo v 16) 16))
      (vector-set! *decode-table-3a* i (quotient v 4))
      (vector-set! *decode-table-3b* i (* (modulo v 4) 64)))))




(provide "rfc/base64")

