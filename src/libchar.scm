;;;
;;; libchar.scm - builtin character procedures
;;;
;;;   Copyright (c) 2000-2013  Shiro Kawai  <shiro@acm.org>
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

(select-module gauche.internal)

(inline-stub
 (declcode (.include <gauche/vminsn.h>
                     <gauche/char_attr.h>
                     <ctype.h>))
 )

;;;
;;; Characters
;;;

(select-module scheme)
(define-cproc char? (obj) ::<boolean> :fast-flonum :constant
  (inliner CHARP) SCM_CHARP)

(inline-stub
 (define-cise-expr char-cmp
   ;; Assumes local variables c1, c2, chars
   [(_ op)
    `(while 1
       (cond [(not (SCM_PAIRP chars)) (result (,op c1 c2)) (break)]
             [(,op c1 c2) (unless (SCM_CHARP (SCM_CAR chars))
                            (Scm_TypeError "char" "character" (SCM_CAR chars)))
              (set! c1 c2)
              (set! c2 (SCM_CHAR_VALUE (SCM_CAR chars)))
              (set! chars (SCM_CDR chars))]
             [else (result FALSE) (break)]))])
 )

(define-cproc char=? (c1::<char> c2::<char> :rest chars)
  ::<boolean> :constant (char-cmp ==))
(define-cproc char<? (c1::<char> c2::<char> :rest chars)
  ::<boolean> :constant (char-cmp <))
(define-cproc char>? (c1::<char> c2::<char> :rest chars)
  ::<boolean> :constant (char-cmp >))
(define-cproc char<=? (c1::<char> c2::<char> :rest chars)
  ::<boolean> :constant (char-cmp <=))
(define-cproc char>=? (c1::<char> c2::<char> :rest chars)
  ::<boolean> :constant (char-cmp >=))

(inline-stub
 (define-cise-expr char-ci-cmp
   ;; Assumes local variables c1, c2, chars
   [(_ op)
    `(begin
       (set! c1 (Scm_CharFoldcase c1))
       (set! c2 (Scm_CharFoldcase c2))
       (while 1
         (cond [(not (SCM_PAIRP chars)) (result (,op c1 c2)) (break)]
               [(,op c1 c2) (unless (SCM_CHARP (SCM_CAR chars))
                              (Scm_TypeError "char" "character" (SCM_CAR chars)))
                (set! c1 c2)
                (set! c2 (Scm_CharFoldcase
                          (SCM_CHAR_VALUE (SCM_CAR chars))))
                (set! chars (SCM_CDR chars))]
               [else (result FALSE) (break)])))])
 )

(define-cproc char-ci=? (c1::<char> c2::<char> :rest chars)
  ::<boolean> :constant (char-ci-cmp ==))
(define-cproc char-ci<? (c1::<char> c2::<char> :rest chars)
  ::<boolean> :constant (char-ci-cmp <))
(define-cproc char-ci>? (c1::<char> c2::<char> :rest chars)
  ::<boolean> :constant (char-ci-cmp >))
(define-cproc char-ci<=? (c1::<char> c2::<char> :rest chars)
  ::<boolean> :constant (char-ci-cmp <=))
(define-cproc char-ci>=? (c1::<char> c2::<char> :rest chars)
  ::<boolean> :constant (char-ci-cmp >=))

(define-cproc char-alphabetic? (c::<char>) ::<boolean> Scm_CharAlphabeticP)
(define-cproc char-numeric? (c::<char>) ::<boolean> Scm_CharNumericP)
(define-cproc char-whitespace? (c::<char>) ::<boolean>
  (result (or (and (SCM_CHAR_ASCII_P c) (isspace c))
              (SCM_CHAR_EXTRA_WHITESPACE c))))
(define-cproc char-upper-case? (c::<char>) ::<boolean> Scm_CharUppercaseP)
(define-cproc char-lower-case? (c::<char>) ::<boolean> Scm_CharLowercaseP)

(define-cproc char->integer (c::<char>) ::<long> :constant
  (result (cast (signed long) c)))
(define-cproc integer->char (c::<int>) ::<char> :constant
  (result (cast ScmChar c)))

(define-cproc char-upcase (c::<char>)   ::<char> Scm_CharUpcase)
(define-cproc char-downcase (c::<char>) ::<char> Scm_CharDowncase)

(select-module gauche)
(define-cproc digit->integer (ch::<char> :optional (radix::<fixnum> 10))
  :constant
  (let* ([r::int])
    (when (or (< radix 2) (> radix 36))
      (Scm_Error "radix must be between 2 and 36, but got %d" radix))
    (set! r (Scm_DigitToInt ch radix))
    (result (?: (>= r 0) (SCM_MAKE_INT r) '#f))))

(define-cproc integer->digit (n::<fixnum> :optional (radix::<fixnum> 10))
  :constant
  (let* ([r::ScmChar])
    (when (or (< radix 2) (> radix 36))
      (Scm_Error "radix must be between 2 and 36, but got %d" radix))
    (set! r (Scm_IntToDigit n radix))
    (result (?: (== r SCM_CHAR_INVALID) '#f (SCM_MAKE_CHAR r)))))

(define-cproc ucs->char (n::<int>)
  (let* ([ch::ScmChar (Scm_UcsToChar n)])
    (result (?: (== ch SCM_CHAR_INVALID) '#f (SCM_MAKE_CHAR ch)))))

(define-cproc char->ucs (c::<char>)
  (let* ([ucs::int (Scm_CharToUcs c)])
    (result (?: (< ucs 0) '#f (Scm_MakeInteger ucs)))))

(define-cproc gauche-character-encoding () Scm_CharEncodingName)

(define-cproc supported-character-encodings ()
  (result (Scm_CStringArrayToList (Scm_SupportedCharacterEncodings) -1 0)))

(define-cproc supported-character-encoding? (encoding::<const-cstring>)
  ::<boolean> Scm_SupportedCharacterEncodingP)

(define-cproc char-title-case? (c::<char>)
  ::<boolean> :constant Scm_CharTitlecaseP)
(define-cproc char-titlecase (c::<char>) ::<char> Scm_CharTitlecase)
(define-cproc char-foldcase (c::<char>) ::<char> Scm_CharFoldcase)

(define-cproc char-general-category (c::<char>) :constant
  (case (Scm_CharGeneralCategory c)
    [(SCM_CHAR_CATEGORY_Lu) (result 'Lu)]
    [(SCM_CHAR_CATEGORY_Ll) (result 'Ll)]
    [(SCM_CHAR_CATEGORY_Lt) (result 'Lt)]
    [(SCM_CHAR_CATEGORY_Lm) (result 'Lm)]
    [(SCM_CHAR_CATEGORY_Lo) (result 'Lo)]
    [(SCM_CHAR_CATEGORY_Mn) (result 'Mn)]
    [(SCM_CHAR_CATEGORY_Mc) (result 'Mc)]
    [(SCM_CHAR_CATEGORY_Me) (result 'Me)]
    [(SCM_CHAR_CATEGORY_Nd) (result 'Nd)]
    [(SCM_CHAR_CATEGORY_Nl) (result 'Nl)]
    [(SCM_CHAR_CATEGORY_No) (result 'No)]
    [(SCM_CHAR_CATEGORY_Pc) (result 'Pc)]
    [(SCM_CHAR_CATEGORY_Pd) (result 'Pd)]
    [(SCM_CHAR_CATEGORY_Ps) (result 'Ps)]
    [(SCM_CHAR_CATEGORY_Pe) (result 'Pe)]
    [(SCM_CHAR_CATEGORY_Pi) (result 'Pi)]
    [(SCM_CHAR_CATEGORY_Pf) (result 'Pf)]
    [(SCM_CHAR_CATEGORY_Po) (result 'Po)]
    [(SCM_CHAR_CATEGORY_Sm) (result 'Sm)]
    [(SCM_CHAR_CATEGORY_Sc) (result 'Sc)]
    [(SCM_CHAR_CATEGORY_Sk) (result 'Sk)]
    [(SCM_CHAR_CATEGORY_So) (result 'So)]
    [(SCM_CHAR_CATEGORY_Zs) (result 'Zs)]
    [(SCM_CHAR_CATEGORY_Zl) (result 'Zl)]
    [(SCM_CHAR_CATEGORY_Zp) (result 'Zp)]
    [(SCM_CHAR_CATEGORY_Cc) (result 'Cc)]
    [(SCM_CHAR_CATEGORY_Cf) (result 'Cf)]
    [(SCM_CHAR_CATEGORY_Cs) (result 'Cs)]
    [(SCM_CHAR_CATEGORY_Co) (result 'Co)]
    [(SCM_CHAR_CATEGORY_Cn) (result 'Cn)]))


;;;
;;; Character sets
;;;

(select-module gauche)
(inline-stub
 (define-constant *char-code-max* (c "Scm_MakeInteger(SCM_CHAR_MAX)"))

 (define-cfn char_set_add (cs::ScmCharSet* chars) ::void :static
   (dolist [ch chars]
     (unless (SCM_CHARP ch) (Scm_Error "character required, but got %S" ch))
     (Scm_CharSetAddRange cs (SCM_CHAR_VALUE ch) (SCM_CHAR_VALUE ch))))
 )

(define-cproc char-set? (obj) ::<boolean> :constant SCM_CHARSETP)

(define-cproc char-set (:rest chars) ::<char-set>
  (let* ([cs::ScmCharSet* (SCM_CHARSET (Scm_MakeEmptyCharSet))])
    (char_set_add cs chars)
    (result cs)))

(define-cproc char-set-copy (cs::<char-set>) Scm_CharSetCopy)

(define (char-set-size cs)
  (rlet1 count 0
    (for-each (^[range] (inc! count (- (cdr range) (car range) -1)))
              ((with-module gauche.internal %char-set-ranges) cs))))

(define-cproc read-char-set
  (port::<input-port> :key (error::<boolean> #t) (posix-bracket::<boolean> #t))
  (result (Scm_CharSetRead port NULL error posix-bracket)))

(define-cproc char-set-contains? (cs::<char-set> ch::<char>)
  ::<boolean> :constant Scm_CharSetContains)

(define-cproc char-set-complement! (cs::<char-set>) Scm_CharSetComplement)

(define-in-module gauche (char-set-complement cs)
  (char-set-complement! (char-set-copy cs)))

(select-module gauche.internal)

(define-cproc %char-set-equal? (x::<char-set> y::<char-set>)
  ::<boolean> Scm_CharSetEq)

(define-cproc %char-set<=? (x::<char-set> y::<char-set>)
  ::<boolean> Scm_CharSetLE)

(define-cproc %char-set-add-chars! (cs::<char-set> chars::<list>) ::<char-set>
  (char_set_add cs chars) (result cs))

(define-cproc %char-set-add-range! (cs::<char-set> from to)
  (let* ([f::long -1] [t::long -1])
    (cond [(SCM_INTP from) (set! f (SCM_INT_VALUE from))]
          [(SCM_CHARP from) (set! f (SCM_CHAR_VALUE from))])
    (when (< f 0) (SCM_TYPE_ERROR from "character or positive exact integer"))
    (when (> f SCM_CHAR_MAX)
      (Scm_Error "'from' argument out of range: %S" from))
    (cond [(SCM_INTP to) (set! t (SCM_INT_VALUE to))]
          [(SCM_CHARP to) (set! t (SCM_CHAR_VALUE to))])
    (when (< t 0) (SCM_TYPE_ERROR to "character or positive exact integer"))
    (when (> t SCM_CHAR_MAX)
      (Scm_Error "'to' argument out of range: %S" to))
    (result (Scm_CharSetAddRange cs (cast ScmChar f) (cast ScmChar t)))))

(define-cproc %char-set-add! (dst::<char-set> src::<char-set>) Scm_CharSetAdd)
(define-cproc %char-set-ranges (cs::<char-set>) Scm_CharSetRanges)
(define-cproc %char-set-predefined (num::<fixnum>) Scm_GetStandardCharSet)

(define-cproc %char-set-dump (cs::<char-set>) ::<void>
  (Scm_CharSetDump cs SCM_CUROUT))
