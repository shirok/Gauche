;;;
;;; libchar.scm - builtin character procedures
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
       (cond [(not (SCM_PAIRP chars)) (return (,op c1 c2)) (break)]
             [(,op c1 c2) (unless (SCM_CHARP (SCM_CAR chars))
                            (Scm_TypeError "char" "character" (SCM_CAR chars)))
              (set! c1 c2)
              (set! c2 (SCM_CHAR_VALUE (SCM_CAR chars)))
              (set! chars (SCM_CDR chars))]
             [else (return FALSE) (break)]))])
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
         (cond [(not (SCM_PAIRP chars)) (return (,op c1 c2)) (break)]
               [(,op c1 c2) (unless (SCM_CHARP (SCM_CAR chars))
                              (Scm_TypeError "char" "character" (SCM_CAR chars)))
                (set! c1 c2)
                (set! c2 (Scm_CharFoldcase
                          (SCM_CHAR_VALUE (SCM_CAR chars))))
                (set! chars (SCM_CDR chars))]
               [else (return FALSE) (break)])))])
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
  (return (or (and (SCM_CHAR_ASCII_P c) (isspace c))
              (SCM_CHAR_EXTRA_WHITESPACE c))))
(define-cproc char-upper-case? (c::<char>) ::<boolean> Scm_CharUppercaseP)
(define-cproc char-lower-case? (c::<char>) ::<boolean> Scm_CharLowercaseP)

(define-cproc char->integer (c::<char>) ::<long> :constant
  (return (cast (signed long) c)))
(define-cproc integer->char (c::<int>) ::<char> :constant
  (return (cast ScmChar c)))

(define-cproc char-upcase (c::<char>)   ::<char> Scm_CharUpcase)
(define-cproc char-downcase (c::<char>) ::<char> Scm_CharDowncase)

(select-module gauche)
(define-cproc digit->integer (ch::<char> :optional (radix::<fixnum> 10) (extended-range?::<boolean> #f))
  :constant
  (let* ([r::int])
    (when (or (< radix 2) (> radix 36))
      (Scm_Error "radix must be between 2 and 36, but got %d" radix))
    (when (and extended-range? (> radix 10))
      (Scm_Error "for extended range, radix can't exceed 10" radix))
    (set! r (Scm_DigitToInt ch radix extended-range?))
    (return (?: (>= r 0) (SCM_MAKE_INT r) '#f))))

(define-cproc integer->digit (n::<fixnum> :optional (radix::<fixnum> 10) (basechar1::<char> #\0) (basechar2::<char> #\a))
  :constant
  (let* ([r::ScmChar])
    (when (or (< radix 2) (> radix 36))
      (Scm_Error "radix must be between 2 and 36, but got %d" radix))
    (set! r (Scm_IntToDigit n radix basechar1 basechar2))
    (return (?: (== r SCM_CHAR_INVALID) '#f (SCM_MAKE_CHAR r)))))

(define-cproc ucs->char (n::<int>)
  (let* ([ch::ScmChar (Scm_UcsToChar n)])
    (return (?: (== ch SCM_CHAR_INVALID) '#f (SCM_MAKE_CHAR ch)))))

(define-cproc char->ucs (c::<char>)
  (let* ([ucs::int (Scm_CharToUcs c)])
    (return (?: (< ucs 0) '#f (Scm_MakeInteger ucs)))))

(define-cproc gauche-character-encoding () Scm_CharEncodingName)

(define-cproc supported-character-encodings ()
  (return (Scm_CStringArrayToList (Scm_SupportedCharacterEncodings) -1 0)))

(define-cproc supported-character-encoding? (encoding::<const-cstring>)
  ::<boolean> Scm_SupportedCharacterEncodingP)

(define-cproc char-title-case? (c::<char>)
  ::<boolean> :constant Scm_CharTitlecaseP)
(define-cproc char-titlecase (c::<char>) ::<char> Scm_CharTitlecase)
(define-cproc char-foldcase (c::<char>) ::<char> Scm_CharFoldcase)

(define-cproc char-general-category (c::<char>) :constant
  (case (Scm_CharGeneralCategory c)
    [(SCM_CHAR_CATEGORY_Lu) (return 'Lu)]
    [(SCM_CHAR_CATEGORY_Ll) (return 'Ll)]
    [(SCM_CHAR_CATEGORY_Lt) (return 'Lt)]
    [(SCM_CHAR_CATEGORY_Lm) (return 'Lm)]
    [(SCM_CHAR_CATEGORY_Lo) (return 'Lo)]
    [(SCM_CHAR_CATEGORY_Mn) (return 'Mn)]
    [(SCM_CHAR_CATEGORY_Mc) (return 'Mc)]
    [(SCM_CHAR_CATEGORY_Me) (return 'Me)]
    [(SCM_CHAR_CATEGORY_Nd) (return 'Nd)]
    [(SCM_CHAR_CATEGORY_Nl) (return 'Nl)]
    [(SCM_CHAR_CATEGORY_No) (return 'No)]
    [(SCM_CHAR_CATEGORY_Pc) (return 'Pc)]
    [(SCM_CHAR_CATEGORY_Pd) (return 'Pd)]
    [(SCM_CHAR_CATEGORY_Ps) (return 'Ps)]
    [(SCM_CHAR_CATEGORY_Pe) (return 'Pe)]
    [(SCM_CHAR_CATEGORY_Pi) (return 'Pi)]
    [(SCM_CHAR_CATEGORY_Pf) (return 'Pf)]
    [(SCM_CHAR_CATEGORY_Po) (return 'Po)]
    [(SCM_CHAR_CATEGORY_Sm) (return 'Sm)]
    [(SCM_CHAR_CATEGORY_Sc) (return 'Sc)]
    [(SCM_CHAR_CATEGORY_Sk) (return 'Sk)]
    [(SCM_CHAR_CATEGORY_So) (return 'So)]
    [(SCM_CHAR_CATEGORY_Zs) (return 'Zs)]
    [(SCM_CHAR_CATEGORY_Zl) (return 'Zl)]
    [(SCM_CHAR_CATEGORY_Zp) (return 'Zp)]
    [(SCM_CHAR_CATEGORY_Cc) (return 'Cc)]
    [(SCM_CHAR_CATEGORY_Cf) (return 'Cf)]
    [(SCM_CHAR_CATEGORY_Cs) (return 'Cs)]
    [(SCM_CHAR_CATEGORY_Co) (return 'Co)]
    [(SCM_CHAR_CATEGORY_Cn) (return 'Cn)]))


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
    (return cs)))

(define-cproc char-set-copy (cs::<char-set>) Scm_CharSetCopy)

(define (char-set-size cs)
  (rlet1 count 0
    (for-each (^[range] (inc! count (- (cdr range) (car range) -1)))
              ((with-module gauche.internal %char-set-ranges) cs))))

(define-cproc read-char-set
  (port::<input-port> :key (error::<boolean> #t) (posix-bracket::<boolean> #t))
  (return (Scm_CharSetRead port NULL error posix-bracket)))

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
  (char_set_add cs chars) (return cs))

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
    (return (Scm_CharSetAddRange cs (cast ScmChar f) (cast ScmChar t)))))

(define-cproc %char-set-add! (dst::<char-set> src::<char-set>) Scm_CharSetAdd)
(define-cproc %char-set-ranges (cs::<char-set>) Scm_CharSetRanges)
(define-cproc %char-set-predefined (num::<fixnum>) Scm_GetStandardCharSet)

(define-cproc %char-set-dump (cs::<char-set>) ::<void>
  (Scm_CharSetDump cs SCM_CUROUT))
