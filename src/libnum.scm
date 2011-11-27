;;;
;;; libnum.stub - builtin number libraries
;;;  
;;;   Copyright (c) 2000-2011  Shiro Kawai  <shiro@acm.org>
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
                     <gauche/bignum.h>
                     <stdlib.h>
                     <math.h>))
 (when "!defined(M_PI)"
   (declcode "#define M_PI 3.1415926535897932384")))

;;
;; Predicates
;;

(select-module scheme)
(define-cproc number? (obj)  ::<boolean> :fast-flonum :constant
  (inliner NUMBERP) SCM_NUMBERP)
(define-cproc complex? (obj) ::<boolean> :fast-flonum :constant
  (inliner NUMBERP) SCM_NUMBERP)
(define-cproc real? (obj)    ::<boolean> :fast-flonum :constant
  (inliner REALP) SCM_REALP)
(define-cproc rational? (obj)::<boolean> :fast-flonum :constant
  (result (and (SCM_REALP obj) (Scm_FiniteP obj))))
(define-cproc integer? (obj) ::<boolean> :fast-flonum :constant
  (result (and (SCM_NUMBERP obj) (Scm_IntegerP obj))))

(define-cproc exact? (obj)   ::<boolean> :fast-flonum :constant SCM_EXACTP)
(define-cproc inexact? (obj) ::<boolean> :fast-flonum :constant SCM_INEXACTP)

(define-cproc zero? (obj::<number>) ::<boolean> :fast-flonum :constant
  (result (and (SCM_REALP obj) (== (Scm_Sign obj) 0))))

(define-cproc positive? (obj) ::<boolean> :fast-flonum :constant
  (result (> (Scm_Sign obj) 0)))
(define-cproc negative? (obj) ::<boolean> :fast-flonum :constant
  (result (< (Scm_Sign obj) 0)))
(define-cproc odd? (obj)  ::<boolean> :fast-flonum :constant Scm_OddP)
(define-cproc even? (obj) ::<boolean> :fast-flonum :constant
  (result (not (Scm_OddP obj))))

(select-module gauche)
;; fixnum? and bignum? is not :constant, since it is platform-dependent.
(define-cproc fixnum? (x) ::<boolean> :fast-flonum SCM_INTP)
(define-cproc bignum? (x) ::<boolean> :fast-flonum SCM_BIGNUMP)
(define-cproc flonum? (x) ::<boolean> :fast-flonum :constant SCM_FLONUMP)

(define-cproc finite?   (x::<number>) ::<boolean> :fast-flonum Scm_FiniteP)
(define-cproc infinite? (x::<number>) ::<boolean> :fast-flonum Scm_InfiniteP)
(define-cproc nan?      (x::<number>) ::<boolean> :fast-flonum Scm_NanP)

;;
;; Platform introspection
;;

(select-module gauche)
;; Names are from R6RS.
(define-cproc fixnum-width ()    ::<int> (result SCM_SMALL_INT_SIZE))
(define-cproc least-fixnum ()    ::<long> (result SCM_SMALL_INT_MIN))
(define-cproc greatest-fixnum () ::<long> (result SCM_SMALL_INT_MAX))

;; default-endian is defined in Scm__InitNumber().
(define-cproc native-endian () Scm_NativeEndian)

(select-module gauche.internal)
(define-cproc %bignum-dump (obj) ::<void>
  (when (SCM_BIGNUMP obj)
    (Scm_DumpBignum (SCM_BIGNUM obj) SCM_CUROUT)))

;;
;; Comparison
;;

(select-module scheme)
(inline-stub
 ;; NB: numeric procedures =, <, <=, >, >=, +, -, * and / have inliners
 ;; defined in compile.scm.   When one of these operators appears at the
 ;; operator position of an expression, it is inlined so that the following
 ;; SUBRs won't be called.  N-ary arithmetic operations (N>2) are expanded
 ;; to a series of binary arithmetic operations in the compiler.
 ;; N-ary comparison operations (N>2) are NOT expanded by the compiler,
 ;; and these SUBRs are called.  In order to avoid extra consing for those
 ;; N-ary comparison expressions, we use :optarray to receive the first few
 ;; arguments on stack.
 ;;
 ;; SUBRs are always called when those numeric procedures are invoked via
 ;; apply, or as a result of expression in the operator position,
 ;; such as ((if x + *) 2 3).

 (define-cise-stmt numcmp
   [(_ compar)
    `(begin
       (result FALSE)
       (cond [(not (,compar arg0 arg1))]
             [(== optcnt 0) (result TRUE)]
             [(not (,compar arg1 (aref oarg 0)))]
             [(== optcnt 1) (result TRUE)]
             [(not (,compar (aref oarg 0) (aref oarg 1)))]
             [(and (== optcnt 2) (SCM_NULLP args)) (result TRUE)]
             [else
              (set! arg0 (aref oarg 1)
                    arg1 (SCM_CAR args)
                    args (SCM_CDR args))
              (loop (cond [(not (,compar arg0 arg1)) (break)]
                          [(SCM_NULLP args) (result TRUE) (break)]
                          [else (set! arg0 arg1
                                      arg1 (SCM_CAR args)
                                      args (SCM_CDR args))]))]))])
 )

(define-cproc =  (arg0 arg1 :optarray (oarg optcnt 2) :rest args)
  ::<boolean> :fast-flonum :constant (numcmp Scm_NumEq))
(define-cproc <  (arg0 arg1 :optarray (oarg optcnt 2) :rest args)
  ::<boolean> :fast-flonum :constant (numcmp Scm_NumLT))
(define-cproc <= (arg0 arg1 :optarray (oarg optcnt 2) :rest args)
  ::<boolean> :fast-flonum :constant (numcmp Scm_NumLE))
(define-cproc >  (arg0 arg1 :optarray (oarg optcnt 2) :rest args)
  ::<boolean> :fast-flonum :constant (numcmp Scm_NumGT))
(define-cproc >= (arg0 arg1 :optarray (oarg optcnt 2) :rest args)
  ::<boolean> :fast-flonum :constant (numcmp Scm_NumGE))

(define-cproc max (arg0 :rest args) ::<number> :constant
  (Scm_MinMax arg0 args NULL (& SCM_RESULT)))
(define-cproc min (arg0 :rest args) ::<number> :constant
  (Scm_MinMax arg0 args (& SCM_RESULT) NULL))

(select-module gauche)
(define-cproc min&max (arg0 :rest args) ::(<top> <top>)
  (Scm_MinMax arg0 args (& SCM_RESULT0) (& SCM_RESULT1)))

;;
;; Conversions
;;

(select-module scheme)
(define-cproc exact->inexact (obj) :fast-flonum :constant Scm_Inexact)
(define-cproc inexact->exact (obj) :fast-flonum :constant Scm_Exact)

(define-cproc number->string
  (obj :optional (radix::<fixnum> 10) (use-upper? #f)) :fast-flonum :constant
  (result (Scm_NumberToString obj radix (not (SCM_FALSEP use_upperP)))))

(define-cproc string->number (obj::<string> :optional (radix::<fixnum> 10))
  (result (Scm_StringToNumber obj radix FALSE)))

(select-module gauche)
(define-cproc floor->exact (num) :fast-flonum :constant
  (result (Scm_RoundToExact num SCM_ROUND_FLOOR)))
(define-cproc ceiling->exact (num) :fast-flonum :constant
  (result (Scm_RoundToExact num SCM_ROUND_CEIL)))
(define-cproc truncate->exact (num) :fast-flonum :constant
  (result (Scm_RoundToExact num SCM_ROUND_TRUNC)))
(define-cproc round->exact (num) :fast-flonum :constant
  (result (Scm_RoundToExact num SCM_ROUND_ROUND)))

(define-cproc decode-float (num)        ;from ChezScheme
  (cond [(SCM_FLONUMP num)
         (let* ([exp::int] [sign::int]
                [f (Scm_DecodeFlonum (SCM_FLONUM_VALUE num) (& exp) (& sign))]
                [v (Scm_MakeVector 3 '#f)])
           (set! (SCM_VECTOR_ELEMENT v 0) f
                 (SCM_VECTOR_ELEMENT v 1) (Scm_MakeInteger exp)
                 (SCM_VECTOR_ELEMENT v 2) (Scm_MakeInteger sign))
           (result v))]
        [(SCM_INTP num)
         (let* ([v (Scm_MakeVector 3 '#f)])
           (set! (SCM_VECTOR_ELEMENT v 0) (Scm_Abs num)
                 (SCM_VECTOR_ELEMENT v 1) (Scm_MakeInteger 0)
                 (SCM_VECTOR_ELEMENT v 2) (Scm_MakeInteger (Scm_Sign num)))
           (result v))]
        [else (SCM_TYPE_ERROR num "real number") (result SCM_UNDEFINED)]))

;;
;; Arithmetics
;;

(select-module scheme)
(define-cproc * (:rest args) ::<number> :fast-flonum
  (cond [(not (SCM_PAIRP args)) (result (SCM_MAKE_INT 1))]
        [else (let* ([r::ScmObj (SCM_CAR args)])
                (dolist [v (SCM_CDR args)] (set! r (Scm_Mul r v)))
                (result r))]))

(define-cproc + (:rest args) ::<number> :fast-flonum
  (cond [(not (SCM_PAIRP args)) (result (SCM_MAKE_INT 0))]
        [else (let* ([r::ScmObj (SCM_CAR args)])
                (dolist [v (SCM_CDR args)] (set! r (Scm_Add r v)))
                (result r))]))

(define-cproc - (arg1 :rest args) ::<number> :fast-flonum
  (if (SCM_NULLP args)
    (result (Scm_VMNegate arg1))
    (begin (dolist [v args] (set! arg1 (Scm_Sub arg1 v)))
           (result arg1))))

(define-cproc / (arg1 :rest args) ::<number> :fast-flonum
  (if (SCM_NULLP args)
    (result (Scm_VMReciprocal arg1))
    (begin (dolist [v args] (set! arg1 (Scm_Div arg1 v)))
           (result arg1))))

(define-cproc abs (obj) :fast-flonum :constant Scm_VMAbs)

(define-cproc quotient (n1 n2) :fast-flonum :constant
  (result (Scm_Quotient n1 n2 NULL)))
(define-cproc remainder (n1 n2) :fast-flonum :constant
  (result (Scm_Modulo n1 n2 TRUE)))
(define-cproc modulo (n1 n2)    :fast-flonum :constant
  (result (Scm_Modulo n1 n2 FALSE)))

(define-cproc %gcd (n1 n2)    :fast-flonum :constant Scm_Gcd)
(define-cproc numerator (n)   :fast-flonum :constant Scm_Numerator)
(define-cproc denominator (n) :fast-flonum :constant Scm_Denominator)
;; gcd, lcd - in gauche/numerical.scm

(define-cproc floor (v) ::<number> :fast-flonum :constant
  (result (Scm_Round v SCM_ROUND_FLOOR)))
(define-cproc ceiling (v) ::<number> :fast-flonum :constant
  (result (Scm_Round v SCM_ROUND_CEIL)))
(define-cproc truncate (v) ::<number> :fast-flonum :constant
  (result (Scm_Round v SCM_ROUND_TRUNC)))
(define-cproc round (v) ::<number> :fast-flonum :constant
  (result (Scm_Round v SCM_ROUND_ROUND)))

;; rationalize - not implemented


;; Transcedental functions.  Complex numbers are handled in Scheme.

(define-cproc %exp (x::<real>) ::<real> :fast-flonum :constant exp)

(define-cproc %log (x) ::<number> :fast-flonum :constant
  (unless (SCM_REALP x) (SCM_TYPE_ERROR x "real number"))
  (if (< (Scm_Sign x) 0)
    (result (Scm_MakeComplex (log (- (Scm_GetDouble x))) M_PI))
    ;; NB: I intentionally delegate handling of the case x==0.0 to the
    ;; system log() function.  Most systems should yield NaN or Inf.
    (result (Scm_VMReturnFlonum (log (Scm_GetDouble x))))))

(define-cproc %sin (x::<real>) ::<real> :fast-flonum :constant sin)
(define-cproc %cos (x::<real>) ::<real> :fast-flonum :constant cos)
(define-cproc %tan (x::<real>) ::<real> :fast-flonum :constant tan)

(define-cproc %asin (x::<real>) ::<number> :fast-flonum :constant
  (cond [(> x 1.0)
         (result (Scm_MakeComplex (/ M_PI 2.0)
                                  (- (log (+ x (sqrt (- (* x x) 1.0)))))))]
        [(< x -1.0)
         (result (Scm_MakeComplex (/ (- M_PI) 2.0)
                                  (- (log (- (- x) (sqrt (- (* x x) 1.0)))))))]
        [else (result (Scm_VMReturnFlonum (asin x)))]))

(define-cproc %acos (x::<real>) ::<number> :fast-flonum :constant
  (cond [(> x 1.0)
         (result (Scm_MakeComplex 0 (log (+ x (sqrt (- (* x x) 1.0))))))]
        [(< x -1.0)
         (result (Scm_MakeComplex 0 (log (+ x (sqrt (- (* x x) 1.0))))))]
        [else (result (Scm_VMReturnFlonum (acos x)))]))

(define-cproc %atan (z::<real> :optional x) ::<double> :fast-flonum :constant
  (cond [(SCM_UNBOUNDP x) (result (atan z))]
        [else (unless (SCM_REALP x) (SCM_TYPE_ERROR x "real number"))
              (result (atan2 z (Scm_GetDouble x)))]))

(define-cproc %sinh (x::<real>) ::<real> :fast-flonum :constant sinh)
(define-cproc %cosh (x::<real>) ::<real> :fast-flonum :constant cosh)
(define-cproc %tanh (x::<real>) ::<real> :fast-flonum :constant tanh)
;; NB: asinh and acosh are not in POSIX.

(define-cproc %sqrt (x::<real>) :fast-flonum :constant
  (if (< x 0)
    (result (Scm_MakeComplex 0.0 (sqrt (- x))))
    (result (Scm_VMReturnFlonum (sqrt x)))))

(define-cproc %expt (x y) :fast-flonum :constant Scm_Expt)

(select-module gauche)

(define-cproc ash (num cnt::<fixnum>) :constant Scm_Ash)

(define-cproc lognot (x) :constant Scm_LogNot)

(inline-stub
 (define-cise-stmt logop
   [(_ fn ident)
    `(cond [(== optcnt 0) (result ,ident)]
           [(== optcnt 1)
            (unless (SCM_INTEGERP (aref arg2 0))
              (Scm_Error "Exact integer required, but got %S" (aref arg2 0)))
            (result (aref arg2 0))]
           [else
            (let* ([r (,fn (aref arg2 0) (aref arg2 1))])
              (for-each (lambda (v) (set! r (,fn r v))) args)
              (result r))])])
 )

(define-cproc logand (:optarray (arg2 optcnt 2) :rest args) :constant
  (logop Scm_LogAnd (SCM_MAKE_INT -1)))
(define-cproc logior (:optarray (arg2 optcnt 2) :rest args) :constant
  (logop Scm_LogIor (SCM_MAKE_INT 0)))
(define-cproc logxor (:optarray (arg2 optcnt 2) :rest args) :constant
  (logop Scm_LogXor (SCM_MAKE_INT 0)))

(define-cproc logcount (n) ::<int> :constant 
  (cond [(SCM_EQ n (SCM_MAKE_INT 0)) (result 0)]
        [(SCM_INTP n)
         (let* ([z::ScmBits (cast ScmBits (cast long (SCM_INT_VALUE n)))])
           (if (> (SCM_INT_VALUE n) 0)
             (result (Scm_BitsCount1 (& z) 0 SCM_WORD_BITS))
             (result (Scm_BitsCount0 (& z) 0 SCM_WORD_BITS))))]
        [(SCM_BIGNUMP n) (result (Scm_BignumLogCount (SCM_BIGNUM n)))]
        [else (SCM_TYPE_ERROR n "exact integer") (result 0)]))

(define-cproc integer-length (n) ::<int> :constant
  (cond [(SCM_INTP n)
         (let* ([z::ScmBits (cast ScmBits (cast long (SCM_INT_VALUE n)))])
           (if (>= (SCM_INT_VALUE n) 0)
             (result (+ (Scm_BitsHighest1 (& z) 0 SCM_WORD_BITS) 1))
             (result (+ (Scm_BitsHighest0 (& z) 0 SCM_WORD_BITS) 1))))]
        [(SCM_BIGNUMP n)
         ;; 2's complement adjustment.
         (when (< (SCM_BIGNUM_SIGN n) 0)
           (set! n (Scm_Add n (SCM_MAKE_INT 1))))
         ;; The above operation may change n to fixnum, so we check again
         (if (SCM_BIGNUMP n)
           (let* ([z::ScmBits* (cast ScmBits* (-> (SCM_BIGNUM n) values))]
                  [k::int (SCM_BIGNUM_SIZE n)])
             (result (+ (Scm_BitsHighest1 z 0 (* k SCM_WORD_BITS)) 1)))
           ;; If n+1 becomes fixnum, we know n was the least fixnum.
           (result (+ SCM_SMALL_INT_SIZE 1)))]
        [else (SCM_TYPE_ERROR n "exact integer") (result 0)]))

;; As of 0.8.8 we started to support exact rational numbers.  Some existing
;; code may count on exact integer division to be coerced to flonum
;; if it isn't produce a whole number, and such programs start
;; running very slowly on 0.8.8 by introducing unintentional exact
;; rational arithmetic.
;;
;; For the smooth transition, we provide the original behavior as
;; inexact-/.  If the program uses compat.no-rational, '/' is overridden
;; by inexact-/ and the old code behaves the same.
(define-cproc inexact-/ (arg1 :rest args)
  (cond [(SCM_NULLP args) (result (Scm_ReciprocalInexact arg1))]
        [else (dolist [x args] (set! arg1 (Scm_DivInexact arg1 x)))
              (result arg1)]))

;; Inexact arithmetics.  Useful for speed-sensitive code to avoid
;; accidental use of bignum or ratnum.   We might want to optimize
;; these more, even adding special VM insns for them.
(define-cproc +. (:rest args) :constant
  (let* ([a '0.0])
    (dolist [x args] (set! a (Scm_Add a (Scm_Inexact x))))
    (result a)))
(define-cproc *. (:rest args) :constant
  (let* ([a '1.0])
    (dolist [x args] (set! a (Scm_Mul a (Scm_Inexact x))))
    (result a)))
(define-cproc -. (arg1 :rest args) :constant
  (cond
   [(SCM_NULLP args) (result (Scm_Negate (Scm_Inexact arg1)))]
   [else (dolist [x args] (set! arg1 (Scm_Sub arg1 (Scm_Inexact x))))
         (result arg1)]))
(define-cproc /. (arg1 :rest args) :constant
  (cond
   [(SCM_NULLP args) (result (Scm_Reciprocal (Scm_Inexact arg1)))]
   [else (dolist [x args] (set! arg1 (Scm_Div arg1 (Scm_Inexact x))))
         (result arg1)]))

(define-cproc clamp (x :optional (min #f) (max #f)) :fast-flonum :constant
  (let* ([r x] [maybe_exact::int (SCM_EXACTP x)])
    (unless (SCM_REALP x) (SCM_TYPE_ERROR x "real number"))
    (cond [(SCM_EXACTP min) (when (< (Scm_NumCmp x min) 0) (set! r min))]
          [(SCM_FLONUMP min)
           (set! maybe_exact FALSE)
           (when (< (Scm_NumCmp x min) 0) (set! r min))]
          [(not (SCM_FALSEP min)) (SCM_TYPE_ERROR min "real number or #f")])
    (cond [(SCM_EXACTP max) (when (> (Scm_NumCmp x max) 0) (set! r max))]
          [(SCM_FLONUMP max)
           (set! maybe_exact FALSE)
           (when (> (Scm_NumCmp x max) 0) (set! r max))]
          [(not (SCM_FALSEP max)) (SCM_TYPE_ERROR max "real number or #f")])
    (if (and (not maybe_exact) (SCM_EXACTP r))
      (return (Scm_Inexact r))
      (return r))))

(define-cproc quotient&remainder (n1 n2) ::(<top> <top>)
  (set! SCM_RESULT0 (Scm_Quotient n1 n2 (& SCM_RESULT1))))

;;
;; Complex numbers
;;

(select-module gauche)
(define-cproc make-rectangular (a::<real> b::<real>) :constant Scm_MakeComplex)
(define-cproc make-polar (r::<real> t::<real>) :constant Scm_MakeComplexPolar)

;; we don't use Scm_RealPart and Scm_ImagPart, for preserving exactness
;; and avoiding extra allocation.
(define-cproc real-part (z::<number>) :fast-flonum :constant
  (if (SCM_REALP z)
    (result z)
    (result (Scm_VMReturnFlonum (SCM_COMPNUM_REAL z)))))

(define-cproc imag-part (z::<number>) :fast-flonum :constant
  (cond [(SCM_EXACTP z) (result (SCM_MAKE_INT 0))]
        [(SCM_REALP z)  (result (Scm_VMReturnFlonum 0.0))]
        [else (result (Scm_VMReturnFlonum (SCM_COMPNUM_IMAG z)))]))

(define-cproc magnitude (z) ::<double> :fast-flonum :constant Scm_Magnitude)
(define-cproc angle (z)     ::<double> :fast-flonum :constant Scm_Angle)
