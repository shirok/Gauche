;;;
;;; computil.scm - comparator primitives.  autoloaded
;;;
;;;   Copyright (c) 2014  Shiro Kawai  <shiro@acm.org>
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

;; Provides some essential comparator objects & procedures.
;; Autoloaded.
;; Complete comparator API will be provided by srfi-114 module.

(define-module gauche.computil
  (export default-comparator

          boolean-comparator char-comparator char-ci-comparator
          string-comparator string-ci-comparator symbol-comparator
          exact-integer-comparator integer-comparator rational-comparator
          real-comparator complex-comparator number-comparator
          pair-comparator list-comparator vector-comparator
          bytevector-comparator uvector-comparator
          
          eq-comparator eqv-comparator equal-comparator))
(select-module gauche.computil)

;; Needed to have string-ci compare.
;; TODO: We don't want to depend on text.unincode here, though.
(autoload text.unicode string-foldcase)

(define (a-number? x) (and (number? x) (not (nan? x))))
(define (a-real-number? x) (and (real? x) (not (nan? x))))

;; internal
(define (%default-comparator-accepts? v)
  (any-pred boolean? a-number? char? string? symbol?
            pair? vector? uvector?))

(define default-comparator
  (make-comparator %default-comparator-accepts?
                   #t compare hash 'default-comparator))

(define eq-comparator
  (make-comparator #t eq? #f eq-hash 'eq-comparator))
(define eqv-comparator
  (make-comparator #t eqv? #f eqv-hash 'eqv-comparator))
(define equal-comparator
  (make-comparator #t equal? #f hash 'equal-comparator))

(define boolean-comparator
  (make-comparator boolean? eqv? compare eq-hash 'boolean-comparator))
(define char-comparator
  (make-comparator char? eqv? compare eqv-hash 'char-comparator))
(define char-ci-comparator
  ($ make-comparator char? char-ci=? 
     (^[a b] (compare (char-foldcase a) (char-foldcase b)))
     eqv-hash 'char-ci-comparator))
(define string-comparator
  ($ make-comparator string? string=? compare
     (with-module gauche.internal %hash-string)
     'string-comparator))
(define string-ci-comparator
  ($ make-comparator string? string=?
     (^[a b] (compare (string-foldcase a) (string-foldcase b)))
     (^s ((with-module gauche.internal %hash-string) (string-foldcase s)))
     'string-ci-comparator))
(define symbol-comparator
  (make-comparator symbol? eq? compare eq-hash 'symbol-comparator))

;; Number comparators
;; For integer-comparator hash, we need 1 and 1.0 to yield the same
;; hash value.  Any inexact integer can be mapped to exact integer,
;; so we convert the former to the latter to hash.
(define exact-integer-comparator
  (make-comparator exact-integer? eqv? compare eqv-hash
                   'exact-integer-comparator))
(define integer-comparator
  (make-comparator integer? = compare (^n (eqv-hash (exact n)))
                   'integer-comparator))
(define rational-comparator
  (make-comparator rational? = compare eqv-hash 'rational-comparator))
(define real-comparator
  (make-comparator a-real-number? = compare eqv-hash 'real-comparator))
(define complex-comparator
  (make-comparator a-number? compare eqv-hash 'complex-comparator))
(define number-comparator complex-comparator)

(define pair-comparator
  (make-comparator pair? #t compare hash 'pair-comparator))
(define list-comparator
  (make-comparator list? #t compare hash 'list-comparator))
(define vector-comparator
  (make-comparator vector? #t compare hash 'vector-comparator))
(define uvector-comparator
  (make-comparator uvector? equal? compare hash 'uvector-comparator))
(define bytevector-comparator
  (make-comparator (cut is-a? <> <u8vector>) equal? compare hash
                   'bytevector-comparator))
