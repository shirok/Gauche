;;;
;;; C type stuff
;;;
;;;   Copyright (c) 2021  Shiro Kawai  <shiro@acm.org>
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

;; Common definitions related to C types.
;; This is internal module; not to be directly use'd.

(define-module lang.c.type
  (use gauche.dictionary)
  (use util.match)
  (export grok-basic-type
          c-basic-type-integral?
          c-basic-type-flonum?
          c-basic-type-scalar-numeric?
          c-basic-type-numeric?
          c-type-narrower?
          c-wider-type)
  )
(select-module lang.c.type)

;; Type representation
;;   We use plain S-expr rather than a bunch of records, for flexibility.
;;
;;
;;   <c-type> : (<basic-type> <qual> ...)
;;            | (.pointer (<qual> ...) <c-type>)
;;            | (.array <c-type> <size> (<aqual> ...))
;;            | (.function (<fqual> ...>) <args> <c-type>)
;;            | (.struct <tag> (<field-name> <c-type> [<size>]) ...)
;;            | (.union <tag> (<field-name> <c-type> [<size>]) ...)
;;            | (.type <type-name> (<qual> ...) <c-type>)
;;
;;   <basic-type> : one of the symbols in *c-basic-types*
;;
;;   <qual>   : 'volatile | 'const | 'restrict
;;   <fqual>  : <qual> | 'inline
;;
;;   <aqual> : <qual> | 'static
;;   <size>  : scheme-integer
;;           | c-constant-expression
;;           | #f
;;
;;   <args>  : (<arg> ...)
;;           | (<arg> ... <rest-arg>)
;;           | 'unknown
;;   <arg>   : (<arg-name> <arg-type>)
;;   <arg-name> : symbol | #f
;;   <arg-type> : <c-type> | #f
;;   <rest-arg> : '...
;;
;;   <tag>    : <symbol> | #f
;;   <field-name> : <symbol> | #f
;;   <type-name> : symbol
;;
;;   The (.type ...) form is used for typedef'ed types.

(define-constant *c-basic-types*
  '(char s-char u-char
    short u-short
    int u-int
    long u-long
    long-long u-long-long
    float double long-double
    float-complex double-complex long-double-complex
    bool
    ;; the followings are non-standard
    long-long-double long-long-double-complex))

;; (const unsigned volatile int) -> (u-int (const volatile))
(define (grok-basic-type words typedef-dict)
  (define (bad) (error "Invalid type:" words))
  (define (integer-type sign size int)
    (ecase int
      [(#f int)
       (if (eq? sign 'unsigned)
         (ecase size
           [(#f)        'u-int]
           [(short)     'u-short]
           [(long)      'u-long]
           [(long-long) 'u-long-long])
         (ecase size
           [(#f)        'int]
           [(short)     'short]
           [(long)      'long]
           [(long-long) 'long-long]))]
      [(char)
       (when size (bad))
       (case sign
         [(#f)       'char]
         [(unsigned) 'u-char]
         [(signed)   's-char])]
      [(bool)
       (when (or size sign) (bad))
       'bool]))
  (define (float-type size complex? flo)
    (when (eq? size 'short) (bad))
    (let1 base (if (memq size '(long long-long))
                 (if (eq? flo 'float)
                   (bad)
                   (symbol-append size '-double))
                 flo)
      (if complex?
        (symbol-append base '-complex)
        base)))

  ;; we make quals appear in the order of the original code, just to make
  ;; testing and debugging easier---the caller shouldn't rely on that.
  (define (finish-type quals sign size complex? core)
    (case core
      [(#f int char bool) (if complex?
                            (bad)
                            `(,(integer-type sign size core)
                              ,(reverse quals)))]
      [(float double) (if sign
                        (bad)
                        `(,(float-type size complex? core)
                          ,(reverse quals)))]
      [else (if-let1 def (and typedef-dict (dict-get typedef-dict core #f))
              `(.type ,core ,(reverse quals) ,def)
              (error "Unknown typedef name:" core))]))

  (let loop ([words words]
             [quals '()]
             [sign #f]      ; #f, signed, unsigned
             [size #f]      ; short, long, long-long
             [complex? #f]
             [core #f])     ; int, char, bool, float, double, typedef-name
    (match words
      [() (finish-type quals sign size complex? core)]
      [('_Bool . r) (if core
                      (bad)
                      (loop r quals sign size complex? 'bool))]
      [('char . r)  (if core
                      (bad)
                      (loop r quals sign size complex? 'char))]
      [('int . r)   (if core
                      (bad)
                      (loop r quals sign size complex? 'int))]
      [('float . r) (if core
                      (bad)
                      (loop r quals sign size complex? 'float))]
      [('double . r) (if core
                       (bad)
                       (loop r quals sign size complex? 'double))]
      [('unsigned . r) (if sign
                         (bad)
                         (loop r quals 'unsigned size complex? core))]
      [('signed . r) (if sign
                       (bad)
                       (loop r quals 'signed size complex? core))]
      [('short . r) (if size
                      (bad)
                      (loop r quals sign 'short complex? core))]
      [('long . r) (case size
                     [(#f)  (loop r quals sign 'long complex? core)]
                     [(long) (loop r quals sign 'long-long complex? core)]
                     [else (bad)])]
      [('_Complex . r) (if complex?
                         (bad)
                         (loop r quals sign size #t core))]
      [((and (or 'const 'volatile 'restrict) q) . r)
       (loop r (cons q quals) sign size complex? core)]
      [(('ident name) . r) (loop r quals sign size complex? name)]
      [else (bad)])))

;; Types and its "weight".  The latter is used for type upgrading.
(define-constant *integral-type-weight*
  '((bool . 0) (char . 1) (s-char . 1) (u-char . 2) (short . 3) (u-short 4)
    (int . 5) (u-int . 6) (long . 7) (u-long . 8) (long-long . 9)
    (u-long-long . 10)))

(define-constant *float-type-weight*
  '((float . 10) (double . 11) (long-double . 12) (long-long-double . 13)))

(define-constant *complex-type-weight*
  '((float-complex . 20) (double-complex . 21)
    (long-double-complex . 11) (long-long-double-complex . 23)))

(define-constant *type-weight*
  (append *integral-type-weight*
          *float-type-weight*
          *complex-type-weight*))

(define (c-basic-type-integral? c-type)
  (match c-type
    [(t _) (boolean (assq t *integral-type-weight*))]
    [_ #f]))

(define (c-basic-type-flonum? c-type)
  (match c-type
    [(t _) (boolean (assq t *float-type-weight*))]
    [_ #f]))

(define (c-basic-type-scalar-numeric? c-type)
  (match c-type
    [(t _) (boolean (or (assq t *integral-type-weight*)
                        (assq t *float-type-weight*)))]
    [_ #f]))

(define (c-basic-type-numeric? c-type)
  (match c-type
    [(t _) (boolean (assq t *type-weight*))]
    [_ #f]))

;; for type coertion.  a and b must be numeric types.
(define (c-type-narrower? a b)
  (let ([wa (assq-ref *type-weight* (car a))]
        [wb (assq-ref *type-weight* (car b))])
    (< wa wb)))

(define (c-wider-type c-type-1 c-type-2)
  (and (c-basic-type-numeric? c-type-1)
       (c-basic-type-numeric? c-type-2)
       (if (c-type-narrower? c-type-1 c-type-2)
         c-type-2
         c-type-1)))
