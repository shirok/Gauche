;;;
;;; C type stuff
;;;
;;;   Copyright (c) 2021-2022  Shiro Kawai  <shiro@acm.org>
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
  (use srfi-13)
  (export grok-c-type
          c-primary-type
          c-basic-type-integral?
          c-basic-type-flonum?
          c-basic-type-scalar-numeric?
          c-basic-type-numeric?
          c-type-narrower?
          c-wider-type
          c-actual-type)
  )
(select-module lang.c.type)

;; Type representation
;;   We use plain S-expr rather than a bunch of records, for flexibility.
;;
;;
;;   <c-type> : (<basic-type> (<qual> ...))
;;            | (.pointer (<qual> ...) <c-type>)
;;            | (.array <c-type> <size> (<aqual> ...))
;;            | (.function (<fqual> ...>) <c-type> <args>)
;;            | (.struct <tag> (<qual> ...)
;;                   ((<field-name> <c-type> [<size>]) ...))
;;            | (.union <tag> (<qual> ...)
;;                   ((<field-name> <c-type> [<size>]) ...))
;;            | (.enum <tag> (<qual> ...)
;;                   ((<enum-name> <enum-value>) ...))
;;            | (.type <type-name> (<qual> ...) <c-type>)
;;
;;   <basic-type> : one of the symbols in *c-basic-types*
;;
;;   <qual>   : 'volatile | 'const | 'restrict
;;            | ('attribute ...) | ('asm ...)
;;   <fqual>  : 'inline
;;
;;   <aqual> : <qual> | 'static
;;   <size>  : scheme-integer
;;           | c-constant-expression
;;           | #f
;;
;;   <args>  : (<arg> ...)
;;           | (<arg> ... <rest-arg>)
;;           | 'unknown-args
;;   <arg>   : (<arg-name> <arg-type>)
;;   <arg-name> : symbol | #f
;;   <arg-type> : <c-type> | #f
;;   <rest-arg> : '...
;;
;;   <tag>    : <symbol> | #f
;;   <field-name> : <symbol> | #f
;;   <type-name> : symbol
;;
;;   <enum-name> : symbol
;;   <enum-value> : scheme-integer
;;                | c-constant-expression
;;                | #f
;;
;;   The (.type ...) form is used for typedef'ed types.


;; API
;;  Input is a list of type specs obtained from the parser.
;;
;;   <type-specs> : (<type-modifier> ... <core-type-spec>)
;;   <type-modifier> : (* <qual> ...)
;;                   | (array-dimension (<aqual> ...) <dim-expr>)
;;                   | (function <params>)
;;                   | (attribute <attrs>)
;;                   | (asm <asm>)
;;   <params> : (<param-spec> ...)
;;            | (<param-spec> ... '...)
;;            | 'no-args
;;            | 'unknown-args
;;   <core-type-spec> : mixed list of zero or more <qual>s, and <core-type>
;;   <core-type> : <basic-type>
;;               | (struct <tag> <member-spec> ...)
;;               | (union <tag> <member-spec> ...)
;;               | (enum <tag> <member-id> ...)
;;
;;  NB: attribute and asm may appear in both type-modifier and in core quals
;;
;;  Output is a well-formed <c-type>.
(define (grok-c-type type-specs typedef-dict)
  (define (rec type-specs)
    (match type-specs
      [() '()]
      [(('* . quals) . rest) `(.pointer ,quals ,(rec rest))]
      [(('array-dimension quals size) . rest)
       (match (fold-c-constant size)
         [#f `(.array ,(rec rest) ,quals ,size)] ;variable size
         [(val type)
          (unless (c-basic-type-integral? type)
            (error "non-integral array dimension not allowed:" type-specs))
          `(.array ,(rec rest) ,quals ,val)])]
      [(('function params) . rest)
       ;; special handling of 'inline' - syntactically it is attached to
       ;; the return type, but we need to attach it to the function part.
       (let* ([inline? (memq 'inline (last rest))]
              [return-type (rec (if inline?
                                          (append
                                           (drop-right rest 1)
                                           (list (delete 'inline (last rest))))
                                          rest))]
              [param-spec (case params
                            [(no-args) '()]
                            [(unknown-args) 'unknown-args]
                            [else (map (^p (match p
                                             [('ident n) `(,n #f)]
                                             [(n t) p]
                                             ['... p]))
                                       params)])])
         `(.function ,(if inline? '(inline) '()) ,return-type ,param-spec))]
      [((and ('attribute . _) attr) . rest) (attach-qualifier (rec rest) attr)]
      [((and ('asm . _) asm) . rest) (attach-qualifier (rec rest) asm)]
      [((xs ...)) (grok-core-type xs typedef-dict)]
      [_ (error "[internal] unrecognized type specs:" type-specs)]))
  (rec type-specs))

(define-constant *c-basic-types*
  '(char s-char u-char
    short u-short
    int u-int
    long u-long
    long-long u-long-long
    float double long-double
    float-complex double-complex long-double-complex
    bool void
    ;; the followings are non-standard
    builtin-va-list
    float128 float64x float-ibm128
    long-long-double long-long-double-complex))

;; (const unsigned volatile int) -> (u-int (const volatile))
(define (grok-core-type words typedef-dict)
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

  (define (aggregate-tag tag)
    (match tag
      [('ident x) x]
     [#f #f]))

  (define (aggregate-type quals core)
    (define (make-agg-type mark tag members)
      `(,mark ,(aggregate-tag tag) ,(reverse quals) ,members))
    (define (enum-member mem)
      (match-let1 (name init) mem
        (match (fold-c-constant init)
          [#f mem]
          [(val type) `(,name ,val)])))
    (match core
      [('struct tag . members) (make-agg-type '.struct tag members)]
      [('union tag . members)  (make-agg-type '.union tag members)]
      [('enum tag . members)   (make-agg-type '.enum tag
                                              (map enum-member members))]))

  ;; we make quals appear in the order of the original code, just to make
  ;; testing and debugging easier---the caller shouldn't rely on that.
  (define (finish-type quals sign size complex? core)
    (case core
      [(#f int char bool)
       (if complex?
         (bad)
         `(,(integer-type sign size core) ,(reverse quals)))]
      [(float double float128 float64x float-ibm128)
       (if sign
         (bad)
         `(,(float-type size complex? core) ,(reverse quals)))]
      [(void) (if (or sign size complex?)
                (bad)
                `(void ,(reverse quals)))]
      [(builtin-va-list)
       (if (or sign size complex?)
         (bad)
         `(buildin-va-list ,(reverse quals)))]
      [else
       (if (symbol? core)
         ;; typedef name
         (if-let1 def (and typedef-dict (dict-get typedef-dict core #f))
           `(.type ,core ,(reverse quals) ,def)
           (error "Unknown typedef name:" core))
         ;; aggregate
         (if (or sign size complex?)
           (bad)
           (aggregate-type quals core)))]))

  (let loop ([words words]
             [quals '()]
             [sign #f]      ; #f, signed, unsigned
             [size #f]      ; short, long, long-long
             [complex? #f]
             [core #f])     ; int, char, bool, float, double,
                            ;   typedef-name, or struct etc.
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
      [('_Float128 . r) (if core
                          (bad)
                          (loop r quals sign size complex? 'float128))]
      [('_Float64x . r) (if core
                          (bad)
                          (loop r quals sign size complex? 'float64x))]
      [('__ibm128 . r) (if core
                         (bad)
                         (loop r quals sign size complex? 'float-ibm128))]
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
      [('__builtin_va_list . r)
       (if core
         (bad)
         (loop r quals sign size complex? 'builtin-va-list))]
      [('void . r) (loop r quals sign size complex? 'void)]
      [((and (or 'const 'volatile 'restrict) q) . r)
       (loop r (cons q quals) sign size complex? core)]
      [(('ident name) . r) (loop r quals sign size complex? name)]
      [((and ((or 'struct 'union 'enum) . _) aggregate) . r)
       (loop r quals sign size complex? aggregate)]
      [((and ((or 'attribute 'asm) . _) xqual) . r)
       (loop r (cons xqual quals) sign size complex? core)]
      [else (bad)])))

;; Add qualifier to the 'main' type, which would be attached to the variable.
(define (attach-qualifier c-type qual)
  (match c-type
    [('.pointer qs inner) `(.pointer ,(cons qual qs) ,inner)]
    [('.array inner . rest) `(.array ,(attach-qualifier inner qual) ,@rest)]
    [('.function qs . rest) `(.function ,(cons qual qs) ,@rest)]
    [('.struct tag qs . rest) `(.struct ,tag ,(cons qual qs) ,@rest)]
    [('.union tag qs . rest) `(.union ,tag ,(cons qual qs) ,@rest)]
    [('.enum tag qs . rest) `(.enum ,tag ,(cons qual qs) ,@rest)]
    [('.type name qs inner) `(.type ,name ,(cons qual qs) ,inner)]
    [(btype qs) `(,btype ,(cons qual qs))]))

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

;; Returns <basic-type>, .pointer, .array, .function, .struct, .union, .enum
;; typedef is followed.
(define (c-primary-type c-type)
  (match c-type
    [('.type _ _ inner) (c-primary-type inner)]
    [(p . _) p]))

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

;; strip typedefs and returns the actual type
(define (c-actual-type c-type)
  (define (merge-quals new-qs qs)
    (if (null? new-qs)
      qs
      (let1 qs (merge-quals (cdr new-qs) qs)
        (if (memq (car new-qs) qs)
          qs
          (cons (car new-qs) qs)))))
  (match c-type
    [('.type _ quals inner)
     (match-let1 (t qs) (c-actual-type inner)
       `(,t ,(merge-quals quals qs)))]
    [_ c-type]))

;;;
;;;  Folding C constant
;;;

(define-constant *constant-folders*
  `((+ .    ,(^[x y type] (+ x y)))
    (- .    ,(^[x y type] (- x y)))
    (* .    ,(^[x y type] (* x y)))
    (/ .    ,(^[x y type] (if (c-basic-type-integral? type)
                            (quotient x y)
                            (/ x y))))
    (% .    ,(^[x y type] (modulo x y)))
    (& .    ,(^[x y type] (logand x y)))
    (|\|| . ,(^[x y type] (logior x y)))
    (^ .    ,(^[x y type] (logxor x y)))
    (<< .   ,(^[x y type] (ash x y)))
    (>> .   ,(^[x y type] (ash x (- y))))
    ))

;; If EXPR is foldable, returns (<scheme-value> <c-type>).
;; Otherwise returns #f
(define (fold-c-constant expr)
  (match expr
    [('const type lit)
     (cond [(c-basic-type-integral? `(,type ()))
            (let1 n (if (memq type '(char s-char u-char)) ;;TODO: wchar
                      (char->integer (string-ref lit 0))
                      (string->number (string-trim-right lit #[uUlL])))
              (if (exact-integer? n)
                `(,n (,type (const)))
                (error "bad literal integer constant:" lit)))]
           [(c-basic-type-flonum? `(,type ()))
            (let1 n (string->number (string-trim-right lit #[lLfF]))
              (if (real? n)
                `(,n (,type (const)))
                (error "bad floating-point number constant:" lit)))]
           [else #f])]
    [((? (cut assq <> *constant-folders*) op) x1 x2)
     (and-let* ([v1 (fold-c-constant x1)]
                [v2 (fold-c-constant x2)]
                [proc (assq-ref *constant-folders* op)]
                [type (c-wider-type (cadr v1) (cadr v2))])
       `(,(proc (car v1) (car v2) type) ,type))]
    [_ #f]))
