;;;
;;; C type stuff
;;;
;;;   Copyright (c) 2021-2025  Shiro Kawai  <shiro@acm.org>
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
  (export grok-c-type
          c-primary-type
          c-basic-type-integral?
          c-basic-type-flonum?
          c-basic-type-scalar-numeric?
          c-basic-type-numeric?
          c-type-narrower?
          c-wider-type
          c-actual-type
          c-type->native-type)
  )
(select-module lang.c.type)

;; Avoid statically depending on gauche.native-type.
(autoload gauche.native-type
          make-c-pointer-type make-c-opaque-pointer-type
          make-c-function-type make-c-array-type
          make-c-struct-type make-c-union-type
          make-c-enum-type default-c-enum-base-type)

;; Type representation
;;   We use plain S-expr rather than a bunch of records, for flexibility.
;;
;;
;;   <c-type> : (<basic-type> (<qual> ...))
;;            | (.pointer (<qual> ...) <c-type>)
;;            | (.array <c-type> (<aqual> ...) <size>)
;;            | (.function (<fqual> ...>) <c-type> <args>)
;;            | (.struct <tag> (<qual> ...) <fields>)
;;            | (.union <tag> (<qual> ...) <fields>)
;;            | (.enum <tag> (<qual> ...) <enumerators>)
;;            | (.type <type-name> (<qual> ...) <c-type>)
;;
;;   <basic-type> : one of the symbols in *c-basic-types*
;;
;;   <qual>   : 'volatile | 'const | 'restrict
;;            | ('attribute ...) | ('asm ...)
;;   <fqual>  : 'inline
;;
;;   <aqual> : <qual> | 'static
;;   <size>  : c-constant-expression
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
;;   <fields> : ((<field-name> <c-type> [<size>]) ...)
;;            | #f
;;   <enumerators> : ((<enum-name> <enum-value>) ...)
;;                 | #f
;;   <field-name> : <symbol> | #f
;;   <type-name> : symbol
;;
;;   <enum-name> : symbol
;;   <enum-value> : c-constant-expression
;;                | #f
;;
;;   The (.type ...) form is used for typedef'ed types.
;;
;;   Struct, union and enum types have identity.  The parser creates
;;   one for each tag in a scope (or each anonymous one), and every
;;   occurrence of the type shares the last pair of the form, which holds
;;   <fields> or <enumerators>.  That is, two such types are the same iff
;;   their last pairs are eq?.  Code that rebuilds a type, e.g. to alter
;;   qualifiers, must keep the last pair (see attach-qualifier).
;;   <fields> and <enumerators> are #f while the type is incomplete, and
;;   the parser destructively fills the last pair when it sees the
;;   definition.  So the c-type can be circular (e.g. self-referencing
;;   struct).  Use write-shared to save it.
;;
;;   c-constant-expression is the expression as the parser returns, kept
;;   unevaluated.  Evaluating it needs the knowledge of the constants
;;   defined so far (including enumerators), so it is up to the grokker.


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
;;               | (struct <tag> <fields>)
;;               | (union <tag> <fields>)
;;               | (enum <tag> <enumerators>)
;;     where the last pair of each form is shared, as described above.
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
       `(.array ,(rec rest) ,quals ,size)]
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

  ;; NB: We share the last pair of CORE; see the comment at the top.
  (define (aggregate-type quals core)
    (match core
      [('struct tag . cell) `(.struct ,tag ,(reverse quals) . ,cell)]
      [('union tag . cell)  `(.union ,tag ,(reverse quals) . ,cell)]
      [('enum tag . cell)   `(.enum ,tag ,(reverse quals) . ,cell)]))

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
;; NB: For struct/union/enum, the last pair must be shared.
(define (attach-qualifier c-type qual)
  (define (merge-quals new-qs qs)
    (if (null? new-qs)
      qs
      (let1 qs (merge-quals (cdr new-qs) qs)
        (if (memq (car new-qs) qs)
          qs
          (cons (car new-qs) qs)))))
  (match c-type
    [('.pointer qs inner) `(.pointer ,(merge-quals qual qs) ,inner)]
    [('.array inner . rest) `(.array ,(attach-qualifier inner qual) ,@rest)]
    [('.function qs . rest) `(.function ,(merge-quals qual qs) ,@rest)]
    [('.struct tag qs . cell) `(.struct ,tag ,(merge-quals qual qs) . ,cell)]
    [('.union tag qs . cell) `(.union ,tag ,(merge-quals qual qs) . ,cell)]
    [('.enum tag qs . cell) `(.enum ,tag ,(merge-quals qual qs) . ,cell)]
    [('.type name qs inner) `(.type ,name ,(merge-quals qual qs) ,inner)]
    [(btype qs) `(,btype ,(merge-quals qual qs))]))

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
  (match c-type
    [('.type _ quals inner)
     (attach-qualifier (c-actual-type inner) quals)]
    [_ c-type]))

;;;
;;; Conversion to native type
;;;

;; API
;;   Returns a <native-type> corresponding to C-TYPE, or #f if it can't
;;   be represented as a native type.
;;
;;   During conversion, we may need to evaluate constant expression
;;   (e.g. array dimensions or enumerator values).  Evaluation needs
;;   global knowledge of the program, so we delegate it to caller-provided
;;   EVAL-CONST procedure---it takes parsed C expression form and must
;;   return either a Scheme number, or #f when the expression doesn't
;;   have constant value.  EVAL-CONST may throw an error; see lang.c.grokker.
;;
;;   Qualifiers are ignored.  Bit fields and anonymous members of
;;   struct/union aren't supported yet.  Incomplete struct/union can only
;;   be a pointee, and represented as an opaque pointer.  So is a pointer
;;   to a struct/union being converted (e.g. self-referencing struct).
(define (c-type->native-type c-type :optional (eval-const (^_ #f)))
  (define converting '())           ;last pairs of struct/union being converted
  (define (const-int expr)
    (let1 v (eval-const expr)
      (and (exact-integer? v) v)))
  (define (field f)
    (match f
      [((? symbol? name) type) (and-let1 t (conv type) (list name t))]
      [_ #f]))                          ;anonymous or bit field
  (define (aggregate make tag cell)
    (and-let1 fields (car cell)         ;#f if incomplete
      (let1 saved converting
        (push! converting cell)
        (let1 fs (map field fields)
          (set! converting saved)
          (and (every values fs) (make tag fs))))))
  (define (pointer pointee)
    (match (c-actual-type pointee)
      [('void _) <void*>]
      [((and (or '.struct '.union) kind) tag _ . cell)
       (if (or (not (car cell)) (memq cell converting))
         (make-c-opaque-pointer-type
          (string->symbol (format "~a ~a"
                                  (if (eq? kind '.struct) 'struct 'union)
                                  (or tag 'anonymous))))
         (and-let1 t (conv pointee) (make-c-pointer-type t)))]
      [_ (and-let1 t (conv pointee) (make-c-pointer-type t))]))
  ;; int x[3][2] is (.array (.array int 2) 3); we flatten it to
  ;; an array of int with dimensions (3 2).  Only the first dimension
  ;; can be unknown.
  (define (array type)
    (let loop ([type type] [dims '()] [first? #t])
      (match (c-actual-type type)
        [('.array elt _ dim)
         (and-let1 d (cond [dim (const-int dim)]
                           [first? '*]
                           [else #f])
           (loop elt (cons d dims) #f))]
        [_ (and-let1 elt (conv type)
             (make-c-array-type elt (reverse dims)))])))
  (define (function ret args)
    (and-let* ([r (conv ret)]
               [as (match args
                     ['unknown-args '(...)]
                     [_ (map (^a (match a
                                   ['... '...]
                                   [(_ t) (conv t)]))
                             args)])]
               [ (every values as) ])
      (make-c-function-type r as)))
  (define (enum tag members)
    (if members
      (let1 es (map (^m (match m
                          [(name #f) name]
                          [(name expr) (and-let1 v (const-int expr)
                                         (list name v))]))
                    members)
        (and (every values es) (make-c-enum-type tag #f es)))
      ;; Incomplete enum.  We use the provisional base type.
      (make-c-enum-type tag (default-c-enum-base-type) '())))
  (define (conv type)
    (match (c-actual-type type)
      [('.pointer _ pointee) (pointer pointee)]
      [('.array . _) (array type)]
      [('.function _ ret args) (function ret args)]
      [('.struct tag _ . cell) (aggregate make-c-struct-type tag cell)]
      [('.union tag _ . cell) (aggregate make-c-union-type tag cell)]
      [('.enum tag _ members) (enum tag members)]
      [('char _) <c-char>]
      [('s-char _) <int8>]
      [('u-char _) <uint8>]
      [('short _) <short>]
      [('u-short _) <ushort>]
      [('int _) <int>]
      [('u-int _) <uint>]
      [('long _) <long>]
      [('u-long _) <ulong>]
      [('long-long _) <longlong>]
      [('u-long-long _) <ulonglong>]
      [('bool _) <c-bool>]
      [('float _) <float>]
      [('double _) <double>]
      [('long-double _) <longdouble>]
      [('float-complex _) <float-complex>]
      [('double-complex _) <double-complex>]
      [('long-double-complex _) <longdouble-complex>]
      [('void _) <void>]
      [_ #f]))
  (conv c-type))
