;;;
;;; typeutil.scm - type utilities.
;;;
;;;   Copyright (c) 2020  Shiro Kawai  <shiro@acm.org>
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

;; *EXPERIMENTAL*

;; The classes useful for type validation and other type-related operations.
;; They're abstract types---they won't create an instance---but can be
;; used for of-type? to check if an object satisfies the type constraints.

(define-module gauche.typeutil
  (use util.match)
  (export define-type-constructor of-type?
          <type-constructor-meta> <type-instance-meta>
          <^> </> <?> <Tuple> <List> <Vector>)
  )
(select-module gauche.typeutil)

;; Metaclass: <type-constructor-meta>
;;   Instance classes of this metaclass are used to create an abstract types.
;;   They have object-apply method, and each intance class can be used
;;   as a procedure that takes classes and returns a class.
(define-class <type-constructor-meta> (<class>)
  ())

(define-syntax define-type-constructor
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ name supers slots . opts)
        (let ([meta-name (rxmatch-if (#/^<(.*)>$/ (symbol->string name))
                             [_ trimmed]
                           (string->symbol #"<~|trimmed|-meta>")
                           (string->symbol #"~|name|-meta"))]
              [supers (if (null? supers)
                        (list (r'<type-instance-meta>))
                        supers)])
          (quasirename r
            `(begin
               (define-class ,meta-name (<type-constructor-meta>) ())
               (define-class ,name ,supers ,slots
                 :metaclass ,meta-name
                 ,@opts))))]))))

;; Metaclass: <type-instance-meta>
;;   An abstract type instance, which is a class but won't create instances.
;;   It can be used for of-type? method.
(define-class <type-instance-meta> (<class>)
  ())

(define-method allocate-instance ((z <type-instance-meta>) initargs)
  (error "Abstract type intance cannot instantiate a concrete object:" z))

;; Utilities to create reasonable name
(define (join-class-names classes)
  (string-join (map (^k (if (is-a? k <class>)
                          ($ symbol->string $ class-name k)
                          (x->string k)))
                    classes)
               " " 'prefix))

(define (make-compound-type-name op-name classes)
  ($ string->symbol 
     $ string-append "<" (x->string op-name) (join-class-names classes) ">"))

(define (make-min-max-len-type-name op-name classes min max)
  ($ string->symbol
     $ string-append "<" (x->string op-name) (join-class-names classes)
     (if min
       (if max
         (if (= min max)
           (format " ~d" min)
           (format " ~d..~d" min max))
         (format " ~d.." min))
       (if max
         (format " ..~d" max)
         ""))
     ">"))

;;;
;;; Class: <^>  (maybe we want to name it <λ>)
;;;   Creates a function type.
;;;   The signature can be specified as
;;;
;;;       <argtype1> <argtype2> ... :- <rettype1> <rettype2> ...
;;;
;;;   Argument types and/or return types can be also a single symbol '*,
;;;   indicating arbitrary number of args/values.   That is, any procedure
;;;   can be of type '* :- '*.
;;;
;;;   NB: Currently we don't keep the return value info in procedure, so
;;;   we only allow "wild card" '* as the results.
;;;
;;;   TODO: How to type optional, keyword and rest arguments?
;;;

(define-type-constructor <^> ()
  ((arguments :init-keyword :arguments)
   (results :init-keyword :results)))

(define-method object-apply ((k <^-meta>) . rest)
  (define (scan-args xs as)
    (match xs
      [() (error "Missing ':-' in the procedure type constructor arguments:" 
                 rest)]
      [(':- . xs) (scan-results xs (reverse as) '())]
      [('* ':- . xs)
       (if (null? as)
         (scan-results xs '* '())
         (error "Invalid '* in the procedure type constructor arguments:"
                rest))]
      [else
       (if (is-a? (car xs) <class>) 
         (scan-args (cdr xs) (cons (car xs) as))
         (error "Non-class argument in the procedure type constructor:"
                (car xs)))]))
  (define (scan-results xs args rs)
    (cond [(null? xs) (values args (reverse rs))]
          [(and (null? rs) (eq? (car xs) '*) (null? (cdr xs)))
           (values args '*)]
          [(is-a? (car xs) <class>) 
           (scan-results (cdr xs) args (cons (car xs) as))]
          [else
           (error "Non-class argument in the procedure type constructor:"
                  (car xs))]))

  (receive (args results) (scan-args rest '())
    (unless (eq? results '*)
      (error "Result type must be '*, for we don't support result type checking \
              yet:" results))
    (make <^>
      :name (make-compound-type-name '^ rest)
      :arguments args
      :results results)))

(define-method of-type? (obj (type <^>))
  (if (eq? (~ type'arguments) '*)
    (or (is-a? obj <procedure>)
        (is-a? obj <generic>)
        (is-a? obj <method>)
        (let1 k (class-of obj)
          (let loop ([ms (~ object-apply'methods)])
            (cond [(null? ms) #f]
                  [(subtype? k (car (~ (car ms)'specializers)))]
                  [else (loop (cdr ms))]))))
    (apply applicable? obj (~ type'arguments))))

;;;
;;; Class: </>
;;;   Creates a union type.
;;;

(define-type-constructor </> ()
  ((members :init-keyword :members)))

(define-method object-apply ((k </-meta>) . args)
  (assume (every (cut is-a? <> <class>) args))
  (make </> 
    :name (make-compound-type-name '/ args)
    :members args))

(define-method of-type? (obj (type </>))
  (any (cut of-type? obj <>) (~ type'members)))

;;;
;;; Class: <?>
;;;   Creates a boolean-optional type, that is, <type> or #f.
;;;

(define-type-constructor <?> ()
  ((primary-type :init-keyword :primary-type)))

(define-method object-apply ((k <?-meta>) ptype)
  (assume (is-a? ptype <class>))
  (make <?>
    :name (make-compound-type-name '? `(,ptype))
    :primary-type ptype))

(define-method of-type? (obj (type <?>))
  (or (eqv? obj #f) (of-type? obj (~ type'primary-type))))

;;;
;;; Class: <Tuple>
;;;   Fixed-lenght list, each element having its own type constraints.
;;;

(define-type-constructor <Tuple> ()
  ((elements :init-keyword :elements)))

(define-method object-apply ((k <Tuple-meta>) . args)
  (assume (every (cut is-a? <> <class>) args))
  (make <Tuple>
    :name (make-compound-type-name 'Tuple args)
    :elements args))

(define-method of-type? (obj (type <Tuple>))
  (let loop ((obj obj) (elts (~ type'elements)))
    (if (null? obj)
      (null? elts)
      (and (pair? obj)
           (pair? elts)
           (of-type? (car obj) (car elts))
           (loop (cdr obj) (cdr elts))))))

;;;
;;; Class: <List>
;;;   A list of specified types.
;;;

(define-type-constructor <List> ()
  ((element-type :init-keyword :element-type)
   (min-length :init-keyword :min-length :init-value #f)
   (max-length :init-keyword :max-length :init-value #f)))

(define-method object-apply ((k <List-meta>) etype :optional (min #f) (max #f))
  (assume-type etype <class>)
  (assume-type min (<?> <integer>))
  (assume-type max (<?> <integer>))
  (make <List>
    :name (make-min-max-len-type-name 'List (list etype) min max)
    :element-type etype
    :min-length min
    :max-length max))

(define-method of-type? (obj (type <List>))
  (let ([et (~ type'element-type)]
        [mi (~ type'min-length)]
        [ma (~ type'max-length)])
    (if (not (or mi ma))
      ;; simple case
      (let loop ([obj obj])
        (cond [(null? obj) #t]
              [(not (pair? obj)) #f]
              [(of-type? (car obj) et) (loop (cdr obj))]
              [else #f]))
      ;; general case
      (let loop ([obj obj] [n 0])
        (cond [(null? obj) (or (not mi) (<= mi n))]
              [(and ma (<= ma n)) #f]
              [(not (pair? obj)) #f]
              [(of-type? (car obj) et) (loop (cdr obj) (+ n 1))]
              [else #f])))))

;;;
;;; <Vector> element-type [min-length [max-length]]
;;;

(define-type-constructor <Vector> ()
  ((element-type :init-keyword :element-type)
   (min-length :init-keyword :min-length :init-value #f)
   (max-length :init-keyword :max-length :init-value #f)))

(define-method object-apply ((k <Vector-meta>) etype 
                             :optional (min #f) (max #f))
  (assume-type etype <class>)
  (assume-type min (<?> <integer>))
  (assume-type max (<?> <integer>))
  (make <Vector>
    :name (make-min-max-len-type-name 'Vector (list etype) min max)
    :element-type etype
    :min-length min
    :max-length max))

(define-method of-type? (obj (type <Vector>))
  (and (vector? obj)
       (let ([et (~ type'element-type)]
             [mi (~ type'min-length)]
             [ma (~ type'max-length)]
             [len (vector-length obj)])
         (and (or (not mi) (<= mi len))
              (or (not ma) (<= len ma))
              (let loop ([i 0])
                (cond [(= i len) #t]
                      [(of-type? (vector-ref obj i) et) (loop (+ i 1))]
                      [else #f]))))))
