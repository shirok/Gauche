;; Tests for typeutil

(use gauche.test)
(test-start "typeutil")

(use gauche.typeutil)
(test-module 'gauche.typeutil)

(use gauche.record)                     ;for internal define-record-type
(use gauche.threads)

(test-section "type constuctor memoization")

;; This tests the constructed types from the same arguments gets eq?,
;; because of the memoization.

(define-syntax t-identity
  (syntax-rules ()
    [(_ expect a b)
     (test* (list 'a 'b) expect (eq? a b))]))

(t-identity #t (<?> <integer>) (<?> <integer>))
(t-identity #t (</> <integer> <string>) (</> <integer> <string>))
(t-identity #f (<?> <integer>) (<?> <int>))
(t-identity #t (</> <uint8> <uint16>) (</> <uint8> <uint16>))
(t-identity #f (</> <uint8> <uint16>) (</> <uint16> <uint8>))
(t-identity #t (<Assortment> 'a 'b) (<Assortment> 'a 'b))

(test-section "subtype?")

(define-syntax t-subtype
  (syntax-rules ()
    [(_ sub sup expect)
     (test* (list 'subtype? 'sub 'sup) expect (subtype? sub sup))]))

(t-subtype <fixnum> <fixnum> #t)
(t-subtype <fixnum> <integer> #t)
(t-subtype <fixnum> <real> #t)
(t-subtype <fixnum> <number> #t)
(t-subtype <fixnum> <top> #t)
(t-subtype <bottom>   <fixnum> #t)
(t-subtype <fixnum> <boolean> #f)
(t-subtype <short>  <integer> #t)
(t-subtype <ushort> <integer> #t)
(t-subtype <int>    <integer> #t)
(t-subtype <uint>   <integer> #t)
(t-subtype <long>   <integer> #t)
(t-subtype <ulong>  <integer> #t)
(t-subtype <int8>   <integer> #t)
(t-subtype <uint8>  <integer> #t)
(t-subtype <int16>  <integer> #t)
(t-subtype <uint16> <integer> #t)
(t-subtype <int32>  <integer> #t)
(t-subtype <uint32> <integer> #t)
(t-subtype <int64>  <integer> #t)
(t-subtype <uint64> <integer> #t)
(t-subtype <float>  <integer> #f)
(t-subtype <float>  <real>    #t)
(t-subtype <float>  <number>  #t)
(t-subtype <double> <integer> #f)
(t-subtype <double> <real>    #t)
(t-subtype <double> <number>  #t)
(t-subtype <fixnum> <float> #f)
(t-subtype <number>   <fixnum> #f)
(t-subtype <c-string> <c-string> #t)
(t-subtype <c-string> <string> #t)
(t-subtype <c-string> <boolean> #f)


(t-subtype <integer> (</> <integer> <string>) #t)
(t-subtype <integer> (</>) #f)
(t-subtype <integer> (</> <char> <string>) #f)
(t-subtype <integer> (</> <number> <string>) #t)
(t-subtype (</> <integer> <string>) <top> #t)
(t-subtype (</> <integer> <string>) <integer> #f)
(t-subtype (</> <integer> <real>)   <complex> #t)
(t-subtype (</> <integer> <string>) (</> <string> <integer>) #t)
(t-subtype (</> <integer> <string>) (</> <string> <real>) #t)
(t-subtype (</> <integer> <string>) (</> <string> <char> <integer>) #t)
(t-subtype (</> <integer> <string>) (</> <char> <integer>) #f)
(t-subtype (</> <integer> <string>) (<?> (</> <number> <string>)) #t)
(t-subtype <int> (</> <int> <char>) #t)
(t-subtype <char> (</> <int> <char>) #t)
(t-subtype <int> (</> <integer> <char>) #t)
(t-subtype <integer> (</> <int> <char>) #f)

(t-subtype <integer> (<?> <integer>) #t)
(t-subtype <boolean> (<?> <integer>) #f)
(t-subtype <integer> (<?> <real>) #t)
(t-subtype <real>    (<?> <integer>) #f)
(t-subtype (<?> <integer>) (<?> <real>) #t)
(t-subtype (<?> <integer>) <integer> #f)
(t-subtype (<?> <boolean>) <boolean> #t)
(t-subtype (<?> <integer>) (</> (<?> <number>) (<?> <string>)) #t)
(t-subtype (<?> <char>) (</> <boolean> <char>) #t)
(t-subtype (<?> <char>) (</> <integer> <char>) #f)

(t-subtype (<Tuple> <integer> <string>) <list> #t)
(t-subtype (<Tuple> <integer> <string>) (<Tuple> <integer> <string>) #t)
(t-subtype (<Tuple> <integer> <string>) (<Tuple> <integer> <string> <char>) #f)
(t-subtype (<Tuple> <integer> <string>) (<Tuple> <real> <string>) #t)
(t-subtype (<Tuple> <integer> <integer>) (<List> <integer>) #t)
(t-subtype (<Tuple> <integer> <integer>) (<List> <integer> 2) #t)
(t-subtype (<Tuple> <integer> <integer>) (<List> <integer> 0 2) #t)
(t-subtype (<Tuple> <integer> <integer>) (<List> <integer> 0 1) #f)
(t-subtype (<Tuple> <integer> <string>) (<List> <integer>) #f)
(t-subtype (<Tuple> <char> <string> *) <list> #t)
(t-subtype (<Tuple> <char> <string> *) (<Tuple> <char>) #f)
(t-subtype (<Tuple> <char> <string> *) (<Tuple> <char> <string>) #t)
(t-subtype (<Tuple> <char> <string> *) (<Tuple> <char> <string> <char>) #t)
(t-subtype (<Tuple> *) <list> #t)
(t-subtype (<Tuple> *) (<Tuple> <integer> *) #t)
(t-subtype (<Tuple> <integer>) (<Tuple> <integer> *) #t)
(t-subtype (<Tuple> <integer>) (<Tuple> <integer> <integer> *) #f)
(t-subtype (<Tuple> <integer> *) (<Tuple> <integer> <integer> *) #t)

(t-subtype (<List> <integer>) <list> #t)
(t-subtype (<List> <integer>) (<List> <number>) #t)
(t-subtype (<List> <number>) (<List> <integer>) #f)
(t-subtype (<List> <integer> 2 3) (<List> <integer> 0 4) #t)
(t-subtype (<List> <integer> 0 3) (<List> <integer> 2 3) #f)
(t-subtype (<List> <integer> 2 4) (<List> <integer> 2 3) #f)
(t-subtype (<List> <integer> #f 3) (<List> <integer> 0 4) #t)
(t-subtype (<List> <integer> #f 3) (<List> <integer> 0) #t)
(t-subtype (<List> <integer> 0) (<List> <integer> 0 3) #f)
(t-subtype (<List> <integer>) (<?> (<List> <number>)) #t)
(t-subtype (<List> <integer>) (</> (<List> <string>) (<List> <number>)) #t)

(t-subtype (<Vector> <integer>) <vector> #t)
(t-subtype (<Vector> <integer>) (<Vector> <number>) #t)
(t-subtype (<Vector> <number>) (<Vector> <integer>) #f)
(t-subtype (<Vector> <integer> 2 3) (<Vector> <integer> 0 4) #t)
(t-subtype (<Vector> <integer> 0 3) (<Vector> <integer> 2 3) #f)
(t-subtype (<Vector> <integer> 2 4) (<Vector> <integer> 2 3) #f)
(t-subtype (<Vector> <integer> #f 3) (<Vector> <integer> 0 4) #t)
(t-subtype (<Vector> <integer> #f 3) (<Vector> <integer> 0) #t)
(t-subtype (<Vector> <integer> 0) (<Vector> <integer> 0 3) #f)
(t-subtype (<Vector> <integer>) (<?> (<Vector> <number>)) #t)
(t-subtype (<Vector> <integer>) (</> (<Vector> <string>) (<Vector> <number>)) #t)

(t-subtype (<Assortment> 'a) (<Assortment> 'a 'b 'c) #t)
(t-subtype (<Assortment> 1 2 3) (<Assortment> 1 2 3) #t)
(t-subtype (<Assortment> 1 2 3) (<Assortment> 1 2) #f)
(t-subtype (<Assortment> 1 3) (<Assortment> 1 2) #f)
(t-subtype (<Assortment> 1 3) <integer> #t)
(t-subtype (<Assortment> 1 'a) (</> <integer> <symbol>) #t)
(t-subtype (<Assortment> 1 'a) (</> <integer> <string>) #f)
(t-subtype (</> (<Assortment> #f) <char>) (<?> <char>) #t)
(t-subtype (<?> <char>) (</> (<Assortment> #f) <char>) #t)

(test-section "built-in type constructors")

(define (validation-test type alist)
  (dolist [p alist]
    (test* (format "~a ~s" (class-name type) (car p))
           (cdr p)
           (of-type? (car p) type))))

(validation-test (</> <string> <integer>)
                 '(("abc" . #t)
                   (123 . #t)
                   (abc . #f)
                   (#f . #f)
                   (#t . #f)
                   (("abc") . #f)))

(validation-test (<Tuple> <char> <integer> <symbol>)
                 '(((#\a 1 a) . #t)
                   ((#\a 1) . #f)
                   (() . #f)
                   ((1 #\a b) . #f)
                   ((#\a 1 b x) . #f)))

(validation-test (<?> <integer>)
                 '((3 . #t)
                   (#f . #t)
                   (#t . #f)
                   (3.5 . #f)))

(validation-test (<Tuple> (<?> <char>) (<?> <string>))
                 '((3 . #f)
                   ((#\a "a") . #t)
                   ((#f "a") . #t)
                   ((#\a . "a") . #f)
                   ((#\a "a" . z) . #f)
                   ((#\a #f) . #t)
                   ((#f #f) . #t)
                   ((#f) . #f)
                   ((#\a) . #f)
                   (("a") . #f)))

(validation-test (<Tuple> <integer> <real> *)
                 '(((2 2.3) . #t)
                   ((2 2.3 3) . #t)
                   ((2.2 3) . #f)
                   ((2 2.3 . 3) . #f)))

(validation-test (<^> * -> *)
                 `((,car . #t)
                   (,cons . #t)
                   (,list . #t)
                   (1 . #f)
                   ;;(#/abc/ . #t) ; applicable objects are not supported yet
                   ))

(validation-test (<^> <top> -> *)
                 `((,car . #t)
                   (,cons . #f)
                   (,list . #f)
                   (,cons* . #t)
                   (,current-input-port . #f)
                   (,(lambda () #f) . #f)))

(validation-test (<^> -> *)
                 `((,(lambda () #f) . #t)
                   (,car . #f)
                   (,list . #t)))

(validation-test (<^> <top> <top> -> *)
                 `((,cons . #t)
                   (,car . #f)))

(validation-test (<^> <top> <top> -> *)
                 `((,(case-lambda ((a) 1) ((a b) 2)) . #t)))

(validation-test (</> (<^> <top> -> *) (<^> <top> <top> -> *))
                 `((,(case-lambda ((a) 1) ((a b) 2)) . #t)))

(validation-test (<List> <integer>)
                 '((() . #t)
                   ((1) . #t)
                   ((1 2 3 4 5 6 7) . #t)
                   ((1 . 2) . #f)
                   ((1 2 a 3 4) . #f)
                   (1 . #f)))

(validation-test (<List> <integer> 3)
                 '((() . #f)
                   ((1) . #f)
                   ((1 2 3) . #t)
                   ((1 2 3 4) . #t)
                   ((1 2 3 4 5 6 7) . #t)
                   ((1 . 2) . #f)
                   ((1 2 a 3 4) . #f)
                   (1 . #f)))

(validation-test (<List> <integer> #f 3)
                 '((() . #t)
                   ((1) . #t)
                   ((1 2 3) . #t)
                   ((1 2 3 4) . #f)
                   ((1 2 3 4 5 6 7) . #f)
                   ((1 . 2) . #f)
                   ((1 2 a 3 4) . #f)
                   (1 . #f)))

(validation-test (<List> <integer> 3 3)
                 '((() . #f)
                   ((1) . #f)
                   ((1 2 3) . #t)
                   ((1 2 3 4) . #f)
                   ((1 2 3 4 5 6 7) . #f)
                   ((1 . 2) . #f)
                   ((1 2 a 3 4) . #f)
                   (1 . #f)))

(validation-test (<Vector> <integer>)
                 '((#() . #t)
                   (#(1) . #t)
                   (#(1 2 3 4 5 6 7) . #t)
                   (#(1 2 a 3 4) . #f)
                   ((1) . #f)))

(validation-test (<Vector> <integer> 3)
                 '((#() . #f)
                   (#(1) . #f)
                   (#(1 2 3) . #t)
                   (#(1 2 3 4) . #t)
                   (#(1 2 3 4 5 6 7) . #t)
                   (#(1 2 a 3 4) . #f)
                   ((1) . #f)))

(validation-test (<Vector> <integer> #f 3)
                 '((#() . #t)
                   (#(1) . #t)
                   (#(1 2 3) . #t)
                   (#(1 2 3 4) . #f)
                   (#(1 2 3 4 5 6 7) . #f)
                   (#(1 2 a 3 4) . #f)
                   (1 . #f)))

(validation-test (<Vector> <integer> 3 3)
                 '((#() . #f)
                   (#(1) . #f)
                   (#(1 2 3) . #t)
                   (#(1 2 3 4) . #f)
                   (#(1 2 3 4 5 6 7) . #f)
                   (#(1 2 a 3 4) . #f)
                   (1 . #f)))

(test-section "procedure types")

(define-syntax proctype-test
  (syntax-rules ()
    [(_ proc supposed-type)
     (test* '(procedure-type proc) supposed-type
            (procedure-type proc))]))


(proctype-test cons (<^> <top> <top> -> <pair>))
(proctype-test car  (<^> <pair> -> <top>))
(proctype-test list (<^> * -> <list>))
(proctype-test set-cdr! (<^> <pair> <top> -> <void>))

;; This tests gf's type is recomputed after method addition
(define-method a-gf ((x <number>)) x)

(proctype-test a-gf (</> (<^> <number> -> *)))

(define-method a-gf ((x <string>)) x)

(proctype-test a-gf (</> (<^> <string> -> *) (<^> <number> -> *)))

(test-section "typecase")

(define (t-typecase obj)
  (typecase obj
    [<integer> 'int]
    [<string> 'str]
    [(<?> <symbol>) 'maybe-symbol]
    [(<List> (</> <string> <symbol>)) 'string-or-symbol-list]
    [<vector> => (^v `(vector ,v))]
    [else 'other]))

(test* "typecase" '(int str maybe-symbol maybe-symbol
                        string-or-symbol-list (vector #()) other)
       (map t-typecase
            '(10 "abc" foo #f (a "b" c) #() (d 3 f))))

(define (t-etypecase obj)
  (etypecase obj
    [<string> 'str]
    [<symbol> 'sym]))

(test* "etypecase" 'str (t-etypecase "abc"))
(test* "etypecase" 'sym (t-etypecase 'abc))
(test* "etypecase"
       (test-error <error> #/expecting one of types in \(<string> <symbol>\)/)
       (t-etypecase 1))

(test-section "deferred type binding")

;; A type constructor expression is evaluated at the compile time, so the
;; value of the type used in it must be known to the compiler.  When the
;; type definition is grouped with its use in the same toplevel form, the
;; definition isn't executed by the time the use is compiled; the compiler
;; leaves a placeholder binding, and the actual type is looked up lazily.

(begin
  (define-class <deferred-a> () ())

  (define (deferred-a? x) (of-type? x (<?> <deferred-a>)))

  (define-type <maybe-deferred-a> (<?> <deferred-a>))

  (define (deferred-a-2? x) (of-type? x <maybe-deferred-a>)))

(test* "deferred class in a type ctor expr" '(#t #t #f)
       (list (deferred-a? #f)
             (deferred-a? (make <deferred-a>))
             (deferred-a? 1)))

(test* "deferred class via define-type" '(#t #t #f)
       (list (deferred-a-2? #f)
             (deferred-a-2? (make <deferred-a>))
             (deferred-a-2? 1)))

(test* "name of a type built from a deferred class" '|<? <deferred-a>>|
       (~ <maybe-deferred-a> 'name))

(t-identity #t (<?> <deferred-a>) <maybe-deferred-a>)

;; A type alias to a non-class type.
(begin
  (define-type <deferred-b> <int8>)

  (define (deferred-b? x) (of-type? x (<?> <deferred-b>))))

(test* "deferred alias to a native type" '(#t #t #f #f)
       (list (deferred-b? #f)
             (deferred-b? 100)
             (deferred-b? 1000)
             (deferred-b? "x")))

;; A deferred type needn't be a class---the reference is resolved to whatever
;; type the binding turns out to hold.
(begin
  (define-type <deferred-c1> (<?> <string>))

  (define (deferred-c1? x) (of-type? x (</> <deferred-c1> <integer>)))

  (define (deferred-c1-sub? x) (subtype? x (</> <deferred-c1> <integer>))))

(test* "deferred reference to a descriptive type" '(#t #t #t #f)
       (list (deferred-c1? #f)
             (deferred-c1? "abc")
             (deferred-c1? 42)
             (deferred-c1? 'sym)))

(test* "subtype? through a deferred reference" '(#t #t #f)
       (list (deferred-c1-sub? <string>)
             (deferred-c1-sub? <integer>)
             (deferred-c1-sub? <symbol>)))

;; Class redefinition must still be seen through the deferred proxy type.
(begin
  (define-class <deferred-c> () ())

  (define-type <maybe-deferred-c> (<?> <deferred-c>)))

(define deferred-c-instance (make <deferred-c>))

(define-class <deferred-c> () ((x)))

(test* "redefined class through a deferred proxy type" '(#t #t)
       (list (of-type? deferred-c-instance <maybe-deferred-c>)
             (of-type? (make <deferred-c>) <maybe-deferred-c>)))

;; An unbound name is still rejected at the compile time---we only leave
;; a placeholder for names we've seen a type definition for.
(test* "unknown type in a type ctor expr"
       (test-error <error> #/non-inlinable global variable/)
       (eval '(of-type? 1 (<?> <no-such-type-at-all>)) (current-module)))

(test-section "local proxy type")

(define %make-local-proxy-type
  (with-module gauche.internal %make-local-proxy-type))
(define proxy-type-id (with-module gauche.internal proxy-type-id))
(define proxy-type-ref (with-module gauche.internal proxy-type-ref))
(define construct-type (with-module gauche.internal construct-type))

(define-class <local-a> () ())
(define local-a-proxy (%make-local-proxy-type <local-a>))

(test* "local proxy type basics" '(#t #f #t)
       (list (type? local-a-proxy)
             (proxy-type-id local-a-proxy) ; no identifier to refer to
             (eq? (proxy-type-ref local-a-proxy) <local-a>)))

(test* "local proxy type must stand for a type"
       (test-error <error> #/must stand for a type/)
       (%make-local-proxy-type 3))

(test* "printing a local proxy type" "#<local #<class <local-a>>>"
       (write-to-string local-a-proxy))

(test* "of-type? through a local proxy type" '(#t #f)
       (list (of-type? (make <local-a>) local-a-proxy)
             (of-type? 3 local-a-proxy)))

(test* "subtype? through a local proxy type" '(#t #t #f)
       (list (subtype? local-a-proxy <local-a>)
             (subtype? <local-a> local-a-proxy)
             (subtype? <string> local-a-proxy)))

(test* "type ctor with a local proxy type" '(#t #t #f)
       (let1 t (construct-type <?> (list local-a-proxy))
         (list (of-type? #f t) (of-type? (make <local-a>) t) (of-type? 3 t))))

(test* "compound type name with a local proxy type"
       '("<? <local-a>>" "<List <? <local-a>>>")
       (let1 t (construct-type <?> (list local-a-proxy))
         (list (x->string (~ t'name))
               (x->string (~ (construct-type <List> (list t))'name)))))

;; A fresh local proxy type is created per activation of the scope binding
;; the type, so two of them are never the same, even for the same type.
(test* "local proxy types compare by identity" '(#t #f)
       (list (equal? local-a-proxy local-a-proxy)
             (equal? local-a-proxy (%make-local-proxy-type <local-a>))))

(test* "type built from a local proxy type isn't memoized" '(#f #f #t)
       (let1 t (construct-type <?> (list local-a-proxy))
         (list (eq? t (construct-type <?> (list local-a-proxy)))
               (eq? (construct-type <List> (list t))
                    (construct-type <List> (list t)))
               (eq? (construct-type <?> (list <string>))
                    (construct-type <?> (list <string>))))))

(define (make-local-type)
  (let1 c (make <class> :name '<local-b> :supers (list <top>) :slots '())
    (cons c (%make-local-proxy-type c))))

(test* "local types from different activations are independent"
       '(#f #t #t #f)
       (let* ([a (make-local-type)]
              [b (make-local-type)]
              [ta (construct-type <?> (list (cdr a)))]
              [tb (construct-type <?> (list (cdr b)))])
         (list (equal? (cdr a) (cdr b))
               (of-type? (make (car a)) ta)
               (of-type? (make (car b)) tb)
               (of-type? (make (car a)) tb))))

;; Repeating the same construction must keep working (and must not accumulate
;; anything globally---see the memoization test above).
(test* "repeated construction from a local proxy type" #t
       (every (^_ (let1 t (construct-type <?> (list local-a-proxy))
                    (and (of-type? (make <local-a>) t)
                         (of-type? #f t)
                         (not (of-type? 3 t)))))
              (iota 100)))

(test-section "internal type definition")

;; An internal define-type whose right-hand side is a type we can compute at
;; the compile time binds the name as a compile-time constant, so it can be
;; used where a type is required.

(define (i-ctor-arg x) (define-type <myint> <int>) (of-type? x (<?> <myint>)))
(test* "internal define-type in a type ctor arg" '(#t #t #f)
       (list (i-ctor-arg 3) (i-ctor-arg #f) (i-ctor-arg "a")))

(define (i-native x) (define-type <t> <int>) (of-type? x <t>))
(test* "internal define-type, native type rhs" '(#t #f)
       (list (i-native 3) (i-native "a")))

(define (i-ctor-rhs x) (define-type <t> (</> <integer> <string>)) (of-type? x <t>))
(test* "internal define-type, type ctor rhs" '(#t #t #f)
       (list (i-ctor-rhs 3) (i-ctor-rhs "a") (i-ctor-rhs 'b)))

(define (i-chained x)
  (define-type <a> <int>)
  (define-type <b> (<?> <a>))            ; refers to the previous one
  (list (of-type? x <b>) (of-type? x <a>)))
(test* "internal define-type, chained" '((#t #t) (#t #f) (#f #f))
       (list (i-chained 3) (i-chained #f) (i-chained "a")))

(define (i-annotation x)
  (define-type <t> <int>)
  (define (g y :: <t>) (* y 2))
  (g x))
(test* "internal define-type in a :: annotation" 6 (i-annotation 3))
(test* "internal define-type in a :: annotation (violation)" (test-error)
       (i-annotation "a"))

(define (i-shadow x)
  (define-type <t> <int>)
  (list (of-type? x <t>)
        (let () (define-type <t> <string>) (of-type? x <t>))))
(test* "internal define-type is lexically scoped" '((#t #f) (#f #t))
       (list (i-shadow 3) (i-shadow "a")))

(define-syntax def-int-type
  (syntax-rules () [(_ n) (define-type n <int>)]))
(define (i-via-macro x) (def-int-type <mine>) (of-type? x (<?> <mine>)))
(test* "internal define-type introduced by a macro" '(#t #t #f)
       (list (i-via-macro 3) (i-via-macro #f) (i-via-macro "a")))

;; A generative right-hand side can't be computed at the compile time.  The
;; value stays in an ordinary internal binding, and a type expression
;; mentioning it builds the type at runtime, out of the local proxy type the
;; activation holds.  (We build the class directly here, to keep these tests
;; from depending on gauche.record; there's one with a record below.)
(define (i-generative)
  (define-type <gen> (make <class> :name '<gen> :supers (list <top>) :slots '()))
  (list (is-a? <gen> <class>) (class-name <gen>) (of-type? (make <gen>) <gen>)))
(test* "generative internal define-type still works as a value"
       '(#t <gen> #t)
       (i-generative))

(define (i-gen-ctor x)
  (define-type <gen> (make <class> :name '<gen> :supers (list <top>) :slots '()))
  (list (of-type? x (<?> <gen>))
        (of-type? (make <gen>) (<?> <gen>))))
(test* "generative internal type in a type ctor expression"
       '((#t #t) (#f #t) (#f #t))
       (list (i-gen-ctor #f) (i-gen-ctor 3) (i-gen-ctor "a")))

;; The same local type in several type expressions, including nested ones.
(define (i-gen-several x)
  (define-type <gen> (make <class> :name '<gen> :supers (list <top>) :slots '()))
  (let1 obj (make <gen>)
    (list (of-type? x (<?> <gen>))
          (of-type? x (</> <string> <gen>))
          (of-type? (list obj) (<List> (<?> <gen>)))
          (of-type? obj (</> <string> (<?> <gen>))))))
(test* "local type in several type expressions"
       '((#t #f #t #t) (#f #t #t #t))
       (list (i-gen-several #f) (i-gen-several "a")))

;; The property a compile-time constant can't have: each activation builds
;; its own type, and an instance of one doesn't satisfy the other's.
(define (i-gen-activation)
  (define-type <gen> (make <class> :name '<gen> :supers (list <top>) :slots '()))
  (values (make <gen>) (^x (of-type? x (<?> <gen>)))))
(test* "generative internal type is per-activation" '(#t #f #f #t)
       (receive (obj1 pred1) (i-gen-activation)
         (receive (obj2 pred2) (i-gen-activation)
           (list (pred1 obj1) (pred1 obj2) (pred2 obj1) (pred2 obj2)))))

;; Re-entering the scope, and evaluating the same type expression over and
;; over within one activation, must keep working.
(define (i-gen-loop n)
  (define-type <gen> (make <class> :name '<gen> :supers (list <top>) :slots '()))
  (let1 obj (make <gen>)
    (let loop ([i 0])
      (cond [(= i n) #t]
            [(and (of-type? obj (<?> <gen>))
                  (not (of-type? 3 (<?> <gen>))))
             (loop (+ i 1))]
            [else #f]))))
(test* "generative internal type in a loop" '(#t #t)
       (list (i-gen-loop 100) (i-gen-loop 100)))

;; Two threads running the same closure, each with its own activation.
(define (i-gen-thread)
  (define-type <gen> (make <class> :name '<gen> :supers (list <top>) :slots '()))
  (cons (make <gen>) (^x (of-type? x (<?> <gen>)))))
(test* "generative internal type in threads" '(#t #f #f #t)
       (let* ([run (^[] (thread-join! (thread-start! (make-thread i-gen-thread))))]
              [a (run)]
              [b (run)])
         (list ((cdr a) (car a)) ((cdr a) (car b))
               ((cdr b) (car a)) ((cdr b) (car b)))))

;; The shape this is all for: an internal define-record-type, used both as a
;; value (constructor and accessors) and in type expressions.
(define (i-record x)
  (define-record-type point #t #t px py)
  (let1 p (make-point 1 2)
    (list (point-px p)
          (of-type? p point)
          (of-type? x (<?> point))
          (of-type? p (<?> point)))))
(test* "internal define-record-type in a type expression"
       '((1 #t #t #t) (1 #t #f #t))
       (list (i-record #f) (i-record 3)))

;; A local proxy type is built on entry to the scope, so the value of a
;; generative define-type must be a type even if no type expression uses it.
(test* "generative internal define-type with a non-type value"
       (test-error <error> #/must stand for a type/)
       (eval '(let () (define-type <t> (+ 40 2)) <t>) (current-module)))

;; Out of scope for now: a `::' annotation is resolved at the compile time,
;; so it can't refer to a type that only exists at runtime.
(test* "generative internal type in a :: annotation is unsupported"
       (test-error <error> #/Invalid type expression/)
       (eval '(define (h)
                (define-type <gen>
                  (make <class> :name '<gen> :supers (list <top>) :slots '()))
                (define (k p :: <gen>) p)
                (k (make <gen>)))
             (current-module)))

(test* "set! on an internal type binding"
       (test-error <error> #/cannot assign to a type binding/)
       (eval '(define (h) (define-type <t> <int>) (set! <t> 3))
             (current-module)))

(test* "set! on a generative internal type binding"
       (test-error <error> #/cannot assign to a type binding/)
       (eval '(define (h)
                (define-type <gen>
                  (make <class> :name '<gen> :supers (list <top>) :slots '()))
                (set! <gen> 3))
             (current-module)))

(test* "malformed internal define-type" (test-error)
       (eval '(define (h) (define-type <t>) 1) (current-module)))

(test* "internal define-type clashing with an internal define" (test-error)
       (eval '(define (h) (define-type <t> <int>) (define <t> 3) 1)
             (current-module)))

(test* "generative internal define-type clashing with an internal define"
       (test-error)
       (eval '(define (h)
                (define-type <t>
                  (make <class> :name '<t> :supers (list <top>) :slots '()))
                (define <t> 3)
                1)
             (current-module)))

(test-end)
