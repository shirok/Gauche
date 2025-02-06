;; Tests for typeutil

(use gauche.test)
(test-start "typeutil")

(use gauche.typeutil)
(test-module 'gauche.typeutil)

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
(t-subtype <bottom> <fixnum> #t)
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
(t-subtype <number> <fixnum> #f)
(t-subtype <const-cstring>  <const-cstring> #t)
(t-subtype <const-cstring>  <string> #t)
(t-subtype <const-cstring>  <boolean> #f)


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
    [else 'other]))

(test* "typecase" '(int str maybe-symbol maybe-symbol
                        string-or-symbol-list other)
       (map t-typecase
            '(10 "abc" foo #f (a "b" c) (d 3 f))))

(define (t-etypecase obj)
  (etypecase obj
    [<string> 'str]
    [<symbol> 'sym]))

(test* "etypecase" 'str (t-etypecase "abc"))
(test* "etypecase" 'sym (t-etypecase 'abc))
(test* "etypecase"
       (test-error <error> #/expecting one of types in \(<string> <symbol>\)/)
       (t-etypecase 1))

(test-end)
