;;
;; test for procedures
;;

(use gauche.test)
(test-start "procedures")

;;-------------------------------------------------------------------
(test-section "C-stack boundary crossing")

(test* "apply-rec0" 'a
       ((with-module gauche.internal %apply-rec0) (lambda () 'a)))
(test* "apply-rec1" '(a . b)
       ((with-module gauche.internal %apply-rec1) (lambda (x) (cons 'a x)) 'b))
(test* "apply-rec2" '(x y c)
       ((with-module gauche.internal %apply-rec2)
        (lambda (a b) (list a b 'c)) 'x 'y))
(test* "apply-rec3" '(x y z c)
       ((with-module gauche.internal %apply-rec3)
        (lambda (a b c) (list a b c 'c)) 'x 'y 'z))
(test* "apply-rec4" '(x y z w c)
       ((with-module gauche.internal %apply-rec4)
        (lambda (a b c d) (list a b c d 'c)) 'x 'y 'z 'w))
(test* "apply-rec" '(x y z w u v c)
       ((with-module gauche.internal %apply-rec)
        (lambda (a b c d e f) (list a b c d e f 'c)) 'x 'y 'z 'w 'u 'v))

;;-------------------------------------------------------------------
(test-section "combinatorial programming utilities")

(test* "pa$" 10 ((pa$ + 3) 7))
(test* "pa$" '(a b c)
       ((pa$ list 'a) 'b 'c))
(test* "pa$" '(a b c)
       ((pa$ list 'a 'b) 'c))
(test* "pa$" '(a b c)
       ((pa$ (pa$ list 'a) 'b) 'c))

(test "map$" '(2 4 6)
      (lambda ()
        (define map2* (map$ (pa$ * 2)))
        (map2* '(1 2 3))))

(test "compose" '(#t #f #t)
      (lambda ()
        (define not-zero? (compose not zero?))
        (list (not-zero? 3)
              (not-zero? 0)
              (not-zero? -100))))

(test "compose" 'a (lambda () ((compose car) '(a b c))))
(test "compose" '(a b c) (lambda () ((compose) '(a b c))))

(test "complement" '(#t #f #t)
      (lambda () (map (complement even?) '(1 2 3))))
(test "complement" '(#t #f #t)
      (lambda () (map (complement zero?) '(-1 0 1))))
(test "complement" '(#f #t #f)
      (lambda () (map (complement =) '(1 2 3) '(1 1 3))))
(test "complement" '(#f #t #f)
      (lambda () (map (complement (lambda (x y) (= x y))) '(1 2 3) '(1 1 3))))
(test "complement" #t
      (lambda () ((complement (lambda () #f)))))

(test "compose, apply$, map$" 32
      (lambda ()
        (define dot-product (compose (apply$ +) (map$ *)))
        (dot-product '(1 2 3) '(4 5 6))))

(test "any-pred" '(#t #t #f)
      (lambda ()
        (define string-or-symbol? (any-pred string? symbol?))
        (list (string-or-symbol? "abc")
              (string-or-symbol? 'abc)
              (string-or-symbol? 3))))

(test "any-pred" '(b c)
      (lambda ()
        ((any-pred (cut memq <> '(a b c))
                   (cut memq <> '(1 2 3)))
         'b)))

(test "any-pred" '(#t #f)
      (lambda ()
        (define <> (any-pred < >))
        (list (<> 3 4)
              (<> 3 3))))

(test "every-pred" '(#t #f #f)
      (lambda ()
        (list ((every-pred odd? positive?) 3)
              ((every-pred odd? positive?) 4)
              ((every-pred odd? positive?) -3))))

(test "every-pred" '(3 #f)
      (lambda ()
        (define safe-length (every-pred list? length))
        (list (safe-length '(a b c))
              (safe-length "aaa"))))

;;-------------------------------------------------------------------
#|  disabled for now.

(test-section "currying")

(define-curry (curried2 a b) (list a b))
(define-curry (curried2+ a b . c) (list a b c))
(define-curry (curried3 a b c) (list a b c))
(define-curry (curried3+ a b c . d) (list a b c d))
(define-curry (curried4 a b c d) (list a b c d))
(define-curry (curried4+ a b c d . e) (list a b c d e))
(define-curry (curried5 a b c d e) (list a b c d e))
(define-curry (curried5+ a b c d e . f) (list a b c d e f))

(define-macro (ctest expected . tests)
  `(begin
     ,@(map (lambda (test)
              `(test* ,(write-to-string test) ,expected ,test))
            tests)))

(ctest '(1 2) (curried2 1 2) ((curried2 1) 2))
(ctest (test-error) ((curried2 1) 2 3) (curried2))

(ctest '(1 2)
       (apply curried2 '(1 2))
       (apply curried2 1 '(2))
       (apply curried2 1 2 '())
       ((apply curried2 '(1)) 2)
       ((apply curried2 1 '()) 2)
       (apply (apply curried2 '(1)) '(2)))
(ctest (test-error) (apply curried2 '()))

(ctest '(1 2 ()) (curried2+ 1 2) ((curried2+ 1) 2))
(ctest '(1 2 (3 4)) (curried2+ 1 2 3 4) ((curried2+ 1) 2 3 4))
(ctest (test-error) ((curried2+ 1 2) 3 4))

(ctest '(1 2 ()) (apply curried2+ '(1 2)) ((apply curried2+ '(1)) 2))
(ctest '(1 2 (3 4))
       (apply curried2+ '(1 2 3 4))
       (apply curried2+ 1 '(2 3 4))
       (apply curried2+ 1 2 '(3 4))
       (apply curried2+ 1 2 3 '(4))
       (apply curried2+ 1 2 3 4 '())
       ((apply curried2+ '(1)) 2 3 4)
       ((apply curried2+ 1 '()) 2 3 4)
       (apply (apply curried2+ '(1)) '(2 3 4)))
(ctest (test-error) (apply curried2+ '()))

(ctest '(1 2 3) (curried3 1 2 3) ((curried3 1) 2 3) ((curried3 1 2) 3))
(ctest '(1 2 3)
       (apply curried3 '(1 2 3))
       (apply curried3  1 '(2 3))
       (apply curried3  1 2 '(3))
       (apply curried3  1 2 3 '())
       ((apply curried3 '(1 2)) 3)
       ((apply curried3 1 '(2)) 3)
       ((apply curried3 1 2 '()) 3)
       ((apply curried3 '(1)) 2 3)
       ((apply curried3 1 '()) 2 3)
       (apply (apply curried3 '(1)) '(2 3))
       ((apply (apply curried3 '(1)) '(2)) 3)
       ((apply (apply curried3 '(1)) 2 '()) 3))

(ctest '(1 2 3 (4 5))
       (apply curried3+ '(1 2 3 4 5))
       (apply curried3+ 1 '(2 3 4 5))
       (apply curried3+ 1 2 '(3 4 5))
       (apply curried3+ 1 2 3 '(4 5))
       (apply curried3+ 1 2 3 4 '(5))
       (apply curried3+ 1 2 3 4 5 '())
       ((apply curried3+ '(1)) 2 3 4 5)
       ((apply curried3+ 1 '()) 2 3 4 5)
       (apply (apply curried3+ '(1)) '(2 3 4 5))
       (apply (apply curried3+ '(1)) 2 '(3 4 5))
       (apply (apply curried3+ '(1)) 2 3 '(4 5))
       (apply (apply curried3+ '(1)) 2 3 4 '(5))
       (apply (apply curried3+ 1 '()) '(2 3 4 5))
       (apply (apply curried3+ 1 '()) 2 '(3 4 5))
       (apply (apply curried3+ 1 '()) 2 3 '(4 5))
       (apply (apply curried3+ 1 '()) 2 3 4 '(5))
       (apply (apply curried3+ '(1 2)) '(3 4 5))
       (apply (apply curried3+ '(1 2)) 3 '(4 5))
       (apply (apply curried3+ '(1 2)) 3 4 '(5))
       (apply (apply curried3+ 1 '(2)) '(3 4 5))
       (apply (apply curried3+ 1 '(2)) 3 '(4 5))
       (apply (apply curried3+ 1 '(2)) 3 4 '(5))
       (apply (apply curried3+ 1 2 '()) '(3 4 5))
       (apply (apply curried3+ 1 2 '()) 3 '(4 5))
       (apply (apply curried3+ 1 2 '()) 3 4 '(5))
       (apply (apply (apply curried3+ '(1)) '(2)) '(3 4 5))
       (apply (apply (apply curried3+ '(1)) '(2)) 3 '(4 5))
       (apply (apply (apply curried3+ 1 '()) 2 '()) '(3 4 5))
       (apply (apply (apply curried3+ 1 '()) 2 '()) 3 '(4 5))
       )
|#

;;-----------------------------------------------------------------------
;; procedures in src/scmlib.scm

(test-section "scmlib builtins")

(test* "map* 1 arg" '(-1 -2 -3 . 1/4)
       (map* - / '(1 2 3 . 4)))

(test* "map* 1 arg" '(-1 -2 -3 . #t)
       (map* - null? '(1 2 3)))

(test* "map* 1 arg" #f
       (map* - null? 1))

(test* "map* n arg same elements" '(6 9 12 . 120)
       (map* + * '(1 2 3 . 4) '(2 3 4 . 5) '(3 4 5 . 6)))

(test* "map* n arg uneven elements" '(6 9 12 4 () (6))
       (map* + list '(1 2 3 . 4) '(2 3 4) '(3 4 5 6)))

(test* "map* n arg uneven elements" '((1 2 3 . 4) () (3 4 5 6))
       (map* + list '(1 2 3 . 4) '() '(3 4 5 6)))

(test* "map* n arg uneven elements" '((1 2 3 . 4) 0 (3 4 5 6))
       (map* + list '(1 2 3 . 4) 0 '(3 4 5 6)))

;;-------------------------------------------------------------------
(test-section "optional arguments")

(define (oof x . args)
  (let-optionals* args ((a 'a)
                        (b 'b)
                        (c 'c))
    (list x a b c)))

(test* "let-optionals*" '(0 a b c) (oof 0))
(test* "let-optionals*" '(0 1 b c) (oof 0 1))
(test* "let-optionals*" '(0 1 2 c) (oof 0 1 2))
(test* "let-optionals*" '(0 1 2 3) (oof 0 1 2 3))

(define (oof* x . args)
  (let-optionals* args ((a 'a)
                        (b 'b)
                        . c)
    (list x a b c)))

(test* "let-optionals*" '(0 a b ()) (oof* 0))
(test* "let-optionals*" '(0 1 b ()) (oof* 0 1))
(test* "let-optionals*" '(0 1 2 ()) (oof* 0 1 2))
(test* "let-optionals*" '(0 1 2 (3)) (oof* 0 1 2 3))

(define (oof+ x . args)
  (let ((i 0))
    (let-optionals* (begin (inc! i) args)
        ((a 'a)
         (b 'b)
         (c 'c))
      i)))

(test* "let-optionals*" 1 (oof+ 0))
(test* "let-optionals*" 1 (oof+ 0 1))
(test* "let-optionals*" 1 (oof+ 0 1 2))
(test* "let-optionals*" 1 (oof+ 0 1 2 3))

(define (oaf x . args)
  (let ((y (get-optional args 'foof)))
    (list x y)))

(test* "get-optional" '(0 foof) (oaf 0))
(test* "get-optional" '(0 1)    (oaf 0 1))

(define (oaf+ x . args)
  (let ((i 0))
    (let ((y (get-optional (begin (inc! i) args) 'foof)))
      i)))

(test* "get-optional" 1 (oaf+ 0))
(test* "get-optional" 1 (oaf+ 0 1))

(define (oef x . args)
  (let-keywords* args ((a 'a)
                       (b :bb 'b)
                       (c 'c))
    (list x a b c)))

(test* "let-keywords*" '(0 a b c) (oef 0))
(test* "let-keywords*" '(0 1 b c) (oef 0 :a 1))
(test* "let-keywords*" '(0 a 1 c) (oef 0 :bb 1))
(test* "let-keywords*" '(0 a b 1) (oef 0 :c 1))
(test* "let-keywords*" '(0 1 2 3) (oef 0 :c 3 :bb 2 :a 1))
;;(test* "let-keywords*" (test-error) (oef 0 :c 3 :bb 2 :a 1 :unknown 1))

(define (oef+ x . args)
  (let ((i 0))
    (let-keywords* (begin (inc! i) args)
        ((a 'a)
         (b :bb 'b)
         (c 'c))
      i)))

(test* "let-keywords*" 1 (oef+ 0))
(test* "let-keywords*" 1 (oef+ 0 :a 1))
(test* "let-keywords*" 1 (oef+ 0 :bb 1))
(test* "let-keywords*" 1 (oef+ 0 :c 1))
(test* "let-keywords*" 1 (oef+ 0 :c 3 :bb 2 :a 1))
;;(test* "let-keywords*" (test-error) (oef+ 0 :c 3 :bb 2 :a 1 :unknown 1))

(define (orf x . args)
  (let-keywords args ((a 'a)
                      (b :bb 'b)
                      (c 'c))
    (list x a b c)))

(test* "let-keywords" '(0 a b c)   (orf 0))
(test* "let-keywords" '(0 1 b c)   (orf 0 :a 1))
(test* "let-keywords" '(0 a 1 c)   (orf 0 :bb 1))
(test* "let-keywords" '(0 a b 1)   (orf 0 :c 1))
(test* "let-keywords" '(0 1 2 3)   (orf 0 :c 3 :bb 2 :a 1))
(test* "let-keywords" (test-error) (orf 0 :c 3 :bb 2 :a 1 :unknown 1))

(define (orf+ x . args)
  (let ((i 0))
    (let-keywords (begin (inc! i) args)
        ((a 'a)
         (b :bb 'b)
         (c 'c))
      i)))

(test* "let-keywords" 1 (orf+ 0))
(test* "let-keywords" 1 (orf+ 0 :a 1))
(test* "let-keywords" 1 (orf+ 0 :bb 1))
(test* "let-keywords" 1 (orf+ 0 :c 1))
(test* "let-keywords" 1 (orf+ 0 :c 3 :bb 2 :a 1))
(test* "let-keywords" (test-error) (orf 0 :c 3 :bb 2 :a 1 :unknown 1))

;; let-keywords* combined with syntax rules
(define-syntax lambda++
  (syntax-rules (:key)
    ((lambda++ "sub" () (margs ...) kargs . body)
     (lambda (margs ... . rest)
       (let-keywords* rest kargs
         . body)))
    ((lambda++ "sub" (:key) margs kargs . body)
     (lambda++ "sub" () margs kargs . body))
    ((lambda++ "sub" (:key (arg1 def1) args ...) margs (kargs ...) . body)
     (lambda++ "sub" (:key args ...) margs (kargs ... (arg1 def1)) . body))
    ((lambda++ "sub" (:key arg1 args ...) margs (kargs ...) . body)
     (lambda++ "sub" (:key args ...) margs (kargs ... (arg1 #f)) . body))
    ((lambda++ "sub" (arg1 args ...) (margs ...) kargs . body)
     (lambda++ "sub" (args ...) (margs ... arg1) kargs . body))
    ((lambda++ args . body)
     (lambda++ "sub" args () () . body))
    ))

(test* "macro + let-keywords*" '(1 2 3 #f 5)
       ((lambda++ (a b c :key d e) (list a b c d e))
        1 2 3 :e 5))

(test* "macro + let-keywords*" (test-error)
       ((lambda++ (a b c :key d e) (list a b c d e))
        1 2 :d 3))

(test* "macro + let-keywords*" '(1 2 3 4 #f)
       ((lambda++ (a b c :key d e) (list a b c d e))
        1 2 3 :d 4))

(test* "macro + let-keywords*" '(1 2 3 0 1)
       ((lambda++ (a b c :key (d 0) (e 1)) (list a b c d e))
        1 2 3))

;; extended lambda formals are supported within compile.scm, but
;; it expands to let-optionals*/let-keywords*, so we test it here.
(test-section "extended lambda formals")

(define (test-optkey sig proc arg res)
  (let loop ((res  res)
             (rarg (reverse arg)))
    (test* sig (car res) (apply proc (reverse rarg)))
    (unless (null? rarg)
      (loop (cdr res) (cdr rarg)))))

(test-optkey ":optional c d"
             (lambda (a b :optional c d) (list a b c d))
             '(1 2 3 4)
             `((1 2 3 4) (1 2 3 ,(undefined))
               (1 2 ,(undefined) ,(undefined))
               ,(test-error) ,(test-error)))

(test-optkey ":optional (c 99) (d 100)"
             (lambda (a b :optional (c 99) (d 100)) (list a b c d))
             '(1 2 3 4)
             `((1 2 3 4) (1 2 3 100) (1 2 99 100)
               ,(test-error) ,(test-error)))

(test-optkey ":optional a (b 99) (c 100) d"
             (lambda (:optional a (b 99) (c 100) d) (list a b c d))
             '(1 2 3 4)
             `((1 2 3 4) (1 2 3 ,(undefined)) (1 2 100 ,(undefined))
               (1 99 100 ,(undefined)) (,(undefined) 99 100 ,(undefined))))

(test-optkey ":key (c 99) (d 100)"
             (lambda (a b :key (c 99) (d 100)) (list a b c d))
             '(1 2 :c 3 :d 4)
             `((1 2 3 4) ,(test-error) (1 2 3 100) ,(test-error)
               (1 2 99 100)
               ,(test-error) ,(test-error)))

(test-optkey ":optional (c 0) (d 1) :rest z"
             (lambda (a b :optional (c 0) (d 1) :rest z) (list a b c d z))
             '(1 2 3 4 5 6)
             `((1 2 3 4 (5 6))
               (1 2 3 4 (5))
               (1 2 3 4 ())
               (1 2 3 1 ())
               (1 2 0 1 ())
               ,(test-error)
               ,(test-error)))

(test-optkey ":rest z :optional (c 0) (d 1)"
             (lambda (a b :rest z :optional (c 0) (d 1)) (list a b c d z))
             '(1 2 3 4 5 6)
             `((1 2 3 4 (5 6))
               (1 2 3 4 (5))
               (1 2 3 4 ())
               (1 2 3 1 ())
               (1 2 0 1 ())
               ,(test-error)
               ,(test-error)))

(test-optkey ":key (b 0) (c 1) :rest z"
             (lambda (a :key (b 0) (c 1) :rest z) (list a b c z))
             '(1 :c 99 :b 88)
             `((1 88 99 (:c 99 :b 88)) ,(test-error)
               (1 0 99 (:c 99)) ,(test-error)
               (1 0 1 ()) ,(test-error)))

(test-optkey ":key (b 0) (c 1) :allow-other-keys :rest z"
             (lambda (a :key (b 0) (c 1) :allow-other-keys :rest z)
               (list a b c z))
             '(1 :c 99 :a 77)
             `((1 0 99 (:c 99 :a 77)) ,(test-error)
               (1 0 99 (:c 99)) ,(test-error)
               (1 0 1 ()) ,(test-error)))

(test-optkey ":key (b 0) (c 1) :allow-other-keys y :rest z"
             (lambda (a :key (b 0) (c 1) :allow-other-keys y :rest z)
               (list a b c y z))
             '(1 :d 66 :c 99 :a 77)
             `((1 0 99 (:a 77 :d 66) (:d 66 :c 99 :a 77)) ,(test-error)
               (1 0 99 (:d 66) (:d 66 :c 99)) ,(test-error)
               (1 0 1 (:d 66) (:d 66)) ,(test-error)
               (1 0 1 () ()) ,(test-error)))

(test-optkey ":optional b c :key (d 0) (e 1) :rest z"
             (lambda (a :optional b c :key (d 0) (e 1) :rest z)
               (list a b c d e z))
             '(1 :e 99 :d 66)
             `((1 :e 99 66 1 (:d 66)) ,(test-error)
               (1 :e 99 0 1 ())
               (1 :e ,(undefined) 0 1 ())
               (1 ,(undefined) ,(undefined) 0 1 ())
               ,(test-error)))

;;-----------------------------------------------------------------------
;; case-lambda
(test-section "case-lambda")

(define (case-lambda-test sig fn . arg&exp)
  (for-each (lambda (arg&exp)
              (let [(args (car arg&exp))
                    (exp  (cdr arg&exp))]
                (test* #"case-lambda ~sig ~(length args)" exp
                       (apply fn args))))
            arg&exp))

(case-lambda-test "[0..]"
                  (case-lambda 
                    (() 0)
                    ((x) x)
                    ((x y) (+ x y))
                    ((x y z) (+ (+ x y) z))
                    (args (apply + args)))
                  '[() . 0]
                  '[(1) . 1]
                  '[(1 2) . 3]
                  '[(1 2 3) . 6]
                  '[(1 2 3 4) . 10])

(case-lambda-test "[2..4]"
                  (case-lambda
                    [(a b) `(two ,a ,b)]
                    [(a b c) `(three ,a ,b ,c)]
                    [(a b c d) `(four ,a ,b ,c ,d)])
                  `[() . ,(test-error)]
                  `[(1) . ,(test-error)]
                  `[(1 2) . (two 1 2)]
                  `[(1 2 3) . (three 1 2 3)]
                  `[(1 2 3 4) . (four 1 2 3 4)]
                  `[(1 2 3 4 5) . ,(test-error)])

(case-lambda-test "[1..] matching order"
                  (case-lambda
                    [(x y)   `(foo ,x ,y)]
                    [(x . y) `(bar ,x ,y)])
                  `[() . ,(test-error)]
                  `[(1) . (bar 1 ())]
                  `[(1 2) . (foo 1 2)]
                  `[(1 2 3) . (bar 1 (2 3))])

(case-lambda-test "[0..] matching order"
                  (case-lambda
                    [a   `(foo ,a)]
                    [(x) `(bar ,x)])
                  `[(1) . (foo (1))])

(case-lambda-test "[0,2]"
                  (case-lambda
                    [() 'yot]
                    [(x y) `(bot ,x, y)])
                  `[() . yot]
                  `[(1) . ,(test-error)]
                  `[(1 2) . (bot 1 2)])

(case-lambda-test "one lambda"
                  (case-lambda
                    [(x . y) `(foo ,x ,y)])
                  `[() . ,(test-error)]
                  `[(1) . (foo 1 ())]
                  `[(1 2) . (foo 1 (2))])

;;-----------------------------------------------------------------------
;; generator-*
(test-section "generator-*")

(define (gen-1arg-test name input expect thunk :optional (wrap identity))
  (test* (format "~a, 1 arg" name) expect
         (wrap (with-input-from-string input thunk))))

(gen-1arg-test "generator-fold" "a b c d e" '(e d c b a . z)
               (cut generator-fold cons 'z read))
(gen-1arg-test "generator-fold-right" "a b c d e" '(a b c d e . z)
               (cut generator-fold-right cons 'z read))
(gen-1arg-test "generator-map" "a b c d e" '("a" "b" "c" "d" "e")
               (cut generator-map symbol->string read))
(let1 r '()
  (gen-1arg-test "generator-for-each" "a b c d e" '(e d c b a)
                 (cut generator-for-each (^v (push! r v)) read)
                 (^_ r)))
(gen-1arg-test "generator-find" "2 4 0 8 3 6 9" 3
               (cut generator-find odd? read))

(define (gen-2arg-test name input1 input2 expect proc :optional (wrap identity))
  (test* (format "~a, 2 args" name) expect
         (wrap (call-with-input-string input1
                 (^x (call-with-input-string input2
                       (^y (proc x y))))))))

(gen-2arg-test "generator-fold" "a b c d e f" "1 2 3 4 5"
               '(e 5 (d 4 (c 3 (b 2 (a 1 z)))))
               (^[x y] (generator-fold list 'z (cut read x) (cut read y))))
(gen-2arg-test "generator-fold-right" "a b c d e f" "1 2 3 4 5"
               '(a 1 (b 2 (c 3 (d 4 (e 5 z)))))
               (^[x y] (generator-fold-right list 'z (cut read x) (cut read y))))
(gen-2arg-test "generator-map" "1 2 3 4 5" "6 7 8 9"
               '(7 9 11 13)
               (^[x y] (generator-map + (cut read x) (cut read y))))
(let1 r '()
  (gen-2arg-test "generator-for-each" "1 2 3 4 5" "6 7 8 9"
                 '(13 11 9 7)
                 (^[x y] (generator-for-each (^[a b] (push! r (+ a b)))
                                             (cut read x) (cut read y)))
                 (^_ r)))

(test-end)
