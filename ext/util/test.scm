(use gauche.test)

(test-start "util.*")

;;--------------------------------------------------------------
(test-section "util.match")
(use util.match)
(test-module 'util.match)

(test* "primitive types" '(emptylist
                           true
                           false
                           string
                           number
                           character
                           symbol
                           any)
       (map (^[exp]
              (match exp
                [(a b) (list 'huh? a b)]
                [()    'emptylist]
                [#t    'true]
                [#f    'false]
                ["a"   'string]
                [1     'number]
                [#\a   'character]
                ['a    'symbol]
                [_     'any]))
            '(() #t #f "a" 1 #\a a #())))

(test* "pattern" '([5 x (y z)]
                   [4 x y]
                   [2 x (y z) (u v w)]
                   [1 x y (u v)]
                   [2 x () (y)]
                   [3 x y z])
       (map (^[exp]
              (match exp
                [((a b) c)   (list 1 a b c)]
                [((a . b) c) (list 2 a b c)]
                [#(a b c)    (list 3 a b c)]
                [(a b)       (list 4 a b)]
                [(a . b)     (list 5 a b)]))
            '((x y z)
              (x y)
              ((x y z) (u v w))
              ((x y) (u v))
              ((x) (y))
              #(x y z))))


(test* "repetition pattern" '((1 1 2 (3 4 5))
                                        ;(2 1 2 (3 4 5))
                              (3 1 2 (3 4))
                              (4 1 2 (3 4))
                              (3 1 2 ())
                              (4 1 2 ()))
       (map (^[exp]
              (match exp
                [(a b c ..3)  (list 1 a b c)]
                [#(a b c ..3) (list 2 a b c)]
                [(a b c ...)  (list 3 a b c)]
                [#(a b c ...) (list 4 a b c)]
                ))
            '((1 2 3 4 5)
                                        ;#(1 2 3 4 5)  ; doesn't work?
              (1 2 3 4)
              #(1 2 3 4)
              (1 2)
              #(1 2))))

(test* "nested pattern" '((1 4) (2 5) (3 6))
       (match '((1 (2 3)) (4 (5 6)))
         [((a (b c)) ...) (list a b c)]))

;; a bug pointed by Hira
(test* "nested pattern, conditional" 'a
       (match '((1 a b) (4 e) (6 q u e r))
         [(((? number?) (? symbol?) ...) ...) 'a]))

;; the cause of https://github.com/shirok/Gauche/issues/47
(test* "... must only match a proper list" 'c
       (match '(1 . 2)
         [(x) 'a]
         [(x ...) 'b]
         [_ 'c]))

;; examples shown in Wright&Duba
(test* "xmap" '(2 4 6)
       (letrec ([xmap (^[f l]
                        (match l
                          [() ()]
                          [(x . y) (cons (f x) (xmap f y))]))])
         (xmap (cut * <> 2) '(1 2 3))))

(test* "Y?" '(#t #f)
       (letrec ([y? (match-lambda
                      [('lambda (f1)
                         ('lambda (y1)
                           ((('lambda (x1) (f2 ('lambda (z1) ((x2 x3) z2))))
                             ('lambda (a1) (f3 ('lambda (b1) ((a2 a3) b2)))))
                            y2)))
                       (and (symbol? f1) (symbol? y1) (symbol? x1)
                            (symbol? z1) (symbol? a1) (symbol? b1)
                            (eq? f1 f2) (eq? f1 f3) (eq? y1 y2)
                            (eq? x1 x2) (eq? x1 x3) (eq? z1 z2)
                            (eq? a1 a2) (eq? a1 a3) (eq? b1 b2))]
                      [_ #f])])
         (list
          (y? '(lambda (F)
                 (lambda (Y)
                   (((lambda (j) (F (lambda (k) ((j j) k))))
                     (lambda (l) (F (lambda (m) ((l l) m)))))
                    Y))))
          (y? '(lambda (F)
                 (lambda (Y)
                   (((lambda (j) (F (lambda (k) ((j j) k))))
                     (lambda (l) (F (lambda (m) ((l l) l)))))
                    Y))))
          )))

;; interference with hygienic macro
(let ()
  (define-syntax define-registrar
    (syntax-rules ()
      [(_ varname key default2-var default3-var)
       (define (varname proc . opts)
         (match opts
           [() (set! default3-var proc)]
           [(':perspective)  (set! default3-var proc)]
           [(':orthographic) (set! default2-var proc)]
           [(name)
            (cond [(name->window name) => (^[win] (ref win'closure) 'key proc)]
                  [else
                   (errorf "~a: no such window with name: ~a" 'varname name)])]
           ))]))

  (define a #f)
  (define b #f)
  (define-registrar z x a b)
  (test* "match in hygienic expansion" '(2 1)
         (begin (z 1 :perspective)
                (z 2 :orthographic)
                (list a b)))
  )


;;--------------------------------------------------------------

(test* "pred" '(a b c)
       (match '("abc" a b c)
         [((? string?) x ...) x]))

(test* "pred" "abc"
       (match '("abc" a b c)
         [((? string? k) x ...) k]))

;;--------------------------------------------------------------

(define-class <foo> ()
  ((a :init-keyword :a :accessor a-of)
   (b :init-keyword :b :accessor b-of)))

(test* "struct" '(0 "foo")
       (match (make <foo> :a 0 :b "foo")
         [($ <foo> x y) (list x y)]))

(test* "field" 0
       (match (make <foo> :a 0 :b "foo")
         [(= a-of aa) aa]))

(test* "object" '(1 "bar")
       (match (make <foo> :a 1 :b "bar")
         [(object <foo> (b bb) (a aa)) (list aa bb)]))

;; examples shown in Wright&Duba
(define-class Lam ()
  ((args :init-keyword :args)
   (body :init-keyword :body)))
(define-class Var ()
  ((s :init-keyword :s)))
(define-class Const ()
  ((n :init-keyword :n)))
(define-class App ()
  ((fun  :init-keyword :fun)
   (args :init-keyword :args)))

(define parse
  (match-lambda
    [(and s (? symbol?) (not 'lambda))
     (make Var :s s)]
    [(? number? n)
     (make Const :n n)]
    [('lambda (and args ((? symbol?) ...) (not (? repeats?))) body)
     (make Lam :args args :body (parse body))]
    [(f args ...)
     (make App :fun (parse f) :args (map parse args))]
    [x
     (error "invalid expression" x)]))

(define (repeats? l)
  (and (not (null? l))
       (or (memq (car l) (cdr l)) (repeats? (cdr l)))))

(define unparse
  (match-lambda
    [($ Var s) s]
    [($ Const n) n]
    [($ Lam args body) `(lambda ,args ,(unparse body))]
    [($ App f args) `(,(unparse f) ,@(map unparse args))]))

(test* "parse-unparse" '(lambda (a b c) (map (lambda (d) (a d 3)) (list b c)))
       (unparse (parse '(lambda (a b c)
                          (map (lambda (d) (a d 3)) (list b c))))))

(define unparse-obj
  (match-lambda
    [(@ Var (s symbol)) symbol]
    [(@ Const (n number)) number]
    [(@ Lam (body body-expr) (args lambda-list))
     `(lambda ,lambda-list ,(unparse-obj body-expr))]
    [(@ App (fun f) (args args))
     `(,(unparse-obj f) ,@(map unparse-obj args))]))

(test* "parse-unparse-obj"
       '(lambda (a b c) (map (lambda (d) (a d 3)) (list b c)))
       (unparse-obj (parse '(lambda (a b c)
                              (map (lambda (d) (a d 3)) (list b c))))))

;;--------------------------------------------------------------

(test* "get! / pair" 3
       (let1 x (list 1 (list 2 3))
         (match x
           [(_ (_ (get! getter))) (getter)])))

(test* "set! / pair" '(1 (2 4))
       (rlet1 x (list 1 (list 2 3))
         (match x
           [(_ (_ (set! setter))) (setter 4)])))

(test* "get! / vector" 3
       (let1 x (vector 1 2 3 4 5)
         (match x
           [#(a b (get! getter) d e) (getter)])))

(test* "set! / vector" '#(1 2 o 4 5)
       (rlet1 x (vector 1 2 3 4 5)
         (match x
           [#(a b (set! setter) d e) (setter 'o)])))

(test* "get! / $" 'foo
       (let* ([v (make Var :s 'foo)]
              [x (list 0 0 (vector 0 0 v 0) 0)])
         (match x
           [(_ _ #(_ _ ($ Var (get! getter)) _) _) (getter)])))

(test* "get! / @" 'foo
       (let* ([v (make Var :s 'foo)]
              [x (list 0 0 (vector 0 0 v 0) 0)])
         (match x
           [(_ _ #(_ _ (@ Var (s (get! getter))) _) _) (getter)])))

(test* "set! / $" 'bar
       (let* ([v (make Var :s 'foo)]
              [x (list 0 0 (vector 0 0 v 0) 0)])
         (match x
           [(_ _ #(_ _ ($ Var (set! setter)) _) _)
            (setter 'bar)
            (ref v 's)])))

(test* "set! / @" 'bar
       (let* ([v (make Var :s 'foo)]
              [x (list 0 0 (vector 0 0 v 0) 0)])
         (match x
           [(_ _ #(_ _ (@ Var (s (set! setter))) _) _)
            (setter 'bar)
            (ref v 's)])))

;;--------------------------------------------------------------

;; test case derived from the bug report from Tatsuya BIZENN.

(define (test-match ls)
  (match ls
    [(a b . c)   (=> next) (values a b c)]
    [(a . b)     (=> next) (values #f a b)]
    [()          (=> next) (values #f #f '())]))

(test* "failure continuation #1" '(1 2 (3))
       (values->list (test-match '(1 2 3))))

(test* "failure continuation #2" '(#f 1 ())
       (values->list (test-match '(1))))

(test* "failure continuation #2" '(#f #f ())
       (values->list (test-match '())))

(test* "failure continuation #2" (test-error)
       (test-match 1))


;;--------------------------------------------------------------

(test* "match-let1" '((a b c) (1 2 3))
       (match-let1 ((ca . cd) ...) '((a . 1) (b . 2) (c . 3))
         (list ca cd)))

;;--------------------------------------------------------------

;; Interaction between hygienic macro and match
(define-syntax gen-match
  (syntax-rules () [(_) (match 1 [(and 1 x) x])]))

(test* "match in hygienic macro" 1 (gen-match))

(test-end)
