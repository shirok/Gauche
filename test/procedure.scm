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
;;(test* "let-keywords*" *test-error* (oef 0 :c 3 :bb 2 :a 1 :unknown 1))

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
;;(test* "let-keywords*" *test-error* (oef+ 0 :c 3 :bb 2 :a 1 :unknown 1))

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
(test* "let-keywords" *test-error* (orf 0 :c 3 :bb 2 :a 1 :unknown 1))

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
(test* "let-keywords" *test-error* (orf 0 :c 3 :bb 2 :a 1 :unknown 1))

;; let-keywords* combined with syntax rules
(define-syntax lambda++
  (syntax-rules ()
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

(test* "macro + let-keywords*" *test-error*
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

(define (test-optkey proc arg res)
  (let loop ((res  res)
             (rarg (reverse arg)))
    (test* "let :optional" (car res) (apply proc (reverse rarg)))
    (unless (null? rarg)
      (loop (cdr res) (cdr rarg)))))

(test-optkey (lambda (a b :optional c d) (list a b c d))
             '(1 2 3 4)
             `((1 2 3 4) (1 2 3 ,(undefined))
               (1 2 ,(undefined) ,(undefined))
               ,*test-error* ,*test-error*))

(test-optkey (lambda (a b :optional (c 99) (d 100)) (list a b c d))
             '(1 2 3 4)
             `((1 2 3 4) (1 2 3 100) (1 2 99 100)
               ,*test-error* ,*test-error*))

(test-optkey (lambda (:optional a (b 99) (c 100) d) (list a b c d))
             '(1 2 3 4)
             `((1 2 3 4) (1 2 3 ,(undefined)) (1 2 100 ,(undefined))
               (1 99 100 ,(undefined)) (,(undefined) 99 100 ,(undefined))))

(test-optkey (lambda (a b :key (c 99) (d 100)) (list a b c d))
             '(1 2 :c 3 :d 4)
             `((1 2 3 4) ,*test-error* (1 2 3 100) ,*test-error*
               (1 2 99 100)
               ,*test-error* ,*test-error*))

(test-end)
