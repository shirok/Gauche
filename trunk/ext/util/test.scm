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
       (map (lambda (exp)
              (match exp
                ((a b) (list 'huh? a b))
                (()    'emptylist)
                (#t    'true)
                (#f    'false)
                ("a"   'string)
                (1     'number)
                (#\a   'character)
                ('a    'symbol)
                (_     'any)))
            '(() #t #f "a" 1 #\a a #())))

(test* "pattern" '((5 x (y z))
                   (4 x y)
                   (2 x (y z) (u v w))
                   (1 x y (u v))
                   (2 x () (y))
                   (3 x y z))
       (map (lambda (exp)
              (match exp
                (((a b) c)   (list 1 a b c))
                (((a . b) c) (list 2 a b c))
                (#(a b c)    (list 3 a b c))
                ((a b)       (list 4 a b))
                ((a . b)     (list 5 a b))))
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
       (map (lambda (exp)
              (match exp
                ((a b c ..3)  (list 1 a b c))
                (#(a b c ..3) (list 2 a b c))
                ((a b c ...)  (list 3 a b c))
                (#(a b c ...) (list 4 a b c))
                ))
            '((1 2 3 4 5)
                                        ;#(1 2 3 4 5)  ; doesn't work?
              (1 2 3 4)
              #(1 2 3 4)
              (1 2)
              #(1 2))))

(test* "nested pattern" '((1 4) (2 5) (3 6))
       (match '((1 (2 3)) (4 (5 6)))
         (((a (b c)) ...) (list a b c))))

;; a bug pointed by Hira
(test* "nested pattern, conditional" 'a
       (match '((1 a b) (4 e) (6 q u e r))
         ((((? number?) (? symbol?) ...) ...) 'a)))

;; examples shown in Wright&Duba
(test* "xmap" '(2 4 6)
       (letrec ((xmap (lambda (f l)
                        (match l
                          (() ())
                          ((x . y) (cons (f x) (xmap f y)))))))
         (xmap (cut * <> 2) '(1 2 3))))

(test* "Y?" '(#t #f)
       (letrec ((y? (match-lambda
                      (('lambda (f1)
                         ('lambda (y1)
                           ((('lambda (x1) (f2 ('lambda (z1) ((x2 x3) z2))))
                             ('lambda (a1) (f3 ('lambda (b1) ((a2 a3) b2)))))
                            y2)))
                       (and (symbol? f1) (symbol? y1) (symbol? x1)
                            (symbol? z1) (symbol? a1) (symbol? b1)
                            (eq? f1 f2) (eq? f1 f3) (eq? y1 y2)
                            (eq? x1 x2) (eq? x1 x3) (eq? z1 z2)
                            (eq? a1 a2) (eq? a1 a3) (eq? b1 b2)))
                      (_ #f))))
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

;;--------------------------------------------------------------

(test* "pred" '(a b c)
       (match '("abc" a b c)
         (((? string?) x ...) x)))

(test* "pred" "abc" 
       (match '("abc" a b c)
         (((? string? k) x ...) k)))

;;--------------------------------------------------------------

(define-class <foo> ()
  ((a :init-keyword :a :accessor a-of)
   (b :init-keyword :b :accessor b-of)))

(test* "struct" '(0 "foo")
       (match (make <foo> :a 0 :b "foo")
         (($ <foo> x y) (list x y))))

(test* "field" 0
       (match (make <foo> :a 0 :b "foo")
         ((= a-of aa) aa)))

(test* "object" '(1 "bar")
       (match (make <foo> :a 1 :b "bar")
         ((object <foo> (b bb) (a aa)) (list aa bb))))

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
   ((and s (? symbol?) (not 'lambda))
    (make Var :s s))
   ((? number? n)
    (make Const :n n))
   (('lambda (and args ((? symbol?) ...) (not (? repeats?))) body)
    (make Lam :args args :body (parse body)))
   ((f args ...)
    (make App :fun (parse f) :args (map parse args)))
   (x
    (error "invalid expression" x))))

(define (repeats? l)
  (and (not (null? l))
       (or (memq (car l) (cdr l)) (repeats? (cdr l)))))

(define unparse
  (match-lambda
   (($ Var s) s)
   (($ Const n) n)
   (($ Lam args body) `(lambda ,args ,(unparse body)))
   (($ App f args) `(,(unparse f) ,@(map unparse args)))))

(test* "parse-unparse" '(lambda (a b c) (map (lambda (d) (a d 3)) (list b c)))
       (unparse (parse '(lambda (a b c)
                          (map (lambda (d) (a d 3)) (list b c))))))

(define unparse-obj
  (match-lambda
   ((@ Var (s symbol)) symbol)
   ((@ Const (n number)) number)
   ((@ Lam (body body-expr) (args lambda-list))
    `(lambda ,lambda-list ,(unparse-obj body-expr)))
   ((@ App (fun f) (args args))
    `(,(unparse-obj f) ,@(map unparse-obj args)))))

(test* "parse-unparse-obj"
       '(lambda (a b c) (map (lambda (d) (a d 3)) (list b c)))
       (unparse-obj (parse '(lambda (a b c)
                              (map (lambda (d) (a d 3)) (list b c))))))

;;--------------------------------------------------------------

(test* "get! / pair" 3
       (let ((x (list 1 (list 2 3))))
         (match x
           ((_ (_ (get! getter))) (getter)))))

(test* "set! / pair" '(1 (2 4))
       (let ((x (list 1 (list 2 3))))
         (match x
           ((_ (_ (set! setter))) (setter 4)))
         x))

(test* "get! / vector" 3
       (let ((x (vector 1 2 3 4 5)))
         (match x
           (#(a b (get! getter) d e) (getter)))))

(test* "set! / vector" '#(1 2 o 4 5)
       (let ((x (vector 1 2 3 4 5)))
         (match x
           (#(a b (set! setter) d e) (setter 'o)))
         x))

(test* "get! / $" 'foo
       (let* ((v (make Var :s 'foo))
              (x (list 0 0 (vector 0 0 v 0) 0)))
         (match x
           ((_ _ #(_ _ ($ Var (get! getter)) _) _) (getter)))))

(test* "get! / @" 'foo
       (let* ((v (make Var :s 'foo))
              (x (list 0 0 (vector 0 0 v 0) 0)))
         (match x
           ((_ _ #(_ _ (@ Var (s (get! getter))) _) _) (getter)))))
           
(test* "set! / $" 'bar
       (let* ((v (make Var :s 'foo))
              (x (list 0 0 (vector 0 0 v 0) 0)))
         (match x
           ((_ _ #(_ _ ($ Var (set! setter)) _) _)
            (setter 'bar)
            (ref v 's)))))
           
(test* "set! / @" 'bar
       (let* ((v (make Var :s 'foo))
              (x (list 0 0 (vector 0 0 v 0) 0)))
         (match x
           ((_ _ #(_ _ (@ Var (s (set! setter))) _) _)
            (setter 'bar)
            (ref v 's)))))

;;--------------------------------------------------------------

;; test case derived from the bug report from Tatsuya BIZENN.

(define (test-match ls)
   (match ls
     ((a b . c)   (=> next) (values a b c))
     ((a . b)     (=> next) (values #f a b))
     (()          (=> next) (values #f #f '()))))

(test* "failure continuation #1" '(1 2 (3))
       (receive r (test-match '(1 2 3)) r))

(test* "failure continuation #2" '(#f 1 ())
       (receive r (test-match '(1)) r))

(test* "failure continuation #2" '(#f #f ())
       (receive r (test-match '()) r))

(test* "failure continuation #2" (test-error)
       (test-match 1))


;;--------------------------------------------------------------

(test* "match-let1" '((a b c) (1 2 3))
       (match-let1 ((ca . cd) ...) '((a . 1) (b . 2) (c . 3))
         (list ca cd)))

;;-----------------------------------------------
(test-section "util.queue")
(use util.queue)
(test-module 'util.queue)

(define (queue-basic-test what maker)
  (define q (maker))

  (test* #`",|what| queue?" #f (queue? (cons 'a 'b)))
  (test* #`",|what| queue?" #f (queue? 3))
  (test* #`",|what| queue?" #f (queue? '()))
  (test* #`",|what| queue?" #t (queue? q))
  (test* #`",|what| enqueue!" #t (begin (enqueue! q 'a) (queue? q)))
  (test* #`",|what| enqueue!" #t (begin (enqueue! q 'b) (queue? q)))
  (test* #`",|what| enqueue!" #t (begin (enqueue! q 'c) (queue? q)))

  (test* #`",|what| queue-front" 'a (queue-front q))
  (test* #`",|what| queue-rear" 'c (queue-rear q))

  (test* #`",|what| enqueue!" '(a f)
         (begin
           (enqueue! q 'd 'e 'f)
           (list (queue-front q) (queue-rear q))))

  (test* #`",|what| dequeue!" 'a (dequeue! q))
  (test* #`",|what| dequeue!" 'b (dequeue! q))
  (test* #`",|what| queue-empty?" #f (queue-empty? q))
  (test* #`",|what| dequeue!" 'c (dequeue! q))
  (test* #`",|what| dequeue!" 'd (dequeue! q))
  (test* #`",|what| dequeue!" 'e (dequeue! q))
  (test* #`",|what| dequeue!" 'f (dequeue! q))
  (test* #`",|what| queue-empty?" #t (queue-empty? q))

  (test* #`",|what| dequeue! (error)" (test-error) (dequeue! q))
  (test* #`",|what| dequeue! (fallback)" "empty!" (dequeue! q "empty!"))
  (test* #`",|what| queue-front (error)" (test-error) (queue-front q))
  (test* #`",|what| queue-front (fallback)" "foo" (queue-front q "foo"))
  (test* #`",|what| queue-rear (error)" (test-error) (queue-rear q))
  (test* #`",|what| queue-rear (fallback)" "foo" (queue-rear q "foo"))

  (test* #`",|what| queue-push!" '(c a)
         (begin
           (queue-push! q 'a) (queue-push! q 'b) (queue-push! q 'c)
           (list (queue-front q) (queue-rear q))))
  (test* #`",|what| queue-push!" '(f a)
         (begin
           (queue-push! q 'd 'e 'f)
           (list (queue-front q) (queue-rear q))))
  (test* #`",|what| queue-pop!" 'f (queue-pop! q))
  (test* #`",|what| queue-pop!" 'e (queue-pop! q))
  (test* #`",|what| queue-empty?" #f (queue-empty? q))
  (test* #`",|what| queue-pop!" 'd (queue-pop! q))
  (test* #`",|what| queue-pop!" 'c (queue-pop! q))
  (test* #`",|what| queue-pop!" 'b (queue-pop! q))
  (test* #`",|what| queue-pop!" 'a (queue-pop! q))
  (test* #`",|what| queue-empty?" #t (queue-empty? q))

  (test* #`",|what| dequeue-all!" '(a b c d e)
         (begin (enqueue! q 'a 'b 'c 'd 'e) (dequeue-all! q)))
  (test* #`",|what| dequeue-all!" '() (dequeue-all! q))
  (test* #`",|what| dequeue-all!" #t  (queue-empty? q))

  (test* #`",|what| find-in-queue" #f (find-in-queue (cut eq? <> 'a) q))
  (test* #`",|what| find-in-queue" 'a (begin (enqueue! q 'a 'b 'c 'd 'e)
                                             (find-in-queue (cut eq? <> 'a) q)))
  (test* #`",|what| find-in-queue" 'c (find-in-queue (cut eq? <> 'c) q))
  (test* #`",|what| find-in-queue" 'e (find-in-queue (cut eq? <> 'e) q))
  (test* #`",|what| find-in-queue" '#f (find-in-queue (cut eq? <> 'f) q))

  (test* #`",|what| any-in-queue?" 'ok
         (any-in-queue (^x (and (eq? x 'c) 'ok)) q))
  (test* #`",|what| any-in-queue?" #f
         (any-in-queue (^x (and (eq? x 'z) 'ok)) q))
  (test* #`",|what| every-in-queue?" #t (every-in-queue symbol? q))
  (test* #`",|what| every-in-queue?" #f (every-in-queue (cut eq? <> 'a) q))
  
  (test* #`",|what| remove-from-queue!" #f
         (remove-from-queue! (cut eq? <> 'f) q))
  (test* #`",|what| remove-from-queue!" #t
         (remove-from-queue! (cut eq? <> 'e) q))
  (test* #`",|what| remove-from-queue!" #f
         (remove-from-queue! (cut eq? <> 'e) q))
  (test* #`",|what| remove-from-queue!" #t
         (remove-from-queue! (cut eq? <> 'a) q))
  (test* #`",|what| remove-from-queue!" #t
         (remove-from-queue! (cut memq <> '(b c)) q))
  (test* #`",|what| remove-from-queue!" #t
         (remove-from-queue! (cut eq? <> 'd) q))
  (test* #`",|what| remove-from-queue!" #t
         (queue-empty? q))
  (test* #`",|what| remove-from-queue!" #f
         (remove-from-queue! (cut eq? <> 'd) q))


  (let ((q (make-queue)))
    (test* #`",|what| enqueue-unique!" '("a")
           (begin (enqueue-unique! q equal? "a")
                  (queue->list q)))
    (test* #`",|what| enqueue-unique!" '("a" "b")
           (begin (enqueue-unique! q equal? "b")
                  (queue->list q)))
    (test* #`",|what| enqueue-unique!" '("a" "b")
           (begin (enqueue-unique! q equal? "a")
                  (queue->list q)))
    (test* #`",|what| enqueue-unique!" '("a" "b" "c" "d")
           (begin (enqueue-unique! q equal? "a" "b" "c" "d")
                  (queue->list q)))
    (test* #`",|what| queue-push-unique!" '("e" "a" "b" "c" "d")
           (begin (queue-push-unique! q equal? "d" "e")
                  (queue->list q)))
    (set! q (make-queue))
    (test* #`",|what| queue-push-unique!" '("e" "d")
           (begin (queue-push-unique! q equal? "d" "e")
                  (queue->list q)))
    (test* #`",|what| queue-push-unique!" '("c" "b" "a" "e" "d")
           (begin (queue-push-unique! q equal? "a" "b" "c" "d" "e")
                  (queue->list q)))
    )
  )

(queue-basic-test "simple queue" make-queue)
(queue-basic-test "mtqueue"      make-mtqueue)

(let ((q (make-mtqueue :max-length 3)))
  (test* "mtqueue room" 3 (mtqueue-room q))

  (test* "mtqueue maxlen" 'c
         (begin (enqueue! q 'a)
                (enqueue! q 'b)
                (enqueue! q 'c)
                (queue-rear q)))
  (test* "mtqueue maxlen (enqueue! overflow)" (test-error)
         (enqueue! q 'd))
  (test* "mtqueue maxlen (enqueue! unchanged after overflow)" '(a b c)
         (queue->list q))
  (test* "mtqueue room" 0 (mtqueue-room q))
  (test* "mtqueue maxlen (enqueue! multiarg overflow)" (test-error)
         (begin (dequeue! q)
                (enqueue! q 'd 'e 'f)))
  (test* "mtqueue maxlen (enqueue! atomicity)" '(b c)
         (queue->list q))
  (test* "mtqueue room" 1 (mtqueue-room q))

  (test* "mtqueue maxlen (queue-push! overflow)" (test-error)
         (begin (queue-push! q 'a)
                (queue-push! q 'z)))
  (test* "mtqueue maxlen (queue-push! postcheck)" '(a b c)
         (queue->list q))
  (test* "mtqueue maxlen (queue-push! multiarg overflow)" (test-error)
         (begin (dequeue! q)
                (queue-push! q 'd 'e 'f)))
  (test* "mtqueue maxlen (queue-push! atomicity)" '(b c)
         (queue->list q))
  )

(test* "mtqueue room" +inf.0 (mtqueue-room (make-mtqueue)))

;; Note: */wait! APIs are tested in ext/threads/test.scm instead of here,
;; since we need threads working.

(test-end)
