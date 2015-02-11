;;
;; testing macro expansion
;;

(use gauche.test)

(test-start "macro")

;; strip off syntactic information from identifiers in the macro output.
(define (unident form)
  (cond
   ((identifier? form) (identifier->symbol form))
   ((pair? form) (cons (unident (car form)) (unident (cdr form))))
   ((vector? form)
    (list->vector (map unident (vector->list form))))
   (else form)))

(define-macro (test-macro msg expect form)
  `(test ,msg ',expect (lambda () (unident (%macroexpand ,form)))))

;;----------------------------------------------------------------------
;; 

(test-section "primitive macro transformer")

(define-syntax defmac  ; (defmac name args . body)
  (primitive-macro-transformer
   (lambda (form def-env use-env)
     (let ([name (cadr form)]
           [args (caddr form)]
           [body (cdddr form)])
       `(define-syntax ,name
          (let ((,name (lambda ,args ,@body)))
            (primitive-macro-transformer
             (lambda (f d u) (apply ,name (cdr f))))))))))

(defmac myif (test then else) `(if ,test ,then ,else))

(test-macro "defmac if" (if a b c) (myif a b c))

;; explicit renaming macros

(test-section "ER macro basics")

(define-syntax er-when
  (er-macro-transformer
   (^[f r c]
     (let ([test (cadr f)]
           [exprs (cddr f)])
       `(,(r 'if) ,test (,(r 'begin) ,@exprs))))))

(test "when - basic" #t (^[] (let ((x #f)) (er-when #t (set! x #t)) x)))
(test "when - basic" #f (^[] (let ((x #f)) (er-when #f (set! x #t)) x)))

(test "when - hygene" 3
      (^[] (let ([if list]
                 [begin list])
             (er-when #t 1 2 3))))

(define-syntax er-aif
  (er-macro-transformer
   (^[f r c]
     (let ([test (cadr f)]
           [then (caddr f)]
           [else (cadddr f)])
       `(,(r 'let) ((it ,test))
           (,(r 'if) it ,then ,else))))))

(test "aif - basic" 4 (^[] (er-aif (+ 1 2) (+ it 1) #f)))
(test "aif - basic" 5 (^[] (let ((it 999)) (er-aif (+ 1 2) (+ it 2) #f))))

(test "aif - hygene" 6
      (^[] (let ((it 999)
                 (let list))
             (er-aif (+ 1 2) (+ it 3) #f))))
(test "aif - nesting" #t
      (^[] (let ([it 999])
             (er-aif (+ 1 2) (er-aif (odd? it) it #f) #f))))

(test-section "ER macro local scope")

(let ([if list])
  (let-syntax ([fake-if (er-macro-transformer
                         (^[f r c] `(,(r 'if) ,@(cdr f))))])
    (test "fake-if" '(1 2 3) (^[] (fake-if 1 2 3)))
    (let ([if +])
      (test "fake-if" '(4 5 6) (^[] (fake-if 4 5 6))))))

(test-section "ER compare literals")

;; from Clinger "Hygienic Macros Through Explicit Renaming"
(define-syntax er-cond
  (er-macro-transformer
   (^[f r c]
     (let1 clauses (cdr f)
       (if (null? clauses)
         `(,(r 'quote) ,(r 'unspecified))
         (let* ([first (car clauses)]
                [rest  (cdr clauses)]
                [test  (car first)])
           (cond [(and (or (symbol? test)
                           (identifier? test))
                       (c test (r 'else)))
                  `(,(r 'begin) ,@(cdr first))]
                 [else `(,(r 'if) ,test
                         (,(r 'begin) ,@(cdr first))
                         (er-cond ,@rest))])))))))

(define (er-cond-tester1 x)
  (er-cond [(odd? x) 'odd] [else 'even]))

(test "er-cond 1" '(even odd)
      (^[] (list (er-cond-tester1 0) (er-cond-tester1 1))))

(let ([else #f])
  (define (er-cond-tester2 x)
    (er-cond [(odd? x) 'odd] [else 'even]))
  (test "er-cond 2" '(unspecified odd)
        (^[] (list (er-cond-tester2 0) (er-cond-tester2 1)))))

(define-module er-test-mod
  (export er-cond2)
  (define-syntax er-cond2
    (er-macro-transformer
     (^[f r c]
       (let1 clauses (cdr f)
         (if (null? clauses)
           `(,(r 'quote) ,(r 'unspecified))
           (let* ([first (car clauses)]
                  [rest  (cdr clauses)]
                  [test  (car first)])
             (cond [(and (or (symbol? test)
                             (identifier? test))
                         (c test (r 'else)))
                    `(,(r 'begin) ,@(cdr first))]
                   [else `(,(r 'if) ,test
                           (,(r 'begin) ,@(cdr first))
                           (er-cond2 ,@rest))]))))))))

(define-module er-test-mod2
  (use gauche.test)
  (import er-test-mod)
  (define (er-cond-tester1 x)
    (er-cond2 [(odd? x) 'odd] [else 'even]))
  (test "er-cond (cross-module)" '(even odd)
        (^[] (list (er-cond-tester1 0) (er-cond-tester1 1)))))

;; Introducing local bindings
(let ((x 3))
  (let-syntax ([foo (er-macro-transformer
                     (^[f r c]
                       (let1 body (cdr f)
                         `(,(r 'let) ([,(r 'x) (,(r '+) ,(r 'x) 2)])
                           (,(r '+) ,(r 'x) ,@body)))))])
    (let ((x -1))
      (test* "er-macro introducing local bindings" 4
             (foo x)))))

;; with-renaming
;; Note: currently with-renaming dependns on util.match, too.
(let ((unquote list)
      (x 1)
      (y 2))
  (let-syntax ([foo (er-macro-transformer
                     (^[f r c]
                       (let ([a (cadr f)]
                             [b (caddr f)])
                         (with-renaming r
                           (list x ,a y ,b '#(x ,a y ,b))))))])
    (let ((list vector)
          (x 10)
          (y 20))
      (test* "er-macro and with-renaming"
             '(1 3 2 4 #(x 3 y 4))
             (foo 3 4)))))

;;----------------------------------------------------------------------
;; basic tests

(test-section "basic expansion")

(define-syntax simple (syntax-rules ()
                        ((_ "a" ?a) (a ?a))
                        ((_ "b" ?a) (b ?a))
                        ((_ #f ?a)  (c ?a))
                        ((_ (#\a #\b) ?a) (d ?a))
                        ((_ #(1 2) ?a) (e ?a))
                        ((_ ?b ?a)  (f ?a ?b))))

(test-macro "simple" (a z) (simple "a" z))
(test-macro "simple" (b z) (simple "b" z))
(test-macro "simple" (c z) (simple #f z))
(test-macro "simple" (d z) (simple (#\a #\b) z))
(test-macro "simple" (e z) (simple #(1 2) z))
(test-macro "simple" (f z #(1.0 2.0)) (simple #(1.0 2.0) z))
(test-macro "simple" (f z (#\b #\a)) (simple (#\b #\a) z))
(test-macro "simple" (f z #(2 1)) (simple #(2 1) z))

(define-syntax repeat (syntax-rules ()
                        ((_ 0 (?a ?b) ...)     ((?a ...) (?b ...)))
                        ((_ 1 (?a ?b) ...)     (?a ... ?b ...))
                        ((_ 2 (?a ?b) ...)     (?a ... ?b ... ?a ...))
                        ((_ 0 (?a ?b ?c) ...)  ((?a ...) (?b ?c) ...))
                        ((_ 1 (?a ?b ?c) ...)  (?a ... (?c 8 ?b) ...))
                        ))

(test-macro "repeat" ((a c e) (b d f))
            (repeat 0 (a b) (c d) (e f)))
(test-macro "repeat" (a c e b d f)
            (repeat 1 (a b) (c d) (e f)))
(test-macro "repeat" (a c e b d f a c e)
            (repeat 2 (a b) (c d) (e f)))
(test-macro "repeat" ((a d g) (b c) (e f) (h i))
            (repeat 0 (a b c) (d e f) (g h i)))
(test-macro "repeat" (a d g (c 8 b) (f 8 e) (i 8 h))
            (repeat 1 (a b c) (d e f) (g h i)))

(define-syntax repeat2 (syntax-rules () ;r7rs
                         ((_ 0 (?a ?b ... ?c))    (?a (?b ...) ?c))
                         ((_ 1 (?a ?b ... ?c ?d)) (?a (?b ...) ?c ?d))
                         ((_ 2 (?a ?b ... . ?c))  (?a (?b ...) ?c))
                         ((_ 3 (?a ?b ... ?c ?d . ?e))  (?a (?b ...) ?c ?d ?e))
                         ((_ ?x ?y) ho)))

(test-macro "repeat2" (a (b c d e f) g)
            (repeat2 0 (a b c d e f g)))
(test-macro "repeat2" (a () b)
            (repeat2 0 (a b)))
(test-macro "repeat2" ho
            (repeat2 0 (a)))
(test-macro "repeat2" (a (b c d e) f g)
            (repeat2 1 (a b c d e f g)))
(test-macro "repeat2" (a () b c)
            (repeat2 1 (a b c)))
(test-macro "repeat2" ho
            (repeat2 1 (a b)))
(test-macro "repeat2" (a (b c d e f g) ())
            (repeat2 2 (a b c d e f g)))
(test-macro "repeat2" (a (b c d e) f g ())
            (repeat2 3 (a b c d e f g)))
(test-macro "repeat2" (a (b c d) e)
            (repeat2 2 (a b c d . e)))
(test-macro "repeat2" (a (b) c d e)
            (repeat2 3 (a b c d . e)))

(define-syntax nest1 (syntax-rules ()
                       ((_ (?a ...) ...)        ((?a ... z) ...))))

(test-macro "nest1" ((a z) (b c d z) (e f g h i z) (z) (j z))
            (nest1 (a) (b c d) (e f g h i) () (j)))

(define-syntax nest2 (syntax-rules ()
                       ((_ ((?a ?b) ...) ...)   ((?a ... ?b ...) ...))))

(test-macro "nest2" ((a c b d) () (e g i f h j))
            (nest2 ((a b) (c d)) () ((e f) (g h) (i j))))

(define-syntax nest3 (syntax-rules ()
                       ((_ ((?a ?b ...) ...) ...) ((((?b ...) ...) ...)
                                                   ((?a ...) ...)))))

(test-macro "nest3" ((((b c d e) (g h i)) (() (l m n) (p)) () ((r)))
                     ((a f) (j k o) () (q)))
            (nest3 ((a b c d e) (f g h i)) ((j) (k l m n) (o p)) () ((q r))))

(define-syntax nest4 (syntax-rules () ; r7rs
                       ((_ ((?a ?b ... ?c) ... ?d))
                        ((?a ...) ((?b ...) ...) (?c ...) ?d))))

(test-macro "nest4"((a d f)  
                    ((b) () (g h i))
                    (c e j)
                    (k l m))
            (nest4 ((a b c) (d e) (f g h i j) (k l m)))) 

(define-syntax nest5 (syntax-rules () ; r7rs
                       ((_ (?a (?b ... ?c ?d) ... . ?e))
                        (?a ((?b ...) ...) (?c ...) (?d ...) ?e))))
(test-macro "nest5" (z
                     ((a) (d e) ())
                     (b f h)
                     (c g i)
                     j)
            (nest5 (z (a b c) (d e f g) (h i) . j)))


(define-syntax mixlevel1 (syntax-rules ()
                           ((_ (?a ?b ...)) ((?a ?b) ...))))

(test-macro "mixlevel1" ((1 2) (1 3) (1 4) (1 5) (1 6))
            (mixlevel1 (1 2 3 4 5 6)))

(define-syntax mixlevel2 (syntax-rules ()
                           ((_ (?a ?b ...) ...)
                            (((?a ?b) ...) ...))))

(test-macro "mixlevel2" (((1 2) (1 3) (1 4)) ((2 3) (2 4) (2 5) (2 6)))
            (mixlevel2 (1 2 3 4) (2 3 4 5 6)))

(define-syntax mixlevel3 (syntax-rules ()
                           ((_ ?a (?b ?c ...) ...)
                            (((?a ?b ?c) ...) ...))))

(test-macro "mixlevel3" (((1 2 3) (1 2 4) (1 2 5) (1 2 6))
                         ((1 7 8) (1 7 9) (1 7 10)))
            (mixlevel3 1 (2 3 4 5 6) (7 8 9 10)))

;; test that wrong usage of ellipsis is correctly identified
(test "bad ellipsis 1" (test-error)
      (lambda () 
        (eval '(define-syntax badellipsis
                 (syntax-rules () (t) (3 ...)))
              (interaction-environment))))
(test "bad ellipsis 2" (test-error)
      (lambda ()
        (eval '(define-syntax badellipsis
                 (syntax-rules () (t a) (a ...)))
              (interaction-environment))))
(test "bad ellipsis 3" (test-error)
      (lambda ()
        (eval '(define-syntax badellipsis
                 (syntax-rules () (t a b ...) (a ...)))
              (interaction-environment))))
(test "bad ellipsis 4" (test-error)
      (lambda ()
        (eval '(define-syntax badellipsis
                 (syntax-rules () (t a ...) ((a ...) ...)))
              (interaction-environment))))

(test "bad ellipsis 5" (test-error)
      (lambda ()
        (eval '(define-syntax badellipsis
                 (syntax-rules () ((a ... b ...)) ((a ...) (b ...))))
              (interaction-environment))))
(test "bad ellipsis 6" (test-error)
      (lambda ()
        (eval '(define-syntax badellipsis
                 (syntax-rules () ((... a b)) (... a b )))
              (interaction-environment))))

(define-syntax hygiene (syntax-rules ()
                         ((_ ?a) (+ ?a 1))))
(test "hygiene" 3
      (lambda () (let ((+ *)) (hygiene 2))))

(define-syntax vect1 (syntax-rules ()
                       ((_ #(?a ...)) (?a ...))
                       ((_ (?a ...))  #(?a ...))))
(test-macro "vect1" (1 2 3 4 5)  (vect1 #(1 2 3 4 5)))
(test-macro "vect1" #(1 2 3 4 5) (vect1 (1 2 3 4 5)))

(define-syntax vect2 (syntax-rules ()
                       ((_ #(#(?a ?b) ...))  #(?a ... ?b ...))
                       ((_ #((?a ?b) ...))    (?a ... ?b ...))
                       ((_ (#(?a ?b) ...))    (#(?a ...) #(?b ...)))))

(test-macro "vect2" #(a c e b d f) (vect2 #(#(a b) #(c d) #(e f))))
(test-macro "vect2"  (a c e b d f) (vect2 #((a b) (c d) (e f))))
(test-macro "vect2"  (#(a c e) #(b d f)) (vect2 (#(a b) #(c d) #(e f))))

(define-syntax vect3 (syntax-rules ()
                       ((_ 0 #(?a ... ?b)) ((?a ...) ?b))
                       ((_ 0 ?x) ho)
                       ((_ 1 #(?a ?b ... ?c ?d ?e)) (?a (?b ...) ?c ?d ?e))
                       ((_ 1 ?x) ho)))

(test-macro "vect3" ((a b c d e) f)
            (vect3 0 #(a b c d e f)))
(test-macro "vect3" (() a)
            (vect3 0 #(a)))
(test-macro "vect3" ho
            (vect3 0 #()))
(test-macro "vect3" (a (b c) d e f)
            (vect3 1 #(a b c d e f)))
(test-macro "vect3" (a () b c d)
            (vect3 1 #(a b c d)))
(test-macro "vect3" ho
            (vect3 1 #(a b c)))

(define-syntax dot1 (syntax-rules ()
                      ((_ (?a . ?b)) (?a ?b))
                      ((_ ?loser) #f)))
(test-macro "dot1" (1 2)     (dot1 (1 . 2)))
(test-macro "dot1" (1 (2))   (dot1 (1 2)))
(test-macro "dot1" (1 ())    (dot1 (1)))
(test-macro "dot1" (1 (2 3)) (dot1 (1 2 3)))
(test-macro "dot1" #f        (dot1 ()))

(define-syntax dot2 (syntax-rules ()
                      ((_ ?a . ?b) (?b . ?a))
                      ((_ . ?loser) #f)))
(test-macro "dot2" (2 . 1)     (dot2 1 . 2))
(test-macro "dot2" ((2) . 1)   (dot2 1 2))
(test-macro "dot2" (() . 1)    (dot2 1))
(test-macro "dot2" ((2 3) . 1) (dot2 1 2 3))
(test-macro "dot2" #f          (dot2))

;; pattern to yield (. x) => x
(define-syntax dot3 (syntax-rules ()
                      ((_ (?a ...) ?b) (?a ... . ?b))))
(test-macro "dot3" (1 2 . 3)   (dot3 (1 2) 3))
(test-macro "dot3" 3           (dot3 () 3))

;; see if effective quote introduced by quasiquote properly unwrap
;; syntactic enviornment.
(define-syntax unwrap1 (syntax-rules ()
                         ((_ x) `(a ,x))))
(test "unwrap1" '(a 3) (lambda () (unwrap1 3))
      (lambda (x y) (and (eq? (car x) (car y)) (eq? (cadr x) (cadr y)))))
(test "unwrap1" '(a 4) (lambda () (let ((a 4)) (unwrap1 a))) 
      (lambda (x y) (and (eq? (car x) (car y)) (eq? (cadr x) (cadr y)))))

;; regression check for quasiquote hygienty handling code
(define-syntax qq1 (syntax-rules ()
                     ((_ a) `(,@a))))
(define-syntax qq2 (syntax-rules ()
                     ((_ a) `#(,@a))))

(test "qq1" '()  (lambda () (qq1 '())))
(test "qq2" '#() (lambda () (qq2 '())))

;; R7RS style alternative ellipsis
(test-section "alternative ellipsis")

(define-syntax alt-elli1
  (syntax-rules ooo ()
    [(_ ... ooo) '((... ...) ooo)]))

(test "alt-elli1" '((a a) (b b) (c c)) (lambda () (alt-elli1 a b c)))

(define-syntax alt-elli2
  (syntax-rules ::: ()
    [(_ ... :::) '((... ...) :::)]))

(test "alt-elli2" '((a a) (b b) (c c)) (lambda () (alt-elli2 a b c)))

;;----------------------------------------------------------------------
;; cond, taken from R5RS section 7.3

(test-section "recursive expansion")

(define-syntax %cond
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((temp test))
       (if temp (result temp))))
    ((cond (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (%cond clause1 clause2 ...))))
    ((cond (test)) test)
    ((cond (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp temp (%cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...) clause1 clause2 ...)
     (if test (begin result1 result2 ...) (%cond clause1 clause2 ...)))
    ))

(test-macro "%cond" (begin a) (%cond (else a)))
(test-macro "%cond" (begin a b c) (%cond (else a b c)))
(test-macro "%cond" (let ((temp a)) (if temp (b temp))) (%cond (a => b)))
(test-macro "%cond" (let ((temp a)) (if temp (b temp) (%cond c))) (%cond (a => b) c))
(test-macro "%cond" (let ((temp a)) (if temp (b temp) (%cond c d))) (%cond (a => b) c d))
(test-macro "%cond" (let ((temp a)) (if temp (b temp) (%cond c d e))) (%cond (a => b) c d e))
(test-macro "%cond" a (%cond (a)))
(test-macro "%cond" (let ((temp a)) (if temp temp (%cond b))) (%cond (a) b))
(test-macro "%cond" (let ((temp a)) (if temp temp (%cond b c))) (%cond (a) b c))
(test-macro "%cond" (if a (begin b)) (%cond (a b)))
(test-macro "%cond" (if a (begin b c d)) (%cond (a b c d)))
(test-macro "%cond" (if a (begin b c d) (%cond e f g)) (%cond (a b c d) e f g))

;; test for higiene
(test "%cond" '(if a (begin => b))
      (lambda () (let ((=> #f)) (unident (%macroexpand (%cond (a => b)))))))
(test "%cond" '(if else (begin z))
      (lambda () (let ((else #t)) (unident (%macroexpand (%cond (else z)))))))

;;----------------------------------------------------------------------
;; letrec, taken from R5RS section 7.3
(define-syntax %letrec
  (syntax-rules ()
    ((_ ((var1 init1) ...) body ...)
     (%letrec "generate_temp_names"
              (var1 ...)
              ()
              ((var1 init1) ...)
              body ...))
    ((_ "generate_temp_names" () (temp1 ...) ((var1 init1) ...) body ...)
     (let ((var1 :undefined) ...)
       (let ((temp1 init1) ...)
         (set! var1 temp1) ...
         body ...)))
    ((_ "generate_temp_names" (x y ...) (temp ...) ((var1 init1) ...) body ...)
     (%letrec "generate_temp_names"
              (y ...)
              (newtemp temp ...)
              ((var1 init1) ...)
              body ...))))

;; Note: if you "unident" the expansion result of %letrec, you see a symbol
;; "newtemp" appears repeatedly in the let binding, seemingly expanding
;; into invalid syntax.  Internally, however, those symbols are treated 
;; as identifiers with the correct identity, so the expanded code works
;; fine (as tested in the second test).
(test-macro "%letrec"
            (let ((a :undefined)
                  (c :undefined))
              (let ((newtemp b)
                    (newtemp d))
                (set! a newtemp)
                (set! c newtemp)
                e f g))
            (%letrec ((a b) (c d)) e f g))
(test "%letrec" '(1 2 3)
      (lambda () (%letrec ((a 1) (b 2) (c 3)) (list a b c))))

;;----------------------------------------------------------------------
;; do, taken from R5RS section 7.3
(define-syntax %do
  (syntax-rules ()
    ((_ ((var init step ...) ...)
        (test expr ...)
        command ...)
     (letrec
         ((loop
           (lambda (var ...)
             (if test
                 (begin
                   (if #f #f)
                   expr ...)
                 (begin
                   command
                   ...
                   (loop (%do "step" var step ...)
                         ...))))))
       (loop init ...)))
    ((_ "step" x)
     x)
    ((_ "step" x y)
     y)))

(test-macro "%do"
            (letrec ((loop (lambda (x y)
                             (if (>= x 10)
                                 (begin (if #f #f) y)
                                 (begin (loop (%do "step" x (+ x 1))
                                              (%do "step" y (* y 2))))))))
              (loop 0 1))
            (%do ((x 0 (+ x 1))
                  (y 1 (* y 2)))
                 ((>= x 10) y)))
(test "%do" 1024
      (lambda () (%do ((x 0 (+ x 1))
                       (y 1 (* y 2)))
                      ((>= x 10) y))))

(test-macro "%do"
            (letrec ((loop (lambda (y x)
                             (if (>= x 10)
                                 (begin (if #f #f) y)
                                 (begin (set! y (* y 2))
                                        (loop (%do "step" y)
                                              (%do "step" x (+ x 1))))))))
              (loop 1 0))
            (%do ((y 1)
                  (x 0 (+ x 1)))
                 ((>= x 10) y)
                 (set! y (* y 2))))
(test "%do" 1024
      (lambda () (%do ((y 1)
                       (x 0 (+ x 1)))
                      ((>= x 10) y)
                      (set! y (* y 2)))))

;;----------------------------------------------------------------------
;; non-syntax-rule transformers

(test-section "transformers other than syntax-rules")

(define-syntax xif if)
(test "xif" 'ok (lambda () (xif #f 'ng 'ok)))

(define-syntax fi (syntax-rules () [(_ a b c) (xif a c b)]))
(define-syntax xfi fi)
(test "xfi" 'ok (lambda () (xfi #f 'ok 'ng)))

;;----------------------------------------------------------------------
;; local syntactic bindings.

(test-section "local syntactic bindings")

(test "let-syntax"                      ; R5RS 4.3.1
      'now
      (lambda ()
        (let-syntax ((%when (syntax-rules ()
                             ((_ test stmt1 stmt2 ...)
                              (if test (begin stmt1 stmt2 ...))))))
          (let ((if #t))
            (%when if (set! if 'now))
            if))))

(test "let-syntax"                      ; R5RS 4.3.1
      'outer
      (lambda ()
        (let ((x 'outer))
          (let-syntax ((m (syntax-rules () ((m) x))))
            (let ((x 'inner))
              (m))))))

(test "let-syntax (multi)"
      81
      (lambda ()
        (let ((+ *))
          (let-syntax ((a (syntax-rules () ((_ ?x) (+ ?x ?x))))
                       (b (syntax-rules () ((_ ?x) (* ?x ?x)))))
            (let ((* -)
                  (+ /))
              (a (b 3)))))))

(test "let-syntax (nest)"
      19
      (lambda ()
        (let-syntax ((a (syntax-rules () ((_ ?x ...) (+ ?x ...)))))
          (let-syntax ((a (syntax-rules ()
                            ((_ ?x ?y ...) (a ?y ...))
                            ((_) 2))))
            (a 8 9 10)))))

(test "let-syntax (nest)"
      '(-6 11)
      (lambda ()
        (let-syntax ((a (syntax-rules () ((_ ?x) (+ ?x 8))))
                     (b (syntax-rules () ((_ ?x) (- ?x 8)))))
          (let-syntax ((a (syntax-rules () ((_ ?x) (b 2))))
                       (b (syntax-rules () ((_ ?x) (a 3)))))
            (list (a 7) (b 8))))))

(test "letrec-syntax"                   ; R5RS 4.3.1
      7
      (lambda ()
        (letrec-syntax ((%or (syntax-rules ()
                               ((_) #f)
                               ((_ e) e)
                               ((_ e f ...)
                                (let ((temp e))
                                  (if temp temp (%or f ...)))))))
           (let ((x #f)
                 (y 7)
                 (temp 8)
                 (let odd?)
                 (if even?))
             (%or x (let temp) (if y) y)))))

(test "letrec-syntax (nest)"
      2
      (lambda ()
        (letrec-syntax ((a (syntax-rules () ((_ ?x ...) (+ ?x ...)))))
          (letrec-syntax ((a (syntax-rules ()
                               ((_ ?x ?y ...) (a ?y ...))
                               ((_) 2))))
            (a 8 9 10)))))
      
(test "letrec-syntax (nest)"
      '(9 11)
      (lambda ()
        (letrec-syntax ((a (syntax-rules () ((_ ?x) (+ ?x 8))))
                        (b (syntax-rules () ((_ ?x) (- ?x 8)))))
          (letrec-syntax ((a (syntax-rules ()
                               ((_ ?x)    (b ?x 2))
                               ((_ ?x ?y) (+ ?x ?y))))
                          (b (syntax-rules ()
                               ((_ ?x)    (a ?x 3))
                               ((_ ?x ?y) (+ ?x ?y)))))
            (list (a 7) (b 8))))))

(test "letrec-syntax (recursive)"
      #t
      (lambda ()
        (letrec-syntax ((o? (syntax-rules ()
                              ((o? ()) #f)
                              ((o? (x . xs)) (e? xs))))
                        (e? (syntax-rules ()
                              ((e? ()) #t)
                              ((e? (x . xs)) (o? xs)))))
          (e? '(a a a a)))))

;; This is from comp.lang.scheme posting by Antti Huima
;; http://groups.google.com/groups?hl=ja&selm=7qpu5ncg2l.fsf%40divergence.tcs.hut.fi
(test "let-syntax (huima)" '(1 3 5 9)
      (lambda ()
        (define the-procedure
          (let-syntax((l(syntax-rules()((l((x(y ...))...)b ...)(let-syntax((x (syntax-rules()y ...))...) b ...)))))(l('(('(a b ...)(lambda a b ...)))`((`(a b c)(if a b c))(`(a)(car a))),((,(a b)(set! a b))(,(a)(cdr a))),@((,@z(call-with-current-continuation z))))'((ls)('((s)('((i) ('((d)('((j)('((c)('((p)('((l)('(()(l l))))'((k)`((pair?,(p))('((c) ,(p(append,(,(p))(d c)))(k k))(c`(p)`(,(p))c))`(p)))))(cons(d)(map d ls))))'((x y c),@'((-)(s x y null? - s)(j x y c)))))'((x y c)('((q)('((f)(cons`(q)(c((f x)x)((f y)y)c)))'((h)`((eq? q h)'((x),(x)) i)))),@'((-)(s x y'((z)(>=`(z)(sqrt(*`(x)`(y)))))- s))))))list)) '((z)z)))'((x y p k l),@'((-)`((p x)(k y)(l y x'((z)`((p z)-(- #f)))k l)))))))))
        (the-procedure '(5 1 9 3))))


(test "let-syntax, rebinding syntax" 'ok
      (lambda ()
        (let-syntax ([xif if] [if when]) (xif #f 'ng 'ok))))

(test "let-syntax, rebinding macro" 'ok
      (lambda ()
        (let-syntax ([if fi]) (if #f 'ok 'ng))))

;; Macro-generating-macro scoping
;; Currently it's not working.
(define-syntax mgm-bar
  (syntax-rules ()
    ((_ . xs) '(bad . xs))))

(define-syntax mgm-foo
  (syntax-rules ()
    ((_ xs)
     (letrec-syntax ((mgm-bar
                      (syntax-rules ()
                        ((_ (%x . %xs) %ys)
                         (mgm-bar %xs (%x . %ys)))
                        ((_ () %ys)
                         '%ys))))
       (mgm-bar xs ())))))

'(test "macro-generating-macro scope" '(x y)
      (lambda () (mgm-foo (x y))))

;;----------------------------------------------------------------------
;; macro and internal define

(test-section "macro and internal define")

(define-macro (gen-idef-1 x)
  `(define foo ,x))

(test "define foo (legacy)" 3
      (lambda ()
        (gen-idef-1 3)
        foo))
(test "define foo (legacy)" '(3 5)
      (lambda ()
        (let ((foo 5))
          (list (let () (gen-idef-1 3) foo)
                foo))))
(define foo 10)
(test "define foo (legacy)" '(3 10)
      (lambda ()
        (list (let () (gen-idef-1 3) foo) foo)))
(test "define foo (legacy)" '(4 5)
      (lambda ()
        (gen-idef-1 4)
        (define bar 5)
        (list foo bar)))
(test "define foo (legacy)" '(4 5)
      (lambda ()
        (define bar 5)
        (gen-idef-1 4)
        (list foo bar)))

(test "define foo (error)" (test-error)
      (lambda ()
        (eval '(let ()
                 (list 3 4)
                 (gen-idef-1 5)))))
(test "define foo (error)" (test-error)
      (lambda ()
        (eval '(let ()
                 (gen-idef-1 5)))))

(test "define foo (shadow)" 10
      (lambda ()
        (let ((gen-idef-1 -))
          (gen-idef-1 5)
          foo)))

(define-macro (gen-idef-2 x y)
  `(begin (define foo ,x) (define bar ,y)))

(test "define foo, bar (legacy)" '((0 1) 10)
      (lambda ()
        (let ((l (let () (gen-idef-2 0 1) (list foo bar))))
          (list l foo))))
(test "define foo, bar (legacy)" '(-1 -2 20)
      (lambda ()
        (define baz 20)
        (gen-idef-2 -1 -2)
        (list foo bar baz)))
(test "define foo, bar (legacy)" '(-1 -2 20)
      (lambda ()
        (gen-idef-2 -1 -2)
        (define baz 20)
        (list foo bar baz)))
(test "define foo, bar (legacy)" '(3 4 20 -10)
      (lambda ()
        (begin
          (define biz -10)
          (gen-idef-2 3 4)
          (define baz 20))
        (list foo bar baz biz)))
(test "define foo, bar (legacy)" '(3 4 20 -10)
      (lambda ()
        (define biz -10)
        (begin
          (gen-idef-2 3 4)
          (define baz 20)
          (list foo bar baz biz))))
(test "define foo, bar (legacy)" '(3 4 20 -10)
      (lambda ()
        (begin
          (define biz -10))
        (begin
          (gen-idef-2 3 4))
        (define baz 20)
        (list foo bar baz biz)))
(test "define foo, bar (error)" (test-error)
      (lambda ()
        (eval '(let ()
                 (list 3)
                 (gen-idef-2 -1 -2)
                 (list foo bar)))))
(test "define foo, bar (error)" (test-error)
      (lambda ()
        (eval '(let ()
                 (gen-idef-2 -1 -2)))))

(define-syntax gen-idef-3
  (syntax-rules ()
    ((gen-idef-3 x y)
     (begin (define x y)))))

(test "define boo (r5rs)" 3
      (lambda ()
        (gen-idef-3 boo 3)
        boo))
(test "define boo (r5rs)" '(3 10)
      (lambda ()
        (let ((l (let () (gen-idef-3 foo 3) foo)))
          (list l foo))))

(define-syntax gen-idef-4
  (syntax-rules ()
    ((gen-idef-4 x y)
     (begin (define x y) (+ x x)))))

(test "define poo (r5rs)" 6
      (lambda ()
        (gen-idef-4 poo 3)))

(test "define poo (r5rs)" 3
      (lambda ()
        (gen-idef-4 poo 3) poo))

(define-macro (gen-idef-5 o e)
  `(begin
     (define (,o n)
       (if (= n 0) #f (,e (- n 1))))
     (define (,e n)
       (if (= n 0) #t (,o (- n 1))))))

(test "define (legacy, mutually-recursive)" '(#t #f)
      (lambda ()
        (gen-idef-5 ooo? eee?)
        (list (ooo? 5) (eee? 7))))


(define-syntax gen-idef-6
  (syntax-rules ()
    ((gen-idef-6 o e)
     (begin
       (define (o n) (if (= n 0) #f (e (- n 1))))
       (define (e n) (if (= n 0) #t (o (- n 1))))))))

(test "define (r5rs, mutually-recursive)" '(#t #f)
      (lambda ()
        (gen-idef-5 ooo? eee?)
        (list (ooo? 5) (eee? 7))))

;;----------------------------------------------------------------------
;; macro defining macros

(test-section "macro defining macros")

(define-syntax mdm-foo1
  (syntax-rules ()
    ((mdm-foo1 x y)
     (define-syntax x
       (syntax-rules ()
         ((x z) (cons z y)))))
    ))

(mdm-foo1 mdm-cons 0)

(test "define-syntax - define-syntax" '(1 . 0)
      (lambda () (mdm-cons 1)))

(define-syntax mdm-foo2
  (syntax-rules ()
    ((mdm-foo2 x y)
     (let-syntax ((x (syntax-rules ()
                       ((x z) (cons z y)))))
       (x 1)))))

(test "define-syntax - let-syntax" '(1 . 0)
      (lambda () (mdm-foo2 cons 0)))

(test "let-syntax - let-syntax" '(4 . 3)
      (lambda ()
        (let-syntax ((mdm-foo3 (syntax-rules ()
                                 ((mdm-foo3 x y body)
                                  (let-syntax ((x (syntax-rules ()
                                                    ((x z) (cons z y)))))
                                    body)))))
          (mdm-foo3 list 3 (list 4)))))

;; this doesn't work for now, due to the bug of macro expander
'(test "let-syntax - let-syntax" 3
      (lambda ()
        (let-syntax ((mdm-foo4
                      (syntax-rules ()
                        ((mdm-foo4 () n) n)
                        ((mdm-foo4 (x . xs) n)
                         (let-syntax ((mdm-foo5
                                       (syntax-rules ()
                                         ((mdm-foo5)
                                          (mdm-foo4 xs (+ n 1))))))
                           (mdm-foo5))))))
          (mdm-foo4 (#f #f #f) 0))))

(define-syntax mdm-foo3
  (syntax-rules ()
    ((mdm-foo3 y)
     (letrec-syntax ((o? (syntax-rules ()
                           ((o? ()) #f)
                           ((o? (x . xs)) (not (e? xs)))))
                     (e? (syntax-rules ()
                           ((e? ()) #t)
                           ((e? (x . xs)) (not (o? xs))))))
       (%macroexpand (e? y))))))

;; this doesn't work for now, due to the bug of macro expander
'(test "define-syntax - letrec-syntax" #t
      (lambda () (mdm-foo3 (a))))

;; Examples from "Two pitfalls in programming nested R5RS macros"
;; by Oleg Kiselyov
;;  http://pobox.com/~oleg/ftp/Scheme/r5rs-macros-pitfalls.txt

(define-syntax mdm-bar-m
  (syntax-rules ()
    ((_ x y)
     (let-syntax
         ((helper
           (syntax-rules ()
             ((_ u) (+ x u)))))
       (helper y)))))

(test "lexical scope" 5
      (lambda () (mdm-bar-m 4 1)))

(define-syntax mdm-bar-m1
  (syntax-rules ()
    ((_ var body)
     (let-syntax
         ((helper
           (syntax-rules ()
             ((_) (lambda (var) body)))))
       (helper)))))

(test "lexical scope" 5
      (lambda () ((mdm-bar-m1 z (+ z 1)) 4)))

(define-syntax mdm-bar-m3
  (syntax-rules ()
    ((_ var body)
     (let-syntax
         ((helper
           (syntax-rules ()
             ((_ vvar bbody) (lambda (vvar) bbody)))))
       (helper var body)))))

(test "passing by parameters" 5
      (lambda () ((mdm-bar-m3 z (+ z 1)) 4)))

;; Macro defining toplevel macros.
(define-syntax defMyQuote
  (syntax-rules ()
    ((_ name)
     (begin
       (define-syntax TEMP
         (syntax-rules ()
           ((_ arg)
            `arg)))
       (define-syntax name
         (syntax-rules ()
           ((_ arg)
            (TEMP arg))))))))

(defMyQuote MyQuote)

(test "macro defining a toplevel macro" '(1 2 3)
      (lambda () (MyQuote (1 2 3))))

;; Macro inserting toplevel identifier
(define-module defFoo-test
  (export defFoo)
  (define-syntax defFoo
    (syntax-rules ()
      [(_ accessor)
       (begin
         (define foo-toplevel 42)
         (define (accessor) foo-toplevel))])))

(import defFoo-test)
(defFoo get-foo)

(test "macro injecting toplevel definition" '(#f #f 42)
      (lambda ()
        (list (global-variable-ref (current-module) 'foo-toplevel #f)
              (global-variable-ref (find-module 'defFoo-test) 'foo-toplevel #f)
              (get-foo))))

;;----------------------------------------------------------------------
;; identifier comparison

;; This is EXPERIMENTAL: may be changed in later release.

(define-syntax hoge (syntax-rules () ((hoge foo ...) (cdr b))))
(test "comparison of identifiers" '(cdr b)
      (lambda () (macroexpand '(hoge bar))))
(test "comparison of identifiers" (macroexpand '(hoge bar))
      (lambda () (macroexpand '(hoge bar))))

;;----------------------------------------------------------------------
;; common-macros

(test-section "common-macros utilities")

(test "push!" '(1 2 3)
      (lambda ()
        (let ((a '()))
          (push! a 3) (push! a 2) (push! a 1)
          a)))

(test "push!" '(0 1 2 3)
      (lambda ()
        (let ((a (list 0)))
          (push! (cdr a) 3) (push! (cdr a) 2) (push! (cdr a) 1)
          a)))

(test "push!" '#((1 2) (3 . 0))
      (lambda ()
        (let ((a (vector '() 0)))
          (push! (vector-ref a 0) 2)
          (push! (vector-ref a 0) 1)
          (push! (vector-ref a 1) 3)
          a)))

(test "pop!" '((2 3) . 1)
      (lambda ()
        (let* ((a (list 1 2 3))
               (b (pop! a)))
          (cons a b))))

(test "pop!" '((1 3) . 2)
      (lambda ()
        (let* ((a (list 1 2 3))
               (b (pop! (cdr a))))
          (cons a b))))

(test "pop!" '(#((2)) . 1)
      (lambda ()
        (let* ((a (vector (list 1 2)))
               (b (pop! (vector-ref a 0))))
          (cons a b))))

(test "push!, pop!" '((2 3) (4 1))
      (lambda ()
        (let ((a (list 1 2 3))
              (b (list 4)))
          (push! (cdr b) (pop! a))
          (list a b))))

(test "inc!" 3
      (lambda () (let ((x 2)) (inc! x) x)))
(test "inc!" 4
      (lambda () (let ((x 2)) (inc! x 2) x)))
(test "inc!" '(4 . 1)
      (lambda ()
        (let ((x (cons 3 1)))
          (inc! (car x)) x)))
(test "inc!" '(1 . 1)
      (lambda ()
        (let ((x (cons 3 1)))
          (inc! (car x) -2) x)))
(test "inc!" '((4 . 1) 1)
      (lambda ()
        (let ((x (cons 3 1))
              (y 0))
          (define (zz) (inc! y) car)
          (inc! ((zz) x))
          (list x y))))
(test "dec!" 1
      (lambda () (let ((x 2)) (dec! x) x)))
(test "dec!" 0
      (lambda () (let ((x 2)) (dec! x 2) x)))
(test "dec!" '(2 . 1)
      (lambda ()
        (let ((x (cons 3 1)))
          (dec! (car x)) x)))
(test "dec!" '(5 . 1)
      (lambda ()
        (let ((x (cons 3 1)))
          (dec! (car x) -2) x)))
(test "dec!" '((2 . 1) -1)
      (lambda ()
        (let ((x (cons 3 1))
              (y 0))
          (define (zz) (dec! y) car)
          (dec! ((zz) x))
          (list x y))))

(test "dotimes" '(0 1 2 3 4 5 6 7 8 9)
      (lambda ()
        (let ((m '()))
          (dotimes (n 10) (push! m n))
          (reverse m))))
(test "dotimes" '(0 1 2 3 4 5 6 7 8 9)
      (lambda ()
        (let ((m '()))
          (dotimes (n 10 (reverse m)) (push! m n)))))
(test "dotimes" '(0 1 2 3 4 5 6 7 8 9)
      (lambda ()
        (let ((m '()))
          (dotimes (n (if (null? m) 10 (error "Boom!")) (reverse m))
                   (push! m n)))))

(test "while" 9
      (lambda ()
        (let ((a 10)
              (b 0))
          (while (positive? (dec! a))
            (inc! b))
          b)))
(test "while" 0
      (lambda ()
        (let ((a -1)
              (b 0))
          (while (positive? (dec! a))
            (inc! b))
          b)))

(test "while =>" 6
      (lambda ()
        (let ((a '(1 2 3 #f))
              (b 0))
          (while (pop! a)
            => val
            (inc! b val))
          b)))

(test "while => guard" 45
      (lambda ()
        (let ((a 10)
              (b 0))
          (while (dec! a)
            positive? => val
            (inc! b a))
          b)))

(test "until" 10
      (lambda ()
        (let ((a 10) (b 0))
          (until (negative? (dec! a))
            (inc! b))
          b)))
(test "until => guard" 45
      (lambda ()
        (let ((a 10) (b 0))
          (until (dec! a)
            negative? => val
            (inc! b a))
          b)))

(test "values-ref" 3
      (lambda ()
        (values-ref (quotient&remainder 10 3) 0)))
(test "values-ref" 1
      (lambda ()
        (values-ref (quotient&remainder 10 3) 1)))
(test "values-ref" 'e
      (lambda ()
        (values-ref (values 'a 'b 'c 'd 'e) 4)))
(test "values-ref" '(d b)
      (lambda ()
        (receive r
            (values-ref (values 'a 'b 'c 'd 'e) 3 1)
          r)))
(test "values-ref" '(d a b)
      (lambda ()
        (receive r
            (values-ref (values 'a 'b 'c 'd 'e) 3 0 1)
          r)))
(test "values-ref" '(e d c b a)
      (lambda ()
        (receive r
            (values-ref (values 'a 'b 'c 'd 'e) 4 3 2 1 0)
          r)))

(test "values->list" '(3 1)
      (lambda () (values->list (quotient&remainder 10 3))))
(test "values->list" '(1)
      (lambda () (values->list 1)))
(test "values->list" '()
      (lambda () (values->list (values))))

(test "let1" '(2 2 2)
      (lambda () (let1 x (+ 1 1) (list x x x))))
(test "let1" '(2 4)
      (lambda () (let1 x (+ 1 1) (list x (let1 x (+ x x) x)))))

(test "rlet1" 1 (lambda () (rlet1 x (/ 2 2) (+ x x))))

(test "if-let1" 4
      (lambda () (if-let1 it (+ 1 1) (* it 2))))
(test "if-let1" 'bar
      (lambda () (if-let1 it (memq 'a '(b c d)) 'boo 'bar)))

(test "ecase" 'b
      (lambda () (ecase 3 ((1) 'a) ((2 3) 'b) ((4) 'c))))
(test "ecase" (test-error)
      (lambda () (ecase 5 ((1) 'a) ((2 3) 'b) ((4) 'c))))
(test "ecase" 'd
      (lambda () (ecase 5 ((1) 'a) ((2 3) 'b) ((4) 'c) (else 'd))))

(test "$" '(0 1)
      (lambda () ($ list 0 1)))
(test "$" '(0 1 (2 3 (4 5 (6 7))))
      (lambda () ($ list 0 1 $ list 2 3 $ list 4 5 $ list 6 7)))
(test "$ - $*" '(0 1 (2 3 4 5 6 7))
      (lambda () ($ list 0 1 $ list 2 3 $* list 4 5 $* list 6 7)))
(test "$ - $*" '(0 1 2 3 (4 5 6 7))
      (lambda () ($ list 0 1 $* list 2 3 $ list 4 5 $* list 6 7)))
(test "$ - $*" '(0 1 2 3 4 5 (6 7))
      (lambda () ($ list 0 1 $* list 2 3 $* list 4 5 $ list 6 7)))
(test "$ - partial" '(0 1 (2 3 (4 5 a)))
      (lambda () (($ list 0 1 $ list 2 3 $ list 4 5 $) 'a)))
(test "$ - $* - partial" '(0 1 2 3 4 5 a)
      (lambda () (($ list 0 1 $* list 2 3 $* list 4 5 $) 'a)))
(test "$ - $* - partial" '(0 1 (2 3 (4 5 a b)))
      (lambda () (($ list 0 1 $ list 2 3 $ list 4 5 $*) 'a 'b)))

(test "$ - hygienty" `(0 1 a ,list 2 3 b ,list 4 5)
      (lambda ()
        (let-syntax ([$$ (syntax-rules ()
                           [($$ . xs) ($ . xs)])])
          (let ([$ 'a] [$* 'b])
            ($$ list 0 1 $ list 2 3 $* list 4 5)))))

(test* "cond-list" '() (cond-list))
(test* "cond-list" '(a) (cond-list ('a)))
(test* "cond-list" '(a) (cond-list (#t 'a) (#f 'b)))
(test* "cond-list" '(b) (cond-list (#f 'a) (#t 'b)))
(test* "cond-list" '(a b d) (cond-list (#t 'a) (#t 'b) (#f 'c) (#t 'd)))
(test* "cond-list" '((b)) (cond-list (#f 'a) ('b => list)))
(test* "cond-list" '(a b c d x)
       (cond-list (#t @ '(a b)) (#t @ '(c d)) (#f @ '(e f))
                  ('x => @ list)))

;;----------------------------------------------------------------------
;; macro-expand

(test-section "macroexpand")

(define-macro (foo x)   `(bar ,x ,x))
(define-macro (bar x y) `(list ,x ,x ,y ,y))

(test "macroexpand" '(list 1 1 1 1)
      (lambda () (macroexpand '(foo 1))))
(test "macroexpand-1" '(bar 1 1)
      (lambda () (macroexpand-1 '(foo 1))))

;;----------------------------------------------------------------------
;; not allowing first-class macro

(test-section "failure cases")

(define-macro (bad-if a b c) `(,if ,a ,b ,c))
(test "reject first-class syntax usage" (test-error)
      (lambda () (bad-if #t 'a 'b)))

(define-macro (bad-fi a b c) `(,fi ,a ,b ,c))
(test "reject first-class macro usage" (test-error)
      (lambda () (bad-fi #t 'a 'b)))

(test-end)
