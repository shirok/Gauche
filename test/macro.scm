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
  `(test ,msg ',expect (lambda () (unident (%macro-expand ,form)))))

;;----------------------------------------------------------------------
;; basic tests
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
(test-macro "simple" (e z) (simple #(1.0 2.0) z))
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

;;----------------------------------------------------------------------
;; cond, taken from R5RS section 7.3
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
      (lambda () (let ((=> #f)) (unident (%macro-expand (%cond (a => b)))))))
(test "%cond" '(if else (begin z))
      (lambda () (let ((else #t)) (unident (%macro-expand (%cond (else z)))))))

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
;; local syntactic bindings.

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

(test-end)
