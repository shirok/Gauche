;;;
;;; primitive syntax test
;;;

(use gauche.test)

(test-start "primitive syntax")

;;----------------------------------------------------------------
(test-section "contitionals")

(test "if" 5 (lambda ()  (if #f 2 5)))
(test "if" 2 (lambda ()  (if (not #f) 2 5)))

(test "and" #t (lambda ()  (and)))
(test "and" 5  (lambda ()  (and 5)))
(test "and" #f (lambda ()  (and 5 #f 2)))
(test "and" #f (lambda ()  (and 5 #f unbound-var)))
(test "and" 'a (lambda ()  (and 3 4 'a)))

(test "or"  #f (lambda ()  (or)))
(test "or"  3  (lambda ()  (or 3 9)))
(test "or"  3  (lambda ()  (or #f 3 unbound-var)))

(test "when" 4          (lambda ()  (when 3 5 4)))
(test "when" (test-undef)    (lambda ()  (when #f 5 4)))
(test "unless" (test-undef)  (lambda ()  (unless 3 5 4)))
(test "unless" 4        (lambda ()  (unless #f 5 4)))

(test "cond" (test-undef)  (lambda ()  (cond (#f 2))))
(test "cond" 5        (lambda ()  (cond (#f 2) (else 5))))
(test "cond" 2        (lambda ()  (cond (1 2) (else 5))))
(test "cond" 8        (lambda ()  (cond (#f 2) (1 8) (else 5))))
(test "cond" 3        (lambda ()  (cond (1 => (lambda (x) (+ x 2))) (else 8))))

(test "case" #t (lambda ()  (case (+ 2 3) ((1 3 5 7 9) #t) ((0 2 4 6 8) #f))))
(test "case" #t (lambda () (undefined? (case 1 ((2 3) #t)))))

;;----------------------------------------------------------------
(test-section "binding")

(test "let" 35
      (lambda ()
        (let ((x 2) (y 3))
          (let ((x 7) (z (+ x y)))
            (* z x)))))
(test "let*" 70
      (lambda ()
        (let ((x 2) (y 3))
          (let* ((x 7) (z (+ x y)))
            (* z x)))))
(test "let*" 2
      (lambda ()
        (let* ((x 1) (x (+ x 1))) x)))

(test "named let" -3
      (lambda ()
        (let ((f -))
          (let f ((a (f 3)))
            a))))

;;----------------------------------------------------------------
(test-section "closure and saved env")

(test "lambda" 5  (lambda ()  ((lambda (x) (car x)) '(5 6 7))))
(test "lambda" 12
      (lambda ()
        ((lambda (x y)
           ((lambda (z) (* (car z) (cdr z))) (cons x y))) 3 4)))

(define (addN n) (lambda (a) (+ a n)))
(test "lambda" 5 (lambda ()  ((addN 2) 3)))
(define add3 (addN 3))
(test "lambda" 9 (lambda ()  (add3 6)))

(define count (let ((c 0)) (lambda () (set! c (+ c 1)) c)))
(test "lambda" 1 (lambda ()  (count)))
(test "lambda" 2 (lambda ()  (count)))

;;----------------------------------------------------------------
(test-section "application")

(test "apply" '(1 2 3) (lambda ()  (apply list 1 '(2 3))))
(test "apply" '(1 2 3) (lambda ()  (apply apply (list list 1 2 '(3)))))

(test "map" '()         (lambda ()  (map car '())))
(test "map" '(1 2 3)    (lambda ()  (map car '((1) (2) (3)))))
(test "map" '(() () ()) (lambda ()  (map cdr '((1) (2) (3)))))
(test "map" '((1 . 4) (2 . 5) (3 . 6))  (lambda ()  (map cons '(1 2 3) '(4 5 6))))

;;----------------------------------------------------------------
(test-section "loop")

(define (fact-non-tail-rec n)
  (if (<= n 1) n (* n (fact-non-tail-rec (- n 1)))))
(test "loop non-tail-rec" 120 (lambda ()  (fact-non-tail-rec 5)))

(define (fact-tail-rec n r)
  (if (<= n 1) r (fact-tail-rec (- n 1) (* n r))))
(test "loop tail-rec"     120 (lambda ()  (fact-tail-rec 5 1)))

(define (fact-named-let n)
  (let loop ((n n) (r 1)) (if (<= n 1) r (loop (- n 1) (* n r)))))
(test "loop named-let"    120 (lambda ()  (fact-named-let 5)))

(define (fact-int-define n)
  (define (rec n r) (if (<= n 1) r (rec (- n 1) (* n r))))
  (rec n 1))
(test "loop int-define"   120 (lambda ()  (fact-int-define 5)))

(define (fact-do n)
  (do ((n n (- n 1)) (r 1 (* n r))) ((<= n 1) r)))
(test "loop do"           120 (lambda ()  (fact-do 5)))

;; tricky case
(test "do" #f (lambda () (do () (#t #f) #t)))

;;----------------------------------------------------------------
(test-section "quasiquote")

(test "qq" '(1 2 3)        (lambda ()  `(1 2 3)))
(test "qq" '()             (lambda ()  `()))
(test "qq," '((1 . 2))     (lambda ()  `(,(cons 1 2))))
(test "qq," '((1 . 2) 3)   (lambda ()  `(,(cons 1 2) 3)))
(test "qq@" '(1 2 3 4)     (lambda ()  `(1 ,@(list 2 3) 4)))
(test "qq@" '(1 2 3 4)     (lambda ()  `(1 2 ,@(list 3 4))))
(test "qq." '(1 2 3 4)     (lambda ()  `(1 2 . ,(list 3 4))))
(test "qq#," '#((1 . 2) 3) (lambda ()  `#(,(cons 1 2) 3)))
(test "qq#@" '#(1 2 3 4)   (lambda ()  `#(1 ,@(list 2 3) 4)))
(test "qq#@" '#(1 2 3 4)   (lambda ()  `#(1 2 ,@(list 3 4))))
(test "qq#" '#()           (lambda ()  `#()))
(test "qq#@" '#()          (lambda ()  `#(,@(list))))

(test "qq@@" '(1 2 1 2)    (lambda ()  `(,@(list 1 2) ,@(list 1 2))))
(test "qq@@" '(1 2 a 1 2)  (lambda ()  `(,@(list 1 2) a ,@(list 1 2))))
(test "qq@@" '(a 1 2 1 2)  (lambda ()  `(a ,@(list 1 2) ,@(list 1 2))))
(test "qq@@" '(1 2 1 2 a)  (lambda ()  `(,@(list 1 2) ,@(list 1 2) a)))
(test "qq@@" '(1 2 1 2 a b) (lambda ()  `(,@(list 1 2) ,@(list 1 2) a b)))
(test "qq@." '(1 2 1 2 . a)
      (lambda ()  `(,@(list 1 2) ,@(list 1 2) . a)))
(test "qq@." '(1 2 1 2 1 . 2)
      (lambda ()  `(,@(list 1 2) ,@(list 1 2) . ,(cons 1 2))))
(test "qq@." '(1 2 1 2 a 1 . 2)
      (lambda ()  `(,@(list 1 2) ,@(list 1 2) a . ,(cons 1 2))))

(test "qq#@@" '#(1 2 1 2)    (lambda ()  `#(,@(list 1 2) ,@(list 1 2))))
(test "qq#@@" '#(1 2 a 1 2)  (lambda ()  `#(,@(list 1 2) a ,@(list 1 2))))
(test "qq#@@" '#(a 1 2 1 2)  (lambda ()  `#(a ,@(list 1 2) ,@(list 1 2))))
(test "qq#@@" '#(1 2 1 2 a)  (lambda ()  `#(,@(list 1 2) ,@(list 1 2) a)))
(test "qq#@@" '#(1 2 1 2 a b) (lambda () `#(,@(list 1 2) ,@(list 1 2) a b)))

(test "qqq"   '(1 `(1 ,2 ,3) 1)  (lambda ()  `(1 `(1 ,2 ,,(+ 1 2)) 1)))
(test "qqq"   '(1 `(1 ,@2 ,@(1 2))) (lambda () `(1 `(1 ,@2 ,@,(list 1 2)))))
(test "qqq#"  '#(1 `(1 ,2 ,3) 1)  (lambda ()  `#(1 `(1 ,2 ,,(+ 1 2)) 1)))
(test "qqq#"  '#(1 `(1 ,@2 ,@(1 2))) (lambda () `#(1 `(1 ,@2 ,@,(list 1 2)))))

;;----------------------------------------------------------------
(test-section "multiple values")

(test "receive" '(1 2 3)
      (lambda ()  (receive (a b c) (values 1 2 3) (list a b c))))
(test "receive" '(1 2 3)
      (lambda ()  (receive (a . r) (values 1 2 3) (cons a r))))
(test "receive" '(1 2 3)
      (lambda ()  (receive x (values 1 2 3) x)))
(test "receive" 1
      (lambda ()  (receive (a) 1 a)))
(test "call-with-values" '(1 2 3)
      (lambda ()  (call-with-values (lambda () (values 1 2 3)) list)))
(test "call-with-values" '()
      (lambda ()  (call-with-values (lambda () (values)) list)))

;;----------------------------------------------------------------
(test-section "eval")

(test "eval" '(1 . 2)
      (lambda () (eval '(cons 1 2) (interaction-environment))))

(define (vector-ref x y) 'foo)

(test "eval" '(foo foo 3)
      (lambda ()
        (list (vector-ref '#(3) 0)
              (eval '(vector-ref '#(3) 0) (interaction-environment))
              (eval '(vector-ref '#(3) 0) (scheme-report-environment 5)))))

(define vector-ref (with-module scheme vector-ref))

(test "eval" #t
      (lambda ()
        (with-error-handler
         (lambda (e) #t)
         (lambda () (eval '(car '(3 2)) (null-environment 5))))))

;;----------------------------------------------------------------
(test-section "optimized frames")

;; Empty environment frame is omitted by compiler optimization.
;; The following tests makes sure if it works correctly.

(test "lambda (empty env)" 1
      (lambda ()
        (let* ((a 1)
               (b (lambda ()
                    ((lambda () a)))))
          (b))))

(test "let (empty env)" 1
      (lambda ()
        (let ((a 1))
          (let ()
            (let ()
              a)))))

(test "let (empty env)" '(1 . 1)
      (lambda ()
        (let ((a 1))
          (cons (let () (let () a))
                (let* () (letrec () a))))))

(test "let (empty env)" '(3 . 1)
      (lambda ()
        (let ((a 1)
              (b 0))
          (cons (let () (let () (set! b 3)) b)
                (let () (let () a))))))

(test "named let (empty env)" 1
      (lambda ()
        (let ((a -1))
          (let loop ()
            (unless (positive? a)
              (set! a (+ a 1))
              (loop)))
          a)))

(test "do (empty env)" 1
      (lambda () (let ((a 0)) (do () ((positive? a) a) (set! a (+ a 1))))))

(test-end)

