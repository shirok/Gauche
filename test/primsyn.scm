;;;
;;; primitive syntax test
;;;

(define (section msg)
  (format #t "<~a>-------------------------------------------------\n" msg))

(define (title msg expect)
  (format #t "test ~a, expects ~s ==> " msg expect))
(define (run result)
  (write result) (newline))

(define *undef* (when #f #t))

;;----------------------------------------------------------------
(section "contitionals")

(title "if" 5)  (run (if #f 2 5))
(title "if" 2)  (run (if (not #f) 2 5))

(title "and" #t) (run (and))
(title "and" 5)  (run (and 5))
(title "and" #f) (run (and 5 #f 2))
(title "and" #f) (run (and 5 #f unbound-var))
(title "and" 'a) (run (and 3 4 'a))

(title "or"  #f) (run (or))
(title "or"  3)  (run (or 3 9))
(title "or"  3)  (run (or #f 3 unbound-var))

(title "when" 4)          (run (when 3 5 4))
(title "when" *undef*)    (run (when #f 5 4))
(title "unless" *undef*)  (run (unless 3 5 4))
(title "unless" 4)        (run (unless #f 5 4))

(title "cond" *undef*)  (run (cond (#f 2)))
(title "cond" 5)        (run (cond (#f 2) (else 5)))
(title "cond" 2)        (run (cond (1 2) (else 5)))
(title "cond" 8)        (run (cond (#f 2) (1 8) (else 5)))
(title "cond" 3)        (run (cond (1 => (lambda (x) (+ x 2))) (else 8)))

(title "case" #t)       (run (case (+ 2 3) ((1 3 5 7 9) #t) ((0 2 4 6 8) #f)))

;;----------------------------------------------------------------
(section "closure and saved env")

(title "lambda" 5)  (run ((lambda (x) (car x)) '(5 6 7)))
(title "lambda" 12) (run ((lambda (x y) ((lambda (z) (* (car z) (cdr z))) (cons x y))) 3 4))

(define (addN n) (lambda (a) (+ a n)))
(title "lambda" 5)  (run ((addN 2) 3))
(define add3 (addN 3))
(title "lambda" 9)  (run (add3 6))

(define count (let ((c 0)) (lambda () (set! c (+ c 1)) c)))
(title "lambda" 1)  (run (count))
(title "lambda" 2)  (run (count))

;;----------------------------------------------------------------
(section "application")

(title "apply" '(1 2 3))  (run (apply list 1 '(2 3)))
(title "apply" '(1 2 3))  (run (apply apply (list list 1 2 '(3))))

(title "map" '())         (run (map car '()))
(title "map" '(1 2 3))    (run (map car '((1) (2) (3))))
(title "map" '(() () ())) (run (map cdr '((1) (2) (3))))
(title "map" '((1 . 4) (2 . 5) (3 . 6)))  (run (map cons '(1 2 3) '(4 5 6)))

;;----------------------------------------------------------------
(section "loop")

(define (fact-non-tail-rec n)
  (if (<= n 1) n (* n (fact-non-tail-rec (- n 1)))))
(title "loop non-tail-rec" 120) (run (fact-non-tail-rec 5))

(define (fact-tail-rec n r)
  (if (<= n 1) r (fact-tail-rec (- n 1) (* n r))))
(title "loop tail-rec"     120) (run (fact-tail-rec 5 1))

(define (fact-named-let n)
  (let loop ((n n) (r 1)) (if (<= n 1) r (loop (- n 1) (* n r)))))
(title "loop named-let"    120) (run (fact-named-let 5))

(define (fact-int-define n)
  (define (rec n r) (if (<= n 1) r (rec (- n 1) (* n r))))
  (rec n 1))
(title "loop int-define"   120) (run (fact-int-define 5))

(define (fact-do n)
  (do ((n n (- n 1)) (r 1 (* n r))) ((<= n 1) r)))
(title "loop do"           120) (run (fact-do 5))

;;----------------------------------------------------------------
(section "quasiquote")

(title "qq" '(1 2 3))       (run `(1 2 3))
(title "qq" '())            (run `())
(title "qq" '((1 . 2)))     (run `(,(cons 1 2)))
(title "qq" '((1 . 2) 3))   (run `(,(cons 1 2) 3))
(title "qq" '(1 2 3 4))     (run `(1 ,@(list 2 3) 4))
(title "qq" '(1 2 3 4))     (run `(1 2 ,@(list 3 4)))
(title "qq" '(1 2 3 4))     (run `(1 2 . ,(list 3 4)))
(title "qq" '#((1 . 2) 3))  (run `#(,(cons 1 2) 3))
(title "qq" '#(1 2 3 4))    (run `#(1 ,@(list 2 3) 4))
(title "qq" '#(1 2 3 4))    (run `#(1 2 ,@(list 3 4)))
(title "qq" '#())           (run `#())
(title "qq" '#())           (run `#(,@(list)))

;;----------------------------------------------------------------
(section "multiple values")

(title "receive" '(1 2 3))
(run (receive (a b c) (values 1 2 3) (list a b c)))
(title "receive" '(1 2 3))
(run (receive (a . r) (values 1 2 3) (cons a r)))
(title "receive" '(1 2 3))
(run (receive x (values 1 2 3) x))
(title "receive" 1)
(run (receive (a) 1 a))
(title "call-with-values" '(1 2 3))
(run (call-with-values (lambda () (values 1 2 3)) list))
(title "call-with-values" '())
(run (call-with-values (lambda () (values)) list))
