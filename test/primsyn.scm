;;;
;;; primitive syntax test
;;;

(use gauche.test)

(test-start "primitive syntax")

;; We use prim-test instead of test, for error-handler is not tested yet.

;;----------------------------------------------------------------
(test-section "conditionals")

(prim-test "if" 5 (lambda ()  (if #f 2 5)))
(prim-test "if" 2 (lambda ()  (if (not #f) 2 5)))

(prim-test "and" #t (lambda ()  (and)))
(prim-test "and" 5  (lambda ()  (and 5)))
(prim-test "and" #f (lambda ()  (and 5 #f 2)))
(prim-test "and" #f (lambda ()  (and 5 #f unbound-var)))
(prim-test "and" 'a (lambda ()  (and 3 4 'a)))

(prim-test "or"  #f (lambda ()  (or)))
(prim-test "or"  3  (lambda ()  (or 3 9)))
(prim-test "or"  3  (lambda ()  (or #f 3 unbound-var)))

(prim-test "when" 4          (lambda ()  (when 3 5 4)))
(prim-test "when" (undefined)    (lambda ()  (when #f 5 4)))
(prim-test "when" (undefined)    (lambda ()  (when #f unbound-var)))
(prim-test "unless" (undefined)  (lambda ()  (unless 3 5 4)))
(prim-test "unless" (undefined)  (lambda ()  (unless #t unbound-var)))
(prim-test "unless" 4        (lambda ()  (unless #f 5 4)))

(prim-test "cond" (undefined)  (lambda ()  (cond (#f 2))))
(prim-test "cond" 5        (lambda ()  (cond (#f 2) (else 5))))
(prim-test "cond" 2        (lambda ()  (cond (1 2) (else 5))))
(prim-test "cond" 8        (lambda ()  (cond (#f 2) (1 8) (else 5))))
(prim-test "cond" 3        (lambda ()  (cond (1 => (lambda (x) (+ x 2))) (else 8))))
(prim-test "cond (srfi-61)" 1 (lambda () (cond (1 number? => values) (else 8))))
(prim-test "cond (srfi-61)" 8 (lambda () (cond (1 string? => values) (else 8))))
(prim-test "cond (srfi-61)" '(1 2)
           (lambda () (cond ((values 1 2)
                             (lambda (x y) (and (= x 1) (= y 2)))
                             => list))))

(prim-test "case" #t (lambda ()  (case (+ 2 3) ((1 3 5 7 9) #t) ((0 2 4 6 8) #f))))
(prim-test "case" #t (lambda () (undefined? (case 1 ((2 3) #t)))))
(prim-test "case" #t (lambda () (case 1 (() #f) ((1) #t))))
(prim-test "case" #t (lambda () (case 1 (() #f) (else #t))))
(prim-test "case" #t (lambda () (undefined? (case 1 (() #t)))))
(prim-test "case (srfi-87)" 0 (lambda () (case (+ 2 3) ((1 3 5) 0) (else => values))))
(prim-test "case (srfi-87)" 6 (lambda () (case (+ 2 3) ((1 3 5) => (cut + 1 <>)) (else => values))))
(prim-test "case (srfi-87)" 5 (lambda () (case (+ 2 3) ((2 4 6) 0) (else => values))))

;;----------------------------------------------------------------
(test-section "binding")

(prim-test "let" 35
      (lambda ()
        (let ((x 2) (y 3))
          (let ((x 7) (z (+ x y)))
            (* z x)))))
(prim-test "let*" 70
      (lambda ()
        (let ((x 2) (y 3))
          (let* ((x 7) (z (+ x y)))
            (* z x)))))
(prim-test "let*" 2
      (lambda ()
        (let* ((x 1) (x (+ x 1))) x)))

(prim-test "named let" -3
      (lambda ()
        (let ((f -))
          (let f ((a (f 3)))
            a))))

;;----------------------------------------------------------------
(test-section "closure and saved env")

(prim-test "lambda" 5  (lambda ()  ((lambda (x) (car x)) '(5 6 7))))
(prim-test "lambda" 12
      (lambda ()
        ((lambda (x y)
           ((lambda (z) (* (car z) (cdr z))) (cons x y))) 3 4)))

(define (addN n) (lambda (a) (+ a n)))
(prim-test "lambda" 5 (lambda ()  ((addN 2) 3)))
(define add3 (addN 3))
(prim-test "lambda" 9 (lambda ()  (add3 6)))

(define count (let ((c 0)) (lambda () (set! c (+ c 1)) c)))
(prim-test "lambda" 1 (lambda ()  (count)))
(prim-test "lambda" 2 (lambda ()  (count)))

;;----------------------------------------------------------------
(test-section "application")

(prim-test "apply" '(1 2 3) (lambda ()  (apply list '(1 2 3))))
(prim-test "apply" '(2 3 4) (lambda ()  (apply list 2 '(3 4))))
(prim-test "apply" '(3 4 5) (lambda ()  (apply list 3 4 '(5))))
(prim-test "apply" '(4 5 6) (lambda ()  (apply list 4 5 6 '())))

(prim-test "apply^2" '() (lambda () (apply apply list '() '())))
(prim-test "apply^2" '() (lambda () (apply apply list '(()))))
(prim-test "apply^2" '(1 . 2) (lambda () (apply apply cons '((1 2)))))
(prim-test "apply^2" '(3 . 4) (lambda () (apply apply cons 3 '((4)))))
(prim-test "apply^2" '(5 . 6) (lambda () (apply apply (list cons 5 '(6)))))
                                          

(prim-test "apply" '(6 7 8) (lambda ()  (apply apply (list list 6 7 '(8)))))


;; This tests 'unfolding' path in ADJUST_ARGUMENT_FRAME.
(prim-test "apply, copying args" '(1 2 3)
           (lambda ()
             (let ((orig (list 1 2 3)))
               (let ((new (apply list orig)))
                 (set-car! (cdr new) '100)
                 orig))))

;; This tests 'folding' path in ADJUST_ARGUMENT_FRAME
(prim-test "apply, copying args" '(2 3)
           (lambda ()
             (let ((orig (list 2 3)))
               (let ((new (apply list 1 orig)))
                 (set-car! (cdr new) '100)
                 orig))))

;; This test exhibits the optimizer bug reported by Michael Campbell.
(define bug-optimizer-local-inliner
  (lambda (flag)
    (define (a . args)
      (receive x args
        (cons x x)
        (apply values x))
      (apply format args))
    (define (b bar)
      (a "~a" bar))
    (b 1)
    (cond
     (flag (b 1))
     (else (a "~a" 1)))))
(prim-test "apply local inliner optimizer" "1"
           (lambda () (bug-optimizer-local-inliner #f)) equal?)
(prim-test "apply local inliner optimizer" "1"
           (lambda () (bug-optimizer-local-inliner #t)) equal?)

(prim-test "map" '()         (lambda ()  (map car '())))
(prim-test "map" '(1 2 3)    (lambda ()  (map car '((1) (2) (3)))))
(prim-test "map" '(() () ()) (lambda ()  (map cdr '((1) (2) (3)))))
(prim-test "map" '((1 . 4) (2 . 5) (3 . 6))  (lambda ()  (map cons '(1 2 3) '(4 5 6))))

;;----------------------------------------------------------------
(test-section "loop")

(define (fact-non-tail-rec n)
  (if (<= n 1) n (* n (fact-non-tail-rec (- n 1)))))
(prim-test "loop non-tail-rec" 120 (lambda ()  (fact-non-tail-rec 5)))

(define (fact-tail-rec n r)
  (if (<= n 1) r (fact-tail-rec (- n 1) (* n r))))
(prim-test "loop tail-rec"     120 (lambda ()  (fact-tail-rec 5 1)))

(define (fact-named-let n)
  (let loop ((n n) (r 1)) (if (<= n 1) r (loop (- n 1) (* n r)))))
(prim-test "loop named-let"    120 (lambda ()  (fact-named-let 5)))

(define (fact-int-define n)
  (define (rec n r) (if (<= n 1) r (rec (- n 1) (* n r))))
  (rec n 1))
(prim-test "loop int-define"   120 (lambda ()  (fact-int-define 5)))

(define (fact-do n)
  (do ((n n (- n 1)) (r 1 (* n r))) ((<= n 1) r)))
(prim-test "loop do"           120 (lambda ()  (fact-do 5)))

;; tricky case
(prim-test "do" #f (lambda () (do () (#t #f) #t)))

;;----------------------------------------------------------------
(test-section "quasiquote")

;; The new compiler generates constant list for much wider
;; range of quasiquoted forms (e.g. constant numerical expressions
;; and constant variable definitions are folded at the compile time).

(define-constant quasi0 99)
(define quasi1 101)
(define-constant quasi2 '(a b))
(define quasi3 '(c d))

(prim-test "qq" '(1 2 3)        (lambda ()  `(1 2 3)))
(prim-test "qq" '()             (lambda ()  `()))
(prim-test "qq"  99             (lambda ()  `,quasi0))
(prim-test "qq"  101            (lambda ()  `,quasi1))
(prim-test "qq," '((1 . 2))     (lambda ()  `(,(cons 1 2))))
(prim-test "qq," '((1 . 2) 3)   (lambda ()  `(,(cons 1 2) 3)))
(prim-test "qq," '(0 (1 . 2))   (lambda ()  `(0 ,(cons 1 2))))
(prim-test "qq," '(0 (1 . 2) 3) (lambda ()  `(0 ,(cons 1 2) 3)))
(prim-test "qq," '(((1 . 2)))   (lambda ()  `((,(cons 1 2)))))
(prim-test "qq," '(((1 . 2)) 3) (lambda ()  `((,(cons 1 2)) 3)))
(prim-test "qq," '(99 3)        (lambda ()  `(,quasi0 3)))
(prim-test "qq," '(3 99)        (lambda ()  `(3 ,quasi0)))
(prim-test "qq," '(3 99 3)      (lambda ()  `(3 ,quasi0 3)))
(prim-test "qq," '(100 3)       (lambda ()  `(,(+ quasi0 1) 3)))
(prim-test "qq," '(3 100)       (lambda ()  `(3 ,(+ quasi0 1))))
(prim-test "qq," '(101 3)       (lambda ()  `(,quasi1 3)))
(prim-test "qq," '(3 101)       (lambda ()  `(3 ,quasi1)))
(prim-test "qq," '(102 3)       (lambda ()  `(,(+ quasi1 1) 3)))
(prim-test "qq," '(3 102)       (lambda ()  `(3 ,(+ quasi1 1))))
(prim-test "qq,(r6rs)" '(98 99 (a b) 100)
           (lambda () `(98 (unquote quasi0 quasi2) 100)))
(prim-test "qq,(r6rs)" '(98 99 101 100)
           (lambda () `(98 (unquote quasi0 quasi1) 100)))
(prim-test "qq,(r6rs)" '(98 99 (a b) 100)
           (lambda () `(98 (unquote quasi0 quasi2) 100)))
(prim-test "qq,(r6rs)" '(98 99 (a b) (1 2) (3 4))
           (lambda () `(98 (unquote quasi0 quasi2) (unquote (list 1 2) (list 3 4)))))
(prim-test "qq@" '(1 2 3 4)     (lambda ()  `(1 ,@(list 2 3) 4)))
(prim-test "qq@" '(1 2 3 4)     (lambda ()  `(1 2 ,@(list 3 4))))
(prim-test "qq@" '(a b c d)     (lambda ()  `(,@quasi2 ,@quasi3)))
(prim-test "qq@(r6rs)" '(1 a b a b 2)
           (lambda () `(1 (unquote-splicing quasi2 quasi2) 2)))
(prim-test "qq@(r6rs)" '(1 a b c d 2)
           (lambda () `(1 (unquote-splicing quasi2 quasi3) 2)))
(prim-test "qq@(r6rs)" '(1 a b c d 2)
           (lambda () `(1 (unquote-splicing (list 'a 'b) '(c d)) ,@(list 2))))
(prim-test "qq." '(1 2 3 4)     (lambda ()  `(1 2 . ,(list 3 4))))
(prim-test "qq." '(a b c d)     (lambda ()  `(,@quasi2 . ,quasi3)))
(prim-test "qq#," '#((1 . 2) 3) (lambda ()  `#(,(cons 1 2) 3)))
(prim-test "qq#," '#(99 3)      (lambda ()  `#(,quasi0 3)))
(prim-test "qq#," '#(100 3)     (lambda ()  `#(,(+ quasi0 1) 3)))
(prim-test "qq#," '#(3 101)     (lambda ()  `#(3 ,quasi1)))
(prim-test "qq#," '#(3 102)     (lambda ()  `#(3 ,(+ quasi1 1))))
(prim-test "qq#@" '#(1 2 3 4)   (lambda ()  `#(1 ,@(list 2 3) 4)))
(prim-test "qq#@" '#(1 2 3 4)   (lambda ()  `#(1 2 ,@(list 3 4))))
(prim-test "qq#@" '#(a b c d)   (lambda ()  `#(,@quasi2 ,@quasi3)))
(prim-test "qq#@" '#(a b (c d)) (lambda ()  `#(,@quasi2 ,quasi3)))
(prim-test "qq#@" '#((a b) c d) (lambda ()  `#(,quasi2  ,@quasi3)))
(prim-test "qq#"  '#()          (lambda ()  `#()))
(prim-test "qq#@" '#()          (lambda ()  `#(,@(list))))

(prim-test "qq@@" '(1 2 1 2)    (lambda ()  `(,@(list 1 2) ,@(list 1 2))))
(prim-test "qq@@" '(1 2 a 1 2)  (lambda ()  `(,@(list 1 2) a ,@(list 1 2))))
(prim-test "qq@@" '(a 1 2 1 2)  (lambda ()  `(a ,@(list 1 2) ,@(list 1 2))))
(prim-test "qq@@" '(1 2 1 2 a)  (lambda ()  `(,@(list 1 2) ,@(list 1 2) a)))
(prim-test "qq@@" '(1 2 1 2 a b) (lambda ()  `(,@(list 1 2) ,@(list 1 2) a b)))
(prim-test "qq@." '(1 2 1 2 . a)
      (lambda ()  `(,@(list 1 2) ,@(list 1 2) . a)))
(prim-test "qq@." '(1 2 1 2 1 . 2)
      (lambda ()  `(,@(list 1 2) ,@(list 1 2) . ,(cons 1 2))))
(prim-test "qq@." '(1 2 1 2 a b)
      (lambda ()  `(,@(list 1 2) ,@(list 1 2) . ,quasi2)))
(prim-test "qq@." '(1 2 1 2 a 1 . 2)
      (lambda ()  `(,@(list 1 2) ,@(list 1 2) a . ,(cons 1 2))))
(prim-test "qq@." '(1 2 1 2 a c d)
      (lambda ()  `(,@(list 1 2) ,@(list 1 2) a . ,quasi3)))

(prim-test "qq#@@" '#(1 2 1 2)    (lambda ()  `#(,@(list 1 2) ,@(list 1 2))))
(prim-test "qq#@@" '#(1 2 a 1 2)  (lambda ()  `#(,@(list 1 2) a ,@(list 1 2))))
(prim-test "qq#@@" '#(a 1 2 1 2)  (lambda ()  `#(a ,@(list 1 2) ,@(list 1 2))))
(prim-test "qq#@@" '#(1 2 1 2 a)  (lambda ()  `#(,@(list 1 2) ,@(list 1 2) a)))
(prim-test "qq#@@" '#(1 2 1 2 a b) (lambda () `#(,@(list 1 2) ,@(list 1 2) a b)))

(prim-test "qqq"   '(1 `(1 ,2 ,3) 1)
           (lambda ()  `(1 `(1 ,2 ,,(+ 1 2)) 1)))
(prim-test "qqq"   '(1 `(1 ,99 ,101) 1)
           (lambda ()  `(1 `(1 ,,quasi0 ,,quasi1) 1)))
(prim-test "qqq"   '(1 `(1 ,@2 ,@(1 2)))
           (lambda () `(1 `(1 ,@2 ,@,(list 1 2)))))
(prim-test "qqq"   '(1 `(1 ,@2 (unquote 1 2)))
           (lambda () `(1 `(1 ,@2 ,,@(list 1 2)))))
(prim-test "qqq"   '(1 `(1 ,@2 (unquote-splicing 1 2)))
           (lambda () `(1 `(1 ,@2 ,@,@(list 1 2)))))
(prim-test "qqq"   '(1 `(1 ,@(a b) ,@(c d)))
           (lambda () `(1 `(1 ,@,quasi2 ,@,quasi3))))
(prim-test "qqq"   '(1 `(1 ,(a b x) ,(y c d)))
           (lambda () `(1 `(1 ,(,@quasi2 x) ,(y ,@quasi3)))))
(prim-test "qqq#"  '#(1 `(1 ,2 ,3) 1)
           (lambda ()  `#(1 `(1 ,2 ,,(+ 1 2)) 1)))
(prim-test "qqq#"  '#(1 `(1 ,99 ,101) 1)
           (lambda ()  `#(1 `(1 ,,quasi0 ,,quasi1) 1)))
(prim-test "qqq#"  '#(1 `(1 ,@2 ,@(1 2)))
           (lambda () `#(1 `(1 ,@2 ,@,(list 1 2)))))
(prim-test "qqq#"  '#(1 `(1 ,@(a b) ,@(c d)))
           (lambda () `#(1 `(1 ,@,quasi2 ,@,quasi3))))
(prim-test "qqq#"  '#(1 `(1 ,(a b x) ,(y c d)))
           (lambda () `#(1 `(1 ,(,@quasi2 x) ,(y ,@quasi3)))))
(prim-test "qqq#"  '(1 `#(1 ,(a b x) ,(y c d)))
           (lambda () `(1 `#(1 ,(,@quasi2 x) ,(y ,@quasi3)))))

(prim-test "qq-hygiene 0" '(2 1)
           (lambda () (let ((quasiquote reverse)) `(list 1 2))))
(prim-test "qq-hygiene 1" '(,(+ 1 2))
           (lambda () (let ((unquote 3)) `(,(+ 1 2)))))
(prim-test "qq-hygiene 2" '(,@(+ 1 2))
           (lambda () (let ((unquote-splicing 3)) `(,@(+ 1 2)))))

;;----------------------------------------------------------------
(test-section "multiple values")

(prim-test "receive" '(1 2 3)
      (lambda ()  (receive (a b c) (values 1 2 3) (list a b c))))
(prim-test "receive" '(1 2 3)
      (lambda ()  (receive (a . r) (values 1 2 3) (cons a r))))
(prim-test "receive" '(1 2 3)
      (lambda ()  (receive x (values 1 2 3) x)))
(prim-test "receive" 1
      (lambda ()  (receive (a) 1 a)))
(prim-test "call-with-values" '(1 2 3)
      (lambda ()  (call-with-values (lambda () (values 1 2 3)) list)))
(prim-test "call-with-values" '()
      (lambda ()  (call-with-values (lambda () (values)) list)))

;; This is not 'right' in R5RS sense---for now, I just tolerate it
;; by CommonLisp way, i.e. if more than one value is passed to an
;; implicit continuation that expects one value, the second and after
;; values are just discarded.  This behavior may be changed later,
;; so do not count on it.   The test just make sure it doesn't screw
;; up anything.
(prim-test "receive" '((0 0))
      (lambda ()  (receive l (list 0 (values 0 1 2)) l)))

;;----------------------------------------------------------------
(test-section "eval")

(prim-test "eval" '(1 . 2)
      (lambda () (eval '(cons 1 2) (interaction-environment))))

(define (vector-ref x y) 'foo)

(prim-test "eval" '(foo foo 3)
      (lambda ()
        (list (vector-ref '#(3) 0)
              (eval '(vector-ref '#(3) 0) (interaction-environment))
              (eval '(vector-ref '#(3) 0) (scheme-report-environment 5)))))

(define vector-ref (with-module scheme vector-ref))

(prim-test "eval" #t
      (lambda ()
        (with-error-handler
         (lambda (e) #t)
         (lambda () (eval '(car '(3 2)) (null-environment 5))))))

;; check interaction w/ modules
(define-module primsyn.test (define foo 'a))
(define foo '(x y))

(prim-test "eval (module)" '(a b (x y))
      (lambda ()
        (let* ((m (find-module 'primsyn.test))
               (a (eval 'foo m))
               (b (eval '(begin (set! foo 'b) foo) m)))
          (list a b foo))))

(prim-test "eval (module)" '(x y)
      (lambda ()
        (with-error-handler
            (lambda (e) foo)
          (lambda ()
            (eval '(apply car foo '()) (find-module 'primsyn.test))))))

;;----------------------------------------------------------------
(test-section "max literal arguments")

;; Fix this after we have separate compile-error condition.
(define (test-max-literal-args msg expr)
  (prim-test (string-append "max literal arguments for " msg)
             'caught 
             (lambda ()
               (with-error-handler (lambda (e) 'caught)
                 (lambda () (eval expr (interaction-environment)))))))

(test-max-literal-args "inliner" `(list ,@(make-list 10000 #f)))
(test-max-literal-args "global proc" `(make ,@(make-list 10000 #f)))
(test-max-literal-args "local proc"
                       `(let ((foo (lambda x x)))
                          (foo ,@(make-list 10000 #f))))

;;----------------------------------------------------------------
(test-section "local procedure optimization")

;; this caused an internal compiler error in 0.8.6.
;; (found and fixed by Jun Inoue)
(prim-test "internal-define inilining" '(1)
           (lambda ()
             (with-error-handler
                 (lambda (e) 'ouch!)
               (lambda ()
                 (eval '(let ()
                          (define (a x) x)
                          (define (b x) (a x))
                          (define (c x) (b x))
                          (list 1))
                       (interaction-environment))))))

;; this caused an internal compiler error in 0.8.6
;; (found and fixed by Kazuki Tsujimoto)
(prim-test "multiple inlining" 0
           (lambda ()
             (let ((f (lambda (i) (set! i 0) i))) (f (f 1)))))

;; this caused an internal compiler error in 0.9.1
(define (zero) 0)
(prim-test "pass3 inlining with pass3/$call optimization" #t
           (lambda ()
             (eval '((letrec ((f (lambda (a b)
                                   (do ((x a (+ x 1)))
                                       ((>= x b))))))
                       f)
                     (zero) (zero))
                   (interaction-environment))))

;; This caused internal error in 0.9.1, and infinite loop in dev version
;; after it.
(prim-test "pass3/$call inlining problem" #t
           (lambda ()
             (procedure?
              (eval '(lambda (n p t)
                       (define (y a r s f)
                         (let loop ([e 0])
                           (cond [(a n) (unwind-protect (s) (r n))]
                                 [(< e 10) (loop (+ 1 e))]
                                 [else (f)])))
                       (define (l0 a r)
                         (y a r (^() (r n)) (^() (error "oo"))))
                       ;; Main locker
                       (define (l1 a r)
                         (y a r p (^() (if (and-let* ([ t ]
                                                      [m (file-mtime n)])
                                             (< (+ m t) 10))
                                         (begin (l0 a r) (l1 a r))))))
                       (error "zz"))
                    (interaction-environment)))))

;;----------------------------------------------------------------
(test-section "optimized frames")

;; Empty environment frame is omitted by compiler optimization.
;; The following tests makes sure if it works correctly.

(prim-test "lambda (empty env)" 1
      (lambda ()
        (let* ((a 1)
               (b (lambda ()
                    ((lambda () a)))))
          (b))))

(prim-test "let (empty env)" 1
      (lambda ()
        (let ((a 1))
          (let ()
            (let ()
              a)))))

(prim-test "let (empty env)" '(1 . 1)
      (lambda ()
        (let ((a 1))
          (cons (let () (let () a))
                (let* () (letrec () a))))))

(prim-test "let (empty env)" '(3 . 1)
      (lambda ()
        (let ((a 1)
              (b 0))
          (cons (let () (let () (set! b 3)) b)
                (let () (let () a))))))

(prim-test "named let (empty env)" 1
      (lambda ()
        (let ((a -1))
          (let loop ()
            (unless (positive? a)
              (set! a (+ a 1))
              (loop)))
          a)))

(prim-test "do (empty env)" 1
      (lambda () (let ((a 0)) (do () ((positive? a) a) (set! a (+ a 1))))))

;;----------------------------------------------------------------
(test-section "hygienity")

(prim-test "hygienity (named let)" 4
      (lambda ()
        (let ((lambda list))
          (let loop ((x 0))
            (if (> x 3) x (loop (+ x 1)))))))

(prim-test "hygienity (internal defines)" 4
      (lambda ()
        (let ((lambda list))
          (define (x) 4)
          (x))))

(prim-test "hygienity (do)" 4
      (lambda ()
        (let ((lambda #f)
              (begin  #f)
              (if     #f)
              (letrec #f))
          (do ((x 0 (+ x 1)))
              ((> x 3) x)
            #f))))

;;----------------------------------------------------------------
(test-section "letrec and letrec*")

(prim-test "letrec reordering" '((1 3) . (2 3 1))
           (lambda ()
             (let ((r '()))
               (cons (letrec ((a (begin (set! r (cons 1 r)) 1))
                              (b (begin (set! r (cons 2 r)) 2))
                              (c (begin (set! r (cons 3 r)) 3)))
                       (list a c))
                     r))))

(prim-test "letrec* non-reordering" '((1 3) . (3 2 1))
           (lambda ()
             (let ((r '()))
               (cons (letrec* ((a (begin (set! r (cons 1 r)) 1))
                               (b (begin (set! r (cons 2 r)) 2))
                               (c (begin (set! r (cons 3 r)) 3)))
                       (list a c))
                     r))))


(test-end)

