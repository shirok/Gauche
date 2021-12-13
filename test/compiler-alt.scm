;;
;; Test alternative compiler backend
;;
;; The modules tested here is not yet in the production

(use gauche.test)
(test-start "alternative compiler backend")

(test-section "modules")
(use gauche.cgen.bbb)
(test-module 'gauche.cgen.bbb)

(use gauche.vm.register-machine)
(test-module 'gauche.vm.register-machine)


(test-section "simple code")

(define (t msg module expect program)
  (test* msg expect
         (if module
           (run-on-register-machine program module)
           (run-on-register-machine program))))


(t "simple expression" #f "abcxyz"
   '(string-append (list->string '(#\a #\b #\c))
                   "xyz"))

(t "calling inlined numerics" #f 5
   '(+ (* 2 3) (/ (- 4 5 6) 7)))

(let ((mod (make-module #f)))
  (t "simple expression, definition 1" mod 'add-one
     '(define (add-one n) (+ 1 n)))
  (t "simple expression, definition 2" mod 'add-two
     '(define (add-two n) (+ 2 n)))
  (t "simple expression, calling defined ones" mod 20402
     '(* (add-one 100) (add-two 200)))

  (t "local bindings" mod '(-3 -6)
     '(let ((a (add-one 4)) (b (add-two 5)))
        (list (add-two (- a)) (add-one (- b)))))
  )

(t "$IT node handling" #f '(2 1 #f)
   '(begin
      (define (foo a b c) (and a (or b c)))
      (list (foo #t #f 2)
            (foo #t 1 2)
            (foo #f 3 4))))

(t "letrec and loop" #f
   '(1 2 Fizz 4 Buzz Fizz 7 8 Fizz Buzz 11 Fizz 13 14 FizzBuzz 16 17
       Fizz 19 Buzz)
   '(let loop ((n 1) (r '()))
      (cond [(> n 20) (reverse r)]
            [(zero? (modulo n 15)) (loop (+ n 1) (cons 'FizzBuzz r))]
            [(zero? (modulo n 3)) (loop (+ n 1) (cons 'Fizz r))]
            [(zero? (modulo n 5)) (loop (+ n 1) (cons 'Buzz r))]
            [else (loop (+ n 1) (cons n r))])))

(t "letrec and recursive call" #f 21
   '(let fib ((n 7))
      (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))

(t "bulitin ops" #f '(A (1 2) 1 1 2 Z)
   '(let ((x (list 1 2)))
      `(A ,x ,(car x) ,@x Z)))

(test-end)
