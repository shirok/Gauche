;;
;; testing module system
;;

(use gauche.test)

(test-start "module")

;;------------------------------------------------------------------
;; basic test

(define-module M
  (define a 3)
  (define cons +))

(define a 100)
(define b 200)

(test "define-module" #t (lambda () (module? (find-module 'M))))
(test "with-module" 3 (lambda () (with-module M a)))
(test "with-module" 5 (lambda () (with-module M (cons a 2))))
(test "with-module" '(3 . 2) (lambda () (cons (with-module M a) 2)))
(test "with-module" 5
      (lambda ()
        (eval '(with-module M (define b 2) (cons a b))
              (interaction-environment))))
(test "with-module" 2 (lambda () (with-module M b)))
(test "with-module" 300
      (lambda () (with-module M
                    (cons (with-module user a) (with-module user b)))))

(test "with-module (error)" (test-error)
      (lambda () (eval '(with-module MM 4) (interaction-environment))))

(with-module M
  (define + list)
  (define if list))

(test "with-module in head position" '(2 3)
      (lambda ()
        ((with-module M +) 2 3)))

(test "with-module in head position" '(3 5 6)
      (lambda ()
        (with-module M
          (if ((with-module scheme if) 2 3 4) 5 6))))

(define-module MA
  (export with-module)
  (define-syntax with-module
    (syntax-rules ()
      ((_ a b) list))))

(test "with-module in head position (shadowed)" '(1 2 3)
      (lambda ()
        (with-module MA
          ((with-module x y) 1 2 3))))

(define-module MB
  (import MA)
  (export oops)
  (define-syntax oops
    (syntax-rules ()
      ((_ a) (define a 3)))))

(test "with-module in head position (shadowed)" '(1 2 3)
      (lambda ()
        (with-module MB
          ((with-module x y) 1 2 3))))

(test "with-module in head position (in lambda body)" 6
      (lambda ()
        (let ((x 1))
          ((with-module MB oops) x)
          (+ x x))))

;;------------------------------------------------------------------
;; define-in-module

(test "define-in-module" 99
      (lambda ()
        (eval '(define-in-module M aa 99) (interaction-environment))
        (eval '(with-module M aa) (interaction-environment))))

(test "define-in-module" (test-error)
      (lambda ()
        (eval '(define-in-module MM aa 99) (interaction-environment))
        (eval '(with-module MM aa) (interaction-environment))))

;;------------------------------------------------------------------
;; import, export

(define-module N
  (export push-result get-result reset-result)

  (define result '())
  (define (get-result) (reverse result))
  (define (push-result r) (set! result (cons r result)))
  (define (reset-result) (set! result '())))

(define-module O
  (import N)

  (define + *)
  )

(test "import/export" '(56 72)
      (lambda ()
        (eval '(with-module O
                 (reset-result)
                 (define a 7)
                 (define b 8)
                 (define c 9)
                 (push-result (+ a b))
                 (push-result (+ b c))
                 (get-result))
              (interaction-environment))))

(test "import (error)" (test-error)
      (lambda () (eval '(import MM) (interaction-environment))))

(define-module OO
  (import (N :prefix N:))
  (define + *))

(test "import w/prefix" '(56 72)
      (lambda ()
        (eval '(with-module OO
                 (N:reset-result)
                 (define a 7)
                 (define b 8)
                 (define c 9)
                 (N:push-result (+ a b))
                 (N:push-result (+ b c))
                 (N:get-result))
              (interaction-environment))))

(test "import w/prefix (error)" (test-error)
      (lambda ()
        (eval '(with-module OO (reset-result))
              (interaction-environment))))

(test "import w/prefix (insertion)" 99
      (lambda ()
        (eval '(with-module N (export new-binding) (define (new-binding) 99))
              (interaction-environment))
        (eval '(with-module OO (N:new-binding)) (interaction-environment))))

;;------------------------------------------------------------------
;; select-module, and restoration in load().

(test "select-module" '(O O N O)
      (lambda ()
        (eval
         '(with-module O
            (define load-data '((select-module O)
                                (push-result (module-name (current-module)))
                                (select-module N)
                                (push-result (module-name (current-module)))))
            (reset-result)
            (push-result (module-name (current-module)))
            (with-output-to-file "tmp.t"
              (lambda () (for-each write load-data)))
            (load "./tmp.t")
            (push-result (module-name (current-module)))
            (sys-unlink "tmp.t")
            (get-result)
            )
         (interaction-environment))))

(test "select-module" 'user (lambda () (module-name (current-module))))

(test "select-module (error)" (test-error)
      (lambda () (eval '(select-moulde MM) (interaction-environment))))

;;------------------------------------------------------------------
;; module inheritance

(define-module P
  (export a b)
  (define a 'alpha)
  (define b 'beta))
(define-module Q
  (export a b d)
  (define a 'ei)
  (define b 'bee)
  (define d 'dee))
(define-module R
  (export c)
  (extend P)
  (define c 'gamma))
(define-module S
  (export c)
  (extend Q P)
  (define c 'delta))
(define-module T
  (export c)
  (extend Q)
  (define c 'delta))
(define-module U
  (extend R T)
  )
(define-module V
  (import U)
  )

(test "module inheritance" 'alpha (lambda () (with-module R a)))
(test "module inheritance" 'ei    (lambda () (with-module S a)))
(test "module inheritance" '(gamma beta)
      (lambda ()
        (with-module U (list c b))))
(test "module inheritance" '(alpha beta gamma dee)
      (lambda ()
        (with-module V (list a b c d))))

(test "moduel inheritance (error)" (test-error)
      (lambda ()
        (eval '(with-module V (extend Q MM)) (interaction-environment))))

(test "global-variable-ref" 'gamma
      (lambda ()
        (global-variable-ref 'U 'c)))

(test "global-variable-ref" (test-error)
      (lambda ()
        (global-variable-ref 'U 'e)))

(test "global-variable-ref" 'huh?
      (lambda ()
        (global-variable-ref 'U 'e 'huh?)))

(test "global-variable-ref" 'huh?
      (lambda ()
        (global-variable-ref 'U 'c 'huh? #t)))

;;------------------------------------------------------------------
;; creates modules on-the-fly

(test "make-module" #t
      (lambda ()
        (make-module 'foo)
        (module? (find-module 'foo))))

(test "make-module (duplicate name)" (test-error)
      (lambda ()
        (make-module 'foo)))

(test "make-module (duplicate name)" (test-error)
      (lambda ()
        (make-module 'foo :if-exists :error)))

(test "make-module (duplicate name)" #f
      (lambda ()
        (make-module 'foo :if-exists #f)))

(test "anynomous module" #t
      (lambda ()
        (let ((m0 (make-module #f))
              (m1 (make-module #f)))
          (and (module? m0) (module? m1) (not (eq? m0 m1))))))

(test "anonymous module" 13
      (lambda ()
        (let ((m0 (make-module #f)))
          (eval '(define x 13) m0)
          (eval 'x m0))))
              
(test "anonymous module" (test-error)
      (lambda ()
        (let ((m0 (make-module #f))
              (m1 (make-module #f)))
          (eval '(define x 13) m0)
          (eval 'x m1))))

(test-end)
