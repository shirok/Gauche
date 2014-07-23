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
  (export push-result get-result reset-result
          shared-gloc0 shared-gloc1)

  (define result '())
  (define (get-result) (reverse result))
  (define (push-result r) (set! result (cons r result)))
  (define (reset-result) (set! result '()))

  ;; for testing %alias-binding via :only and :rename
  (define shared-gloc0 0)
  (define shared-gloc1 1))

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

(define-module O1
  (import (N :only (reset-result get-result shared-gloc0))))
(test "import w/only" '()
      (lambda () (eval '(begin (reset-result) (get-result)) (find-module 'O1))))
(test "import w/only (error)" (test-error)
      (lambda () (eval '(push-result 'a) (find-module 'O1))))
(test "import w/only propagation" '(0 10)
      (lambda ()
        (let ((pre (eval 'shared-gloc0 (find-module 'O1))))
          (with-module N (set! shared-gloc0 10))
          (list pre (eval 'shared-gloc0 (find-module 'O1))))))

(define-module O2
  (import (N :only (reset-result get-result) :prefix N:)))
(test "import w/only-prefix" '()
      (lambda ()
        (eval '(begin (N:reset-result) (N:get-result)) (find-module 'O2))))
(test "import w/only-prefix (error)" (test-error)
      (lambda () (eval '(N:push-result 'a) (find-module 'O2))))

(define-module O3
  (import (N :prefix N: :only (N:reset-result N:get-result))))
(test "import w/prefix-only" '()
      (lambda ()
        (eval '(begin (N:reset-result) (N:get-result)) (find-module 'O3))))
(test "import w/prefix-only (error)" (test-error)
      (lambda () (eval '(N:push-result 'a) (find-module 'O3))))
(test "import w/prefix-only (nonexistent error)" (test-error)
      (lambda () (eval '(define-module O3bis
                          (import N :prefix N: :only (reset-result)))
                       (current-module))))

(define-module O4
  (import (N :except (push-result))))
(test "import w/except" '()
      (lambda () (eval '(begin (reset-result) (get-result)) (find-module 'O4))))
(test "import w/except (error)" (test-error)
      (lambda () (eval 'push-result (find-module 'O4))))

(define-module O5
  (import (N :except (push-result) :prefix N:)))
(test "import w/except-prefix" '()
      (lambda ()
        (eval '(begin (N:reset-result) (N:get-result)) (find-module 'O5))))
(test "import w/except-prefix (error)" (test-error)
      (lambda () (eval 'N:push-result (find-module 'O5))))

(define-module O6
  (import (N :prefix N: :except (N:push-result))))
(test "import w/prefix-except" '()
      (lambda ()
        (eval '(begin (N:reset-result) (N:get-result)) (find-module 'O6))))
(test "import w/prefix-except (error)" (test-error)
      (lambda ()
        (eval 'N:push-result (find-module 'O6))))
(test "import w/prefix-except (nonexistent error)" (test-error)
      (lambda ()
        (eval '(define-module O6bis
                 (import N :prefix N: :except (reset-result)))
              (current-module))))

(define-module O7
  (import (N :rename ((reset-result reset) (get-result get)))))
(test "import w/rename" '(1 2)
      (lambda ()
        (eval '(begin (reset) (push-result 1) (push-result 2) (get))
              (find-module 'O7))))
(test "import w/rename (make sure old binding is removed)" (test-error)
      (lambda ()
        (eval '(reset-result) (find-module 'O7))))

(define-module O8
  ;; swapping names
  (import (N :rename ((reset-result get-result) (get-result reset-result)))))
(test "import w/rename (swap)" '(1 2)
      (lambda ()
        (eval '(begin
                 (get-result) (push-result 1) (push-result 2) (reset-result))
              (find-module 'O8))))

(define-module O9
  (import (N :rename ((reset-result r) (get-result g)) :prefix n:)))
(test "import w/rename-prefix" '(1 2)
      (lambda ()
        (eval '(begin (n:r) (n:push-result 1) (n:push-result 2) (n:g))
              (find-module 'O9))))
(test "import w/rename-prefix (hiding)" (test-error)
      (lambda ()
        (eval 'n:reset-result (find-module 'O9))))
(test "import w/rename-prefix (hiding)" (test-error)
      (lambda ()
        (eval 'n:get-result (find-module 'O9))))

(define-module Oa
  (import (N :prefix n: :rename ((n:reset-result r) (n:get-result g)))))
(test "import w/prefix-rename" '(1 2)
      (lambda ()
        (eval '(begin (r) (n:push-result 1) (n:push-result 2) (g))
              (find-module 'Oa))))
(test "import w/prefix-rename (hiding)" (test-error)
      (lambda () (eval 'n:reset-result (find-module 'Oa))))
(test "import w/prefix-rename (hiding)" (test-error)
      (lambda () (eval 'n:get-result (find-module 'Oa))))
(test "import w/prefix-rename (hiding)" (test-error)
      (lambda () (eval 'reset-result (find-module 'Oa))))
(test "import w/prefix-rename (hiding)" (test-error)
      (lambda () (eval 'n:r (find-module 'Oa))))
(test "import w/prefix-rename (hiding)" (test-error)
      (lambda () (eval 'n:g (find-module 'Oa))))

(define-module Ob
  (import (N :prefix n: :rename ((n:push-result p))
             :prefix nn: :rename ((nn:n:get-result g)))))
(test "import w/prefix-rename-prefix-rename" '(1 2)
      (lambda ()
        (eval '(begin (nn:n:reset-result) (nn:p 1) (nn:p 2) (g))
              (find-module 'Ob))))

(define-module Oc
  (export ichi (rename shi yon) go)
  (define ichi 1)
  (define shi 4)
  (define go (+ ichi shi))
  (define kyu (+ shi go))
  (export (rename kyu ku)))
(define-module Oc-1
  (import Oc))

(test "export-time renaming" '(1 4 5 9 #f #f)
      (lambda ()
        (let [(m (find-module 'Oc-1))]
          (list (global-variable-ref m 'ichi #f)
                (global-variable-ref m 'yon #f)
                (global-variable-ref m 'go #f)
                (global-variable-ref m 'ku #f)
                (global-variable-ref m 'shi #f)
                (global-variable-ref m 'kyu #f)))))

(define-module Od
  (import (Oc :rename ((ku kokono)) :prefix mm:)))

(test "export-time renaming plus import renaming" '(#f 9)
      (lambda ()
        (let [(m (find-module 'Od))]
          (list (global-variable-ref m 'mm:ku #f)
                (global-variable-ref m 'mm:kokono #f)))))

(define-module Oe
  (export (rename a x) (rename x a))
  (define a 'A)
  (define x 'X))
(define-module Oe-1
  (import Oe))

(test "export-time renaming with swapping" '(X A)
      (lambda ()
        (let [(m (find-module 'Oe-1))]
          (list (global-variable-ref m 'a #f)
                (global-variable-ref m 'x #f)))))

(define-module Of
  (export foo)
  (define foo 1))
(define-module Of-1
  (import Of)
  (export foo))

(test "transitive export" 1
      (lambda () (global-variable-ref (find-module 'Of-1) 'foo #f)))

(define-module Of-2
  (import Of)
  (export (rename foo foo-alias)))
(define-module Of-3
  (import Of-2))

(test "export-time renaming and transitive export" '(1 #f)
      (lambda ()
        (list
         (global-variable-ref (find-module 'Of-3) 'foo-alias #f)
         (global-variable-ref (find-module 'Of-3) 'foo #f))))

(define-module Of-4
  (import (Of-2 :rename ((foo-alias foo-newname)))))

(test "export-time renaming, transitive export, plus import renaming"
      '(1 #f #f)
      (lambda ()
        (list
         (global-variable-ref (find-module 'Of-4) 'foo-newname #f)
         (global-variable-ref (find-module 'Of-4) 'foo-alias #f)
         (global-variable-ref (find-module 'Of-4) 'foo #f))))

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

;;------------------------------------------------------------------
;; mpl search bug in 0.9.1 reported by Ryo Akagi
;; http://sourceforge.jp/projects/gauche/lists/archive/devel-jp/2010-December/001909.html

(define-module mplbug-x (export x) (define x 0))
(define-module mplbug-a (export a) (define a 1))
(define-module mplbug-b (export b) (define b 2))
(define-module mplbug-A (extend mplbug-x mplbug-a))
(define-module mplbug-B (extend mplbug-x mplbug-b))

(define-module mplbug-user1 (import mplbug-A) (import mplbug-B))
(define-module mplbug-user2 (import mplbug-B) (import mplbug-A))

(define (mplbug-test mod var)
  (test* #"mpl search (~mod,~var)" #t
         (global-variable-bound? (find-module mod) var)))

(mplbug-test 'mplbug-user1 'a)
(mplbug-test 'mplbug-user1 'b)
(mplbug-test 'mplbug-user1 'x)
(mplbug-test 'mplbug-user2 'a)
(mplbug-test 'mplbug-user2 'b)
(mplbug-test 'mplbug-user2 'x)

(test-end)
