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
      (lambda () (with-module M (define b 2) (cons a b))))
(test "with-module" 2 (lambda () (with-module M b)))
(test "with-module" 300
      (lambda () (with-module M
                    (cons (with-module user a) (with-module user b)))))

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
      (lambda () (with-module O
                   (reset-result)
                   (define a 7)
                   (define b 8)
                   (define c 9)
                   (push-result (+ a b))
                   (push-result (+ b c))
                   (get-result))))

;;------------------------------------------------------------------
;; select-module, and restoration in load().

(test "select-module" '(O O N O)
      (lambda ()
        (with-module O
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
          )))

(test "select-module" 'user (lambda () (module-name (current-module))))

;;------------------------------------------------------------------
;; module inheritance

(define-module P
  (define a 'alpha)
  (define b 'beta))
(define-module Q
  (define a 'ei)
  (define b 'bee))
(define-module R
  (extend P)
  (define c 'gamma))
(define-module S
  (extend Q P)
  (define c 'delta))
(define-module T
  (extend Q)
  (define c 'delta))
(define-module U
  (extend R T)
  )

(test "module inheritance" 'alpha (lambda () (with-module R a)))
(test "module inheritance" 'ei    (lambda () (with-module S a)))
(test "module inheritance" '(gamma beta)
      (lambda ()
        (with-module U (list c b))))


(test-end)
