;;
;; testing auxsys
;;

(use gauche.test)

(load "auxsys")
(import gauche.auxsys)

(test-start "auxsys")

;; It is difficult to test some functions in gauche.auxsys
;; This is a partial test.

(test-section "math")

(define (nearly= x y)
  (cond ((real? x)
         (if (real? y)
             (< (abs (- x y)) 1e-6)
             #f))
        ((pair? x)
         (if (pair? y)
             (and (nearly= (car x) (car y))
                  (nearly= (cdr x) (cdr y)))
             #f))
        (else (equal? x y))))

(test "sys-fmod" 0.25 (lambda () (sys-fmod 5.25 1)) nearly=)
(test "sys-fmod" 2.3  (lambda () (sys-fmod 8.3 3))  nearly=)
(test "sys-fmod" 8.3  (lambda () (sys-fmod 8.3 33)) nearly=)

(test "sys-frexp" '(0.785 2)
      (lambda () (receive x (sys-frexp 3.14) x))
      nearly=)

(test "sys-modf" '(0.14 3.0)
      (lambda () (receive x (sys-modf 3.14) x))
      nearly=)

;; putenv

(when (symbol-bound? 'sys-putenv)
  (test "sys-putenv" "foo"
        (lambda ()
          (sys-putenv "ZZGGGBBB" "foo")
          (sys-getenv "ZZGGGBBB"))))

(test-end)
