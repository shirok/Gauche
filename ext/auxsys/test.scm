;;
;; testing auxsys
;;

(use gauche.test)

(load "auxsys")
(import gauche.auxsys)

(test-start "auxsys")
(test-module 'gauche.auxsys)

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

(test "fmod" 0.25 (lambda () (fmod 5.25 1)) nearly=)
(test "fmod" 2.3  (lambda () (fmod 8.3 3))  nearly=)
(test "fmod" 8.3  (lambda () (fmod 8.3 33)) nearly=)

(test "frexp" '(0.785 2)
      (lambda () (receive x (frexp 3.14) x))
      nearly=)

(test "ldexp" 3.14
      (lambda () (ldexp 0.785 2))
      nearly=)

(test "modf" '(0.14 3.0)
      (lambda () (receive x (modf 3.14) x))
      nearly=)

;; putenv

(test "sys-putenv" "foo"
      (lambda ()
        (with-error-handler
         (lambda (e) "foo") ;need to catch if system doesn't have putenv().
         (lambda ()
           (sys-putenv "ZZGGGBBB" "foo")
           (sys-getenv "ZZGGGBBB")))))

(test-end)
