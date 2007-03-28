;;
;; testing auxsys
;;

(use gauche.test)

(test-start "auxsys")
(use gauche.auxsys)
(test-module 'gauche.auxsys
             ;; A bunch of variables are defined conditionally depending
             ;; on the platform support.  So we exclude them from 
             ;; undefined variable reference check.
             :allow-undefined '(sys-mkfifo sys-setgid sys-setpgid
                                sys-getpgrp sys-setsid sys-setuid
                                sys-getgroups sys-uname %sys-gethostname
                                %sys-getdomainname sys-ctermid sys-chown))


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

(cond-expand
 (gauche.sys.setenv
  (test "sys-putenv" "foo"
        (lambda ()
          (sys-putenv "ZZGGGBBB=foo")
          (sys-getenv "ZZGGGBBB")))
  (test "sys-putenv" "foo"
        (lambda ()
          (sys-putenv "ZZGGGBBB" "foo")  ;;old API
          (sys-getenv "ZZGGGBBB"))))
 (else #f))

;; setenv

(cond-expand
 (gauche.sys.setenv
  (test "sys-setenv" "foo"
        (lambda ()
          (sys-setenv "ZZGGGBBB" "foo" #t)
          (sys-getenv "ZZGGGBBB"))))
 (else #f))

;; unsetenv

(cond-expand
 (gauche.sys.unsetenv
  (test "sys-unsetenv" #f
        (lambda ()
          (sys-setenv "ZZGGGBBB" "foo" #t)
          (sys-unsetenv "ZZGGGBBB")
          (sys-getenv "ZZGGGBBB"))))
 (else #f))

(test-end)
