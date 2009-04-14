;;
;; testing auxsys
;;

(use gauche.test)
(use gauche.process)

(test-start "auxsys")
(use gauche.auxsys)
(test-module 'gauche.auxsys
             ;; A bunch of variables are defined conditionally depending
             ;; on the platform support.  So we exclude them from 
             ;; undefined variable reference check.
             :allow-undefined '(sys-mkfifo sys-setgid sys-setpgid
                                sys-getpgrp sys-setsid sys-setuid
                                sys-getgroups sys-uname %sys-gethostname
                                %sys-getdomainname sys-ctermid))


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
 (else))

;; setenv

(cond-expand
 (gauche.sys.setenv
  (test "sys-setenv" "foo"
        (lambda ()
          (sys-setenv "ZZGGGBBB" "foo" #t)
          (sys-getenv "ZZGGGBBB")))
  (test "sys-setenv" "foo"
        (lambda ()
          (sys-setenv "ZZGGGBBB" "bar" #f)
          (sys-getenv "ZZGGGBBB"))))
 (else))

;; unsetenv

(cond-expand
 (gauche.sys.unsetenv
  (test "sys-unsetenv" #f
        (lambda ()
          (sys-setenv "ZZGGGBBB" "foo" #t)
          (sys-unsetenv "ZZGGGBBB")
          (sys-getenv "ZZGGGBBB"))))
 (else))

;; environ
(test* "sys-environ->alist" '(("A" . "B") ("A" . "") ("" . "B") ("A" . "B=C"))
       (sys-environ->alist '("A=B" "A=" "=B" "A=B=C")))

(let ((envs (sys-environ)))
  (define (env-test var)
    (test* #`"sys-environ (,var)" #t
           (cond [(sys-getenv var)
                  => (lambda (val)
                       (not (not (member #`",|var|=,|val|" envs))))]
                 [else #t])))
  (env-test "HOME")
  (env-test "USER")
  (env-test "LANG")
  (env-test "PWD")
  (env-test "TERM")
  (env-test "SHELL"))

;; sys-realpath

(define (expected-path p)
  (let1 pp (sys-normalize-pathname p :absolute #t :canonicalize #t)
    (if (eqv? (string-ref pp (- (string-length pp) 1)) #\/)
      (substring pp 0 (- (string-length pp) 1))
      pp)))

(test* "sys-realpath (/)" "/" (sys-realpath "/"))
(test* "sys-realpath (.)" (expected-path ".") (sys-realpath "."))

(cond-expand
 [gauche.sys.symlink
  (sys-unlink "test1.o")
  (sys-unlink "test2.o")
  (with-output-to-file "test1.o" (cut print))

  (sys-symlink "test1.o" "test2.o")
  (test* "sys-realpath (symlink)"
         (expected-path "./test1.o")
         (sys-realpath "./test2.o"))
  (sys-unlink "test2.o")

  (sys-symlink "./test1.o" "test2.o")
  (test* "sys-realpath (symlink)"
         (expected-path "./test1.o")
         (sys-realpath "./test2.o"))
  (sys-unlink "test2.o")

  (sys-symlink "../auxsys/test1.o" "test2.o")
  (test* "sys-realpath (symlink)"
         (expected-path "./test1.o")
         (sys-realpath "./test2.o"))

  (sys-unlink "test1.o")
  (test* "sys-realpath (dangling)"
         *test-error*
         (sys-realpath "./test2.o"))

  (sys-mkdir "test1.o" #o777)
  (with-output-to-file "test1.o/test.o" (cut print))
  (test* "sys-realpath (symlink to dir)"
         (expected-path "./test1.o/test.o")
         (sys-realpath "./test2.o/test.o"))

  (sys-unlink "test1.o/test.o")
  (sys-rmdir "test1.o")
  (sys-unlink "test2.o")

  (test* "sys-realpath (NOENT)"
         *test-error*
         (sys-realpath "./test2.o/test.o"))
  ]
 [else])

(test-end)
