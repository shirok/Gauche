;; Testing autoload subtleties part 2
;; This is splitted from test/load.scm, since these tests here uses
;; object system, so we want them to run after test/object.scm.

(use gauche.test)

(test-start "autoload and autoprovide")

(add-load-path ".")

;;---------------------------------------------------------------
(test-section "autoload and implicit generic definition")

(sys-system "rm -rf test.o")
(sys-mkdir "test.o" #o777)
(with-output-to-file "test.o/a.scm"
  (lambda ()
    (write '(autoload "test.o/b" foo1 foo2))
    (write '(define-method foo1 ((x <string>)) 'foo1-string))
    (write '(define-method foo2 ((x <string>)) 'foo2-string))
    (write '(provide #f))
    ))
(with-output-to-file "test.o/b.scm"
  (lambda ()
    (write '(define-method foo1 ((x <list>)) 'foo1-list))
    (write '(define-method foo2 ((x <list>)) 'foo2-list))
    (write '(provide #f))
    ))

(test* "autoload triggered by implicit generic definition" 'foo1-list
       (and (load "test.o/a") (foo1 '())))

(test* "autoload triggered by implicit generic definition" 'foo2-list
       (foo2 '()))

(sys-system "rm -rf test.o")

;;---------------------------------------------------------------
(test-section "autoload and class redefinition check")

(sys-mkdir "test.o" #o777)
(with-output-to-file "test.o/a.scm"
  (lambda ()
    (write '(autoload "test.o/b" <foo>))
    (write '(define-class <foo> () ((x) (y :init-value 'y))))
    (write '(provide #f))
    ))
(with-output-to-file "test.o/b.scm"
  (lambda ()
    (write '(define-class <foo> () ((x :init-value 'x))))
    (write '(define *foo* (make <foo>)))
    (write '(provide #f))
    ))

(test* "autoload triggered by class redefinition check" 'y
       (and (load "test.o/a")
            (slot-ref *foo* 'y)))

(sys-system "rm -rf test.o")

;;---------------------------------------------------------------
(test-section "circular autoload")

(sys-mkdir "test.o" #o777)
(with-output-to-file "test.o/a.scm"
  (lambda ()
    (write '(autoload "test.o/b" <bar>))
    (write '(define-class <bar> () ()))
    (write '(provide #f))
    ))
(with-output-to-file "test.o/b.scm"
  (lambda ()
    (write '(define-method bar ((x <bar>)) x))
    (write '(provide #f))
    ))

(test* "circular-autoload" (test-error)
       (load "test.o/a"))

(sys-system "rm -rf test.o")

;;---------------------------------------------------------------
(test-section "autoprovide feature")

(sys-mkdir "test.o" #o777)

(test* "autoprovide - basic" #t
       (begin
         (with-output-to-file "test.o/autoprovide-a.scm"
           (lambda () (write 1)))
         (eval '(require "test.o/autoprovide-a") (interaction-environment))
         (provided? "test.o/autoprovide-a")))

(test* "autoprovide - providing different feature" '(#f #t #t)
       (begin
         (with-output-to-file "test.o/autoprovide-b.scm"
           (lambda ()
             (write '(provide "test.o/autoprovide-bb"))
             (write '(provide "test.o/autoprovide-ba"))))
         (eval '(require "test.o/autoprovide-b") (interaction-environment))
         (list (provided? "test.o/autoprovide-b")
               (provided? "test.o/autoprovide-ba")
               (provided? "test.o/autoprovide-bb"))))

(test* "autoprovide - disabling autoprovide" #f
       (begin
         (with-output-to-file "test.o/autoprovide-c.scm"
           (lambda () (write '(provide #f))))
         (eval '(require "test.o/autoprovide-c") (interaction-environment))
         (provided? "test.o/autoprovide-c")))

(test* "autoprovide - cascading require" '(#f #t)
       (begin
         (with-output-to-file "test.o/autoprovide-d.scm"
           (lambda ()
             (write '(require "test.o/autoprovide-e"))
             (write '(provide #f))))
         (with-output-to-file "test.o/autoprovide-e.scm"
           (lambda () (write 1)))
         (eval '(require "test.o/autoprovide-d") (interaction-environment))
         (list (provided? "test.o/autoprovide-d")
               (provided? "test.o/autoprovide-e"))))

(sys-system "rm -rf test.o")


;; relative load path -------------------------------

(test-section "relative load path")

(test* "relative load path" "ok!"
       ;; avoid using process-output->string since it is tested after
       ;; this file.
       (begin
         (cond-expand
          [gauche.os.windows
           (let1 gosh (or (sys-getenv "TESTGOSH") ".\\gosh")
             (sys-system #"~gosh -ftest ..\\test\\relative-load-path > test.o"))]
          [else
           (let1 gosh (or (sys-getenv "TESTGOSH") "./gosh")
             (sys-system #"~gosh -ftest ../test/relative-load-path > test.o"))])
         (rlet1 r (with-input-from-file "test.o" read-line)
           (sys-unlink "test.o"))))

(test-end)

