;; Testing autoload subtleties part 2
;; This is splitted from test/load.scm, since these tests here uses
;; object system, so we want them to run after test/object.scm.

(use gauche.test)

(test-start "autoload")

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
    ))
(with-output-to-file "test.o/b.scm"
  (lambda ()
    (write '(define-method foo1 ((x <list>)) 'foo1-list))
    (write '(define-method foo2 ((x <list>)) 'foo2-list))
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
    ))
(with-output-to-file "test.o/b.scm"
  (lambda ()
    (write '(define-class <foo> () ((x :init-value 'x))))
    (write '(define *foo* (make <foo>)))
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
    ))
(with-output-to-file "test.o/b.scm"
  (lambda ()
    (write '(define-method bar ((x <bar>)) x))
    ))

(test* "circular-autoload" *test-error*
       (load "test.o/a"))

(sys-system "rm -rf test.o")


(test-end)



