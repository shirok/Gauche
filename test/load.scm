;;
;; Tests for subtle effects of loading and autoloading
;;

(use gauche.test)

(test-start "load")

(add-load-path ".")

;;----------------------------------------------------------------
(test-section "require and provide")

(sys-system "rm -rf test.o")
(sys-mkdir "test.o" #o777)
(with-output-to-file "test.o/a.scm"
  (lambda ()
    (write '(provide "test.o/a"))
    (newline)))

(test "double require"
      #t
      (lambda ()
        (eval '(require "test.o/a") (interaction-environment))
        (sys-unlink "test.o/a.scm")
        (eval '(require "test.o/a") (interaction-environment))
        #t))

(sys-system "rm -rf test.o")
(sys-mkdir "test.o" #o777)
(with-output-to-file "test.o/b.scm"
  (lambda ()
    (write '(require "test.o/c"))
    (write '(provide "test.o/b"))
    (newline)))
(with-output-to-file "test.o/c.scm"
  (lambda ()
    (write '(require "test.o/b"))
    (write '(provide "test.o/c"))
    (newline)))

(test "detecting loop of require"
      'error
      (lambda ()
        (with-error-handler
         (lambda (e) 'error)
         (lambda ()
           (eval '(require "test.o/b") (interaction-environment)))))
      )

(sys-system "rm -rf test.o")
(sys-mkdir "test.o" #o777)
(with-output-to-file "test.o/d.scm"
  (lambda ()
    (display "(define z 0)(")
    (newline)))

(test "reload after error"
      1
      (lambda ()
        (with-error-handler
         (lambda (e) #t)
         (lambda ()
           (eval '(require "test.o/d") (interaction-environment))))
        (with-output-to-file "test.o/d.scm"
          (lambda ()
            (write '(define z 1))
            (write '(provide "tset.o/d"))))
        (eval '(require "test.o/d") (interaction-environment))
        (eval 'z (interaction-environment))))

;; :environment arg -------------------------------------
(with-output-to-file "test.o/d.scm"
  (lambda ()
    (display "(define foo 3)")))
(define-module load.test )
(define foo 8)

(test ":environment argument"
      3
      (lambda ()
        (load "test.o/d" :environment (find-module 'load.test))
        (with-module load.test foo)))

(sys-system "rm -rf test.o")

(test-end)
