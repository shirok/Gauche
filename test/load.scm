;;
;; Tests for subtle effects of loading and autoloading
;;

(use gauche.test)

(test-start "load")

(add-load-path ".")

;;----------------------------------------------------------------
(test-start "require and provide")

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

(test-end)
