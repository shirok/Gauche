;;;
;;; Test @@modname@@
;;;

(use gauche.test)

(test-start "@@modname@@")
(use @@modname@@)
(test-module '@@modname@@)

;; The following is a dummy test code.
;; Replace it for your tests.
(test* "test-@@extname@@" "@@extname@@ is working"
       (test-@@extname@@))

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
