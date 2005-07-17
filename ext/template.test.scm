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

;; epilogue
(test-end)





