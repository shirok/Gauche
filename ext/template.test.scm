;;;
;;; Test @@extname@@
;;;

(use gauche.test)

(test-start "@@extname@@")
(use @@extname@@)
(test-module '@@extname@@)

;; The following is a dummy test code.
;; Replace it for your tests.
(test* "test-@@extname@@" "@@extname@@ is working"
       (test-@@extname@@))

;; epilogue
(test-end)





