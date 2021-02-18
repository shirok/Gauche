;;;
;;; @@extname@@
;;;

(define-module @@modname@@
  (export test-@@extname@@ ;; dummy
          )
  )
(select-module @@modname@@)

;; Loads extension
(dynamic-load "@@extname@@")

;;
;; Put your Scheme definitions here
;;
