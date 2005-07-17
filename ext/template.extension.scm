;;;
;;; @@extname@@
;;;

(define-module @@extname@@
  (export test-@@extname@@ ;; dummy
          )
  )
(select-module @@extname@@)

;; Loads extension
(dynamic-load "@@extname@@")

;;
;; Put your Scheme definitions here
;;

;; Epilogue
(provide "@@extname@@")


