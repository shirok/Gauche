;;;
;;; mqueue_cpp
;;;

(define-module example.mqueue-cpp
  (export <mqueue>
          make-mqueue mqueue-find mqueue-push! mqueue-pop!)
  )
(select-module example.mqueue-cpp)

;; Loads extension
(dynamic-load "mqueue_cpp")

;; Epilogue
(provide "example/mqueue-cpp")


