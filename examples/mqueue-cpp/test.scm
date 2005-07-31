;;;
;;; Test example.mqueue-cpp
;;;

(use gauche.test)

(test-start "example.mqueue-cpp")
(use example.mqueue-cpp)
(test-module 'example.mqueue-cpp)

(define mq0 #f)
(define mq1 #f)

(test* "constructor" #t
       (let ((q (make-mqueue "mqueue #0")))
         (set! mq0 q)
         (is-a? q <mqueue>)))

(test* "find" mq0 (mqueue-find "mqueue #0"))
(test* "find" #f  (mqueue-find "mqueue #1"))


;; epilogue
(test-end)





