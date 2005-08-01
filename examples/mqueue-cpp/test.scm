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

;(test* "mqueue-pop! (exception)" *test-error* (mqueue-pop! mq0))

(test* "mqueue-push!" 1 (mqueue-push! mq0 "no news is good news"))
(test* "mqueue-push!" 2 (mqueue-push! mq0 "something happening" 1))
(test* "mqueue-push!" 3 (mqueue-push! mq0 "it's nothing"))
(test* "mqueue-push!" 4 (mqueue-push! mq0 "ALERT ALERT!" 4))

(test* "mqueue-pop!" "ALERT ALERT!" (mqueue-pop! mq0))
(test* "mqueue-pop!" "something happening" (mqueue-pop! mq0))
(test* "mqueue-pop!" "no news is good news" (mqueue-pop! mq0))
(test* "mqueue-pop!" "it's nothing" (mqueue-pop! mq0))


;; epilogue
(test-end)





