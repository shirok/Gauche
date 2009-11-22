;;;
;;; Test example.mqueue-cpp
;;;

(use srfi-1)
(use gauche.test)
(use gauche.sequence)

(test-start "example.mqueue-cpp")
(use example.mqueue-cpp)
(test-module 'example.mqueue-cpp)

(define mq0 #f)
(define mq1 #f)

(test-section "basic APIs")

(test* "constructor" #t
       (let ((q (make-mqueue "mqueue #0")))
         (set! mq0 q)
         (is-a? q <mqueue>)))

(test* "find" mq0 (mqueue-find "mqueue #0"))
(test* "find" #f  (mqueue-find "mqueue #1"))

(test* "mqueue-name" "mqueue #0" (mqueue-name mq0))

(test* "mqueue-empty?" #t (mqueue-empty? mq0))

(test* "mqueue-push!" 1 (mqueue-push! mq0 "no news is good news"))
(test* "mqueue-push!" 2 (mqueue-push! mq0 "something happening" 1))
(test* "mqueue-push!" 3 (mqueue-push! mq0 "it's nothing"))
(test* "mqueue-push!" 4 (mqueue-push! mq0 "ALERT ALERT!" 4))

(test* "mqueue-empty?" #f (mqueue-empty? mq0))

(test* "mqueue-pop!" "ALERT ALERT!" (mqueue-pop! mq0))
(test* "mqueue-pop!" "something happening" (mqueue-pop! mq0))
(test* "mqueue-pop!" "no news is good news" (mqueue-pop! mq0))
(test* "mqueue-pop!" "it's nothing" (mqueue-pop! mq0))

(test* "mqueue-pop! (exception)" *test-error* (mqueue-pop! mq0))

(test* "mqueue-empty?" #t (mqueue-empty? mq0))

(test-section "multiple queue instance")

(set! mq1 (make-mqueue "mqueue #1"))

(test* "multiple instance" #t
       (and (eq? (mqueue-find "mqueue #0") mq0)
            (eq? (mqueue-find "mqueue #1") mq1)
            (not (eq? mq0 mq1))))

(test* "multiple instance" '("0" "1")
       (begin
         (mqueue-push! mq0 "0" 5)
         (mqueue-push! mq0 "a" 1)
         (mqueue-push! mq1 "1" -2)
         (mqueue-push! mq0 "b" -7)
         (list (mqueue-pop! mq0) (mqueue-pop! mq1))))
         
(test-section "stress test & GC")

(define *size* 1000)
(define *vec*  (make-vector *size*))
(define *wvec* (make-weak-vector *size*))

(test* "stress test" #t
       (begin
         (dotimes (n *size*)
           (let1 q (make-mqueue (x->string n))
             (set! (ref *vec* n) q)
             (set! (ref *wvec* n) q)))
         (dotimes (n *size*)
           (dotimes (k 10)
             (mqueue-push! (ref *vec* n) (x->string k)))
           (dotimes (k 10)
             (mqueue-pop! (ref *vec* n)))
           (gc))
         (dotimes (n *size*) (set! (ref *vec* n) #f))
         ;; smash the stack
         (let loop ((j 0)) (unless (= j *size*) (loop (+ j 1))) #t)
         (dotimes (k 10) (gc))
         (every (cut not <>) (coerce-to <list> *wvec*))))

;; epilogue
(test-end)





