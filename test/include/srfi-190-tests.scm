;;; Taken from srfi-190 repo
;;; https://github.com/scheme-requests-for-implementation/srfi-190/test.scm

(import (scheme base)
        (srfi 158)
        (srfi 190)
        (srfi 64))

(test-begin "SRFI 190")

(test-group "Coroutine Generators"
  (define g
    (coroutine-generator
      (do ((i 0 (+ i 1)))
          ((<= 3 i))
        (yield i))))

  (define-coroutine-generator (h n)
    (do ((i 0 (+ i 1)))
        ((<= 3 i))
      (yield i)))

  (define gg
    (let-syntax ((yield-square (syntax-rules () ((_ i) (yield (* i i))))))
      (coroutine-generator
       (do ((i 0 (+ i 1)))
           ((<= 3 i))
         (yield-square i)))))

  (test-equal '(0 1 2) (generator->list g))
  (test-equal '(0 1 2) (generator->list (h 3)))
  (test-equal '(0 1 4) (generator->list gg))
  )

(test-end)
