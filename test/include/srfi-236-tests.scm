(import (scheme base)
        (srfi 78)
        (srfi 236))

;; just to test edge case
(check
 (let ((v 'ok))
   (independently)
   v)
 => 'ok)

(check
 (let ((v (cons #f #f)))
   (independently
    (set-car! v 'ok))
   v)
 => '(ok . #f))

(check
 (let ((v (list 1 2)))
   (independently
    (list-set! v 0 10)
    (list-set! v 1 10))
   (+ (car v) (cadr v)))
 => 20)

(check
 (let ((v (vector 1 2 3)))
   (independently
    (vector-set! v 0 10)
    (vector-set! v 1 20)
    (vector-set! v 2 30))
   v)
 => '#(10 20 30))

(check-report)
