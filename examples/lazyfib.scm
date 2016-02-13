;; Example of using delay and force to realize lazy stream

(define (lcar lis) ;; lazy car
  (car (force lis)))

(define (lcdr lis) ;; lazy cdr
  (cdr (force lis)))

(define (ltake lis n) ;; lazy take
  (if (<= n 0) '() (cons (lcar lis) (ltake (lcdr lis) (- n 1)))))

(define (lmap proc l1 l2) ;; lazy map
  (if (null? l1)
    '()
    (cons (proc (lcar l1) (lcar l2))
          (delay (lmap proc (lcdr l1) (lcdr l2))))))

;; lazy list fibonacci numbers
(define fibs (list* 1 1 (delay (lmap + fibs (cdr fibs)))))

;; try
;(ltake fibs 20)
;(ltake fibs 200)
