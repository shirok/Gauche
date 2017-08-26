;;
;; For optimizing comparator comparison procedures
;;

(use gauche.time)
(use srfi-42)

(define-constant *count* 1000000)

(define (binary cmp a b)
  (^[] (dotimes [*count*]
         (list
          (=? cmp a b)
          (>? cmp a b)
          (>=? cmp a b)
          (<? cmp a b)
          (<=? cmp a b)))))

(define (binary-primitive a b)
  (^[] (dotimes [*count*]
         (list
          (= a b)
          (> a b)
          (>= a b)
          (< a b)
          (<= a b)))))

(define (ternary cmp a b c)
  (^[] (dotimes [*count*]
         (list
          (=? cmp a b c)
          (>? cmp a b c)
          (>=? cmp a b c)
          (<? cmp a b c)
          (<=? cmp a b c)))))

(define (ternary-primitive a b c)
  (^[] (dotimes [*count*]
         (list
          (=  a b c)
          (>  a b c)
          (>= a b c)
          (<  a b c)
          (<= a b c)))))

(define (main args)
  ($ time-these/report 1
     `((primitive/2 . ,(binary-primitive 0 1))
       (default/2 . ,(binary default-comparator 0 1))
       (integer/2 . ,(binary integer-comparator 0 1))
       (number/2  . ,(binary number-comparator 0 1))
       (custom1/2 . ,(binary (make-comparator #t eqv? < #f) 0 1))
       (custom2/2 . ,(binary (make-comparator integer? eqv? < #f) 0 1))
       (primitive/3 . ,(ternary-primitive 0 1 2))
       (default/3 . ,(ternary default-comparator 0 1 2))
       (integer/3 . ,(ternary integer-comparator 0 1 2))
       (number/3  . ,(ternary number-comparator 0 1 2))
       (custom1/3 . ,(ternary (make-comparator #t eqv? < #f) 0 1 2))
       (custom2/3 . ,(ternary (make-comparator integer? eqv? < #f) 0 1 2))))
  )


   











    

  


