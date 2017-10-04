(use gauche.time)

(use srfi-155 :prefix srfi-155:)
(use scheme.lazy :prefix lazy:)

(define *count* 0)

(define srfi-155-integers
  (let next ((n 0))
    (srfi-155:delay (cons n (next (+ n 1))))))

(define lazy-integers
  (let next ((n 0))
    (lazy:delay (cons n (next (+ n 1))))))

(define lseq-integers (liota))

(define (xtake stream n)
  (let loop ((s stream) (r '()) (n n))
    (if (= n 0)
      (reverse r)
      (loop (cdr (force s))
            (cons (car (force s)) r)
            (- n 1)))))

(define *cnt* 200)

'($ time-these/report 1
   `((s155 . ,(cut xtake srfi-155-integers *cnt*))
     (lazy . ,(cut xtake lazy-integers (* 1000 *cnt*)))
     (lseq . ,(cut take lseq-integers (* 1000 *cnt*)))
     ))

(print (length (xtake srfi-155-integers *cnt*)))
(print *count*)
