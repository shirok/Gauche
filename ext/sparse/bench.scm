(add-load-path ".")

(use util.sparse)
(use util.match)
(use gauche.time)
(use srfi-27)

(define *problem-size* 2000000)

(define *num-repeat* 5)

(define *problem-set*
  (let1 ht (make-hash-table 'eqv?)
    (let loop ((i 0) (r '()))
      (if (= i *problem-size*)
        r
        (let1 k (random-integer (expt 2 32))
          (if (hash-table-exists? ht k)
            (loop i r)
            (loop (+ i 1) (cons k r))))))))

(define (null)
  (dolist [n *problem-set*] n))

(define (ht-set ht)
  (dolist [n *problem-set*]
    (hash-table-put! ht n n)))

(define (ht-ref ht)
  (dolist [n *problem-set*]
    (hash-table-get ht n)))

(define (sv-set spv)
  (dolist [n *problem-set*]
    (spvector-set! spv n n)))

(define (sv-ref spv)
  (dolist [n *problem-set*]
    (spvector-ref spv n)))

(define (st-set st)
  (dolist [n *problem-set*]
    (sptable-set! st n n)))

(define (st-ref st)
  (dolist [n *problem-set*]
    (sptable-ref st n n)))
  
(define (bench-speed)
  (let ([null-timer  (make <user-time-counter>)]
        [htset-timer (make <user-time-counter>)]
        [htref-timer (make <user-time-counter>)]
        [svset-timer (make <user-time-counter>)]
        [svref-timer (make <user-time-counter>)]
        [stset-timer (make <user-time-counter>)]
        [stref-timer (make <user-time-counter>)])

    (define (calc-time timer)
      (* (/. (- (time-counter-value timer) (time-counter-value null-timer))
             *num-repeat*
             *problem-size*)
         1e9))                          ;nanosec

    (dotimes [i *num-repeat*]
      (with-time-counter null-timer (null))
      (let1 ht (make-hash-table 'eqv?)
        (with-time-counter htset-timer (ht-set ht))
        (with-time-counter htref-timer (ht-ref ht))
        (hash-table-clear! ht))
      (let1 sv (make-spvector)
        (with-time-counter svset-timer (sv-set sv))
        (with-time-counter svref-timer (sv-ref sv)))
      (let1 st (make-sptable 'eqv?)
        (with-time-counter stset-timer (st-set st))
        (with-time-counter stref-timer (st-ref st))))

    (print "Hash table insertion: "    (calc-time htset-timer))
    (print "Hash table lookup: "       (calc-time htref-timer))
    (print "Sparse vector insertion: " (calc-time svset-timer))
    (print "Sparse vector lookup: "    (calc-time svref-timer))
    (print "Sparse table insertion: "  (calc-time stset-timer))
    (print "Sparse table lookup: "     (calc-time stref-timer))
    ))

(define (active-memory-size)
  (gc) (gc)
  (let1 s (gc-stat)
    (- (cadr (assq :total-heap-size s)) (cadr (assq :free-bytes s)))))

(define (bench-mem thunk)
  (let1 pre (active-memory-size)
    (dotimes [i *num-repeat*] (thunk))
    (- (active-memory-size) pre)))

(define (main args)
  (match (cdr args)
    [("speed") (bench-speed)]
    [("htmem") (print "Hash table mem: "
                      (bench-mem (lambda () (ht-set (make-hash-table 'eqv?)))))]
    [("svmem") (print "Sparse vector mem: "
                      (bench-mem (lambda () (sv-set (make-spvector)))))]
    [("stmem") (print "Sparse table mem: "
                      (bench-mem (lambda () (st-set (make-sptable 'eqv?)))))]
    [_ (print "Usage: bench speed|htmem|svmem|stmem")])
  0)
  
    
    
       



  
    
  
