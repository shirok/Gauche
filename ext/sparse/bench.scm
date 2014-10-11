(add-load-path ".")

(use data.sparse)
(use util.match)
(use gauche.time)
(use srfi-27)

(define *problem-size* 200000)

(define *num-repeat* (ceiling->exact (/. 10000000 *problem-size*)))

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
    (sparse-vector-set! spv n n)))

(define (sv-ref spv)
  (dolist [n *problem-set*]
    (sparse-vector-ref spv n)))

(define (st-set st)
  (dolist [n *problem-set*]
    (sparse-table-set! st n n)))

(define (st-ref st)
  (dolist [n *problem-set*]
    (sparse-table-ref st n n)))

(define (bench-speed name %make %ref %set %cleanup)
  (let ([null-timer  (make <user-time-counter>)]
        [set-timer   (make <user-time-counter>)]
        [ref-timer   (make <user-time-counter>)])

    (define (calc-time timer)
      (* (/. (- (time-counter-value timer) (time-counter-value null-timer))
             *num-repeat*
             *problem-size*)
         1e9))                          ;nanosec

    (dotimes [i *num-repeat*]
      (with-time-counter null-timer (null))
      (let1 obj (%make)
        (with-time-counter set-timer (%set obj))
        (with-time-counter ref-timer (%ref obj))
        (%cleanup obj)))

    (print name " insertion: " (calc-time set-timer))
    (print name " lookup:    " (calc-time ref-timer))
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
    [("ht" "speed") (bench-speed "Hash table" (cut make-hash-table 'eqv?)
                                 ht-ref ht-set hash-table-clear!)]
    [("sv" "speed") (bench-speed "Sparse vector" (cut make-sparse-vector)
                                 sv-ref sv-set sparse-vector-clear!)]
    [("suv" "speed") (bench-speed "Sparse u32vector" (cut make-sparse-vector 'u32)
                                 sv-ref sv-set sparse-vector-clear!)]
    [("st" "speed") (bench-speed "Sparse table" (cut make-sparse-table 'eqv?)
                                 st-ref st-set sparse-table-clear!)]

    [("ht" "mem") (print "Hash table mem: "
                         (bench-mem (cut ht-set (make-hash-table 'eqv?))))]
    [("sv" "mem") (print "Sparse vector mem: "
                        (bench-mem (cut sv-set (make-sparse-vector))))]
    [("suv" "mem") (print "Sparse u32vector mem: "
                        (bench-mem (cut sv-set (make-sparse-vector 'u32))))]
    [("st" "mem") (print "Sparse table mem: "
                         (bench-mem (cut st-set (make-sparse-table 'eqv?))))]
    [_ (exit 1 "Usage: bench ht|sv|st speed|mem")])
  (print "size: "  *problem-size*)
  0)
