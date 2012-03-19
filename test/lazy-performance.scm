;;;
;;; Some performance test for lazy sequences.
;;;

(use gauche.time)
(use gauche.generator)
(use gauche.lazy)
(use file.util)
(use srfi-1)
(use srfi-42)

;; Compare crating list of integers (0 ... N-1) with
;;  - simple loop
;;  - srfi-42 list-ec
;;  - generator
;;  - lazy sequence
(define (integer-sequence n)
  (define (simple n)
    (let loop ([k 0] [r '()])
      (if (>= k n)
        (reverse r)
        (loop (+ k 1) (cons k r)))))
  (define (ec n)
    (list-ec (: k n) k))
  (define (genr n)
    (let1 g (giota n)
      (let loop ([k (g)] [r '()])
        (if (eof-object? k)
          (reverse r)
          (loop (g) (cons k r))))))
  (define (laz n)
    (lrange 0 n))

  (time-these/report
   '(cpu 5)
   `((lazy-seq    . ,(^[] (length (laz n))))
     (generator   . ,(^[] (length (genr n))))
     (list-ec     . ,(^[] (length (ec n))))
     (simple-loop . ,(^[] (length (simple n))))))
  )

#|
(integer-sequence 1000000)
|#

;; This compares eagerly created temporary list vs lazy seqeucne.
;; Eagerly creating temp list supposedly stresses GC.

(define (folding1 n)
  (define (eager n)
    (fold + 0 (iota n)))
  (define (lazy n)
    (fold + 0 (lrange 0 n)))
  (time-these/report
   '(cpu 10)
   `((lazy-seq     . ,(^[] (lazy n)))
     (eager-iota   . ,(^[] (eager n))))))

#|
(folding1 10000000)
|#

;; Reading file

(define (file f)
  (define (basic)
    (with-input-from-file f
      (^[] (let loop ([c (read-char)] [cnt 0])
             (cond [(eof-object? c) cnt]
                   [(char-whitespace? c) (loop (read-char) cnt)]
                   [else (loop (read-char) (+ cnt 1))])))))
  (define (eager)
    (call-with-input-file f
      (^p (count char-whitespace? (port->list read-char p)))))
  (define (lazy)
    (with-input-from-file f
      (cut count char-whitespace? (lseq read-char))))
  
  (with-input-from-file f (cut generator-for-each identity read-char)) ;fill buffer
  (time-these/report
   '(cpu 5)
   `((lazy-seq   . ,lazy)
     (eager      . ,eager)
     (basic      . ,basic))))

#|
(file "./compile.c")
|#
