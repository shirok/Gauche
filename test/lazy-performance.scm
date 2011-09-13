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
   `((lazy-seq    . ,(lambda () (length (laz n))))
     (generator   . ,(lambda () (length (genr n))))
     (list-ec     . ,(lambda () (length (ec n))))
     (simple-loop . ,(lambda () (length (simple n))))))
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
   `((lazy-seq     . ,(lambda () (lazy n)))
     (eager-iota   . ,(lambda () (eager n))))))

#|
(folding1 10000000)
|#

;; Reading file

(define (file f)
  (define (eager)
    (with-input-from-file f
      (^()
        (let1 cs (port-map identity read-char)
          (length cs)))))
  (define (lazy)
    (with-input-from-file f
      (^()
        (let1 cs (lseq read-char)
          (length cs)))))

  (file->string-list f) ;; to fill the file buffer
  (time-these/report
   '(cpu 5)
   `((lazy-seq   . ,lazy)
     (eager      . ,eager))))

#|
(file "./compile.c")
|#
