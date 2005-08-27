;;
;; If you compile load.c with SHOW_LOAD_TIMINGS defined,
;; gosh dumps the timing information about load when it exits.
;; This script reads the dump and show the info in comprehensive way.
;;

(use srfi-1)
(use util.match)

(define (main args)
  (define results '())
  
  (port-for-each
   (lambda (form)
     (let rec ((form form))
       (match-let1 (file t0 . more) form
         (let* ((t1 (last more))
                (kids-total (reduce + 0 (map rec (drop-right more 1))))
                (total (- t1 t0)))
           (push! results (cons (- total kids-total) file))
           total))))
   read)
  (for-each (lambda (k)
              (format #t "~10d ~s\n" (car k) (cdr k)))
            (sort results (lambda (a b) (> (car a) (car b)))))
  0)

           
