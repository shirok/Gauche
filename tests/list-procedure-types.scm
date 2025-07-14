;;
;; List types of all procedures in Gauche
;;

(define (show module-name)
  (let1 m (find-module module-name)
    (dolist [n (hash-table-keys (module-table m))]
      (let1 v (module-binding-ref m n #f)
        (when (procedure? v)
          ;; we flush the name first, so that we can detect which one got
          ;; the problem.
          (format #t "~20s  " n)
          (flush)
          (format #t "~s\n" (procedure-type v)))))))

(define *modules*
  '(scheme gauche))

(define (main args)
  (if (null? (cdr args))
    (for-each show *modules*)
    (for-each show (map string->symbol (cdr args))))
  0)
