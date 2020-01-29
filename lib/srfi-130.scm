;;
;; srfi-130 - Cursor-based string library
;;

(define-module srfi-130
  (use srfi-13)
  (export string-fold
          string-fold-right
          string-for-each-cursor
          string-index
          string-index-right
          string-skip
          string-skip-right

          ;; These are from SRFI-13
          string-count
          ))
(select-module srfi-130)

(define (string-index . args)
  (car (apply (with-module srfi-13 %string-index) args)))

(define (string-index-right . args)
  (car (apply (with-module srfi-13 %string-index-right) args)))

(define (string-skip . args)
  (car (apply (with-module srfi-13 %string-skip) args)))

(define (string-skip-right . args)
  (car (apply (with-module srfi-13 %string-skip-right) args)))

(define (string-for-each-cursor proc s :optional
                                (start (string-cursor-start s))
                                (end (string-cursor-end s)))
  (assume-type s <string>)
  (let ([end (string-index->cursor s end)])
    (let loop ([cur (string-index->cursor s start)])
      (unless (string-cursor=? cur end)
        (proc cur)
        (loop (string-cursor-next s cur))))))
