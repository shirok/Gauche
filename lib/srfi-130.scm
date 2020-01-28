;;
;; srfi-130 - Cursor-based string library
;;

(define-module srfi-130
  (use srfi-13)
  (export string-index
          string-index-right
          string-skip
          string-skip-right
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
