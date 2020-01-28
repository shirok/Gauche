;;
;; srfi-130 - Cursor-based string library
;;

(define-module srfi-130
  (use srfi-13)
  (export string-index
          ))
(select-module srfi-130)

(define (string-index . args)
  (car (apply (with-module srfi-13 %string-index) args)))
