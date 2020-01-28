;;
;; srfi-130 - Cursor-based string library
;;

(define-module srfi-130
  (use srfi-13)
  (export string-contains
          string-fold
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

(define %maybe-substring (with-module gauche.internal %maybe-substring))

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

(define (string-contains s1 s2 :optional (start1 0) end1 start2 end2)
  (assume-type s1 <string>)
  (assume-type s2 <string>)
  (let* ((str1 (%maybe-substring s1 start1 end1))
         (str2 (%maybe-substring s2 start2 end2))
         (res  (string-scan str1 str2 'cursor)))
    ;; This only works because substring 'str1' shares the same space
    ;; as the original string 's1', and <string-cursor> just holds a C
    ;; pointer. If any of that changes, we'll need to convert 'res'
    ;; (of 'str1') to the cursor of 's1'.
    res))
