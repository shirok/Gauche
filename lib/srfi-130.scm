;;
;; srfi-130 - Cursor-based string library
;;

(define-module srfi-130
  (use srfi-13)
  (export string-contains
          string-contains-right
          string-fold
          string-fold-right
          string-for-each-cursor
          string-index
          string-index-right
          string-replicate
          string-skip
          string-skip-right
          string-split

          ;; These are from SRFI-13
          reverse-list->string
          string-any
          string-concatenate
          string-concatenate-reverse
          string-count
          string-drop
          string-drop-right
          string-every
          string-join
          string-null?
          string-pad
          string-pad-right
          string-prefix-length
          string-prefix?
          string-replace
          string-reverse
          string-suffix-length
          string-suffix?
          string-tabulate
          string-take
          string-take-right
          string-trim
          string-trim-both
          string-trim-right
          string-unfold
          string-unfold-right
          string-filter

          ;; Aliases
          string->list/cursors
          string->vector/cursors
          string-copy/cursors
          string-ref/cursor
          string-remove
          substring/cursors

          ;; Gauche supports the following functions natively, but
          ;; we re-export them so that they will be available by
          ;; importing srfi-130 into vanilla environment.
          string-cursor->index
          string-cursor-back
          string-cursor-diff
          string-cursor-end
          string-cursor-forward
          string-cursor-next
          string-cursor-prev
          string-cursor-start
          string-cursor<=?
          string-cursor<?
          string-cursor=?
          string-cursor>=?
          string-cursor>?
          string-cursor?
          string-index->cursor
          ))
(select-module srfi-130)

(define string->list/cursors string->list)
(define string->vector/cursors string->vector)
(define string-copy/cursors string-copy)
(define string-ref/cursor string-ref)
(define string-remove string-delete)
(define substring/cursors substring)

(define (string-index . args)
  (values-ref (apply (with-module srfi-13 %string-index) args) 0))

(define (string-index-right . args)
  (values-ref (apply (with-module srfi-13 %string-index-right) args) 0))

(define (string-skip . args)
  (values-ref (apply (with-module srfi-13 %string-skip) args) 0))

(define (string-skip-right . args)
  (values-ref (apply (with-module srfi-13 %string-skip-right) args) 0))

(define (string-for-each-cursor proc s :optional
                                (start 0)
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
  (let* ([str1 (opt-substring s1 start1 end1)]
         [str2 (opt-substring s2 start2 end2)]
         [res  (string-scan str1 str2 'cursor)])
    (and res
         (string-cursor-forward s1
                                (string-index->cursor s1 start1)
                                (string-cursor->index str1 res)))))

(define (string-contains-right s1 s2 :optional (start1 0) end1 start2 end2)
  (assume-type s1 <string>)
  (assume-type s2 <string>)
  (let* ([str1 (opt-substring s1 start1 end1)]
         [str2 (opt-substring s2 start2 end2)]
         [res  (string-scan-right str1 str2 'cursor)])
    (and res
         (string-cursor-forward s1
                                (string-index->cursor s1 start1)
                                (string-cursor->index str1 res)))))

;; 'to' is not optional in srfi-130
(define (string-replicate s from to :optional start end)
  (xsubstring s from to start end))
