;;;
;;; srfi-152 - String Library (reduced)
;;;

(define-module srfi-152
  (use srfi-13)
  ;; Note, 130 version returns a cursor, not an index
  (use srfi-130
       :only (string-index string-index-right)
       :rename ((string-index %string-index)
                (string-index-right %string-index-right)))
  (use gauche.unicode)
  (export
   ;;
   string? make-string string
   string->vector string->list list->string vector->string
   string-length string-ref substring string-copy
   string=? string<? string>? string<=? string>=?
   string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
   string-append string-map string-for-each
   read-string write-string
   string-set! string-fill! string-copy!
   string=? string<? string>? string<=? string>=?
   string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?

   string-null? string-every string-any
   string-tabulate string-unfold string-unfold-right
   reverse-list->string

   string-take string-drop string-take-right string-drop-right
   string-pad string-pad-right
   string-trim string-trim-right string-trim-both
   string-replace
   string-prefix-length string-suffix-length
   string-prefix? string-suffix?
   string-index string-index-right string-skip string-skip-right
   string-contains string-contains-right
   string-take-while string-take-while-right
   string-drop-while string-drop-while-right
   string-break string-span
   string-append string-concatenate string-concatenate-reverse
   string-join
   string-fold string-fold-right string-count
   string-filter string-remove
   string-replicate string-segment string-split))
(select-module srfi-152)

(define (%negate pred)
  (lambda (x)
    (not (pred x))))

(define (string-take-while s pred :optional
                           (start 0)
                           (end (string-cursor-end s)))
  (substring s start (%string-index s (%negate pred) start end)))

(define (string-take-while-right s pred :optional
                                 (start 0)
                                 (end (string-cursor-end s)))
  (substring s (%string-index-right s (%negate pred) start end) end))

(define (string-span s pred :optional
                     (start 0)
                     (end (string-cursor-end s)))
  (let ([cur (%string-index s (%negate pred) start end)])
    (values (substring s start cur)
            (substring s cur end))))

(define (string-break s pred :optional
                      (start 0)
                      (end (string-cursor-end s)))
  (let ([cur (%string-index s pred start end)])
    (values (substring s start cur)
            (substring s cur end))))

(define (string-segment s k)
  (unless (and (exact-integer? k)
               (positive? k))
    (error "positive exact integer required, but got:" k))
  (let loop ([r '()] [s s])
    (if (< (string-length s) k)
      (if (equal? s "")
        (reverse r)
        (reverse (cons s r)))
      (loop (cons (string-copy s 0 k) r)
            (string-copy s k)))))

(define (string-contains-right s1 s2 :optional (start1 0) end1 start2 end2)
  (let* ((str1 (opt-substring s1 start1 end1))
         (str2 (opt-substring s2 start2 end2))
         (res  (string-scan-right str1 str2)))
    (and res (+ start1 res))))

;; Compatibility
(define string-drop-while string-trim)
(define string-drop-while-right string-trim-right)
(define string-remove string-delete)

;; 'to' is not optional in srfi-152
(define (string-replicate s from to :optional start end)
  (xsubstring s from to start end))
