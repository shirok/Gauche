;;;
;;; srfi-152 - String Library (reduced)
;;;

(define-module srfi-152
  (use srfi-13)
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

(define %subs (with-module gauche.internal %maybe-substring))

;; return string pointer
(define-inline (%string-span-ptr s pred negate reverse?)
  (let ([p ((with-module srfi-13 %get-char-pred) pred)]
        [sp (make-string-pointer s (if reverse? -1 0))]
        [move! (if reverse? string-pointer-prev! string-pointer-next!)])
    (when reverse? (string-pointer-prev! sp))
    (let loop ()
      (let1 c (string-pointer-ref sp)
        (cond [(eof-object? c) sp]
              [(negate (p pred c))
               (when reverse? (string-pointer-next! sp))
               sp]
              [else (move! sp) (loop)])))))
  
(define (string-take-while s pred :optional (start 0) (end -1))
  (assume-type s <string>)
  (let* ([ss (%subs s start end)]
         [sp (%string-span-ptr ss pred not #f)])
    (string-pointer-substring sp)))
(define (string-take-while-right s pred :optional (start 0) (end -1))
  (assume-type s <string>)
  (let* ([ss (%subs s start end)]
         [sp (%string-span-ptr ss pred not #t)])
    (string-pointer-substring sp :after #t)))
(define (string-drop-while s pred :optional (start 0) (end -1))
  (assume-type s <string>)
  (let* ([ss (%subs s start end)]
         [sp (%string-span-ptr ss pred not #f)])
    (string-pointer-substring sp :after #t)))
(define (string-drop-while-right s pred :optional (start 0) (end -1))
  (assume-type s <string>)
  (let* ([ss (%subs s start end)]
         [sp (%string-span-ptr ss pred not #t)])
    (string-pointer-substring sp)))
(define (string-span s pred :optional (start 0) (end -1))
  (assume-type s <string>)
  (let* ([ss (%subs s start end)]
         [sp (%string-span-ptr ss pred not #f)])
    (values (string-pointer-substring sp)
            (string-pointer-substring sp :after #t))))
(define (string-break s pred :optional (start 0) (end -1))
  (assume-type s <string>)
  (let* ([ss (%subs s start end)]
         [sp (%string-span-ptr ss pred identity #f)])
    (values (string-pointer-substring sp)
            (string-pointer-substring sp :after #t))))

(define (string-segment s k)
  (let loop ([r '()] [s s])
    (if (< (string-length s) k)
      (if (equal? s "")
        (reverse r)
        (reverse (cons s r)))
      (loop (cons (string-copy s 0 k) r)
            (string-copy s k)))))

(define (string-contains-right s1 s2 :optional start1 (end1 -1) start2 end2)
  ;; This looks terrible---but once we reverse the strings, we can take
  ;; advantage of internal fast string-scan.  It is arguable whether writing
  ;; yet-another search for reverse direction might be better or not.
  (let ([rs1 (string-reverse (%subs s1 start1 end1))]
        [rs2 (string-reverse (%subs s2 start2 end2))])
    (and-let1 z (string-contains rs1 rs2)
      (- (if (< end1 0) (string-length s1) end1)
         z
         (string-length rs2)))))

;; Compatibility
(define string-remove string-delete)
(define string-replicate xsubstring)



