;;;
;;; srfi-13.scm - string library
;;;
;;;   Copyright (c) 2000-2019  Shiro Kawai  <shiro@acm.org>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(define-module srfi-13
  (export string-null? string-every string-any
          string-tabulate reverse-list->string
          substring/shared string-copy!
          string-take string-take-right
          string-drop string-drop-right
          string-pad string-pad-right
          string-trim string-trim-right string-trim-both
          string-compare string-compare-ci
          string= string<> string< string> string<= string>=
          string-ci= string-ci<> string-ci< string-ci> string-ci<= string-ci>=
          string-hash string-hash-ci
          string-prefix-length string-suffix-length
          string-prefix-length-ci string-suffix-length-ci
          string-prefix? string-suffix?
          string-prefix-ci? string-suffix-ci?
          string-index string-index-right
          string-skip string-skip-right
          string-count string-contains string-contains-ci
          string-titlecase string-titlecase!
          string-upcase string-upcase!
          string-downcase string-downcase!
          string-reverse string-reverse!
          string-concatenate string-append/shared string-concatenate/shared
          string-concatenate-reverse string-concatenate-reverse/shared
          string-map string-map!
          string-fold string-fold-right
          string-unfold string-unfold-right
          string-for-each string-for-each-index
          xsubstring string-xcopy!
          string-replace string-tokenize
          string-filter string-delete
          string-parse-start+end string-parse-final-start+end
          let-string-start+end
          check-substring-spec substring-spec-ok?
          make-kmp-restart-vector
          kmp-step
          string-kmp-partial-search

          ;; Gauche supports the following functions natively, but
          ;; we re-export them so that they will be available by
          ;; importing srfi-13 into vanilla environment.
          string? make-string string string->list list->string
          string-join string-length string-ref string-copy
          string-set! string-fill!
          string-append
          ))
(select-module srfi-13)

;; Utility macro to handle char/char-set/pred match.
;; To avoid closure creation in inner loop, this returns a static procedure
;; that takes char/char-set/pred and char.
;; NB: This is not exported, but shared with srfi-152.
(define-inline (%get-char-pred char/char-set/pred)
  (cond [(char? char/char-set/pred) char=?]
        [(char-set? char/char-set/pred) char-set-contains?]
        [(procedure? char/char-set/pred) %char-pred/pred]
        [else (error "argument needs to be either a character, a char-set, \
                      or a procedure:" char/char-set/pred)]))

(define (%char-pred/pred c/s/p x) (c/s/p x))

(define %maybe-substring (with-module gauche.internal %maybe-substring))
(define %hash-string (with-module gauche.internal %hash-string))
(define %string-replace-body! (with-module gauche.internal %string-replace-body!))
;;;
;;; Predicates
;;;

(define (string-null? str)
  (assume-type str <string>)
  (equal? "" str))

(define (string-every c/s/p s . args)
  (let* ((src  (open-input-string (apply %maybe-substring s args)))
         (pred (%get-char-pred c/s/p)))
    (let loop ((ch (read-char src))
               (r  #t))
      (cond ((not r) #f)
            ((eof-object? ch) r)
            (else (loop (read-char src) (pred c/s/p ch)))))))

(define (string-any c/s/p s . args)
  (let* ((src  (open-input-string (apply %maybe-substring s args)))
         (pred (%get-char-pred c/s/p)))
    (let loop ((ch (read-char src)))
      (cond ((eof-object? ch) #f)
            ((pred c/s/p ch))
            (else (loop (read-char src)))))))

;;;
;;; Generators
;;;

(define (string-tabulate proc len)
  (check-arg (^l (and (integer? l) (>= l 0))) len)
  (let ((sink (open-output-string)))
    (do ((i 0 (+ i 1)))
        ((>= i len) (get-output-string sink))
      (write-char (proc i) sink))))

(define (reverse-list->string char-list)
  (list->string (reverse char-list)))

;;;
;;; Selectors
;;;

(define substring/shared string-copy)  ; same in Gauche

(define (string-copy! target tstart s . args)
  (assume-type target <string>)
  (check-arg (^x (and (integer? x) (>= x 0))) tstart)
  (let* ((str (apply %maybe-substring s args))
         (slen (string-length str))
         (tlen (string-length target)))
    (when (> (+ tstart slen) tlen)
      (error "copy operation runs off the target string:" target))
    (if (= slen tlen)
      (%string-replace-body! target str)
      (%string-replace-body! target
                             (string-append (substring target 0 tstart)
                                            str
                                            (substring target
                                                       (+ tstart slen)
                                                       tlen))))))

(define (string-pad s len :optional (char #\space) start end)
  (assume-type char <char>)
  (let* ((str (%maybe-substring s start end))
         (slen (string-length str)))
    (cond ((< slen len)
           (string-append (make-string (- len slen) char) str))
          ((> slen len)
           (string-take-right str len))
          (else str))))

(define (string-pad-right s len :optional (char #\space) start end)
  (assume-type char <char>)
  (let* ((str (%maybe-substring s start end))
         (slen (string-length str)))
    (cond ((< slen len)
           (string-append str (make-string (- len slen) char)))
          ((> slen len)
           (string-take str len))
          (else str))))

(define (string-take s nchars)
  (assume-type s <string>)
  (when (or (< nchars 0) (> nchars (string-length s)))
    (error "argument out of range:" nchars))
  (%maybe-substring s 0 nchars))

(define (string-drop s nchars)
  (assume-type s <string>)
  (when (or (< nchars 0) (> nchars (string-length s)))
    (error "argument out of range:" nchars))
  (%maybe-substring s nchars))

(define (string-take-right s nchars)
  (assume-type s <string>)
  (when (or (< nchars 0) (> nchars (string-length s)))
    (error "argument out of range:" nchars))
  (%maybe-substring s (- (string-length s) nchars)))

(define (string-drop-right s nchars)
  (assume-type s <string>)
  (when (or (< nchars 0) (> nchars (string-length s)))
    (error "argument out of range:" nchars))
  (%maybe-substring s 0 (- (string-length s) nchars)))

(define (string-trim s :optional
                     (c/s/p #[\s])
                     (start 0)
                     (end (string-cursor-end s)))
  (substring s (car (%string-skip s c/s/p start end)) end))

(define (string-trim-right s :optional
                           (c/s/p #[\s])
                           (start 0)
                           (end (string-cursor-end s)))
  (substring s start (car (%string-skip-right s c/s/p start end))))

(define (string-trim-both s :optional
                          (c/s/p #[\s])
                          (start 0)
                          (end (string-cursor-end s)))
  (let ([new-start (car (%string-skip s c/s/p start end))])
    (substring s new-start (car (%string-skip-right s c/s/p new-start end)))))

;;;
;;; Comparison functions
;;;

(define (%string-compare-int off str1 str2 cmp< cmp> proc< proc= proc>)
  (let ((end1 (string-cursor-end str1))
        (end2 (string-cursor-end str2)))
    (let loop ((cur1 (string-cursor-start str1))
               (cur2 (string-cursor-start str2))
               (off off))
      (cond ((string-cursor=? cur1 end1)
             (if (string-cursor=? cur2 end2) (proc= off) (proc< off)))
            ((string-cursor=? cur2 end2)
             (proc> off))
            ((cmp< (string-ref str1 cur1) (string-ref str2 cur2))
             (proc< off))
            ((cmp> (string-ref str1 cur1) (string-ref str2 cur2))
             (proc> off))
            (else (loop (string-cursor-next str1 cur1)
                        (string-cursor-next str2 cur2)
                        (+ off 1)))))))

(define (string-compare s1 s2 proc< proc= proc>
                        :optional (start1 0) end1 start2 end2)
  (assume-type s1 <string>)
  (assume-type s2 <string>)
  (let ((str1 (%maybe-substring s1 start1 end1))
        (str2 (%maybe-substring s2 start2 end2)))
    (%string-compare-int start1 str1 str2
                         char<? char>?
                         proc< proc= proc>)))

(define (string-compare-ci s1 s2 proc< proc= proc>
                           :optional (start1 0) end1 start2 end2)
  (assume-type s1 <string>)
  (assume-type s2 <string>)
  (let ((str1 (%maybe-substring s1 start1 end1))
        (str2 (%maybe-substring s2 start2 end2)))
    (%string-compare-int start1 str1 str2
                         char-ci<? char-ci>?
                         proc< proc= proc>)))

(define (string= s1 s2 . args)
  (assume-type s1 <string>)
  (assume-type s2 <string>)
  (apply string-compare
         s1 s2 (^_ #f) (^_ #t) (^_ #f)
         args))

(define (string<> s1 s2 . args)
  (assume-type s1 <string>)
  (assume-type s2 <string>)
  (apply string-compare
         s1 s2 (^_ #t) (^_ #f) (^_ #t)
         args))

(define (string< s1 s2 . args)
  (assume-type s1 <string>)
  (assume-type s2 <string>)
  (apply string-compare
         s1 s2 (^_ #t) (^_ #f) (^_ #f)
         args))

(define (string<= s1 s2 . args)
  (assume-type s1 <string>)
  (assume-type s2 <string>)
  (apply string-compare
         s1 s2 (^_ #t) (^_ #t) (^_ #f)
         args))

(define (string> s1 s2 . args)
  (assume-type s1 <string>)
  (assume-type s2 <string>)
  (apply string-compare
         s1 s2 (^_ #f) (^_ #f) (^_ #t)
         args))

(define (string>= s1 s2 . args)
  (assume-type s1 <string>)
  (assume-type s2 <string>)
  (apply string-compare
         s1 s2 (^_ #f) (^_ #t) (^_ #t)
         args))

(define (string-ci= s1 s2 . args)
  (assume-type s1 <string>)
  (assume-type s2 <string>)
  (apply string-compare-ci
         s1 s2 (^_ #f) (^_ #t) (^_ #f)
         args))

(define (string-ci<> s1 s2 . args)
  (assume-type s1 <string>)
  (assume-type s2 <string>)
  (apply string-compare-ci
         s1 s2 (^_ #t) (^_ #f) (^_ #t)
         args))

(define (string-ci< s1 s2 . args)
  (assume-type s1 <string>)
  (assume-type s2 <string>)
  (apply string-compare-ci
         s1 s2 (^_ #t) (^_ #f) (^_ #f)
         args))

(define (string-ci<= s1 s2 . args)
  (assume-type s1 <string>)
  (assume-type s2 <string>)
  (apply string-compare-ci
         s1 s2 (^_ #t) (^_ #t) (^_ #f)
         args))

(define (string-ci> s1 s2 . args)
  (assume-type s1 <string>)
  (assume-type s2 <string>)
  (apply string-compare-ci
         s1 s2 (^_ #f) (^_ #f) (^_ #t)
         args))

(define (string-ci>= s1 s2 . args)
  (assume-type s1 <string>)
  (assume-type s2 <string>)
  (apply string-compare-ci
         s1 s2 (^_ #f) (^_ #t) (^_ #t)
         args))

;;;
;;; Hash functions
;;;

(define (string-hash s :optional bound start end)
  (assume-type s <string>)
  (%hash-string (%maybe-substring s start end) bound))

(define (string-hash-ci s :optional bound start end)
  (assume-type s <string>)
  ;; oops! optimization required!
  (%hash-string (string-upcase (%maybe-substring s start end)) bound))

;;;
;;; Prefix and suffix
;;;

(define (%string-prefix-int str1 str2 =)
  (let ((end1 (string-cursor-end str1))
        (end2 (string-cursor-end str2)))
    (let loop ((cur1 (string-cursor-start str1))
               (cur2 (string-cursor-start str2))
               (count 0))
      (if (and (string-cursor<? cur1 end1)
               (string-cursor<? cur2 end2)
               (= (string-ref str1 cur1) (string-ref str2 cur2)))
        (loop (string-cursor-next str1 cur1)
              (string-cursor-next str2 cur2)
              (+ count 1))
        count))))

(define (string-prefix-length s1 s2 :optional start1 end1 start2 end2)
  (assume-type s1 <string>)
  (assume-type s2 <string>)
  (let ((str1 (%maybe-substring s1 start1 end1))
        (str2 (%maybe-substring s2 start2 end2)))
    (%string-prefix-int str1 str2 char=?)))

(define (string-prefix-length-ci s1 s2 :optional start1 end1 start2 end2)
  (assume-type s1 <string>)
  (assume-type s2 <string>)
  (let ((str1 (%maybe-substring s1 start1 end1))
        (str2 (%maybe-substring s2 start2 end2)))
    (%string-prefix-int str1 str2 char-ci=?)))

(define (string-prefix? s1 s2 :optional start1 end1 start2 end2)
  (assume-type s1 <string>)
  (assume-type s2 <string>)
  (let ((str1 (%maybe-substring s1 start1 end1))
        (str2 (%maybe-substring s2 start2 end2)))
    (= (%string-prefix-int str1 str2 char=?)
       (string-length str1))))

(define (string-prefix-ci? s1 s2 :optional start1 end1 start2 end2)
  (assume-type s1 <string>)
  (assume-type s2 <string>)
  (let ((str1 (%maybe-substring s1 start1 end1))
        (str2 (%maybe-substring s2 start2 end2)))
    (= (%string-prefix-int str1 str2 char-ci=?)
       (string-length str1))))

(define (%string-suffix-int str1 str2 =)
  (let ((start1 (string-cursor-start str1))
        (start2 (string-cursor-start str2)))
    (let loop ((cur1 (string-cursor-end str1))
               (cur2 (string-cursor-end str2))
               (count 0))
      (if (and (string-cursor>? cur1 start1)
               (string-cursor>? cur2 start2))
        (let ([new-cur1 (string-cursor-prev str1 cur1)]
              [new-cur2 (string-cursor-prev str2 cur2)])
          (if (= (string-ref str1 new-cur1)
                 (string-ref str2 new-cur2))
            (loop new-cur1 new-cur2 (+ count 1))
            count))
        count))))

(define (string-suffix-length s1 s2 :optional start1 end1 start2 end2)
  (assume-type s1 <string>)
  (assume-type s2 <string>)
  (let ((str1 (%maybe-substring s1 start1 end1))
        (str2 (%maybe-substring s2 start2 end2)))
    (%string-suffix-int str1 str2 char=?)))

(define (string-suffix-length-ci s1 s2 :optional start1 end1 start2 end2)
  (assume-type s1 <string>)
  (assume-type s2 <string>)
  (let ((str1 (%maybe-substring s1 start1 end1))
        (str2 (%maybe-substring s2 start2 end2)))
    (%string-suffix-int str1 str2 char-ci=?)))

(define (string-suffix? s1 s2 :optional start1 end1 start2 end2)
  (assume-type s1 <string>)
  (assume-type s2 <string>)
  (let ((str1 (%maybe-substring s1 start1 end1))
        (str2 (%maybe-substring s2 start2 end2)))
    (= (%string-suffix-int str1 str2 char=?)
       (string-length str1))))

(define (string-suffix-ci? s1 s2 :optional start1 end1 start2 end2)
  (assume-type s1 <string>)
  (assume-type s2 <string>)
  (let ((str1 (%maybe-substring s1 start1 end1))
        (str2 (%maybe-substring s2 start2 end2)))
    (= (%string-suffix-int str1 str2 char-ci=?)
       (string-length str1))))

;;;
;;; Search
;;;

;; these % variants return a pair of result and end cursor
(define (%string-index s c/s/p
                       :optional
                       (start 0)
                       (end (string-cursor-end s)))
  (assume-type s <string>)
  (let ([pred (%get-char-pred c/s/p)]
        [end (string-index->cursor s end)])
    (let loop ([cur (string-index->cursor s start)])
      (if (or (string-cursor=? cur end)
              (pred c/s/p (string-ref s cur)))
          (cons cur end)
          (loop (string-cursor-next s cur))))))

(define (%string-index-right s c/s/p
                             :optional
                             (start 0)
                             (end (string-cursor-end s)))
  (assume-type s <string>)
  (let ([pred (%get-char-pred c/s/p)]
        [start (string-index->cursor s start)])
    (let loop ([cur (string-index->cursor s end)])
      (if (string-cursor>? cur start)
          (let ([prev (string-cursor-prev s cur)])
            (if (pred c/s/p (string-ref s prev))
                (cons cur start)
                (loop prev)))
          (cons cur start)))))

(define (%string-skip s c/s/p
                      :optional
                      (start 0)
                      (end (string-cursor-end s)))
  (assume-type s <string>)
  (let ([pred (%get-char-pred c/s/p)]
        [end (string-index->cursor s end)])
    (let loop ([cur (string-index->cursor s start)])
      (if (and (string-cursor<? cur end)
               (pred c/s/p (string-ref s cur)))
          (loop (string-cursor-next s cur))
          (cons cur end)))))

(define (%string-skip-right s c/s/p
                            :optional
                            (start 0)
                            (end (string-cursor-end s)))
  (assume-type s <string>)
  (let ([pred (%get-char-pred c/s/p)]
        [start (string-index->cursor s start)])
    (let loop ([cur (string-index->cursor s end)])
      (if (string-cursor>? cur start)
          (let ([prev (string-cursor-prev s cur)])
            (if (pred c/s/p (string-ref s prev))
                (loop prev)
                (cons cur start)))
          (cons cur start)))))

(define (string-index s c/s/p . args)
  (let ([result (apply %string-index s c/s/p args)])
    (if (string-cursor=? (car result) (cdr result))
        #f
        (string-cursor->index s (car result)))))

(define (string-index-right s c/s/p . args)
  (let ([result (apply %string-index-right s c/s/p args)])
    (if (string-cursor=? (car result) (cdr result))
        #f
        (- (string-cursor->index s (car result)) 1))))

(define (string-skip s c/s/p . args)
  (let ([result (apply %string-skip s c/s/p args)])
    (if (string-cursor=? (car result) (cdr result))
        #f
        (string-cursor->index s (car result)))))

(define (string-skip-right s c/s/p . args)
  (let ([result (apply %string-skip-right s c/s/p args)])
    (if (string-cursor=? (car result) (cdr result))
        #f
        (- (string-cursor->index s (car result)) 1))))

(define (string-count s c/s/p
                      :optional
                      (start 0)
                      (end (string-cursor-end s)))
  (assume-type s <string>)
  (let ((pred (%get-char-pred c/s/p))
        (end (string-index->cursor s end)))
    (let loop ((cur (string-index->cursor s start))
               (count 0))
      (if (string-cursor=? cur end)
          count
          (loop (string-cursor-next s cur)
                (if (pred c/s/p (string-ref s cur))
                    (+ count 1)
                    count))))))

(define (string-contains s1 s2 :optional (start1 0) end1 start2 end2)
  (assume-type s1 <string>)
  (assume-type s2 <string>)
  (let* ((str1 (%maybe-substring s1 start1 end1))
         (str2 (%maybe-substring s2 start2 end2))
         (res  (string-scan str1 str2)))
    (and res (+ start1 res))))

;; not tuned (maybe to be moved to native)
(define (string-contains-ci s1 s2 :optional (start1 0) end1 start2 end2)
  (assume-type s1 <string>)
  (assume-type s2 <string>)
  (let* ((str1 (string-upcase (%maybe-substring s1 start1 end1)))
         (str2 (string-upcase (%maybe-substring s2 start2 end2)))
         (res  (string-scan str1 str2)))
    (and res (+ start1 res))))

;;;
;;; Case mapping
;;;

(define (string-upcase s . args)
  (let ((src (open-input-string (apply %maybe-substring s args)))
        (dst (open-output-string)))
    (let loop ((ch (read-char src)))
      (if (eof-object? ch)
          (get-output-string dst)
          (begin (write-char (char-upcase ch) dst)
                 (loop (read-char src)))))))

(define (string-downcase s . args)
  (let ((src (open-input-string (apply %maybe-substring s args)))
        (dst (open-output-string)))
    (let loop ((ch (read-char src)))
      (if (eof-object? ch)
          (get-output-string dst)
          (begin (write-char (char-downcase ch) dst)
                 (loop (read-char src)))))))

(define *%cased-char-set* #[A-Za-z]) ;; fixme

(define (string-titlecase s . args)
  (let ((src (open-input-string (apply %maybe-substring s args)))
        (dst (open-output-string)))
    (let loop ((title? #t)
               (ch   (read-char src)))
      (cond ((eof-object? ch) (get-output-string dst))
            ((char-set-contains? *%cased-char-set* ch)
             (if title?
               (write-char (char-upcase ch) dst) ;; This should be char-titlecase
               (write-char (char-downcase ch) dst))
             (loop #f (read-char src)))
            (else
             (write-char ch dst)
             (loop #t (read-char src))))
      )))

(define (%string-*case! converter)
  (lambda (s :optional (start 0) (end (string-length s)))
    (if (and (= start 0) (= end (string-length s)))
      (%string-replace-body! s (converter s))
      (string-copy! s start (converter s start end)))))

(define string-upcase!    (%string-*case! string-upcase))
(define string-downcase!  (%string-*case! string-downcase))
(define string-titlecase! (%string-*case! string-titlecase))

;;;
;;; Reverse & append
;;;

(define (string-reverse s :optional
                        (start 0)
                        (end (string-cursor-end s)))
  (let ((start (string-index->cursor s start))
        (dst (open-output-string)))
    (let loop ((cur (string-index->cursor s end)))
      (if (string-cursor=? cur start)
        (get-output-string dst)
        (let ([new-cur (string-cursor-prev s cur)])
          (write-char (string-ref s new-cur) dst)
          (loop new-cur))))))

(define (string-reverse! s . args)
  (let ((rev (apply string-reverse s args)))
    (string-copy! s (get-optional args 0) rev)))

(define (string-concatenate lis)
  (cond
   ((null? lis) "")
   ((not (pair? lis))
    (error "string-concatenate: argument ouf of domain:" lis))
   ((and (null? (cdr lis)) (string? (car lis)))
    (string-copy (car lis)))
   (else
    (let loop ((l lis)
               (out (open-output-string :private? #t))
               (incomplete? #f))
      (if (pair? l)
        (let ((e (car l)))
          (unless (string? e)
            (error "string-concatenate: argument contains non-string:" e))
          (display e out)
          (loop (cdr l) out (or incomplete? (string-incomplete? e))))
        (if incomplete?
          (string-complete->incomplete (get-output-string out))
          (get-output-string out)))))))

(define string-concatenate/shared string-concatenate)

(define string-append/shared string-append)

(define (string-concatenate-reverse list . args)
  (cond ((null? args)
         (string-concatenate (reverse list)))
        ((null? (cdr args))
         (string-concatenate (reverse (cons (car args) list))))
        (else
         (string-concatenate (reverse (cons (string-take (car args)
                                                         (cadr args))
                                            list))))))

(define string-concatenate-reverse/shared
  string-concatenate-reverse)

;;;
;;; Mappers
;;;

(define (string-map proc s :optional
                    (start 0)
                    (end (string-cursor-end s)))
  (assume-type s <string>)
  (let ((end  (string-index->cursor s end))
        (dest (open-output-string)))
    (let loop ((cur (string-index->cursor s start)))
      (if (string-cursor=? cur end)
        (get-output-string dest)
        (begin
          (write-char (proc (string-ref s cur)) dest)
          (loop (string-cursor-next s cur)))))))

(define (string-map! proc s . args)
  (assume-type s <string>)
  (let ((mapped (apply string-map proc s args)))
    (string-copy! s (get-optional args 0) mapped)))

(define (string-fold kons knil s
                     :optional
                     (start 0)
                     (end (string-cursor-end s)))
  (assume-type s <string>)
  (let ([end (string-index->cursor s end)])
    (let loop ([cur (string-index->cursor s start)]
               [r knil])
      (if (string-cursor=? cur end)
        r
        (loop (string-cursor-next s cur)
              (kons (string-ref s cur) r))))))

(define (string-fold-right kons knil s
                           :optional
                           (start 0)
                           (end (string-cursor-end s)))
  (assume-type s <string>)
  (let ([start (string-index->cursor s start)])
    (let loop ([cur (string-index->cursor s end)]
               [r knil])
      (if (string-cursor=? cur start)
        r
        (let ([new-cur (string-cursor-prev s cur)])
          (loop new-cur
                (kons (string-ref s new-cur) r)))))))

(define (string-unfold p f g seed
                       :optional (base "") (make-final (^_ "")))
  (let ((dest (open-output-string)))
    (display base dest)
    (let loop ((seed seed))
      (if (p seed)
        (begin (display (make-final seed) dest)
               (get-output-string dest))
        (begin (write-char (f seed) dest)
               (loop (g seed)))))))

(define (string-unfold-right p f g seed
                             :optional (base "") (make-final (^_ "")))
  (let ((dest (open-output-string)))
    (let loop ((seed seed))
      (if (p seed)
        (string-append (make-final seed)
                       (string-reverse (get-output-string dest))
                       base)
        (begin (write-char (f seed) dest)
               (loop (g seed)))))))

(define (string-for-each proc s
                         :optional
                         (start 0)
                         (end (string-cursor-end s)))
  (assume-type s <string>)
  (let ([end (string-index->cursor s end)])
    (let loop ([cur (string-index->cursor s start)])
      (unless (string-cursor=? cur end)
        (proc (string-ref s cur))
        (loop (string-cursor-next s cur))))))

(define (string-for-each-index proc s :optional (start 0) (end (string-length s)))
  (assume-type s <string>)
  (do ((i start (+ i 1)))
      ((>= i end))
    (proc i)))

;;;
;;; Rotator
;;;

(define (xsubstring s from :optional to start end)
  (assume-type s <string>)
  (check-arg (^x (and (integer? x) (exact? x))) from)
  (let* ([str (%maybe-substring s start end)]
         [len (string-length str)]
         [dest (open-output-string)])
    ;; Fill TO if it's not provided; also check the validity.
    (cond [(undefined? to) (set! to (+ from len))]
          [(not (and (integer? to) (exact? to)))
           (error "argument out of domain:" to)]
          [(= len 0)
           (error "zero length source string is not allowed")]
          [(< to from)
           (errorf "argument out of range (from, to): (~s, ~s)" from to)])
    ;; Adjust the range
    (let* ([from. (modulo from len)]
           [to. (- to (- from from.))])
      (let ([start-cur (string-cursor-start str)]
            [end-cur (string-cursor-end str)])
        (let loop ([count from.]
                   [cur (string-index->cursor str from.)])
          (cond [(>= count to.)
                 (get-output-string dest)]
                [(string-cursor=? cur end-cur)
                 ;; infinite loop if start-cur == end-cur, but that
                 ;; can't happen because we catch len == 0 earlier
                 (loop count start-cur)]
                [else
                 (write-char (string-ref str cur) dest)
                 (loop (+ count 1) (string-cursor-next str cur))]))))))

(define (string-xcopy! target tstart s sfrom . args)
  (assume-type target <string>)
  (check-arg (^x (and (integer? x) (exact? x))) tstart)
  (let1 result (apply xsubstring s sfrom args)
    (string-copy! target tstart result)))

;;;
;;; Miscellaneous
;;;

(define (string-replace s1 s2 start1 end1 . args)
  (assume-type s1 <string>)
  (assume-type s2 <string>)
  (string-append (substring s1 0 start1)
                 (apply %maybe-substring s2 args)
                 (substring s1 end1 (string-length s1))))

(define (string-tokenize s :optional (token-set #[\S]) start end)
  (assume-type s <string>)
  (letrec ([src (open-input-string (%maybe-substring s start end))]
           [in-word (^[ch dst r]
                      (cond [(eof-object? ch)
                             (reverse! (cons (get-output-string dst) r))]
                            [(char-set-contains? token-set ch)
                             (write-char ch dst)
                             (in-word (read-char src) dst r)]
                            [else
                             (out-word (read-char src)
                                       (cons (get-output-string dst) r))]))]
           [out-word (^[ch r]
                       (cond [(eof-object? ch) (reverse! r)]
                             [(char-set-contains? token-set ch)
                              (let ((dst (open-output-string)))
                                (write-char ch dst)
                                (in-word (read-char src) dst r))]
                             [else
                              (out-word (read-char src) r)]))])
    (out-word (read-char src) '())))

;;;
;;; Filter
;;;

;; The argument order of string-filter and string-delete is changed
;; by post-finalization amendment.  In order to avoid breaking existing
;; code, we accept old argument oder as well.

(define (string-filter c/s/p s . args)
  (if (string? c/s/p)
    (apply string-filter s c/s/p args) ;; for the backward compatibility
    (let ([src (open-input-string (apply %maybe-substring s args))]
          [dest (open-output-string)]
          [pred (%get-char-pred c/s/p)])
      (let loop ([ch (read-char src)])
        (cond [(eof-object? ch) (get-output-string dest)]
              [(pred c/s/p ch) (write-char ch dest) (loop (read-char src))]
              [else (loop (read-char src))])))))

(define (string-delete c/s/p s . args)
  (if (string? c/s/p)
    (apply string-delete s c/s/p args) ;; for the backward compatibility
    (let ([src (open-input-string (apply %maybe-substring s args))]
          [dest (open-output-string)]
          [pred (%get-char-pred c/s/p)])
      (let loop ([ch (read-char src)])
        (cond [(eof-object? ch) (get-output-string dest)]
              [(pred c/s/p ch) (loop (read-char src))]
              [else (write-char ch dest) (loop (read-char src))])))))

;;; Low-level procedures.  These are included for completeness, but
;;; I'm not using these in other SRFI-13 routines, since it is more
;;; efficient to let %maybe-substring handle argument checking as well.

(define (string-parse-start+end proc s args)
  (assume-type s <string>)
  (let ((slen (string-length s)))
    (let-optionals* args (start end)
      (if (undefined? start)
        (values '() 0 slen)
        (if (not (and (integer? start) (exact? start) (<= 0 start slen)))
          (errorf "~s: argument out of range: ~s" proc start)
          (if (undefined? end)
            (values '() start slen)
            (if (not (and (integer? end) (exact? end) (<= start end slen)))
              (errorf "~s: argument out of range: ~s" proc end)
              (values (cddr args) start end))
            )
          )
        )
      )))

(define (string-parse-final-start+end proc s args)
  (assume-type s <string>)
  (let ((slen (string-length s)))
    (let-optionals* args (start end)
      (if (undefined? start)
        (values '() 0 slen)
        (if (not (and (integer? start) (exact? start) (<= 0 start slen)))
          (errorf "~s: argument out of range: ~s" proc start)
          (if (undefined? end)
            (values '() start slen)
            (if (not (and (integer? end) (exact? end) (<= start end slen)))
              (errorf "~s: argument out of range: ~s" proc end)
              (if (not (null? (cddr args)))
                (errorf "~s: too many arguments" proc)
                (values (cddr args) start end)))
            )
          )
        )
      )))

(define-syntax let-string-start+end
  (syntax-rules ()
    ((_ (?start ?end ?rest) ?proc ?s ?args . ?body)
     (call-with-values
      (lambda () (string-parse-start+end ?proc ?s ?args))
      (lambda (?rest ?start ?end) . ?body)))
    ((_ (?start ?end) ?proc ?s ?args . ?body)
     (call-with-values
      (lambda () (string-parse-final-start+end ?proc ?s ?args))
      (lambda (?start ?end) . ?body)))
    ))

(define (check-substring-spec proc s start end)
  (unless (substring-spec-ok? s start end)
    (errorf "~s: invalid substring spec: ~s (~s, ~s)" proc s start end)))

(define (substring-spec-ok? s start end)
  (and (string? s)
       (integer? start) (exact? start)
       (integer? end) (exact? end)
       (<= 0 start end (string-length s))))

;;;
;;; Knuth-Morris-Pratt search
;;;

;; Knuth-Morris-Pratt search constructs.
;;
;; The SRFI-13 specification assumes accessing the pattern by index is
;; a lightweight operation, but it may not be true in Gauche if the pattern
;; contains multibyte characters.  So the programs using these functions
;; may not be very efficient, in spite of the efforts for efficiency put
;; in the original SRFI design.

(define (make-kmp-restart-vector s :optional (c= char=?) start end)
  (let* ((pat (%maybe-substring s start end))
         (rv (make-vector (string-length pat) -1))
         (plen (string-length pat))
         (plen-1 (- plen 1)))
    (do ((i 0 (+ i 1)))
        ((= i plen-1) rv)
      (let loop ((k (+ (vector-ref rv i) 1)))
        (if (and (> k 0)
                 (not (c= (string-ref pat i) (string-ref pat (- k 1)))))
          (loop (+ (vector-ref rv (- k 1)) 1))
          (vector-set! rv (+ i 1) k)))
      )
    ))

(define (kmp-step pat rv c i c= p-start)
  (let loop ((i i))
    (if (c= c (string-ref pat (+ i p-start)))
      (+ i 1)
      (let ((i (vector-ref rv i)))
        (if (= i -1) 0 (loop i))))))

;; This is inefficient if input string s contains multibyte characters.
(define (string-kmp-partial-search pat rv s i :optional
                                   (c= char=?) (p-start 0) start end)
  (let ((patlen (vector-length rv)))
    (let lp ((si start)
             (vi i))
      (cond ((= vi patlen) (- si))
            ((= si end) vi)
            (else (lp (+ si 1)
                      (kmp-step pat rv (string-ref s si)
                                vi c= p-start)))))))



