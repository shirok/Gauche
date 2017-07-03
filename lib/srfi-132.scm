;;;
;;; srfi-132 - sort library
;;;

;; This is a thin adaptor for Gauche's built-in sort procedures.

(define-module srfi-132
  (use gauche.sequence)
  (use gauche.generator)
  (export list-sorted? vector-sorted?
          list-sort list-stable-sort
          list-sort! list-stable-sort!
          vector-sort vector-stable-sort
          vector-sort! vector-stable-sort!
          list-merge list-merge!
          vector-merge vector-merge!
          list-delete-neighbor-dups
          list-delete-neighbor-dups!
          vector-delete-neighbor-dups
          vector-delete-neighbor-dups!
          ;; vector-find-median
          ;; vector-find-median!
          ;; vector-select!
          ;; vector-separate!
          ))
(select-module srfi-132)

(define (list-sort < lis) (assume-type lis <list>) (sort lis <))
(define (list-sort! < lis) (assume-type lis <list>) (sort! lis <))
(define (list-stable-sort < lis) (assume-type lis <list>) (stable-sort lis <))
(define (list-stable-sort! < lis) (assume-type lis <list>) (stable-sort! lis <))
(define (list-sorted? < lis) (assume-type lis <list>) (sorted? lis <))
(define (list-merge < lis1 lis2) (merge lis1 lis2 <))
(define (list-merge! < lis1 lis2) (merge! lis1 lis2 <))

;; NB: We could get range-restricted linear-update version more efficient.

(define-inline (%check-range v start end)
  (assume-type start <integer>)
  (assume-type end <integer>)
  (unless (<= 0 start end (- (vector-length v) 1))
    (errorf "Start/end arguments must be nonnegative exact integers, \
             and must be (<= 0 start end (- (vector-length v) 1)). \
             We got (start end): (~s ~s)" start end)))

(define (%vector-sorter %sort!)
  (^[< v :optional (s 0) (e (vector-length v))]
    (assume-type v <vector>)
    (%check-range v s e)
    (let1 sorted (%sort! (subseq v s e) <)
      (if (and (= s 0) (= e (vector-length v)))
        sorted
        (vector-append (subseq v 0 s) sorted (subseq v e (vector-length v)))))))

(define (%vector-sorter! %sort!)
  (^[< v :optional (s 0) (e (vector-length v))]
    (assume-type v <vector>)
    (%check-range v s e)
    (if (and (= s 0) (= e (vector-length v)))
      (%sort! v <)
      (begin
        (set! (subseq v s e) (%sort! (subseq v s e) <))
        v))))

(define vector-sort (%vector-sorter sort!))
(define vector-sort! (%vector-sorter! sort!))
(define vector-stable-sort (%vector-sorter stable-sort!))
(define vector-stable-sort! (%vector-sorter! stable-sort!))

(define (%maybe-subseq v s e)
  (if (and (= s 0) (= e (vector-length v)))
    v
    (subseq v s e)))

(define (vector-sorted? < v :optional (s 0) (e (vector-length v)))
  (assume-type v <vector>)
  (%check-range v s e)
  (sorted? (%maybe-subseq v s e) <))

(define (%vector-merge! < dst start v1 v2)
  (let ([len1 (vector-length v1)]
        [len2 (vector-length v2)])
    (cond [(zero? len1) (vector-copy! dst start v2)]
          [(zero? len2) (vector-copy! dst start v1)]
          [else (let loop ([e1 (vector-ref v1 0)]
                           [e2 (vector-ref v2 0)]
                           [i1 1]
                           [i2 1]
                           [d start])
                  (cond [(< e2 e1)
                         (vector-set! dst d e2)
                         (if (= i2 len2)
                           (vector-copy! dst (+ d 1) v1 (- i1 1))
                           (loop e1 (vector-ref v2 i2) i1 (+ i2 1) (+ d 1)))]
                        [else
                         (vector-set! dst d e1)
                         (if (= i1 len1)
                           (vector-copy! dst (+ d 1) v2 (- i2 1))
                           (loop (vector-ref v1 i1) e2 (+ i1 1) i2 (+ d 1)))]))]
          )))

(define (vector-merge < v1 v2 :optional (s1 0) (e1 (vector-length v1))
                                        (s2 0) (e2 (vector-length v2)))
  (assume-type v1 <vector>)
  (assume-type v2 <vector>)
  (%check-range v1 s1 e1)
  (%check-range v2 s2 e2)
  (rlet1 vr (make-vector (+ (- e1 s1) (- e2 s2)))
    (%vector-merge! < vr 0 (%maybe-subseq v1 s1 e1) (%maybe-subseq v2 s2 e2))))

(define (vector-merge! < vr v1 v2 :optional (sr 0)
                                            (s1 0) (e1 (vector-length v1))
                                            (s2 0) (e2 (vector-length v2)))
  (assume-type vr <vector>)
  (assume-type v1 <vector>)
  (assume-type v2 <vector>)
  (%check-range v1 s1 e1)
  (%check-range v2 s2 e2)
  (unless (>= (vector-length vr) (+ sr (- e1 s1) (- e2 s2)))
    (errorf "Destination vector is too short (length=~s, required=~s)"
            (vector-length vr) (+ sr (- e1 s1) (- e2 s2))))
  (%vector-merge! < vr sr (%maybe-subseq v1 s1 e1) (%maybe-subseq v2 s2 e2))
  vr)

;; duplicate elimination

(define (list-delete-neighbor-dups = lis)
  (assume-type lis <list>)
  (delete-neighbor-dups lis :test =))
(define (list-delete-neighbor-dups! = lis)
  (assume-type lis <list>)
  (delete-neighbor-dups-squeeze! lis :test =))
(define (vector-delete-neighbor-dups = vec :optional (start 0) (end #f))
  (assume-type vec <vector>)
  (delete-neighbor-dups vec :test = :start start :end end))
(define (vector-delete-neighbor-dups! = vec :optional (start 0) (end #f))
  (assume-type vec <vector>)
  (delete-neighbor-dups! vec :test = :start start :end end))
