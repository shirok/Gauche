;;;
;;; srfi-196 - Range objects
;;;
;;;   Copyright (c) 2021  Shiro Kawai  <shiro@acm.org>
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

(define-module srfi-196
  (use gauche.sequence)
  (use scheme.list)
  (use srfi-42)
  (export
   ;; Constructors
   range numeric-range iota-range vector-range string-range
   range-append

   ;; Predicates
   range? range=?

   ;; Accessors
   range-length range-ref range-first range-last

   ;; Iteration
   range-split-at subrange range-segment
   range-take range-take-right
   range-drop range-drop-right
   range-count range-any
   range-every range-map range-map->list range-map->vector
   range-for-each range-filter-map range-filter-map->list
   range-filter range-filter->list range-remove range-remove->list
   range-fold range-fold-right

   ;; Searching
   range-index range-index-right
   range-take-while range-take-while-right
   range-drop-while range-drop-while-right

   ;; Conversion
   range->list range->vector range->string
   vector->range range->generator))
(select-module srfi-196)

;; The classes <range> and <flat-range> are private.  Users must use
;; srfi-196 API.

;; In Gauche, a range is a sequence whose element may be computed from
;; the index.
(define-class <range> (<sequence>)
  ((length :init-keyword :length)
   (ref :init-keyword :indexer)))   ; obj, integer -> element

(define-method write-object ((r <range>) port)
  (format port "#<range (~d)>" (~ r'length)))

;; special case, where indexer is simply a indexed reference from a sequence
(define-class <flat-range> (<range>)
  ((indexer :init-value (^[o i]
                          (assume (exact-integer? i))
                          (assume (< -1 i (~ o'length)))
                          (let1 off (+ (~ o'offset) i)
                            (ref (~ o'storage) off))))
   (storage :init-keyword :storage)
   (offset :init-keyword :offset :init-value 0)))

;; appended ranges.
;; ranges slot contains #((offset . subrange) ...)
(define (%appended-range-index range i)
  (assume (exact-integer? i))
  (assume (< -1 i (~ range'length)))
  (let* ([vec (~ range'ranges)]
         [len (vector-length vec)])
    (define (call-indexer p i)
      ((~ (cdr p)'indexer) (cdr p) (- i (car p))))
    (let loop ([lo 0] [hi len] [k (ash len -1)])
      (let1 p (vector-ref vec k)
        (cond [(= k lo) (call-indexer p i)]
              [(= k (- hi 1)) (call-indexer p i)]
              [(> (car p) i) (loop lo k (+ lo (ash (- k lo) -1)))]
              [else (loop k hi (+ k (ash (- hi k) -1)))])))))

(define-class <appended-range> (<range>)
  ((indexer :init-value %appended-range-index)
   (ranges :init-keyword :ranges)))

;; take a section of a range.
;; Use subrange to construct this type of instance.  It tries to "flatten"
;; nested subranges.
(define (%subrange-index range i)
  ((~ range'range'indexer) (~ range'range) (- i (~ range'offset))))

(define-class <subrange> (<range>)
  ((indexer :init-value %subrange-index)
   (range  :init-keyword :range)
   (offset :init-keyword :offset)))


;;;
;;; Constructors
;;;

(define (range length indexer)
  (assume (and (exact-integer? length) (>= length 0)))
  (make <range> :length length :ref (^[_ i] (indexer i))))

(define (numeric-range start end :optional (step 1))
  (make <range>
    :length (exact (floor-quotient (- end start) step))
    :ref (^[_ i] (+ start (* i step)))))

(define (iota-range length :optional (start 0) (step 1))
  (make <range>
    :length length
    :ref (^[_ i] (+ start (* i step)))))

;; optional arguments are Gauche-specific
(define (vector-range vec :optional (start 0) (end (vector-length vec)))
  (make <flat-range>
    :length (- end start)
    :storage vec
    :offset start))

;; optional arguments are Gauche-specific
(define (string-range str :optional (start 0) (end (string-length str)))
  (make <flat-range>
    :length (- end start)
    :storage str
    :offset start))

(define (range-append . ranges)
  (receive (off&ranges total)
      (map-accum (^[range total]
                   (values (cons total range)
                           (+ total (range-length range))))
                 0 ranges)
    (make <appended-range>
      :length total
      :ranges (list->vector off&ranges))))

;;;
;;; Predicates
;;;

(define (range? x) (is-a? x <range>))

(define (range=? elt= . ranges)
  (or (null? ranges)
      (and (assume (range? (car ranges))) (null? (cdr ranges)))
      (and (apply list= (^[a b] (= (range-length a) (range-length b))) ranges)
           (every?-ec (: i (range-length (car ranges)))
                      (apply list= elt= (map (cut range-ref <> i) ranges))))))

;;;
;;; Accessors
;;;

(define (range-length range)
  (assume (range? range))
  (~ range'length))

(define (range-ref range n)
  (assume (range? range))
  (assume (< -1 n (range-length range)))
  ((~ range'indexer) range n))

(define (range-first range) (range-ref range 0))
(define (range-last range) (range-ref range (- (range-length range) 1)))

;;;
;;; Iteration
;;;

(define (range-split-at range index)
  (assume-type range <range>)
  (assume (<= 0 index (range-length range)))
  (values (subrange range 0 index)
          (subrange range index (range-length range))))

(define (subrange range start end)
  (assume-type range <range>)
  (if (> (- end start) (range-length range))
    (error "subrange start/end index out of original range:"
           (list range start end)))
  (if (is-a? range <subrange>)
    ;; We flatten the range
    (make <subrange>
      :length (- end start)
      :range (~ range'range) ; use inner range
      :offset (+ (~ range'offset) start))
    (make <subrange>
      :length (- end start)
      :range range
      :offset start)))

(define range-segment)

(define (range-take range index)
  (subrange range 0 index))
(define (range-take-right range index)
  (subrange range (- (range-length range) index) (range-length range)))

(define (range-drop range index)
  (subrange range index (range-length range)))
(define (range-drop-right range index)
  (subrange range 0 (- (range-length range) index)))

(define (range-count pred range1 . ranges)
  (if (null? ranges)
    (range-fold (^[cnt val] (if (pred val) (+ cnt 1) cnt)) 0 range1)
    (range-fold (^[cnt . vals] (if (apply pred vals) (+ cnt 1) cnt))
                0
                (cons range1 ranges))))

(define (range-any pred range1 . ranges)
  (if (null? ranges)
    (let1 g (range->generator range1)
      (let loop ([v (g)])
        (cond [(eof-object? v) #f]
              [(pred v)]
              [else (loop (g))])))
    (let1 gs (cons (range->generator range1)
                   (map range->generator ranges))
      (let loop ([vs (map (^g (g)) gs)])
        (cond [(any eof-object? vs) #f]
              [(apply pred vs)]
              [else (loop (map (^g (g)) gs))])))))

(define (range-every pred range1 . ranges)
  (if (null? ranges)
    (let1 g (range->generator range1)
      (let loop ([v (g)] [last #t])
        (cond [(eof-object? v) last]
              [(pred v) => (cut loop (g) <>)]
              [else #f])))
    (let1 gs (cons (range->generator range1)
                   (map range->generator ranges))
      (let loop ([vs (map (^g (g)) gs)] [last #t])
        (cond [(any eof-object? vs) last]
              [(apply pred vs) => (cut loop (map (^g (g)) gs) <>)]
              [else #f])))))

(define (range-map proc range1 . ranges)
  (vector-range (apply range-map->vector range1 ranges)))

(define (range-map->list proc range1 . ranges)
  (if (null? ranges)
    (range-fold-right (^[r v] (cons (proc v) r)) '() range1)
    (apply range-fold-right (^[r . vs] (cons (apply proc vs) r)) '()
           range1 ranges)))
(define (range-map->vector proc range1 . ranges)
  (if (null? ranges)
    (let1 vec (make-vector (range-length range1))
      (do-ec (: k (range-length range1))
             (vector-set! vec k (proc (range-ref range1 k))))
      (vector-range vec))
    (let* ([ranges (cons range1 ranges)]
           [vec (make-vector (apply min (map range-length ranges)))])
      (do-ec (: k (range-length range1))
             (vector-set! vec k (apply proc (map (cut range-ref <> k) ranges))))
      (vector-range vec))))

(define (range-for-each proc range1 . ranges)
  (if (null? ranges)
    (range-fold (^[_ v] (proc v)) #f range1)
    (apply range-fold (^[_ . vs] (apply proc vs)) #f range1 ranges)))

(define range-filter-map)
(define range-filter-map->list)

(define range-filter)
(define range-filter->list)
(define range-remove)
(define range-remove->list)

;; kons is invoked with the same order of vector-fold (state first)
(define (range-fold kons knil range1 . ranges)
  (if (null? ranges)
    (fold (^[i seed] (kons seed (range-ref range1 i)))
          knil
          (liota (range-length range1)))
    (let1 len (fold (^[r m] (min m (range-length r)))
                    (range-length range1)
                    ranges)
      (fold (^[i seed] (apply kons seed
                              (range-ref range1 i)
                              (map (^r (range-ref r i)) ranges)))
            knil
            (liota len)))))

(define (range-fold-right kons knil range1 . ranges)
  (if (null? ranges)
    (fold (^[i seed] (kons seed (range-ref range1 i)))
          knil
          (liota (range-length range1) (- (range-length range1) 1) -1))
    (let1 len (fold (^[r m] (min m (range-length r)))
                    (range-length range1)
                    ranges)
      (fold (^[i seed] (apply kons seed
                              (range-ref range1 i)
                              (map (^r (range-ref r i)) ranges)))
            knil
            (liota len (- len 1) -1)))))

;;;
;;; Searching
;;;

(define range-index)
(define range-index-right)

(define range-take-while)
(define range-take-while-right)

(define range-drop-while)
(define range-drop-while-right)

;;;
;;; Conversion
;;;

(define (range->list range)
  (assume-type range <range>)
  (list-ec (: i (range-length range))
           (range-ref range i)))

(define (range->vector range)
  (assume-type range <range>)
  ;; We might be able to return the internal storage directly if range is
  ;; <flat-range> on a vector, since "it is an error" to mutate the return
  ;; value; but there would be no check if the caller conforms it.  Until
  ;; we see the case that such optimization is effective, we just build
  ;; a new vector.
  (vector-ec (: i (range-length range))
             (range-ref range i)))

(define (range->string range)
  (assume-type range <range>)
  (if (and (is-a? range <flat-range>)
           (string? (~ range'storage)))
    (substring (~ range'storage)
               (~ range'offset)
               (+ (~ range'offset) (~ range'length)))
    (string-ec (: i (range-length range))
               (range-ref range i))))

;; optional arguments are Gauche-specific
(define (vector->range vec :optional (start 0) (end (vector-length vec)))
  (vector-range (vector-copy vec start end)))

;; optional arguments are Gauche-specific
(define (range->generator range :optional (start 0) (end (range-length range)))
  (define i start)
  (^[] (if (>= i end)
         (eof-object)
         (begin0 (range-ref range i) (inc! i)))))
