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
  (assume (< -1 i (~ o'length)))
  (let* ([vec (~ o'ranges)]
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

;; take a section of a range
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
    :length (floor->exact (- end start) step)
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
(define (string-range str :optional (start 0) (end (string-length vec)))
  (make <flat-range>
    :length (- end start)
    :storage vec
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
      (and (assert (range? (car ranges))) (null? (cdr ranges)))
      (and (apply list= (^[a b] (= (range-length a) (range-length b))) ranges)
           (every?-ec (: i (range-length (car ranges)))
                      (apply list= elt= (map (cut range-ref <> i) ranges))))))

;;;
;;; Accessors
;;;

(define (range-length range)
  (assert (range? range))
  (~ range'length))

(define (range-ref range n)
  (assert (range? range))
  (assert (< -1 n (range-length range)))
  ((~ range'indexer) range n))

(define (range-first range) (range-ref range 0))
(define (range-last range) (range-ref range (- (range-length range) 1)))

;;;
;;; Iteration
;;;
