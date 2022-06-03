;;;
;;; data.range - Range objects
;;;
;;;   Copyright (c) 2021-2022  Shiro Kawai  <shiro@acm.org>
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

;; Superset of srfi-196

(define-module data.range
  (use gauche.sequence)
  (use scheme.list)
  (use scheme.division)
  (use srfi-42)
  (export
   ;; we expose only the root classes (not the subclasses; they're
   ;; internal details)
   <range-meta> <range>

   ;; Constructors
   range numeric-range iota-range vector-range uvector-range string-range
   range-append range-reverse

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
(select-module data.range)

;; The classes <range> and <flat-range> are private.  Users must use
;; srfi-196 API.

;; In Gauche, a range is a sequence whose element may be computed from
;; the index.
(define-class <range-meta> (<class>) ())

(define-class <range> (<sequence>)
  ((length :init-keyword :length)
   (indexer :init-keyword :indexer))   ; obj, integer -> element
  :metaclass <range-meta>)

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
;; Use subrange to construct this type of instance.  The constructor
;; tries to "flatten" nested subranges.
;; If the reverse? flag is true, the indexing is reversed.
(define (%subrange-index range i)
  ((~ range'range'indexer) (~ range'range) (+ i (~ range'offset))))
(define (%subrange-index-reverse range i)
  ((~ range'range'indexer) (~ range'range)
   (+ (- (~ range'length) i 1) (~ range'offset))))

(define-class <subrange> (<range>)
  ((indexer :init-value %subrange-index :init-keyword :indexer)
   (range  :init-keyword :range)
   (offset :init-keyword :offset)))

(define (%subrange-reversed? subrange)
  (eq? (~ subrange'indexer) %subrange-index-reverse))

;;;
;;; Constructors
;;;

(define (range length indexer)
  (assume (and (exact-integer? length) (>= length 0)))
  (make <range> :length length :indexer (^[_ i] (indexer i))))

(define (numeric-range start end :optional (step 1))
  (make <range>
    :length (max 0 (ceiling->exact (/ (- end start) step)))
    :indexer (^[_ i] (+ start (* i step)))))

(define (iota-range length :optional (start 0) (step 1))
  (make <range>
    :length length
    :indexer (^[_ i] (+ start (* i step)))))

;; optional arguments are Gauche-specific
(define (vector-range vec :optional (start 0) (end (vector-length vec)))
  (assume-type vec <vector>)
  (assume (<= 0 start end (vector-length vec)))
  (make <flat-range>
    :length (- end start)
    :indexer vector-ref
    :storage vec
    :offset start))

;; optional arguments are Gauche-specific
(define (string-range str :optional (start 0) (end (string-length str)))
  (assume-type str <string>)
  (assume (<= 0 start end (string-length str)))
  (make <flat-range>
    :length (- end start)
    :indexer string-ref
    :storage (string-build-index! str)
    :offset start))

;; Gauche extension
(define (uvector-range uvec :optional (start 0) (end (uvector-length uvec)))
  (assume-type uvec <uvector>)
  (assume (<= 0 start end (uvector-length uvec)))
  (make <flat-range>
    :length (- end start)
    :storage uvec
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

 (define (range-reverse rg :optional (start 0) (end (range-length rg)))
  (assume-type rg <range>)
  (assume (<= 0 start end (range-length rg)))
  (if (is-a? rg <subrange>)
    (if (%subrange-reversed? rg)
      (make <subrange>
        :length (- end start)
        :range (~ rg'range)
        :offset (+ start (~ rg'offset)))
      (make <subrange>
        :indexer %subrange-index-reverse
        :length (- end start)
        :range (~ rg'range)
        :offset (+ start (~ rg'offset))))
    (make <subrange>
      :indexer %subrange-index-reverse
      :range rg
      :length (- end start)
      :offset start)))

;;;
;;; Predicates
;;;

(define (range? x) (is-a? x <range>))

(define (range=? elt= . ranges)
  (or (null? ranges)
      (and (assume (range? (car ranges))) (null? (cdr ranges)))
      (let1 len (range-length (car ranges))
        (every?-ec (: r (cdr ranges))
                   (and (= len (range-length r))
                        (every?-ec (: i len)
                                   (elt= (range-ref (car ranges) i)
                                         (range-ref r i))))))))

;;;
;;; Accessors
;;;

(define (range-length range)
  (assume (range? range))
  (~ range'length))

;; The optional fallback is Gauche extension
(define (range-ref range n :optional fallback)
  (assume (range? range))
  (cond [(<=:< 0 n (range-length range)) ((~ range'indexer) range n)]
        [(undefined? fallback) (error "Index out of range:" n)]
        [else fallback]))

(define (range-first range :optional fallback)
  (if (zero? (range-length range))
    (if (undefined? fallback)
      (error "range is empty:" range)
      fallback)
    (range-ref range 0)))
(define (range-last range :optional fallback)
  (let1 len (range-length range)
    (if (zero? len)
      (if (undefined? fallback)
        (error "range is empty:" range)
        fallback)
      (range-ref range (- len 1)))))

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

(define (range-segment range length)
  (let* ([total-length (range-length range)]
         [n (ceiling-quotient total-length length)])
    (map (^i (subrange range
                       (* i length)
                       (min (* (+ i 1) length) total-length)))
         (liota n))))

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
    (apply range-fold (^[cnt . vals] (if (apply pred vals) (+ cnt 1) cnt))
           0 range1 ranges)))

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
  (vector-range (apply range-map->vector proc range1 ranges)))

(define (range-map->list proc range1 . ranges)
  (if (null? ranges)
    (range-fold-right (^[r v] (cons (proc v) r)) '() range1)
    (apply range-fold-right (^[r . vs] (cons (apply proc vs) r)) '()
           range1 ranges)))
(define (range-map->vector proc range1 . ranges)
  (if (null? ranges)
    (rlet1 vec (make-vector (range-length range1))
      (do-ec (: k (range-length range1))
             (vector-set! vec k (proc (range-ref range1 k)))))
    (let* ([ranges (cons range1 ranges)]
           [vec (make-vector (apply min (map range-length ranges)))])
      (do-ec (: k (vector-length vec))
             (vector-set! vec k (apply proc (map (cut range-ref <> k) ranges))))
      vec)))

(define (range-for-each proc range1 . ranges)
  (if (null? ranges)
    (range-fold (^[_ v] (proc v)) #f range1)
    (apply range-fold (^[_ . vs] (apply proc vs)) #f range1 ranges)))

(define (range-filter-map proc range1 . ranges)
  ($ vector-range $ list->vector
     $ apply range-filter-map->list proc range1 ranges))

(define (range-filter-map->list proc range1 . ranges)
  (if (null? ranges)
    (range-fold-right (^[r e] (if-let1 v (proc e)
                                (cons v r)
                                r))
                      '() range1)
    (apply range-fold-right
           (^[r . es] (if-let1 v (apply proc es)
                        (cons v r)
                        r))
           '() range1 ranges)))

(define (range-filter pred range)
  ($ vector-range $ list->vector $ range-filter->list pred range))
(define (range-filter->list pred range)
  (range-fold-right (^[r e] (if (pred e) (cons e r) r)) '() range))

(define (range-remove pred range)
  ($ vector-range $ list->vector $ range-remove->list pred range))
(define (range-remove->list pred range)
  (range-fold-right (^[r e] (if (pred e) r (cons e r))) '() range))

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

(define (range-index pred range1 . ranges)
  (if (null? ranges)
    (let1 len (range-length range1)
      (let loop ([i 0])
        (cond [(= i len) #f]
              [(pred (range-ref range1 i)) i]
              [else (loop (+ i 1))])))
    (let1 len (apply min (range-length range1) (map range-length ranges))
      (let loop ([i 0])
        (cond [(= i len) #f]
              [(apply pred (range-ref range1 i)
                      (map (cut range-ref <> i) ranges)) i]
              [else (loop (+ i 1))])))))

(define (range-index-right pred range1 . ranges)
  (if (null? ranges)
    (let1 len (range-length range1)
      (let loop ([i (- len 1)])
        (cond [(= i 0) #f]
              [(pred (range-ref range1 i)) i]
              [else (loop (- i 1))])))
    (let1 len (apply min (range-length range1) (map range-length ranges))
      (let loop ([i (- len 1)])
        (cond [(= i 0) #f]
              [(apply pred (range-ref range1 i)
                      (map (cut range-ref <> i) ranges)) i]
              [else (loop (- i 1))])))))

(define (range-take-while pred r)
  (let1 len (range-length r)
    (let loop ([i 0])
      (if (= i len)
        r
        (let1 e (range-ref r i)
          (if (pred e)
            (loop (+ i 1))
            (subrange r 0 i)))))))

(define (range-take-while-right pred r)
  (let1 len (range-length r)
    (let loop ([i (- len 1)])
      (if (= i 0)
        r
        (let1 e (range-ref r i)
          (if (pred e)
            (loop (- i 1))
            (subrange r (+ i 1) len)))))))

(define (range-drop-while pred r)
  (let1 len (range-length r)
    (let loop ([i 0])
      (if (= i len)
        (range 0 (constantly #f))
        (let1 e (range-ref r i)
          (if (pred e)
            (loop (+ i 1))
            (subrange r i len)))))))

(define (range-drop-while-right pred r)
  (let1 len (range-length r)
    (let loop ([i (- len 1)])
      (if (= i 0)
        (range 0 (constantly #f))
        (let1 e (range-ref r i)
          (if (pred e)
            (loop (- i 1))
            (subrange r 0 (+ i 1))))))))

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

;;;
;;; Sequence protocol
;;;

(define-method referencer ((r <range>)) (~ r'indexer))

(define-method call-with-iterator ((r <range>) proc
                                   :key (start 0)
                                        (end (range-length r))
                                   :allow-other-keys)
  (let ([i start])
    (proc (^[] (>= i end))
          (^[] (begin0 (range-ref r i) (inc! i))))))

(define-method call-with-reverse-iterator ((r <range>) proc
                                           :key (start 0)
                                                (end (range-length r))
                                           :allow-other-keys)
  (let ([i (- end 1)])
    (proc (^[] (< i start)
          (^[] (begin0 (range-ref r i) (dec! i)))))))

(define-method call-with-builder ((c <range-meta>) proc :key (size #f))
  (if (integer? size)
    (let ([buf (make-vector size #f)]
          [i 0])
      (proc (^v (vector-set! buf i v) (inc! i))
            (^[] (vector-range buf))))
    (let ([vs '()])
      (proc (^v (push! vs v))
            (^[] (vector-range (reverse-list->vector vs)))))))

;; some shortcuts
(define-method subseq ((r <range>) s e) (subrange r s e))
(define-method subseq ((r <range>) s) (subrange r s (range-length e)))
(define-method coerce-to ((r <range-meta>) (v <vector>)) (vector-range v))
(define-method coerce-to ((r <range-meta>) (v <string>)) (string-range v))
