;;;
;;;  data.heap - Heaps
;;;
;;;   Copyright (c) 2014  Shiro Kawai  <shiro@acm.org>
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

(define-module data.heap
  (use gauche.sequence)
  (use gauche.uvector)
  (use srfi-1)
  (export <binary-heap>
          make-binary-heap binary-heap-empty? binary-heap-num-entries
          binary-heap-copy binary-heap-check binary-heap-clear!
          binary-heap-push!
          binary-heap-find-min binary-heap-find-max
          binary-heap-pop-min! binary-heap-pop-max!
          ))
(select-module data.heap)

;; we use sparse-vector by default; we just make it autoload
;; so that the tests won't depend on data.sparse.
(autoload data.sparse make-sparse-vector <sparse-vector>
          sparse-vector-copy sparse-vector-clear!)

;;;
;;; Binary Heap
;;;

;; We use min-max heap; whatever gain earned by limiting to min-heap
;; or max-heap would be small compared to other overhead.

;; M.D. Atkinson, J.-R. Sack, N. Santoro, T. Strothotte,
;; Min-max heaps and generalized priority queues, CACM 29(10) pp.996-1000
;; Oct. 1986, http://dx.doi.org/10.1145/6617.6621

;; Binary heap is stored in an array, S[].
;;
;; - S[0] isn't used.
;; - S[1] is the root.
;; - S[i]'s child is S[2i] and S[2i+1]
;; - S[i]'s parent is S[floor(i/2)] when i > 1.
;; - Stored data can be anything comparable.
;;
;; Backing storage can be either a flat vector (vector, uvector) or
;; a sparse vector.  If it is a flat vector, the heap has maximum size
;; (we don't extend the buffer).


(define-class <binary-heap> ()
  ((comparator :init-keyword :comparator)
   (key        :init-keyword :key)
   (capacity   :init-keyword :capacity)  ; +inf.0 if storage is a sparse vector
   ;; internal
   (<:         :init-keyword :<:) ; cached less-than proc
   (>:         :init-keyword :>:) ; cached greater-than proc
   (storage    :init-keyword :storage)
   (next-leaf  :init-keyword :next-leaf :init-value 1)  ; next leaf index
   ))

(define (make-binary-heap :key (comparator default-comparator)
                               (storage (make-sparse-vector))
                               (key identity))
  (unless (comparator-comparison-procedure? comparator)
    (error "make-binary-heap requires comparator with comparison procedure, \
            but got:" comparator))
  (let1 cmp (comparator-comparison-procedure comparator)
    (make <binary-heap> :comparator comparator
          :storage (if (or (vector? storage) (uvector? storage)
                           (is-a? storage <sparse-vector>))
                     storage
                     (error "make-binary-heap requires a vector, a uvector \
                             or a sparse vector as a storage, but got:"
                            storage))
          :key key
          :capacity (cond [(vector? storage) (- (vector-length storage) 1)]
                          [(uvector? storage) (- (uvector-length storage) 1)]
                          [else +inf.0])
          :<: (^[a b] (< (cmp (key a) (key b)) 0))
          :>: (^[a b] (> (cmp (key a) (key b)) 0)))))

(define (binary-heap-copy hp)
  (make <binary-heap>
    :comparator (~ hp'comparator)
    :storage (let1 s (~ hp'storage)
               (cond [(vector? s) (vector-copy s)]
                     [(uvector? s) (uvector-copy s)]
                     [(is-a? s <sparse-vector>) (sparse-vector-copy s)]
                     [else (error "[internal] binary-heap-copy: invalid storage:" s)]))
    :key (~ hp'key)
    :capaticy (~ hp'capacity)
    :<: (~ hp'<:)
    :>: (~ hp'>:)
    :next-leaf (~ hp'next-leaf)))

(define (binary-heap-clear! hp)
  (set! (~ hp'next-leaf) 1)
  ;; These are theoretically unnecessary, but works nicely with GC.
  (let1 st (~ hp'storage)
    (cond [(vector? st) (vector-fill! st #f)]
          [(is-a? st <sparse-vector>) (sparse-vector-clear! st)])))

(define (binary-heap-push! hp item)
  (let1 next (~ hp'next-leaf)
    (when (>= (- next 1) (~ hp'capacity))
      (errorf "binary heap ~s is full: couldn't insert ~s" hp item))
    (comparator-check-type (~ hp'comparator) item)
    (set! (~ hp'storage next) item)
    (set! (~ hp'next-leaf) (+ next 1))
    (when (> next 1)
      (bh-bubble-up (~ hp'storage) (~ hp'<:) (~ hp'>:) next))))

(define (binary-heap-num-entries hp) (- (~ hp'next-leaf) 1))

(define (binary-heap-empty? hp) (= (~ hp'next-leaf) 1))

(define (binary-heap-find-min hp :optional (fallback (undefined)))
  (if (= (~ hp'next-leaf) 1)
    (if (undefined? fallback)
      (error "binary heap is empty:" hp)
      fallback)
    (~ hp'storage 1)))

(define (binary-heap-find-max hp :optional (fallback (undefined)))
  (case (~ hp'next-leaf)
    [(1) (if (undefined? fallback)
           (error "binary heap is empty:" hp)
           fallback)]
    [(2) (~ hp'storage 1)]
    [(3) (~ hp'storage 2)]
    [else (let ([a (~ hp'storage 2)]
                [b (~ hp'storage 3)])
            (if ((~ hp'>:) a b) a b))]))

(define (binary-heap-pop-min! hp)
  (let1 nelts (binary-heap-num-entries hp)
    (when (= nelts 0) (error "binary heap is empty:" hp))
    (rlet1 r (~ hp'storage 1)
      (set! (~ hp'storage 1) (~ hp'storage nelts))
      (set! (~ hp'next-leaf) nelts)
      (bh-trickle-down (~ hp'storage) (~ hp'<:) (~ hp'>:) 1 nelts))))

(define (binary-heap-pop-max! hp)
  (let1 nelts (binary-heap-num-entries hp)
    (define (swap-and-adjust index)
      (set! (~ hp'storage index) (~ hp'storage nelts))
      (set! (~ hp'next-leaf) nelts)
      (bh-trickle-down (~ hp'storage) (~ hp'<:) (~ hp'>:) index nelts))
    (case nelts
      [(0) (error "binary heap is empty:" hp)]
      [(1 2) (set! (~ hp'next-leaf) nelts) (~ hp'storage nelts)]
      [else
       (let ([a (~ hp'storage 2)]
             [b (~ hp'storage 3)])
         (if ((~ hp'>:) a b)
           (begin (swap-and-adjust 2) a)
           (begin (swap-and-adjust 3) b)))])))

;; Internal procedures
(define-inline (min-node? index) (odd? (integer-length index)))

(define (binary-heap-check hp)
  (unless (>= (~ hp'next-leaf) 1)
    (error "next-leaf index is less than 1"))
  (let rec ((i 1))
    (let ([kids  (take-while (cute < <> (~ hp'next-leaf)) (iota 2 (* i 2)))]
          [gkids (take-while (cute < <> (~ hp'next-leaf)) (iota 4 (* i 4)))]
          [cmp (if (min-node? i) (~ hp'<:) (~ hp'>:))])
      (unless (every (^k (cmp (~ hp'storage i) (~ hp'storage k))) kids)
        (errorf "parent-kid relation violated: ~a-node[~a]=~s, kids[~a]=~s"
               (if (min-node? i) 'min 'max)
               i (~ hp'storage i) kids (map (cut ~ hp'storage <>) kids)))
      (unless (every (^k (cmp (~ hp'storage i) (~ hp'storage k))) gkids)
        (errorf "grandparent-grandkid relation violated: ~a-node[~a]=~s, grandkids[~a]=~s"
               (if (min-node? i) 'min 'max)
               i (~ hp'storage i) gkids (map (cut ~ hp'storage <>) gkids)))
      (for-each rec kids)
      #t)))

;; index
;;  1   : min(2,3)
;;  2,3 : max(4,5) and max(6,7)
;;  4-7 : min(8,9) ... min(14,15)
;;  8-15: max(16,17) ... max(30,31)
;; etc.
;; if (integer-length i) is odd, we have min node
;; else we have max node

(define-syntax swap!
  (syntax-rules ()
    [(_ storage i j)
     (let1 v (~ storage i)
       (set! (~ storage i) (~ storage j))
       (set! (~ storage j) v))]))

;; called with index > 1
(define (bh-bubble-up storage <: >: index)

  (define (bubble-up-rec >< index)
    (when (> index 3)
      (let1 grandparent-index (ash index -2)
        (unless (>< (~ storage grandparent-index) (~ storage index))
          (swap! storage grandparent-index index)
          (bubble-up-rec >< grandparent-index)))))

  (let1 parent-index (ash index -1)
    (if (min-node? parent-index)
      (if (<: (~ storage parent-index) (~ storage index))
        (bubble-up-rec >: index)
        (begin
          (swap! storage parent-index index)
          (bubble-up-rec <: parent-index)))
      (if (>: (~ storage parent-index) (~ storage index))
        (bubble-up-rec <: index)
        (begin
          (swap! storage parent-index index)
          (bubble-up-rec >: parent-index))))))


(define (bh-trickle-down storage <: >: index size)
  
  (define-syntax getval (syntax-rules () [(getval i) (~ storage i)]))
  (define-syntax in-bound? (syntax-rules () [(in-bound? i) (< i size)]))

  ;; We need to check up to 2 kids and 4 grandkids.  This returns appropriate
  ;; index for n in 0..5; that is, n=0,1 for kids, n=2,3,4,5 for grandkids.
  (define (descendant n index)
    (if (< n 2)
      (+ (ash index 1) n)
      (+ (ash index 2) (- n 2))))

  ;; Among self, children and grandchildren, find an index that has
  ;; the minimum or maximum value.
  ;; NB: This can be witten more cleanly by making the list of kids index
  ;; and folding over it; but we want to avoid allocation in it, for this
  ;; will be called log(n) times in average for every deletion.
  (define (find-extreme >< index)
    (let loop ([minval (getval index)]
               [minidx index]
               [n 0])
      (if (> n 5)
        minidx
        (let1 i (descendant n index)
          (if (not (in-bound? i))
            minidx
            (let1 v (getval i)
              (if (>< v minval)
                (loop v i (+ n 1))
                (loop minval minidx (+ n 1)))))))))

  (define (trickle-down-rec >< index)
    (let1 pick (find-extreme >< index)
      (unless (= pick index)
        (swap! storage pick index)
        (when (>= pick (ash index 2)) ; grandchild
          (unless (>< (getval pick) (getval (ash pick -1)))
            (swap! storage (ash pick -1) pick))
          (trickle-down-rec >< pick)))))

  (trickle-down-rec (if (min-node? index) <: >:) index))
