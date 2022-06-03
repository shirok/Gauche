;;;
;;; data.skew-list - Skewed Binary Random Access List
;;;
;;;   Copyright (c) 2019-2022  Shiro Kawai  <shiro@acm.org>
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

;; Implements SkewList as described in
;; Chris Okasaki: Purely Functional Data Structures
;;
;; Tree a = Leaf a | Node a (Tree a) (Tree a)
;; SkewList a = SL [(Int, Tree a)]

(define-module data.skew-list
  (use gauche.record)
  (use gauche.sequence)
  (use util.match)
  (use util.queue)
  (export <skew-list>
          skew-list?
          skew-list-empty?
          skew-list-null
          skew-list-cons
          skew-list-car
          skew-list-cdr
          skew-list-ref
          skew-list-set
          skew-list-fold
          skew-list-map
          skew-list-length
          skew-list-length<=?
          list*->skew-list
          list->skew-list
          skew-list->list
          skew-list->generator
          skew-list->lseq
          skew-list-take
          skew-list-drop
          skew-list-split-at
          skew-list-append)
  )
(select-module data.skew-list)

(define-class <skew-list-meta> (<record-meta>) ())
(define-record-type (<skew-list> #f :mixins (<sequence>)
                                    :metaclass <skew-list-meta>)
  %make-sl skew-list?
  (elements skew-list-elements))               ; [(Int, Tree)]]

(define (SL elts)
  (if (null? elts)
    skew-list-null                      ;singleton to save allocation
    (%make-sl elts)))

;; We use these frequently.  NB: n is always 2^k-1.
(define-inline (/2 n) (ash n -1))
(define-inline (/4 n) (ash n -2))

;;;
;;; Primitives
;;;

(define (skew-list-empty? sl)
  (assume-type sl <skew-list>)
  (null? (skew-list-elements sl)))
(define skew-list-null (%make-sl '()))

(define (skew-list-cons x y)
  (assume-type y <skew-list>)
  (match (skew-list-elements y)
    [([w1 . t1] [w2 . t2] . ts)
     (if (= w1 w2)
       (SL `([,(+ 1 w1 w2) . (Node ,x ,t1 ,t2)] ,@ts))
       (SL `([1 . (Leaf ,x)] ,@(skew-list-elements y))))]
    [_ (SL `([1 . (Leaf ,x)] ,@(skew-list-elements y)))]))

(define (skew-list-car sl)
  (assume-type sl <skew-list>)
  (match (skew-list-elements sl)
    [() (error "Attempt to take skew-list-car of empty skew-list")]
    [([_ . ('Leaf x)] . _) x]
    [([_ . ('Node x _ _)] . _) x]))

(define (skew-list-cdr sl)
  (assume-type sl <skew-list>)
  (match (skew-list-elements sl)
    [() (error "Attempt to take skew-list-cdr of empty skew-list")]
    [([_ . ('Leaf _)] . ts) (SL ts)]
    [([w . ('Node x t1 t2)] . ts)
     (SL `([,(/2 w) . ,t1] [,(/2 w) . ,t2] ,@ts))]))

(define (skew-list-ref sl n :optional fallback)
  (define (tree-ref w i t)
    (if (= i 0)
      (cadr t)
      (if (= w 1)
        (if (undefined? fallback)
          (error "index out of range" n)
          fallback)
        (match-let1 ('Node x t1 t2) t
          (let1 w2 (/2 w)
            (if (<= i w2)
              (tree-ref w2 (- i 1) t1)
              (tree-ref w2 (- i 1 w2) t2)))))))
  (define (ref i ts)
    (match ts
      [() (if (undefined? fallback)
            (error "index out of range" n)
            fallback)]
      [((w . t) . ts)
       (if (< i w) (tree-ref w i t) (ref (- i w) ts))]))
  (assume-type sl <skew-list>)
  (ref n (skew-list-elements sl)))

(define (skew-list-set sl n v)
  (define (tree-set w i t)
    (if (= i 0)
      (match t
        [('Leaf _) `(Leaf ,v)]
        [(`Node _ t1 t2) `(Node ,v ,t1 ,t2)])
      (if (= w 1)
        (error "index out of range" n)
        (match-let1 ('Node x t1 t2) t
          (let1 w2 (/2 w)
            (if (<= i w2)
              `(Node ,x ,(tree-set w2 (- i 1) t1) ,t2)
              `(Node ,x ,t1 ,(tree-set w2 (- i 1 w2) t2))))))))
  (define (set i ts)
    (match ts
      [() (error "index out of range" n)]
      [((w . t) . ts)
       (if (< i w)
         `((,w . ,(tree-set w i t)) ,@ts)
         `((,w . ,t) ,@(set (- i w) ts)))]))
  (assume-type sl <skew-list>)
  (SL (set n (skew-list-elements sl))))

;;;
;;; Conversion
;;;

;; Input may be an improper list.  Returns SL and the last cdr.
(define (list*->skew-list lis)
  (if (pair? lis)
    (let1 spine-len (let loop ((lis lis) (n 0))
                      (if (pair? lis) (loop (cdr lis) (+ n 1)) n))
      ;; divide n into (k0 k1 ...) where each k is 2^j-1 and decreasing order
      (define (series-2^n-1 n)
        (cond [(= n 0) '()]
              [(= n 1) '(1)]
              [else (let1 k (- (ash 1 (- (integer-length (+ n 1)) 1)) 1)
                      (cons k (series-2^n-1 (- n k))))]))
      ;; make tree from first n elts of lis (n = 2^j-1)
      ;; returns the rest of the lis as well,
      ;; being careful not to copy the spine of lis.
      (define (make-tree n lis)
        (if (= n 1)
          (values `(Leaf ,(car lis)) (cdr lis))
          (let1 n2 (ash (- n 1) -1)
            (receive (t1 rest) (make-tree n2 (cdr lis))
              (receive (t2 rest) (make-tree n2 rest)
                (values `(Node ,(car lis) ,t1 ,t2) rest))))))
      ;; get reversed series-2^n-1
      ;; returns [(Size . Tree)] and the last cdr of lis
      (define (make-forest ns lis)
        (if (null? ns)
          (values '() lis)
          (receive (tree rest) (make-tree (car ns) lis)
            (receive (forest last-cdr) (make-forest (cdr ns) rest)
              (values (acons (car ns) tree forest) last-cdr)))))
      ;; Build one.
      (receive (forest last-cdr)
          (make-forest (reverse (series-2^n-1 spine-len)) lis)
        (values (SL forest) last-cdr)))
    (values skew-list-null lis)))

(define (list->skew-list lis)
  (receive (sl last-cdr) (list*->skew-list lis)
    (unless (null? last-cdr)
      (error "proper list required, but got" lis))
    sl))

(define (skew-list->list sl)
  (assume-type sl <skew-list>)
  (reverse (skew-list-fold sl cons '())))

;;;
;;; Comparison
;;;

(define-method object-equal? ((a <skew-list>) (b <skew-list>))
  (equal? (skew-list-elements a)
          (skew-list-elements b)))

;;;
;;; Utilities
;;;

(define (skew-list-fold sl proc seed)
  (define (tree-fold tree seed)
    (match tree
      [('Leaf x) (proc x seed)]
      [('Node x t1 t2)
       (tree-fold t2 (tree-fold t1 (proc x seed)))]))
  (assume-type sl <skew-list>)
  (fold (^[p s] (tree-fold (cdr p) s)) seed (skew-list-elements sl)))

;; NB: We don't support general (n-ary) map; it can be done via
;; sequence framework.  One arg map is worth to support, for we can take
;; advantage of isomorphism of input and output.
(define (skew-list-map sl f)
  (define (tmap tree)
    (match tree
      [('Leaf x) `(Leaf ,(f x))]
      [('Node x t0 t1) `(Node ,(f x) ,(tmap t0) ,(tmap t1))]))
  (SL (map (^p `(,(car p) . ,(tmap (cdr p)))) (skew-list-elements sl))))

(define (skew-list-length sl)
  (assume-type sl <skew-list>)
  (fold (^[p s] (+ (car p) s)) 0 (skew-list-elements sl)))

(define (skew-list-length<=? sl k)
  (assume-type sl <skew-list>)
  (let loop ([es (skew-list-elements sl)] [s 0])
    (cond [(< k s) #f]
          [(null? es) #t]
          [else (loop (cdr es) (+ s (caar es)))])))

;; Tree, Int, Int, [Tree] -> [Tree]
;; Given Tree of size N, returns [Tree] which includes the K-th element
;; and after.  The last input is the tail of the output.
(define (tree-kth-and-after t n k tail)
  (if (zero? k)
    (cons t tail)
    (match-let1 ('Node _ t1 t2) t
      (let1 m (/2 n)
        (if (<= k m)
          (tree-kth-and-after t1 m (- k 1) (cons t2 tail))
          (tree-kth-and-after t2 m (- k m 1) tail))))))

;; [(Int, Tree)], Int -> [(Int, Tree)], Int
;; Skips elts in seq that doesn't contain K-th element.  Returns offset as well.
(define (skip-seq seq k)
  (if (< k (caar seq))
    (values seq k)
    (skip-seq (cdr seq) (- k (caar seq)))))

(define (%skew-list-iterator sl start end_)
  (define seq (skew-list-elements sl))
  (define stack '())  ; [Tree]
  (define end (or end_ (skew-list-length sl)))
  (define pos start)
  (define (next)
    (if (= pos end)
      (values #f #t)
      (match stack
        [() (set! stack (list (cdr (pop! seq)))) (next)]
        [(('Leaf x) . xs) (set! stack xs) (inc! pos)(values x #f)]
        [(('Node x t1 t2) . xs) (set! stack (list* t1 t2 xs)) (inc! pos) (values x #f)])))
  ;; Adjust start point
  (unless (<= start end) (errorf "start ~s is greater than end ~s" start end))
  (when (< 0 start)
    (receive (seq_ k) (skip-seq seq start)
      (set! stack (tree-kth-and-after (cdar seq_) (caar seq_) k '()))
      (set! seq (cdr seq_))))
  next)

(define (skew-list->generator sl :optional start end)
  (define iter (%skew-list-iterator sl
                                    (if (undefined? start) 0 start)
                                    (if (undefined? end) #f end)))
  (^[] (receive (x eof?) (iter)
         (if eof? (eof-object) x))))

(define (skew-list->lseq sl :optional start end)
  (define iter (%skew-list-iterator sl
                                    (if (undefined? start) 0 start)
                                    (if (undefined? end) #f end)))
  ((rec (loop)
     (receive (x eof?) (iter)
       (if eof?
         '()
         (lcons x (loop)))))))

;; take/drop first k elements
;;  In some cases we can share the some of the trees in the original skew-list.
;;  Other than the obvious cases when k is the sum of prefix of the original
;;  seq, there are some nontrivial cases.  Suppose 1 < n and n = 2m+1.
;;  If original seq begins with:
;;    [1 n ...]   ->  We can take 2 and 2+m if m>1
;;                    (e.g. from [1 7 ..] we can take [1 1] and [1 1 3]
;;    [n ...]     ->  We can take 1, 2, 1+m, 2+m' (where m = 2m'+1)
;;                    (e.g. from [15 ...] we can take [1 7], [1 1 3]

(define (%skew-list-splitter sl k rettype)
  ;; The branch logic is common among take, drop and split-at.  This macro
  ;; dispatches the return value.
  (define-syntax ret
    (syntax-rules ()
      [(_ taken dropped)
       (case rettype
         [(take) (SL taken)]
         [(drop) (SL dropped)]
         [(split) (cons (SL taken) (SL dropped))])]))
  (define elts (skew-list-elements sl))
  (define (trivial-prefix)
    (let loop ([seq elts] [sum 0] [i 0])
      (cond [(= k sum) (ret (take elts i) (drop elts i))]
            [(> k sum) (loop (cdr seq) (+ sum (caar seq)) (+ i 1))]
            [else #f])))
  (define (special-prefix)
    (match (skew-list-elements sl)
      [((1 . x) (n . ('Node y t1 t2)) . zs)
       (if (= k 2)
         (ret `((1 . ,x) (1 . (Leaf ,y)))
              `((,(/2 n) . ,t1) (,(/2 n) . ,t2)  ,@zs))
         (and (> n 3)
              (= k (+ 2 (/2 n)))
              (ret `((1 . ,x) (1 . (Leaf ,y)) (,(/2 n) . ,t1))
                   `((,(/2 n) . ,t2) ,@zs))))]
      [((n . ('Node x (and ('Node y t11 t12) t1) t2)) . zs)
       (cond [(= k 1) (ret `((1 . (Leaf ,x)))
                           `((,(/2 n) . ,t1) (,(/2 n) . ,t2) ,@zs))]
             [(= k 2) (ret `((1 . (Leaf ,x)) (1 . (Leaf ,y)))
                           `((,(/4 n) . ,t11) (,(/4 n) . ,t12) (,(/2 n) . ,t2) ,@zs))]
             [(= k (+ 1 (/2 n)))
              (ret `((1 . (Leaf ,x)) (,(/2 n) . ,t1))
                   `((,(/2 n) . ,t2) ,@zs))]
             [(and (> k 3) (= k (+ 2 (/4 n))) )
              (ret `((1 . (Leaf ,x)) (1 . (Leaf ,y)) (,(/4 n) . ,t11))
                   `((,(/4 n) . ,t12) (,(/2 n) . ,t2) ,@zs))]
             [else #f])]
      [((n . ('Node x ('Leaf y) t2)) . zs)
       (cond [(= k 1) (ret `((1 . (Leaf ,x)))
                           `((1 . (Leaf ,y)) (,(/2 n) . ,t2) ,@zs))]
             [(= k 2) (ret `((1 . (Leaf ,x)) (1 . (Leaf ,y)))
                           `((,(/2 n) . ,t2) ,@zs))]
             [else #f])]))
  (define (fallback)
    (case rettype
      [(take) (list->skew-list (skew-list->lseq sl 0 k))]
      [(drop) (list->skew-list (skew-list->lseq sl k))]
      [(split) (let1 lis (skew-list->list sl)
                 (receive (t d) (split-at lis k)
                   (cons (list->skew-list t) (list->skew-list d))))]))
  (or (trivial-prefix)
      (special-prefix)
      (fallback)))

(define (skew-list-take sl k) (%skew-list-splitter sl k 'take))
(define (skew-list-drop sl k) (%skew-list-splitter sl k 'drop))
(define (skew-list-split-at sl k)
  (let1 r (%skew-list-splitter sl k 'split)
    (values (car r) (cdr r))))

(define (skew-list-append sl . sls)
  (cond [(null? sls) sl]
        [(skew-list-empty? sl) (apply skew-list-append sls)]
        [(not (skew-list? sl)) (error "argument must be a skew list:" sl)]
        [else (%skew-list-append2 sl (apply skew-list-append sls))]))

(define (%skew-list-append2 sl1 sl2)
  (define (slow-append sl1 sl2)
    (list->skew-list (append (skew-list->list sl1) (skew-list->list sl2))))
  (match (skew-list-elements sl1)
    [((1 . ('Leaf x))) (skew-list-cons x sl2)]
    [((w0 . t0))
     (match-let1 ((w1 . t1) . ts) (skew-list-elements sl2)
       (if (= w0 w1)
         (SL (cons (car (skew-list-elements sl1)) (skew-list-elements sl2)))
         (slow-append sl1 sl2)))]
    [_
     (match-let1 (wz . tz) (last (skew-list-elements sl1))
       (match-let1 ((w1 . t1) . ts) (skew-list-elements sl2)
         (if (< wz w1)
           (SL (append (skew-list-elements sl1) (skew-list-elements sl2)))
           (slow-append sl1 sl2))))]))


;;;
;;; Collection & sequence protocol
;;;

(define-method call-with-iterator ((sl <skew-list>) proc)
  (define iter (%skew-list-iterator sl 0 #f))
  (define-values (item end) (iter))
  (define (end?) end)
  (define (next) (begin0 item
                   (set!-values (item end) (iter))))
  (proc end? next))

(define-method call-with-builder ((slc <skew-list-meta>) proc :allow-other-keys)
  (let1 q (make-queue)
    (proc (cut enqueue! q <>)
          (cut list->skew-list (dequeue-all! q)))))

(define-method fold (proc knil (sl <skew-list>))
  (skew-list-fold sl proc knil))

(define-method size-of ((sl <skew-list>)) (skew-list-length sl))

(define-method referencer ((sl <skew-list>))
  (^[o i] (skew-list-ref o i)))
