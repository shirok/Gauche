;;;
;;; Matrix operations on arrays
;;;
;;;
;;;  Copyright (C) 2004 Alex Shinn (foof@synthcode.com)
;;;
;;;  Permission to use, copy, modify, distribute this software and
;;;  accompanying documentation for any purpose is hereby granted,
;;;  provided that existing copyright notices are retained in all
;;;  copies and that this notice is included verbatim in all
;;;  distributions.
;;;  This software is provided as is, without express or implied
;;;  warranty.  In no circumstances the author(s) shall be liable
;;;  for any damages arising out of the use of this software.
;;;

(select-module gauche.array)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general array manipulation

(define (array-concatenate a b :optional (dimension 0))
  (let* ([a-start (start-vector-of a)]
         [a-end (end-vector-of a)]
         [b-start (start-vector-of b)]
         [b-end (end-vector-of b)]
         [diff (s32vector->vector (s32vector-sub a-start b-start))]
         [a-rank (s32vector-length a-start)]
         [b-rank (s32vector-length b-start)])
    (unless (= a-rank b-rank)
      (error "rank mismatch"))
    (unless (every (^d (or (= d dimension)
                           (= (- (s32vector-ref a-end d) (s32vector-ref a-start d))
                              (- (s32vector-ref b-end d) (s32vector-ref b-start d)))))
                   (iota a-rank))
      (error "shape mismatch"))
    (let ([c-start (s32vector-copy a-start)]
          [c-end (s32vector-copy a-end)])
      (s32vector-set! c-end dimension (+ (s32vector-ref c-end dimension)
                                         (- (s32vector-ref b-end dimension)
                                            (s32vector-ref b-start dimension))))
      (let ([c (make-minimal-backend-array
                (list a b) (start/end-vector->shape c-start c-end))]
            [off (- (s32vector-ref a-end dimension) (s32vector-ref a-start dimension))])
        (array-for-each-index a
          (^i (array-set! c i (array-ref a i)))
          (make-vector a-rank))
        (array-for-each-index b
          (^i (let1 j (vector-copy i)
                (dotimes [dim a-rank]
                  (vector-set! j dim (+ (vector-ref j dim) (vector-ref diff dim))))
                (vector-set! j dimension (+ off (vector-ref j dimension)))
                (array-set! c j (array-ref b i))))
          (make-vector b-rank))
        c))))

(define (array-transpose a :optional (dim1 0) (dim2 1))
  (let* ([sh (array-copy (array-shape a))]
         [rank (array-rank a)]
         [tmp0 (array-ref sh dim1 0)]
         [tmp1 (array-ref sh dim1 1)])
    (array-set! sh dim1 0 (array-ref sh dim2 0))
    (array-set! sh dim1 1 (array-ref sh dim2 1))
    (array-set! sh dim2 0 tmp0)
    (array-set! sh dim2 1 tmp1)
    (rlet1 res (make-array-internal (class-of a) sh)
      (array-for-each-index a
        (^[vec1] (let* ([vec2 (vector-copy vec1)]
                        [tmp (vector-ref vec2 dim1)])
                   (vector-set! vec2 dim1 (vector-ref vec2 dim2))
                   (vector-set! vec2 dim2 tmp)
                   (array-set! res vec2 (array-ref a vec1))))
        (make-vector rank)))))

(define (array-rotate-90 a :optional (dim1 0) (dim2 1))
  (let* ([sh (array-copy (array-shape a))]
         [rank (array-rank a)]
         [tmp0 (array-ref sh dim1 0)]
         [tmp1 (array-ref sh dim1 1)]
         [off (- (array-ref sh dim1 1) 1)])
    (array-set! sh dim1 0 (array-ref sh dim2 0))
    (array-set! sh dim1 1 (array-ref sh dim2 1))
    (array-set! sh dim2 0 tmp0)
    (array-set! sh dim2 1 tmp1)
    (rlet1 res (make-array-internal (class-of a) sh)
      (array-for-each-index a
        (^[vec1] (let* ([vec2 (vector-copy vec1)]
                        [tmp (vector-ref vec2 dim1)])
                   (vector-set! vec2 dim1 (vector-ref vec2 dim2))
                   (vector-set! vec2 dim2 (- off tmp))
                   (array-set! res vec2 (array-ref a vec1))))
        (make-vector rank)))))

(define (array-flip! a :optional (dimension 0))
  (let* ([lo (s32vector-ref (start-vector-of a) dimension)]
         [end (end-vector-of a)]
         [hi (s32vector-ref end dimension)]
         [half (quotient hi 2)]
         [rank (s32vector-length end)])
    (array-for-each-index-by-dimension a (delete! dimension (iota rank))
      (^[vec1]
        (do ([i lo (+ i 1)])
            [(= i half)]
          (vector-set! vec1 dimension i)
          (let* ([j (- hi i 1)]
                 [vec2 (vector-copy vec1)]
                 [tmp (array-ref a vec1)])
            (vector-set! vec2 dimension j)
            (array-set! a vec1 (array-ref a vec2))
            (array-set! a vec2 tmp))))
      (make-vector rank))
    a))

(define (array-flip a . args)
  (rlet1 res (array-copy a)
    (apply array-flip! res args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; linear algebra

(define (identity-array n :optional (class <array>))
  (let1 res (make-array-internal class (shape 0 n 0 n) 0)
    (do ([i 0 (+ i 1)])
        [(= i n) res]
      (array-set! res i i 1))))

;; Gaussian elimination, returns factor applied to determinant
(define (array-row-echelon! a)
  (let* ([start (start-vector-of a)]
         [row-start (s32vector-ref start 0)]
         [col-start (s32vector-ref start 1)]
         [end (end-vector-of a)]
         [row-end (s32vector-ref end 0)]
         [col-end (s32vector-ref end 1)]
         [row-col-offset (- row-start col-start)])
    (define (row-swap! i j)
      (do ([k col-start (+ k 1)])
          [(= k col-end)]
        (let1 temp (array-ref a i k)
          (array-set! a i k (array-ref a j k))
          (array-set! a j k temp))))
    (define (row-sub! i j factor)
      (do ([k col-start (+ k 1)])
          [(= k col-end)]
        (array-set! a i k (- (array-ref a i k) (* factor (array-ref a j k))))))
    (let loop ([i row-start] [factor 1])
      (let1 col (- i row-col-offset)
        (cond [(= i row-end) factor]
              [(zero? (array-ref a i col))
               ;; pivot non-zero row to top
               (let loop2 ((j (+ i 1)))
                 (cond [(= j row-end) 0]
                       [(zero? (array-ref a j col))
                        (loop2 (+ j 1))]
                       [else
                        (row-swap! j i)
                        (loop i (* factor -1))]))]
              [else
               ;; eliminate other non-zero rows
               (let loop2 ([j (+ i 1)])
                 (cond [(= j row-end) (loop (+ i 1) factor)]
                       [(zero? (array-ref a j col))
                        (loop2 (+ j 1))]
                       [else
                        (let1 factor (/ (array-ref a j col)
                                        (array-ref a i col))
                          (row-sub! j i factor)
                          (loop2 (+ j 1)))]))])))))

(define (array-solve-left-identity! a)
  (array-row-echelon! a)
  (let* ([start (start-vector-of a)]
         [row-start (s32vector-ref start 0)]
         [col-start (s32vector-ref start 1)]
         [end (end-vector-of a)]
         [row-end (s32vector-ref end 0)]
         [col-end (s32vector-ref end 1)]
         [row-col-offset (- row-start col-start)])
    ;; zero-out
    (do ([i (- row-end 1) (- i 1)])   ; for-each row in reverse
        [(< i row-start)]
      (let1 divisor (array-ref a i (- i row-col-offset))
        (unless (zero? divisor)
          (do ([j (- i 1) (- j 1)])   ; for-each lower row
              [(< j row-start)]
            (let1 factor (/ (array-ref a j (- i row-col-offset)) divisor)
              (array-set! a i (- j row-col-offset) 0)
              (do ([k (- j row-col-offset) (+ k 1)]) ; for-each column
                  [(= k col-end)]
                (array-set! a j k (- (array-ref a j k)
                                     (* factor (array-ref a i k))))))))))
    ;; reduce
    (do ([i row-start (+ i 1)])       ; for-each row
        [(= i row-end)]
      (let1 divisor (array-ref a i (- i row-col-offset))
        (unless (or (zero? divisor) (= divisor 1))
          (array-set! a i (- i row-col-offset) 1)
          (do ([j (- i row-col-offset -1) (+ j 1)]) ; for-each higher column
              [(= j col-end)]
            (array-set! a i j (/ (array-ref a i j) divisor))))))))

(define (array-inverse a)
  (let* ([start (start-vector-of a)]
         [end (end-vector-of a)]
         [rank (s32vector-length start)]
         [n (- (s32vector-ref end 0) (s32vector-ref start 0))]
         [m (- (s32vector-ref end 1) (s32vector-ref start 1))])
    (unless (= 2 rank)
      (error "can only compute inverses of 2D arrays"))
    (unless (= n m)
      (error "can only compute inverses of square matrices"))
    (let* ([class (class-of a)]
           [id (identity-array n (if (or (eq? class <f32array>)
                                         (eq? class <f64array>))
                                   class <array>))]
           [tmp (array-concatenate a id 1)])
      (array-solve-left-identity! tmp)
      (and (= 1 (array-ref tmp (- (s32vector-ref end 0) 1)
                           (- (s32vector-ref end 1) 1)))
           (subarray tmp (shape (s32vector-ref start 0) (s32vector-ref end 0)
                                (s32vector-ref end 1) (+ (s32vector-ref end 1) n)))))))


(define (determinant! a)
  (let* ([start (s32vector->list (start-vector-of a))]
         [end (s32vector->list (end-vector-of a))]
         [row-col-offset (- (car start) (cadr start))]
         [factor (array-row-echelon! a)])
    (unless (= 2 (length start)) ; add determinant for the 2x2x2 case?
      (error "can't compute hyperdeterminants in the general case"))
    (unless (apply = (map - end start))
      (error "can't compute determinants of non-square matrices"))
    (apply * factor (map (^i (array-ref a i (- i row-col-offset)))
                         (map (cute + <> (car start))
                              (iota (- (car end) (car start))))))))

(define (determinant a)
  (let1 class (class-of a)
    (if (or (eq? class <f32array>)
            (eq? class <f64array>)
            (eq? class <array>))
      (determinant! (array-copy a))
      (let* ([rank (s32vector-length (start-vector-of a))]
             [b (tabulate-array (array-shape a)
                                (^[ind] (array-ref a ind))
                                (make-vector rank))])
        (determinant! b)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; matrix arithmetic

(define (array-mul2 a b) ; NxM * MxP => NxP
  (let ([a-start (start-vector-of a)]
        [a-end (end-vector-of a)]
        [b-start (start-vector-of b)]
        [b-end (end-vector-of b)])
    (unless (= 2 (s32vector-length a-start) (s32vector-length b-start))
      (error "array-mul matrices must be of rank 2"))
    (let ([n (- (s32vector-ref a-end 0) (s32vector-ref a-start 0))]
          [m (- (s32vector-ref a-end 1) (s32vector-ref a-start 1))]
          [p (- (s32vector-ref b-end 1) (s32vector-ref b-start 1))]
          [a-col-b-row-off (- (s32vector-ref a-start 1)
                              (s32vector-ref b-start 0))])
      (let1 res (make-minimal-backend-array (list a b) (shape 0 n 0 p))
        (unless (= m (- (s32vector-ref b-end 0) (s32vector-ref b-start 0)))
          (errorf "dimension mismatch: can't mul shapes ~S and ~S"
                  (array-shape a) (array-shape b)))
        (do ([i (s32vector-ref a-start 0) (+ i 1)])       ; for-each row of a
            [(= i (s32vector-ref a-end 0)) res]
          (do ([k (s32vector-ref b-start 1) (+ k 1)])     ; for-each col of b
              [(= k (s32vector-ref b-end 1))]
            (let1 tmp 0
              (do ([j (s32vector-ref a-start 1) (+ j 1)]) ; for-each col of a & row of b
                  [(= j (s32vector-ref a-end 1))]
                (inc! tmp (* (array-ref a i j)
                             (array-ref b (- j a-col-b-row-off) k))))
              (array-set! res
                          (- i (s32vector-ref a-start 0))
                          (- k (s32vector-ref b-start 1)) tmp))))))))

(define (%array-mul r a b) ; NxM * MxP => NxP
  (let ([a-start (start-vector-of a)]
        [a-end (end-vector-of a)]
        [b-start (start-vector-of b)]
        [b-end (end-vector-of b)])
    (unless (= 2 (s32vector-length a-start) (s32vector-length b-start))
      (error "array-mul matrices must be of rank 2"))
    (let ([a-start-row (s32vector-ref a-start 0)]
          [a-end-row (s32vector-ref a-end 0)]
          [a-start-col (s32vector-ref a-start 1)]
          [a-end-col (s32vector-ref a-end 1)]
          [b-start-col (s32vector-ref b-start 1)]
          [b-end-col (s32vector-ref b-end 1)])
      (let ([n (- a-end-row a-start-row)]
            [m (- a-end-col a-start-col)]
            [p (- b-end-col b-start-col)]
            [a-col-b-row-off (- a-start-col (s32vector-ref b-start 0))])
        (unless (= m (- (s32vector-ref b-end 0) (s32vector-ref b-start 0)))
          (errorf "dimension mismatch: can't multiply shapes ~S and ~S"
                  (array-shape a) (array-shape b)))
        '(when (and r (not (and (= n (array-length r 0))
                               (= p (array-length r 1)))))
          (errof "result array can't hold the result of multiplication"))
        (let1 res (or r (make-minimal-backend-array (list a b) (shape 0 n 0 p)))
          (do ([i a-start-row (+ i 1)])       ; for-each row of a
              [(= i a-end-row) res]
            (do ([k b-start-col (+ k 1)])     ; for-each col of b
                [(= k b-end-col)]
              (let1 tmp 0
                (do ([j a-start-col (+ j 1)]) ; for-each col of a & row of b
                    [(= j a-end-col)]
                  (inc! tmp (* (array-ref a i j)
                               (array-ref b (- j a-col-b-row-off) k))))
                (array-set! res (- i a-start-row) (- k b-start-col) tmp)))))))))

(define (array-mul a b) (%array-mul #f a b))

(define (array-mul3 a b) ; NxM * MxP => NxP
  (let ([a-start (start-vector-of a)]
        [a-end (end-vector-of a)]
        [b-start (start-vector-of b)]
        [b-end (end-vector-of b)])
    (unless (= 2 (s32vector-length a-start) (s32vector-length b-start))
      (error "array-mul matrices must be of rank 2"))
    (let* ([a-start-row (s32vector-ref a-start 0)]
           [a-end-row (s32vector-ref a-end 0)]
           [a-start-col (s32vector-ref a-start 1)]
           [a-end-col (s32vector-ref a-end 1)]
           [b-start-col (s32vector-ref b-start 1)]
           [b-end-col (s32vector-ref b-end 1)]
           [n (- a-end-row a-start-row)]
           [m (- a-end-col a-start-col)]
           [p (- b-end-col b-start-col)]
           [a-col-b-row-off (- a-start-col (s32vector-ref b-start 0))]
           [res (make-minimal-backend-array (list a b) (shape 0 n 0 p))])
      (unless (= m (- (s32vector-ref b-end 0) (s32vector-ref b-start 0)))
        (errorf "dimension mismatch: can't mul shapes ~S and ~S"
                (array-shape a) (array-shape b)))
      (do ([i a-start-row (+ i 1)])       ; for-each row of a
          [(= i a-end-row) res]
        (do ([k b-start-col (+ k 1)])     ; for-each col of b
            [(= k b-end-col)]
          (let1 tmp 0
            (do ([j a-start-col (+ j 1)]) ; for-each col of a & row of b
                [(= j a-end-col)]
              (inc! tmp (* (array-ref a i j) (array-ref b (- j a-col-b-row-off) k))))
            (array-set! res (- i a-start-row) (- k b-start-col) tmp)))))))



(define (array-div-left a b)
  (if-let1 b-1 (array-inverse b)
    (array-mul b-1 a)
    (error "Matrix is not regular:" b)))

(define (array-div-right a b)
  (if-let1 b-1 (array-inverse b)
    (array-mul a b-1)
    (error "Matrix is not regular:" b)))

(define (array-expt ar pow)
  (let loop ([a ar] [n pow])
    (case n
      [(0) (identity-array (s32vector-length (start-vector-of a)) (class-of a))]
      [(1) a]
      [(2) (array-mul a a)]
      [(3) (array-mul (array-mul a a) a)]
      [else
       (let* ([res1 (loop a (ash n -1))]
              [res (array-mul res1 res1)])
         (if (odd? n)
           (array-mul res a)
           res))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; element-wise operations (advantage over a direct array-map! is
;; ability to intermingle scalars)

;; add

(define-method array-add-elements! ((a <array-base>) b c . rest)
  (array-add-elements! a b)
  (apply array-add-elements! a c rest))

(define-method array-add-elements! ((a <array-base>))
  a)

(define-method array-add-elements! ((a <array-base>) (b <number>))
  (array-map! a (^i (+ b i)) a)
  a)

(define-method array-add-elements! ((a <number>) (b <array-base>))
  (rlet1 res (array-copy b)
    (array-map! res (^i (+ a i)) b)))

(define-method array-add-elements! ((a <array-base>) (b <array-base>))
  (array-map! a (^[i j] (+ i j)) a b)
  a)

(define (array-add-elements a . rest)
  (if (null? rest)
    (begin (assume (array? a)) a)
    (apply array-add-elements! (array-copy a) rest)))

;; sub

(define-method array-sub-elements! ((a <array-base>)) a)


(define-method array-sub-elements! ((a <array-base>) b c . rest)
  (array-sub-elements! a b)
  (apply array-sub-elements! a c rest))

(define-method array-sub-elements! ((a <array-base>) (b <number>))
  (array-map! a (^i (- i b)) a)
  a)

(define-method array-sub-elements! ((a <number>) (b <array-base>))
  (rlet1 res (array-copy b)
    (array-map! res (^i (- a i)) b)))

(define-method array-sub-elements! ((a <array-base>) (b <array-base>))
  (array-map! a (^[i j] (- i j)) a b)
  a)

(define (array-sub-elements a . rest)
  (if (null? rest)
    (begin (assume (array? a)) a)
    (apply array-sub-elements! (array-copy a) rest)))

(define (array-negate-elements! a) (array-sub-elements! 0 a))
(define (array-negate-elements a) (array-sub-elements! 0 (array-copy a)))

;; mul

(define-method array-mul-elements! ((a <array-base>) b c . rest)
  (array-mul-elements! a b)
  (apply array-mul-elements! a c rest))

(define-method array-mul-elements! ((a <array-base>))
  a)

(define-method array-mul-elements! ((a <array-base>) (b <number>))
  (array-map! a (^i (* b i)) a)
  a)

(define-method array-mul-elements! ((a <number>) (b <array-base>))
  (rlet1 res (array-copy b)
    (array-map! res (^i (* a i)) b)))

(define-method array-mul-elements! ((a <array-base>) (b <array-base>))
  (array-map! a (^[i j] (* i j)) a b)
  a)

(define (array-mul-elements a . rest)
  (if (null? rest)
    (begin (assume (array? a)) a)
    (apply array-mul-elements! (array-copy a) rest)))

;; div

(define-method array-div-elements! ((a <array-base>) b c . rest)
  (array-div-elements! a b)
  (apply array-div-elements! a c rest))

(define-method array-div-elements! ((a <array-base>))
  a)

(define-method array-div-elements! ((a <array-base>) (b <number>))
  (array-map! a (^i (/ i b)) a)
  a)

(define-method array-div-elements! ((a <number>) (b <array-base>))
  (rlet1 res (array-copy b)
    (array-map! res (^i (/ a i)) b)))

(define-method array-div-elements! ((a <array-base>) (b <array-base>))
  (array-map! a (^[i j] (/ i j)) a b)
  a)

(define (array-div-elements a . rest)
  (if (null? rest)
    (begin (assume (array? a)) a)
    (apply array-div-elements! (array-copy a) rest)))

(define (array-reciprocate-elements! a) (array-div-elements! 1 a))
(define (array-reciprocate-elements a) (array-div-elements! 1 (array-copy a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities

;; Nicely formatted printing for arrays of any rank, by default both
;; readable (using #,() syntax) and filled so columns line up.  Ranks
;; higher than 2 are represented as successive 2D drawings.
;;
;; Can generate (generally non-readable) box drawings with :left,
;; :right, etc. keywords.  For examples, Jacal-style matrix output can
;; be done with:
;;
;;  (pretty-print-array a #t :readable? #f :left "[" :right "]")
;;
;; and a tic-tac-toe board can be printed with
;;
;;  (pretty-print-array a #t :readable? #f :line-char "|"
;;     :top "-" :middle-col "|" :center-row "-")
;;
;; The API is subject to change.

(define (pretty-print-array a :optional (port (current-output-port))
                            :key (readable? #t) (fill? #t)
                            (line-char #f) (left line-char) (right left)
                            (middle-col #\space) (top line-char)
                            (bottom top) (center-row #f) (pad-char #\space))
  (let* ([p (cond [(port? port) port]
                  [port (current-output-port)]
                  [else (open-output-string)])]
         [rank (array-rank a)]
         [row-dim (- rank 2)]
         [col-dim (- rank 1)]
         [shape (array-shape a)]
         [shape-list (array->list shape)]
         [inner-shape (take-right shape-list 4)]
         [nrows (cadr inner-shape)]
         [ncols (cadddr inner-shape)])

    (define (replicate x width)
      (if (char? x)
        (make-string width x)
        (let1 len (string-length x)
          (if (eq? len 1)
            (make-string width (string-ref x 0))
            (receive (quot rem) (quotient&remainder width len)
              (string-append
               (apply string-append (map (^n x) (iota quot)))
               (substring x 0 rem)))))))
    (define (pad x width)
      (let* ([res (x->string x)]
             [len (string-length res)])
        (if (< len width)
          (string-append (make-string (- width len) pad-char) res )
          res)))
    (define (print-width x)
      (cond [(char? x) 1]
            [(string? x) (string-length x)]
            [else (string-length (x->string x))]))
    (let1 fixed-width (+ (if left (print-width left) 0)
                         (if right (print-width right) 0)
                         (if middle-col (* (- ncols 1) (print-width middle-col)) 0))
      (define (column-widths vec)
        (let loop1 ([col (caddr inner-shape)] [res '()])
          (cond
           [(< col ncols)
            (vector-set! vec col-dim col)
            (let loop2 ([row (car inner-shape)] [x 0])
              (cond
               [(< row nrows)
                (vector-set! vec row-dim row)
                (loop2 (+ row 1) (max x (print-width (array-ref a vec))))]
               [else
                (loop1 (+ col 1) (cons x res))]))]
           [else (reverse res)])))
      (define (print2d vec)
        (newline p)
        (let* ([widths (if fill? (column-widths (vector-copy vec)) '())]
               [total-width (apply + fixed-width widths)])
          (when top
            (display (if fill? (replicate top total-width) top) p)
            (newline p))
          (let loop1 ([row (car inner-shape)])
            (when (< row nrows)
              (when left (display left p))
              (vector-set! vec row-dim row)
              (let loop2 ([col (caddr inner-shape)] [w widths])
                (when (< col ncols)
                  (vector-set! vec col-dim col)
                  (display (if (pair? w)
                             (pad (array-ref a vec) (car w))
                             (array-ref a vec))
                           p)
                  (when (and middle-col (< (+ col 1) ncols))
                    (display middle-col p))
                  (loop2 (+ col 1) (if (pair? w) (cdr w) w))))
              (when right (display right p))
              (newline p)
              (when (and center-row (< (+ row 1) nrows))
                (display (if fill? (replicate bottom total-width) bottom))
                (newline p))
              (loop1 (+ row 1))))
          (when bottom
            (display (if fill? (replicate bottom total-width) bottom) p)
            (newline p))))
      (cond
       [(< rank 2)
        (if readable? (write a p) (display a p))]
       [else
        (when readable?
          (format p "#,(~A ~S" (class-name (class-of a)) shape-list))
        (if (= rank 2)
          (print2d (make-vector rank))
          (array-for-each-index-by-dimension
           a (drop-right (iota rank) 2)
           print2d
           (make-vector rank)))
        (when readable?
          (format p "  )"))
        (newline p)])
      (if (not port) (get-output-string p)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal utility to keep arrays uniform when possible

(define *signed-arrays*
  (list <s64array> <s32array> <s16array> <s8array>))

(define (make-minimal-backend-array ls sh . args)
  (define (return class)
    (apply make-array-internal class sh args))
  (define (signed? x)
    (member x *signed-arrays*))
  (define (size x)
    (cond [(or (eq? x <s64array>) (eq? x <u64array>)) 64]
          [(or (eq? x <s32array>) (eq? x <u32array>)) 32]
          [(or (eq? x <s16array>) (eq? x <u16array>)) 16]
          [(or (eq? x <s8array>) (eq? x <u8array>)) 8]
          [else 0]))
  (define (backend signed? size)
    (if signed?
      (case size
        [(64) <s64array>] [(32) <s32array>] [(16) <s16array>] [(8) <s8array>] [else <array>])
      (case size
        [(64) <u64array>] [(32) <u32array>] [(16) <u16array>] [(8) <u8array>] [else <array>])))
  (define (join a b)
    (cond [(or (eq? a <array>) (eq? b <array>)) <array>]
          [(or (eq? a <f32array>) (eq? a <f64array>))
           (if (or (eq? b <f32array>) (eq? b <f64array>))
             (if (eq? a b) a <f64array>)
             <array>)]
          [else
           (let ([a-s? (signed? a)]
                 [a-size (size a)]
                 [b-s? (signed? b)]
                 [b-size (size b)])
             (if (or (zero? a-size) (zero? b-size))
               <array>
               (let1 res-size (max a-size b-size)
                 (backend (or a-s? b-s?)
                          (if (if a-s? (not b-s?) b-s?)
                            (* 2 res-size) ; signs differ, double size
                            res-size)))))]))
  (let loop ([class (class-of (car ls))] [l ls])
    (if (null? l)
      (return class)
      (let1 next (join class (class-of (car l)))
        (if (eq? next <array>)
          (return <array>) ; already most general array
          (loop next (cdr l)))))))
