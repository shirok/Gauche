;;;
;;;  data.random - Random data genarators
;;;
;;;   Copyright (c) 2013  Shiro Kawai  <shiro@acm.org>
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

(define-module data.random
  (use srfi-1)
  (use srfi-14)
  (use srfi-42)
  (use math.const)
  (use math.mt-random)
  (use gauche.uvector)
  (use gauche.generator)
  (use gauche.parameter)
  (use gauche.sequence)
  
  (export make-random-data-state current-random-data-seed with-random-data-seed

          integer integer-between fixnum char boolean
          int8 uint8 int16 uint16 int32 uint32 int64 uint64
          real real-between normal exponential geometric poisson

          default-sizer
          one-of pair-of tuple-of list-of vector-of string-of
          permutation-of combination-of weighted-sample
          ))
(select-module data.random)

;; Random state management
;; Random state is kept in %random-source parameter as (seed . #<mt>)
;; We use mt-random directly instead of srfi-27, for we need
;; a portable way to save and restore the random state.
;; Srfi-27's random state isn't guaranteed to be printable.

(define (make-random-data-state seed)
  (cons seed (make <mersenne-twister> :seed seed)))

(define %random-data-state
  (make-parameter (make-random-data-state 42)))

(define (current-random-data-seed)
  (car (%random-data-state)))

(define (with-random-data-seed seed thunk)
  (parameterize ([%random-data-state (make-random-data-state seed)])
    (thunk)))


(define (%rand-int n) (mt-random-integer (cdr (%random-data-state)) n))
(define (%rand-real0) (mt-random-real0 (cdr (%random-data-state))))
(define (%rand-real)  (mt-random-real (cdr (%random-data-state))))

;;;
;;; Primitive generators
;;;

;; A naming idea: We could provide plural names for those primitive samplers
;; (e.g. integers for integer, integers-between for integer-between),
;; then we could write a generator of a list of three random integers as
;; (list-of 3 fixnums), for example.  This is kind of cool... though
;; I'm not sure if it's a good idea to double the number of exported names
;; just for this kind of cosmetic readability.

;;
;; Uniform distribution
;;

;; API.  Generate integers uniformly in [start, start+size)
(define (integer size :optional (start 0))
  (^[] (+ (%rand-int size) start)))

;; API.  Generate integers uniformly in [lb ub]
;; (We avoid 'integer-range', for 'range' API takes exclusive upper bound.)
(define (integer-between lb ub)
  (let1 range (- ub lb -1)
    (^[] (+ (%rand-int range) lb))))

;; API.
(define fixnum (integer-between (least-fixnum) (+ (greatest-fixnum) 1)))
(define int8   (integer 256 -128))
(define uint8  (integer 256))
(define int16  (integer 65536 -32768))
(define uint16 (integer 65536))
(define int32  (integer (expt 2 32) (- (expt 2 31))))
(define uint32 (integer (expt 2 32)))
(define int64  (integer (expt 2 64) (- (expt 2 64))))
(define uint64 (integer (expt 2 64)))

;; API.
(define boolean (^[] (zero? (%rand-int 2))))

;; API.
;; The default value of cset is debatable.  We play "safe" here, limiting
;; ascii alphabets and digits, which would satisfy typical use cases without
;; worrying character encodings too much.
(define (char :optional (cset #[A-Za-z0-9]))
  ;; We map the integer within the total # of chars in CSET into
  ;; the delimited ranges of characters in CSET.  For example, if CSET
  ;; is splitted into a ranges ((48 . 57) (65 . 90) (97 . 122)),
  ;; the table has the following mappings:
  ;;    0 -> 48         (for range 48-57) 
  ;;   10 -> (- 65 10)  (for range 65-90)
  ;;   36 -> (- 97 36)  (for range 97-122)
  ;; We generate a random integer in [0, 61], that is, the size same as
  ;; the # of chars in the CSET, then use tree-map-floor to find where it
  ;; belongs; that gives us an offset to add to the sample number to get
  ;; the char code.
  ;;
  ;; If CSET has only one range, however, we use simpler sampling
  ;; for optimization.
  (let1 ranges ((with-module gauche.internal %char-set-ranges) cset)
    (if (null? (cdr ranges))
      ;; simple case
      (let* ([lb (caar ranges)]
             [ub (cdar ranges)]
             [size (- (+ ub 1) lb)])
        (^[] (integer->char (+ (%rand-int size) lb))))
      ;; general case
      (let* ([tab (make-tree-map)]
             [total (do ([ranges ranges (cdr ranges)]
                         [cumu 0 (+ cumu (- (+ (cdar ranges) 1) (caar ranges)))])
                        [(null? ranges) cumu]
                      (tree-map-put! tab cumu (- (caar ranges) cumu)))])
        (^[] (let1 p (%rand-int total)
               (receive (_ off) (tree-map-floor tab p)
                 (integer->char (+ p off)))))))))

;; API.  Extra clamp ensures fp errors won't cause out-of-range value.
;; NB: We clamp instead of rejecting the out-of-range value---since such
;; value can only be produced on the FP values on the edge, and we should
;; regard it inappropriate rounding (for our purpose).  If we reject them,
;; it will skew the distribution.
(define (real :optional (size 1.0) (start 0.0))
  (let1 ub (+ size start)
    (^[] (clamp (+ start (* size (%rand-real0))) start ub))))
(define (real-between lb ub)
  (let1 range (+ lb ub)
    (^[] (clamp (- (* range (%rand-real0)) lb) lb ub))))

;; rational
;; complex

;;
;; Nonuniform distributions
;;

;; Normal distribution (continuous - generates real numbers)
;; Box-Muller algorithm
(define (normal :optional (mean 0) (deviation 1))
  (^[] (let ([r (%sqrt (* -2 (%log (%rand-real))))]
             [theta (* 2pi (%rand-real))])
         (+ mean (* deviation r (%sin theta))))))

#|
Simple test of gaussian sampling: Generate some data with this:

(with-output-to-file "tmp"
  (^[] (let1 g (normal 2 3) (dotimes [i 1000000] (print (g))))))

Then plot with gnuplot:

binwidth=0.025
bin(x,width)=width*floor(x/width)
plot 'tmp' using (bin($1,binwidth)):(1.0) smooth freq with boxes
|#

;; The following definition of `normal' implements Ziggurat method in
;; Jurgen A Doornik, An Improved Ziggurat Method to Generate Normal Random
;; Samples, 2005.  http://www.doornik.com/research.html
;; Supposedly it is faster than Box-Muller, but written in Gauche,
;; Box-Muller is actually faster (about 12%).
#|
(define normal2
  (let ([blocks 128]        ; # of blocks
        [X1 3.442619855899] ; x coord of the start of the right tail
        [A 9.91256303526217e-3] ; area of each block
        ;; Arrays Xs and Rs are initialized when gaussian is called first time.
        [Xs #f] ; X coords of right edge of each block.  Xs[1] is same as X1.
                ; X0 isn't corresponds to the box, but used for rejection.
        [Rs #f] ; Precomputed ratio of adjacent Xs; Rs[i] = Xs[i+1]/Xs[i].
                ; After the block selection, if an absolute value of a sample
                ; value u <- U(0,1) falls below this, we know we can accept it.
                ; for sure.
        )

    (define (fd a b) (exp (* -0.5 (- (* a a) (* b b)))))
    (define (f x)    (exp (* -0.5 x x)))

    ;; Initialization.  Only called once when Xs is #f, and sets up Xs and Rs.
    (define (init)
      (let ([xs (make-f64vector (+ blocks 1))]
            [rs (make-f64vector blocks)])
        (f64vector-set! xs 0 (/ A (f X1)))
        (f64vector-set! xs blocks 0)
        (do ([i 1 (+ i 1)]
             [x X1 (sqrt (* -2 (log (+ (/ A x) (f x)))))])
            [(= i blocks)]
          (f64vector-set! xs i x))
        (do-ec (: i blocks)
               (f64vector-set! rs i (/ (f64vector-ref xs (+ i 1))
                                       (f64vector-ref xs i))))
        ;; MT-safe; the result is always the same, and we ensure Xs is
        ;; set after Rs.
        (set! Rs rs)
        (set! Xs xs)))

    ;; Tail case.  Called rarely, so we can spend some time.
    (define (normal-tail)
      (let ([x (/ (%log (%rand-real)) X1)]
            [y (%log (%rand-real))])
        (if (< (* y -2) (* x x))
          (normal-tail)
          (- X1 x))))

    ;; Draw from normal distribution.
    (define (sample)
      (let ([u (- (* 2 (%rand-real)) 1)]  ; U(-1,1)
            [i (%rand-int 128)])
        (cond [(< (abs u) (f64vector-ref Rs i)) ; we can accept u
               (* u (f64vector-ref Xs i))]
              [(= i 0)                          ; bottom block
               (if (< u 0) (normal-tail) (- (normal-tail)))]
              [else                             ; check precisely
               (let* ([x (* u (f64vector-ref Xs i))]
                      [f0 (fd (f64vector-ref Xs i) x)]
                      [f1 (fd (f64vector-ref Xs (+ i 1)) x)])
                 (if (< (+ f1 (* (%rand-real) (- f0 f1))) 1.0)
                   x                            ; accepted
                   (sample)))])))

    ;; Main body
    (^[:optional (mean 0) (deviation 1)]
      (unless Xs (init))
      (^[] (+ (* (sample) deviation) mean)))))
|#

;; Exponential distribution - continuous
(define (exponential m)
  (^[] (- (* m (log (%rand-real))))))

;; Draw from geometric distribution, with success probability p.
;; Mean is 1/p, variance is (1-p)/p^2
(define (geometric p)
  (let1 c (/ (log (- 1.0 p)))
    (^[] (ceiling->exact (* c (log (%rand-real)))))))

;; Draw from poisson distribution with mean L, variance L.
;; For small L, we use Knuth's method.  For larger L, we use rejection
;; method by Atkinson, The Computer Generation of Poisson Random Variables,
;; J. of the Royal Statistical Society Series C (Applied Statistics), 28(1),
;; pp29-35, 1979.  The code here is a port by John D Cook's C++ implementation
;; (http://www.johndcook.com/stand_alone_code.html )
(define (poisson L)
  (if (< L 36)
    (^[] (do ([exp-L (exp (- L))]
              [k 0 (+ k 1)]
              [p 1.0 (* p (%rand-real))])
             [(<= p exp-L) (- k 1)]))
    (let* ([c (- 0.767 (/ 3.36 L))]
           [beta (/ pi (sqrt (* 3 L)))]
           [alpha (* beta L)]
           [k (- (log c) L (log beta))])
      (rec (loop)
        (let* ([u (%rand-real)]
               [x (/ (- alpha (log (/ (- 1.0 u) u))) beta)]
               [n (floor->exact (+ x 0.5))])
          (if (< n 0)
            (loop)
            (let* ([v (%rand-real)]
                   [y (- alpha (* beta x))]
                   [t (+ 1.0 (exp y))]
                   [lhs (+ y (log (/ v (* t t))))]
                   [rhs (+ k (* n (log L)) (- (lgamma (+ n 1))))])
              (if (<= lhs rhs)
                n
                (loop)))))))))

#|
;; This generates histograms of samples
;; e.g.  (gen-hist 1000 (poisson 5) (poisson 10) (poisson 20) (poisson 50))
(define (gen-hist count . thunks)
  (let ([bins (map (^_ (make-hash-table 'eqv?)) thunks)]
        [maxkey 0])
    (dotimes [n count]
      (do-ec (:parallel (: thunk thunks)
                        (: bin bins))
        (let1 key (thunk)
          (when (> key maxkey) (set! maxkey key))
          (hash-table-update! bin key (cut + <> 1) 0))))
    (dotimes [n (+ maxkey 1)]
      (print
       (string-join (cons (x->string n)
                          (map (^[bin] (x->string (hash-table-get bin n 0)))
                               bins))
                    " ")))))
|#

;;;
;;; Generator combinators
;;;

(define (one-of seq-of-gen)
  (let1 s (size-of seq-of-gen)
    (^[] ((~ seq-of-gen (%rand-int s))))))

(define (pair-of car-gen cdr-gen) (^[] (cons (car-gen) (cdr-gen))))

(define (tuple-of . gens) (gmap (^g (g)) gens))

;; We accept constant integer or generator
(define-syntax %with-sizer
  (syntax-rules ()
    [(_ sizer . body)
     (let1 sizer (if (integer? sizer) (^[] sizer) sizer)
       . body)]))

(define default-sizer (make-parameter (poisson 4)))
     
(define list-of
  (case-lambda
    [(item-gen) (list-of (default-sizer) item-gen)]
    [(sizer item-gen)
     (%with-sizer sizer (^[] (list-tabulate (sizer) (^_ (item-gen)))))]))

(define vector-of
  (case-lambda
    [(item-gen) (vector-of (default-sizer) item-gen)]
    [(sizer item-gen)
     (%with-sizer sizer
       (^[] (let1 len (sizer)
              (rlet1 vec (make-vector len)
                (do-ec (: i len) (vector-set! vec i (item-gen)))))))]))

(define string-of
  (case-lambda
    [()         (string-of (default-sizer) (char))]
    [(item-gen) (string-of (default-sizer) item-gen)]
    [(sizer item-gen) 
     (%with-sizer sizer
       (^[] (let1 len (sizer)
              (with-output-to-string
                (^[] (do-ec (: i len) (display (item-gen))))))))]))

(define (permutation-of seq)
  (^[] (shuffle seq (cdr (%random-data-state)))))

(define (combination-of seq)
  (let1 indices (list->vector (iota len))
    (^[] (let1 ix (shuffle indices)
           (list-ec (: i len) (ref seq (vector-ref ix i)))))))

;; weight&gens :: ((<real> . <generator>) ...)
(define (weighted-sample weight&gens)
  (let* ([tab (make-tree-map)]
         [total (do [(w&g weight&gens (cdr w&g))
                     (cumu 0 (+ cumu (caar w&g)))]
                    [(null? w&g) cumu]
                  (tree-map-put! tab cumu (cdar w&g)))])
    (^[] (receive (_ gen) (tree-map-floor tab (* (%rand-real0) total))
           (gen)))))
