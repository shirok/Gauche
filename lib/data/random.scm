;;;
;;;  data.random - Random data genarators
;;;
;;;   Copyright (c) 2013-2024  Shiro Kawai  <shiro@acm.org>
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
  (use scheme.list)
  (use scheme.charset)
  (use srfi.27)
  (use srfi.42)
  (use util.match)
  (use math.const)
  (use math.mt-random)
  (use gauche.uvector)
  (use gauche.generator)
  (use gauche.sequence)

  (export random-data-random-source

          integers$ integers-between$ fixnums$ chars$
          samples$ booleans$
          int8s$ uint8s$ int16s$ uint16s$ int32s$ uint32s$ int64s$ uint64s$
          reals$ reals-between$ finite-flonums$
          complexes-rectangular$ complexes-polar$
          reals-normal$ reals-exponential$ reals-power-law$
          integers-geometric$ integers-poisson$
          regular-strings$

          ;; preconstructed
          fixnums booleans
          int8s uint8s int16s uint16s int32s uint32s int64s uint64s

          default-sizer
          samples-from pairs-of tuples-of lists-of vectors-of strings-of
          sequences-of
          permutations-of combinations-of weighted-samples-from

          ;; Deprecated API
          random-data-seed with-random-data-seed
          ))
(select-module data.random)

;; API
;;  Use SRFI-27's default-random-source as the default, for the compatibility
;;  to SRFI-194.
;;  In the current implementation, this is <mersenne-twister> instance, and
;;  unless the user explicitly randomize it, it uses a fixed predetermined
;;  seed.  It guarantees reproducibility.
(define random-data-random-source
  (make-parameter default-random-source
                  (^x (assume (random-source? x)) x)))

;; Deprecated API
;;  This is no longer useful, for the construted generators capture the
;;  random source at the time of construction and won't be affected
;;  by the changes of random-data-random-source afterwards.
(define random-data-seed
  (getter-with-setter
   (^[] (let1 rs (random-data-random-source)
          (if (is-a? rs <mersenne-twister>)
            (mt-random-get-seed rs)
            (undefined))))
   (^[seed] (random-data-random-source
             (make <mersenne-twister> :seed seed)))))

;; Deprecated API
(define (with-random-data-seed seed thunk)
  ;; create st here so that reentering thunk retains the state.
  (let1 st (make <mersenne-twister> :seed seed)
    (parameterize ([random-data-random-source st])
      (thunk))))

(define (%rand-int n s) (mt-random-integer s n))
(define (%rand-real0 s) (mt-random-real0 s))
(define (%rand-real s)  (mt-random-real s))

;;;
;;; Primitive generators
;;;

;;
;; Uniform distribution
;;

;; API.  Generate integers uniformly in [start, start+size)
(define (integers$ size :optional (start 0))
  (let1 s (random-data-random-source)
    (^[] (+ (%rand-int size s) start))))

;; API.  Generate integers uniformly in [lb ub]
;; (We avoid 'integer-range', for 'range' API takes exclusive upper bound.)
(define (integers-between$ lb ub)
  (let ([s (random-data-random-source)]
        [range (- ub lb -1)])
    (^[] (+ (%rand-int range s) lb))))

;; API.
(define (fixnums$) (integers-between$ (least-fixnum) (greatest-fixnum)))
(define (int8s$)   (integers$ 256 -128))
(define (uint8s$)  (integers$ 256))
(define (int16s$)  (integers$ 65536 -32768))
(define (uint16s$) (integers$ 65536))
(define (int32s$)  (integers$ (expt 2 32) (- (expt 2 31))))
(define (uint32s$) (integers$ (expt 2 32)))
(define (int64s$)  (integers$ (expt 2 64) (- (expt 2 63))))
(define (uint64s$) (integers$ (expt 2 64)))
(define (booleans$) (let1 g (integers$ 2) (cut zero? (g))))

;; API
;; Preconstructed generators.  They use default random source.
(define fixnums  (fixnums$))
(define int8s    (int8s$))
(define uint8s   (uint8s$))
(define int16s   (int16s$))
(define uint16s  (uint16s$))
(define int32s   (int32s$))
(define uint32s  (uint32s$))
(define int64s   (int64s$))
(define uint64s  (uint64s$))
(define booleans (booleans$))

;; API.
;; The default value of cset is debatable.  We play "safe" here, limiting
;; ascii alphabets and digits, which would satisfy typical use cases without
;; worrying character encodings too much.
(define (chars$ :optional (cset #[A-Za-z0-9]))
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
  (let ([ranges ((with-module gauche.internal %char-set-ranges) cset)]
        [s (random-data-random-source)])
    (if (null? (cdr ranges))
      ;; simple case
      (let* ([lb (caar ranges)]
             [ub (cdar ranges)]
             [size (- (+ ub 1) lb)])
        (^[] (integer->char (+ (%rand-int size s) lb))))
      ;; general case
      (let* ([tab (make-tree-map)]
             [total (do ([ranges ranges (cdr ranges)]
                         [cumu 0 (+ cumu (- (+ (cdar ranges) 1) (caar ranges)))])
                        [(null? ranges) cumu]
                      (tree-map-put! tab cumu (- (caar ranges) cumu)))])
        (^[] (let1 p (%rand-int total s)
               (receive (_ off) (tree-map-floor tab p)
                 (integer->char (+ p off)))))))))

;; API. Choose one of the elements from collection randomly.
;; NB: Not to be confused with samples-from below.
(define (samples$ coll)
  (let* ([vs (coerce-to <vector> coll)]
         [index-gen (integers$ (vector-length vs))])
    (^[] (vector-ref vs (index-gen)))))

;; API.  Extra clamp ensures fp errors won't cause out-of-range value.
;; NB: We clamp instead of rejecting the out-of-range value---since such
;; value can only be produced on the FP values on the edge, and we should
;; regard it inappropriate rounding (for our purpose).  If we reject them,
;; it will skew the distribution.
(define (reals$ :optional (size 1.0) (start 0.0))
  (let ([ub (+ size start)]
        [s (random-data-random-source)])
    (^[] (clamp (+ start (* size (%rand-real0 s))) start ub))))
(define (reals-between$ lb ub)
  (assume (<= lb ub))
  (let1 s (random-data-random-source)
    ;; lb + t*(ub-lb) = (1-t)*lb + t*ub
    ;; The first way may overflow when lb << ub, and suffer precision
    ;; loss if |lb| << |ub|.
    (^[] (let1 t (%rand-real0 s)
           (+ (* (- 1.0 t) lb) (* t ub))))))

;; API.  Uniformly sample from all possible finite flonums.
;; The distribution is not uniform in mathematical sense, since flonums
;; are denser when it's closer to zero.
;; The imlementation assumes IEEE double.
;; NB: We generate zeros with non-zero exponents, but they are coerced
;; to the canonical zeros internally.  That makes zeros appear x2000 likely
;; than other numbers.  Whether it is an issue or not depends on the
;; application; we leave it as it is for now.
(define (finite-flonums$)
  (let ([expgen (integers-between$ -1074 971)]
        [sgngen (samples$ '#(1 -1))]
        [mntgen (integers$ (real-expt 2 52))])
    (^[]
      (let ([exp (expgen)]
            [mantissa (mntgen)]
            [sign (sgngen)])
        (if (or (= -1074 exp) (zero? mantissa))
          (encode-float (vector mantissa exp sign)) ;denormalized or zero
          (encode-float (vector (+ (real-expt 2 52) mantissa) exp sign)))))))

;; rational

(define (complexes-rectangular$ re-lb re-ub im-lb im-ub)
  (assume (<= re-lb re-ub))
  (assume (<= im-lb im-ub))
  (let ([rgen (reals-between$ re-lb re-ub)]
        [igen (reals-between$ im-lb im-ub)])
    (^[] (make-rectangular (rgen) (igen)))))

(define complexes-polar$
  (case-lambda
    [(mag-lb mag-ub)
     (complexes-polar$ 0 mag-lb mag-ub 0 (* 2 pi))]
    [(origin mag-lb mag-ub)
     (complexes-polar$ origin mag-lb mag-ub 0 (* 2 pi))]
    [(mag-lb mag-ub ang-lb ang-ub)
     (complexes-polar$ 0 mag-lb mag-ub ang-lb ang-ub)]
    [(origin mag-lb mag-ub ang-lb ang-ub)
     (assume (<= mag-lb mag-ub))
     (assume (<= ang-lb ang-ub))
     (let* ([b (square mag-lb)]
            [m (- (square mag-ub) b)]
            [maggen (reals$ m b)]
            [anggen (reals-between$ ang-lb ang-ub)])
       (^[] (+ origin (make-polar (sqrt (maggen)) (anggen)))))]))

;;
;; Nonuniform distributions
;;

;; Normal distribution (continuous - generates real numbers)
;; Box-Muller algorithm
;; NB: We tested Ziggurat method, too (see git repo for the code),
;; only to find out Box-Muller is faster about 12% - presumably
;; the overhead of each ops is larger in Gauche than C/C++, and
;; so the difference of cost of log or sin from the primitive
;; addition/multiplication are negligible.
(define (reals-normal$ :optional (mean 0) (deviation 1))
  (let1 s (random-data-random-source)
    (^[] (let ([r (real-sqrt (* -2 (real-ln (%rand-real s))))]
               [theta (* 2pi (%rand-real s))])
           (+ mean (* deviation r (real-sin theta)))))))

#|
Simple test of gaussian sampling: Generate some data with this:

(with-output-to-file "tmp"
  (^[] (let1 g (normal$ 2 3) (dotimes [i 1000000] (print (g))))))

Then plot with gnuplot:

binwidth=0.025
bin(x,width)=width*floor(x/width)
plot 'tmp' using (bin($1,binwidth)):(1.0) smooth freq with boxes
|#

;; Exponential distribution - continuous
(define (reals-exponential$ m)
  (let1 s (random-data-random-source)
    (^[] (- (* m (log (%rand-real s)))))))

;; Power-law distribution
;;    p(x) = (α-1)/xmin (x/xmin)^{-α}    x >= xmin, α > 1
;; We use method described in Appendix D of
;;   Aaron Clauset, Cosma Rohilla Shalizi, M.E.J.Newman, Power-Law Distributions
;;   in Empirical Data, SIAM Review, 51(4) pp.661--703, Dec. 2009.
;; Preprint https://arxiv.org/abs/0706.1062
(define (reals-power-law$ xmin power)
  (assume (positive? xmin))
  (assume (> power 1))
  (let ([s (random-data-random-source)]
        [exponent (- (/ (- power 1)))])
    (^[] (* xmin (expt (- 1 (%rand-real s)) exponent)))))


;; Draw from geometric distribution, with success probability p.
;; Mean is 1/p, variance is (1-p)/p^2
(define (integers-geometric$ p)
  (let ([c (/ (log (- 1.0 p)))]
        [s (random-data-random-source)])
    (^[] (ceiling->exact (* c (log (%rand-real s)))))))

;; Draw from poisson distribution with mean L, variance L.
;; For small L, we use Knuth's method.  For larger L, we use rejection
;; method by Atkinson, The Computer Generation of Poisson Random Variables,
;; J. of the Royal Statistical Society Series C (Applied Statistics), 28(1),
;; pp29-35, 1979.  The code here is a port by John D Cook's C++ implementation
;; (http://www.johndcook.com/stand_alone_code.html )
(define (integers-poisson$ L)
  (define s (random-data-random-source))
  (if (< L 36)
    (^[] (do ([exp-L (exp (- L))]
              [k 0 (+ k 1)]
              [p 1.0 (* p (%rand-real s))])
             [(<= p exp-L) (- k 1)]))
    (let* ([c (- 0.767 (/ 3.36 L))]
           [beta (/ pi (sqrt (* 3 L)))]
           [alpha (* beta L)]
           [k (- (log c) L (log beta))])
      (rec (loop)
        (let* ([u (%rand-real s)]
               [x (/ (- alpha (log (/ (- 1.0 u) u))) beta)]
               [n (floor->exact (+ x 0.5))])
          (if (< n 0)
            (loop)
            (let* ([v (%rand-real s)]
                   [y (- alpha (* beta x))]
                   [t (+ 1.0 (exp y))]
                   [lhs (+ y (log (/ v (* t t))))]
                   [rhs (+ k (* n (log L)) (- (lgamma (+ n 1))))])
              (if (<= lhs rhs)
                n
                (loop)))))))))

#|
;; This generates histograms of samples
;; e.g.  (gen-hist 1000 (poisson$ 5) (poisson$ 10) (poisson$ 20) (poisson$ 50))
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

;; API.
;; Generate strings that is accepted with a regular expression regex
;;
;; We construct NFA from regex AST.  Each node is a closure that emits
;; character(s), and optionally calls other node.

(define (regular-strings$ regex)
  (assume-type regex <regexp>)
  (let1 nfa (%regex->nfa (regexp-ast regex))
    (^[] (with-output-to-string (cut nfa #f)))))

(define (%regex->nfa ast)
  (define s (random-data-random-source))
  (define (%emit c case-fold)
    (let1 cc (if (and case-fold (zero? (%rand-int 2 s)))
               (cond [(char-lower-case? c) (char-upcase c)]
                       [(char-upper-case? c) (char-downcase c)]
                       [else c])
               c)
      (display cc))
    #t)
  (define (rec a)
    (match a
      [(? char?)     (cut %emit a <>)]
      [(? char-set?) (let1 g (chars$ a) (cut %emit (g) <>))]
      ['any          (let1 g (chars$ char-set:full) (cut %emit (g) <>))]
      [(or 'bol 'wb 'nwb) (^[case-fold] #t)]
      ['eol          (^[case-fold] #f)]
      [('seq . as)   (let1 nodes (map rec as)
                       (^[case-fold] (every (cut <> case-fold) nodes)))]
      [('seq-uncase . as) (let1 nodes (map rec as)
                            (^_ (every (cut <> #t) nodes)))]
      [('seq-case . as)   (let1 nodes (map rec as)
                            (^_ (every (cut <> #f) nodes)))]
      [('alt . as)   (let1 choice-gen (samples$ (map rec as))
                       (^[case-fold] ((choice-gen) case-fold)))]
      [((or 'rep 'rep-min 'rep-while) m n . as)
       (let1 nodes (map rec as)
         (^[case-fold]
           (let loop ([k 1])
             (and (every (cut <> case-fold) nodes)
                  (cond [(< k m) (loop (+ k 1))]
                        [(or (not n) (< k n))
                         (if (zero? (%rand-int 2 s))
                           (loop (+ k 1))
                           #t)]
                        [else #t])))))]
      [('comp . cs)  (let1 g (chars$ (char-set-complement cs))
                       (cut %emit (g) <>))]
      [('cpat _ _ _)
       (error "Conditional regexp can't be used (yet) to generate strings:"
              (regexp-unparse ast))]
      [('once . as)  (let1 nodes (map rec as)
                       (^[case-fold] (every (cut <> case-fold) nodes)))]
      [((or 'assert 'nassert) . _)
       (error "Lookahead/behind assertion can't be used (yet) to \
               generate strings:" (regexp-unparse ast))]
      [((? integer? n) _ . as)
       (let1 nodes (map rec as)
         (^[case-fold] (every (cut <> case-fold) nodes)))]
      [_ (error "Unsupported regexp to generate strings:"
                (regexp-unparse ast))]))

  (rec ast))

;;;
;;; Generator combinators
;;;

;; API
(define (samples-from seq-of-gen)
  (let* ([sources   (coerce-to <vector> seq-of-gen)]
         [num-sources (vector-length sources)])
    (case num-sources
      [(0) eof-object]
      [(1) (~ sources 0)]
      [else
       (let ([num-active num-sources]
             [exhausted (make-vector num-sources #f)]
             [s (random-data-random-source)])
         (^[]
           (let loop ()
             (if (zero? num-active)
               (eof-object)
               (let* ([choice (%rand-int num-sources s)]
                      [r ((vector-ref sources choice))])
                 (if (eof-object? r)
                   (begin
                     (unless (vector-ref exhausted choice)
                       (vector-set! exhausted choice #t)
                       (dec! num-active))
                     (loop))
                   r))))))])))

;; API
;; weight&gens :: ((<real> . <generator>) ...)
(define (weighted-samples-from weight&gens)
  (let* ([tab (make-tree-map)]
         [total (do [(w&g weight&gens (cdr w&g))
                     (cumu 0 (+ cumu (caar w&g)))]
                    [(null? w&g) cumu]
                  (tree-map-put! tab cumu (cdar w&g)))]
         [s (random-data-random-source)])
    (^[] (receive (_ gen) (tree-map-floor tab (* (%rand-real0 s) total))
           (gen)))))

;; API
(define (pairs-of car-gen cdr-gen) (^[] (cons (car-gen) (cdr-gen))))

;; API
(define (tuples-of . gens) (^[] (map (^g (g)) gens)))

;; We accept constant integer or generator
(define-syntax %with-sizer
  (syntax-rules ()
    [(_ sizer . body)
     (let1 sizer (if (integer? sizer) (^[] sizer) sizer)
       . body)]))

(define default-sizer (make-parameter (integers-poisson$ 4)))

;; API
(define lists-of
  (case-lambda
    [(item-gen) (lists-of (default-sizer) item-gen)]
    [(sizer item-gen)
     (%with-sizer sizer (^[] (list-tabulate (sizer) (^_ (item-gen)))))]))

;; API
(define vectors-of
  (case-lambda
    [(item-gen) (vectors-of (default-sizer) item-gen)]
    [(sizer item-gen)
     (%with-sizer sizer
       (^[] (let1 len (sizer)
              (rlet1 vec (make-vector len)
                (do-ec (: i len) (vector-set! vec i (item-gen)))))))]))

;; API
(define strings-of
  (case-lambda
    [()         (strings-of (default-sizer) (chars$))]
    [(item-gen) (strings-of (default-sizer) item-gen)]
    [(sizer item-gen)
     (%with-sizer sizer
       (^[] (let1 len (sizer)
              (with-output-to-string
                (^[] (do-ec (: i len) (display (item-gen))))))))]))

;; API
(define sequences-of
  (case-lambda
    [(class item-gen) (sequences-of class (default-sizer) item-gen)]
    [(class sizer item-gen)
     (%with-sizer sizer
       (^[] (let1 len (sizer)
              (with-builder (class add! get :size len)
                (dotimes [len] (add! (item-gen)))
                (get)))))]))

;; API
(define (permutations-of seq)
  (let1 s (random-data-random-source)
    (cut shuffle seq s)))

;; API
(define (combinations-of len seq)
  (let1 indices (list->vector (iota (size-of seq)))
    (^[] (let1 ix (shuffle indices)
           (coerce-to (class-of seq)
                      (list-ec (: i len) (ref seq (vector-ref ix i))))))))
