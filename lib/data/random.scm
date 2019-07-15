;;;
;;;  data.random - Random data genarators
;;;
;;;   Copyright (c) 2013-2019  Shiro Kawai  <shiro@acm.org>
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
  (use util.match)
  (use math.const)
  (use math.mt-random)
  (use gauche.uvector)
  (use gauche.generator)
  (use gauche.parameter)
  (use gauche.sequence)
  
  (export make-random-data-state random-data-seed with-random-data-seed

          integers$ integers-between$ fixnums chars$ samples$ booleans
          int8s uint8s int16s uint16s int32s uint32s int64s uint64s
          reals$ reals-between$ reals-normal$ reals-exponential$
          integers-geometric$ integers-poisson$
          regular-strings$

          default-sizer
          samples-from pairs-of tuples-of lists-of vectors-of strings-of
          sequences-of
          permutations-of combinations-of weighted-samples-from
          ))
(select-module data.random)

;; Random state management
;; Random state is kept in %random-source parameter as (seed . #<mt>)
;; We use mt-random directly instead of srfi-27, for we need
;; a portable way to save and restore the random state.
;; Srfi-27's random state isn't guaranteed to be printable.

;; API
(define (make-random-data-state seed)
  (cons seed (make <mersenne-twister> :seed seed)))

(define %random-data-state
  (make-parameter (make-random-data-state 42)))

;; API
(define random-data-seed
  (getter-with-setter
   (^[] (car (%random-data-state)))
   (^[seed] (%random-data-state (make-random-data-state seed)))))

;; API
(define (with-random-data-seed seed thunk)
  ;; create st here so that reentering thunk retains the state.
  (let1 st (make-random-data-state seed)
    (parameterize ([%random-data-state st])
      (thunk))))

(define (%rand-int n) (mt-random-integer (cdr (%random-data-state)) n))
(define (%rand-real0) (mt-random-real0 (cdr (%random-data-state))))
(define (%rand-real)  (mt-random-real (cdr (%random-data-state))))

;;;
;;; Primitive generators
;;;

;;
;; Uniform distribution
;;

;; API.  Generate integers uniformly in [start, start+size)
(define (integers$ size :optional (start 0))
  (^[] (+ (%rand-int size) start)))

;; API.  Generate integers uniformly in [lb ub]
;; (We avoid 'integer-range', for 'range' API takes exclusive upper bound.)
(define (integers-between$ lb ub)
  (let1 range (- ub lb -1)
    (^[] (+ (%rand-int range) lb))))

;; API.
(define fixnums (integers-between$ (least-fixnum) (greatest-fixnum)))
(define int8s   (integers$ 256 -128))
(define uint8s  (integers$ 256))
(define int16s  (integers$ 65536 -32768))
(define uint16s (integers$ 65536))
(define int32s  (integers$ (expt 2 32) (- (expt 2 31))))
(define uint32s (integers$ (expt 2 32)))
(define int64s  (integers$ (expt 2 64) (- (expt 2 64))))
(define uint64s (integers$ (expt 2 64)))

;; API.
(define booleans (^[] (zero? (%rand-int 2))))

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
  (let1 ub (+ size start)
    (^[] (clamp (+ start (* size (%rand-real0))) start ub))))
(define (reals-between$ lb ub)
  (let1 range (+ lb ub)
    (^[] (clamp (- (* range (%rand-real0)) lb) lb ub))))

;; rational
;; complex

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
  (^[] (let ([r (%sqrt (* -2 (%log (%rand-real))))]
             [theta (* 2pi (%rand-real))])
         (+ mean (* deviation r (%sin theta))))))

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
  (^[] (- (* m (log (%rand-real))))))

;; Draw from geometric distribution, with success probability p.
;; Mean is 1/p, variance is (1-p)/p^2
(define (integers-geometric$ p)
  (let1 c (/ (log (- 1.0 p)))
    (^[] (ceiling->exact (* c (log (%rand-real)))))))

;; Draw from poisson distribution with mean L, variance L.
;; For small L, we use Knuth's method.  For larger L, we use rejection
;; method by Atkinson, The Computer Generation of Poisson Random Variables,
;; J. of the Royal Statistical Society Series C (Applied Statistics), 28(1),
;; pp29-35, 1979.  The code here is a port by John D Cook's C++ implementation
;; (http://www.johndcook.com/stand_alone_code.html )
(define (integers-poisson$ L)
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
  (define (%emit c case-fold)
    (let1 cc (if (and case-fold (zero? (%rand-int 2)))
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
                         (if (zero? (%rand-int 2))
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
  (let1 s (size-of seq-of-gen)
    (^[] ((~ seq-of-gen (%rand-int s))))))

;; API
;; weight&gens :: ((<real> . <generator>) ...)
(define (weighted-samples-from weight&gens)
  (let* ([tab (make-tree-map)]
         [total (do [(w&g weight&gens (cdr w&g))
                     (cumu 0 (+ cumu (caar w&g)))]
                    [(null? w&g) cumu]
                  (tree-map-put! tab cumu (cdar w&g)))])
    (^[] (receive (_ gen) (tree-map-floor tab (* (%rand-real0) total))
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
  (^[] (shuffle seq (cdr (%random-data-state)))))

;; API
(define (combinations-of len seq)
  (let1 indices (list->vector (iota (size-of seq)))
    (^[] (let1 ix (shuffle indices)
           (coerce-to (class-of seq)
                      (list-ec (: i len) (ref seq (vector-ref ix i))))))))

