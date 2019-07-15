;;;
;;; math/prime.scm - utilities related to prime numbers
;;;
;;;   Copyright (c) 2013-2019  Shiro Kawai  <shiro@acm.org>
;;;   Copyright (c) 2013  @cddddr
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

(define-module math.prime
  (use srfi-27)
  (use srfi-42)
  (use gauche.uvector)
  (use gauche.generator)
  (use gauche.sequence)
  (use gauche.threads)
  (use data.sparse)
  (use util.match)
  (export primes *primes* reset-primes
          small-prime? *small-prime-bound*
          miller-rabin-prime? bpsw-prime?
          naive-factorize mc-factorize
          jacobi totient))
(select-module math.prime)

;;;
;;; Infinite sequence of prime numbers
;;;

;; This section of code is based on the segment sieve prime number generator
;; written by @cddddr.  Optimized by SK.  As of 0.9.4_pre2, it can generate
;; first 10^7 primes in 14sec on 2.4GHz Core2 machine.

(define (take-first-term a0 d lower-bound)
  (if (<= lower-bound a0)
      a0
      (+ a0 (* d (quotient (- lower-bound a0 (- d) 1) d)))))
(define (->odd x)  (if (odd? x) x (+ x 1)))
(define (->even x) (if (even? x) x (+ x 1)))
(define-inline (integer->index x) (ash x -1))

(define-constant *segment-size* (->even #e5e4))
(define-constant *first-segment-start*
  (->odd (ceiling->exact (expt *segment-size* 0.6))))
(define-constant *sieve-vec-size* (/ *segment-size* 2))
(define-constant *sieve-filler* '#u8(0))

;; sieve is a bytevector, representing a range of odd numbers.
;; given odd number N in the range [start, end] (where start and end
;; are both odd), (vector-ref bytevec (/ (- N start) 2)) is 0 if N
;; is composite, 1 if not.
(define (segment-sieve! bytevec start primes)
  (let ([root (floor->exact (sqrt (+ start *segment-size* -1)))]
        [start-index (integer->index start)])
    (let sieve! ([ps (cdr primes)])
      (match-let1 (p . qs) ps
        (when (<= p root)
          (let1 i (- (take-first-term (integer->index (* p p)) p start-index)
                     start-index)
            (u8vector-multi-copy! bytevec i p *sieve-filler*))
          (sieve! qs))))))

(define (bytevec->generator bytevec start)
  (let1 i 0
    (^[] (let loop ([j i])
           (cond [(>= j *sieve-vec-size*) (set! i j) (eof-object)]
                 [(not (zero? (u8vector-ref bytevec j)))
                  (set! i (+ j 1))
                  (+ (* j 2) start)]
                 [else (loop (+ j 1))])))))

(define (segment-prime-generator start primes)
  (let1 bytevec (make-u8vector *sieve-vec-size* 1)
    (segment-sieve! bytevec start primes)
    (bytevec->generator bytevec start)))

;; Head of prime sequence.  We don't make it a generator,
;; for we only need to calculate them once and it won't
;; take long.  We just wrap it with delay so that it won't
;; tax loading time.
(define *small-primes*
  (delay (list-ec (:range n 2 *first-segment-start*)
                  (if (every?-ec (:range j 2 (+ 1 (floor->exact (sqrt n))))
                                 (< 0 (mod n j))))
                  n)))

;; API
(define (primes)
  (define start *first-segment-start*)
  (define gen (list->generator (force *small-primes*)))
  (define (gen-primes)
    (let loop ([v (gen)])
      (if (eof-object? v)
        (begin
          (set! gen (segment-prime-generator start prime-lseq))
          (inc! start *segment-size*)
          (loop (gen)))
        v)))
  (define prime-lseq (generator->lseq gen-primes))
  prime-lseq)

;; API
(define *primes* (primes))

;; API
(define (reset-primes) (set! *primes* (primes)) (undefined))

;;;
;;; Primarity test
;;;

;; We provide both deterministic and probabilistic method.

;; single Miller-Rabin test.
;; n is the number to be tested, a is the chosen base.
;; returns #f if n is composite.
(define (miller-rabin-test a n)
  (let* ([n-1 (- n 1)]
         [s (twos-exponent-factor n-1)]
         [d (ash n-1 (- s))]
         [a^d (expt-mod a d n)])
    (or (= a^d 1)
        (let loop ([i 0] [a^d a^d])
          (cond [(= i s) #f]
                [(= a^d n-1) #t]
                [else (loop (+ i 1) (expt-mod a^d 2 n))])))))

;; For small integers, determinisitc Miller-Rabin is known.
;; Selfridge&Wagstaff  doi:10.2307/2006210
;; Jaeschke doi:10.2307/2153262
(define *deterministic-witnesses*
  ;; ((bound a ...) ...)
  ;; If the number to test is less than bound, we only need to test with a ...
  '((1373653 2 3)
    (9080191 31 73)
    (4759123141 2 7 61)
    (2152302898747 2 3 5 7 11)
    (3474749660383 2 3 5 7 11 13)
    (341550071728321 2 3 5 7 11 13 17)))

(define *small-prime-bound*
  (car (last *deterministic-witnesses*)))

(define (deterministic-miller-rabin n witnesses)
  (every?-ec (: a witnesses) (miller-rabin-test a n)))

;; If n is below *small-prime-bound*, returns deterministic
;; answer.  If n is over, always return #f.
(define (small-prime? n)
  (if (<= n 100)
    (boolean (memv n (take *primes* 25))) ; we have 25 primes below 100.
    (and-let* ([ (odd? n) ]
               [p (find (^p (< n (car p))) *deterministic-witnesses*)])
      (deterministic-miller-rabin n (cdr p)))))

(define *miller-rabin-random-source*
  (rlet1 s (make-random-source)
    (random-source-pseudo-randomize! s 1 1)))

(define default-miller-rabin-random-integer
  (random-source-make-integers *miller-rabin-random-source*))

;; API
(define (miller-rabin-prime? n :key (num-tests 7)
                             (random-integer default-miller-rabin-random-integer))
  (unless (and (exact-integer? n) (> n 1))
    (error "exact positive integer greater than 1 is expected, but got:" n))
  (and (odd? n) ; filter out the trivial case
       (if (< n *small-prime-bound*)
         (small-prime? n)
         (let ([bound (- (integer-length n) 2)]
               [tested '()]) ;; tested primes
           (define (rand)
             (let1 r (+ 2 (random-integer bound))
               (if (memv r tested)
                 (rand)
                 (begin (push! tested r) r))))
           (every?-ec (: k num-tests) (miller-rabin-test (rand) n))))))

;; Jacobi symbol calculation, used in bpsw-prime?
;;  http://en.wikipedia.org/wiki/Jacobi_symbol
;; There exists better algorithms, but let's see how the straightforward
;; one goes.

;; API
(define (jacobi a n) ; n is odd
  (define (J a n s)
    (cond [(= n 1) (* s 1)]
          [(= a 0) 0]
          [(odd? a) 
           (if (and (= (logand a 3) 3) (= (logand n 3) 3))
             (J (modulo n a) a (- s))
             (J (modulo n a) a s))]
          [else
           (let1 k (- (twos-exponent-factor a))
             (if (and (memv (logand n 7) '(3 5)) (odd? k))
               (J (ash a k) n (- s))
               (J (ash a k) n s)))]))
  (when (or (even? n) (< n 1))
    (error "n must be positive odd number, but got" n))
  (if (< a 0)
    (J (- a) n (if (= (logand n 3) 1) 1 -1))
    (J a n 1)))

;; Baillie-PSW primality test
;;  http://www.trnicely.net/misc/bpsw.html
;; It is known that this correctly identifies primes/composites below 10^17,
;; and it is very likely correct below 2^64 empirically.

;; Find first element D in the sequence (-1)^n (2n+1) where n >= 2
;; and JacobiSymbol(D,N) = -1.
;; D is expected to be small if N is not a perfect square.
(define (bpsw-find-D n)
  (let loop ([d 5] [s 1])
    (if (= (jacobi (* s d) n) -1)
      (* s d)
      (loop (+ d 2) (- s)))))

;; Strong Lucas-Selfridge test
(define (lucas-selfridge-test n P Q)

  ;; calculate U_d, V_d and Q^d mod n.  (d is such that n = d * 2^s)
  (define (calculate-UV n P Q D d)
    (let1 dsize (integer-length d)
      (let loop ([U 1] [V P] [U_2^m 1] [V_2^m P] [Q_m Q] [Q^d Q] [bit 1])
        (if (= bit dsize)
          (values U V Q^d)
          (let ([U_2^m_1 (modulo (* U_2^m V_2^m) n)]
                [V_2^m_1 (modulo (- (* V_2^m V_2^m) (* 2 Q_m)) n)]
                [Q_m_1   (modulo (* Q_m Q_m) n)])
            (if (not (logbit? bit d))
              (loop U V U_2^m_1 V_2^m_1 Q_m_1 Q^d (+ bit 1))
              (let ([U (half-modn (+ (* U_2^m_1 V) (* U V_2^m_1)))]
                    [V (half-modn (+ (* V_2^m_1 V) (* D U U_2^m_1)))]
                    [Q^d (modulo (* Q^d Q_m_1) n)])
                (loop U V U_2^m_1 V_2^m_1 Q_m_1 Q^d (+ bit 1)))))))))

  ;; divide by 2 modulo n; we know n is odd
  (define (half-modn x)
    (if (odd? x)
      (modulo (ash (+ x n) -1) n)
      (modulo (ash x -1) n)))
  
  (let* ([n+1 (+ n 1)]
         [s   (twos-exponent-factor n+1)]
         [d   (ash n+1 (- s))]
         [D   (bpsw-find-D n)]
         [P   1]
         [Q   (/ (- 1 D) 4)])
    (receive (U V Q^d) (calculate-UV n P Q D d)
      (or (zero? U)
          (zero? V)
          (let loop ([r 1] [V V] [Q^d Q^d])
            (if (>= r s)
              #f
              (let1 V (modulo (- (* V V) (* 2 Q^d)) n)
                (or (zero? V)
                    (loop (+ r 1) V (modulo (* Q^d Q^d) n))))))))))

;; API
(define (bpsw-prime? n)
  (cond [(< n 2) #f]
        [(= n 2) #t]
        [(even? n) #f]
        [else
         (let1 fs (naive-factorize n 1000)
           (cond
            [(not (null? (cdr fs))) #f] ; definitely composite
            [(< n 1000000) #t] ; we know it's prime
            [(not (miller-rabin-test 2 n)) #f]
            [(zero? (values-ref (exact-integer-sqrt n) 1)) #f] ;perfect square
            [else (lucas-selfridge-test n 1 (/ (- 1 (bpsw-find-D n)) 4))]))]))

;;;
;;; Factorization
;;;

(define-constant *small-factorize-table-limit* 50000)

(define-constant *small-factorize-table-index-limit*
  (/ (- *small-factorize-table-limit* 1) 2))

;; suitable for small n with memoization.  n is odd number.
;; mem-vec[k] remembers factorization of k*2+1.
;;
(define naive-factorize-1
  (let1 mem-vec (atom (make-sparse-vector))
    (define (->index n) (/ (- n 1) 2)) ; n must be odd
    (define (memo! i val)
      (when (< i *small-factorize-table-index-limit*)
        (atomic mem-vec (cut sparse-vector-set! <> i val)))
      val)
    (^[n divisor-limit]
      (let try [(n n) (ps *primes*)]
        ;; try to divide n with given primes.
        (let1 i (->index n)
          (or (and (< i *small-factorize-table-index-limit*)
                   (atomic mem-vec (cut sparse-vector-ref <> i #f)))
              (and (small-prime? n)
                   (memo! i `(,n)))
              (let loop ([ps ps])
                (let* ([p (car ps)] [p^2 (* p p)])
                  (cond [(> p divisor-limit) `(,n)] ; n can be composite, so no memo
                        [(< n p^2) (memo! i `(,n))]
                        [(= n p^2) (memo! i `(,p ,p))]
                        [else
                         (receive (q r) (quotient&remainder n p)
                           (if (zero? r)
                             (memo! i (cons p (try q ps)))
                             (loop (cdr ps))))])))))))))

;; API
(define (naive-factorize n :optional (divisor-limit +inf.0))
  (if (<= n 3)
    `(,n)
    (let1 k (twos-exponent-factor n)
      (if (= k 0)
        (naive-factorize-1 n divisor-limit)
        ;; avoid simple recursion to naive-factorize for every factor of 2
        ;; to save intermediate results generation (effective when n is bignum).
        (let loop ([i 0] [r '()])
          (if (= i k)
            (let1 m (ash n (- k))
              (reverse r (if (= m 1)
                           '()
                           (naive-factorize (ash n (- k)) divisor-limit))))
            (loop (+ i 1) (cons 2 r))))))))

;; Monte Carlo factorization
;;  R. P. Brent, An improved Monte Carlo factorization algorithm, BIT 20 (1980), 176-184.
;;  http://maths-people.anu.edu.au/~brent/pub/pub051.html

;; Single trial of factorizing n using x0 as the initial seed.
;; If this returns a number, it's a nontrivial divisor of n.
;; If this returns #f, you need to retry with different x0.
(define (mc-find-divisor-1 n x0)
  (define (f x) (modulo (+ (* x x) 3) n))
  (define (f^ r x) ; apply f on x for r times, e.g (f (f x)) for r=2
    (let loop ([r r] [x x])
      (if (= r 1) (f x) (loop (- r 1) (f x)))))
  ;; the step value m: in the big-step loop, we only compute gcd for
  ;; every m-th value of the series x_i.
  (define m (ceiling->exact (log n 2)))
  ;; 'big-step' loop
  (define (big-step x y q r k)
    (do ([i (min m (- r k)) (- i 1)]
         [y y (f y)]
         [q q (modulo (* q (abs (- x y))) n)])
        [(= i 0) (values (gcd q n) q y)]))
  (define (big-stride x y q r)
    (let loop ([ys y] [k 0])
      (receive (G q y) (big-step x ys q r k)
        (cond [(or (>= k (- r m)) (> G 1)) (values G ys y q)]
              [else (loop y (+ k m))]))))
  (define (small-stride x ys)
    (let loop ([ys (f ys)])
      (let1 G (gcd (abs (- x ys)) n)
        (if (> G 1) G (loop (f ys))))))

  ;; The main body
  (let loop ([y x0] [r 1] [q 1])
    (let ([x y] [y (f^ r y)])
      (receive (G ys y q) (big-stride x y q r)
        (if (> G 1)
          (if (< G n)
            G
            (let1 G (small-stride x ys)
              (and (< G n) G)))           ; if (= G N), we failed.
          (loop y (* r 2) q))))))

;; Try MC factorization.  Returns (divisor . quotient).
;; Note: This will loop forever if N is a prime.  The caller should
;; exclude primes.  Unfortunately, we don't have a deterministic primality
;; test > 2^64 yet.  
(define (mc-try-factorize n)
  (let loop ()
    (if-let1 d (mc-find-divisor-1 n (random-integer n))
      (cons d (quotient n d))
      (loop))))

;; API
(define (mc-factorize n)
  ;; Break up n.  We first exclude primes if possible.
  ;; The worst case scenario is that n contains a factor
  ;; greater than 2^64---in which case we'll take forever.
  (define (smash n)
    (if (definite-prime? n)
      `(,n)
      (let1 d (mc-try-factorize n)
        (append (smash (car d)) (smash (cdr d))))))

  (define (definite-prime? n)
    (cond [(< n *small-prime-bound*) (small-prime? n)]
          [(< n 18446744073709551616) (bpsw-prime? n)]; (expt 2 64)
          [else #f]))

  (define try-prime-limit 1000)

  ;; We exclude trivial factors first.
  (let* ([ps (naive-factorize n try-prime-limit)]
         [n (last ps)])
    (if (< n (* try-prime-limit try-prime-limit))
      ps ;; we're completely done
      (let1 nf (smash n) ; n may be composite, so try to break it more.
        (if (null? (cdr nf))
          ps  ; n is unbreakable, so the original factorization was fine.
          (sort (append nf (drop-right ps 1))))))))

;;;
;;; Fun stuff
;;;

(define (totient n)
  (if (<= n 2)
    1
    (fold (^[pk phi] (* phi (expt (car pk) (- (length pk) 1)) (- (car pk) 1)))
          1 (group-sequence (mc-factorize n)))))

;; Wishlist
;;   deterministic prime?  (maybe using AKS primality test)
;;   more sophisticated integer factorization
