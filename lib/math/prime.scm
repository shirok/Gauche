;;;
;;; math/prime.scm - utilities related to prime numbers
;;;
;;;   Copyright (c) 2013  Shiro Kawai  <shiro@acm.org>
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
  (use util.sparse)
  (use util.match)
  (export primes *primes* reset-primes
          small-prime? *small-prime-bound*
          miller-rabin-prime?
          naive-factorize mc-factorize))
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
         [s (- (integer-length (logxor n-1 (- n-1 1))) 1)]
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
(define (miller-rabin-prime? n :key (num-tests 20)
                             (random-integer default-miller-rabin-random-integer))
  (unless (and (exact-integer? n) (> n 1))
    (error "exact positive integer greater than 1 is expected, but got:" n))
  (and (odd? n) ; filter out the trivial case
       (if (< n *small-prime-bound*)
         (small-prime? n)
         (let1 bound (- (integer-length n) 1)
           (every?-ec (: k num-tests)
                      (miller-rabin-test (+ 1 (random-integer bound)) n))))))

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
  (let1 mem-vec (make-sparse-vector)
    (define (->index n) (/ (- n 1) 2)) ; n must be odd
    (define (memo! i val)
      (when (< i *small-factorize-table-index-limit*)
        (sparse-vector-set! mem-vec i val))
      val)
    (^[n divisor-limit]
      (let try [(n n) (ps *primes*)]
        ;; try to divide n with given primes.
        (let1 i (->index n)
          (or (and (< i *small-factorize-table-index-limit*)
                   (sparse-vector-ref mem-vec i #f))
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
  (cond [(<= n 3) `(,n)]
        [(even? n) (cons 2 (naive-factorize (/ n 2) divisor-limit))]
        [else (naive-factorize-1 n divisor-limit)]))

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
  (define m (ceiling->exact (log n)))
  ;; 'big-step' loop
  (define (big-step x y q r k)
    (do ([i (min m (- r k)) (- i 1)]
         [y y (f y)]
         [q q (modulo (* q (abs (- x y))) n)])
        [(= i 0) (values (gcd q n) q)]))
  (define (big-stride x y q r)
    (let1 y (f^ r y)
      (let loop ([k 0])
        (receive (G q) (big-step x y q r k)
          (cond [(> G 1) (values G x y)]
                [(>= k (- r m))
                 (if (< r 1000) ;; if r gets rather big, give up and try different x0.
                   (big-stride y y q (* r 2))
                   (values #f #f #f))]
                [else (loop (+ k m))])))))
  (define (small-stride x y)
    (let loop ([y (f y)])
      (let1 G (gcd (abs (- x y)) n)
        (if (> G 1) G (loop (f y))))))

  ;; The main body
  (receive (G x y) (big-stride x0 x0 1 1)
    (and G
         (if (< G n)
           G
           (let1 G (small-stride x y)
             (and (< G n) G))))))  ; if (= G N), we failed.

;; Try MC factorization up to num-tries pass.  If we find any
;; divisor, returns (divisor . quotient).  Otherwise return #f.
(define (mc-try-factorize n num-tries)
  (let loop ([i 0] [x0 (random-integer n)])
    (and (< i num-tries)
         (or (and-let* ([d (mc-find-divisor-1 n x0)])
               (cons d (quotient n d)))
             (loop (+ i 1) (random-integer n))))))

;; API
(define (mc-factorize n :optional (num-tries +inf.0))
  ;; Break up n.  We first exclude primes if possible.
  ;; The worst case scenario is that n contains a factor
  ;; greater than *small-prime-bound*---in which case
  ;; we'll take forever.   Once we have general deterministic
  ;; primality test, however, we can significantly speed up such case.
  (define (smash n)
    (if (small-prime? n)
      `(,n)
      (let1 d (mc-try-factorize n num-tries)
        (if d
          (append (smash (car d)) (smash (cdr d)))
          `(,n 1))))) ;; indicating that n may be composite
  
  ;; We exclude trivial factors first.
  (let* ([ps (naive-factorize n 1000)]
         [n (last ps)]   ; n may be composite.
         [nf (smash n)])
    (if (null? (cdr nf))
      ps  ; n is unbreakable, so the original factorization was fine.
      (sort (append nf (drop-right ps 1))))))

;; Wishlist
;;   deterministic prime?  (maybe using AKS primality test)
;;   totient (used in AKS algorithm)
;;   more sophisticated integer factorization
