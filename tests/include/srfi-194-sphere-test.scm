; Take REPS samples from unit sphere, verify random distribution.
;
; This test checks that:
; * Every sample has unit length, within numerical tolerance.
; * The REPS samples are uniformly distributed.
; * Rotations of the REPS samples are uniformly distributed.
(define (test-sphere sphereg dim-sizes REPS rotate?)
  (define random-int (random-source-make-integers (current-random-source)))
  (define random-real (random-source-make-reals (current-random-source)))
  (define N (- (vector-length dim-sizes) 1))

  ; Fix list of samples
  (define samples
    (generator->list (gtake sphereg REPS)))

  (define (l2-norm VEC)
    (sqrt (vector-fold
            (lambda (sum x l) (+ sum (/ (* x x)
                                        (* l l))))
            0
            VEC
            dim-sizes)))

  ; Rotate the j'th amnd k'th coordinates of a vector VEC
  ; by cosine co and sine si
  (define (pair-rot VEC j k co si)
    (define oj (vector-ref VEC j))
    (define ok (vector-ref VEC k))
    (define nj (+ (* co oj) (* si ok)))
    (define nk (+ (* (- si) oj) (* co ok)))
    (list->vector
      (map (lambda (index)
             (cond
               ((= index j) nj)
               ((= index k) nk)
               (else (vector-ref VEC index))))
           (iota (vector-length VEC)))))

  ; Apply a random rotation to a collection of vectors
  (define how-many-rots
    (if (< 10 N) 10 N))

  (define (arb-rot VEC-LIST)
    (define j (random-int N))
    (define k (+ j 1 (random-int (- N j))))
    (define theta (* 3.14 (random-real)))
    (define co (cos theta))
    (define si (sin theta))
    (define rvl
      (map (lambda (vec)
             (pair-rot vec j k co si))
           VEC-LIST))
    (if (not (= 0 (random-int how-many-rots)))
        (arb-rot rvl)
        rvl))

  ; Expect a vector approaching zero. That is, each individual
  ; coordinate should be uniformly randomly distributed in the
  ; interval [-1,1]. The sum of REPS samples of these should
  ; converge to zero, within pi/2 sqrt(REPS).
  (define (converge-to-zero samples)
    (fold (lambda (acc sample) (vector-map + sample acc))
          (make-vector REPS 0.0)
          samples))

  (define (should-be-zero samples)
    (l2-norm (converge-to-zero samples)))

  (define (norm-should-be-zero samples)
    (/ (should-be-zero samples) (* 1.57 (sqrt REPS))))

  (define (check-zero samples)
    (define zz (norm-should-be-zero samples))
    (test-assert (< zz 1)))

  ; maximum allowed tolerance for radius deviation
  (define EPS (* 2e-15 (sqrt N)))

  ; Each individual sphere radius should be 1.0 to within float
  ; tolerance.
  (for-each
    (lambda (SAMP)
      (test-approximate 1.0 (l2-norm SAMP) EPS))
    samples)

  ; The distribution should be zero
  (check-zero samples)

  ; Rotate wildly. Should still be uniform.
  (when rotate?
    (for-each
      (lambda (junk) (check-zero (arb-rot samples)))
      (make-list 12))))

(define (test-ball ballg dim-sizes)
  (define (l2-norm VEC)
    (sqrt (vector-fold
            (lambda (sum x l) (+ sum (/ (* x x)
                                        (* l l))))
            0
            VEC
            dim-sizes)))

  (define (test-ball-generates-on-radius radius err)
    (test-assert
      (generator-any
        (lambda (vec)
          (define n (l2-norm vec))
          (and (> n (- radius err))
               (< n (+ radius err))))
        (gtake ballg 10000))))

  (define (test-ball-avg-zero N)
    (define vec-sum
      (generator-fold
        (lambda (vec acc)
          (vector-map + vec acc))
        (make-vector (vector-length dim-sizes) 0.0)
        (gtake ballg N)))
    (define avg-vec
      (vector-map
        (lambda (e)
          (/ e N))
        vec-sum))
    (define n (l2-norm avg-vec))
    (test-assert (< n 1)))

  (test-ball-generates-on-radius 0.0 0.1)
  (test-ball-generates-on-radius 0.5 0.1)
  (test-ball-generates-on-radius 1.0 0.1)

  (test-ball-avg-zero 5000))
