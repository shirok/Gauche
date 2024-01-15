;;;
;;; Grafted from srfi-194 reference implementation
;;; Original code by Arvydas Silankas
;;;

(define-module srfi.194.sphere
  (use scheme.vector)
  (use srfi.194)
  (export make-sphere-generator
          make-ellipsoid-generator
          make-ball-generator)
  )
(select-module srfi.194.sphere)

;
; sphere.scm
; Uniform distributions on a sphere, and a ball.
; Submitted for inclusion in srfi-194
;
; Algorithm based on BoxMeuller as described in
; http://extremelearning.com.au/how-to-generate-uniformly-random-points-on-n-spheres-and-n-balls/
;

; make-sphere-generator N - return a generator of points uniformly
; distributed on an N-dimensional sphere.
; This implements the BoxMeuller algorithm, that is, of normalizing
; N+1 Gaussian random variables.
(define (make-sphere-generator arg)
  (cond
    ((integer? arg) (make-ellipsoid-generator* (make-vector (+ 1 arg) 1.0)))
    (else (error "expected argument to be an integer dimension"))))

(define (make-ellipsoid-generator arg)
  (cond
    ((vector? arg) (make-ellipsoid-generator* arg))
    (else (error "expected argument to be a vector of axis lengths"))))

; -----------------------------------------------
; Generator of points uniformly distributed on an N-dimensional ellipsoid.
;
; The `axes` should be a vector of floats, specifying the axes of the
; ellipsoid. The algorithm used is an accept/reject sampling algo,
; wherein the acceptance rate is proportional to the measure of a
; surface element on the ellipsoid. The measure is straight-forward to
; arrive at, and the 3D case is described by `mercio` in detail at
; https://math.stackexchange.com/questions/973101/how-to-generate-points-uniformly-distributed-on-the-surface-of-an-ellipsoid
;
; Note that sampling means that performance goes as
; O(B/A x C/A x D/A x ...) where `A` is the shorest axis,
; and `B`, `C`, `D`, ... are the other axes. Maximum performance
; achieved on spheres.
;
(define (make-ellipsoid-generator* axes)

  ; A vector of normal gaussian generators
  (define gaussg-vec
    (make-vector (vector-length axes) (make-normal-generator 0 1)))

  ; Banach l2-norm of a vector
  (define (l2-norm VEC)
    (sqrt (vector-fold
            (lambda (sum x) (+ sum (* x x)))
            0
            VEC)))

  ; Generate one point on a sphere
  (define (sph)
    ; Sample a point
    (define point
      (vector-map (lambda (gaussg) (gaussg)) gaussg-vec))
    ; Project it to the unit sphere (make it unit length)
    (define norm (/ 1.0 (l2-norm point)))
    (vector-map (lambda (x) (* x norm)) point))

  ; Distance from origin to the surface of the
  ; ellipsoid along direction RAY.
  (define (ellipsoid-dist RAY)
    (sqrt (vector-fold
            (lambda (sum x a) (+ sum (/ (* x x) (* a a))))
            0 RAY axes)))

  ; Find the shortest axis.
  (define minor
    (vector-fold
        (lambda (mino l) (if (< l mino) l mino))
        1e308 axes))

  ; Uniform generator [0,1)
  (define uni (make-random-real-generator 0.0 1.0))

  ; Return #t if the POINT can be kept; else must resample.
  (define (keep POINT)
    (< (uni) (* minor (ellipsoid-dist POINT))))

  ; Sample until a good point is found. The returned sample is a
  ; vector of unit length (we already normed up above).
  (define (sample)
    (define vect (sph))
    (if (keep vect) vect (sample)))

  (lambda ()
  ; Find a good point, and rescale to ellipsoid.
    (vector-map
         (lambda (x a) (* x a)) (sample) axes))
)

; -----------------------------------------------
; make-ball-generator N - return a generator of points uniformly
; distributed inside an N-dimensional ball.
; This implements the Harman-Lacko-Voelker Dropped Coordinate method.
(define (make-ball-generator arg)
  (define dim-sizes
    (cond
      ((integer? arg) (make-vector (+ 2 arg) 1.0))
      ((vector? arg) (vector-append arg (vector 1.0 1.0)))
      (else (error "expected argument to either be a number (dimension), or vector (axis length for the dimensions)"))))
  (define N (- (vector-length dim-sizes) 2))
  (define sphereg (make-sphere-generator (+ N 2)))
  ; Create a vector of N+2 values, and drop the last two.
  ; (The sphere already added one, so we only add one more)
  (lambda ()
    (vector-map
      (lambda (el dim-size _) (* el dim-size))
      (sphereg)
      dim-sizes
      (make-vector N #f))))
