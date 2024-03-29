;;;
;;; aobench - Ambient Occlusion Benchmark
;;; Based on the code by Syoyo Fujita
;;; https://github.com/syoyo/aobench
;;;

;; More discussion about this code is here:
;; http://practical-scheme.net/wiliki/wiliki.cgi?Gauche%3AAOBench

;; Copyright 2009-2014, Syoyo Fujita
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;; 1. Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright
;; notice, this list of conditions and the following disclaimer in the
;; documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
;; BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
;; FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
;; OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
;; TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
;; USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
;; DAMAGE.

;; Run this script as
;;  $ gosh ./aobench.scm
;; The output is written to ao.ppm.

;; This script uses multiple threads if multiple cores are available.
;; To force single thread execution, set GAUCHE_AVAILABLE_PROCESSORS
;; environment variable to 1.

(use gauche.uvector)
(use gauche.record)
(use gauche.threads)
(use control.thread-pool)
(use math.const)
(use srfi.27)

(define-constant WIDTH       256)
(define-constant HEIGHT      256)
(define-constant NSUBSAMPLES   2)
(define-constant NAO_SAMPLES   8)

;; Eventually these should be written by define-record-type with pseudo-rtd.
;; Currently, the accessors generated by define-record-type are not inlined,
;; so we define these manually for speed.
(define-inline (vec x y z) (f64vector x y z))
(define-inline (vec-x v)   (f64vector-ref v 0))
(define-inline (vec-y v)   (f64vector-ref v 1))
(define-inline (vec-z v)   (f64vector-ref v 2))
(define-inline (vec-x-set! v x) (f64vector-set! v 0 x))
(define-inline (vec-y-set! v y) (f64vector-set! v 1 y))
(define-inline (vec-z-set! v z) (f64vector-set! v 2 z))
(define-inline (make-vec)  (make-f64vector 3))

(define-inline (vdot v0 v1) (f64vector-dot v0 v1))
(define-inline (vcross! v v0 v1)
  (f64vector-set! v 0 (- (* (vec-y v0) (vec-z v1)) (* (vec-z v0) (vec-y v1))))
  (f64vector-set! v 1 (- (* (vec-z v0) (vec-x v1)) (* (vec-x v0) (vec-z v1))))
  (f64vector-set! v 2 (- (* (vec-x v0) (vec-y v1)) (* (vec-y v0) (vec-x v1)))))
(define-inline (vnormalize! v)
  (let1 size (real-sqrt (vdot v v))
    (when (> (abs size) 1.0e-17)
      (f64vector-div! v size))))
(define-inline (vec-diff v0 v1) (f64vector-sub v0 v1))
(define-inline (vec-mul! v0 x)  (f64vector-mul! v0 x))
(define-inline (vec-add! v0 v1) (f64vector-add! v0 v1))
(define-inline (vec-sub! v0 v1) (f64vector-sub! v0 v1))
(define-inline (vec-set! v0 v1) (f64vector-copy! v0 0 v1))

;(define-record-type isect #t #t (t) (p) (n) (hit))
(define (make-isect) (vector 1.0e+17 (make-vec) (make-vec) #f))
(define-inline (isect-t isect) (vector-ref isect 0))
(define-inline (isect-p isect) (vector-ref isect 1))
(define-inline (isect-n isect) (vector-ref isect 2))
(define-inline (isect-hit isect) (vector-ref isect 3))
(define-inline (isect-t-set! isect t) (vector-set! isect 0 t))
(define-inline (isect-hit-set! isect hit) (vector-set! isect 3 hit))
(define-inline (isect-init! isect)
  (isect-t-set! isect 1.0e+17)
  (isect-hit-set! isect #f))

(define (make-sphere x y z r) (cons (vec x y z) r))
(define-inline sphere-center car)
(define-inline sphere-radius cdr)

(define make-plane cons)
(define-inline plane-p car)
(define-inline plane-n cdr)

(define make-ray cons)
(define-inline ray-org car)
(define-inline ray-dir cdr)

(define *spheres*
  `(,(make-sphere -2.0 0.0 -3.5 0.5)
    ,(make-sphere -0.5 0.0 -3.0 0.5)
    ,(make-sphere  1.0 0.0 -2.2 0.5)))

(define *plane*
  (make-plane (vec 0.0 -0.5 0.0) (vec 0.0 1.0 0.0)))

(define (ray-sphere-intersect! isect ray sphere)  ; modify isect
  (let* ([rs (vec-diff (ray-org ray) (sphere-center sphere))]
         [B  (vdot rs (ray-dir ray))]
         [C  (- (vdot rs rs) (square (sphere-radius sphere)))]
         [D  (- (* B B) C)])
    (when (> D 0.0)
      (let1 t (- (+ B (real-sqrt D)))
        (when (< 0.0 t (isect-t isect))
          (isect-t-set! isect t)
          (isect-hit-set! isect #t)
          (let ([p (isect-p isect)]
                [n (isect-n isect)])
            (vec-set! p (ray-dir ray))
            (vec-mul! p t)
            (vec-add! p (ray-org ray))
            (vec-set! n p)
            (vec-sub! n (sphere-center sphere))
            (vnormalize! n)))))))

(define (ray-plane-intersect! isect ray plane) ; modify isect
  (let* ([d (- (vdot (plane-p plane) (plane-n plane)))]
         [v (vdot (ray-dir ray) (plane-n plane))])
    (unless (< (abs v) 1.0e-17)
      (let1 t (/ (- (+ (vdot (ray-org ray) (plane-n plane)) d)) v)
        (when (< 0.0 t (isect-t isect))
          (isect-t-set! isect t)
          (isect-hit-set! isect #t)
          (let ([p (isect-p isect)]
                [n (isect-n isect)])
            (vec-set! p (ray-dir ray))
            (vec-mul! p t)
            (vec-add! p (ray-org ray))
            (vec-set! n (plane-n plane))))))))

;; initialize three vectors b0, b1 and b2
(define (ortho-basis! b0 b1 b2 n)
  (vec-set! b2 n)
  (cond [(< -0.6 (vec-x n) 0.6) (vec-set! b1 '#f64(1.0 0.0 0.0))]
        [(< -0.6 (vec-y n) 0.6) (vec-set! b1 '#f64(0.0 1.0 0.0))]
        [(< -0.6 (vec-z n) 0.6) (vec-set! b1 '#f64(0.0 0.0 1.0))]
        [else                   (vec-set! b1 '#f64(1.0 0.0 0.0))])
  (vcross! b0 b1 b2)
  (vnormalize! b0)
  (vcross! b1 b2 b0)
  (vnormalize! b1))

;; `Out' is a vector of 5 vecs; the first one is used to store
;; the result color; the rest is used to hold intermediate values.
(define (ambient-occlusion! out isect occ-isect)
  (let ([ntheta NAO_SAMPLES]
        [nphi   NAO_SAMPLES]
        [eps    0.0001]
        [col (vector-ref out 0)]
        [p   (vector-ref out 1)]
        [b0  (vector-ref out 2)]
        [b1  (vector-ref out 3)]
        [b2  (vector-ref out 4)]
        [occlusion 0.0])
    (vec-set! p (isect-n isect))
    (vec-mul! p eps)
    (vec-add! p (isect-p isect))
    (ortho-basis! b0 b1 b2 (isect-n isect))
    (dotimes [j ntheta]
      (dotimes [i nphi]
        (let ([theta (real-sqrt (random-real))]
              [phi   (* 2.0 pi (random-real))])
          (let ([x (* (real-cos phi) theta)]
                [y (* (real-sin phi) theta)]
                [z (real-sqrt (- 1.0 (* theta theta)))])
            (let ([rx (+ (* x (vec-x b0)) (* y (vec-x b1)) (* z (vec-x b2)))]
                  [ry (+ (* x (vec-y b0)) (* y (vec-y b1)) (* z (vec-y b2)))]
                  [rz (+ (* x (vec-z b0)) (* y (vec-z b1)) (* z (vec-z b2)))])
              (let1 ray (make-ray p (vec rx ry rz))
                (isect-init! occ-isect)
                (dolist [sphere *spheres*]
                  (ray-sphere-intersect! occ-isect ray sphere))
                (ray-plane-intersect! occ-isect ray *plane*)
                (when (isect-hit occ-isect) (inc! occlusion))))))))
    (let1 occ (/ (- (* ntheta nphi) occlusion)
                 (* ntheta nphi))
      (vec-x-set! col occ)
      (vec-y-set! col occ)
      (vec-z-set! col occ)
      )))

(define (render-1 fimg y w h nsubsamples)
  (let ([buf   (vector-tabulate 5 (^_ (make-vec)))]
        [isect (make-isect)]
        [o-isect (make-isect)])
    (dotimes [x w]
      (define off (* 3 (+ (* y w) x)))
      (dotimes [v nsubsamples]
        (dotimes [u nsubsamples]
          (let* ([px (/ (+ x (- (/. u nsubsamples) (/ w 2.0))) (/ w 2.0))]
                 [py (/ (- (+ y (- (/. v nsubsamples) (/ h 2.0)))) (/ h 2.0))]
                 [ray (make-ray '#f64(0 0 0) (vec px py -1.0))])
            (vnormalize! (ray-dir ray))
            (isect-init! isect)
            (dolist [sphere *spheres*]
              (ray-sphere-intersect! isect ray sphere))
            (ray-plane-intersect! isect ray *plane*)
            (when (isect-hit isect)
              (ambient-occlusion! buf isect o-isect)
              (let1 col (vector-ref buf 0)
                ($ f64vector-set! fimg (+ off 0)
                   $ + (f64vector-ref fimg (+ off 0)) (vec-x col))
                ($ f64vector-set! fimg (+ off 1)
                   $ + (f64vector-ref fimg (+ off 1)) (vec-y col))
                ($ f64vector-set! fimg (+ off 2)
                   $ + (f64vector-ref fimg (+ off 2)) (vec-z col))))))))))

(define (render img w h nsubsamples)
  (let ([fimg (make-f64vector (* w h 3))]
        [nprocs (sys-available-processors)])
    (if (= nprocs 1)
      (dotimes [y h] (render-1 fimg y w h nsubsamples))
      (let1 pool (make-thread-pool nprocs)
        (dotimes [y h] (add-job! pool (^[] (render-1 fimg y w h nsubsamples))))
        (wait-all pool #f #e1e8)
        (terminate-all! pool)))
    (f64vector-div! fimg (/ (* nsubsamples nsubsamples) 255.5))
    (dotimes [i (* w h 3)]
      (u8vector-set! img i (clamp (f64vector-ref fimg i) 0 255)))))

(define (save-ppm name w h img)
  (with-output-to-file name
    (^[]
      (format #t "P6\n~d ~d\n255\n" w h)
      (write-uvector img))))

(define (main args)
  (let1 img (make-u8vector (* 3 WIDTH HEIGHT))
    (render img WIDTH HEIGHT NSUBSAMPLES)
    (save-ppm "ao.ppm" WIDTH HEIGHT img)
    0))
