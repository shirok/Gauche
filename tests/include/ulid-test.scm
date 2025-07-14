(import (scheme base))
(import (ulid))
(import (srfi 19))                      ;time & date
(import (srfi 78))                      ;check

(define g (make-ulid-generator))

(check (ulid? (g)) => #t)

(let ((u (g)))
  (check (integer? (ulid-timestamp u)) => #t)
  (check (integer? (ulid-randomness u)) => #t)

  (let ((v (ulid->bytevector u)))
    (check (bytevector? v) => #t)
    (check (bytevector-length v) => 16)
    (check (bytevector->ulid v) (=> ulid=?) u))

  (let ((s (ulid->string u)))
    (check (string? s) => #t)
    (check (string-length s) => 26)
    (check (string->ulid s) (=> ulid=?) u))

  (let ((n (ulid->integer u)))
    (check (integer? n) => #t)
    (check (< 0 n (expt 2 128)) => #t)
    (check (integer->ulid n) (=> ulid=?) u))

  ;; uniqueness & ordering
  (let* ((u1 (g))
         (u2 (g)))
    (check (ulid=? u1 u2) => #f)
    (check (ulid<? u1 u2) => #t)
    (check (ulid<? u2 u1) => #f))
  )

;; Timestamp check
(let* ((epoch (date->time-utc (make-date 0 0 0 0 1 1 1970 0)))
       (t0 (current-time))
       (u (g))
       (t1 (current-time)))
  (define (nsecs-from-epoch t)
    (let ((dt (time-difference t epoch)))
      (+ (* (time-second dt) #e1e9) (time-nanosecond dt))))
  (check (<= (floor (/ (nsecs-from-epoch t0) #e1e6))
             (ulid-timestamp u)
             (ceiling (/ (nsecs-from-epoch t1) #e1e6)))
         => #t))

;; Counstruct with specific timestamp
;; (This test can fail in 1/2^80 probability)
(let* ((u0 (g 12345))
       (u1 (g 12345)))
  (check (ulid-timestamp u0) => 12345)
  (check (ulid-timestamp u1) => 12345)
  (check (+ (ulid-randomness u0) 1) => (ulid-randomness u1)))

(check-report)
