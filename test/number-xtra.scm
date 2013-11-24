;;
;; Extra test for number reading/writing
;;

;; Generate test file.
(define (generate-fixnum-file file num)
  (with-output-to-file file
    (lambda ()
      (dotimes (n num)
;        (display (modulo (sys-random) #x20000000))
        (display (sys-random))
        (newline)))))

(define (generate-bignum-file file num)
  (with-output-to-file file
    (lambda ()
      (dotimes (n num)
        (let ((k (+ (* RAND_MAX RAND_MAX (sys-random))
                    (* RAND_MAX (sys-random))
                    (sys-random))))
          (display k) (newline))))))

(define (generate-flonum-file file num)
  (with-output-to-file file
    (lambda ()
      (dotimes (n num)
        (let* ((m (/ (sys-random) RAND_MAX))
               (e (inexact->exact (floor (* (/ (sys-random) RAND_MAX) 1023))))
               (s (if (odd? (sys-random)) -1 1))
               (f (ldexp m (* s e))))
          (display f) (newline))))))

;; Benchmarking readers
(define (test-reader file repeat)
  (let ((input (call-with-input-file file port->string-list)))
    (receive (sec0 usec0) (sys-gettimeofday)
      (dotimes (n repeat)
        (for-each string->number input))
      (receive (sec1 usec1) (sys-gettimeofday)
        (- (+ (* sec1 1000000) usec1)
           (+ (* sec0 1000000) usec0))))))

;; Benchmarking writers
(define (test-writer file repeat)
  (let ((input (call-with-input-file file port->sexp-list)))
    (receive (sec0 usec0) (sys-gettimeofday)
      (dotimes (n repeat)
        (for-each number->string input))
      (receive (sec1 usec1) (sys-gettimeofday)
        (- (+ (* sec1 1000000) usec1)
           (+ (* sec0 1000000) usec0))))))

;; test writer-reader invariance
(define (test-writer-reader-invariance file)
  (with-input-from-file file
    (lambda ()
      (generator-for-each (lambda (input)
                            (let* ((num (string->number input))
                                   (num2 (string->number (number->string num))))
                              (unless (eqv? num num2)
                                (print #"ERROR: ~num and ~num2 (original ~input)"))))
                          read-line))))


;; benchmarking bignum arithmetic
(define (test-bignum-arith file repeat)
  (let ((input (call-with-input-file file port->sexp-list)))
    (receive (sec0 usec0) (sys-gettimeofday)
      (dotimes (i repeat)
        (for-each (^x (- (* (+ x x) x) x))
                  input))
      (receive (sec1 usec1) (sys-gettimeofday)
        (- (+ (* sec1 1000000) usec1)
           (+ (* sec0 1000000) usec0))))))

