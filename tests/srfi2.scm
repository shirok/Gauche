;;
;; Tests SRFIs that relies on multiple extension libraries and needs
;; to be run after they are tested.
;;

(use gauche.test)

(test-start "Additional SRFIs")

(test-section "SRFI-271")

(use srfi.271)
(test-module 'srfi.271)
(use srfi.271.randomized)
(test-module 'srfi.271.randomized)
(use srfi.271.determinized)
(test-module 'srfi.271.determinized)

(define-module srfi-271-tests
  (use gauche.test)
  (use srfi.64)
  (test-include-r7 "include/srfi-271-tests")
  )

(define-module srfi-271-stat-tests
  (use gauche.test)
  (use gauche.uvector)
  (use gauche.sequence)
  (use srfi.271.randomized :prefix r:)
  (use srfi.271.determinized :prefix d:)

  ;; This test reads NUM-BYTES samples from random input port,
  ;; one from r:make-random-port and one from d:make-random-port,
  ;; and check if they are distributed evenly.

  (define num-bytes 16384)              ;64 samples per bin on average

  (define (byte-histogram port nbytes)
    (rlet1 hist (make-vector 256 0)
      (dotimes [nbytes]
        (let1 b (read-u8 port)
          (assume (and (exact-integer? b) (<= 0 b 255))
                  "Random port returned a bogus octet:" b)
          (inc! (vector-ref hist b))))))

  ;; Pearson's chi-square statistic of HIST against the uniform
  ;; distribution.  With 256 bins, it has 255 degrees of freedom,
  ;; so it should be around 255 +- 22.6 (= sqrt(2*255)).
  (define (chi-square hist nbytes)
    (let1 expected (/ nbytes (vector-length hist) 1.0)
      (/ (fold (^[observed sum] (+ sum (square (- observed expected)))) 0 hist)
         expected)))

  ;; The bounds below are way out of 5-sigma, so a sane generator
  ;; hardly ever trips them.
  (define (test-uniformity name make-port)
    (test* #"octet distribution of ~|name| random port" #t
           (let* ([hist (call-with-port (make-port)
                          (cut byte-histogram <> num-bytes))]
                  [chi2 (chi-square hist num-bytes)])
             ;; Returns chi2 itself on failure, to show it in the report.
             (or (< 120 chi2 450) chi2))))

  (test-uniformity "randomized" r:make-random-port)
  (test-uniformity "determinized" d:make-random-port)
  )

(test-section "SRFI-274")

(use srfi.274)
(test-module 'srfi.274)



(test-section "SRFI-281")

(use srfi.281)
(test-module 'srfi.281)

(test-end)
