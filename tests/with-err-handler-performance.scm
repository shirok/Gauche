;;
;; benchmarking with-error-handler optimization
;;

(use gauche.time)

(define (f n p)
  (if (zero? n)
    (if p
      (raise 'f)
      n)
    (with-error-handler
     (lambda (e) (raise e))
     (lambda () (f (- n 1) p)))))

(define (f-exc n)
  (with-error-handler values (cut f n #t)))

(define (f-no-exc n)
  (with-error-handler values (cut f n #f)))

(define (main args)
  (time-these/report 30000
                     `((f-exc . ,(cut f-exc 1000))
                       (f-no-exc . ,(cut f-no-exc 1000))))
  0)

#|
OS  : Windows 11 25H2
CPU : AMD Ryzen 7 7735U (2.70 GHz)
RAM : 16.0 GB

;; 0.9.16_pre2 (change vm error handling (2026-03-17))
Benchmark: ran f-exc, f-no-exc, each for 30000 times.
     f-exc: 190.959 real, 190.188 cpu (157.250 user + 32.938 sys)@ 157.74/s n=30000
  f-no-exc:  12.808 real,  12.734 cpu ( 11.578 user +  1.156 sys)@2355.90/s n=30000

             Rate  f-exc f-no-exc
     f-exc  158/s     --    0.067
  f-no-exc 2356/s 14.935       --

;; 0.9.16_pre2 (commit f37f980 (2026-03-16))
Benchmark: ran f-exc, f-no-exc, each for 30000 times.
     f-exc: 213.385 real, 212.766 cpu (178.844 user + 33.922 sys)@141.00/s n=30000
  f-no-exc:  31.590 real,  31.500 cpu ( 30.000 user +  1.500 sys)@952.38/s n=30000

            Rate f-exc f-no-exc
     f-exc 141/s    --    0.148
  f-no-exc 952/s 6.754       --

;; 0.9.15
Benchmark: ran f-exc, f-no-exc, each for 30000 times.
     f-exc: 151.355 real, 151.953 cpu (127.500 user + 24.453 sys)@197.43/s n=30000
  f-no-exc:  45.784 real,  46.328 cpu ( 38.609 user +  7.719 sys)@647.56/s n=30000

            Rate f-exc f-no-exc
     f-exc 197/s    --    0.305
  f-no-exc 648/s 3.280       --
|#
