;;
;; benchmarking with-exception-handler optimization
;;

(use gauche.time)

(define (f n p)
  (if (zero? n)
    (if p
      (raise 'f)
      n)
    (with-exception-handler
     (lambda (e) (raise e))
     (lambda () (f (- n 1) p)))))

(define (f-exc n)
  (with-exception-handler values (cut f n #t)))

(define (f-no-exc n)
  (with-exception-handler values (cut f n #f)))

(define (main args)
  (time-these/report 30000
                     `((f-exc . ,(cut f-exc 1000))
                       (f-no-exc . ,(cut f-no-exc 1000))))
  0)

#|
;; 0.9.15
Benchmark: ran f-exc, f-no-exc, each for 30000 times.
     f-exc: 38.958 real, 77.360 cpu (76.080 user + 1.280 sys)@ 387.80/s n=30000
  f-no-exc:  5.994 real, 25.960 cpu (25.330 user + 0.630 sys)@1155.62/s n=30000

             Rate f-exc f-no-exc
     f-exc  388/s    --    0.336
  f-no-exc 1156/s 2.980       --

;; As of Dec 12 2024
Benchmark: ran f-exc, f-no-exc, each for 30000 times.
     f-exc: 34.362 real, 50.910 cpu (50.340 user + 0.570 sys)@ 589.28/s n=30000
  f-no-exc:  2.631 real,  5.800 cpu ( 5.680 user + 0.120 sys)@5172.41/s n=30000

             Rate f-exc f-no-exc
     f-exc  589/s    --    0.114
  f-no-exc 5172/s 8.778       --
|#
