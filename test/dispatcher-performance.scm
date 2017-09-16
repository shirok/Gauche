;;
;; Method dispatcher benchmark
;;

;; NB: We assume dispatch accelerator isn't on by default.

(use gauche.sequence)
(use gauche.parameter)
(use gauche.time)
(use util.match)
(use data.sparse)

;; To reduce the effect of funcall
(define-syntax unroll
  (er-macro-transformer
   (^[f r c]
     (match (cdr f)
       [(fn vec)
        `(begin ,@(map (^[i] (quasirename r (,fn ,vec ,(mod i 3))))
                       (iota 10000)
                       ))]
       [(fn)
        `(begin ,@(map (^[i] (quasirename r (,fn)))
                       (iota 10000)
                       ))]
       ))))
        

(define (do-vector v)
  (dotimes [1000] (unroll vector-ref v)))

(define (do-sparse v)
  (dotimes [1000] (unroll sparse-vector-ref v)))

(define (do-ref v)
  (dotimes [1000] (unroll ref v)))

(define (do-~ v)
  (dotimes [1000] (unroll ref v)))

(define param (make-parameter #f))

(define (do-param)
  (dotimes [1000] (unroll param)))

(define (bench)
  (define v (make-vector 10))
  (define sv (make-sparse-vector #f :default 0))
  (time-these/report 1
                     `((vec-specific . ,(cut do-vector v))
                       (vec-ref      . ,(cut do-ref v))
                       (vec-~        . ,(cut do-~ v))
                       (sv-specific  . ,(cut do-sparse sv))
                       (sv-ref       . ,(cut do-ref sv))
                       (sv-~         . ,(cut do-~ sv))
                       (parameter    . ,(cut do-param))
                       ))
  )

(define (main args)
  (print "Before dispatcher")
  (bench)
  ((with-module gauche.object generic-build-dispatcher!) ref 0)
  ((with-module gauche.object generic-build-dispatcher!) object-apply 0)
  (print "After dispatcher")
  (bench)
  )

#|
in Scm_SortMethods shortcut for method length=1 list
using sparse vector
   generic: 2.639 real, 3.010 cpu (3.010 user + 0.000 sys)@0.33/s n=1
  specific: 0.412 real, 0.410 cpu (0.410 user + 0.000 sys)@2.44/s n=1
   generic: 2.532 real, 2.870 cpu (2.870 user + 0.000 sys)@0.35/s n=1
  specific: 0.440 real, 0.430 cpu (0.430 user + 0.000 sys)@2.33/s n=1
   generic: 2.502 real, 2.870 cpu (2.870 user + 0.000 sys)@0.35/s n=1
  specific: 0.412 real, 0.410 cpu (0.410 user + 0.000 sys)@2.44/s n=1

shortcut in vmcall.c
   generic: 2.472 real, 2.850 cpu (2.850 user + 0.000 sys)@0.35/s n=1
  specific: 0.417 real, 0.410 cpu (0.410 user + 0.000 sys)@2.44/s n=1
   generic: 2.406 real, 2.770 cpu (2.770 user + 0.000 sys)@0.36/s n=1
  specific: 0.410 real, 0.410 cpu (0.410 user + 0.000 sys)@2.44/s n=1
   generic: 2.417 real, 2.760 cpu (2.750 user + 0.010 sys)@0.36/s n=1
  specific: 0.458 real, 0.460 cpu (0.460 user + 0.000 sys)@2.17/s n=1

w/o shortcut
   generic: 3.036 real, 3.860 cpu (3.860 user + 0.000 sys)@0.26/s n=1
  specific: 0.416 real, 0.410 cpu (0.410 user + 0.000 sys)@2.44/s n=1
   generic: 3.027 real, 3.850 cpu (3.830 user + 0.020 sys)@0.26/s n=1
  specific: 0.437 real, 0.430 cpu (0.430 user + 0.000 sys)@2.33/s n=1
   generic: 3.132 real, 3.880 cpu (3.880 user + 0.000 sys)@0.26/s n=1
  specific: 0.436 real, 0.430 cpu (0.430 user + 0.000 sys)@2.33/s n=1

|#
  
         
  

