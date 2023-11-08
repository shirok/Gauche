;;
;; How slow if we make default 'map' and 'for-each' generic functions?
;;

(use gauche.time)
(use gauche.sequence :rename ((map generic-map) (for-each generic-for-each)))

((with-module gauche.object generic-build-dispatcher!)
 generic-for-each 1)

(define (repeat f)
  (dotimes [1000]
    (f identity '(a a a a a a a a a a))))

(define (bench)
  (time-these/report '(cpu 10)
                     `((specific . ,(cut repeat for-each))
                       (generic  . ,(cut repeat generic-for-each)))))

(define (main args)
  (bench)
  0)
