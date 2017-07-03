;;;
;;; Vector library (srfi-43)
;;;

;; This is kept only for the backward compatibility.
;; Most functions are covered by srfi-133.   A few have the same
;; functionality with diffrent names in the core.

(define-module srfi-43
  (use srfi-133 :except (vector-fold vector-fold-right
                         vector-map vector-map! vector-for-each
                         vector-count))
  (export
   ;; Those are either built-in or srfi-133
   vector-unfold vector-unfold-right
   vector-reverse-copy vector-append vector-concatenate
   vector-empty? vector=
   vector-index vector-index-right
   vector-skip vector-skip-right
   vector-binary-search
   vector-any vector-every
   vector-copy! vector-swap! vector-reverse! vector-reverse-copy!
   reverse-vector->list reverse-list->vector

   ;; srfi-43 specific procedures
   vector-fold vector-fold-right vector-count
   (rename vector-map-with-index vector-map) ; built-in
   (rename vector-map-with-index! vector-map!) ; built-in
   (rename vector-for-each-with-index vector-for-each) ;built-in
   ))
(select-module srfi-43)

;; A few incompatible procedures

(define vector-fold
  (case-lambda
    ([proc seed v] ; fast path
     (assume-type v <vector>)
     (let1 len (vector-length v)
       (let loop ([i 0] [seed seed])
         (if (= i len) seed (loop (+ i 1) (proc i seed (vector-ref v i)))))))
    ([proc seed v . vs]
     (let* ([vs (cons v vs)]
            [len (fold (^[v len]
                         (assume-type v <vector>)
                         (if len (min (vector-length v) len) (vector-length v)))
                       #f vs)])
       (let loop ([i 0] [seed seed])
         (if (= i len)
           seed
           (loop (+ i 1)
                 (apply proc i seed (map (cut vector-ref <> i) vs)))))))))

(define vector-fold-right
  (case-lambda
    ([proc seed v] ; fast path
     (assume-type v <vector>)
     (let1 len (vector-length v)
       (let loop ([i (- len 1)] [seed seed])
         (if (= i -1) seed (loop (- i 1) (proc i seed (vector-ref v i)))))))
    ([proc seed v . vs]
     (let* ([vs (cons v vs)]
            [len (fold (^[v len]
                         (assume-type v <vector>)
                         (if len (min (vector-length v) len) (vector-length v)))
                       #f vs)])
       (let loop ([i (- len 1)] [seed seed])
         (if (= i -1)
           seed
           (loop (- i 1)
                 (apply proc i seed (map (cut vector-ref <> i) vs)))))))))

(define vector-count
  (case-lambda
    ([pred v] ; fast path
     (vector-fold (^[i c e] (if (pred i e) (+ 1 c) c)) 0 v))
    ([pred v . vs]
     (apply vector-fold (^[i c . es] (if (apply pred i es) (+ 1 c) c)) 0 v vs))))
