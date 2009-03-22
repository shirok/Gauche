;;
;; testing util.sparse
;;

(use gauche.test)
(use srfi-27)

(test-start "util.sparse")
(use util.sparse)
(test-module 'util.sparse)

;; sparse vector
(test-section "spvector")

(let1 v (make-spvector)
  (test* "spvector basic set!/ref" 'ok
         (begin (spvector-set! v 0 'ok)
                (spvector-ref v 0)))
  (test* "spvector referencing noval" *test-error*
         (spvect-ref v 1))
  (test* "spvector more values" 'okok
         (begin (spvector-set! v 0 'okok)
                (spvector-ref v 0)))
  )

(define *data-set-size* 500000)

(define *data-set*
  (rlet1 ht (make-hash-table 'eqv?)
    (let loop ((i 0))
      (unless (= i *data-set-size*)
        (let1 k (random-integer (expt 2 32))
          (cond [(hash-table-exists? ht k) (loop i)]
                [else (hash-table-put! ht k (* k k)) (loop (+ i 1))]))))))

(let1 spv (make-spvector)
  (test* "spvector many set!" *data-set-size*
         (let/cc return
           (hash-table-fold *data-set*
                            (lambda (k v cnt)
                              (if (and (not (spvector-ref spv k #f))
                                       (begin
                                         (spvector-set! spv k v)
                                         (equal? v (spvector-ref spv k #f))))
                                (+ cnt 1)
                                (return `(error ,cnt ,k ,v
                                                (spvector-ref spv k #f)))))
                            0)))

  (test* "numelements" *data-set-size* (spvector-num-elements spv))
  (test* "spvector many ref" *data-set-size*
         (let/cc return
           (hash-table-fold *data-set*
                            (lambda (k v cnt)
                              (if (equal? (spvector-ref spv k) v)
                                (+ cnt 1)
                                (return `(error ,cnt ,k ,v ,(spvector-ref spv k)))))
                            0)))
  )

(test-end)
