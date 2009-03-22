;;
;; testing util.sparse
;;

(use gauche.test)
(use srfi-27)

(test-start "util.sparse")
(use util.sparse)
(test-module 'util.sparse)

(define (simple-test name obj %ref %set! key1 key2)
  (test* #`",name basic set!/ref" 'ok
         (begin (%set! obj (key1) 'ok)
                (%ref obj (key1))))
  (test* #`",name referencing nokey" *test-error*
         (%ref obj (key2)))
  (test* #`",name referencing nokey fallback" 'huh?
         (%ref obj (key2) 'huh?))
  (test* #`",name replace" 'okok
         (begin (%set! obj (key1) 'okok)
                (%ref obj (key1))))
  (test* #`",name add" 'okokok
         (begin (%set! obj (key2) 'okokok)
                (%ref obj (key2))))
  (test* #`",name ref" 'okok (%ref obj (key1)))
  )

(define (const x) (lambda () x))

;; sparse vector-------------------------------------------------
(test-section "spvector")

(simple-test "spvector" (make-spvector) spvector-ref spvector-set!
             (const 0) (const 1))

;(define *data-set-size* 500000)
(define *data-set-size* 200000)

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
                                                ,(spvector-ref spv k #f)))))
                            0)))

  (test* "spvector many numelements" *data-set-size* (spvector-num-elements spv))
  (test* "spvector many ref" *data-set-size*
         (let/cc return
           (hash-table-fold *data-set*
                            (lambda (k v cnt)
                              (if (equal? (spvector-ref spv k) v)
                                (+ cnt 1)
                                (return `(error ,cnt ,k ,v
                                                ,(spvector-ref spv k)))))
                            0)))
  )

;; sparse table----------------------------------------------------
(test-section "sptable")

(simple-test "sptable (eq?)" (make-sptable 'eq?)
             sptable-ref sptable-set! (const 'a) (const 'b))
(simple-test "sptable (eqv?)" (make-sptable 'eqv?)
             sptable-ref sptable-set! (cut / 3) (cut / 2))
(simple-test "sptable (equal?)" (make-sptable 'equal?)
             sptable-ref sptable-set! (cut list 1) (cut list 2))
(simple-test "sptable (string=?)" (make-sptable 'string=?)
             sptable-ref sptable-set! (cut string #\a) (cut string #\b))

(define (heavy-test type keygen)
  (let1 st (make-sptable type)
    (test* #`"sptable (,type) many set!" *data-set-size*
           (let/cc return
             (hash-table-fold *data-set*
                              (lambda (k v cnt)
                                (if (and (not (sptable-ref st (keygen k) #f))
                                         (begin
                                           (sptable-set! st (keygen k) v)
                                           (equal? v (sptable-ref st (keygen k) #f))))
                                  (+ cnt 1)
                                  (return `(error ,cnt ,(keygen k) ,v
                                                  ,(sptable-ref st (keygen k) #f)))))
                              0)))

    (test* #`"sptable (,type) many numelements" *data-set-size*
           (sptable-num-elements st))

    (unless (eqv? (sptable-num-elements st) *data-set-size*)
      (%sptable-dump st))

    (test* #`"sptable (,type) many ref" *data-set-size*
           (let/cc return
             (hash-table-fold *data-set*
                              (lambda (k v cnt)
                                (if (equal? (sptable-ref st (keygen k)) v)
                                  (+ cnt 1)
                                  (return `(error ,cnt ,(keygen k) ,v
                                                  ,(sptable-ref st (keygen k))))))
                              0)))
    ))

(heavy-test 'eqv? values)
(heavy-test 'equal? (lambda (k) (list k k)))
(heavy-test 'string=? (lambda (k) (number->string k 36)))

(test-end)
