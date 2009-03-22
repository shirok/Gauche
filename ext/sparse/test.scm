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

(define *data-set-size* 200000)

(define *data-set*
  (rlet1 ht (make-hash-table 'eqv?)
    (let loop ((i 0))
      (unless (= i *data-set-size*)
        (let1 k (random-integer (expt 2 32))
          (cond [(hash-table-exists? ht k) (loop i)]
                [else (hash-table-put! ht k (* k k)) (loop (+ i 1))]))))))

(define (heavy-test name obj %ref %set! %cnt %clr keygen)
  (test* #`",name many set!" *data-set-size*
         (let/cc return
           (hash-table-fold *data-set*
                            (lambda (k v cnt)
                              (let1 kk (keygen k)
                                (if (and (not (%ref obj kk #f))
                                         (begin (%set! obj kk v)
                                                (equal? v (%ref obj kk #f))))
                                  (+ cnt 1)
                                  (return
                                   `(error ,cnt ,kk ,v ,(%ref obj kk #f))))))
                            0)))

  (test* #`",name many numelements" *data-set-size* (%cnt obj))
  (test* #`",name many ref" *data-set-size*
         (let/cc return
           (hash-table-fold *data-set*
                            (lambda (k v cnt)
                              (let1 kk (keygen k)
                                (if (equal? (%ref obj kk) v)
                                  (+ cnt 1)
                                  (return `(error ,cnt ,kk ,v ,(%ref obj kk))))))
                            0)))
  (test* #`",name many clear!" 0 (begin (%clr obj) (%cnt obj)))
  (test* #`",name many ref2" *data-set-size*
         (let/cc return
           (hash-table-fold *data-set*
                            (lambda (k v cnt)
                              (let1 kk (keygen k)
                                (if (%ref obj kk #f)
                                  (return `(error ,cnt ,kk ,v ,(%ref obj kk)))
                                  (+ cnt 1))))
                            0)))
  )

;; sparse vector-------------------------------------------------
(test-section "spvector")

(simple-test "spvector" (make-spvector) spvector-ref spvector-set!
             (const 0) (const 1))

(heavy-test "spvector" (make-spvector) spvector-ref spvector-set!
            spvector-num-entries spvector-clear! values)

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

(define (sptab-heavy-test type keygen)
  (heavy-test #`"sptable (,type)" (make-sptable type)
              sptable-ref sptable-set! sptable-num-entries sptable-clear!
              keygen))

(sptab-heavy-test 'eqv? values)
(sptab-heavy-test 'equal? (lambda (k) (list k k)))
(sptab-heavy-test 'string=? (lambda (k) (number->string k 36)))

(test-end)
