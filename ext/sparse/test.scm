;;
;; testing util.sparse
;;

(use gauche.test)
(use srfi-27)

(test-start "util.sparse")
(use util.sparse)
(test-module 'util.sparse)

(define (simple-test name obj %ref %set! %exists? key1 key2
                     :optional (val1 'ok) (val2 'okok) (val3 'okokok))
  (test* #`",name basic set!/ref" val1
         (begin (%set! obj (key1) val1)
                (%ref obj (key1))))
  (test* #`",name referencing nokey" *test-error*
         (%ref obj (key2)))
  (test* #`",name referencing nokey fallback" 'huh?
         (%ref obj (key2) 'huh?))
  (test* #`",name exists?" #t (%exists? obj (key1)))
  (test* #`",name exists?" #f (%exists? obj (key2)))
  (test* #`",name replace" val2
         (begin (%set! obj (key1) val2)
                (%ref obj (key1))))
  (test* #`",name add" val3
         (begin (%set! obj (key2) val3)
                (%ref obj (key2))))
  (test* #`",name ref" val2 (%ref obj (key1)))
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

(define (heavy-test name obj %ref %set! %cnt %clr %keys %vals %del keygen)
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
  (when %keys
    (test* #`",name keys" *data-set-size*
           (let/cc return
             (let1 tt (make-sparse-table 'equal?)
               (hash-table-for-each *data-set*
                                    (lambda (k v)
                                      (sparse-table-set! tt (keygen k) #t)))
               (fold (lambda (k cnt)
                       (if (sparse-table-ref tt k #f)
                         (+ cnt 1)
                         (return `(error ,cnt ,k))))
                     0 (%keys obj))))))
  (when %vals
    (test* #`",name values" *data-set-size*
           (let/cc return
             (let1 tt (make-sparse-table 'equal?)
               (hash-table-for-each *data-set*
                                    (lambda (k v)
                                      (sparse-table-set! tt v #t)))
               (fold (lambda (v cnt)
                       (if (sparse-table-ref tt v #f)
                         (+ cnt 1)
                         (return `(error ,cnt ,v))))
                     0 (%vals obj))))))
    
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

  (when %del
    (test* #`",name many delete!" '(#t 0)
           (begin
             (hash-table-for-each *data-set*
                                  (lambda (k v) (%set! obj (keygen k) v)))
             (let1 r
                 (hash-table-fold *data-set*
                                  (lambda (k v s) (and s (%del obj (keygen k))))
                                  #t)
               (list r (%cnt obj))))))
  )

;; sparse vector-------------------------------------------------
(test-section "sparse-vector")

(define (spvec-simple tag)
  (apply simple-test #`"sparse-,(or tag \"\")vector" (make-sparse-vector tag)
         sparse-vector-ref sparse-vector-set! sparse-vector-exists?
         (const 0) (const 1)
         (if (memq tag '(f16 f32 f64))
           '(3.0 6.0 9.0)
           '(3 6 9))))

(for-each spvec-simple '(#f s8 u8 s16 u16 s32 u32 s64 u64 f16 f32 f64))

(heavy-test "sparse-vector" (make-sparse-vector) sparse-vector-ref sparse-vector-set!
            sparse-vector-num-entries sparse-vector-clear!
            sparse-vector-keys sparse-vector-values sparse-vector-delete!
            values)

;; sparse table----------------------------------------------------
(test-section "sparse-table")

(define (sptab-simple type key1 key2)
  (simple-test #`"sparse-table (,type)" (make-sparse-table type)
               sparse-table-ref sparse-table-set! sparse-table-exists?
               key1 key2))

(sptab-simple 'eq?     (const 'a) (const 'b))
(sptab-simple 'eqv?    (cut / 3) (cut / 2))
(sptab-simple 'equal?  (cut list 1) (cut list 2))
(sptab-simple 'string=? (cut string #\a) (cut string #\b))

(define (sptab-heavy-test type keygen)
  (heavy-test #`"sparse-table (,type)" (make-sparse-table type)
              sparse-table-ref sparse-table-set! sparse-table-num-entries sparse-table-clear!
              sparse-table-keys sparse-table-values sparse-table-delete! keygen))

(sptab-heavy-test 'eqv? values)
(sptab-heavy-test 'equal? (lambda (k) (list k k)))
(sptab-heavy-test 'string=? (lambda (k) (number->string k 36)))

(test-end)
