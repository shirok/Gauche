;;
;; testing util.sparse
;;

(use gauche.test)
(use srfi-1)
(use srfi-27)

(test-start "util.sparse")
(use util.sparse)
(test-module 'util.sparse)

(define (simple-test name obj %ref %set! %exists? key1 key2
                     :optional (val1 'ok) (val2 'okok) (val3 'okokok))
  (test* #`",name basic set!/ref" val1
         (begin (%set! obj (key1) val1)
                (%ref obj (key1))))
  (test* #`",name referencing nokey" (test-error)
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
  (test* #`",name generic setter" val1
         (begin (set! (%ref obj (key1)) val1)
                (%ref obj (key1))))
  )

(define (const x) (^[] x))

;; To test sparse table seriously, set *data-set-size* larger number
;; like 20000.  The default size is chosen not to take too long for the tests.
(define *data-set-size* 5000)

(define *data-set*
  (rlet1 ht (make-hash-table 'eqv?)
    (let loop ([i 0])
      (unless (= i *data-set-size*)
        (let1 k (random-integer (expt 2 32))
          (cond [(hash-table-exists? ht k) (loop i)]
                [else (hash-table-put! ht k (* k k)) (loop (+ i 1))]))))))

(define (heavy-test name obj %ref %set! %cnt %clr %keys %vals %del %copy
                    %check keygen valgen)
  (test* #`",name many set!" *data-set-size*
         (let/cc return
           (hash-table-fold *data-set*
                            (^[k v cnt]
                              (let ([kk (keygen k)]
                                    [vv (valgen v)])
                                (if (and (not (%ref obj kk #f))
                                         (begin (%set! obj kk vv)
                                                (equal? vv (%ref obj kk #f))))
                                  (+ cnt 1)
                                  (return
                                   `(error ,cnt ,kk ,vv ,(%ref obj kk #f))))))
                            0)))

  (test* #`",name many numelements" *data-set-size* (%cnt obj))

  (when %check (test* #`",name many check" #t (begin (%check obj) #t)))

  (test* #`",name many ref" *data-set-size*
         (let/cc return
           (hash-table-fold *data-set*
                            (^[k v cnt]
                              (let ([kk (keygen k)]
                                    [vv (valgen v)])
                                (if (equal? (%ref obj kk) vv)
                                  (+ cnt 1)
                                  (return `(error ,cnt ,kk ,vv ,(%ref obj kk))))))
                            0)))
  (test* #`",name keys" *data-set-size*
         (let/cc return
           (let1 tt (make-sparse-table 'equal?)
             (hash-table-for-each *data-set*
                                  (^[k v]
                                    (sparse-table-set! tt (keygen k) #t)))
             (fold (^[k cnt] (if (sparse-table-ref tt k #f)
                               (+ cnt 1)
                               (return `(error ,cnt ,k))))
                   0 (%keys obj)))))
  (test* #`",name values" *data-set-size*
         (let/cc return
           (let1 tt (make-sparse-table 'equal?)
             (hash-table-for-each *data-set*
                                  (^[k v]
                                    (sparse-table-set! tt (valgen v) #t)))
             (fold (^[v cnt] (if (sparse-table-ref tt v #f)
                               (+ cnt 1)
                               (return `(error ,cnt ,v))))
                   0 (%vals obj)))))
  (test* #`",name many copy" (list *data-set-size* #t #t)
         (let* ([new (%copy obj)]
                [keys (%keys new)])
           (list (length keys)
                 (if %check (begin (%check new) #t) #t)
                 (every (^k (equal? (%ref new k) (%ref obj k))) keys))))

  (test* #`",name many clear!" 0 (begin (%clr obj) (%cnt obj)))
  (test* #`",name many ref2" *data-set-size*
         (let/cc return
           (hash-table-fold *data-set*
                            (^[k v cnt]
                              (let ([kk (keygen k)]
                                    [vv (valgen v)])
                                (if (%ref obj kk #f)
                                  (return `(error ,cnt ,kk ,vv ,(%ref obj kk)))
                                  (+ cnt 1))))
                            0)))

  (test* #`",name many delete!" '(#t 0)
         (begin
           (hash-table-for-each *data-set*
                                (^[k v] (%set! obj (keygen k) (valgen v))))
           (when %check (%check obj))
           (let1 r
               (hash-table-fold *data-set*
                                (^[k v s]
                                  (when %check (%check obj))
                                  (and s (%del obj (keygen k))))
                                #t)
             (list r (%cnt obj)))))
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

(define (spvec-heavy tag valgen)
  (heavy-test #`"sparse-,(or tag \"\")vector"
              (make-sparse-vector tag)
              sparse-vector-ref sparse-vector-set!
              sparse-vector-num-entries sparse-vector-clear!
              sparse-vector-keys sparse-vector-values sparse-vector-delete!
              sparse-vector-copy #f
              values valgen))

(spvec-heavy #f values)
(spvec-heavy 's8  (^x (- (logand x #xff) #x80)))
(spvec-heavy 'u8  (^x (logand x #xff)))
(spvec-heavy 's16 (^x (- (logand x #xffff) #x8000)))
(spvec-heavy 'u16 (^x (logand x #xffff)))
(spvec-heavy 's32 (^x (- (logand x #xffffffff) #x80000000)))
(spvec-heavy 'u32 (^x (logand x #xffffffff)))
(spvec-heavy 's64 (^x (- (logand x #xffffffffffffffff)
                                 #x8000000000000000)))
(spvec-heavy 'u64 (^x (logand x #xffffffffffffffff)))
(spvec-heavy 'f16 (^x (exact->inexact (logand x #x3ff))))
(spvec-heavy 'f32 (^x (exact->inexact (logand x #xfffff))))
(spvec-heavy 'f64 exact->inexact)


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

(define (sptab-heavy type keygen)
  (heavy-test #`"sparse-table (,type)" (make-sparse-table type)
              sparse-table-ref sparse-table-set! sparse-table-num-entries
              sparse-table-clear! sparse-table-keys sparse-table-values
              sparse-table-delete! sparse-table-copy %sparse-table-check
              keygen values))

(sptab-heavy 'eqv? values)
(sptab-heavy 'equal? (^k (list k k)))
(sptab-heavy 'string=? (^k (number->string k 36)))

;; The following tests use specifically crafted keys that
;; have the same hash value, so we go through 'chained' leaf path.

(let ([t (make-sparse-table 'equal?)]
      [keys '((0 . 5) (1 . 0) #(0 5) #(1 0))])

  (define (vals tab) (map (cut sparse-table-ref tab <> #f) keys))

  (sparse-table-set! t '(0 . 5) 'a)
  (sparse-table-set! t '(1 . 0) 'b)
  (sparse-table-set! t '#(0 5) 'c)
  (sparse-table-set! t '#(1 0) 'd)

  (test* "key conflicts / ref" '(a b c d) (vals t))
  (test* "key conflicts / set" '((z . a) (z . b) (z . c) (z . d))
         (begin (for-each (cut sparse-table-push! t <> 'z) keys)
                (vals t)))
  (let1 u (sparse-table-copy t)
    (test* "key conflicts / copy 1" '((z . a) (z . b) (z . c) (z . d))
           (vals u))
    (test* "key conflicts / copy 2" '((z z z z) (a b c d))
           (let1 z (map (cut sparse-table-pop! u <>) keys)
             (list z (vals u))))
    (test* "key conflicts / copy (original)" '((z . a) (z . b) (z . c) (z . d))
           (vals t))

    (test* "key conflicts delete 1" '(#f b c d)
           (begin (sparse-table-delete! u '(0 . 5)) (vals u)))
    (test* "key conflicts delete 2" '(#f b #f d)
           (begin (sparse-table-delete! u '#(0 5)) (vals u)))
    (test* "key conflicts delete 3" '(#f b #f #f)
           (begin (sparse-table-delete! u '#(1 0)) (vals u)))
    (test* "key conflicts delete 4" '(#f #f #f #f)
           (begin (sparse-table-delete! u '(1 . 0)) (vals u)))
    ))

(test-end)
