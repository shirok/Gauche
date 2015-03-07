;;
;; testing data.sparse
;;

(use gauche.test)
(use gauche.generator)
(use data.random)
(use util.match)
(use srfi-1)
(use srfi-27)

(test-start "data.sparse")
(use data.sparse)
(test-module 'data.sparse)
(use gauche.collection)

(define (simple-test name obj %ref %set! %exists? %fold key1 key2
                     :optional (val1 'ok) (val2 'okok) (val3 'okokok))
  (test* #"~name basic set!/ref" val1
         (begin (%set! obj (key1) val1)
                (%ref obj (key1))))
  (test* #"~name generic set!/ref" val1
         (begin (set! (~ obj (key1)) val1)
                (~ obj (key1))))
  (test* #"~name referencing nokey" (test-error)
         (%ref obj (key2)))
  (test* #"~name referencing nokey fallback" 'huh?
         (%ref obj (key2) 'huh?))
  (test* #"~name exists?" #t (%exists? obj (key1)))
  (test* #"~name exists?" #f (%exists? obj (key2)))
  (test* #"~name replace" val2
         (begin (%set! obj (key1) val2)
                (%ref obj (key1))))
  (test* #"~name add" val3
         (begin (%set! obj (key2) val3)
                (%ref obj (key2))))
  (test* #"~name ref" val2 (%ref obj (key1)))
  (test* #"~name generic setter" val1
         (begin (set! (%ref obj (key1)) val1)
                (%ref obj (key1))))
  (test* #"~name folder" '(2 #t)
         (let1 lis (%fold obj acons '())
           (list (length lis)
                 (every (^p (eqv? (%ref obj (car p)) (cdr p))) lis))))
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
  (test* #"~name many set!" *data-set-size*
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

  (test* #"~name many numelements" *data-set-size* (%cnt obj))

  (when %check (test* #"~name many check" #t (begin (%check obj) #t)))

  (test* #"~name many ref" *data-set-size*
         (let/cc return
           (hash-table-fold *data-set*
                            (^[k v cnt]
                              (let ([kk (keygen k)]
                                    [vv (valgen v)])
                                (if (equal? (%ref obj kk) vv)
                                  (+ cnt 1)
                                  (return `(error ,cnt ,kk ,vv ,(%ref obj kk))))))
                            0)))
  (test* #"~name keys" *data-set-size*
         (let/cc return
           (let1 tt (make-sparse-table 'equal?)
             (hash-table-for-each *data-set*
                                  (^[k v]
                                    (sparse-table-set! tt (keygen k) #t)))
             (fold (^[k cnt] (if (sparse-table-ref tt k #f)
                               (+ cnt 1)
                               (return `(error ,cnt ,k))))
                   0 (%keys obj)))))
  (test* #"~name values" *data-set-size*
         (let/cc return
           (let1 tt (make-sparse-table 'equal?)
             (hash-table-for-each *data-set*
                                  (^[k v]
                                    (sparse-table-set! tt (valgen v) #t)))
             (fold (^[v cnt] (if (sparse-table-ref tt v #f)
                               (+ cnt 1)
                               (return `(error ,cnt ,v))))
                   0 (%vals obj)))))
  (test* #"~name generic iteration" *data-set-size*
         (fold (^[kv cnt] (if (equal? (%ref obj (car kv)) (cdr kv))
                            (+ cnt 1)
                            cnt))
               0 obj))
  (test* #"~name many copy" (list *data-set-size* #t #t)
         (let* ([new (%copy obj)]
                [keys (%keys new)])
           (list (length keys)
                 (if %check (begin (%check new) #t) #t)
                 (every (^k (equal? (%ref new k) (%ref obj k))) keys))))

  (test* #"~name many clear!" 0 (begin (%clr obj) (%cnt obj)))
  (test* #"~name many ref2" *data-set-size*
         (let/cc return
           (hash-table-fold *data-set*
                            (^[k v cnt]
                              (let ([kk (keygen k)]
                                    [vv (valgen v)])
                                (if (%ref obj kk #f)
                                  (return `(error ,cnt ,kk ,vv ,(%ref obj kk)))
                                  (+ cnt 1))))
                            0)))
  (test* #"~name many delete!" '(#t 0)
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
  (apply simple-test #"sparse-~(or tag \"\")vector" (make-sparse-vector tag)
         sparse-vector-ref sparse-vector-set! sparse-vector-exists?
         sparse-vector-fold
         (const 0) (const 1)
         (if (memq tag '(f16 f32 f64))
           '(3.0 6.0 9.0)
           '(3 6 9))))

(for-each spvec-simple '(#f s8 u8 s16 u16 s32 u32 s64 u64 f16 f32 f64))

(define (spvec-heavy tag valgen)
  (heavy-test #"sparse-~(or tag \"\")vector"
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
  (simple-test #"sparse-table (~type)" (make-sparse-table type)
               sparse-table-ref sparse-table-set! sparse-table-exists?
               sparse-table-fold
               key1 key2))

(sptab-simple 'eq?     (const 'a) (const 'b))
(sptab-simple 'eqv?    (cut / 3) (cut / 2))
(sptab-simple 'equal?  (cut list 1) (cut list 2))
(sptab-simple 'string=? (cut string #\a) (cut string #\b))

(define (sptab-heavy type keygen)
  (heavy-test #"sparse-table (~type)" (make-sparse-table type)
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

;; custom comparator
(let ()
  (define c (make-comparator #t (^[a b] (= (modulo a 3) (modulo b 3))) #f
                             (^x (modulo x 3))))
  (define t (make-sparse-table c))

  (dotimes [x 10] (sparse-table-set! t x (+ x 100)))
  (test* "custom comparator" '((0 . 109) (1 . 107) (2 . 108))
         (sort-by (sparse-table-map t cons) car))
  (test* "sparse-table-comparator" c
         (sparse-table-comparator t)))

(let ([predefs `((eq? . ,eq-comparator)
                 (eqv? . ,eqv-comparator)
                 (equal? . ,equal-comparator)
                 (string=? . ,string-comparator))])
  (test* "predefined comparators" predefs
         (map (^p (cons (car p)
                        ($ sparse-table-comparator
                           $ make-sparse-table (car p))))
              predefs)))
  

;; Default value behavior
(let ()
  (define z (make-sparse-vector #f :default 99))
  (define y (make-sparse-vector #f :default '(a)))

  (test* "sparse vector with default value" 99
         (sparse-vector-ref z (expt 2 1000)))
  (test* "sparse-vector-inc! with default value" '(100 10)
         (begin
           (sparse-vector-inc! z 3 1)
           (sparse-vector-inc! z 5 1 9)
           (list (sparse-vector-ref z 3)
                 (sparse-vector-ref z 5))))
  (test* "sparse-vector-pop! with default value" 'a
         (sparse-vector-pop! y 0))
  (test* "sparse-vector-push! with default value" '(b a)
         (begin
           (sparse-vector-push! y 1 'b)
           (sparse-vector-ref y 1)))
  )

;; sparse matrix----------------------------------------------------
(test-section "sparse-matrix")

(set! (random-data-seed) 42) ; for consistent result

(let ()
  (define data
    (delete-duplicates
     (generator->list
      (gmap cons
            (lists-of 2 (integers$ (if (> (greatest-fixnum) (expt 2 32))
                                     (expt 2 32)
                                     (expt 2 16))))
            (strings-of))
      1000)
     (^[a b] (equal? (car a) (car b)))))

  (test* "set! and ref" (list (length data) #f)
         (let1 mat (make-sparse-matrix)
           (dolist [d data]
             (match-let1 ((x y) . v) d
               (sparse-matrix-set! mat x y v)))
           (list
            (sparse-matrix-num-entries mat)
            (any (^[d]
                   (match-let1 ((x y) . v) d
                     (if (equal? (sparse-matrix-ref mat x y) v)
                       #f
                       (list x y v (sparse-matrix-ref mat x y)))))
                 data))))

  (test* "fold" '(() ())
         (let ([mat (make-sparse-matrix)]
               [tab (make-hash-table 'equal?)]
               [bad '()])
           (dolist [d data]
             (match-let1 ((x y) . v) d
               (sparse-matrix-set! mat x y v)
               (hash-table-put! tab (cons x y) v)))
           ($ sparse-matrix-for-each mat
              (^[x y v]
                (unless (equal? v (hash-table-get tab (cons x y) #f))
                  (push! bad (list x y (hash-table-get tab (cons x y) #f) v)))
                (hash-table-delete! tab (cons x y))))
           (list bad (hash-table->alist tab))))

  (test* "setter, default value" '(a b (z . a) (a . a))
         (let* ([mat (make-sparse-matrix #f :default 'a)]
                [a (sparse-matrix-ref mat 100 200)]
                [_ (set! (sparse-matrix-ref mat 100 200) 'b)]
                [b (sparse-matrix-ref mat 100 200)]
                [_ (sparse-matrix-push! mat 200 100 'z)]
                [za (sparse-matrix-ref mat 200 100)]
                [_ (sparse-matrix-update! mat 300 400
                                          (^x (cons x x)))]
                [aa (sparse-matrix-ref mat 300 400)])
           (list a b za aa)))

  (test* "uniform" '(#f 3 #t 13)
         (let* ([mat (make-sparse-matrix 'u8)]
                [A (sparse-matrix-exists? mat 123 456)]
                [_ (sparse-matrix-set! mat 123 456 3)]
                [a (sparse-matrix-ref mat 123 456)]
                [B (sparse-matrix-exists? mat 123 456)]
                [_ (sparse-matrix-inc! mat 123 456 10)]
                [b (sparse-matrix-ref mat 123 456)])
           (list A a B b)))
  )

(test-end)
