;; Tests generic comparison procedures
;; NB: Type-specific comparison tests are in each type's test suite.

(use gauche.test)

(test-start "comparison procedures")

(test-section "compare")

(let ()
  (define (test-compare* cmp data)
    (dolist [datum data]
      (apply (^[what a b eq]
               (test* #"compare ~what" (if eq '(0 0) '(-1 1))
                      (list (cmp a b)
                            (cmp b a))))
             datum)))

  (test-compare*
   compare
   '(("boolean" #t #t #t)
     ("boolean" #f #f #t)
     ("boolean" #f #t #f)

     ("char" #\a #\a #t)
     ("char" #\A #\a #f)

     ("string" "ab" "ab"  #t)
     ("string" "ab" "abc" #f)
     ("string" "aB" "ab"  #f)

     ("symbol" a a #t)
     ("symbol" A a #f)
     ("symbol" a ab #f)
     ("symbol" a #:a #f)
     ("symbol" #0=#:a #0# #t)
     
     ("number" 1 1 #t)
     ("number" 1 1.0 #t)
     ("number" -1 1 #f)
     ("number" 1 1+i #f)
     ("number" 1-i 1 #f)
     ("number" 1+i 1+2i #f)
     ("number" 1+i 1+i #t)

     ("null"   () () #t)
     ("null"   () a #f)
     ("pair"   (a b c) (a b c) #t)
     ("pair"   (A b c) (a b c) #f)
     ("pair"   (a b)   (a b c) #f)

     ("vector" #(a b c) #(a b c) #t)
     ("vector" #(a B c) #(a b c) #f)
     ("vector" #(a b) #(a b c) #f)
     ("vector" #() #() #t)

     ("uvector" #u8(1 2 3) #u8(1 2 3) #t)
     ("uvector" #u8(1 0 3) #u8(1 2 3) #f)
     ("uvector" #u8(1 2) #u8(1 2 3) #f)
     ("uvector" #u8() #u8() #t)

     ))

  (test-compare*
   (^[a b] (comparator-compare string-ci-comparator a b))
   '(("string-ci" "abc" "ABC" #t)
     ("string-ci" "ABC" "abd" #f)
     ))
  )

;; comparing different types
(define-class <other-type> () ())
(define-method object-compare ((a <other-type>) (b <other-type>)) 0)

(let ()
  ;; NB: We haven't tested gauche.sequence, so no find-index yet.
  (define (precedence x)
    (let loop ([cls (list <null> <pair> <boolean> <char> <string>
                          <symbol> <number> <vector> <u8vector> <s8vector>
                          <u16vector> <s16vector> <u32vector> <s32vector>
                          <u64vector> <s64vector> <f16vector> <f32vector>
                          <f64vector> <hash-table>)]
               [n 0])
      (cond [(null? cls) n]
            [(is-a? x (car cls)) n]
            [else (loop (cdr cls) (+ n 1))])))
  (define (compare-matrix vals)
    (map (^[x] (map (^[y] (list (compare x y) (compare y x))) vals)) vals))
  (define *data*
    `(() (a . b) #t #\a "a" 0 #(a) #u8(0) #s8(0) #u16(0) #s16(0)
      #u32(0) #s32(0) #u64(0) #s64(0) #f16(0) #f32(0) #f64(0)
      ,(make <other-type>)))
  
  (test* "comparing different types"
         (compare-matrix (map precedence *data*))
         (compare-matrix *data*)))

;; comparing different user-defined types
(define-module compare-test-a
  (define-class <compare-test-x> () ())
  (define-class <compare-test-y> () ()))
(define-module compare-test-b
  (define-class <compare-test-x> () ())
  (define-class <compare-test-y> () ()))
(let ([ax (make (with-module compare-test-a <compare-test-x>))]
      [ay (make (with-module compare-test-a <compare-test-y>))]
      [bx (make (with-module compare-test-b <compare-test-x>))]
      [by (make (with-module compare-test-b <compare-test-y>))])
  (test* "comparing different user-defined types"
         '(-1 1 -1 1 1 -1)
         (list (compare ax ay)
               (compare ay ax)
               (compare ax bx)
               (compare bx ax)
               (compare ay bx)
               (compare bx ay))))

(let ()
  (define ca (make-car-comparator string-comparator))
  (define cd (make-cdr-comparator integer-comparator))
  (define ta (make-hash-table ca))
  (define td (make-hash-table cd))
  (define sa '("orange" "apple" "mango" "papaya"))
  (define sd '(29 34 98 1001))
  (define data (append-map (^a (map (^d (cons a d)) sd)) sa))

  (test* "make-car-comparator" (sort sa)
         (begin (dolist [x data]
                  (hash-table-put! ta x #t))
                (sort (map car (hash-table-keys ta)))))

  (test* "make-cdr-comparator" (sort sd)
         (begin (dolist [x data]
                  (hash-table-put! td x #t))
                (sort (map cdr (hash-table-keys td)))))
  )

(let ()
  (define tc (make-tuple-comparator integer-comparator
                                    (make-reverse-comparator real-comparator)
                                    string-comparator))
  (test* "tuple comparator - type-test" '(#t #f #f #f)
         (list (comparator-test-type tc '(3 9.4 "abc"))
               (comparator-test-type tc '(3.1 9.4 "abc"))
               (comparator-test-type tc '(3 a "abc"))
               (comparator-test-type tc '(3 8 abc))))
  (test* "tuple comparator - equality" '(#t #f #f #f)
         (list (comparator-equal? tc '(1 2/3 "abc") '(1 2/3 "abc"))
               (comparator-equal? tc '(1 2/3 "abc") '(2 2/3 "abc"))
               (comparator-equal? tc '(1 2/3 "abc") '(1 1/3 "abc"))
               (comparator-equal? tc '(1 2/3 "abc") '(1 2/3 "qbc"))))
  (test* "tuple comparator - compare" '(0 1 -1 -1 1 1 -1)
         (list (comparator-compare tc '(1 2/3 "abc") '(1 2/3 "abc"))
               (comparator-compare tc '(1 2/3 "abc") '(0 2/3 "abc"))
               (comparator-compare tc '(0 2/3 "abc") '(1 2/3 "abc"))
               (comparator-compare tc '(1 2/3 "abc") '(1 1/3 "abc"))
               (comparator-compare tc '(1 1/3 "abc") '(1 2/3 "abc"))
               (comparator-compare tc '(1 2/3 "abc") '(1 2/3 "aba"))
               (comparator-compare tc '(1 2/3 "aba") '(1 2/3 "abc"))))
  (test* "tuple comparator - hash"
         (fold-right combine-hash-value
                     (comparator-hash string-comparator "abc")
                     (list (comparator-hash integer-comparator 1)
                           (comparator-hash real-comparator 2/3)))
         (comparator-hash tc '(1 2/3 "abc"))))

;; comparator with no comparison proc / no hash proc

(test* "comparator requires at least either equality or comparison"
       (test-error)
       (make-comparator #t #t #f #f))
(test* "comparator equality fallback behavior" #t
       ((comparator-equality-predicate (make-comparator #t #t (^[a b] 0) #f))
        1 2))
(test* "has comparison proc" #t
       (comparator-comparison-procedure? (make-comparator #t eq? compare #f)))
(test* "no comparison proc" #f
       (comparator-comparison-procedure? (make-comparator #t eq? #f #f)))
(test* "comparator fallback behavior" (test-error)
       ((comparator-comparison-procedure (make-comparator #t eq? #f #f))
        'a 'b))
(test* "has hash function" #t
       (comparator-hash-function? (make-comparator #t eq? #f hash)))
(test* "no hash function" #f
       (comparator-hash-function? (make-comparator #t eq? #f #f)))
(test* "hash fallback behavior" (test-error)
       ((comparator-hash-procedure (make-comparator #t eq? #f #f)) 'a))

;; comparing comparators
;; this is undefined in srfi-114; we compare the slots.
(test* "comparing comparator 1" #t
       (equal? (make-comparator #t eq? #f #f)
               (make-comparator #t eq? #f #f)))
(test* "comparing comparator 2" #f
       (equal? (make-comparator #t eq? #f #f)
               (make-comparator #t eqv? #f #f)))
(test* "comparing comparator 3" #t
       (equal? (make-comparator #t #t compare #f)
               (make-comparator #t #t compare #f)))
(test* "comparing comparator 4" #f
       (equal? (make-comparator #t #t compare #f)
               (make-comparator #t #t compare hash)))
(test* "comparing comparator 5" #t
       (let ([chk (^x (or (char? x) (string? x)))]
             [cmp (^[a b] (cond [(char? a)
                                 (if (char? b)
                                   (compare a b)
                                   -1)]
                                [(string? a)
                                 (if (string? b)
                                   1
                                   (compare a b))]
                                [else 0]))] ; dummy
             [hash (^x (if (char? x) (eqv-hash x) (hash x)))])
         (equal? (make-comparator chk equal? cmp hash)
                 (make-comparator chk equal? cmp hash))))
(test* "comparing comparator 6" #t
       (equal? (make-comparator #t equal? #f #f 'yo)
               (make-comparator #t equal? #f #f 'yo)))
(test* "comparing comparator 7" #f
       (equal? (make-comparator #t equal? #f #f)
               (make-comparator #t equal? #f #f 'yo)))
(test* "comparing comparator 8" #f
       (equal? (make-comparator #t equal? #f #f 'bo)
               (make-comparator #t equal? #f #f 'yo)))

(test-end)


