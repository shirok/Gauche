;; Tests generic comparison procedures
;; NB: Type-specific comparison tests are in each type's test suite.

(use gauche.test)

(test-start "comparison procedures")

(test-section "compare")

(let ()
  (define (test-compare* data)
    (dolist [datum data]
      (apply (^[what a b eq]
               (test* #"compare ~what" (if eq '(0 0) '(-1 1))
                      (list (compare a b)
                            (compare b a))))
             datum)))

  (test-compare*
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

     )))

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


(test-end)


