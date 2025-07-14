(cond-expand
  (chibi
   (import (chibi test)
           (scheme base)
           (srfi 1)
           (srfi 128)
           (only (srfi 132) list-sort)))
  (else))

(define-record-type Person
    (make-person first-name last-name)
    person?
  (first-name person-first-name)
  (last-name person-last-name))

(define person-name-comparator
  (make-product-comparator
   (make-wrapper-comparator person? person-last-name string-ci-comparator)
   (make-wrapper-comparator person? person-first-name string-ci-comparator)))


(test-group "base cases"

  (test-equal eqv? #t (comparator? comparator-one))
  (test-equal eqv? #t (comparator? comparator-zero))

  (define data `(a 1 "a" (a) #(a) ,(make-person "John" "Doe")))

  (test-equal equal?
    '(#t #t #t #t #t #t)
    (map (lambda (d) (comparator-test-type comparator-one d)) data))
  (test-equal equal?
    '(#t #t #t #t #t #t)
    (map (lambda (d) (comparator-test-type (make-product-comparator) d)) data))

  (test-equal equal?
    '(#f #f #f #f #f #f)
    (map (lambda (d) (comparator-test-type comparator-zero d)) data))
  (test-equal equal?
    '(#f #f #f #f #f #f)
    (map (lambda (d) (comparator-test-type (make-sum-comparator) d)) data))

  (test-equal equal?
    '(#f #f #f #f #f #f)
    (map (lambda (d) (<? comparator-one d d)) data))
  (test-equal equal?
    '(#f #f #f #f #f #f)
    (map (lambda (d) (<? (make-product-comparator) d d)) data))

  (test-equal eqv? #f (comparator-ordered? comparator-zero))
  (test-equal eqv? #f (comparator-ordered? (make-sum-comparator)))

  (test-equal equal?
    '(0 0 0 0 0 0)
    (map (lambda (d) (comparator-hash comparator-one d)) data))
  (test-equal equal?
    '(0 0 0 0 0 0)
    (map (lambda (d) (comparator-hash (make-product-comparator) d)) data))

  (test-equal eqv? #f (comparator-hashable? comparator-zero))
  (test-equal eqv? #f (comparator-hashable? (make-sum-comparator)))
  )

(test-group "simple"
  (test-equal eq?
    #t
    (<? person-name-comparator
        (make-person "John" "Cowan")
        (make-person "Daphne" "Preston-Kendal")))

  (test-equal eq?
    #t
    (>? person-name-comparator
        (make-person "Tom" "Smith")
        (make-person "John" "Smith"))))

(define-record-type Book
    (make-book author title)
    book?
  (author book-author)
  (title book-title))

(define book-comparator
  (make-product-comparator
   (make-wrapper-comparator book? book-author person-name-comparator)
   (make-wrapper-comparator book? book-title string-ci-comparator)))

(define-record-type CD
    (make-cd artist title)
    cd?
  (artist cd-artist)
  (title cd-title))

(define cd-comparator
  (make-product-comparator
   (make-wrapper-comparator cd? cd-artist person-name-comparator)
   (make-wrapper-comparator cd? cd-title string-ci-comparator)))

(define item-comparator
  (make-sum-comparator book-comparator cd-comparator))

(test-group "nested"
  (let* ((beatles (make-person "The" "Beatles"))
         (abbey-road (make-cd beatles "Abbey Road"))
         (deutsche-grammatik
          (make-book (make-person "Jacob" "Grimm") "Deutsche Grammatik"))
         (sonnets (make-book (make-person "William" "Shakespeare") "Sonnets"))
         (mnd (make-book (make-person "William" "Shakespeare")
                         "A Midsummer Night’s Dream"))
         (bob (make-cd (make-person "Bob" "Dylan") "Blonde on Blonde"))
         (revolver (make-cd (make-person "The" "Beatles") "Revolver")))
  (test-equal
      equal?
    (list-sort
     (lambda (a b) (<? item-comparator a b))
     (list abbey-road
           deutsche-grammatik
           sonnets
           mnd
           bob
           revolver))
    (list deutsche-grammatik
          mnd
          sonnets
          abbey-road
          revolver
          bob))))
