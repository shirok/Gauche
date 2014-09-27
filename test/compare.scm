;; Tests generic comparison procedures
;; NB: Type-specific comparison tests are in each type's test suite.

(use gauche.test)

(test-start "comparison procedures")

(test-section "compare")

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

(test-end)


