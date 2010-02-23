;;
;; Test reader/writer case (in)sensitivty
;; Must be called with -e "(define *case-fold* #t)" or
;; -e "(define *case-fold* #f)" 
;;

(use gauche.test)

(if *case-fold*
    (test-start "case-insensitive reader/writer")
    (test-start "case-sensitive reader/writer"))

(test* "reader" "abc"
       (symbol->string 'abc))
(test* "reader" (if *case-fold* "abc" "Abc")
       (symbol->string 'Abc))
(test* "reader" (if *case-fold* "abc" "aBc")
       (symbol->string 'aBc))
(test* "reader" "AbC"
       (symbol->string '|AbC|))

(test* "writer" "abc"
       (write-to-string 'abc))
(test* "writer" (if *case-fold* "|Abc|" "Abc")
       (write-to-string '|Abc|))
(test* "writer" (if *case-fold* "|abC|" "abC")
       (write-to-string '|abC|))

(test-end)
