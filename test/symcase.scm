;;
;; Test reader/writer case (in)sensitivty
;; Must be called with -e "(define *case-fold* #t)" or
;; -e "(define *case-fold* #f)" 
;;

;; $Id: symcase.scm,v 1.1 2002-01-12 10:42:47 shirok Exp $

(use gauche.test)

(if *case-fold*
    (test-start "case-insensitive reader/writer")
    (test-start "case-sensitive reader/writer"))

(test "reader" "abc"
      (lambda () (symbol->string 'abc)))
(test "reader" (if *case-fold* "abc" "Abc")
      (lambda () (symbol->string 'Abc)))
(test "reader" (if *case-fold* "abc" "aBc")
      (lambda () (symbol->string 'aBc)))
(test "reader" "AbC"
      (lambda () (symbol->string '|AbC|)))

(test "writer" "abc"
      (lambda () (write-to-string 'abc)))
(test "writer" (if *case-fold* "|Abc|" "Abc")
      (lambda () (write-to-string '|Abc|)))
(test "writer" (if *case-fold* "|abC|" "abC")
      (lambda () (write-to-string '|abC|)))

(test-end)
