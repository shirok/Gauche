(define-module foo
  (use foo.bar1)
  (use foo.bar3)
  (export foo-master foo-literals foo-shared-literals foo-begin1 foo-begin2)
  (include "include/inc1"))
(select-module foo)

(define (foo-master x)
  (list (bar1 x) (bar3 x)))

;; Tests literal generation
(define (foo-literals)
  (include "literals.scm"))

;; Ensure literals are shared if possible
(define *foo-list1* '(a b c d e))
(define *foo-list2* '(b c d e))
(define *foo-list3* '(c d e))
(define *foo-vec1* '#(a b c))
(define *foo-vec2* '#(a b c))
(define *foo-string1* "abc")
(define *foo-string2* "abc")

(define (foo-shared-literals)
  `((list1 . ,*foo-list1*)
    (list2 . ,*foo-list2*)
    (list3 . ,*foo-list3*)
    (vec1  . ,*foo-vec1*)
    (vec2  . ,*foo-vec2*)
    (str1  . ,*foo-string1*)
    (str2  . ,*foo-string2*)
    ))

(begin
  (define (foo-begin1) 'begin1)
  (define (foo-begin2) 'begin2)
  )
