(define-module foo
  (use gauche.uvector)
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
(define-constant *foo-list1* '(a b c d e))
(define-constant *foo-list2* (list 'b 'c 'd 'e))
(define-constant *foo-list3* '(c d e))
(define-constant *foo-vec1* '#(a b c))
(define-constant *foo-vec2* (vector 'a 'b 'c))
(define-constant *foo-uvec1* '#u8(1 2 3))
(define-constant *foo-uvec2* (u8vector 1 2 3))
(define-constant *foo-string1* "abc")
(define-constant *foo-string2* (list->string '(#\a #\b #\c)))

(define (foo-shared-literals)
  `((list1 . ,*foo-list1*)
    (list2 . ,*foo-list2*)
    (list3 . ,*foo-list3*)
    (vec1  . ,*foo-vec1*)
    (vec2  . ,*foo-vec2*)
    (uvec1  . ,*foo-uvec1*)
    (uvec2  . ,*foo-uvec2*)
    (str1  . ,*foo-string1*)
    (str2  . ,*foo-string2*)
    ))

(begin
  (define (foo-begin1) 'begin1)
  (define (foo-begin2) 'begin2)
  )
