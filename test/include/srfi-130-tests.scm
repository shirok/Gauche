;;; Copyright (C) William D Clinger (2016).
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests for SRFI 130.
;;;
;;; To run in Larceny or Sagittarius, cd to the directory containing
;;; this file and incant:
;;;
;;;     larceny --r7rs --path . --program srfi-130-test.scm
;;;     sagittarius -r7 -L . srfi-130-test.scm
;;;
;;; Both Larceny and Sagittarius will look for the (srfi 130) library
;;; in the srfi subdirectory.  The implementations contained within
;;; the foof and srfi-130 directories can be tested by renaming those
;;; directories to srfi.
;;;
;;; FIXME: These tests are incomplete because there's  a combinatorial
;;; explosion of possibilities for optional arguments that could be
;;; either indexes or cursors.  Consider string-prefix-length, for
;;; example.  For each test that calls that procedure with all four
;;; optional arguments, there are 16 possible combinations of indexes
;;; and cursors.  Beginning with string-take, the optional arguments
;;; tested are indexes rather than cursors.

;;; Unicode is the main motivation for string cursors, so we ought
;;; to use at least some non-ASCII strings for testing.
;;; Some systems would blow up if this file were to contain non-ASCII
;;; characters, however, so we have to be careful here.

(use gauche.test)
(use srfi-130)
(test-section "srfi-130")
(test-module 'srfi-130)

(cond-expand (full-unicode
              (define ABC
                (list->string (map integer->char
                                   '(#x3b1 #x3b2 #x3b3))))
              (define ABCDEF
                (list->string (map integer->char
                                   '(#x0c0 #x062 #x0c7 #x064 #x0c9 #x066)))))
             (else
              (define ABC "abc")
              (define ABCDEF "abcdef")))
(define EMPTY "")
(define ABC-ASCII "abc")
(define ABCDEF-ASCII "abcdef")
(define ABCDEFFFFOO "abcdeffffoo")

(define-syntax OR
  (syntax-rules (fail eq? eqv? equal? string=? =)
    [(or (= expected expr) (fail 'symbol))
     (test* (symbol->string 'symbol) expected expr)]
    [(or (eq? expected expr) (fail 'symbol))
     (test* (symbol->string 'symbol) expected expr)]
    [(or (eqv? expected expr) (fail 'symbol))
     (test* (symbol->string 'symbol) expected expr)]
    [(or (equal? expected expr) (fail 'symbol))
     (test* (symbol->string 'symbol) expected expr)]
    [(or (string=? expected expr) (fail 'symbol))
     (test* (symbol->string 'symbol) expected expr)]
    [(or expr (fail 'symbol))
     (test* (symbol->string 'symbol) #t expr)]))

;;-----------------------------------------------------------------------
(test-section "Cursor operations")

(OR (string-cursor? (string-index->cursor "" 0))
    (fail 'string-cursor?))

(OR (not (string-cursor? #f))
    (fail 'string-cursor?))

(OR (string-cursor? (string-index->cursor (make-string 10000) 9999))
    (fail 'string-cursor?))

(OR (= 0
       (string-cursor->index EMPTY (string-cursor-start EMPTY)))
    (fail 'string-cursor-start))

(OR (= 0
       (string-cursor->index ABC (string-cursor-start ABC)))
    (fail 'string-cursor-start))

(OR (= 0
       (string-cursor->index EMPTY (string-cursor-end EMPTY)))
    (fail 'string-cursor-end))

(OR (= 3
       (string-cursor->index ABC (string-cursor-end ABC)))
    (fail 'string-cursor-end))

(OR (= 1
       (string-cursor->index ABC (string-cursor-next ABC 0)))
    (fail 'string-cursor-next))

(OR (= 2
       (string-cursor->index ABC (string-cursor-next ABC 1)))
    (fail 'string-cursor-next))

(OR (= 3
       (string-cursor->index ABC (string-cursor-next ABC 2)))
    (fail 'string-cursor-next))

(OR (= 0
       (string-cursor->index ABC (string-cursor-prev ABC 1)))
    (fail 'string-cursor-prev))

(OR (= 1
       (string-cursor->index ABC (string-cursor-prev ABC 2)))
    (fail 'string-cursor-prev))

(OR (= 2
       (string-cursor->index ABC (string-cursor-prev ABC 3)))
    (fail 'string-cursor-prev))

(OR (= 0
       (string-cursor->index ABC (string-cursor-forward ABC 0 0)))
    (fail 'string-cursor-forward))

(OR (= 2
       (string-cursor->index ABC (string-cursor-forward ABC 0 2)))
    (fail 'string-cursor-forward))

(OR (= 3
       (string-cursor->index ABC (string-cursor-forward ABC 1 2)))
    (fail 'string-cursor-forward))

(OR (= 3
       (string-cursor->index ABC (string-cursor-forward ABC 3 0)))
    (fail 'string-cursor-forward))

(OR (= 0
       (string-cursor->index ABC (string-cursor-back ABC 0 0)))
    (fail 'string-cursor-back))

(OR (= 0
       (string-cursor->index ABC (string-cursor-back ABC 2 2)))
    (fail 'string-cursor-back))

(OR (= 1
       (string-cursor->index ABC (string-cursor-back ABC 3 2)))
    (fail 'string-cursor-back))

(OR (= 3
       (string-cursor->index ABC (string-cursor-back ABC 3 0)))
    (fail 'string-cursor-back))


;;-----------------------------------------------------------------------
(test-section "These are supposed to work on both indexes and cursors.")

(OR (string-cursor=? 0 0)
    (fail 'string-cursor=?))

(OR (not (string-cursor=? 0 1))
    (fail 'string-cursor=?))

(OR (not (string-cursor=? 0 2))
    (fail 'string-cursor=?))

(OR (not (string-cursor=? 0 3))
    (fail 'string-cursor=?))

(OR (not (string-cursor=? 1 0))
    (fail 'string-cursor=?))

(OR (string-cursor=? 1 1)
    (fail 'string-cursor=?))

(OR (not (string-cursor=? 1 2))
    (fail 'string-cursor=?))

(OR (not (string-cursor=? 1 3))
    (fail 'string-cursor=?))

(OR (not (string-cursor=? 2 0))
    (fail 'string-cursor=?))

(OR (not (string-cursor=? 2 1))
    (fail 'string-cursor=?))

(OR (string-cursor=? 2 2)
    (fail 'string-cursor=?))

(OR (not (string-cursor=? 2 3))
    (fail 'string-cursor=?))

(OR (not (string-cursor=? 3 0))
    (fail 'string-cursor=?))

(OR (not (string-cursor=? 3 1))
    (fail 'string-cursor=?))

(OR (not (string-cursor=? 3 2))
    (fail 'string-cursor=?))

(OR (string-cursor=? 3 3)
    (fail 'string-cursor=?))


(OR (not (string-cursor<? 0 0))
    (fail 'string-cursor<?))

(OR (string-cursor<? 0 1)
    (fail 'string-cursor<?))

(OR (string-cursor<? 0 2)
    (fail 'string-cursor<?))

(OR (string-cursor<? 0 3)
    (fail 'string-cursor<?))

(OR (not (string-cursor<? 1 0))
    (fail 'string-cursor<?))

(OR (not (string-cursor<? 1 1))
    (fail 'string-cursor<?))

(OR (string-cursor<? 1 2)
    (fail 'string-cursor<?))

(OR (string-cursor<? 1 3)
    (fail 'string-cursor<?))

(OR (not (string-cursor<? 2 0))
    (fail 'string-cursor<?))

(OR (not (string-cursor<? 2 1))
    (fail 'string-cursor<?))

(OR (not (string-cursor<? 2 2))
    (fail 'string-cursor<?))

(OR (string-cursor<? 2 3)
    (fail 'string-cursor<?))

(OR (not (string-cursor<? 3 0))
    (fail 'string-cursor<?))

(OR (not (string-cursor<? 3 1))
    (fail 'string-cursor<?))

(OR (not (string-cursor<? 3 2))
    (fail 'string-cursor<?))

(OR (not (string-cursor<? 3 3))
    (fail 'string-cursor<?))


(OR (not (string-cursor>? 0 0))
    (fail 'string-cursor>?))

(OR (not (string-cursor>? 0 1))
    (fail 'string-cursor>?))

(OR (not (string-cursor>? 0 2))
    (fail 'string-cursor>?))

(OR (not (string-cursor>? 0 3))
    (fail 'string-cursor>?))

(OR (string-cursor>? 1 0)
    (fail 'string-cursor>?))

(OR (not (string-cursor>? 1 1))
    (fail 'string-cursor>?))

(OR (not (string-cursor>? 1 2))
    (fail 'string-cursor>?))

(OR (not (string-cursor>? 1 3))
    (fail 'string-cursor>?))

(OR (string-cursor>? 2 0)
    (fail 'string-cursor>?))

(OR (string-cursor>? 2 1)
    (fail 'string-cursor>?))

(OR (not (string-cursor>? 2 2))
    (fail 'string-cursor>?))

(OR (not (string-cursor>? 2 3))
    (fail 'string-cursor>?))

(OR (string-cursor>? 3 0)
    (fail 'string-cursor>?))

(OR (string-cursor>? 3 1)
    (fail 'string-cursor>?))

(OR (string-cursor>? 3 2)
    (fail 'string-cursor>?))

(OR (not (string-cursor>? 3 3))
    (fail 'string-cursor>?))


(OR (string-cursor<=? 0 0)
    (fail 'string-cursor<=?))

(OR (string-cursor<=? 0 1)
    (fail 'string-cursor<=?))

(OR (string-cursor<=? 0 2)
    (fail 'string-cursor<=?))

(OR (string-cursor<=? 0 3)
    (fail 'string-cursor<=?))

(OR (not (string-cursor<=? 1 0))
    (fail 'string-cursor<=?))

(OR (string-cursor<=? 1 1)
    (fail 'string-cursor<=?))

(OR (string-cursor<=? 1 2)
    (fail 'string-cursor<=?))

(OR (string-cursor<=? 1 3)
    (fail 'string-cursor<=?))

(OR (not (string-cursor<=? 2 0))
    (fail 'string-cursor<=?))

(OR (not (string-cursor<=? 2 1))
    (fail 'string-cursor<=?))

(OR (string-cursor<=? 2 2)
    (fail 'string-cursor<=?))

(OR (string-cursor<=? 2 3)
    (fail 'string-cursor<=?))

(OR (not (string-cursor<=? 3 0))
    (fail 'string-cursor<=?))

(OR (not (string-cursor<=? 3 1))
    (fail 'string-cursor<=?))

(OR (not (string-cursor<=? 3 2))
    (fail 'string-cursor<=?))

(OR (string-cursor<=? 3 3)
    (fail 'string-cursor<=?))


(OR (string-cursor>=? 0 0)
    (fail 'string-cursor>=?))

(OR (not (string-cursor>=? 0 1))
    (fail 'string-cursor>=?))

(OR (not (string-cursor>=? 0 2))
    (fail 'string-cursor>=?))

(OR (not (string-cursor>=? 0 3))
    (fail 'string-cursor>=?))

(OR (string-cursor>=? 1 0)
    (fail 'string-cursor>=?))

(OR (string-cursor>=? 1 1)
    (fail 'string-cursor>=?))

(OR (not (string-cursor>=? 1 2))
    (fail 'string-cursor>=?))

(OR (not (string-cursor>=? 1 3))
    (fail 'string-cursor>=?))

(OR (string-cursor>=? 2 0)
    (fail 'string-cursor>=?))

(OR (string-cursor>=? 2 1)
    (fail 'string-cursor>=?))

(OR (string-cursor>=? 2 2)
    (fail 'string-cursor>=?))

(OR (not (string-cursor>=? 2 3))
    (fail 'string-cursor>=?))

(OR (string-cursor>=? 3 0)
    (fail 'string-cursor>=?))

(OR (string-cursor>=? 3 1)
    (fail 'string-cursor>=?))

(OR (string-cursor>=? 3 2)
    (fail 'string-cursor>=?))

(OR (string-cursor>=? 3 3)
    (fail 'string-cursor>=?))


(OR (string-cursor=? (string-index->cursor ABC 0)
                     (string-index->cursor ABC 0))
    (fail 'string-cursor=?))

(OR (not (string-cursor=? (string-index->cursor ABC 0)
                          (string-index->cursor ABC 1)))
    (fail 'string-cursor=?))

(OR (not (string-cursor=? (string-index->cursor ABC 0)
                          (string-index->cursor ABC 2)))
    (fail 'string-cursor=?))

(OR (not (string-cursor=? (string-index->cursor ABC 0)
                          (string-index->cursor ABC 3)))
    (fail 'string-cursor=?))

(OR (not (string-cursor=? (string-index->cursor ABC 1)
                          (string-index->cursor ABC 0)))
    (fail 'string-cursor=?))

(OR (string-cursor=? (string-index->cursor ABC 1)
                     (string-index->cursor ABC 1))
    (fail 'string-cursor=?))

(OR (not (string-cursor=? (string-index->cursor ABC 1)
                          (string-index->cursor ABC 2)))
    (fail 'string-cursor=?))

(OR (not (string-cursor=? (string-index->cursor ABC 1)
                          (string-index->cursor ABC 3)))
    (fail 'string-cursor=?))

(OR (not (string-cursor=? (string-index->cursor ABC 2)
                          (string-index->cursor ABC 0)))
    (fail 'string-cursor=?))

(OR (not (string-cursor=? (string-index->cursor ABC 2)
                          (string-index->cursor ABC 1)))
    (fail 'string-cursor=?))

(OR (string-cursor=? (string-index->cursor ABC 2)
                     (string-index->cursor ABC 2))
    (fail 'string-cursor=?))

(OR (not (string-cursor=? (string-index->cursor ABC 2)
                          (string-index->cursor ABC 3)))
    (fail 'string-cursor=?))

(OR (not (string-cursor=? (string-index->cursor ABC 3)
                          (string-index->cursor ABC 0)))
    (fail 'string-cursor=?))

(OR (not (string-cursor=? (string-index->cursor ABC 3)
                          (string-index->cursor ABC 1)))
    (fail 'string-cursor=?))

(OR (not (string-cursor=? (string-index->cursor ABC 3)
                          (string-index->cursor ABC 2)))
    (fail 'string-cursor=?))

(OR (string-cursor=? (string-index->cursor ABC 3)
                     (string-index->cursor ABC 3))
    (fail 'string-cursor=?))


(OR (not (string-cursor<? (string-index->cursor ABC 0)
                          (string-index->cursor ABC 0)))
    (fail 'string-cursor<?))

(OR (string-cursor<? (string-index->cursor ABC 0)
                     (string-index->cursor ABC 1))
    (fail 'string-cursor<?))

(OR (string-cursor<? (string-index->cursor ABC 0)
                     (string-index->cursor ABC 2))
    (fail 'string-cursor<?))

(OR (string-cursor<? (string-index->cursor ABC 0)
                     (string-index->cursor ABC 3))
    (fail 'string-cursor<?))

(OR (not (string-cursor<? (string-index->cursor ABC 1)
                          (string-index->cursor ABC 0)))
    (fail 'string-cursor<?))

(OR (not (string-cursor<? (string-index->cursor ABC 1)
                          (string-index->cursor ABC 1)))
    (fail 'string-cursor<?))

(OR (string-cursor<? (string-index->cursor ABC 1)
                     (string-index->cursor ABC 2))
    (fail 'string-cursor<?))

(OR (string-cursor<? (string-index->cursor ABC 1)
                     (string-index->cursor ABC 3))
    (fail 'string-cursor<?))

(OR (not (string-cursor<? (string-index->cursor ABC 2)
                          (string-index->cursor ABC 0)))
    (fail 'string-cursor<?))

(OR (not (string-cursor<? (string-index->cursor ABC 2)
                          (string-index->cursor ABC 1)))
    (fail 'string-cursor<?))

(OR (not (string-cursor<? (string-index->cursor ABC 2)
                          (string-index->cursor ABC 2)))
    (fail 'string-cursor<?))

(OR (string-cursor<? (string-index->cursor ABC 2)
                     (string-index->cursor ABC 3))
    (fail 'string-cursor<?))

(OR (not (string-cursor<? (string-index->cursor ABC 3)
                          (string-index->cursor ABC 0)))
    (fail 'string-cursor<?))

(OR (not (string-cursor<? (string-index->cursor ABC 3)
                          (string-index->cursor ABC 1)))
    (fail 'string-cursor<?))

(OR (not (string-cursor<? (string-index->cursor ABC 3)
                          (string-index->cursor ABC 2)))
    (fail 'string-cursor<?))

(OR (not (string-cursor<? (string-index->cursor ABC 3)
                          (string-index->cursor ABC 3)))
    (fail 'string-cursor<?))


(OR (not (string-cursor>? (string-index->cursor ABC 0)
                          (string-index->cursor ABC 0)))
    (fail 'string-cursor>?))

(OR (not (string-cursor>? (string-index->cursor ABC 0)
                          (string-index->cursor ABC 1)))
    (fail 'string-cursor>?))

(OR (not (string-cursor>? (string-index->cursor ABC 0)
                          (string-index->cursor ABC 2)))
    (fail 'string-cursor>?))

(OR (not (string-cursor>? (string-index->cursor ABC 0)
                          (string-index->cursor ABC 3)))
    (fail 'string-cursor>?))

(OR (string-cursor>? (string-index->cursor ABC 1)
                     (string-index->cursor ABC 0))
    (fail 'string-cursor>?))

(OR (not (string-cursor>? (string-index->cursor ABC 1)
                          (string-index->cursor ABC 1)))
    (fail 'string-cursor>?))

(OR (not (string-cursor>? (string-index->cursor ABC 1)
                          (string-index->cursor ABC 2)))
    (fail 'string-cursor>?))

(OR (not (string-cursor>? (string-index->cursor ABC 1)
                          (string-index->cursor ABC 3)))
    (fail 'string-cursor>?))

(OR (string-cursor>? (string-index->cursor ABC 2)
                     (string-index->cursor ABC 0))
    (fail 'string-cursor>?))

(OR (string-cursor>? (string-index->cursor ABC 2)
                     (string-index->cursor ABC 1))
    (fail 'string-cursor>?))

(OR (not (string-cursor>? (string-index->cursor ABC 2)
                          (string-index->cursor ABC 2)))
    (fail 'string-cursor>?))

(OR (not (string-cursor>? (string-index->cursor ABC 2)
                          (string-index->cursor ABC 3)))
    (fail 'string-cursor>?))

(OR (string-cursor>? (string-index->cursor ABC 3)
                     (string-index->cursor ABC 0))
    (fail 'string-cursor>?))

(OR (string-cursor>? (string-index->cursor ABC 3)
                     (string-index->cursor ABC 1))
    (fail 'string-cursor>?))

(OR (string-cursor>? (string-index->cursor ABC 3)
                     (string-index->cursor ABC 2))
    (fail 'string-cursor>?))

(OR (not (string-cursor>? (string-index->cursor ABC 3)
                          (string-index->cursor ABC 3)))
    (fail 'string-cursor>?))


(OR (string-cursor<=? (string-index->cursor ABC 0)
                      (string-index->cursor ABC 0))
    (fail 'string-cursor<=?))

(OR (string-cursor<=? (string-index->cursor ABC 0)
                      (string-index->cursor ABC 1))
    (fail 'string-cursor<=?))

(OR (string-cursor<=? (string-index->cursor ABC 0)
                      (string-index->cursor ABC 2))
    (fail 'string-cursor<=?))

(OR (string-cursor<=? (string-index->cursor ABC 0)
                      (string-index->cursor ABC 3))
    (fail 'string-cursor<=?))

(OR (not (string-cursor<=? (string-index->cursor ABC 1)
                           (string-index->cursor ABC 0)))
    (fail 'string-cursor<=?))

(OR (string-cursor<=? (string-index->cursor ABC 1)
                      (string-index->cursor ABC 1))
    (fail 'string-cursor<=?))

(OR (string-cursor<=? (string-index->cursor ABC 1)
                      (string-index->cursor ABC 2))
    (fail 'string-cursor<=?))

(OR (string-cursor<=? (string-index->cursor ABC 1)
                      (string-index->cursor ABC 3))
    (fail 'string-cursor<=?))

(OR (not (string-cursor<=? (string-index->cursor ABC 2)
                           (string-index->cursor ABC 0)))
    (fail 'string-cursor<=?))

(OR (not (string-cursor<=? (string-index->cursor ABC 2)
                           (string-index->cursor ABC 1)))
    (fail 'string-cursor<=?))

(OR (string-cursor<=? (string-index->cursor ABC 2)
                      (string-index->cursor ABC 2))
    (fail 'string-cursor<=?))

(OR (string-cursor<=? (string-index->cursor ABC 2)
                      (string-index->cursor ABC 3))
    (fail 'string-cursor<=?))

(OR (not (string-cursor<=? (string-index->cursor ABC 3)
                           (string-index->cursor ABC 0)))
    (fail 'string-cursor<=?))

(OR (not (string-cursor<=? (string-index->cursor ABC 3)
                           (string-index->cursor ABC 1)))
    (fail 'string-cursor<=?))

(OR (not (string-cursor<=? (string-index->cursor ABC 3)
                           (string-index->cursor ABC 2)))
    (fail 'string-cursor<=?))

(OR (string-cursor<=? (string-index->cursor ABC 3)
                      (string-index->cursor ABC 3))
    (fail 'string-cursor<=?))


(OR (string-cursor>=? (string-index->cursor ABC 0)
                      (string-index->cursor ABC 0))
    (fail 'string-cursor>=?))

(OR (not (string-cursor>=? (string-index->cursor ABC 0)
                           (string-index->cursor ABC 1)))
    (fail 'string-cursor>=?))

(OR (not (string-cursor>=? (string-index->cursor ABC 0)
                           (string-index->cursor ABC 2)))
    (fail 'string-cursor>=?))

(OR (not (string-cursor>=? (string-index->cursor ABC 0)
                           (string-index->cursor ABC 3)))
    (fail 'string-cursor>=?))

(OR (string-cursor>=? (string-index->cursor ABC 1)
                      (string-index->cursor ABC 0))
    (fail 'string-cursor>=?))

(OR (string-cursor>=? (string-index->cursor ABC 1)
                      (string-index->cursor ABC 1))
    (fail 'string-cursor>=?))

(OR (not (string-cursor>=? (string-index->cursor ABC 1)
                           (string-index->cursor ABC 2)))
    (fail 'string-cursor>=?))

(OR (not (string-cursor>=? (string-index->cursor ABC 1)
                           (string-index->cursor ABC 3)))
    (fail 'string-cursor>=?))

(OR (string-cursor>=? (string-index->cursor ABC 2)
                      (string-index->cursor ABC 0))
    (fail 'string-cursor>=?))

(OR (string-cursor>=? (string-index->cursor ABC 2)
                      (string-index->cursor ABC 1))
    (fail 'string-cursor>=?))

(OR (string-cursor>=? (string-index->cursor ABC 2)
                      (string-index->cursor ABC 2))
    (fail 'string-cursor>=?))

(OR (not (string-cursor>=? (string-index->cursor ABC 2)
                           (string-index->cursor ABC 3)))
    (fail 'string-cursor>=?))

(OR (string-cursor>=? (string-index->cursor ABC 3)
                      (string-index->cursor ABC 0))
    (fail 'string-cursor>=?))

(OR (string-cursor>=? (string-index->cursor ABC 3)
                      (string-index->cursor ABC 1))
    (fail 'string-cursor>=?))

(OR (string-cursor>=? (string-index->cursor ABC 3)
                      (string-index->cursor ABC 2))
    (fail 'string-cursor>=?))

(OR (string-cursor>=? (string-index->cursor ABC 3)
                      (string-index->cursor ABC 3))
    (fail 'string-cursor>=?))


(OR (= 0 (string-cursor-diff ABC
                             (string-index->cursor ABC 0)
                             (string-index->cursor ABC 0)))
    (fail 'string-cursor-diff))

(OR (= 3 (string-cursor-diff ABC
                             (string-index->cursor ABC 0)
                             (string-index->cursor ABC 3)))
    (fail 'string-cursor-diff))

(OR (= 0 (string-cursor->index EMPTY (string-index->cursor EMPTY 0)))
    (fail 'string-cursor->index))

(OR (= 3 (string-cursor->index ABC (string-index->cursor ABC 3)))
    (fail 'string-cursor->index))

(OR (= 0 (string-cursor->index EMPTY (string-index->cursor EMPTY 0)))
    (fail 'string-index->cursor))

(OR (= 3 (string-cursor->index ABC (string-index->cursor ABC 3)))
    (fail 'string-index->cursor))

;;-----------------------------------------------------------------------
(test-section "Predicates")

(OR (string-null? "")
    (fail 'string-null))

(OR (not (string-null? "abc"))
    (fail 'string-null))

(OR (eqv? #t (string-every (lambda (c) (if (char? c) c #f)) ""))
    (fail 'string-every))

(OR (eqv? #\c (string-every (lambda (c) (if (char? c) c #f)) "abc"))
    (fail 'string-every))

(OR (eqv? #f (string-every (lambda (c) (if (char>? c #\b) c #f)) "abc"))
    (fail 'string-every))

(OR (eqv? #\c (string-every (lambda (c) (if (char>? c #\b) c #f)) "abc" 2))
    (fail 'string-every))

(test* "string-every"
       #\c
       (string-every (lambda (c) (if (char>? c #\b) c #f))
                     ABC-ASCII
                     (string-index->cursor ABC-ASCII 2)))

(OR (eqv? #t (string-every (lambda (c) (if (char>? c #\b) c #f)) "abc" 1 1))
    (fail 'string-every))

(test* "string-every"
       #t
       (string-every (lambda (c) (if (char>? c #\b) c #f))
                     ABC-ASCII
                     (string-index->cursor ABC-ASCII 1)
                     (string-index->cursor ABC-ASCII 1)))

(OR (eqv? #f (string-any (lambda (c) (if (char? c) c #f)) ""))
    (fail 'string-any))

(OR (eqv? #\a (string-any (lambda (c) (if (char? c) c #f)) "abc"))
    (fail 'string-any))

(OR (eqv? #\c (string-any (lambda (c) (if (char>? c #\b) c #f)) "abc"))
    (fail 'string-any))

(OR (eqv? #\c (string-any (lambda (c) (if (char>? c #\b) c #f)) "abc" 2))
    (fail 'string-any))

(test* "string-any"
       #\c
       (string-any (lambda (c) (if (char>? c #\b) c #f))
                   ABC-ASCII
                   (string-index->cursor ABC-ASCII 2)))

(OR (eqv? #f (string-any (lambda (c) (if (char>? c #\b) c #f)) "abc" 0 2))
    (fail 'string-any))

(test* "string-any"
       #f
       (string-any (lambda (c) (if (char>? c #\b) c #f))
                   ABC-ASCII
                   (string-cursor-start ABC-ASCII)
                   (string-index->cursor ABC-ASCII 2)))

;;-----------------------------------------------------------------------
(test-section "Constructors")

(OR (equal? ""
            (string-tabulate (lambda (i)
                               (integer->char (+ i (char->integer #\a))))
                             0))
    (fail 'string-tabulate))

(OR (equal? "abc"
            (string-tabulate (lambda (i)
                               (integer->char (+ i (char->integer #\a))))
                             3))
    (fail 'string-tabulate))

(OR (equal? "abc"
            (let ((p (open-input-string "abc")))
              (string-unfold eof-object?
                             values
                             (lambda (x) (read-char p))
                             (read-char p))))
    (fail 'string-unfold))

(OR (equal? "" (string-unfold null? car cdr '()))
    (fail 'string-unfold))

(OR (equal? "abc" (string-unfold null? car cdr (string->list "abc")))
    (fail 'string-unfold))

(OR (equal? "def" (string-unfold null? car cdr '() "def"))
    (fail 'string-unfold))

(OR (equal? "defabcG"
            (string-unfold null?
                           car
                           cdr
                           (string->list "abc")
                           "def"
                           (lambda (x) (and (null? x) "G"))))
    (fail 'string-unfold))

(OR (equal? "" (string-unfold-right null? car cdr '()))
    (fail 'string-unfold-right))

(OR (equal? "cba" (string-unfold-right null? car cdr (string->list "abc")))
    (fail 'string-unfold-right))

(OR (equal? "def" (string-unfold-right null? car cdr '() "def"))
    (fail 'string-unfold-right))

(OR (equal? "Gcbadef"
            (string-unfold-right null?
                                 car
                                 cdr
                                 (string->list "abc")
                                 "def"
                                 (lambda (x) (and (null? x) "G"))))
    (fail 'string-unfold-right))

;;-----------------------------------------------------------------------
(test-section "Conversion")

(OR (equal? '() (string->list/cursors ""))
    (fail 'string->list/cursors))

(OR (equal? '() (string->list/cursors "" 0))
    (fail 'string->list/cursors))

(OR (equal? '() (string->list/cursors "" 0 0))
    (fail 'string->list/cursors))

(OR (equal? '(#\a #\b #\c) (string->list/cursors "abc"))
    (fail 'string->list/cursors))

(OR (equal? '() (string->list/cursors "abc" 3))
    (fail 'string->list/cursors))

(OR (equal? '(#\b #\c) (string->list/cursors "abc" 1 3))
    (fail 'string->list/cursors))

(OR (equal? '(#\b #\c)
            (string->list/cursors ABC-ASCII
                                  (string-index->cursor ABC-ASCII 1)
                                  (string-index->cursor ABC-ASCII 3)))
    (fail 'string->list/cursors))

(OR (equal? '#() (string->vector/cursors ""))
    (fail 'string->vector/cursors))

(OR (equal? '#() (string->vector/cursors "" 0))
    (fail 'string->vector/cursors))

(OR (equal? '#() (string->vector/cursors "" 0 0))
    (fail 'string->vector/cursors))

(OR (equal? '#(#\a #\b #\c) (string->vector/cursors "abc"))
    (fail 'string->vector/cursors))

(OR (equal? '#() (string->vector/cursors "abc" 3))
    (fail 'string->vector/cursors))

(OR (equal? '#(#\b #\c) (string->vector/cursors "abc" 1 3))
    (fail 'string->vector/cursors))

(OR (equal? '#(#\b #\c)
            (string->vector/cursors ABC-ASCII
                                    (string-index->cursor ABC-ASCII 1)
                                    (string-index->cursor ABC-ASCII 3)))
    (fail 'string->vector/cursors))

(OR (equal? "" (reverse-list->string '()))
    (fail 'reverse-list->string))

(OR (equal? "cba" (reverse-list->string '(#\a #\b #\c)))
    (fail 'reverse-list->string))

(OR (equal? "" (string-join '()))
    (fail 'string-join))

(OR (equal? " ab cd  e f "
            (string-join '("" "ab" "cd" "" "e" "f" "")))
    (fail 'string-join))

(OR (equal? "" (string-join '() ""))
    (fail 'string-join))

(OR (equal? "abcdef"
            (string-join '("" "ab" "cd" "" "e" "f" "") ""))
    (fail 'string-join))

(OR (equal? "" (string-join '() "xyz"))
    (fail 'string-join))

(OR (equal? "xyzabxyzcdxyzxyzexyzfxyz"
            (string-join '("" "ab" "cd" "" "e" "f" "") "xyz"))
    (fail 'string-join))

(OR (equal? "" (string-join '() "" 'infix))
    (fail 'string-join))

(OR (equal? "abcdef"
            (string-join '("" "ab" "cd" "" "e" "f" "") "" 'infix))
    (fail 'string-join))

(OR (equal? "" (string-join '() "xyz" 'infix))
    (fail 'string-join))

(OR (equal? "xyzabxyzcdxyzxyzexyzfxyz"
            (string-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'infix))
    (fail 'string-join))

(OR (equal? 'horror
            (guard (exn (#t 'horror))
             (string-join '() "" 'strict-infix)))
    (fail 'string-join))

(OR (equal? "abcdef"
            (string-join '("" "ab" "cd" "" "e" "f" "") "" 'strict-infix))
    (fail 'string-join))

(OR (equal? 'wham
            (guard (exn (else 'wham))
             (string-join '() "xyz" 'strict-infix)))
    (fail 'string-join))

(OR (equal? "xyzabxyzcdxyzxyzexyzfxyz"
            (string-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'strict-infix))
    (fail 'string-join))

(OR (equal? "" (string-join '() "" 'suffix))
    (fail 'string-join))

(OR (equal? "abcdef"
            (string-join '("" "ab" "cd" "" "e" "f" "") "" 'suffix))
    (fail 'string-join))

(OR (equal? "" (string-join '() "xyz" 'suffix))
    (fail 'string-join))

(OR (equal? "xyzabxyzcdxyzxyzexyzfxyzxyz"
            (string-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'suffix))
    (fail 'string-join))

(OR (equal? "" (string-join '() "" 'prefix))
    (fail 'string-join))

(OR (equal? "abcdef"
            (string-join '("" "ab" "cd" "" "e" "f" "") "" 'prefix))
    (fail 'string-join))

(OR (equal? "" (string-join '() "xyz" 'prefix))
    (fail 'string-join))

(OR (equal? "xyzxyzabxyzcdxyzxyzexyzfxyz"
            (string-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'prefix))
    (fail 'string-join))

;;-----------------------------------------------------------------------
(test-section "Selection")

(OR (char=? #\a (string-ref/cursor "abc" 0))
    (fail 'string-ref/cursor))

(OR (char=? #\c (string-ref/cursor "abc" 2))
    (fail 'string-ref/cursor))

(OR (char=? #\a (string-ref/cursor ABC-ASCII (string-index->cursor ABC-ASCII 0)))
    (fail 'string-ref/cursor))

(OR (char=? #\c (string-ref/cursor ABC-ASCII (string-index->cursor ABC-ASCII 2)))
    (fail 'string-ref/cursor))

(OR (string=? "" (substring/cursors "" 0 0))
    (fail 'substring/cursors))

(OR (string=? "" (substring/cursors "abc" 0 0))
    (fail 'substring/cursors))

(OR (string=? "" (substring/cursors "abc" 3 3))
    (fail 'substring/cursors))

(OR (string=? ABC (substring/cursors ABC 0 3))
    (fail 'substring/cursors))

(OR (string=? ABC
              (substring/cursors ABC
                                 (string-index->cursor ABC 0)
                                 (string-index->cursor ABC 3)))
    (fail 'substring/cursors))

(OR (string=? "b" (substring/cursors "abc" 1 2))
    (fail 'substring/cursors))

(OR (string=? "" (string-copy/cursors ""))
    (fail 'string-copy/cursors))

(OR (string=? "abc" (string-copy/cursors "abc"))
    (fail 'string-copy/cursors))

(OR (string=? "" (string-copy/cursors "abc" 3))
    (fail 'string-copy/cursors))

(OR (string=? "c" (string-copy/cursors "abc" 2))
    (fail 'string-copy/cursors))

(OR (string=? "abc" (string-copy/cursors "abc" 0))
    (fail 'string-copy/cursors))

(OR (string=? "b" (string-copy/cursors "abc" 1 2))
    (fail 'string-copy/cursors))

(OR (string=? "" (string-copy/cursors "" 0 0))
    (fail 'string-copy/cursors))

(OR (string=? "" (string-copy/cursors "abc" 0 0))
    (fail 'string-copy/cursors))

(OR (string=? "" (string-copy/cursors "abc" 3 3))
    (fail 'string-copy/cursors))

(OR (string=? "abc" (string-copy/cursors "abc" 0 3))
    (fail 'string-copy/cursors))

(OR (string=? "b" (string-copy/cursors "abc" 1 2))
    (fail 'string-copy/cursors))

(OR (string=? (substring ABC 1 2)
              (string-copy/cursors ABC
                                   (string-index->cursor ABC 1)
                                   (string-index->cursor ABC 2)))
    (fail 'string-copy/cursors))

(OR (string=? "" (string-take "" 0))
    (fail 'string-take))

(OR (string=? "" (string-take "abcdef" 0))
    (fail 'string-take))

(OR (string=? "ab" (string-take "abcdef" 2))
    (fail 'string-take))

(OR (string=? "" (string-drop "" 0))
    (fail 'string-drop))

(OR (string=? "abcdef" (string-drop "abcdef" 0))
    (fail 'string-drop))

(OR (string=? "cdef" (string-drop "abcdef" 2))
    (fail 'string-drop))

(OR (string=? "" (string-take-right "" 0))
    (fail 'string-take-right))

(OR (string=? "" (string-take-right "abcdef" 0))
    (fail 'string-take-right))

(OR (string=? "ef" (string-take-right "abcdef" 2))
    (fail 'string-take-right))

(OR (string=? "" (string-drop-right "" 0))
    (fail 'string-drop-right))

(OR (string=? "abcdef" (string-drop-right "abcdef" 0))
    (fail 'string-drop-right))

(OR (string=? "abcd" (string-drop-right "abcdef" 2))
    (fail 'string-drop-right))

(OR (string=? "" (string-pad "" 0))
    (fail 'string-pad))

(OR (string=? "     " (string-pad "" 5))
    (fail 'string-pad))

(OR (string=? "  325" (string-pad "325" 5))
    (fail 'string-pad))

(OR (string=? "71325" (string-pad "71325" 5))
    (fail 'string-pad))

(OR (string=? "71325" (string-pad "8871325" 5))
    (fail 'string-pad))

(OR (string=? "" (string-pad "" 0 #\*))
    (fail 'string-pad))

(OR (string=? "*****" (string-pad "" 5 #\*))
    (fail 'string-pad))

(OR (string=? "**325" (string-pad "325" 5 #\*))
    (fail 'string-pad))

(OR (string=? "71325" (string-pad "71325" 5 #\*))
    (fail 'string-pad))

(OR (string=? "71325" (string-pad "8871325" 5 #\*))
    (fail 'string-pad))

(OR (string=? "" (string-pad "" 0 #\* 0))
    (fail 'string-pad))

(test* "string-pad" "" (string-pad EMPTY 0 #\* (string-cursor-start EMPTY)))

(OR (string=? "*****" (string-pad "" 5 #\* 0))
    (fail 'string-pad))

(OR (string=? "**325" (string-pad "325" 5 #\* 0))
    (fail 'string-pad))

(OR (string=? "71325" (string-pad "71325" 5 #\* 0))
    (fail 'string-pad))

(OR (string=? "71325" (string-pad "8871325" 5 #\* 0))
    (fail 'string-pad))

(OR (string=? "***25" (string-pad "325" 5 #\* 1))
    (fail 'string-pad))

(test* "string-pad "
       "***25"
       (let ([s "325"])
         (string-pad s 5 #\* (string-index->cursor s 1))))

(OR (string=? "*1325" (string-pad "71325" 5 #\* 1))
    (fail 'string-pad))

(OR (string=? "71325" (string-pad "8871325" 5 #\* 1))
    (fail 'string-pad))

(OR (string=? "" (string-pad "" 0 #\* 0 0))
    (fail 'string-pad))

(OR (string=? "*****" (string-pad "" 5 #\* 0 0))
    (fail 'string-pad))

(OR (string=? "**325" (string-pad "325" 5 #\* 0 3))
    (fail 'string-pad))

(test* "string-pad"
       "**325"
       (let ([s "325"])
         (string-pad s 5 #\* (string-cursor-start s) (string-cursor-end s))))

(OR (string=? "**713" (string-pad "71325" 5 #\* 0 3))
    (fail 'string-pad))

(OR (string=? "**887" (string-pad "8871325" 5 #\* 0 3))
    (fail 'string-pad))

(OR (string=? "***25" (string-pad "325" 5 #\* 1 3))
    (fail 'string-pad))

(OR (string=? "**132" (string-pad "71325" 5 #\* 1 4))
    (fail 'string-pad))

(OR (string=? "*8713" (string-pad "8871325" 5 #\* 1 5))
    (fail 'string-pad))

(OR (string=? "" (string-pad-right "" 0))
    (fail 'string-pad-right))

(OR (string=? "     " (string-pad-right "" 5))
    (fail 'string-pad-right))

(OR (string=? "325  " (string-pad-right "325" 5))
    (fail 'string-pad-right))

(OR (string=? "71325" (string-pad-right "71325" 5))
    (fail 'string-pad-right))

(OR (string=? "88713" (string-pad-right "8871325" 5))
    (fail 'string-pad-right))

(OR (string=? "" (string-pad-right "" 0 #\*))
    (fail 'string-pad-right))

(OR (string=? "*****" (string-pad-right "" 5 #\*))
    (fail 'string-pad-right))

(OR (string=? "325**" (string-pad-right "325" 5 #\*))
    (fail 'string-pad-right))

(OR (string=? "71325" (string-pad-right "71325" 5 #\*))
    (fail 'string-pad-right))

(OR (string=? "88713" (string-pad-right "8871325" 5 #\*))
    (fail 'string-pad-right))

(OR (string=? "" (string-pad-right "" 0 #\* 0))
    (fail 'string-pad-right))

(OR (string=? "*****" (string-pad-right "" 5 #\* 0))
    (fail 'string-pad-right))

(OR (string=? "325**" (string-pad-right "325" 5 #\* 0))
    (fail 'string-pad-right))

(OR (string=? "71325" (string-pad-right "71325" 5 #\* 0))
    (fail 'string-pad-right))

(OR (string=? "88713" (string-pad-right "8871325" 5 #\* 0))
    (fail 'string-pad-right))

(OR (string=? "25***" (string-pad-right "325" 5 #\* 1))
    (fail 'string-pad-right))

(OR (string=? "1325*" (string-pad-right "71325" 5 #\* 1))
    (fail 'string-pad-right))

(OR (string=? "87132" (string-pad-right "8871325" 5 #\* 1))
    (fail 'string-pad-right))

(OR (string=? "" (string-pad-right "" 0 #\* 0 0))
    (fail 'string-pad-right))

(OR (string=? "*****" (string-pad-right "" 5 #\* 0 0))
    (fail 'string-pad-right))

(OR (string=? "325**" (string-pad-right "325" 5 #\* 0 3))
    (fail 'string-pad-right))

(OR (string=? "713**" (string-pad-right "71325" 5 #\* 0 3))
    (fail 'string-pad-right))

(OR (string=? "887**" (string-pad-right "8871325" 5 #\* 0 3))
    (fail 'string-pad-right))

(OR (string=? "25***" (string-pad-right "325" 5 #\* 1 3))
    (fail 'string-pad-right))

(OR (string=? "132**" (string-pad-right "71325" 5 #\* 1 4))
    (fail 'string-pad-right))

(OR (string=? "8713*" (string-pad-right "8871325" 5 #\* 1 5))
    (fail 'string-pad-right))


(OR (string=? "" (string-trim ""))
    (fail 'string-trim))

(OR (string=? "a  b  c  " (string-trim "  a  b  c  "))
    (fail 'string-trim))

(OR (string=? "" (string-trim "" char-whitespace?))
    (fail 'string-trim))

(OR (string=? "a  b  c  " (string-trim "  a  b  c  " char-whitespace?))
    (fail 'string-trim))

(OR (string=? "" (string-trim "  a  b  c  " char?))
    (fail 'string-trim))

(OR (string=? "" (string-trim "" char-whitespace? 0))
    (fail 'string-trim))

(test* "string-trim" "" (string-trim EMPTY char-whitespace? (string-cursor-start EMPTY)))

(OR (string=? "a  b  c  " (string-trim "  a  b  c  " char-whitespace? 0))
    (fail 'string-trim))

(OR (string=? "" (string-trim "  a  b  c  " char? 0))
    (fail 'string-trim))

(OR (string=? "b  c  " (string-trim "  a  b  c  " char-whitespace? 3))
    (fail 'string-trim))

(OR (string=? "" (string-trim "  a  b  c  " char? 3))
    (fail 'string-trim))

(OR (string=? "" (string-trim "  a  b  c  " char? 0 11))
    (fail 'string-trim))

(test* "string-trim"
       ""
       (let ([s "  a  b  c  "])
         (string-trim s
                      char?
                      (string-cursor-start s)
                      (string-index->cursor s 11))))

(OR (string=? "b  c  " (string-trim "  a  b  c  " char-whitespace? 3 11))
    (fail 'string-trim))

(OR (string=? "" (string-trim "  a  b  c  " char? 3 11))
    (fail 'string-trim))

(OR (string=? "" (string-trim "  a  b  c  " char? 0 8))
    (fail 'string-trim))

(OR (string=? "b  " (string-trim "  a  b  c  " char-whitespace? 3 8))
    (fail 'string-trim))

(OR (string=? "" (string-trim "  a  b  c  " char? 3 8))
    (fail 'string-trim))


(OR (string=? "" (string-trim-right ""))
    (fail 'string-trim-right))

(OR (string=? "  a  b  c" (string-trim-right "  a  b  c  "))
    (fail 'string-trim-right))

(OR (string=? "" (string-trim-right "" char-whitespace?))
    (fail 'string-trim-right))

(OR (string=? "  a  b  c" (string-trim-right "  a  b  c  " char-whitespace?))
    (fail 'string-trim-right))

(OR (string=? "" (string-trim-right "  a  b  c  " char?))
    (fail 'string-trim-right))

(OR (string=? "" (string-trim-right "" char-whitespace? 0))
    (fail 'string-trim-right))

(OR (string=? "  a  b  c" (string-trim-right "  a  b  c  " char-whitespace? 0))
    (fail 'string-trim-right))

(OR (string=? "" (string-trim-right "  a  b  c  " char? 0))
    (fail 'string-trim-right))

(OR (string=? "  b  c" (string-trim-right "  a  b  c  " char-whitespace? 3))
    (fail 'string-trim-right))

(OR (string=? "" (string-trim-right "  a  b  c  " char? 3))
    (fail 'string-trim-right))

(OR (string=? "" (string-trim-right "  a  b  c  " char? 0 11))
    (fail 'string-trim-right))

(OR (string=? "  b  c" (string-trim-right "  a  b  c  " char-whitespace? 3 11))
    (fail 'string-trim-right))

(OR (string=? "" (string-trim-right "  a  b  c  " char? 3 11))
    (fail 'string-trim-right))

(OR (string=? "" (string-trim-right "  a  b  c  " char? 0 8))
    (fail 'string-trim-right))

(OR (string=? "  b" (string-trim-right "  a  b  c  " char-whitespace? 3 8))
    (fail 'string-trim-right))

(OR (string=? "" (string-trim-right "  a  b  c  " char? 3 8))
    (fail 'string-trim-right))


(OR (string=? "" (string-trim-both ""))
    (fail 'string-trim-both))

(OR (string=? "a  b  c" (string-trim-both "  a  b  c  "))
    (fail 'string-trim-both))

(OR (string=? "" (string-trim-both "" char-whitespace?))
    (fail 'string-trim-both))

(OR (string=? "a  b  c" (string-trim-both "  a  b  c  " char-whitespace?))
    (fail 'string-trim-both))

(OR (string=? "" (string-trim-both "  a  b  c  " char?))
    (fail 'string-trim-both))

(OR (string=? "" (string-trim-both "" char-whitespace? 0))
    (fail 'string-trim-both))

(OR (string=? "a  b  c" (string-trim-both "  a  b  c  " char-whitespace? 0))
    (fail 'string-trim-both))

(OR (string=? "" (string-trim-both "  a  b  c  " char? 0))
    (fail 'string-trim-both))

(OR (string=? "b  c" (string-trim-both "  a  b  c  " char-whitespace? 3))
    (fail 'string-trim-both))

(OR (string=? "" (string-trim-both "  a  b  c  " char? 3))
    (fail 'string-trim-both))

(OR (string=? "" (string-trim-both "  a  b  c  " char? 0 11))
    (fail 'string-trim-both))

(OR (string=? "b  c" (string-trim-both "  a  b  c  " char-whitespace? 3 11))
    (fail 'string-trim-both))

(OR (string=? "" (string-trim-both "  a  b  c  " char? 3 11))
    (fail 'string-trim-both))

(OR (string=? "" (string-trim-both "  a  b  c  " char? 0 8))
    (fail 'string-trim-both))

(OR (string=? "b" (string-trim-both "  a  b  c  " char-whitespace? 3 8))
    (fail 'string-trim-both))

(OR (string=? "" (string-trim-both "  a  b  c  " char? 3 8))
    (fail 'string-trim-both))


(OR (= 0 (string-prefix-length "" ""))
    (fail 'string-prefix-length))

(OR (= 0 (string-prefix-length "" "aabbccddee"))
    (fail 'string-prefix-length))

(OR (= 0 (string-prefix-length "aisle" ""))
    (fail 'string-prefix-length))

(OR (= 0 (string-prefix-length "" "aabbccddee"))
    (fail 'string-prefix-length))

(OR (= 1 (string-prefix-length "aisle" "aabbccddee"))
    (fail 'string-prefix-length))

(OR (= 0 (string-prefix-length "bail" "aabbccddee"))
    (fail 'string-prefix-length))

(OR (= 4 (string-prefix-length "prefix" "preface"))
    (fail 'string-prefix-length))

(OR (= 0 (string-prefix-length "" "" 0))
    (fail 'string-prefix-length))

(OR (= 0 (string-prefix-length "" "aabbccddee" 0))
    (fail 'string-prefix-length))

(OR (= 0 (string-prefix-length "aisle" "" 0))
    (fail 'string-prefix-length))

(OR (= 1 (string-prefix-length "aisle" "aabbccddee" 0))
    (fail 'string-prefix-length))

(OR (= 0 (string-prefix-length "bail" "aabbccddee" 0))
    (fail 'string-prefix-length))

(OR (= 4 (string-prefix-length "prefix" "preface" 0))
    (fail 'string-prefix-length))

(OR (= 0 (string-prefix-length "aisle" "" 1))
    (fail 'string-prefix-length))

(OR (= 0 (string-prefix-length "aisle" "aabbccddee" 1))
    (fail 'string-prefix-length))

(OR (= 1 (string-prefix-length "bail" "aabbccddee" 1))
    (fail 'string-prefix-length))

(OR (= 0 (string-prefix-length "prefix" "preface" 1))
    (fail 'string-prefix-length))

(OR (= 0 (string-prefix-length "" "" 0 0))
    (fail 'string-prefix-length))

(OR (= 0 (string-prefix-length "" "aabbccddee" 0 0))
    (fail 'string-prefix-length))

(OR (= 0 (string-prefix-length "aisle" "" 0 4))
    (fail 'string-prefix-length))

(OR (= 1 (string-prefix-length "aisle" "aabbccddee" 0 4))
    (fail 'string-prefix-length))

(OR (= 0 (string-prefix-length "bail" "aabbccddee" 0 1))
    (fail 'string-prefix-length))

(OR (= 0 (string-prefix-length "aisle" "" 1 4))
    (fail 'string-prefix-length))

(OR (= 0 (string-prefix-length "aisle" "aabbccddee" 1 4))
    (fail 'string-prefix-length))

(OR (= 1 (string-prefix-length "bail" "aabbccddee" 1 4))
    (fail 'string-prefix-length))

(OR (= 0 (string-prefix-length "prefix" "preface" 1 5))
    (fail 'string-prefix-length))

(OR (= 0 (string-prefix-length "" "" 0 0 0))
    (fail 'string-prefix-length))

(OR (= 0 (string-prefix-length "" "aabbccddee" 0 0 0))
    (fail 'string-prefix-length))

(OR (= 0 (string-prefix-length "aisle" "" 0 4 0))
    (fail 'string-prefix-length))

(OR (= 0 (string-prefix-length "aisle" "aabbccddee" 0 4 2))
    (fail 'string-prefix-length))

(OR (= 1 (string-prefix-length "bail" "aabbccddee" 0 1 2))
    (fail 'string-prefix-length))

(OR (= 0 (string-prefix-length "prefix" "preface" 0 5 1))
    (fail 'string-prefix-length))

(OR (= 0 (string-prefix-length "aisle" "" 1 4 0))
    (fail 'string-prefix-length))

(OR (= 0 (string-prefix-length "aisle" "aabbccddee" 1 4 3))
    (fail 'string-prefix-length))

(OR (= 0 (string-prefix-length "bail" "aabbccddee" 1 4 3))
    (fail 'string-prefix-length))

(OR (= 3 (string-prefix-length "prefix" "preface" 1 5 1))
    (fail 'string-prefix-length))

(OR (= 0 (string-prefix-length "" "" 0 0 0 0))
    (fail 'string-prefix-length))

(OR (= 0 (string-prefix-length "" "aabbccddee" 0 0 0 0))
    (fail 'string-prefix-length))

(OR (= 0 (string-prefix-length "aisle" "" 0 4 0 0))
    (fail 'string-prefix-length))

(OR (= 0 (string-prefix-length "aisle" "aabbccddee" 0 4 2 10))
    (fail 'string-prefix-length))

(OR (= 1 (string-prefix-length "bail" "aabbccddee" 0 1 2 10))
    (fail 'string-prefix-length))

(OR (= 0 (string-prefix-length "prefix" "preface" 0 5 1 6))
    (fail 'string-prefix-length))

(OR (= 0 (string-prefix-length "aisle" "" 1 4 0 0))
    (fail 'string-prefix-length))

(OR (= 0 (string-prefix-length "aisle" "aabbccddee" 1 4 3 3))
    (fail 'string-prefix-length))

(OR (= 0 (string-prefix-length "bail" "aabbccddee" 1 4 3 6))
    (fail 'string-prefix-length))

(OR (= 3 (string-prefix-length "prefix" "preface" 1 5 1 7))
    (fail 'string-prefix-length))


(OR (= 0 (string-suffix-length "" ""))
    (fail 'string-suffix-length))

(OR (= 0 (string-suffix-length "" "aabbccddee"))
    (fail 'string-suffix-length))

(OR (= 0 (string-suffix-length "aisle" ""))
    (fail 'string-suffix-length))

(OR (= 0 (string-suffix-length "" "aabbccddee"))
    (fail 'string-suffix-length))

(OR (= 1 (string-suffix-length "aisle" "aabbccddee"))
    (fail 'string-suffix-length))

(OR (= 0 (string-suffix-length "bail" "aabbccddee"))
    (fail 'string-suffix-length))

(OR (= 3 (string-suffix-length "place" "preface"))
    (fail 'string-suffix-length))

(OR (= 0 (string-suffix-length "" "" 0))
    (fail 'string-suffix-length))

(OR (= 0 (string-suffix-length "" "aabbccddee" 0))
    (fail 'string-suffix-length))

(OR (= 0 (string-suffix-length "aisle" "" 0))
    (fail 'string-suffix-length))

(OR (= 1 (string-suffix-length "aisle" "aabbccddee" 0))
    (fail 'string-suffix-length))

(OR (= 0 (string-suffix-length "bail" "aabbccddee" 0))
    (fail 'string-suffix-length))

(OR (= 3 (string-suffix-length "place" "preface" 0))
    (fail 'string-suffix-length))

(OR (= 0 (string-suffix-length "aisle" "" 1))
    (fail 'string-suffix-length))

(OR (= 1 (string-suffix-length "aisle" "aabbccddee" 1))
    (fail 'string-suffix-length))

(OR (= 0 (string-suffix-length "bail" "aabbccddee" 1))
    (fail 'string-suffix-length))

(OR (= 3 (string-suffix-length "place" "preface" 1))
    (fail 'string-suffix-length))

(OR (= 0 (string-suffix-length "" "" 0 0))
    (fail 'string-suffix-length))

(OR (= 0 (string-suffix-length "" "aabbccddee" 0 0))
    (fail 'string-suffix-length))

(OR (= 0 (string-suffix-length "aisle" "" 0 4))
    (fail 'string-suffix-length))

(OR (= 0 (string-suffix-length "aisle" "aabbccddee" 0 4))
    (fail 'string-suffix-length))

(OR (= 0 (string-suffix-length "bail" "aabbccddee" 0 1))
    (fail 'string-suffix-length))

(OR (= 0 (string-suffix-length "aisle" "" 1 4))
    (fail 'string-suffix-length))

(OR (= 0 (string-suffix-length "aisle" "aabbccddee" 1 4))
    (fail 'string-suffix-length))

(OR (= 1 (string-suffix-length "aisle" "aabbccddee" 1 5))
    (fail 'string-suffix-length))

(OR (= 0 (string-suffix-length "bail" "aabbccddee" 1 4))
    (fail 'string-suffix-length))

(OR (= 3 (string-suffix-length "place" "preface" 1 5))
    (fail 'string-suffix-length))

(OR (= 0 (string-suffix-length "" "" 0 0 0))
    (fail 'string-suffix-length))

(OR (= 0 (string-suffix-length "" "aabbccddee" 0 0 0))
    (fail 'string-suffix-length))

(OR (= 0 (string-suffix-length "aisle" "" 0 4 0))
    (fail 'string-suffix-length))

(OR (= 0 (string-suffix-length "aisle" "aabbccddee" 0 4 2))
    (fail 'string-suffix-length))

(OR (= 0 (string-suffix-length "bail" "aabbccddee" 0 1 2))
    (fail 'string-suffix-length))

(OR (= 3 (string-suffix-length "place" "preface" 0 5 1))
    (fail 'string-suffix-length))

(OR (= 0 (string-suffix-length "aisle" "" 1 4 0))
    (fail 'string-suffix-length))

(OR (= 0 (string-suffix-length "aisle" "aabbccddee" 1 4 3))
    (fail 'string-suffix-length))

(OR (= 0 (string-suffix-length "bail" "aabbccddee" 1 4 3))
    (fail 'string-suffix-length))

(OR (= 3 (string-suffix-length "place" "preface" 1 5 1))
    (fail 'string-suffix-length))

(OR (= 0 (string-suffix-length "" "" 0 0 0 0))
    (fail 'string-suffix-length))

(OR (= 0 (string-suffix-length "" "aabbccddee" 0 0 0 0))
    (fail 'string-suffix-length))

(OR (= 0 (string-suffix-length "aisle" "" 0 4 0 0))
    (fail 'string-suffix-length))

(OR (= 1 (string-suffix-length "aisle" "aabbccddee" 0 5 2 10))
    (fail 'string-suffix-length))

(OR (= 1 (string-suffix-length "bail" "aabbccddee" 0 1 2 4))
    (fail 'string-suffix-length))

(OR (= 0 (string-suffix-length "place" "preface" 0 5 1 6))
    (fail 'string-suffix-length))

(OR (= 2 (string-suffix-length "place" "preface" 0 4 1 6))
    (fail 'string-suffix-length))

(OR (= 0 (string-suffix-length "aisle" "" 1 4 0 0))
    (fail 'string-suffix-length))

(OR (= 0 (string-suffix-length "aisle" "aabbccddee" 1 4 3 3))
    (fail 'string-suffix-length))

(OR (= 0 (string-suffix-length "bail" "aabbccddee" 1 4 3 6))
    (fail 'string-suffix-length))

(OR (= 3 (string-suffix-length "place" "preface" 1 5 1 7))
    (fail 'string-suffix-length))


(OR (eq? #t (string-prefix? "" ""))
    (fail 'string-prefix?))

(OR (eq? #t (string-prefix? "" "abc"))
    (fail 'string-prefix?))

(OR (eq? #t (string-prefix? "a" "abc"))
    (fail 'string-prefix?))

(OR (eq? #f (string-prefix? "c" "abc"))
    (fail 'string-prefix?))

(OR (eq? #t (string-prefix? "ab" "abc"))
    (fail 'string-prefix?))

(OR (eq? #f (string-prefix? "ac" "abc"))
    (fail 'string-prefix?))

(OR (eq? #t (string-prefix? "abc" "abc"))
    (fail 'string-prefix?))

(OR (eq? #t (string-suffix? "" ""))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "" "abc"))
    (fail 'string-suffix?))

(OR (eq? #f (string-suffix? "a" "abc"))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "c" "abc"))
    (fail 'string-suffix?))

(OR (eq? #f (string-suffix? "ac" "abc"))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "bc" "abc"))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "abc" "abc"))
    (fail 'string-suffix?))

(OR (eq? #t (string-prefix? "" "" 0))
    (fail 'string-prefix?))

(OR (eq? #t (string-prefix? "" "abc" 0))
    (fail 'string-prefix?))

(OR (eq? #t (string-prefix? "a" "abc" 0))
    (fail 'string-prefix?))

(OR (eq? #f (string-prefix? "c" "abc" 0))
    (fail 'string-prefix?))

(OR (eq? #t (string-prefix? "ab" "abc" 0))
    (fail 'string-prefix?))

(OR (eq? #f (string-prefix? "ac" "abc" 0))
    (fail 'string-prefix?))

(OR (eq? #t (string-prefix? "abc" "abc" 0))
    (fail 'string-prefix?))

(OR (eq? #t (string-suffix? "" "" 0))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "" "abc" 0))
    (fail 'string-suffix?))

(OR (eq? #f (string-suffix? "a" "abc" 0))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "c" "abc" 0))
    (fail 'string-suffix?))

(OR (eq? #f (string-suffix? "ac" "abc" 0))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "bc" "abc" 0))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "abc" "abc" 0))
    (fail 'string-suffix?))

(OR (eq? #t (string-prefix? "ab" "abc" 2))
    (fail 'string-prefix?))

(OR (eq? #t (string-prefix? "ac" "abc" 2))
    (fail 'string-prefix?))

(OR (eq? #f (string-prefix? "abc" "abc" 2))
    (fail 'string-prefix?))

(OR (eq? #t (string-suffix? "ac" "abc" 2))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "bc" "abc" 2))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "abc" "abc" 2))
    (fail 'string-suffix?))


(OR (eq? #t (string-prefix? "" "" 0 0))
    (fail 'string-prefix?))

(OR (eq? #t (string-prefix? "" "abc" 0 0))
    (fail 'string-prefix?))

(OR (eq? #t (string-prefix? "a" "abc" 0 0))
    (fail 'string-prefix?))

(OR (eq? #f (string-prefix? "c" "abc" 0 1))
    (fail 'string-prefix?))

(OR (eq? #t (string-prefix? "ab" "abc" 0 1))
    (fail 'string-prefix?))

(OR (eq? #t (string-prefix? "ab" "abc" 0 2))
    (fail 'string-prefix?))

(OR (eq? #f (string-prefix? "ac" "abc" 0 2))
    (fail 'string-prefix?))

(OR (eq? #t (string-prefix? "abc" "abc" 0 3))
    (fail 'string-prefix?))

(OR (eq? #t (string-suffix? "" "" 0 0))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "" "abc" 0 0))
    (fail 'string-suffix?))

(OR (eq? #f (string-suffix? "a" "abc" 0 1))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "c" "abc" 0 1))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "ac" "abc" 1 2))
    (fail 'string-suffix?))

(OR (eq? #f (string-suffix? "ac" "abc" 0 2))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "bc" "abc" 0 2))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "abc" "abc" 0 3))
    (fail 'string-suffix?))

(OR (eq? #t (string-prefix? "ab" "abc" 2 2))
    (fail 'string-prefix?))

(OR (eq? #t (string-prefix? "ac" "abc" 2 2))
    (fail 'string-prefix?))

(OR (eq? #f (string-prefix? "abc" "abc" 2 3))
    (fail 'string-prefix?))

(OR (eq? #t (string-suffix? "ac" "abc" 2 2))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "bc" "abc" 2 2))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "abc" "abc" 2 3))
    (fail 'string-suffix?))


(OR (eq? #t (string-prefix? "" "" 0 0 0))
    (fail 'string-prefix?))

(OR (eq? #t (string-prefix? "" "abc" 0 0 0))
    (fail 'string-prefix?))

(OR (eq? #t (string-prefix? "a" "abc" 0 0 0))
    (fail 'string-prefix?))

(OR (eq? #f (string-prefix? "c" "abc" 0 1 0))
    (fail 'string-prefix?))

(OR (eq? #t (string-prefix? "ab" "abc" 0 1 0))
    (fail 'string-prefix?))

(OR (eq? #t (string-prefix? "ab" "abc" 0 2 0))
    (fail 'string-prefix?))

(OR (eq? #f (string-prefix? "ac" "abc" 0 2 0))
    (fail 'string-prefix?))

(OR (eq? #t (string-prefix? "abc" "abc" 0 3 0))
    (fail 'string-prefix?))

(OR (eq? #t (string-suffix? "" "" 0 0 0))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "" "abc" 0 0 0))
    (fail 'string-suffix?))

(OR (eq? #f (string-suffix? "a" "abc" 0 1 0))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "c" "abc" 0 1 0))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "ac" "abc" 1 2 0))
    (fail 'string-suffix?))

(OR (eq? #f (string-suffix? "ac" "abc" 0 2 0))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "bc" "abc" 0 2 0))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "abc" "abc" 0 3 0))
    (fail 'string-suffix?))

(OR (eq? #t (string-prefix? "ab" "abc" 2 2 0))
    (fail 'string-prefix?))

(OR (eq? #t (string-prefix? "ac" "abc" 2 2 0))
    (fail 'string-prefix?))

(OR (eq? #f (string-prefix? "abc" "abc" 2 3 0))
    (fail 'string-prefix?))

(OR (eq? #t (string-suffix? "ac" "abc" 2 2 0))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "bc" "abc" 2 2 0))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "abc" "abc" 2 3 0))
    (fail 'string-suffix?))

(OR (eq? #t (string-prefix? "" "abc" 0 0 1))
    (fail 'string-prefix?))

(OR (eq? #t (string-prefix? "a" "abc" 0 0 1))
    (fail 'string-prefix?))

(OR (eq? #t (string-prefix? "c" "abc" 0 1 2))
    (fail 'string-prefix?))

(OR (eq? #f (string-prefix? "ab" "abc" 0 1 2))
    (fail 'string-prefix?))

(OR (eq? #f (string-prefix? "ab" "abc" 0 2 1))
    (fail 'string-prefix?))

(OR (eq? #f (string-prefix? "ac" "abc" 0 2 1))
    (fail 'string-prefix?))

(OR (eq? #f (string-prefix? "abc" "abc" 0 3 1))
    (fail 'string-prefix?))

(OR (eq? #f (string-suffix? "a" "abc" 0 1 2))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "c" "abc" 0 1 1))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "ac" "abc" 1 2 2))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "bc" "abc" 0 2 1))
    (fail 'string-suffix?))

(OR (eq? #f (string-suffix? "bc" "abc" 0 2 2))
    (fail 'string-suffix?))


(OR (eq? #t (string-prefix? "" "" 0 0 0 0))
    (fail 'string-prefix?))

(OR (eq? #t (string-prefix? "" "abc" 0 0 0 3))
    (fail 'string-prefix?))

(OR (eq? #t (string-prefix? "a" "abc" 0 0 0 3))
    (fail 'string-prefix?))

(OR (eq? #f (string-prefix? "c" "abc" 0 1 0 3))
    (fail 'string-prefix?))

(OR (eq? #t (string-prefix? "ab" "abc" 0 1 0 3))
    (fail 'string-prefix?))

(OR (eq? #t (string-prefix? "ab" "abc" 0 2 0 3))
    (fail 'string-prefix?))

(OR (eq? #f (string-prefix? "ac" "abc" 0 2 0 3))
    (fail 'string-prefix?))

(OR (eq? #t (string-prefix? "abc" "abc" 0 3 0 3))
    (fail 'string-prefix?))

(OR (eq? #t (string-suffix? "" "abc" 0 0 0 3))
    (fail 'string-suffix?))

(OR (eq? #f (string-suffix? "a" "abc" 0 1 0 3))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "c" "abc" 0 1 0 3))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "ac" "abc" 1 2 0 3))
    (fail 'string-suffix?))

(OR (eq? #f (string-suffix? "ac" "abc" 0 2 0 3))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "bc" "abc" 0 2 0 3))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "abc" "abc" 0 3 0 3))
    (fail 'string-suffix?))

(OR (eq? #t (string-prefix? "ab" "abc" 2 2 0 3))
    (fail 'string-prefix?))

(OR (eq? #t (string-prefix? "ac" "abc" 2 2 0 3))
    (fail 'string-prefix?))

(OR (eq? #f (string-prefix? "abc" "abc" 2 3 0 3))
    (fail 'string-prefix?))

(OR (eq? #t (string-suffix? "ac" "abc" 2 2 0 3))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "bc" "abc" 2 2 0 3))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "abc" "abc" 2 3 0 3))
    (fail 'string-suffix?))

(OR (eq? #t (string-prefix? "" "abc" 0 0 1 3))
    (fail 'string-prefix?))

(OR (eq? #t (string-prefix? "a" "abc" 0 0 1 3))
    (fail 'string-prefix?))

(OR (eq? #t (string-prefix? "c" "abc" 0 1 2 3))
    (fail 'string-prefix?))

(OR (eq? #f (string-prefix? "ab" "abc" 0 1 2 3))
    (fail 'string-prefix?))

(OR (eq? #f (string-prefix? "ab" "abc" 0 2 1 3))
    (fail 'string-prefix?))

(OR (eq? #f (string-prefix? "ac" "abc" 0 2 1 3))
    (fail 'string-prefix?))

(OR (eq? #f (string-prefix? "abc" "abc" 0 3 1 3))
    (fail 'string-prefix?))

(OR (eq? #f (string-suffix? "a" "abc" 0 1 2 3))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "c" "abc" 0 1 1 3))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "ac" "abc" 1 2 2 3))
    (fail 'string-suffix?))

(OR (eq? #t (string-suffix? "bc" "abc" 0 2 1 3))
    (fail 'string-suffix?))

(OR (eq? #f (string-suffix? "bc" "abc" 0 2 2 3))
    (fail 'string-suffix?))


(OR (eq? #t (string-prefix? "" "abc" 0 0 0 2))
    (fail 'string-prefix?))

(OR (eq? #t (string-prefix? "a" "abc" 0 0 0 2))
    (fail 'string-prefix?))

(OR (eq? #f (string-prefix? "c" "abc" 0 1 0 2))
    (fail 'string-prefix?))

(OR (eq? #t (string-prefix? "ab" "abc" 0 1 0 2))
    (fail 'string-prefix?))

(OR (eq? #f (string-prefix? "abc" "abc" 0 3 0 2))
    (fail 'string-prefix?))

(OR (eq? #t (string-suffix? "" "abc" 0 0 0 2))
    (fail 'string-suffix?))

(OR (eq? #f (string-suffix? "c" "abc" 0 1 0 2))
    (fail 'string-suffix?))

(OR (eq? #f (string-suffix? "ac" "abc" 1 2 0 2))
    (fail 'string-suffix?))

;;-----------------------------------------------------------------------
(test-section "Searching")

(OR (= 0
       (string-cursor->index EMPTY
                             (string-index EMPTY char?)))
    (fail 'string-index))

(OR (= 0
       (string-cursor->index ABCDEF
                             (string-index ABCDEF char?)))
    (fail 'string-index))

(OR (= 4
       (string-cursor->index ABCDEF-ASCII
                             (string-index ABCDEF-ASCII
                                           (lambda (c) (char>? c #\d)))))
    (fail 'string-index))

(OR (= 6
       (string-cursor->index ABCDEF
                             (string-index ABCDEF char-whitespace?)))
    (fail 'string-index))

(OR (= 0
       (string-cursor->index EMPTY
                             (string-index-right EMPTY char?)))
    (fail 'string-index-right))

(OR (= 6
       (string-cursor->index ABCDEF
                             (string-index-right ABCDEF char?)))
    (fail 'string-index-right))

(OR (= 6
       (string-cursor->index ABCDEF
                             (string-index-right ABCDEF
                                                 (lambda (c) (char>? c #\d)))))
    (fail 'string-index-right))

(OR (= 0
       (string-cursor->index ABCDEF
                             (string-index-right ABCDEF char-whitespace?)))
    (fail 'string-index-right))

(OR (= 0
       (string-cursor->index EMPTY (string-skip EMPTY string?)))
    (fail 'string-skip))

(OR (= 0
       (string-cursor->index ABCDEF
                             (string-skip ABCDEF string?)))
    (fail 'string-skip))

(OR (= 4
       (string-cursor->index ABCDEF-ASCII
                             (string-skip ABCDEF-ASCII
                                          (lambda (c) (char<=? c #\d)))))
    (fail 'string-skip))

(OR (= 6
       (string-cursor->index ABCDEF
                             (string-skip ABCDEF char?)))
    (fail 'string-skip))

(OR (= 0
       (string-cursor->index EMPTY (string-skip-right EMPTY string?)))
    (fail 'string-skip-right))

(OR (= 6
       (string-cursor->index ABCDEF
                             (string-skip-right ABCDEF string?)))
    (fail 'string-skip-right))

(OR (= 6
       (string-cursor->index ABCDEF-ASCII
                             (string-skip-right ABCDEF-ASCII
                                                (lambda (c) (char<=? c #\d)))))
    (fail 'string-skip-right))

(OR (= 0
       (string-cursor->index ABCDEF
                             (string-skip-right ABCDEF char?)))
    (fail 'string-skip-right))


(OR (= 2
       (string-cursor->index ABCDEF
                             (string-index ABCDEF char? 2)))
    (fail 'string-index))

(OR (= 4
       (string-cursor->index ABCDEF-ASCII
                             (string-index ABCDEF-ASCII
                                           (lambda (c) (char>? c #\d)) 2)))
    (fail 'string-index))

(OR (= 6
       (string-cursor->index ABCDEF
                             (string-index ABCDEF char-whitespace? 2)))
    (fail 'string-index))

(OR (= 6
       (string-cursor->index ABCDEF
                             (string-index-right ABCDEF char? 2)))
    (fail 'string-index-right))

(OR (= 6
       (string-cursor->index ABCDEF-ASCII
                             (string-index-right ABCDEF-ASCII
                                                 (lambda (c)
                                                   (char>? c #\d)) 2)))
    (fail 'string-index-right))

(OR (= 2
       (string-cursor->index ABCDEF
                             (string-index-right ABCDEF char-whitespace? 2)))
    (fail 'string-index-right))

(OR (= 2
       (string-cursor->index ABCDEF
                             (string-skip ABCDEF string? 2)))
    (fail 'string-skip))

(OR (= 4
       (string-cursor->index ABCDEF-ASCII
                             (string-skip ABCDEF-ASCII
                                          (lambda (c)
                                            (char<=? c #\d)) 2)))
    (fail 'string-skip))

(OR (= 6
       (string-cursor->index ABCDEF
                             (string-skip ABCDEF char? 2)))
    (fail 'string-skip))

(OR (= 6
       (string-cursor->index ABCDEF
                             (string-skip-right ABCDEF string? 2)))
    (fail 'string-skip-right))

(OR (= 6
       (string-cursor->index ABCDEF-ASCII
                             (string-skip-right ABCDEF-ASCII
                                                (lambda (c)
                                                  (char<=? c #\d)) 2)))
    (fail 'string-skip-right))

(OR (= 2
       (string-cursor->index ABCDEF
                             (string-skip-right ABCDEF char? 2)))
    (fail 'string-skip-right))


(OR (= 2
       (string-cursor->index ABCDEF
                             (string-index ABCDEF char? 2 5)))
    (fail 'string-index))

(OR (= 4
       (string-cursor->index ABCDEF-ASCII
                             (string-index ABCDEF-ASCII
                                           (lambda (c) (char>? c #\d)) 2 5)))
    (fail 'string-index))

(OR (= 5
       (string-cursor->index ABCDEF
                             (string-index ABCDEF char-whitespace? 2 5)))
    (fail 'string-index))

(OR (= 5
       (string-cursor->index ABCDEF
                             (string-index-right ABCDEF char? 2 5)))
    (fail 'string-index-right))

(OR (= 5
       (string-cursor->index ABCDEF-ASCII
                             (string-index-right ABCDEF-ASCII
                                                 (lambda (c)
                                                   (char>? c #\d)) 2 5)))
    (fail 'string-index-right))

(OR (= 2
       (string-cursor->index ABCDEF
                             (string-index-right ABCDEF
                                                 char-whitespace? 2 5)))
    (fail 'string-index-right))

(OR (= 2
       (string-cursor->index ABCDEF
                             (string-skip ABCDEF string? 2 5)))
    (fail 'string-skip))

(OR (= 4
       (string-cursor->index ABCDEF-ASCII
                             (string-skip ABCDEF-ASCII
                                          (lambda (c) (char<=? c #\d)) 2 5)))
    (fail 'string-skip))

(OR (= 5
       (string-cursor->index ABCDEF
                             (string-skip ABCDEF char? 2 5)))
    (fail 'string-skip))

(OR (= 5
       (string-cursor->index ABCDEF
                             (string-skip-right ABCDEF string? 2 5)))
    (fail 'string-skip-right))

(OR (= 5
       (string-cursor->index ABCDEF-ASCII
                             (string-skip-right ABCDEF-ASCII
                                                (lambda (c)
                                                  (char<=? c #\d)) 2 5)))
    (fail 'string-skip-right))

(OR (= 2
       (string-cursor->index ABCDEF
                             (string-skip-right ABCDEF char? 2 5)))
    (fail 'string-skip-right))


(OR (eqv? 0
          (string-cursor->index EMPTY
                                (string-contains EMPTY EMPTY)))
    (fail 'string-contains))

(OR (eqv? 0
          (string-cursor->index ABCDEFFFFOO
                                (string-contains ABCDEFFFFOO EMPTY)))
    (fail 'string-contains))

(OR (eqv? 0
          (string-cursor->index ABCDEFFFFOO
                                (string-contains ABCDEFFFFOO "a")))
    (fail 'string-contains))

(OR (eqv? 5
          (string-cursor->index ABCDEFFFFOO
                                (string-contains ABCDEFFFFOO "ff")))
    (fail 'string-contains))

(OR (eqv? 4
          (string-cursor->index ABCDEFFFFOO
                                (string-contains ABCDEFFFFOO "eff")))
    (fail 'string-contains))

(OR (eqv? 8
          (string-cursor->index ABCDEFFFFOO
                                (string-contains ABCDEFFFFOO "foo")))
    (fail 'string-contains))

(OR (eqv? #f
          (string-contains ABCDEFFFFOO "efffoo"))
    (fail 'string-contains))

#|
(OR (eqv? 0
          (string-cursor->index EMPTY
                                (string-contains-right EMPTY EMPTY)))
    (fail 'string-contains-right))

(OR (eqv? 11
          (string-cursor->index ABCDEFFFFOO
                                (string-contains-right ABCDEFFFFOO EMPTY)))
    (fail 'string-contains-right))

(OR (eqv? 0
          (string-cursor->index ABCDEFFFFOO
                                (string-contains-right ABCDEFFFFOO "a")))
    (fail 'string-contains-right))

(OR (eqv? 7
          (string-cursor->index ABCDEFFFFOO
                                (string-contains-right ABCDEFFFFOO "ff")))
    (fail 'string-contains-right))

(OR (eqv? 4
          (string-cursor->index ABCDEFFFFOO
                                (string-contains-right ABCDEFFFFOO "eff")))
    (fail 'string-contains-right))

(OR (eqv? 8
          (string-cursor->index ABCDEFFFFOO
                                (string-contains-right ABCDEFFFFOO "foo")))
    (fail 'string-contains-right))

(OR (eqv? #f
          (string-cursor->index ABCDEFFFFOO
                                (string-contains-right ABCDEFFFFOO
                                                       "efffoo")))
    (fail 'string-contains-right))
|#

(OR (eqv? 0
          (string-cursor->index EMPTY
                                (string-contains EMPTY EMPTY 0)))
    (fail 'string-contains))

(OR (eqv? 2
          (string-cursor->index ABCDEFFFFOO
                                (string-contains ABCDEFFFFOO EMPTY 2)))
    (fail 'string-contains))

(OR (eqv? #f (string-contains ABCDEFFFFOO "a" 2))
    (fail 'string-contains))

(OR (eqv? 5
          (string-cursor->index ABCDEFFFFOO
                                (string-contains ABCDEFFFFOO "ff" 2)))
    (fail 'string-contains))

(OR (eqv? 4
          (string-cursor->index ABCDEFFFFOO
                                (string-contains ABCDEFFFFOO "eff" 2)))
    (fail 'string-contains))

(OR (eqv? 8
          (string-cursor->index ABCDEFFFFOO
                                (string-contains ABCDEFFFFOO "foo" 2)))
    (fail 'string-contains))

(OR (eqv? #f (string-contains ABCDEFFFFOO "efffoo" 2))
    (fail 'string-contains))

#|
(OR (eqv? 0
          (string-cursor->index EMPTY
                                (string-contains-right EMPTY EMPTY 0)))
    (fail 'string-contains-right))

(OR (eqv? 11
          (string-cursor->index ABCDEFFFFOO
                                (string-contains-right ABCDEFFFFOO
                                                       EMPTY 2)))
    (fail 'string-contains-right))

(OR (eqv? #f
          (string-cursor->index ABCDEFFFFOO
                                (string-contains-right ABCDEFFFFOO
                                                       "a" 2)))
    (fail 'string-contains-right))

(OR (eqv? 7
          (string-cursor->index ABCDEFFFFOO
                                (string-contains-right ABCDEFFFFOO
                                                       "ff" 2)))
    (fail 'string-contains-right))

(OR (eqv? 4
          (string-cursor->index ABCDEFFFFOO
                                (string-contains-right ABCDEFFFFOO
                                                       "eff" 2)))
    (fail 'string-contains-right))

(OR (eqv? 8
          (string-cursor->index ABCDEFFFFOO
                                (string-contains-right ABCDEFFFFOO
                                                       "foo" 2)))
    (fail 'string-contains-right))

(OR (eqv? #f
          (string-cursor->index ABCDEFFFFOO
                                (string-contains-right ABCDEFFFFOO
                                                       "efffoo" 2)))
    (fail 'string-contains-right))
|#


(OR (eqv? 0
          (string-cursor->index EMPTY
                                (string-contains EMPTY EMPTY 0 0)))
    (fail 'string-contains))

(OR (eqv? 2
          (string-cursor->index ABCDEFFFFOO
                                (string-contains ABCDEFFFFOO
                                                 EMPTY 2 10)))
    (fail 'string-contains))

(OR (eqv? #f
          (string-contains ABCDEFFFFOO
                           "a" 2 10))
    (fail 'string-contains))

(OR (eqv? 5
          (string-cursor->index ABCDEFFFFOO
                                (string-contains ABCDEFFFFOO
                                                 "ff" 2 10)))
    (fail 'string-contains))

(OR (eqv? 4
          (string-cursor->index ABCDEFFFFOO
                                (string-contains ABCDEFFFFOO
                                                 "eff" 2 10)))
    (fail 'string-contains))

(OR (eqv? #f (string-contains ABCDEFFFFOO "foo" 2 10))
    (fail 'string-contains))

(OR (eqv? #f (string-contains ABCDEFFFFOO "efffoo" 2 10))
    (fail 'string-contains))

#|
(OR (eqv? 0
          (string-cursor->index EMPTY
                                (string-contains-right EMPTY EMPTY 0 0)))
    (fail 'string-contains-right))

(OR (eqv? 10
          (string-cursor->index ABCDEFFFFOO
                                (string-contains-right ABCDEFFFFOO
                                                       EMPTY 2 10)))
    (fail 'string-contains-right))

(OR (eqv? #f
          (string-cursor->index ABCDEFFFFOO
                                (string-contains-right ABCDEFFFFOO
                                                       "a" 2 10)))
    (fail 'string-contains-right))

(OR (eqv? 7
          (string-cursor->index ABCDEFFFFOO
                                (string-contains-right ABCDEFFFFOO
                                                       "ff" 2 10)))
    (fail 'string-contains-right))

(OR (eqv? 4
          (string-cursor->index ABCDEFFFFOO
                                (string-contains-right ABCDEFFFFOO
                                                       "eff" 2 10)))
    (fail 'string-contains-right))

(OR (eqv? #f
          (string-cursor->index ABCDEFFFFOO
                                (string-contains-right ABCDEFFFFOO
                                                       "foo" 2 10)))
    (fail 'string-contains-right))

(OR (eqv? #f
          (string-cursor->index ABCDEFFFFOO
                                (string-contains-right ABCDEFFFFOO
                                                       "efffoo" 2 10)))
    (fail 'string-contains-right))
|#

(OR (eqv? 0
          (string-cursor->index EMPTY
                                (string-contains EMPTY EMPTY 0 0 0)))
    (fail 'string-contains))

(OR (eqv? 2
          (string-cursor->index ABCDEFFFFOO
                                (string-contains ABCDEFFFFOO
                                                 EMPTY 2 10 0)))
    (fail 'string-contains))

(OR (eqv? 2
          (string-cursor->index ABCDEFFFFOO
                                (string-contains ABCDEFFFFOO
                                                 "a" 2 10 1)))
    (fail 'string-contains))

(OR (eqv? 5
          (string-cursor->index ABCDEFFFFOO
                                (string-contains ABCDEFFFFOO
                                                 "ff" 2 10 1)))
    (fail 'string-contains))

(OR (eqv? 5
          (string-cursor->index ABCDEFFFFOO
                                (string-contains ABCDEFFFFOO
                                                 "eff" 2 10 1)))
    (fail 'string-contains))

(OR (eqv? #f (string-contains ABCDEFFFFOO "foo" 2 10 1))
    (fail 'string-contains))

(OR (eqv? #f (string-contains ABCDEFFFFOO "efffoo" 2 10 1))
    (fail 'string-contains))

#|
(OR (eqv? 0
          (string-cursor->index EMPTY
                                (string-contains-right EMPTY EMPTY 0 0 0)))
    (fail 'string-contains-right))

(OR (eqv? 10
          (string-cursor->index ABCDEFFFFOO
                                (string-contains-right ABCDEFFFFOO
                                                       EMPTY 2 10 0)))
    (fail 'string-contains-right))

(OR (eqv? 10
          (string-cursor->index ABCDEFFFFOO
                                (string-contains-right ABCDEFFFFOO
                                                       "a" 2 10 1)))
    (fail 'string-contains-right))

(OR (eqv? 8
          (string-cursor->index ABCDEFFFFOO
                                (string-contains-right ABCDEFFFFOO
                                                       "ff" 2 10 1)))
    (fail 'string-contains-right))

(OR (eqv? 7
          (string-cursor->index ABCDEFFFFOO
                                (string-contains-right ABCDEFFFFOO
                                                       "eff" 2 10 1)))
    (fail 'string-contains-right))

(OR (eqv? #f
          (string-cursor->index ABCDEFFFFOO
                                (string-contains-right ABCDEFFFFOO
                                                       "foo" 2 10 1)))
    (fail 'string-contains-right))

(OR (eqv? #f
          (string-cursor->index ABCDEFFFFOO
                                (string-contains-right ABCDEFFFFOO
                                                       "efffoo" 2 10 1)))
    (fail 'string-contains-right))
|#

(OR (eqv? 0
          (string-cursor->index EMPTY
                                (string-contains EMPTY EMPTY 0 0 0 0)))
    (fail 'string-contains))

(OR (eqv? 2
          (string-cursor->index ABCDEFFFFOO
                                (string-contains ABCDEFFFFOO
                                                 EMPTY 2 10 0 0)))
    (fail 'string-contains))

(OR (eqv? 2
          (string-cursor->index ABCDEFFFFOO
                                (string-contains ABCDEFFFFOO
                                                 "a" 2 10 1 1)))
    (fail 'string-contains))

(OR (eqv? 5
          (string-cursor->index ABCDEFFFFOO
                                (string-contains ABCDEFFFFOO
                                                 "ff" 2 10 1 2)))
    (fail 'string-contains))

(OR (eqv? 5
          (string-cursor->index ABCDEFFFFOO
                                (string-contains ABCDEFFFFOO
                                                 "eff" 2 10 1 2)))
    (fail 'string-contains))

(OR (eqv? 9
          (string-cursor->index ABCDEFFFFOO
                                (string-contains ABCDEFFFFOO
                                                 "foo" 2 10 1 2)))
    (fail 'string-contains))

(OR (eqv? 4
          (string-cursor->index ABCDEFFFFOO
                                (string-contains ABCDEFFFFOO
                                                 "efffoo" 2 10 0 2)))
    (fail 'string-contains))

#|
(OR (eqv? 0
          (string-cursor->index EMPTY
                                (string-contains-right EMPTY EMPTY 0 0 0 0)))
    (fail 'string-contains-right))

(OR (eqv? 10
          (string-cursor->index ABCDEFFFFOO
                                (string-contains-right ABCDEFFFFOO
                                                       EMPTY 2 10 0 0)))
    (fail 'string-contains-right))

(OR (eqv? 10
          (string-cursor->index ABCDEFFFFOO
                                (string-contains-right ABCDEFFFFOO
                                                       "a" 2 10 1 1)))
    (fail 'string-contains-right))

(OR (eqv? 8
          (string-cursor->index ABCDEFFFFOO
                                (string-contains-right ABCDEFFFFOO
                                                       "ff" 2 10 1 2)))
    (fail 'string-contains-right))

(OR (eqv? 8
          (string-cursor->index ABCDEFFFFOO
                                (string-contains-right ABCDEFFFFOO
                                                       "eff" 2 10 1 2)))
    (fail 'string-contains-right))

(OR (eqv? 9
          (string-cursor->index ABCDEFFFFOO
                                (string-contains-right ABCDEFFFFOO
                                                       "foo" 2 10 1 2)))
    (fail 'string-contains-right))

(OR (eqv? 7
          (string-cursor->index ABCDEFFFFOO
                                (string-contains-right ABCDEFFFFOO
                                                       "efffoo" 2 10 1 3)))
    (fail 'string-contains-right))
|#

;;-----------------------------------------------------------------------
(test-section "The whole string")

(OR (string=? "" (string-reverse ""))
    (fail 'string-reverse))

(OR (string=? "fedcba" (string-reverse "abcdef"))
    (fail 'string-reverse))

(OR (string=? "" (string-reverse "" 0))
    (fail 'string-reverse))

(OR (string=? "fedcba" (string-reverse "abcdef" 0))
    (fail 'string-reverse))

(OR (string=? "fedc" (string-reverse "abcdef" 2))
    (fail 'string-reverse))

(OR (string=? "" (string-reverse "" 0 0))
    (fail 'string-reverse))

(OR (string=? "fedcba" (string-reverse "abcdef" 0 6))
    (fail 'string-reverse))

(OR (string=? "edc" (string-reverse "abcdef" 2 5))
    (fail 'string-reverse))


(OR (string=? "" (string-concatenate '()))
    (fail 'string-concatenate))

(OR (string=? "abcdef" (string-concatenate '("" "a" "bcd" "" "ef" "" "")))
    (fail 'string-concatenate))

(OR (string=? "" (string-concatenate-reverse '()))
    (fail 'string-concatenate-reverse))

(OR (string=? "efbcda"
              (string-concatenate-reverse '("" "a" "bcd" "" "ef" "" "")))
    (fail 'string-concatenate-reverse))

(OR (string=? "huh?" (string-concatenate-reverse '() "huh?"))
    (fail 'string-concatenate-reverse))

(OR (string=? "efbcdaxy"
              (string-concatenate-reverse '("" "a" "bcd" "" "ef" "" "") "xy"))
    (fail 'string-concatenate-reverse))

(OR (string=? "huh" (string-concatenate-reverse '() "huh?" 3))
    (fail 'string-concatenate-reverse))

(OR (string=? "efbcdax"
              (string-concatenate-reverse '("" "a" "bcd" "" "ef" "" "") "x" 1))
    (fail 'string-concatenate-reverse))


(OR (= 8
       (string-fold (lambda (c count)
                      (if (char-whitespace? c)
                          (+ count 1)
                          count))
                    0
                    " ...a couple of spaces in this one... "))
    (fail 'string-fold))

(OR (= 7
       (string-fold (lambda (c count)
                      (if (char-whitespace? c)
                          (+ count 1)
                          count))
                    0
                    " ...a couple of spaces in this one... "
                    1))
    (fail 'string-fold))

(OR (= 6
       (string-fold (lambda (c count)
                      (if (char-whitespace? c)
                          (+ count 1)
                          count))
                    0
                    " ...a couple of spaces in this one... "
                    1
                    32))
    (fail 'string-fold))

(OR (equal? (string->list "abcdef")
            (string-fold-right cons '() "abcdef"))
    (fail 'string-fold-right))

(OR (equal? (string->list "def")
            (string-fold-right cons '() "abcdef" 3))
    (fail 'string-fold-right))

(OR (equal? (string->list "cde")
            (string-fold-right cons '() "abcdef" 2 5))
    (fail 'string-fold-right))

(OR (string=? "aabraacaadaabraa"
              (let* ((s "abracadabra")
                     (ans-len (string-fold (lambda (c sum)
                                             (+ sum (if (char=? c #\a) 2 1)))
                                           0 s))
                     (ans (make-string ans-len)))
                (string-fold (lambda (c i)
                               (let ((i (if (char=? c #\a)
                                            (begin (string-set! ans i #\a)
                                                   (+ i 1))
                                            i)))
                                 (string-set! ans i c)
                                 (+ i 1)))
                             0 s)
                ans))
    (fail 'string-fold))


(OR (equal? '(101 100 99 98 97)
            (let ((s "abcde") (v '()))
              (string-for-each-cursor
               (lambda (cur)
                 (set! v (cons (char->integer (string-ref/cursor s cur)) v)))
               s)
              v))
    (fail 'string-for-each-cursor))


#|
(OR (string=? "cdefabcdefabcd"
              (string-replicate "abcdef" -4 10))
    (fail 'string-replicate))

(OR (string=? "bcdefbcdefbcd"
              (string-replicate "abcdef" 90 103 1))
    (fail 'string-replicate))

(OR (string=? "ecdecdecde"
              (string-replicate "abcdef" -13 -3 2 5))
    (fail 'string-replicate))
|#

(OR (= 6 (string-count "abcdef" char?))
    (fail 'string-count))

(OR (= 4 (string-count "counting  whitespace, again " char-whitespace? 5))
    (fail 'string-count))

(OR (= 3 (string-count "abcdefwxyz"
                       (lambda (c) (odd? (char->integer c)))
                       2 8))
    (fail 'string-count))


(OR (string=? "It's lots of fun to code it up in Scheme."
              (string-replace "It's easy to code it up in Scheme."
                              "lots of fun"
                              5 9))
    (fail 'string-replace))

(OR (string=? "The miserable perl programmer endured daily ridicule."
              (string-replace "The TCL programmer endured daily ridicule."
                              "another miserable perl drone"
                              4 7 8 22))
    (fail 'string-replace))

(OR (string=? "It's really easy to code it up in Scheme."
              (string-replace "It's easy to code it up in Scheme."
                              "really "
                              5 5))
    (fail 'string-replace))


(OR (equal? '() (string-split "" ""))
    (fail 'string-split))

(OR (equal? '("a" "b" "c") (string-split "abc" ""))
    (fail 'string-split))

(OR (equal? '("too" "" "much" "" "data")
            (string-split "too  much  data" " "))
    (fail 'string-split))

(OR (equal? '("" "there" "ya" "go" "")
            (string-split "***there***ya***go***" "***"))
    (fail 'string-split))

(OR (equal? '() (string-split "" "" 'infix))
    (fail 'string-split))

(OR (equal? '("a" "b" "c") (string-split "abc" "" 'infix))
    (fail 'string-split))

(OR (equal? '("too" "" "much" "" "data")
            (string-split "too  much  data" " " 'infix))
    (fail 'string-split))

(OR (equal? '("" "there" "ya" "go" "")
            (string-split "***there***ya***go***" "***" 'infix))
    (fail 'string-split))

(OR (equal? 'error
            (guard (exn (else 'error))
             (string-split "" "" 'strict-infix)))
    (fail 'string-split))

(OR (equal? '("a" "b" "c") (string-split "abc" "" 'strict-infix))
    (fail 'string-split))

(OR (equal? '("too" "" "much" "" "data")
            (string-split "too  much  data" " " 'strict-infix))
    (fail 'string-split))

(OR (equal? '("" "there" "ya" "go" "")
            (string-split "***there***ya***go***" "***" 'strict-infix))
    (fail 'string-split))

(OR (equal? '() (string-split "" "" 'prefix))
    (fail 'string-split))

(OR (equal? '("a" "b" "c") (string-split "abc" "" 'prefix))
    (fail 'string-split))

(OR (equal? '("too" "" "much" "" "data")
            (string-split "too  much  data" " " 'prefix))
    (fail 'string-split))

(OR (equal? '("there" "ya" "go" "")
            (string-split "***there***ya***go***" "***" 'prefix))
    (fail 'string-split))

(OR (equal? '() (string-split "" "" 'suffix))
    (fail 'string-split))

(OR (equal? '("a" "b" "c") (string-split "abc" "" 'suffix))
    (fail 'string-split))

(OR (equal? '("too" "" "much" "" "data")
            (string-split "too  much  data" " " 'suffix))
    (fail 'string-split))

(OR (equal? '("" "there" "ya" "go")
            (string-split "***there***ya***go***" "***" 'suffix))
    (fail 'string-split))


(OR (equal? '() (string-split "" "" 'infix #f))
    (fail 'string-split))

(OR (equal? '("a" "b" "c") (string-split "abc" "" 'infix #f))
    (fail 'string-split))

(OR (equal? '("too" "" "much" "" "data")
            (string-split "too  much  data" " " 'infix #f))
    (fail 'string-split))

(OR (equal? '("" "there" "ya" "go" "")
            (string-split "***there***ya***go***" "***" 'infix #f))
    (fail 'string-split))

(OR (equal? 'error
            (guard (exn (else 'error))
             (string-split "" "" 'strict-infix #f)))
    (fail 'string-split))

(OR (equal? '("a" "b" "c") (string-split "abc" "" 'strict-infix #f))
    (fail 'string-split))

(OR (equal? '("too" "" "much" "" "data")
            (string-split "too  much  data" " " 'strict-infix #f))
    (fail 'string-split))

(OR (equal? '("" "there" "ya" "go" "")
            (string-split "***there***ya***go***" "***" 'strict-infix #f))
    (fail 'string-split))

(OR (equal? '() (string-split "" "" 'prefix #f))
    (fail 'string-split))

(OR (equal? '("a" "b" "c") (string-split "abc" "" 'prefix #f))
    (fail 'string-split))

(OR (equal? '("too" "" "much" "" "data")
            (string-split "too  much  data" " " 'prefix #f))
    (fail 'string-split))

(OR (equal? '("there" "ya" "go" "")
            (string-split "***there***ya***go***" "***" 'prefix #f))
    (fail 'string-split))

(OR (equal? '() (string-split "" "" 'suffix #f))
    (fail 'string-split))

(OR (equal? '("a" "b" "c") (string-split "abc" "" 'suffix #f))
    (fail 'string-split))

(OR (equal? '("too" "" "much" "" "data")
            (string-split "too  much  data" " " 'suffix #f))
    (fail 'string-split))

(OR (equal? '("" "there" "ya" "go")
            (string-split "***there***ya***go***" "***" 'suffix #f))
    (fail 'string-split))


(OR (equal? 'error
            (guard (exn (else 'error))
             (string-split "" "" 'strict-infix 3)))
    (fail 'string-split))

(OR (equal? '("a" "b" "c") (string-split "abc" "" 'strict-infix 3))
    (fail 'string-split))

(OR (equal? '("too" "" "much" " data")
            (string-split "too  much  data" " " 'strict-infix 3))
    (fail 'string-split))

(OR (equal? '("" "there" "ya" "go***")
            (string-split "***there***ya***go***" "***" 'strict-infix 3))
    (fail 'string-split))

(OR (equal? '() (string-split "" "" 'prefix 3))
    (fail 'string-split))

(OR (equal? '("a" "b" "c") (string-split "abc" "" 'prefix 3))
    (fail 'string-split))

(OR (equal? '("too" "" "much" " data")
            (string-split "too  much  data" " " 'prefix 3))
    (fail 'string-split))

(OR (equal? '("there" "ya" "go***")
            (string-split "***there***ya***go***" "***" 'prefix 3))
    (fail 'string-split))

(OR (equal? '() (string-split "" "" 'suffix 3))
    (fail 'string-split))

(OR (equal? '("a" "b" "c") (string-split "abc" "" 'suffix 3))
    (fail 'string-split))

(OR (equal? '("too" "" "much" " data")
            (string-split "too  much  data" " " 'suffix 3))
    (fail 'string-split))

(OR (equal? '("" "there" "ya" "go***")
            (string-split "***there***ya***go***" "***" 'suffix 3))
    (fail 'string-split))


(OR (equal? 'error
            (guard (exn (else 'error))
             (string-split "" "" 'strict-infix 3 0)))
    (fail 'string-split))

(OR (equal? '("b" "c") (string-split "abc" "" 'strict-infix 3 1))
    (fail 'string-split))

(OR (equal? '("oo" "" "much" " data")
            (string-split "too  much  data" " " 'strict-infix 3 1))
    (fail 'string-split))

(OR (equal? '("**there" "ya" "go" "")
            (string-split "***there***ya***go***" "***" 'strict-infix 3 1))
    (fail 'string-split))

(OR (equal? '() (string-split "" "" 'prefix 3 0))
    (fail 'string-split))

(OR (equal? '("b" "c") (string-split "abc" "" 'prefix 3 1))
    (fail 'string-split))

(OR (equal? '("oo" "" "much" " data")
            (string-split "too  much  data" " " 'prefix 3 1))
    (fail 'string-split))

(OR (equal? '("**there" "ya" "go" "")
            (string-split "***there***ya***go***" "***" 'prefix 3 1))
    (fail 'string-split))

(OR (equal? '() (string-split "" "" 'suffix 3 0))
    (fail 'string-split))

(OR (equal? '("b" "c") (string-split "abc" "" 'suffix 3 1))
    (fail 'string-split))

(OR (equal? '("oo" "" "much" " data")
            (string-split "too  much  data" " " 'suffix 3 1))
    (fail 'string-split))

(OR (equal? '("**there" "ya" "go")
            (string-split "***there***ya***go***" "***" 'suffix 3 1))
    (fail 'string-split))


(OR (equal? 'error
            (guard (exn (else 'error))
             (string-split "" "" 'strict-infix 3 0 0)))
    (fail 'string-split))

(OR (equal? '("b") (string-split "abc" "" 'strict-infix 3 1 2))
    (fail 'string-split))

(OR (equal? '("oo" "" "much" " ")
            (string-split "too  much  data" " " 'strict-infix 3 1 11))
    (fail 'string-split))

(test* "string-split"
       '("oo" "" "much" " ")
       (let ([s "too  much  data"])
         (string-split s " " 'strict-infix 3
                       (string-index->cursor s 1)
                       (string-index->cursor s 11))))

(OR (equal? '() (string-split "" "" 'prefix 3 0 0))
    (fail 'string-split))

(OR (equal? '("b") (string-split "abc" "" 'prefix 3 1 2))
    (fail 'string-split))

(OR (equal? '("oo" "" "much" " ")
            (string-split "too  much  data" " " 'prefix 3 1 11))
    (fail 'string-split))

(OR (equal? '() (string-split "" "" 'suffix 3 0 0))
    (fail 'string-split))

(OR (equal? '("b") (string-split "abc" "" 'suffix 3 1 2))
    (fail 'string-split))

(OR (equal? '("oo" "" "much" " ")
            (string-split "too  much  data" " " 'suffix 3 1 11))
    (fail 'string-split))


(OR (string=? "aiueaaaoi"
              (string-filter (lambda (c) (memv c (string->list "aeiou")))
                             "What is number, that man may know it?"))
    (fail 'string-filter))

(OR (string=? "And wmn, tht sh my knw nmbr?"
              (string-remove (lambda (c) (memv c (string->list "aeiou")))
                             "And woman, that she may know number?"))
    (fail 'string-remove))

(OR (string=? "iueaaaoi"
              (string-filter (lambda (c) (memv c (string->list "aeiou")))
                             "What is number, that man may know it?"
                             4))
    (fail 'string-filter))

(OR (string=? "mn, tht sh my knw nmbr?"
              (string-remove (lambda (c) (memv c (string->list "aeiou")))
                             "And woman, that she may know number?"
                             6))
    (fail 'string-remove))

(OR (string=? "aaao"
              (string-filter (lambda (c) (memv c (string->list "aeiou")))
                             "What is number, that man may know it?"
                             16 32))
    (fail 'string-filter))

(OR (string=? "And woman, that sh may know"
              (string-remove (lambda (c) (memv c (string->list "eiu")))
                             "And woman, that she may know number?"
                             0 28))
    (fail 'string-remove))
