;;; Copyright (C) William D Clinger 2015. All Rights Reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without restriction,
;;; including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;; and to permit persons to whom the Software is furnished to do so,
;;; subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; [SK] test, test-assert and test-deny are commented out - provided by
;;; test/srfi.scm
;;; Gauche doesn't support immutable hashtables, so tests of hash-talbe-mutable?
;;; are commented out.

;;; This is a very shallow sanity test for hash tables.
;;;
;;; Tests marked by a "FIXME: glass-box" comment test behavior of the
;;; reference implementation that is not required by the specification.

(import (scheme base)
        (scheme char)
        (scheme write)
        (only (scheme process-context) exit)
        (scheme comparator)            ; was (srfi 128)
        (only (scheme sort) list-sort) ; was (r6rs sorting)
        (rename (srfi 125)
                (string-hash    deprecated:string-hash)
                (string-ci-hash deprecated:string-ci-hash)))

(define (writeln . xs)
  (for-each write xs)
  (newline))

(define (displayln . xs)
  (for-each display xs)
  (newline))

(define ultimate-exit-status 0)

(define (fail token . more)
  (set! ultimate-exit-status 1)
  (displayln "Error: test failed: ")
  (writeln token)
  (if (not (null? more))
      (for-each writeln more))
  (newline)
  #f)

;;; FIXME: when debugging catastrophic failures, printing every expression
;;; before it's executed may help.

'(define-syntax test
  (syntax-rules ()
   ((_ expr expected)
    (let ()
;     (write 'expr) (newline)
      (let ((actual expr))
        (or (equal? actual expected)
            (fail 'expr actual expected)))))))

'(define-syntax test-assert
  (syntax-rules ()
   ((_ expr)
    (or expr (fail 'expr)))))

'(define-syntax test-deny
  (syntax-rules ()
   ((_ expr)
    (or (not expr) (fail 'expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Transition from SRFI 114 to SRFI 128.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define default-comparator (make-default-comparator))

;;; SRFI 128 says the following definition will work, but that's
;;; an error in SRFI 128; the hash function produce non-integers.

#;
(define number-comparator
  (make-comparator real? = < (lambda (x) (exact (abs x)))))

(define number-comparator
  (make-comparator real? = < (lambda (x) (exact (abs (round x))))))

(define string-comparator
  (make-comparator string? string=? string<? string-hash))

(define string-ci-comparator
  (make-comparator string? string-ci=? string-ci<? string-ci-hash))

(define eq-comparator (make-eq-comparator))

(define eqv-comparator (make-eqv-comparator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Transition from earlier draft of SRFI 125 to this draft.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Returns an immutable hash table.

(define (hash-table-tabulate comparator n proc)
  (let ((ht (make-hash-table comparator)))
    (do ((i 0 (+ i 1)))
        ((= i n)
         (hash-table-copy ht))
      (call-with-values
       (lambda ()
         (proc i))
       (lambda (key val)
         (hash-table-set! ht key val))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Constructors.

(define ht-default (make-hash-table default-comparator))

(define ht-eq (make-hash-table eq-comparator 'random-argument "another"))

(define ht-eqv (make-hash-table eqv-comparator))

(define ht-eq2 (make-hash-table eq?))

(define ht-eqv2 (make-hash-table eqv?))

(define ht-equal (make-hash-table equal?))

(define ht-string (make-hash-table string=?))

(define ht-string-ci (make-hash-table string-ci=?))

(define ht-symbol (make-hash-table symbol=?))    ; FIXME: glass-box

(define ht-fixnum (make-hash-table = abs))

(define ht-default2
  (hash-table default-comparator 'foo 'bar 101.3 "fever" '(x y z) '#()))

(define ht-fixnum2
  (hash-table-tabulate number-comparator
                       10
                       (lambda (i) (values (* i i) i))))

(define ht-string2
  (hash-table-unfold (lambda (s) (= 0 (string-length s)))
                     (lambda (s) (values s (string-length s)))
                     (lambda (s) (substring s 0 (- (string-length s) 1)))
                     "prefixes"
                     string-comparator
                     'ignored1 'ignored2 "ignored3" '#(ignored 4 5)))

(define ht-string-ci2
  (alist->hash-table '(("" . 0) ("Mary" . 4) ("Paul" . 4) ("Peter" . 5))
                     string-ci-comparator
                     "ignored1" 'ignored2))

(define ht-symbol2
  (alist->hash-table '((mary . travers) (noel . stookey) (peter .yarrow))
                     eq?))

(define ht-equal2
  (alist->hash-table '(((edward) . abbey)
                       ((dashiell) . hammett)
                       ((edward) . teach)
                       ((mark) . twain))
                     equal?
                     (comparator-hash-function default-comparator)))

(define test-tables
  (list ht-default   ht-default2   ; initial keys: foo, 101.3, (x y z)
        ht-eq        ht-eq2        ; initially empty
        ht-eqv       ht-eqv2       ; initially empty
        ht-equal     ht-equal2     ; initial keys: (edward), (dashiell), (mark)
        ht-string    ht-string2    ; initial keys: "p, "pr", ..., "prefixes"
        ht-string-ci ht-string-ci2 ; initial keys: "", "Mary", "Paul", "Peter"
        ht-symbol    ht-symbol2    ; initial keys: mary, noel, peter
        ht-fixnum    ht-fixnum2))  ; initial keys: 0, 1, 4, 9, ..., 81

;;; Predicates

(test (map hash-table?
           (cons '#()
                 (cons default-comparator
                       test-tables)))
      (append '(#f #f) (map (lambda (x) #t) test-tables)))

(test (map hash-table-contains?
           test-tables
           '(foo 101.3
             x "y"
             (14 15) #\newline
             (edward) (mark)
             "p" "pref"
             "mike" "PAUL"
             jane noel
             0 4))
      '(#f #t #f #f #f #f #f #t #f #t #f #t #f #t #f #t))

(test (map hash-table-contains?
           test-tables
           '(#u8() 47.9
             '#() '()
             foo bar
             19 (henry)
             "p" "perp"
             "mike" "Noel"
             jane paul
             0 5))
      (map (lambda (x) #f) test-tables))

(test (map hash-table-empty? test-tables)
      '(#t #f #t #t #t #t #t #f #t #f #t #f #t #f #t #f))

(test (map (lambda (ht1 ht2) (hash-table=? default-comparator ht1 ht2))
           test-tables
           test-tables)
      (map (lambda (x) #t) test-tables))

;; [SK] This depends on hash-table=? implementation specifics
'(test (map (lambda (ht1 ht2) (hash-table=? default-comparator ht1 ht2))
           test-tables
           (do ((tables (reverse test-tables) (cddr tables))
                (rev '() (cons (car tables) (cons (cadr tables) rev))))
               ((null? tables)
                rev)))
      '(#f #f #t #t #t #t #f #f #f #f #f #f #f #f #f #f))

'(test (map hash-table-mutable? test-tables)
      '(#t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #f))

;;; FIXME: glass-box

'(test (map hash-table-mutable? (map hash-table-copy test-tables))
      (map (lambda (x) #f) test-tables))

'(test (hash-table-mutable? (hash-table-copy ht-fixnum2 #t))
      #t)

;;; Accessors.

;;; FIXME: glass-box (implementations not required to raise an exception here)

(test (map (lambda (ht)
             (guard (exn
                     (else 'err))
              (hash-table-ref ht 'not-a-key)))
           test-tables)
      (map (lambda (ht) 'err) test-tables))

;;; FIXME: glass-box (implementations not required to raise an exception here)

(test (map (lambda (ht)
             (guard (exn
                     (else 'err))
              (hash-table-ref ht 'not-a-key (lambda () 'err))))
           test-tables)
      (map (lambda (ht) 'err) test-tables))

;;; FIXME: glass-box (implementations not required to raise an exception here)

(test (map (lambda (ht)
             (guard (exn
                     (else 'err))
              (hash-table-ref ht 'not-a-key (lambda () 'err) values)))
           test-tables)
      (map (lambda (ht) 'err) test-tables))

(test (map (lambda (ht key)
             (guard (exn
                     (else 'err))
              (hash-table-ref ht key)))
           test-tables
           '(foo 101.3
             x "y"
             (14 15) #\newline
             (edward) (mark)
             "p" "pref"
             "mike" "PAUL"
             jane noel
             0 4))
      '(err "fever" err err err err err twain err 4 err 4 err stookey err 2))

(test (map (lambda (ht key)
             (guard (exn
                     (else 'err))
              (hash-table-ref ht key (lambda () 'eh))))
           test-tables
           '(foo 101.3
             x "y"
             (14 15) #\newline
             (edward) (mark)
             "p" "pref"
             "mike" "PAUL"
             jane noel
             0 4))
      '(eh "fever" eh eh eh eh eh twain eh 4 eh 4 eh stookey eh 2))

(test (map (lambda (ht key)
             (guard (exn
                     (else 'err))
              (hash-table-ref ht key (lambda () 'eh) list)))
           test-tables
           '(foo 101.3
             x "y"
             (14 15) #\newline
             (edward) (mark)
             "p" "pref"
             "mike" "PAUL"
             jane noel
             0 4))
      '(eh ("fever") eh eh eh eh eh (twain) eh (4) eh (4) eh (stookey) eh (2)))

;;; FIXME: glass-box (implementations not required to raise an exception here)

(test (map (lambda (ht)
             (guard (exn
                     (else 'eh))
              (hash-table-ref/default ht 'not-a-key 'eh)))
           test-tables)
      (map (lambda (ht) 'eh) test-tables))

(test (map (lambda (ht key)
             (guard (exn
                     (else 'err))
              (hash-table-ref/default ht key 'eh)))
           test-tables
           '(foo 101.3
             x "y"
             (14 15) #\newline
             (edward) (mark)
             "p" "pref"
             "mike" "PAUL"
             jane noel
             0 4))
      '(eh "fever" eh eh eh eh eh twain eh 4 eh 4 eh stookey eh 2))

(test (begin (hash-table-set! ht-fixnum)
             (list-sort < (hash-table-keys ht-fixnum)))
      '())

(test (begin (hash-table-set! ht-fixnum 121 11 144 12 169 13)
             (list-sort < (hash-table-keys ht-fixnum)))
      '(121 144 169))

(test (begin (hash-table-set! ht-fixnum
                              0 0 1 1 4 2 9 3 16 4 25 5 36 6 49 7 64 8 81 9)
             (list-sort < (hash-table-keys ht-fixnum)))
      '(0 1 4 9 16 25 36 49 64 81 121 144 169))

(test (map (lambda (i) (hash-table-ref/default ht-fixnum i 'error))
           '(169 144 121 0 1 4 9 16 25 36 49 64 81))
      '(13 12 11 0 1 2 3 4 5 6 7 8 9))

(test (begin (hash-table-delete! ht-fixnum)
             (map (lambda (i) (hash-table-ref/default ht-fixnum i 'error))
                  '(169 144 121 0 1 4 9 16 25 36 49 64 81)))
      '(13 12 11 0 1 2 3 4 5 6 7 8 9))

(test (begin (hash-table-delete! ht-fixnum 1 9 25 49 81 200 121 169 81 1)
             (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
                  '(169 144 121 0 1 4 9 16 25 36 49 64 81)))
      '(-1 12 -1 0 -1 2 -1 4 -1 6 -1 8 -1))

(test (begin (hash-table-delete! ht-fixnum 200 100 0 81 36)
             (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
                  '(169 144 121 0 1 4 9 16 25 36 49 64 81)))
      '(-1 12 -1 -1 -1 2 -1 4 -1 -1 -1 8 -1))

(test (begin (hash-table-intern! ht-fixnum 169 (lambda () 13))
             (hash-table-intern! ht-fixnum 121 (lambda () 11))
             (hash-table-intern! ht-fixnum   0 (lambda ()  0))
             (hash-table-intern! ht-fixnum   1 (lambda ()  1))
             (hash-table-intern! ht-fixnum   1 (lambda () 99))
             (hash-table-intern! ht-fixnum 121 (lambda () 66))
             (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
                  '(169 144 121 0 1 4 9 16 25 36 49 64 81)))
      '(13 12 11 0 1 2 -1 4 -1 -1 -1 8 -1))

(test (list-sort (lambda (v1 v2) (< (vector-ref v1 0) (vector-ref v2 0)))
                 (hash-table-map->list vector ht-fixnum))
      '(#(0 0) #(1 1) #(4 2) #(16 4) #(64 8) #(121 11) #(144 12) #(169 13)))

(test (begin (hash-table-prune! (lambda (key val)
                                  (and (odd? key) (> val 10)))
                                ht-fixnum)
             (list-sort (lambda (l1 l2)
                          (< (car l1) (car l2)))
                        (hash-table-map->list list ht-fixnum)))
      '((0 0) (1 1) (4 2) (16 4) (64 8) #;(121 11) (144 12) #;(169 13)))

(test (begin (hash-table-intern! ht-fixnum 169 (lambda () 13))
             (hash-table-intern! ht-fixnum 144 (lambda () 9999))
             (hash-table-intern! ht-fixnum 121 (lambda () 11))
             (list-sort (lambda (l1 l2)
                          (< (car l1) (car l2)))
                        (hash-table-map->list list ht-fixnum)))
      '((0 0) (1 1) (4 2) (16 4) (64 8) (121 11) (144 12) (169 13)))

(test (begin (hash-table-update! ht-fixnum 9 length (lambda () '(a b c)))
             (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
                  '(169 144 121 0 1 4 9 16 25 36 49 64 81)))
      '(13 12 11 0 1 2 3 4 -1 -1 -1 8 -1))

(test (begin (hash-table-update! ht-fixnum 16 -)
             (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
                  '(169 144 121 0 1 4 9 16 25 36 49 64 81)))
      '(13 12 11 0 1 2 3 -4 -1 -1 -1 8 -1))

(test (begin (hash-table-update! ht-fixnum 16 - abs)
             (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
                  '(169 144 121 0 1 4 9 16 25 36 49 64 81)))
      '(13 12 11 0 1 2 3 4 -1 -1 -1 8 -1))

(test (begin (hash-table-update!/default ht-fixnum 25 - 5)
             (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
                  '(169 144 121 0 1 4 9 16 25 36 49 64 81)))
      '(13 12 11 0 1 2 3 4 -5 -1 -1 8 -1))

(test (begin (hash-table-update!/default ht-fixnum 25 - 999)
             (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
                  '(169 144 121 0 1 4 9 16 25 36 49 64 81)))
      '(13 12 11 0 1 2 3 4 5 -1 -1 8 -1))

(test (let* ((n0 (hash-table-size ht-fixnum))
             (ht (hash-table-copy ht-fixnum #t)))
        (call-with-values
         (lambda () (hash-table-pop! ht))
         (lambda (key val)
           (list (= key (* val val))
                 (= (- n0 1) (hash-table-size ht))))))
      '(#t #t))

(test (begin (hash-table-delete! ht-fixnum 75)
             (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
                  '(169 144 121 0 1 4 9 16 25 36 49 64 75 81)))
      '(13 12 11 0 1 2 3 4 5 -1 -1 8 -1 -1))

(let ((ht-eg (hash-table number-comparator 1 1 4 2 9 3 16 4 25 5 64 8)))
  (test (hash-table-delete! ht-eg)
        0)
  (test (hash-table-delete! ht-eg 2 7 2000)
        0)
  (test (hash-table-delete! ht-eg 1 2 4 7 64 2000)
        3)
  (test-assert (= 3 (length (hash-table-keys ht-eg)))))

(test (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
           '(169 144 121 0 1 4 9 16 25 36 49 64 81))
      '(13 12 11 0 1 2 3 4 5 -1 -1 8 -1))

(test (begin (hash-table-set! ht-fixnum 36 6)
             (hash-table-set! ht-fixnum 81 9)
             (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
                  '(169 144 121 0 1 4 9 16 25 36 49 64 81)))
      '(13 12 11 0 1 2 3 4 5 6 -1 8 9))

(test (begin (hash-table-clear! ht-eq)
             (hash-table-size ht-eq))
      0)

;;; The whole hash table.

(test (begin (hash-table-set! ht-eq 'foo 13 'bar 14 'baz 18)
             (hash-table-size ht-eq))
      3)

(test (let* ((ht (hash-table-empty-copy ht-eq))
             (n0 (hash-table-size ht))
             (ignored (hash-table-set! ht 'foo 13 'bar 14 'baz 18))
             (n1 (hash-table-size ht)))
        (list n0 n1 (hash-table=? default-comparator ht ht-eq)))
      '(0 3 #t))

(test (begin (hash-table-clear! ht-eq)
             (hash-table-size ht-eq))
      0)

(test (hash-table-find (lambda (key val)
                         (if (= 144 key (* val val))
                             (list key val)
                             #f))
                       ht-fixnum
                       (lambda () 99))
      '(144 12))

(test (hash-table-find (lambda (key val)
                         (if (= 144 key val)
                             (list key val)
                             #f))
                       ht-fixnum
                       (lambda () 99))
      99)

(test (hash-table-count <= ht-fixnum)
      2)

;;; Mapping and folding.

(test (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
           '(0 1 4 9 16 25 36 49 64 81 100 121 144 169 196))
      '(0 1 2 3 4 5 6 -1 8 9 -1 11 12 13 -1))

(test (let ((ht (hash-table-map (lambda (val) (* val val))
                                eqv-comparator
                                ht-fixnum)))
        (map (lambda (i) (hash-table-ref/default ht i -1))
             '(0 1 4 9 16 25 36 49 64 81 100 121 144 169 196)))
      '(0 1 4 9 16 25 36 -1 64 81 -1 121 144 169 -1))

(test (let ((keys (make-vector 15 -1))
            (vals (make-vector 15 -1)))
        (hash-table-for-each (lambda (key val)
                               (vector-set! keys val key)
                               (vector-set! vals val val))
                             ht-fixnum)
        (list keys vals))
      '(#(0 1 4 9 16 25 36 -1 64 81 -1 121 144 169 -1)
        #(0 1 2 3  4  5  6 -1  8  9 -1  11  12  13 -1)))

(test (begin (hash-table-map! (lambda (key val)
                                (if (<= 10 key)
                                    (- val)
                                    val))
                              ht-fixnum)
             (map (lambda (i) (hash-table-ref/default ht-fixnum i -1))
                  '(0 1 4 9 16 25 36 49 64 81 100 121 144 169 196)))
      '(0 1 2 3 -4 -5 -6 -1 -8 -9 -1 -11 -12 -13 -1))

(test (hash-table-fold (lambda (key val acc)
                         (+ val acc))
                       0
                       ht-string-ci2)
      13)

(test (list-sort < (hash-table-fold (lambda (key val acc)
                                      (cons key acc))
                                    '()
                                    ht-fixnum))
      '(0 1 4 9 16 25 36 64 81 121 144 169))

;;; Copying and conversion.

(test (hash-table=? number-comparator ht-fixnum (hash-table-copy ht-fixnum))
      #t)

(test (hash-table=? number-comparator ht-fixnum (hash-table-copy ht-fixnum #f))
      #t)

(test (hash-table=? number-comparator ht-fixnum (hash-table-copy ht-fixnum #t))
      #t)

'(test (hash-table-mutable? (hash-table-copy ht-fixnum))
      #f)

'(test (hash-table-mutable? (hash-table-copy ht-fixnum #f))
      #f)

'(test (hash-table-mutable? (hash-table-copy ht-fixnum #t))
      #t)

(test (hash-table->alist ht-eq)
      '())

(test (list-sort (lambda (x y) (< (car x) (car y)))
                 (hash-table->alist ht-fixnum))
      '((0 . 0)
        (1 . 1)
        (4 . 2)
        (9 . 3)
        (16 . -4)
        (25 . -5)
        (36 . -6)
        (64 . -8)
        (81 . -9)
        (121 . -11)
        (144 . -12)
        (169 . -13)))

;;; Hash tables as sets.

(test (begin (hash-table-union! ht-fixnum ht-fixnum2)
             (list-sort (lambda (x y) (< (car x) (car y)))
                        (hash-table->alist ht-fixnum)))
      '((0 . 0)
        (1 . 1)
        (4 . 2)
        (9 . 3)
        (16 . -4)
        (25 . -5)
        (36 . -6)
        (49 . 7)
        (64 . -8)
        (81 . -9)
        (121 . -11)
        (144 . -12)
        (169 . -13)))

(test (let ((ht (hash-table-copy ht-fixnum2 #t)))
        (hash-table-union! ht ht-fixnum)
        (list-sort (lambda (x y) (< (car x) (car y)))
                   (hash-table->alist ht)))
      '((0 . 0)
        (1 . 1)
        (4 . 2)
        (9 . 3)
        (16 . 4)
        (25 . 5)
        (36 . 6)
        (49 . 7)
        (64 . 8)
        (81 . 9)
        (121 . -11)
        (144 . -12)
        (169 . -13)))

;; [SK] These two tests depends on the implementation wrt. hashtables of
;; different key comparator can be equal.
'(test (begin (hash-table-union! ht-eqv2 ht-fixnum)
             (hash-table=? default-comparator ht-eqv2 ht-fixnum))
      #t)

'(test (begin (hash-table-intersection! ht-eqv2 ht-fixnum)
             (hash-table=? default-comparator ht-eqv2 ht-fixnum))
      #t)

(test (begin (hash-table-intersection! ht-eqv2 ht-eqv)
             (hash-table-empty? ht-eqv2))
      #t)

(test (begin (hash-table-intersection! ht-fixnum ht-fixnum2)
             (list-sort (lambda (x y) (< (car x) (car y)))
                        (hash-table->alist ht-fixnum)))
      '((0 . 0)
        (1 . 1)
        (4 . 2)
        (9 . 3)
        (16 . -4)
        (25 . -5)
        (36 . -6)
        (49 . 7)
        (64 . -8)
        (81 . -9)))

(test (begin (hash-table-intersection!
              ht-fixnum
              (alist->hash-table '((-1 . -1) (4 . 202) (25 . 205) (100 . 10))
                                 number-comparator))
             (list-sort (lambda (x y) (< (car x) (car y)))
                        (hash-table->alist ht-fixnum)))
      '((4 . 2)
        (25 . -5)))

(test (let ((ht (hash-table-copy ht-fixnum2 #t)))
        (hash-table-difference!
         ht
         (alist->hash-table '((-1 . -1) (4 . 202) (25 . 205) (100 . 10))
                            number-comparator))
        (list-sort (lambda (x y) (< (car x) (car y)))
                   (hash-table->alist ht)))
      '((0 . 0)
        (1 . 1)
        (9 . 3)
        (16 . 4)
        (36 . 6)
        (49 . 7)
        (64 . 8)
        (81 . 9)))

(test (let ((ht (hash-table-copy ht-fixnum2 #t)))
        (hash-table-xor!
         ht
         (alist->hash-table '((-1 . -1) (4 . 202) (25 . 205) (100 . 10))
                            number-comparator))
        (list-sort (lambda (x y) (< (car x) (car y)))
                   (hash-table->alist ht)))
      '((-1 . -1)
        (0 . 0)
        (1 . 1)
        (9 . 3)
        (16 . 4)
        (36 . 6)
        (49 . 7)
        (64 . 8)
        (81 . 9)
        (100 . 10)))

(test (guard (exn
              (else 'key-not-found))
       (hash-table-ref ht-default "this key won't be present"))
      'key-not-found)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Desultory tests of deprecated procedures and usages.
;;; Deprecated usage of make-hash-table and alist->hash-table
;;; has already been tested above.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (let* ((x (list 1 2 3))
             (y (cons 1 (cdr x)))
             (h1 (hash x))
             (h2 (hash y)))
        (list (exact-integer? h1)
              (exact-integer? h2)
              (= h1 h2)))
      '(#t #t #t))

(test (let* ((x "abcd")
             (y (string-append "ab" "cd"))
             (h1 (deprecated:string-hash x))
             (h2 (deprecated:string-hash y)))
        (list (exact-integer? h1)
              (exact-integer? h2)
              (= h1 h2)))
      '(#t #t #t))

(test (let* ((x "Hello There!")
             (y "hello THERE!")
             (h1 (deprecated:string-ci-hash x))
             (h2 (deprecated:string-ci-hash y)))
        (list (exact-integer? h1)
              (exact-integer? h2)
              (= h1 h2)))
      '(#t #t #t))

(test (let* ((x '#(a "bcD" #\c (d 2.718) -42 #u8() #() #u8(19 20)))
             (y x)
             (h1 (hash-by-identity x))
             (h2 (hash-by-identity y)))
        (list (exact-integer? h1)
              (exact-integer? h2)
              (= h1 h2)))
      '(#t #t #t))

(test (let* ((x (list 1 2 3))
             (y (cons 1 (cdr x)))
             (h1 (hash x 60))
             (h2 (hash y 60)))
        (list (exact-integer? h1)
              (exact-integer? h2)
              (= h1 h2)))
      '(#t #t #t))

(test (let* ((x "abcd")
             (y (string-append "ab" "cd"))
             (h1 (deprecated:string-hash x 97))
             (h2 (deprecated:string-hash y 97)))
        (list (exact-integer? h1)
              (exact-integer? h2)
              (= h1 h2)))
      '(#t #t #t))

(test (let* ((x "Hello There!")
             (y "hello THERE!")
             (h1 (deprecated:string-ci-hash x 101))
             (h2 (deprecated:string-ci-hash y 101)))
        (list (exact-integer? h1)
              (exact-integer? h2)
              (= h1 h2)))
      '(#t #t #t))

(test (let* ((x '#(a "bcD" #\c (d 2.718) -42 #u8() #() #u8(19 20)))
             (y x)
             (h1 (hash-by-identity x 102))
             (h2 (hash-by-identity y 102)))
        (list (exact-integer? h1)
              (exact-integer? h2)
              (= h1 h2)))
      '(#t #t #t))

(test (let ((f (hash-table-equivalence-function ht-fixnum)))
        (if (procedure? f)
            (f 34 34)
            #t))
      #t)

(test (let ((f (hash-table-hash-function ht-fixnum)))
        (if (procedure? f)
            (= (f 34) (f 34))
            #t))
      #t)

(test (map (lambda (key) (hash-table-exists? ht-fixnum2 key))
           '(0 1 2 3 4 5 6 7 8 9 10))
      '(#t #t #f #f #t #f #f #f #f #t #f))

(test (let ((n 0))
        (hash-table-walk ht-fixnum2
                         (lambda (key val) (set! n (+ n key))))
        n)
      (apply +
             (map (lambda (x) (* x x))
                  '(0 1 2 3 4 5 6 7 8 9))))

(test (list-sort < (hash-table-fold ht-fixnum2
                                    (lambda (key val acc)
                                      (cons key acc))
                                    '()))
      '(0 1 4 9 16 25 36 49 64 81))

(test (let ((ht (hash-table-copy ht-fixnum2 #t))
            (ht2 (hash-table number-comparator
                             .25 .5 64 9999 81 9998 121 -11 144 -12)))
        (hash-table-merge! ht ht2)
        (list-sort (lambda (x y) (< (car x) (car y)))
                   (hash-table->alist ht)))
      '((0 . 0)
        (.25 . .5)
        (1 . 1)
        (4 . 2)
        (9 . 3)
        (16 . 4)
        (25 . 5)
        (36 . 6)
        (49 . 7)
        (64 . 8)
        (81 . 9)
        (121 . -11)
        (144 . -12)))

;;; Bugs reported on 5 January 2019 by Jéssica Milaré
;;; ( https://srfi-email.schemers.org/srfi-125/msg/10177551 )

;;; Spec says hash-table returns an immutable hash table (if that
;;; is supported) and signal an error if there are duplicate keys,
;;; but standard implementation returns a mutable hash table and
;;; signals no error with duplicate keys.
;;;
;;; Comment by Will Clinger: the spec says specifying a duplicate
;;; key "is an error", so hash-table is not required to signal an
;;; error when there are duplicate keys.  That part of the spec
;;; was added on 8 May 2016, which is why it was not implemented
;;; by the sample implementation of 2 May 2016.  Because a duplicate
;;; key "is an error" rather than "signals an error", testing for
;;; that situation is glass-box, as is testing for immutability.

;;; FIXME: glass-box

'(test (hash-table-mutable?
       (hash-table number-comparator
                   .25 .5 64 9999 81 9998 121 -11 144 -12))
      #f)

;;; FIXME: glass-box (implementations not required to raise an exception here)

'(test (guard (exn
              (else 'eh))
       (hash-table number-comparator .25 .5 .25 -.5))
      'eh)

;;; Spec says hash-table-set! must go left to right, but in
;;; standard implementation it goes right to left.
;;;
;;; Comment by Will Clinger: the left-to-right requirement was
;;; added to the spec on 8 May 2016, which is why it was not
;;; implemented by the sample implementation of 2 May 2016.

(test (let* ((ht (hash-table-empty-copy ht-eq))
             (ignored (hash-table-set! ht 'foo 13 'bar 14 'foo 18)))
        (hash-table-ref ht 'foo))
      18)

;;; Spec says hash-table-empty-copy returns a mutable hash table,
;;; but in standard implementation it returns an immutable hash
;;; table if the given hash table is immutable.

;;; FIXME: glass-box (immutable tables need not be supported)

'(test (hash-table-mutable?
       (hash-table number-comparator))
      #f)

'(test (hash-table-mutable?
       (hash-table-empty-copy
        (hash-table-copy (hash-table number-comparator) #f)))
      #t)

;;; hash-table-delete! seems to loop infinitely once it finds a key.
;;;
;;; Comment by Will Clinger: that bug was added by
;;; commit e17c15203a934ab741300e59619f880f363c2b2f
;;; on 26 September 2018.  I do not understand the purpose of that
;;; commit, as its one change appears to have had no substantive
;;; effect apart from inserting this bug.

(test (let* ((ht
              (hash-table default-comparator 'foo 1 'bar 2 'baz 3))
             (ht (hash-table-copy ht #t)))
        (hash-table-delete! ht 'foo)
        (hash-table-size ht))
      2)

(displayln "Done.")

(exit ultimate-exit-status)

; eof
