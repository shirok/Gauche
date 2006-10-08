;; test for write/ss and read/ss
;;
;; this test is splitted from io.scm, since this one uses util.isomorph,
;; and has to be done after the test of util.* module.

;; $Id: io2.scm,v 1.4 2006-10-08 03:59:42 shirok Exp $

(use gauche.test)
(use srfi-1)
(use util.isomorph)

(test-start "write/ss and read/ss")

;;---------------------------------------------------------------
(test-section "write/ss basic")

(test* "pair" "(#0=(a b) #0#)"
       (let1 x '(a b)
         (write-to-string (list x x) write/ss)))
(test* "pair" "(#0=(a b) . #0#)"
       (let1 x (list 'a 'b)
         (write-to-string (cons x x) write/ss)))
(test* "pair" "(#0=(a b) #1=(a b) #0# . #1#)"
       (let ((x (list 'a 'b))
             (y (list 'a 'b)))
         (write-to-string (list* x y x y) write/ss)))
(test* "pair (circular)" "#0=(a . #0#)"
       (let1 x (list 'a 'b)
         (set-cdr! x x)
         (write-to-string x write/ss)))
(test* "pair (circular)" "#0=(#0# b)"
       (let1 x (list 'a 'b)
         (set-car! x x)
         (write-to-string x write/ss)))
(test* "pair (circular)" "#0=(#0# . #0#)"
       (let1 x (list 'a 'b)
         (set-car! x x)
         (set-cdr! x x)
         (write-to-string x write/ss)))
(test* "pair (circular)" "#0=(a (b . #0#))"
       (let1 x (list 'a (list 'b 'c))
         (set-cdr! (cadr x) x)
         (write-to-string x write/ss)))
(test* "pair (circular)" "#0=(a #1=(b . #0#) . #1#)"
       (let1 x (list 'a (list 'b 'c))
         (set-cdr! (cadr x) x)
         (set-cdr! (cdr x) (cadr x))
         (write-to-string x write/ss)))

(test* "vector" "(#0=#(a b) . #0#)"
       (let1 x (vector 'a 'b)
         (write-to-string (cons x x) write/ss)))
(test* "vector" "(#() . #())"
       (let1 x (vector)
         (write-to-string (cons x x) write/ss)))
(test* "vector" "#(#0=(a b) #0# #0#)"
       (let1 x '(a b)
         (write-to-string (vector x x x) write/ss)))
(test* "vector (circular)" "#0=#(#0#)"
       (let1 x (vector 0)
         (vector-set! x 0 x)
         (write-to-string x write/ss)))

(test* "string" "(#0=\"ab\" . #0#)"
       (let1 x "ab"
         (write-to-string (cons x x) write/ss)))
(test* "string" "(\"\" . \"\")"
       (let1 x ""
         (write-to-string (cons x x) write/ss)))

(test* "more than 10 substructures"
       "(#0=(a) #1=(b) #2=(c) #3=(d) #4=(e) #5=(f) #6=(g) #7=(h) #8=(i) #9=(j) #10=(k) #10# #9# #8# #7# #6# #5# #4# #3# #2# #1# #0#)"
       (let ((a '(a)) (b '(b)) (c '(c)) (d '(d)) (e '(e))
             (f '(f)) (g '(g)) (h '(h)) (i '(i)) (j '(j)) (k '(k)))
         (write-to-string
          (list a b c d e f g h i j k
                k j i h g f e d c b a)
          write/ss)))

(define-class <foo> ()
  ((a :init-keyword :a)
   (b :init-keyword :b)))
(define-method write-object ((self <foo>) port)
  (format port "#,(foo ~s ~s)" (ref self 'a) (ref self 'b)))

(test* "user defined" "#,(foo #0=(a b) #0#)"
       (let* ((x '(a b))
              (foo (make <foo> :a x :b x)))
         (write-to-string foo write/ss)))
(test* "user defined" "#0=#,(foo #0# #0#)"
       (let ((foo (make <foo> :a #f :b #f)))
         (set! (ref foo 'a) foo)
         (set! (ref foo 'b) foo)
         (write-to-string foo write/ss)))
(test* "user defined" "#0=#,(foo foo #,(foo bar #0#))"
       (let* ((foo (make <foo> :a 'foo :b #f))
              (bar (make <foo> :a 'bar :b foo)))
         (set! (ref foo 'b) bar)
         (write-to-string foo write/ss)))
(test* "user defined" "(#0=#,(foo foo #1=#,(foo bar #0#)) #1#)"
       (let* ((foo (make <foo> :a 'foo :b #f))
              (bar (make <foo> :a 'bar :b foo)))
         (set! (ref foo 'b) bar)
         (write-to-string (list foo bar) write/ss)))
(test* "user defined" "#0=(#1=#,(foo #2=#,(foo bar #1#) #0#) #2#)"
       (let* ((foo (make <foo> :a 'foo :b #f))
              (bar (make <foo> :a 'bar :b foo))
              (baz (list foo bar)))
         (set! (ref foo 'a) bar)
         (set! (ref foo 'b) baz)
         (write-to-string baz write/ss)))

;; write/ss with user-defined write-object method.
;; test by UEYAMA Rui
(define-class <bar> ()
  ((a :init-keyword :a)
   (b :init-keyword :b)))
(define-method write-object ((self <bar>) port)
  (display "#,(bar " port)
  (write/ss (ref self 'a) port)
  (display " " port)
  (write/ss (ref self 'b) port)
  (display ")" port))
(test* "user defined" "#,(bar #0=(a b) #0#)"
       (let* ((x '(a b))
              (bar (make <bar> :a x :b x)))
         (write-to-string bar write/ss)))

;;---------------------------------------------------------------
(test-section "format/ss")

(test* "format/ss" "The answer is #0=(\"a\" . #0#)"
       (let ((a (list "a")))
         (set-cdr! a a)
         (format/ss "The answer is ~s" a)))

(test* "format/ss" "The answer is #0=(a . #0#)"
       (let ((a (list "a")))
         (set-cdr! a a)
         (format/ss "The answer is ~a" a)))

(test* "format/ss" "The answer is #0=(a . #0#) #0=(a . #0#)"
       (let ((a (list 'a)))
         (set-cdr! a a)
         (format/ss "The answer is ~s ~s" a a)))

;;---------------------------------------------------------------
(test-section "read/ss basic")

;; NB: in gauche, read/ss is just an alias of read.
(test* "scalar (harmless)" 0
       (read-from-string "#0=0"))
(test* "scalar (harmless)" 1
       (read-from-string "#1=1"))
(test* "scalar (harmless)" 2
       (read-from-string "#0=#1=2"))
(test* "scalar (harmless)" #f
       (read-from-string "#1=#10=#100=#f"))
(test* "scalar (harmless)" "aaa"
       (read-from-string "#1=#0=\"aaa\""))

(test* "bad syntax" *test-error*
       (read-from-string "#1"))
(test* "bad syntax" *test-error*
       (read-from-string "#3#"))
(test* "bad syntax" *test-error*
       (read-from-string "#99999999999999999999999999999999999=3"))
(test* "bad syntax" *test-error*
       (read-from-string "#99999999999999999999999999999999999#"))

(test* "pair 1" (circular-list 1 2)
       (read-from-string "#0=(1 2 . #0#)")
       isomorphic?)
(test* "pair 2" (let1 r (list #f) (set! (car r) r) r)
       (read-from-string "#0=(#0#)")
       isomorphic?)
(test* "pair 3" (let1 r '(a b) (list r r r))
       (read-from-string "(#0=#1=(a b) #0# #1#)")
       isomorphic?)

(test* "vector" (let* ((r (vector 'a 'b))
                       (s (vector 'c 'd))
                       (t (vector r s r s 'e)))
                  (vector-set! r 1 s)
                  (vector-set! s 1 r)
                  (vector-set! t 4 t)
                  t)
       (read-from-string "#0=#(#1=#(a #2=#(c #1#)) #2# #1# #2# #0#)")
       isomorphic?)

(test* "string" (let* ((r (string #\a #\a))
                       (s (string #\a #\a)))
                  (list r s r s))
       (read-from-string "(#0=\"aa\" #1=\"aa\" #0# #1#)")
       isomorphic?)

;; NB: this is an experimental feature.  Do not count on this API!
(define-reader-ctor 'foo
  (lambda x `(quote ,x))
  (lambda (obj)
    (pair-for-each (lambda (p)
                     (when (read-reference? (car p))
                       (set-car! p (read-reference-value (car p)))))
                   (cadr obj))))

(test* "user-defined" '#0='(a #0#)
       (read-from-string "#0=#,(foo a #0#)")
       isomorphic?)

(test-end)
