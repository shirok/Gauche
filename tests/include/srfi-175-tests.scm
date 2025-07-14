;; Copyright 2019 Lassi Kortela
;; SPDX-License-Identifier: MIT

'(import (scheme base) (scheme file) (scheme read) (scheme write) (srfi 175))

'(define-syntax want
  (syntax-rules ()
    ((_ right-answer (proc args ...))
     (unless (equal? right-answer (proc args ...))
       (display "Failed: wanted ")
       (write right-answer)
       (display " but got ")
       (write (proc args ...))
       (display " from ")
       (display '(proc args ...))
       (newline)))))

(want #f (ascii-codepoint? -1))
(want #t (ascii-codepoint? 0))
(want #t (ascii-codepoint? #x7f))
(want #f (ascii-codepoint? #x80))

(want #t (ascii-char? (integer->char 0)))
(want #t (ascii-char? (integer->char #x7f)))
(want #f (ascii-char? (integer->char #x80)))

(want #t (ascii-string? ""))
(want #t (ascii-string? "a"))
(want #t (ascii-string? "a b c"))
(want #f (ascii-string? "å b o"))
(want #t (ascii-string? (make-string 1 (integer->char #x7f))))
(want #f (ascii-string? (make-string 1 (integer->char #x80))))

(want #t (ascii-bytevector? (string->utf8 "")))
(want #t (ascii-bytevector? (string->utf8 "a")))
(want #t (ascii-bytevector? (string->utf8 "a b c")))
(want #f (ascii-bytevector? (string->utf8 "å b o")))
(want #t (ascii-bytevector?
          (string->utf8 (make-string 1 (integer->char #x7f)))))
(want #f (ascii-bytevector?
          (string->utf8 (make-string 1 (integer->char #x80)))))

(want #t (ascii-non-control? #\space))
(want #f (ascii-non-control? #\tab))
(want #f (ascii-non-control? #\newline))
(want #f (ascii-non-control? (integer->char #x0d)))

(want #t (ascii-space-or-tab? #\space))
(want #t (ascii-space-or-tab? #\tab))
(want #f (ascii-space-or-tab? #\newline))
(want #f (ascii-non-control? (integer->char #x0d)))

(let ((lowers "abcdefghijklmnopqrstuvwxyz")
      (uppers "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  (let loop ((i 0))
    (when (< i 26)
      (let ((lower (string-ref lowers i))
            (upper (string-ref uppers i)))
        (want upper (ascii-upcase upper))
        (want upper (ascii-upcase lower))
        (want lower (ascii-downcase upper))
        (want lower (ascii-downcase lower))
        (loop (+ i 1))))))

(let loop ((cc 0))
  (when (< cc #x80)
    (unless (ascii-alphabetic? cc)
      (want cc (ascii-upcase cc))
      (want cc (ascii-downcase cc)))
    (loop (+ cc 1))))

(let loop ((cc 0))
  (when (< cc #x80)
    (want #f (ascii-char? cc))
    (want #t (ascii-char? (integer->char cc)))
    (cond ((ascii-alphabetic? cc)
           (want #t (ascii-upper-case? (ascii-upcase cc)))
           (want #t (ascii-lower-case? (ascii-downcase cc)))
           (want #f (ascii-lower-case? (ascii-upcase cc)))
           (want #f (ascii-upper-case? (ascii-downcase cc)))
           (want #t (ascii-alphanumeric? cc))
           (want #t (ascii-non-control? cc))
           (want #f (ascii-other-graphic? cc))
           (want #f (ascii-control? cc))
           (want #f (ascii-numeric? cc))
           (want #f (ascii-whitespace? cc))
           (want #f (ascii-space-or-tab? cc)))
          ((ascii-control? cc)
           (want #f (ascii-non-control? cc))
           (want #f (ascii-other-graphic? cc))
           (want cc
                 (ascii-graphic->control
                  (ascii-control->graphic cc)))
           (want (integer->char cc)
                 (ascii-graphic->control
                  (ascii-control->graphic (integer->char cc)))))
          ((member cc '(#\( #\) #\[ #\] #\{ #\} #\< #\>))
           (want cc (ascii-mirror-bracket (ascii-mirror-bracket cc)))))
    (loop (+ cc 1))))

(let outer ((a 0))
  (when (< a 26)
    (let inner ((b 0))
      (if (= b 26)
          (outer (+ a 1))
          (begin (want (= a b)  (ascii-ci=?
                                 (ascii-nth-lower-case a)
                                 (ascii-nth-upper-case b)))
                 (want (< a b)  (ascii-ci<?
                                 (ascii-nth-lower-case a)
                                 (ascii-nth-upper-case b)))
                 (want (<= a b) (ascii-ci<=?
                                 (ascii-nth-lower-case a)
                                 (ascii-nth-upper-case b)))
                 (want (> a b)  (ascii-ci>?
                                 (ascii-nth-lower-case a)
                                 (ascii-nth-upper-case b)))
                 (want (>= a b) (ascii-ci>=?
                                 (ascii-nth-lower-case a)
                                 (ascii-nth-upper-case b)))
                 (inner (+ b 1)))))))

(ascii-ci>? #\A #\_)
(ascii-ci>? #\Z #\_)

(want #f (ascii-char? -1))
(want #f (ascii-char? #x80))
(want #f (ascii-char? (integer->char #x80)))

(want #f (ascii-control? -1))
(want #t (ascii-control? #x00))
(want #t (ascii-control? #x1f))
(want #f (ascii-control? #x20))
(want #f (ascii-control? #x7e))
(want #t (ascii-control? #x7f))
(want #f (ascii-control? #x80))

(want 0 (ascii-digit-value #\0 10))
(want 0 (ascii-digit-value #\0 1))
(want #f (ascii-digit-value #\0 0))
(want #f (ascii-digit-value #\0 -1))
(want 7 (ascii-digit-value #\7 8))
(want #f (ascii-digit-value #\7 7))
(want #f (ascii-digit-value #\: 10))

(want 0 (ascii-upper-case-value #\A 0 26))
(want 25 (ascii-upper-case-value #\Z 0 26))
(want #f (ascii-upper-case-value #\Z 0 25))

(want 0 (ascii-lower-case-value #\a 0 26))
(want 25 (ascii-lower-case-value #\z 0 26))
(want #f (ascii-lower-case-value #\z 0 25))

(want 0 (ascii-lower-case-value #\a 0 1))
(want #f (ascii-lower-case-value #\a 0 0))
(want #f (ascii-lower-case-value #\a 0 -1))
(want 9001 (ascii-lower-case-value #\b 9000 2))

(want #f (ascii-nth-digit -1))
(want #\0 (ascii-nth-digit 0))
(want #\9 (ascii-nth-digit 9))
(want #f (ascii-nth-digit 10))

(want #\Z (ascii-nth-upper-case -1))
(want #\A (ascii-nth-upper-case 0))
(want #\Z (ascii-nth-upper-case 25))
(want #\A (ascii-nth-upper-case 26))

(want #\z (ascii-nth-lower-case -1))
(want #\a (ascii-nth-lower-case 0))
(want #\z (ascii-nth-lower-case 25))
(want #\a (ascii-nth-lower-case 26))

(define (count-matching predicates value)
  (let loop ((ps predicates) (n 0))
    (if (null? ps) n (loop (cdr ps) (if ((car ps) value) (+ n 1) n)))))

(define (union? whole . parts)
  (let check ((cc 0))
    (or (= cc #x80)
        (if (and (whole cc) (not (= 1 (count-matching parts cc))))
            #f (check (+ cc 1))))))

(define (subset? small-set . bigger-sets)
  (let check ((cc 0))
    (or (= cc #x80)
        (if (and (small-set cc) (= 0 (count-matching bigger-sets cc)))
            #f (check (+ cc 1))))))

(define (disjoint? . predicates)
  (let check ((cc 0))
    (or (= cc #x80) (and (<= (count-matching predicates cc) 1)
                         (check (+ cc 1))))))

(define (decimal-numeric? x) (ascii-numeric? x))

(want #t (union? ascii-alphanumeric? ascii-alphabetic? decimal-numeric?))
(want #t (union? ascii-alphabetic? ascii-upper-case? ascii-lower-case?))

(want #t (subset? ascii-space-or-tab? ascii-whitespace?))
(want #t (subset? ascii-other-graphic? ascii-non-control?))
(want #t (subset? ascii-upper-case?   ascii-alphabetic? ascii-non-control?))
(want #t (subset? ascii-lower-case?   ascii-alphabetic? ascii-non-control?))
(want #t (subset? ascii-alphabetic?   ascii-alphanumeric? ascii-non-control?))
(want #t (subset? decimal-numeric?    ascii-alphanumeric? ascii-non-control?))
(want #t (subset? ascii-alphanumeric? ascii-non-control?))

(want #t (disjoint? ascii-control? ascii-non-control?))
(want #t (disjoint? ascii-whitespace?
                    ascii-other-graphic?
                    ascii-upper-case?
                    ascii-lower-case?
                    decimal-numeric?))
(want #t (disjoint? ascii-control?
                    ascii-other-graphic?
                    ascii-upper-case?
                    ascii-lower-case?
                    decimal-numeric?))

(define (check-string-ci a b cmp)
  (want (= cmp 0) (ascii-string-ci=? a b))
  (want (< cmp 0) (ascii-string-ci<? a b))
  (want (> cmp 0) (ascii-string-ci>? a b))
  (want (<= cmp 0) (ascii-string-ci<=? a b))
  (want (>= cmp 0) (ascii-string-ci>=? a b)))

(check-string-ci "" "" 0)
(check-string-ci "a" "a" 0)
(check-string-ci "A" "a" 0)
(check-string-ci "a" "A" 0)

(check-string-ci "a" "b" -1)
(check-string-ci "b" "a" 1)

(check-string-ci "a" "B" -1)
(check-string-ci "B" "a" 1)

(check-string-ci "aa" "aa" 0)
(check-string-ci "aa" "ab" -1)
(check-string-ci "ab" "aa" 1)
(check-string-ci "aa" "aaa" -1)
(check-string-ci "aaa" "aa" 1)
