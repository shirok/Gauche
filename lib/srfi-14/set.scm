;;;
;;; srfi-14/set.scm - character set algebra
;;;
;;;  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
;;;
;;;  Permission to use, copy, modify, distribute this software and
;;;  accompanying documentation for any purpose is hereby granted,
;;;  provided that existing copyright notices are retained in all
;;;  copies and that this notice is included verbatim in all
;;;  distributions.
;;;  This software is provided as is, without express or implied
;;;  warranty.  In no circumstances the author(s) shall be liable
;;;  for any damages arising out of the use of this software.
;;;
;;;  $Id: set.scm,v 1.1 2001-04-25 07:41:50 shiro Exp $
;;;

;; Say `(use srfi-14)' and this file is autoloaded on demand.

(select-module srfi-14)

;; auxiliary functions for range operation other than adjoin and union.
;; (adjoin and union can use native functions).  These can also be defined
;; in C, but I don't see the needs of performance yet.
(define (%char-set-sub-range ranges from to)
  (let loop ((ranges ranges)
             (result '()))
    (if (null? ranges)
        (reverse result)
        (let ((lo (caar ranges))
              (hi (cdar ranges))
              (next (cdr ranges)))
          (if (<= lo from hi)
              (if (<= lo to hi)
                  (if (= lo from)
                      (if (= to hi)
                          (loop next result)
                          (loop next (acons (+ to 1) hi result)))
                      (if (= to hi)
                          (loop next (acons lo (- from 1) result))
                          (loop next (list* (cons (+ to 1) hi)
                                            (cons lo (- from 1))
                                            result))))
                  (if (= lo from)
                      (loop next result)
                      (loop next (acons lo (- from 1) result))))
              (if (<= lo to hi)
                  (if (= to hi)
                      (loop next result)
                      (loop next (acons (+ to 1) hi result)))
                  (if (and (< from lo) (< hi to))
                      (loop next result)
                      (loop next (acons lo hi result)))))
          ))
    ))

(define (%char-set-sub-chars range chars)
  (let loop ((range range)
             (chars chars))
    (if (null? chars)
        range
        (let ((code (char->integer (car chars))))
          (loop (%char-set-sub-range range code code) (cdr chars))))))

(define (%ranges->char-set ranges)
  (let ((base (char-set)))
    (for-each (lambda (range)
                (%char-set-add-range! base (car range) (cdr range)))
              ranges)
    base))

(define (char-set-adjoin base . chars)
  (if (null? chars) base (%char-set-add-chars! (char-set-copy base) chars)))

(define (char-set-adjoin! base . chars)
  (%char-set-add-chars! base chars))

(define (char-set-delete base . chars)
  (%ranges->char-set (%char-set-sub-chars (%char-set-ranges base) chars)))

(define char-set-delete! char-set-delete)

(define (char-set-complement cs)
  (%char-set-complement! (char-set-copy cs)))

(define (char-set-complement! cs) (%char-set-complement! cs))

(define (char-set-union . charsets)
  (if (null? charsets)
      char-set:empty
      (let loop ((base (char-set-copy (car charsets)))
                 (rest (cdr charsets)))
        (if (null? rest)
            base
            (loop (%char-set-add! base (car rest)) (cdr rest))))))

(define (char-set-union! base . charsets)
  (if (null? charsets)
      base
      (let loop ((base base) (rest charsets))
        (if (null? rest)
            base
            (loop (%char-set-add! base (car rest)) (cdr rest))))))

(define (%char-set-intersection2 x y)
  (char-set-difference x (char-set-complement y)))

(define (char-set-intersection . charsets)
  (cond ((null? charsets) char-set:full)
        ((null? (cdr charsets)) (car charsets))
        (else (apply char-set-intersection
                     (%char-set-intersection2 (car charsets) (cadr charsets))
                     (cddr charsets)))))

(define (char-set-intersection! base . charsets)
  (if (null? charsets)
      base
      (apply char-set-intersection base charsets)))

(define (char-set-difference base . charsets)
  (define (char-range-sub2 ranges sub)
    (let loop ((ranges ranges) (sub sub))
      (if (null? sub)
          ranges
          (loop (%char-set-sub-range ranges (caar sub) (cdar sub))
                (cdr sub)))))

  (let loop ((ranges (%char-set-ranges base))
             (sets charsets))
    (if (null? sets)
        (%ranges->char-set ranges)
        (loop (char-range-sub2 ranges (%char-set-ranges (car sets)))
              (cdr sets))))
  )
(define char-set-difference! char-set-difference)

(define (%char-set-xor2 x y)
  (char-set-difference (char-set-union x y)
                       (%char-set-intersection2 x y)))

(define (char-set-xor . charsets)
  (cond ((null? charsets) char-set:empty)
        ((null? (cdr charsets)) (car charsets))
        (else (apply char-set-xor
                     (%char-set-xor2 (car charsets) (cadr charsets))
                     (cddr charsets)))))

(define (char-set-xor! base . charsets)
  (if (null? charsets)
      base
      (apply char-set-xor base charsets)))

(define (char-set-diff+intersection base . charsets)
  (values (apply char-set-difference base charsets)
          (char-set-intersection base (apply char-set-union charsets))))

(define (char-set-diff+intersection! cs1 cs2 . charsets)
  (values (apply char-set-difference! cs1 cs2 charsets)
          (char-set-intersection! cs1 (apply char-set-union! cs2 charsets))))

