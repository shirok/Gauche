;;;
;;; srfi-14/set.scm - character set algebra
;;;
;;;   Copyright (c) 2000-2019  Shiro Kawai  <shiro@acm.org>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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

