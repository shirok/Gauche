;;;
;;; srfi-13/kmp - string library (Knuth-Morris-Pratt search)
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
;;;  $Id: kmp.scm,v 1.3 2001-06-29 20:32:47 shirok Exp $
;;;

;; Say `(use srfi-13)' and this file will be autoloaded on demand.

(select-module srfi-13)

;; Knuth-Morris-Pratt search constructs.
;;
;; The SRFI-13 specification assumes accessing the pattern by index is
;; a lightweight operation, but it may not be true in Gauche if the pattern
;; contains multibyte characters.  So the programs using these functions
;; may not be very efficient, in spite of the efforts for efficiency put
;; in the original SRFI design.

(define (make-kmp-restart-vector s . args)
  (let-optionals* args ((c= char=?) start end)
    (let* ((pat (%maybe-substring s start end))
           (rv (make-vector (string-length pat) -1))
           (plen (string-length pat))
           (plen-1 (- plen 1)))
      (do ((i 0 (+ i 1)))
          ((= i plen-1) rv)
        (let loop ((k (+ (vector-ref rv i) 1)))
          (if (and (> k 0)
                   (not (c= (string-ref pat i) (string-ref pat (- k 1)))))
              (loop (+ (vector-ref rv (- k 1)) 1))
              (vector-set! rv (+ i 1) k)))
        )
      )))

(define (kmp-step pat rv c i c= p-start)
  (let loop ((i i))
    (if (c= c (string-ref pat (+ i p-start)))
        (+ i 1)
        (let ((i (vector-ref rv i)))
          (if (= i -1) 0 (loop i))))))

;; This is inefficient if input string s contains multibyte characters.
(define (string-kmp-partial-search pat rv s i . args)
  (let-optionals* args ((c= char=?) (p-start 0) start end)
    (let ((patlen (vector-length rv)))
      (let lp ((si s-start)
               (vi i))
        (cond ((= vi patlen) (- si)) 
              ((= si end) vi)
              (else (lp (+ si 1)
                        (kmp-step pat rv (string-ref s si)
                                  vi c= p-start))))))))
