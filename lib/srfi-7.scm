;; srfi-7 implementation, taken from Richard Kelsey's reference implementation.
;; Gauche module stuff added by Alex Shinn.

;; Copyright (C) Richard Kelsey (1999). All Rights Reserved.

;; This document and translations of it may be copied and furnished to
;; others, and derivative works that comment on or otherwise explain it or
;; assist in its implementation may be prepared, copied, published and
;; distributed, in whole or in part, without restriction of any kind,
;; provided that the above copyright notice and this paragraph are included
;; on all such copies and derivative works. However, this document itself
;; may not be modified in any way, such as by removing the copyright notice
;; or references to the Scheme Request For Implementation process or
;; editors, except as needed for the purpose of developing SRFIs in which
;; case the procedures for copyrights defined in the SRFI process must be
;; followed, or as required to translate it into languages other than
;; English.

;; [SK] the srfi-7 reference implementation using cond-expand expands
;; a form that ends with (begin), although the empty begin isn't defined
;; in R5RS.
;; Gauche usually compiles (begin) into a code that does nothing.
;; However, if there's a form like (begin <constant> (begin)), Gauche's
;; compiler optimizer removes <constant> as well.
;; A user might be surprised when he finds (program (code 4))
;; returns #<undef> ---but note that 'program' form isn't an expression
;; anyway, so expecting its result is beyond the scope of srfi-7.

(define-module srfi-7
  (export program))
(select-module srfi-7)

(define-syntax program
  (syntax-rules (requires files code feature-cond)
    ((program)
     (begin))
    ((program (requires feature-id ...)
              more ...)
     (begin (cond-expand ((and feature-id ...) 'okay))
            (program more ...)))
    ((program (files filename ...)
              more ...)
     (begin (load filename) ...
            (program more ...)))
    ((program (code stuff ...)
              more ...)
     (begin stuff ...
            (program more ...)))
    ((program (feature-cond (requirement stuff ...) ...)
              more ...)
     (begin (cond-expand (requirement (program stuff ...)) ...)
            (program more ...)))))


