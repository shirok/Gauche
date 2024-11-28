;; SRFI-7 implementation, taken from Richard Kelsey's reference implementation.
;; Gauche module stuff added by Alex Shinn.

;; Copyright (C) Richard Kelsey (1999). All Rights Reserved.
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; [SK] the SRFI-7 reference implementation using cond-expand expands
;; a form that ends with (begin), although the empty begin isn't defined
;; in R5RS.
;; Gauche usually compiles (begin) into a code that does nothing.
;; However, if there's a form like (begin <constant> (begin)), Gauche's
;; compiler optimizer removes <constant> as well.
;; A user might be surprised when he finds (program (code 4))
;; returns #<undef> ---but note that 'program' form isn't an expression
;; anyway, so expecting its result is beyond the scope of SRFI-7.

(define-module srfi.7
  (export program))
(select-module srfi.7)

(define-syntax program
  (syntax-rules (requires files code feature-cond)
    ((program)
     (begin))
    ((program (requires feature-id ...)
              more ...)
     (begin (cond-expand :allow-srfi-feature-id ((and feature-id ...) 'okay))
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
     (begin (cond-expand :allow-srfi-feature-id
                         (requirement (program stuff ...)) ...)
            (program more ...)))))
