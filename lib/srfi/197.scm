;;;
;;; Srfi-197 Pipeline operators
;;;
;;; The code is taken from the reference implementation,
;;; and adapted to Gauche module.
;;;

;;;
;;; Copyright (c) 2020 Adam Nelson.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without restriction,
;;; including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;; and to permit persons to whom the Software is furnished to do so,
;;; subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice (including the
;;; next paragraph) shall be included in all copies or substantial
;;; portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;;

(define-module srfi.197
  (export chain chain-and chain-when chain-lambda nest nest-reverse))
(select-module srfi.197)

(define-syntax chain
  (syntax-rules ...1 ()
    ((_ initial-value) initial-value)
    ((_ initial-value (step ...1) ...1)
      (chain initial-value _ ... (step ...1) ...1))
    ((_ initial-value placeholder (step ...1) ...1)
      (chain initial-value placeholder ... (step ...1) ...1))
    ((_ initial-value placeholder ellipsis (first-step ...1) (next-step ...1) ...1)
      (let ()
        (define-syntax %chain
          (syntax-rules ...2 (placeholder ellipsis)
            ; (_ in-step out-step in-vars out-vars in-steps out-steps)
            ((_ () () () ((var) ...2) () (step ...2 last-step))
              (let* ((var step) ...2) last-step))
            ((_ () () () (vars ...2) () (step ...2 last-step))
              (let*-values ((vars step) ...2) last-step))
            ((_ () () () out-vars (step . in-steps) out-steps)
              (%chain step () () out-vars in-steps out-steps))
            ((_ () step () (out-vars ...2) in-steps (out-steps ...2))
              (%chain () () () (out-vars ...2 ignored) in-steps (out-steps ...2 step)))
            ((_ () step vars (out-vars ...2) in-steps (out-steps ...2))
              (%chain () () () (out-vars ...2 vars) in-steps (out-steps ...2 step)))
            ((_ (placeholder ellipsis) (step ...2) () (out-vars ...2) in-steps (out-steps ...2))
              (%chain () () () (out-vars ...2 chain-rest-var) in-steps (out-steps ...2 (apply step ...2 chain-rest-var))))
            ((_ (placeholder ellipsis) (step ...2) (vars ...2) (out-vars ...2) in-steps (out-steps ...2))
              (%chain () () () (out-vars ...2 (vars ...2 . chain-rest-var)) in-steps (out-steps ...2 (apply step ...2 chain-rest-var))))
            ((_ (placeholder ellipsis . rest) . _)
              (syntax-error "_ ... can only be used as a final argument"))
            ((_ (placeholder . in-step) (out-step ...2) (vars ...2) . rest)
              (%chain in-step (out-step ...2 chain-var) (vars ...2 chain-var) . rest))
            ((_ (x . in-step) (out-step ...2) . rest)
              (%chain in-step (out-step ...2 x) . rest))))
        (%chain (first-step ...1) () () () ((next-step ...1) ...1) (initial-value))))))

(define-syntax chain-and
  (syntax-rules ...1 ()
    ((_ initial-value) initial-value)
    ((_ initial-value (step ...1) ...1) (chain-and initial-value _ (step ...1) ...1))
    ((_ initial-value placeholder (first-step ...1) (next-step ...1) ...1)
      (let ()
        (define-syntax %chain-and
          (syntax-rules ...2 (placeholder)
            ; (_ in-step out-step in-vars out-vars in-steps out-steps)
            ((_ () () () (var ...2) () (step ...2 last-step))
              (and-let* ((var step) ...2) last-step))
            ((_ () () () out-vars (step . in-steps) out-steps)
              (%chain-and step () () out-vars in-steps out-steps))
            ((_ () step () (out-vars ...2) in-steps (out-steps ...2))
              (%chain-and () () () (out-vars ...2 ignored) in-steps (out-steps ...2 step)))
            ((_ () step (var) (out-vars ...2) in-steps (out-steps ...2))
              (%chain-and () () () (out-vars ...2 var) in-steps (out-steps ...2 step)))
            ((_ (placeholder . in-step) (out-step ...2) () . rest)
              (%chain-and in-step (out-step ...2 chain-var) (chain-var) . rest))
            ((_ (placeholder . excess) . rest)
              (syntax-error "chain-and does not support multiple _ in a single step"))
            ((_ (x . in-step) (out-step ...2) . rest)
              (%chain-and in-step (out-step ...2 x) . rest))))
        (%chain-and (first-step ...1) () () () ((next-step ...1) ...1) (initial-value))))))

(define-syntax chain-when
  (syntax-rules ...1 ()
    ((_ initial-value) initial-value)
    ((_ initial-value (guard? (step ...1)) ...1)
      (chain-when initial-value _ (guard? (step ...1)) ...1))
    ((_ initial-value placeholder (first-guard? (first-step ...1)) (next-guard? (next-step ...1)) ...1)
      (let ()
        (define-syntax %chain-when
          (syntax-rules ...2 (placeholder)
            ; (_ in-step out-step guard? chain-var in-steps out-expr)
            ((_ () () _1 _2 () out-expr) out-expr)
            ((_ () () _1 _2 ((guard? step) . in-steps) out-expr)
              (%chain-when step () guard? #f in-steps out-expr))
            ((_ () step guard? #f in-steps out-expr)
              (%chain-when () () #f #f in-steps
                (let ((chain-var out-expr))
                  (if guard? step chain-var))))
            ((_ () step guard? chain-var in-steps out-expr)
              (%chain-when () () #f #f in-steps
                (let ((chain-var out-expr))
                  (if guard? step chain-var))))
            ((_ (placeholder . in-step) (out-step ...2) guard? #f . rest)
              (%chain-when in-step (out-step ...2 chain-var) guard? chain-var . rest))
            ((_ (placeholder . excess) . rest)
              (syntax-error "chain-when does not support multiple _ in a single step"))
            ((_ (x . in-step) (out-step ...2) . rest)
              (%chain-when in-step (out-step ...2 x) . rest))))
        (%chain-when (first-step ...1) () first-guard? #f ((next-guard? (next-step ...1)) ...1) initial-value)))))

(define-syntax chain-lambda
  (syntax-rules ...1 ()
    ((_ (step ...1) ...1) (chain-lambda _ ... (step ...1) ...1))
    ((_ placeholder (step ...1) ...1) (chain-lambda placeholder ... (step ...1) ...1))
    ((_ placeholder ellipsis (first-step ...1) (next-step ...1) ...1)
      (let ()
        (define-syntax %chain-lambda
          (syntax-rules ...2 (placeholder ellipsis)
            ; (_ in-step out-step args rest-of-steps)
            ((_ () step args ())
              (lambda args step))
            ((_ () step args steps)
              (lambda args
                (chain step placeholder ellipsis . steps)))
            ((_ (placeholder ellipsis) (step ...2) () ())
              (lambda chain-rest-var (apply step ...2 chain-rest-var)))
            ((_ (placeholder ellipsis) (step ...2) () steps)
              (lambda chain-rest-var
                (chain (apply step ...2 chain-rest-var) placeholder ellipsis . steps)))
            ((_ (placeholder ellipsis) (step ...2) (args ...2) ())
              (lambda (args ...2 . chain-rest-var) (apply step ...2 chain-rest-var)))
            ((_ (placeholder ellipsis) (step ...2) (args ...2) steps)
              (lambda (args ...2 . chain-rest-var)
                (chain (apply step ...2 chain-rest-var) placeholder ellipsis . steps)))
            ((_ (placeholder ellipsis . excess) . rest)
              (syntax-error "_ ... can only be used as a final argument"))
            ((_ (placeholder . in-step) (out-step ...2) (args ...2) . rest)
              (%chain-lambda in-step (out-step ...2 chain-var) (args ...2 chain-var) . rest))
            ((_ (x . in-step) (out-step ...2) . rest)
              (%chain-lambda in-step (out-step ...2 x) . rest))))
        (%chain-lambda (first-step ...1) () () ((next-step ...1) ...1))))))

(define-syntax nest
  (syntax-rules ...1 (_)
    ((nest last) last)
    ((nest (step ...1) ...1 last) (nest _ (step ...1) ...1 last))
    ((nest placeholder (extra-step ...1) ...1 (first-step ...1) last)
      (let ()
        ; let-syntax is buggy in some Schemes, define-syntax is more reliable
        (define-syntax %nest
          (syntax-rules ...2 (placeholder)
            ((%nest result () placeholder ()) result)
            ((%nest result () placeholder (rest ...2 step))
              (%nest () step result (rest ...2)))
            ((%nest result () accum steps)
              (syntax-error "nest: step must contain _"))
            ((%nest result (placeholder . rest) placeholder steps)
              (syntax-error "nest: only one _ allowed per step"))
            ((%nest (result ...2) (placeholder . rest) accum steps)
              (%nest (result ...2 accum) rest placeholder steps))
            ((%nest (result ...2) (element . rest) accum steps)
              (%nest (result ...2 element) rest accum steps))))
        (%nest () (first-step ...1) last ((extra-step ...1) ...1))))
    ((nest placeholder last) last)))

(define-syntax nest-reverse
  (syntax-rules ...1 (_)
    ((nest-reverse first) first)
    ((nest-reverse first (step ...1) ...1) (nest-reverse first _ (step ...1) ...1))
    ((nest-reverse first placeholder (first-step ...1) (extra-step ...1) ...1)
      (let ()
        (define-syntax %nest
          (syntax-rules ...2 (placeholder)
            ((%nest result () placeholder ()) result)
            ((%nest result () placeholder (step . rest))
              (%nest () step result rest))
            ((%nest result () accum steps)
              (syntax-error "nest-reverse: step must contain _"))
            ((%nest result (placeholder . rest) placeholder steps)
              (syntax-error "nest-reverse: only one _ allowed per step"))
            ((%nest (result ...2) (placeholder . rest) accum steps)
              (%nest (result ...2 accum) rest placeholder steps))
            ((%nest (result ...2) (element . rest) accum steps)
              (%nest (result ...2 element) rest accum steps))))
        (%nest () (first-step ...1) first ((extra-step ...1) ...1))))
    ((nest-reverse first placeholder) first)))
