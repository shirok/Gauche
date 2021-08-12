;; -*- coding:utf-8 -*-

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

(define-module srfi-197
  (export chain chain-and chain-when chain-lambda nest nest-reverse))
(select-module srfi-197)

(define-syntax chain
  (syntax-rules …₁ ()
    ((_ initial-value) initial-value)
    ((_ initial-value (step …₁) …₁)
      (chain initial-value _ ... (step …₁) …₁))
    ((_ initial-value placeholder (step …₁) …₁)
      (chain initial-value placeholder ... (step …₁) …₁))
    ((_ initial-value placeholder ellipsis (first-step …₁) (next-step …₁) …₁)
      (let ()
        (define-syntax %chain
          (syntax-rules …₂ (placeholder ellipsis)
            ; (_ in-step out-step in-vars out-vars in-steps out-steps)
            ((_ () () () ((var) …₂) () (step …₂ last-step))
              (let* ((var step) …₂) last-step))
            ((_ () () () (vars …₂) () (step …₂ last-step))
              (let*-values ((vars step) …₂) last-step))
            ((_ () () () out-vars (step . in-steps) out-steps)
              (%chain step () () out-vars in-steps out-steps))
            ((_ () step () (out-vars …₂) in-steps (out-steps …₂))
              (%chain () () () (out-vars …₂ ignored) in-steps (out-steps …₂ step)))
            ((_ () step vars (out-vars …₂) in-steps (out-steps …₂))
              (%chain () () () (out-vars …₂ vars) in-steps (out-steps …₂ step)))
            ((_ (placeholder ellipsis) (step …₂) () (out-vars …₂) in-steps (out-steps …₂))
              (%chain () () () (out-vars …₂ chain-rest-var) in-steps (out-steps …₂ (apply step …₂ chain-rest-var))))
            ((_ (placeholder ellipsis) (step …₂) (vars …₂) (out-vars …₂) in-steps (out-steps …₂))
              (%chain () () () (out-vars …₂ (vars …₂ . chain-rest-var)) in-steps (out-steps …₂ (apply step …₂ chain-rest-var))))
            ((_ (placeholder ellipsis . rest) . _)
              (syntax-error "_ ... can only be used as a final argument"))
            ((_ (placeholder . in-step) (out-step …₂) (vars …₂) . rest)
              (%chain in-step (out-step …₂ chain-var) (vars …₂ chain-var) . rest))
            ((_ (x . in-step) (out-step …₂) . rest)
              (%chain in-step (out-step …₂ x) . rest))))
        (%chain (first-step …₁) () () () ((next-step …₁) …₁) (initial-value))))))

(define-syntax chain-and
  (syntax-rules …₁ ()
    ((_ initial-value) initial-value)
    ((_ initial-value (step …₁) …₁) (chain-and initial-value _ (step …₁) …₁))
    ((_ initial-value placeholder (first-step …₁) (next-step …₁) …₁)
      (let ()
        (define-syntax %chain-and
          (syntax-rules …₂ (placeholder)
            ; (_ in-step out-step in-vars out-vars in-steps out-steps)
            ((_ () () () (var …₂) () (step …₂ last-step))
              (and-let* ((var step) …₂) last-step))
            ((_ () () () out-vars (step . in-steps) out-steps)
              (%chain-and step () () out-vars in-steps out-steps))
            ((_ () step () (out-vars …₂) in-steps (out-steps …₂))
              (%chain-and () () () (out-vars …₂ ignored) in-steps (out-steps …₂ step)))
            ((_ () step (var) (out-vars …₂) in-steps (out-steps …₂))
              (%chain-and () () () (out-vars …₂ var) in-steps (out-steps …₂ step)))
            ((_ (placeholder . in-step) (out-step …₂) () . rest)
              (%chain-and in-step (out-step …₂ chain-var) (chain-var) . rest))
            ((_ (placeholder . excess) . rest)
              (syntax-error "chain-and does not support multiple _ in a single step"))
            ((_ (x . in-step) (out-step …₂) . rest)
              (%chain-and in-step (out-step …₂ x) . rest))))
        (%chain-and (first-step …₁) () () () ((next-step …₁) …₁) (initial-value))))))

(define-syntax chain-when
  (syntax-rules …₁ ()
    ((_ initial-value) initial-value)
    ((_ initial-value (guard? (step …₁)) …₁)
      (chain-when initial-value _ (guard? (step …₁)) …₁))
    ((_ initial-value placeholder (first-guard? (first-step …₁)) (next-guard? (next-step …₁)) …₁)
      (let ()
        (define-syntax %chain-when
          (syntax-rules …₂ (placeholder)
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
            ((_ (placeholder . in-step) (out-step …₂) guard? #f . rest)
              (%chain-when in-step (out-step …₂ chain-var) guard? chain-var . rest))
            ((_ (placeholder . excess) . rest)
              (syntax-error "chain-when does not support multiple _ in a single step"))
            ((_ (x . in-step) (out-step …₂) . rest)
              (%chain-when in-step (out-step …₂ x) . rest))))
        (%chain-when (first-step …₁) () first-guard? #f ((next-guard? (next-step …₁)) …₁) initial-value)))))

(define-syntax chain-lambda
  (syntax-rules …₁ ()
    ((_ (step …₁) …₁) (chain-lambda _ ... (step …₁) …₁))
    ((_ placeholder (step …₁) …₁) (chain-lambda placeholder ... (step …₁) …₁))
    ((_ placeholder ellipsis (first-step …₁) (next-step …₁) …₁)
      (let ()
        (define-syntax %chain-lambda
          (syntax-rules …₂ (placeholder ellipsis)
            ; (_ in-step out-step args rest-of-steps)
            ((_ () step args ())
              (lambda args step))
            ((_ () step args steps)
              (lambda args
                (chain step placeholder ellipsis . steps)))
            ((_ (placeholder ellipsis) (step …₂) () ())
              (lambda chain-rest-var (apply step …₂ chain-rest-var)))
            ((_ (placeholder ellipsis) (step …₂) () steps)
              (lambda chain-rest-var
                (chain (apply step …₂ chain-rest-var) placeholder ellipsis . steps)))
            ((_ (placeholder ellipsis) (step …₂) (args …₂) ())
              (lambda (args …₂ . chain-rest-var) (apply step …₂ chain-rest-var)))
            ((_ (placeholder ellipsis) (step …₂) (args …₂) steps)
              (lambda (args …₂ . chain-rest-var)
                (chain (apply step …₂ chain-rest-var) placeholder ellipsis . steps)))
            ((_ (placeholder ellipsis . excess) . rest)
              (syntax-error "_ ... can only be used as a final argument"))
            ((_ (placeholder . in-step) (out-step …₂) (args …₂) . rest)
              (%chain-lambda in-step (out-step …₂ chain-var) (args …₂ chain-var) . rest))
            ((_ (x . in-step) (out-step …₂) . rest)
              (%chain-lambda in-step (out-step …₂ x) . rest))))
        (%chain-lambda (first-step …₁) () () ((next-step …₁) …₁))))))

(define-syntax nest
  (syntax-rules …₁ (_)
    ((nest last) last)
    ((nest (step …₁) …₁ last) (nest _ (step …₁) …₁ last))
    ((nest placeholder (extra-step …₁) …₁ (first-step …₁) last)
      (let ()
        ; let-syntax is buggy in some Schemes, define-syntax is more reliable
        (define-syntax %nest
          (syntax-rules …₂ (placeholder)
            ((%nest result () placeholder ()) result)
            ((%nest result () placeholder (rest …₂ step))
              (%nest () step result (rest …₂)))
            ((%nest result () accum steps)
              (syntax-error "nest: step must contain _"))
            ((%nest result (placeholder . rest) placeholder steps)
              (syntax-error "nest: only one _ allowed per step"))
            ((%nest (result …₂) (placeholder . rest) accum steps)
              (%nest (result …₂ accum) rest placeholder steps))
            ((%nest (result …₂) (element . rest) accum steps)
              (%nest (result …₂ element) rest accum steps))))
        (%nest () (first-step …₁) last ((extra-step …₁) …₁))))
    ((nest placeholder last) last)))

(define-syntax nest-reverse
  (syntax-rules …₁ (_)
    ((nest-reverse first) first)
    ((nest-reverse first (step …₁) …₁) (nest-reverse first _ (step …₁) …₁))
    ((nest-reverse first placeholder (first-step …₁) (extra-step …₁) …₁)
      (let ()
        (define-syntax %nest
          (syntax-rules …₂ (placeholder)
            ((%nest result () placeholder ()) result)
            ((%nest result () placeholder (step . rest))
              (%nest () step result rest))
            ((%nest result () accum steps)
              (syntax-error "nest-reverse: step must contain _"))
            ((%nest result (placeholder . rest) placeholder steps)
              (syntax-error "nest-reverse: only one _ allowed per step"))
            ((%nest (result …₂) (placeholder . rest) accum steps)
              (%nest (result …₂ accum) rest placeholder steps))
            ((%nest (result …₂) (element . rest) accum steps)
              (%nest (result …₂ element) rest accum steps))))
        (%nest () (first-step …₁) first ((extra-step …₁) …₁))))
    ((nest-reverse first placeholder) first)))
