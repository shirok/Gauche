;;;; lcs.scm -- find out the longest common sequence

;;; Created:    <2002-06-21 15:36:46 foof>
;;; Time-stamp: <2003-02-15 00:09:55 foof>
;;; Author:     Alex Shinn <foof@synthcode.com>

;;; Modified a bit by Shiro Kawai

(define-module util.lcs
  (use gauche.sequence)
  (use srfi-1)
  (use srfi-11)
  (export lcs lcs-with-positions))
(select-module util.lcs)

;; utility for lcs-with-positions
(define (max-seq . opt-ls)
  (if (null? opt-ls)
    (list 0 '())
    (let loop ((a (car opt-ls)) (ls (cdr opt-ls)))
      (if (null? ls)
        a
        (let ((b (car ls)))
          (if (>= (car a) (car b))
            (loop a (cdr ls))
            (loop b (cdr ls))))))))

(define (lcs-with-positions a-ls b-ls . opt-eq)
  (let* ((eq (get-optional opt-eq equal?))
         (a-size (+ 1 (size-of a-ls)))
         (b-size (+ 1 (size-of b-ls)))
         (undef  (cons #f #f))
         (cache  (make-vector (* a-size b-size) undef)))
    (let-syntax ((cache-ref
                  (syntax-rules ()
                    ((_ a b) (vector-ref cache (+ (* a b-size) b)))))
                 (cache-set!
                  (syntax-rules ()
                    ((_ a b v) (vector-set! cache (+ (* a b-size) b) v)))))
      (let loop ((a a-ls) (a-pos 0) (b b-ls) (b-pos 0))
        ;; cache this step if not already done
        (when (eq? (cache-ref a-pos b-pos) undef)
          (cache-set!
           a-pos b-pos
           (if (or (null? a) (null? b))
               (list 0 '()) ;; base case
               (let ((a1 (car a))
                     (b1 (car b))
                     (a-tail (loop (cdr a) (+ a-pos 1) b b-pos))
                     (b-tail (loop a a-pos (cdr b) (+ b-pos 1))))
                 (cond ((eq a1 b1)
                        ;; match found, we either use it or we don't
                        (let* ((a-b-tail (loop (cdr a) (+ a-pos 1)
                                               (cdr b) (+ b-pos 1)))
                               (a-b-res (list (+ 1 (car a-b-tail))
                                              (cons (list a1 a-pos b-pos)
                                                    (cadr a-b-tail)))))
                          (max-seq a-b-res a-tail b-tail)))
                       (else
                        ;; not a match
                        (max-seq a-tail b-tail)))))))
        ;; return the computed cache
        (cache-ref a-pos b-pos) ))))

(define (lcs a b . opt-eq)
  (map car (cadr (lcs-with-positions a b (get-optional opt-eq equal?)))))

(provide "util/lcs")
