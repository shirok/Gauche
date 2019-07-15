;;;
;;; util.unification - Basic unification algorithm
;;;
;;;   Copyright (c) 2016-2019  Shiro Kawai  <shiro@acm.org>
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

(define-module util.unification
  (use gauche.dictionary)
  (use util.match)
  (export unify unify-merge))
(select-module util.unification)

;; Unify two trees.
;;
;;   Tree : Variable | Value | Tuple
;;   Tuple : {Tree ...}
;;
;; The algorithm is independent of the actual representation of the trees
;; to unify.  The caller must provide the following parameters that abstracts
;; the representation of the target tree T.
;;
;;   var-cmpr      A comparator to test, compare and hash "variables".
;;                 Must be hashable.
;;   val-cmpr      A comparator to test and compare "values".  Values
;;                 are non-variable terminals.  A tree node that are
;;                 neither a variable nor a value is a tuple.
;;   (tuple-fold proc seed x [y])
;;        :: (Tree a -> a) a Tree -> a
;;        or (Tree Tree a -> a) a Tree Tree -> a
;;                 Fold elements in the tuple, applying PROC, just like
;;                 fold.  It must be able to fold over one or two tuples.

;; API
;; Unify two trees A and B, and returns a substition dictionary
;; If A and B are not unifyable, returns #t.
(define (unify a b var-cmpr val-cmpr tuple-fold)

  (define variable? (comparator-type-test-predicate var-cmpr))
  (define value?    (comparator-type-test-predicate val-cmpr))
  (define (tuple? x) (and (not (variable? x)) (not (value? x))))
  
  (define (unify-rec a b dict)       ;-> Maybe dict
    (and dict
         (cond [(variable? a) (unify-var a b dict)]
               [(variable? b) (unify-var b a dict)]
               [(and (value? a) (value? b)) (and (=? val-cmpr a b) dict)]
               [(and (tuple? a) (tuple? b))
                (tuple-fold (^[a b dict] (and dict (unify-rec a b dict)))
                            dict a b)]
               [else #f])))

  (define (unify-var var x dict)     ;-> Maybe dict
    (cond [(and (variable? x) (=? var-cmpr var x)) dict]
          [(dict-exists? dict var) (unify-rec (dict-get dict var) x dict)]
          [(and (variable? x) (dict-exists? dict x))
           (unify-rec var (dict-get dict x) dict)]
          [(occurs? var x dict) #f]
          [else (dict-put! dict var x) dict]))

  (define (occurs? var x dict)       ;-> bool
    (cond [(variable? x)
           (cond [(=? var-cmpr var x) #t]
                 [(dict-exists? dict x) (occurs? var (dict-get dict x) dict)]
                 [else #f])]
          [(tuple? x)
           (tuple-fold (^[a flag] (or flag (occurs? var a dict))) #f x)]
          [else #f]))

  (unify-rec a b (make-hash-table var-cmpr)))

;; API
;; Run unification on two trees A and B, and if they are unifiable,
;; returns a merged tree where variables in both trees are eliminated.
;; In addition to the parameters for unify, it must provide a tuple
;; constructor:
;;
;;   (make-tuple proto elements) :: Tree [Tree] -> Tree
;;       Returns a tuple which is the same as PROTO (e.g. number of
;;       elements, auxiliary attributes, etc) except its elements
;;       are substituted with ELEMENTS.
;;
(define (unify-merge a b var-cmpr val-cmpr tuple-fold make-tuple)

  (define variable? (comparator-type-test-predicate var-cmpr))
  (define value?    (comparator-type-test-predicate val-cmpr))
  (define (tuple? x) (and (not (variable? x)) (not (value? x))))

  (define dict (unify a b var-cmpr val-cmpr tuple-fold))

  (define (subst x)
    (cond [(variable? x)
           (if (dict-exists? dict x)
             (subst (dict-get dict x))
             x)]
          [(tuple? x)
           (let* ([elts0 (reverse (tuple-fold cons '() x))]
                  [elts1 (map subst elts0)])
             (if (every eq? elts0 elts1)
               x
               (make-tuple x elts1)))]
          [else x]))

  (and dict (subst a)))
