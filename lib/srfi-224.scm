;;;
;;; srfi-224 - Integer Mappings
;;;
;;;   Copyright (c) 2021  Shiro Kawai  <shiro@acm.org>
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

;; We use sparse vector as imapping.

(define-module srfi-224
  (use data.sparse)
  (use util.match)
  (export
   ;; Constructors
   imapping imapping-unfold imapping-unfold-maybe alist->imapping
   alist->imapping/combinator

   ;; Predicates
   imapping? imapping-contains? imapping-empty? imapping-disjoint?

   ;; Accessors
   imapping-min imapping-max imapping-lookup imapping-ref
   imapping-lookup-min imapping-lookup-max
   imapping-ref/default

   ;; Updaters
   imapping-adjoin imapping-adjoin/combinator imapping-adjust
   imapping-adjust/key imapping-adjoin! imapping-adjoin/combinator!
   imapping-set imapping-set!
   imapping-adjust! imapping-adjust/key! imapping-delete
   imapping-delete-all imapping-alter imapping-alter/key imapping-update
   imapping-update/key imapping-delete! imapping-delete-all!
   imapping-alter! imapping-alter/key! imapping-update! imapping-update/key!
   imapping-delete-min imapping-delete-max imapping-update-min
   imapping-update-max imapping-update-min/key imapping-update-max/key
   imapping-pop-min imapping-pop-max imapping-delete-min!
   imapping-delete-max! imapping-update-min! imapping-update-max!
   imapping-update-min/key! imapping-update-max/key!
   imapping-pop-min! imapping-pop-max!

   ;; The whole imapping
   imapping-size imapping-count imapping-count/key imapping-any?
   imapping-every?

   ;; Traversal
   imapping-fold imapping-fold-right imapping-fold/key
   imapping-fold-right/key imapping-map imapping-map/key imapping-map->list
   imapping-map/key->list imapping-for-each imapping-for-each/key
   imapping-filter-map imapping-filter-map/key imapping-filter-map!
   imapping-filter-map/key! imapping-map-either imapping-map-either/key
   imapping-map-either! imapping-map-either/key!
   imapping-relation-map

   ;; Filter
   imapping-filter imapping-filter/key imapping-remove imapping-remove/key
   imapping-partition imapping-partition/key
   imapping-filter! imapping-filter/key! imapping-remove!
   imapping-remove/key! imapping-partition! imapping-partition/key!

   ;; Copying and conversion
   imapping-keys imapping-values imapping-copy
   imapping->alist imapping->decreasing-alist imapping->generator
   imapping->decreasing-generator

   ;; Comparison
   imapping=? imapping<? imapping>? imapping<=? imapping>=?

   ;; Set theory operations
   imapping-union imapping-intersection imapping-difference imapping-xor
   imapping-union! imapping-intersection! imapping-difference! imapping-xor!
   imapping-union/combinator imapping-intersection/combinator
   imapping-union/combinator! imapping-intersection/combinator!

   ;; Submappings
   imapping-open-interval imapping-closed-interval
   imapping-open-closed-interval imapping-closed-open-interval
   imapping-open-interval! imapping-closed-interval!
   imapping-open-closed-interval! imapping-closed-open-interval!
   isubmapping= isubmapping< isubmapping<= isubmapping>= isubmapping>
   isubmapping=! isubmapping<! isubmapping<=! isubmapping>=!
   isubmapping>!
   imapping-split
   ))
(select-module srfi-224)

(define (imapping . args)
  (apply imapping-adjoin! (make-sparse-vector) args))

(define (%imapping-adjoin-1! m k v)
  (unless (sparse-vector-exists? m k)
    (sparse-vector-set! m (assume k exact-integer?) v)))

(define *unique* (cons #f #f))

(define (%imapping-adjoin-1/combinator! m k v combine)
  (sparse-vector-update! m (assume k exact-integer?)
                         (^[prev]
                           (if (eq? prev *unique*)
                             v
                             (combine v prev)))
                         *unique*))

(define (imapping-adjoin! m . args)
  (let loop ([as args])
    (match as
      [() m]
      [(k) (error "Length of key-value list isn't even:" args)]
      [(k v . rest) (%imapping-adjoin-1! m k v) (loop rest)])))

(define (imapping-unfold stop? val succ seed)
  (rlet1 m (make-sparse-vector)
    (let loop ([seed seed])
      (unless (stop? seed)
        (receive (k v) (val seed)
          (%imapping-adjoin-1! m k v)
          (loop (succ seed)))))))

(define (imapping-unfold-maybe mproc seed)
  (rlet1 m (make-sparse-vector)
    (let loop ([seed seed])
      (maybe-let*-values ([(k v next) (mproc seed)])
         (%imapping-adjoin-1! m k v)
         (loop next)))))

(define (alist->imapping alist)
  (rlet1 m (make-sparse-vector)
    (dolist [p alist]
      (match p
        [(k . v) (%imapping-adjoin-1! m k v)]
        [_ (error "alist contain non-pair:" p)]))))

(define (alist->imapping/combinator proc alist)
  (rlet1 m (make-sparse-vector)
    (dolist [p alist]
      (match p
        [(k . v) (%imapping-adjoin-1/combinator! m k v proc)]
        [_ (error "alist contain non-pair:" p)]))))

(define (imapping-contains? m k)
  (assume-type m <sparse-vector>)
  (assume k exact-integer?)
  (sparse-vector-exists? m k))

(define (imapping-empty? m)
  (assume-type m <sparse-vector>)
  (zero? (sparse-vector-num-entries m)))

(define (imapping-disjoint? m1 m2)
  (receive (k v) (sparse-vector-find (^[k _] (sparse-vector-exists? m2 k))
                                     m1)
    (not k)))

(define (imapping-ref m k
                      :optional (failure #f)
                                (success identity))
  (assume-type m <sparse-vector>)
  (let1 v (sparse-vector-ref m k *unique*)
    (if (eq? v *unique*)
      (if failure
        (failure)
        (errorf "imapping %S doesn't have an entry for %S" m k))
      (success v))))

(define (imapping-lookup m k)
  (assume-type m <sparse-vector>)
  (let1 v (sparse-vector-ref m k *unique*)
    (if (eq? v *unique*)
      (just v)
      (nothing))))
