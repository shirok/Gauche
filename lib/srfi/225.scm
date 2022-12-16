;;;
;;; SRFI-225 - Dictionaries
;;;
;;;   Copyright (c) 2022  Shiro Kawai  <shiro@acm.org>
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

(define-module srfi.225
  (use gauche.dictionary)
  (use util.match)
  (export dictionary? dict-empty? dict-contains? dict=? dict-pure?
          dict-ref dict-ref/default dict-comparator
          dict-set! dict-adjoin! dict-delete! dict-delete-all!
          dict-intern! dict-update! dict-update/default!
          dict-pop! dict-find-update!
          dict-map dict-filter dict-remove
          dict-size dict-count dict-any dict-every
          dict-keys dict-values dict-fold dict-map->list
          dict->alist dict-for-each dict->generator
          dict-set!-accumulator dict-adjoin!-accumulator

          dto? make-dto dto-ref make-alist-dto

          dictionary-error dictionary-error?
          dictionary-message dictionary-irritants

          dictionary?-id dict-find-update!-id dict-comaprator-id
          dict-map-id dict-pure?id dict-remove-id dict-size-id dict->alist-id
          dict-adjoin!-accumulator-id dict-adjoin!-id dict-any-id
          dict-contains?-id dict-count-id dict-delete-all!-id dict-delete!-id
          dict-empty?-id dict-entries-id dict-every-id dict-filter-id
          dict-fold-id dict-for-each-id dict-intern!-id dict-keys-id
          dict-map->list-id dict-map-id dict-pop!-id dict-ref-id
          dict-ref/default-id dict-remove-id dict-replace!-id
          dict-set!-accumulator-id dict-set!-id dict-update!-id
          dict-update/default!-id dict-values-id dict=?-id dict->generator-id

          srrfi-69-dto hash-table-dto srfi-126-dto
          mapping-dto hash-mapping-dto
          eqv-alist-dto equal-alist-dto))
(select-module srfi.225)

(define-syntax define-proc-ids
  (syntax-rules ()
    [(_ id ...)
     (begin
       (define-constant id 'id) ...
       (define-constant *proc-ids* '(id ...)))]))

(define-proc-ids
  dictionary?-id dict-find-update!-id dict-comaprator-id
  dict-map-id dict-pure?-id dict-remove-id dict-size-id dict->alist-id
  dict-adjoin!-accumulator-id dict-adjoin!-id dict-any-id
  dict-contains?-id dict-count-id dict-delete-all!-id dict-delete!-id
  dict-empty?-id dict-entries-id dict-every-id dict-filter-id
  dict-fold-id dict-for-each-id dict-intern!-id dict-keys-id
  dict-map->list-id dict-map-id dict-pop!-id dict-ref-id
  dict-ref/default-id dict-remove-id dict-replace!-id
  dict-set!-accumulator-id dict-set!-id dict-update!-id
  dict-update/default!-id dict-values-id dict=?-id dict->generator-id)

(define-class <dto> ()
  (;; minimum neccessary proc-ids
   (dictionary?-id :init-keyword :dictionary?-id)
   (dict-find-update!-id :init-keyword :dict-find-update!-id)
   (dict-comaprator-id :init-keyword :dict-comaprator-id)
   (dict-map-id :init-keyword :dict-map-id)
   (dict-pure?-id :init-keyword :dict-pure?-id)
   (dict-remove-id :init-keyword :dict-remove-id)
   (dict-size-id :init-keyword :dict-size-id)
   ;; optional proc-ids
   (dict->alist-id :init-keyword :dict->alist-id)
   (dict-adjoin!-accumulator-id :init-keyword :dict-adjoin!-accumulator-id)
   (dict-adjoin!-id :init-keyword :dict-adjoin!-id)
   (dict-any-id :init-keyword :dict-any-id)
   (dict-contains?-id :init-keyword :dict-contains?-id)
   (dict-count-id :init-keyword :dict-count-id)
   (dict-delete-all!-id :init-keyword :dict-delete-all!-id)
   (dict-delete!-id :init-keyword :dict-delete!-id)
   (dict-empty?-id :init-keyword :dict-empty?-id)
   (dict-entries-id :init-keyword :dict-entries-id)
   (dict-every-id :init-keyword :dict-every-id)
   (dict-filter-id :init-keyword :dict-filter-id)
   (dict-fold-id :init-keyword :dict-fold-id)
   (dict-for-each-id :init-keyword :dict-for-each-id)
   (dict-intern!-id :init-keyword :dict-intern!-id)
   (dict-keys-id :init-keyword :dict-keys-id)
   (dict-map->list-id :init-keyword :dict-map->list-id)
   (dict-map-id :init-keyword :dict-map-id)
   (dict-pop!-id :init-keyword :dict-pop!-id)
   (dict-ref-id :init-keyword :dict-ref-id)
   (dict-ref/default-id :init-keyword :dict-ref/default-id)
   (dict-remove-id :init-keyword :dict-remove-id)
   (dict-replace!-id :init-keyword :dict-replace!-id)
   (dict-set!-accumulator-id :init-keyword :dict-set!-accumulator-id)
   (dict-set!-id :init-keyword :dict-set!-id)
   (dict-update!-id :init-keyword :dict-update!-id)
   (dict-update/default!-id :init-keyword :dict-update/default!-id)
   (dict-values-id :init-keyword :dict-values-id)
   (dict=?-id :init-keyword :dict=?-id)
   (dict->generator-id :init-keyword :dict->generator-id)
   ;; Gauche extension
   (name :init-keyword :name)))

;; API
;; portable constructor
;; TODO:
;;  - provide default procedures for optional keywords
;;  - check mandatory proc-ids
(define (make-dto . args)
  (assume (even? (length args))
          "Argument list must have even elements:" args)
  (let loop ([args args] [r '()])
    (if (null? args)
      (apply make <dto> (reverse r))
      (match args
        [(proc-id proc . rest)
         (unless (memq proc-id *proc-ids*)
           (error "Unknown proc-id:" proc-id))
         (loop rest (cons* proc (make-keyword proc-id) r))]))))

;; API
(define (dictionary? dto obj)
  (assume-type dto <dto>)
  ((~ dto'dictionary?-id) obj))


(define-inline (assume-dict dto dict)
  (unless ((~ dto'dictionary?-id) dict)
    (if-let1 name (~ dto'name)
      (errorf "Argument is not a supposed dictionary (~a): ~s" name dict)
      (errorf "Argument is not a supposed dictionary: ~s" dict))))

;; API
(define (dict-empty? dto dict)
  (assume-type dto <dto>)
  (assume-dict dto dict)
  ((~ dto 'dict-empty?-id) dict))

;; API
(define (dict-contains? dto dict key)
  (assume-type dto <dto>)
  (assume-dict dto dict)
  ((~ dto 'dict-contains?-id) dict key))

;; API
(define (dict=? dto dict1 dict2)
  (assume-type dto <dto>)
  (assume-dict dto dict1)
  (assume-dict dto dict2)
  ((~ dto 'dict=?-id) dict1 dict2))

;; API
(define (dict-pure? dti dict)
  (assume-type dto <dto>)
  ((~ dti 'dict-pure?-id) dict))
