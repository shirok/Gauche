;;;
;;; srfi-209 - Enums and enum sets
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

(define-module srfi-209
  (use gauche.record)
  (use gauche.sequence)
  (use gauche.bitvector)
  (use scheme.set)
  (use util.match)
  (export enum-type? enum? enum-type-contains?
          enum=? enum<? enum>? enum<=? enum>=?
          make-enum-type
          enum-type enum-name enum-ordinal enum-value
          enum-name->enum enum-ordinal->enum
          enum-name->ordinal enum-name->value
          enum-ordinal->name enum-ordinal->value
          enum-type-size enum-min enum-max
          enum-type-enums enum-type-names enum-type-values
          enum-next enum-prev
          make-enum-comparator
          enum-type->enum-set enum-set
          list->enum-set
          enum-set-projection
          enum-set-copy
          enum-set? enum-set-contains? enum-set-empty?
          enum-set-disjoint?
          enum-set=? enum-set<? enum-set>? enum-set<=? enum-set>=?
          enum-set-subset?
          enum-set-any? enum-set-every?
          enum-set-type
          enum-set-adjoin enum-set-adjoin!
          enum-set-delete enum-set-delete!
          enum-set-delete-all enum-set-delete-all!
          enum-set-size enum-set->enum-list
          enum-set-count enum-set-filter enum-set-remove
          enum-set-map->list enum-set-for-each enum-set-fold
          enum-set-complement enum-set-complement!
          enum-set-union enum-set-union!
          enum-set-intersection enum-set-intersection!
          enum-set-difference enum-set-difference!
          enum-set-xor enum-set-xor!

          define-enum

          ;; R6RS compatibility
          make-enumeration
          enum-set-universe
          enum-set-constructor
          enum-set-member?
          enum-set-indexer
          enum-set->list
          define-enumeration
          ))
(select-module srfi-209)

(define-class <enum-type> (<collection>)
  (;; All slots are private
   (%name :init-keyword :name
          :init-value #f)
   (%ordinal->enum :init-keyword :ordinal->enum) ;vector
   (%name->enum :init-keyword :name->enum)       ;hashtable
   ))

(define-record-type <enum> %make-enum enum?
  (type enum-type)
  (name enum-name)
  (ordinal enum-ordinal)
  (value enum-value))

(define-class <enum-set> (<collection>)
  ((enum-type :init-keyword :enum-type)
   (members :init-keyword :members)))   ;bitvector

;; Constructor
;; elts : elt ...
;; elt : symbol | (symbol value)
(define (make-enum-type elts :optional (name #f))
  (let* ([elts. (map-with-index (^[i elt]
                                  (match elt
                                    [(? symbol?) `(,elt ,i ,i)]
                                    [((? symbol? sym) val) `(,sym ,i ,val)]
                                    [_ (error "Bad enum entry: " elt)]))
                                elts)]
         [h (make-hash-table eq-comparator)]
         [et (make <enum-type>
               :name name
               :name->enum h)])
    (dolist [e elts.]
      (match-let1 [sym ord val] e
        (hash-table-put! h sym (%make-enum et sym ord val))))
    (set! (~ et'%ordinal->enum)
          (map-to <vector> (^e (hash-table-get h (car e))) elts.))
    (unless (= (vector-lenngth (~ et'%ordinal->enum))
               (hash-table-num-entries h))
      (error "Duplicate enum name: " elts))
    et))

;; API
(define (enum-type? obj) (is-a? obj <enum-type>))

(define (enum-type-contains? etype enum)
  (assume-type etype <enum-type>)
  (assume-type enum <enum>)
  (eq? (enum-type enum) etype))

(define (enum-name->enum enum-type name)
  (assume-type etype <enum-type>)
  (hash-table-get (~ enum-type'%name->enum) name #f))

(define (enum-ordinal->enum enum-type n)
  (assume-type etype <enum-type>)
  (vector-ref (~ enum-type'%ordinal->enum) n #f))

(define (enum-name->ordinal enum-type name)
  (if-let1 e (enum-name->enum enum-type name)
    (enum-ordinal e)
    (errorf "enum-type ~s doesn't have enum named ~s" enum-type name)))

(define (enum-name->value enum-type name)
  (if-let1 e (enum-name->enum enum-type name)
    (enum-value e)
    (errorf "enum-type ~s doesn't have enum named ~s" enum-type name)))

(define (enum-ordinal->name enum-type n)
  (if-let1 e (enum-ordinal->enum enum-type n)
    (enum-name e)
    (errorf "enum-type ~s doesn't have enum with oridnal ~s" enum-type n)))

(define (enum-ordinal->value enum-type n)
  (if-let1 e (enum-ordinal->enum enum-type n)
    (enum-value e)
    (errorf "enum-type ~s doesn't have enum with oridnal ~s" enum-type n)))

(define (enum-type-size enum-type)
  (assume-type enum-type <enum-type>)
  (vector-length (~ enum-type'%ordinal->enum)))

(define (enum-min enum-type)
  (let1 t (enum-type-size enum-type)
    (when (zero? t)
      (error "Cannot take enum-min from zero-element enum-type:" enum-type))
    (~ enum-type'%ordinal->enum 0)))

(define (enum-max enum-type)
  (let1 t (enum-type-size enum-type)
    (when (zero? t)
      (error "Cannot take enum-min from zero-element enum-type:" enum-type))
    (~ enum-type'%ordinal->enum (- t 1))))

(define (enum-type-enums enum-type)
  (vector->list (~ enum-type'%ordinal->enum)))

(define (enum-type-names enum-type)
  (assume-type enum-type <enum-type>)
  (map enum-name (enum-type-enums enum-type)))

(define (enum-type-values enum-type)
  (assume-type enum-type <enum-type>)
  (map enum-value (enum-type-enums enum-type)))


;;
;; enum-set
;;

(define (enum-type->enum-set enum-type)
  (make <enum-type>
    :enum-type enum-type
    :members (make-bitvector (enum-type-size enum-type) 1)))

(define (enum-set enum-type . enums)
  (list->enum-set enum-type enums))

(define (list->enum-set enum-type enums)
  (let1 members (make-bitvector (enum-type-size enum-type) 0)
    (dolist [e enums]
      (unless (enum-type-contains? enum-type e)
        (errorf "enum ~s isn't a member of given enum type ~s" e enum-type))
      (bitvetor-set! members (enum-ordinal e) 1))
    (make <enum-type> :enum-type enum-type :members members)))

;; NB: srfi isn't clear if enum-set contains an enum whose name
;; is not in enum-type-or-set.
(define (enum-set-projection enum-type-or-set enum-set)
  (let1 type (cond [(enum-type? enum-type-or-set) enum-type-or-set]
                   [(enum-set? enum-type-or-set) (enum-set-type enum-type-or-set)]
                   [else (error "enum-type or enum-set required, but got:"
                                enum-type-or-set)])
    (list->enum-set type
                    (enum-set-map->list
                     (^e (enum-name->enum type (enum-name e)))
                     enums))))

(define (enum-set-copy enum-set)
  (make <enum-set>
    :enum-type (~ enum-set'enum-type)
    :members (bitvector-copy (~ enum-set'members))))

(define (enum-set? obj) (is-a? obj <enum-set>))

(define (enum-set-contains? enum-set enum)
  (assume-type enum <enum>)
  (assume-type enum-set <enum-set>)
  (and (eqv? (enum-type enum) (enum-set-type enum-set))
       (bitvector-ref/bool (~ enum-set'members) (enum-ordinal enum))))

;; R6RS
(define (enum-set-member? sym enum-set)
  (assume-type sym <symbol>)
  (assume-type enum-set <enum-set>)
  (boolean (enum-name->enum (enum-set-type enum-set) sym)))

(define (enum-set-empty? enum-set)
  (assume-type enum-set <enum-set>)
  (= (bitvector-first-bit 1 (~ enum-set'members)) -1))

;; NB: srfi is unclear if two sets are not from the same enum-type.
(define (enum-set-disjoint? enum-set1 enum-set2)
  (assume (eqv? (enum-set-type enum-set1) (enum-set-type enum-set2)))
  (bitvector-zero? (bitvector-and (~ enum-set1'members)
                                  (~ enum-set2'members))))

(define (enum-set=? enum-set1 enum-set2)
  (assume (eqv? (enum-set-type enum-set1) (enum-set-type enum-set2)))
  (bitvector=? (~ enum-set1'members) (~ enum-set2'members)))

(define (enum-set<=? enum-set1 enum-set2)
  (assume (eqv? (enum-set-type enum-set1) (enum-set-type enum-set2)))
  (bitvector=? (~ enum-set1'members)
               (bitvector-and (~ enum-set1'members)
                              (~ enum-set2'members))))

(define (enum-set<? enum-set1 enum-set2)
  (and (not (enum-set=? enum-set1 enum-set2))
       (enum-set<=? enum-set1 enum-set2)))

(define (enum-set>=? enum-set1 enum-set2)
  (assume (eqv? (enum-set-type enum-set1) (enum-set-type enum-set2)))
  (bitvector=? (~ enum-set2'members)
               (bitvector-and (~ enum-set2'members)
                              (~ enum-set1'members))))

(define (enum-set>? enum-set1 enum-set2)
  (and (not (enum-set=? enum-set1 enum-set2))
       (enum-set>=? enum-set1 enum-set2)))

;; Two enum sets can belong to different enum types.
(define (enum-set-subset? enum-set-1 enum-set-2)
  (enum-set-every? (^e (enum-set-member? (enum-name e) enum-set2))
                   enum-set-1))

(define (enum-set-any? pred enum-set)
  (assume-type enum-set <enum-set>)
  (let1 etype (enum-set-type enum-set)
    (let/cc return
      ($ bitvector-value-for-each-index
         (^i (when (pred (enum-ordinal->enum etype i))
               (return #t)))
         (~ enum-set'members) #t)
      #f)))

(define (enum-set-every? pred enum-set)
  (assume-type enum-set <enum-set>)
  (let1 etype (enum-set-type enum-set)
    (let/cc return
      ($ bitvector-value-for-each-index
         (^i (unless (pred (enum-ordinal->enum etype i))
               (return #f)))
         (~ enum-set'members) #t)
      #t)))


;; adjoin, delete


;; returns new members bitvector
(define (%update-members! enum-set bv enums value)
  (let1 etype (enum-set-type enum-set)
    (fold (^[e bv]
            (assume-type e <enum>)
            (unless (eqv? (enum-type e) etype)
              (errorf "enum ~s doesn't belong to the same enum type of ~s"
                      e enum-set))
            (bitvector-set! bv (enum-ordinal e) value))
          bv enums)))

(define (enum-set-adjoin! enum-set . enums)
  (assume-type enum-set <enum-set>)
  (update! (~ enum-set'members) (cut %update-members! enum-set <> enums #t))
  enum-set)

(define (enum-set-adjoin enum-set . enums)
  (apply enum-set-adjoin! (enum-set-copy enum-set) enums))

(define (enum-set-delete! enum-set . enums)
  (assume-type enum-set <enum-set>)
  (update! (~ enum-set'members) (cut %update-members! enum-set <> enums #f))
  enum-set)

(define (enum-set-delete enum-set . enums)
  (apply enum-set-delete! (enum-set-copy enum-set) enums))

(define (enum-set-delete-all! enum-set . lists)
  (assume-type enum-set <enum-set>)
  (update! (~ enum-set'members)
           (cut (fold (^[enums bv] (%update-members! enum-set <> enums #f))
                      <> lists)))
  enum-set)

(define (enum-set-delete-all enum-set . lists)
  (apply enum-set-delete-all! (enum-set-copy enum-set) lists))

(define (enum-set-size enum-set)
  (assume-type enum-set <enum-set>)
  (bitvector-count #t (~ enum-set'members)))

(define (enum-set->enum-list enum-set)
  (define etype (enum-set-type enum-set))
  (reverse
   (rlet1 r '()
     ($ bitvector-value-for-each-index
        (cut enum-ordinal->enum etype <>)
        (~ enum-set'members) #t))))

(define (enum-set-count pred enum-set)
  (define etype (enum-set-type enum-set))
  (rlet1 c 0
    ($ bitvector-value-for-each-index
       (^i (when (pred (enum-ordinal->enum etype i))
             (inc! c)))
       (~ enum-set'members) #t)))
