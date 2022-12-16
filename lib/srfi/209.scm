;;;
;;; SRFI-209 - Enums and enum sets
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

(define-module srfi.209
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
          enum-type->enum-set enum-set enum-empty-set
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
(select-module srfi.209)

(define-class <enum-type> (<collection>)
  (;; All slots are private
   (%name :init-keyword :name
          :init-value #f)
   (%ordinal->enum :init-keyword :ordinal->enum) ;vector
   (%name->enum :init-keyword :name->enum)       ;hashtable
   ))

(define (%enum-type-name etype)
  (cond [(~ etype'%name)]
        [(zero? (vector-length (~ etype'%ordinal->enum))) "()"]
        [else (format "(~s ...)"
                      (enum-name (vector-ref (~ etype'%ordinal->enum) 0)))]))

(define-method write-object ((etype <enum-type>) port)
  (format port "#<enum-type ~a>" (%enum-type-name etype)))

(define-method describe ((etype <enum-type>))
  (describe-common etype)
  (format #t "    name: ~s\n" (~ etype'%name))
  (format #t "    size: ~s\n" (size-of (~ etype'%ordinal->enum)))
  (format #t " members: ~s\n" (map enum-name (~ etype'%ordinal->enum)))
  (values))

(define-record-type <enum> %make-enum enum?
  (type enum-type)
  (name enum-name)
  (ordinal enum-ordinal)
  (value enum-value))

(define-method write-object ((enum <enum>) port)
  (format port "#<enum ~a>" (enum-name enum)))

(define-class <enum-set> (<collection>)
  ((enum-type :init-keyword :enum-type)
   (members :init-keyword :members)))   ;bitvector

(define-method write-object ((eset <enum-set>) port)
  (format port "#<enum-set ~a ~,,,,50:s>"
          (%enum-type-name (~ eset'enum-type))
          (enum-set->list eset)))

(define-method describe ((eset <enum-set>))
  (describe-common eset)
  (format #t "    type: ~s\n" (~ eset'enum-type))
  (format #t " members: ~s\n" (enum-set-map->list enum-name eset)))

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
    (unless (= (vector-length (~ et'%ordinal->enum))
               (hash-table-num-entries h))
      (error "Duplicate enum name: " elts))
    et))

(define (enum-type? obj) (is-a? obj <enum-type>))

(define (enum-type-contains? etype enum)
  (assume-type etype <enum-type>)
  (assume-type enum <enum>)
  (eq? (enum-type enum) etype))

(define (%assume-same-etype etype e0 e1)
  (assume (eqv? etype (enum-type e1))
          "Enums don't belong to the same enum type:" e0 e1))

(define (enum=? e0 e1 . es)
  (define etype (enum-type e0))
  (%assume-same-etype etype e0 e1)
  (and (eq? e0 e1)
       (or (null? es)
           (any (^e (%assume-same-etype etype e0 e)
                    (eq? e0 e))
                es))))

(define (%enum-compare op e0 e1 es)
  (define etype (enum-type e0))
  (%assume-same-etype etype e0 e1)
  (and (op (enum-ordinal e0) (enum-ordinal e1))
       (or (null? es)
           (any (^[ea eb]
                  (%assume-same-etype etype e0 eb)
                  (op (enum-ordinal ea) (enum-ordinal eb)))
                (cons e1 es)
                es))))

(define (enum<? e0 e1 . es)  (%enum-compare < e0 e1 es))
(define (enum<=? e0 e1 . es) (%enum-compare <= e0 e1 es))
(define (enum>? e0 e1 . es)  (%enum-compare > e0 e1 es))
(define (enum>=? e0 e1 . es) (%enum-compare >= e0 e1 es))

(define (enum-name->enum etype name)
  (assume-type etype <enum-type>)
  (hash-table-get (~ etype'%name->enum) name #f))

(define (enum-ordinal->enum etype n)
  (assume-type etype <enum-type>)
  (vector-ref (~ etype'%ordinal->enum) n #f))

(define (enum-name->ordinal etype name)
  (if-let1 e (enum-name->enum etype name)
    (enum-ordinal e)
    (errorf "enum-type ~s doesn't have enum named ~s" etype name)))

(define (enum-name->value etype name)
  (if-let1 e (enum-name->enum etype name)
    (enum-value e)
    (errorf "enum-type ~s doesn't have enum named ~s" etype name)))

(define (enum-ordinal->name etype n)
  (if-let1 e (enum-ordinal->enum etype n)
    (enum-name e)
    (errorf "enum-type ~s doesn't have enum with oridnal ~s" etype n)))

(define (enum-ordinal->value etype n)
  (if-let1 e (enum-ordinal->enum etype n)
    (enum-value e)
    (errorf "enum-type ~s doesn't have enum with oridnal ~s" etype n)))

(define (enum-type-size etype)
  (assume-type etype <enum-type>)
  (vector-length (~ etype'%ordinal->enum)))

(define (enum-min etype)
  (let1 t (enum-type-size etype)
    (when (zero? t)
      (error "Cannot take enum-min from zero-element enum-type:" etype))
    (~ etype'%ordinal->enum 0)))

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

(define (enum-next enum)
  (define etype (enum-type enum))
  (and (< (enum-ordinal enum) (- (enum-type-size etype) 1))
       (enum-ordinal->enum etype (+ (enum-ordinal enum) 1))))

(define (enum-prev enum)
  (define etype (enum-type enum))
  (and (> (enum-ordinal enum) 0)
       (enum-ordinal->enum etype (- (enum-ordinal enum) 1))))

(define (make-enum-comparator etype)
  (make-comparator (^e (and (enum? e) (eqv? (enum-type e) etype)))
                   enum=?
                   enum<?
                   default-hash))

;;
;; enum-set
;;

(define (enum-type->enum-set etype)
  (make <enum-set>
    :enum-type etype
    :members (make-bitvector (enum-type-size etype) 1)))

(define (enum-set enum-type . enums)
  (list->enum-set enum-type enums))

;; NB: This is missing from srfi text, but exists in reference impl and tests
(define (enum-empty-set etype)
  (assume-type etype <enum-type>)
  (make <enum-set>
    :enum-type etype
    :members (make-bitvector (enum-type-size etype) 0)))

(define (enum-set-type eset)
  (assume-type eset <enum-set>)
  (~ eset'enum-type))

(define (enum-set-indexer eset)         ;R6RS
  (assume-type eset <enum-set>)
  (let1 etype (enum-set-type eset)
    (^[name] (and-let1 e (enum-name->enum etype name)
               ;; NB: Discrepancy - srfi text says e must be in eset,
               ;; but reference impl and test don't.
               ;; (and (enum-set-contains? eset e)
               ;;      (enum-ordinal e))
               (enum-ordinal e)
               ))))

(define (list->enum-set etype enums)
  (assume-type etype <enum-type>)
  (let1 members (make-bitvector (enum-type-size etype) 0)
    (dolist [e enums]
      (unless (enum-type-contains? etype e)
        (errorf "enum ~s isn't a member of given enum type ~s" e etype))
      (bitvector-set! members (enum-ordinal e) 1))
    (make <enum-set> :enum-type etype :members members)))

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
                     enum-set))))

(define (enum-set-copy eset)
  (assume-type eset <enum-set>)
  (make <enum-set>
    :enum-type (~ eset'enum-type)
    :members (bitvector-copy (~ eset'members))))

(define (make-enumeration names)        ;R6RS
  (enum-type->enum-set (make-enum-type (map (^e `(,e ,e)) names))))

(define (enum-set-universe eset)        ;R6RS
  (assume-type eset <enum-set>)
  (enum-type->enum-set (enum-set-type eset)))

(define (enum-set-constructor eset)     ;R6RS
  (define etype (enum-set-type eset))
  (^[names] (let1 es (map (^n (or (enum-name->enum etype n)
                                  (errorf "enum-type ~s doesn't have name: ~s"
                                          etype n)))
                          names)
              (list->enum-set etype es))))


(define (enum-set? obj) (is-a? obj <enum-set>))

(define (enum-set-contains? enum-set enum)
  (assume-type enum <enum>)
  (assume-type enum-set <enum-set>)
  (and (eqv? (enum-type enum) (enum-set-type enum-set))
       (bitvector-ref/bool (~ enum-set'members) (enum-ordinal enum))))

;; R6RS
(define (enum-set-member? sym eset)
  (assume-type sym <symbol>)
  (assume-type eset <enum-set>)
  (and-let1 e (enum-name->enum (enum-set-type eset) sym)
    (bitvector-ref/bool (~ eset'members) (enum-ordinal e))))

(define (enum-set-empty? eset)
  (assume-type eset <enum-set>)
  (bitvector-every-value? (~ eset'members) 0))

;; NB: srfi is unclear if two sets are not from the same enum-type.
(define (enum-set-disjoint? enum-set1 enum-set2)
  (assume (eqv? (enum-set-type enum-set1) (enum-set-type enum-set2)))
  (bitvector-every-value? (bitvector-and (~ enum-set1'members)
                                         (~ enum-set2'members))
                          0))

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
(define (enum-set-subset? enum-set1 enum-set2)
  (enum-set-every? (^e (enum-set-member? (enum-name e) enum-set2))
                   enum-set1))

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
            (bitvector-set! bv (enum-ordinal e) value)
            bv)
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
           (cut fold (^[enums bv] (%update-members! enum-set bv enums #f))
                <> lists))
  enum-set)

(define (enum-set-delete-all enum-set . lists)
  (apply enum-set-delete-all! (enum-set-copy enum-set) lists))

(define (enum-set-size enum-set)
  (assume-type enum-set <enum-set>)
  (bitvector-count #t (~ enum-set'members)))

(define (enum-set->enum-list enum-set)
  (enum-set-map->list identity enum-set))

(define (enum-set->list enum-set)       ;R6RS
  (enum-set-map->list enum-name enum-set))

(define (enum-set-count pred enum-set)
  (rlet1 c 0
    (enum-set-for-each (^e (when (pred e) (inc! c))) enum-set)))

(define (enum-set-filter pred enum-set)
  (define bv (make-bitvector (bitvector-length (~ enum-set'members)) 0))
  ($ enum-set-for-each
     (^e (when (pred e) (bitvector-set! bv (enum-ordinal e) 1)))
     enum-set)
  (make <enum-set>
    :enum-type (enum-set-type enum-set)
    :members bv))

(define (enum-set-remove pred enum-set)
  (define bv (bitvector-copy (~ enum-set'members)))
  ($ enum-set-for-each
     (^e (when (pred e) (bitvector-set! bv (enum-ordinal e) 0)))
     enum-set)
  (make <enum-set>
    :enum-type (enum-set-type enum-set)
    :members bv))

(define (enum-set-map->list proc enum-set)
  (define etype (enum-set-type enum-set))
  ($ bitvector-value-map-index->list
     (^i (proc (enum-ordinal->enum etype i)))
     (~ enum-set'members) #t))

(define (enum-set-for-each proc enum-set)
  (define etype (enum-set-type enum-set))
  ($ bitvector-value-for-each-index
     (^i (proc (enum-ordinal->enum etype i)))
     (~ enum-set'members) #t))

(define (enum-set-fold proc seed enum-set)
  (define etype (enum-set-type enum-set))
  ($ bitvector-value-fold-index
     (^[i s] (proc (enum-ordinal->enum etype i) s))
     seed
     (~ enum-set'members) #t))

(define (enum-set-complement enum-set)
  (make <enum-set>
    :enum-type (enum-set-type enum-set)
    :members (bitvector-not (~ enum-set'members))))

(define (enum-set-complement! enum-set)
  (update! (~ enum-set'members) bitvector-not!)
  enum-set)

(define (enum-set-union enum-set1 enum-set2)
  (assume (eqv? (enum-set-type enum-set1) (enum-set-type enum-set2)))
  (make <enum-set>
    :enum-type (enum-set-type enum-set1)
    :members (bitvector-ior (~ enum-set1'members) (~ enum-set2'members))))

(define (enum-set-union! enum-set1 enum-set2)
  (assume (eqv? (enum-set-type enum-set1) (enum-set-type enum-set2)))
  (update! (~ enum-set1'members)
           (cute bitvector-ior! <> (~ enum-set2'members)))
  enum-set1)

(define (enum-set-intersection enum-set1 enum-set2)
  (assume (eqv? (enum-set-type enum-set1) (enum-set-type enum-set2)))
  (make <enum-set>
    :enum-type (enum-set-type enum-set1)
    :members (bitvector-and (~ enum-set1'members) (~ enum-set2'members))))

(define (enum-set-intersection! enum-set1 enum-set2)
  (assume (eqv? (enum-set-type enum-set1) (enum-set-type enum-set2)))
  (update! (~ enum-set1'members)
           (cute bitvector-and! <> (~ enum-set2'members)))
  enum-set1)

(define (enum-set-difference enum-set1 enum-set2)
  (assume (eqv? (enum-set-type enum-set1) (enum-set-type enum-set2)))
  (make <enum-set>
    :enum-type (enum-set-type enum-set1)
    :members (bitvector-andc2 (~ enum-set1'members) (~ enum-set2'members))))

(define (enum-set-difference! enum-set1 enum-set2)
  (assume (eqv? (enum-set-type enum-set1) (enum-set-type enum-set2)))
  (update! (~ enum-set1'members)
           (cute bitvector-andc2! <> (~ enum-set2'members)))
  enum-set1)

(define (enum-set-xor enum-set1 enum-set2)
  (assume (eqv? (enum-set-type enum-set1) (enum-set-type enum-set2)))
  (make <enum-set>
    :enum-type (enum-set-type enum-set1)
    :members (bitvector-xor (~ enum-set1'members) (~ enum-set2'members))))

(define (enum-set-xor! enum-set1 enum-set2)
  (assume (eqv? (enum-set-type enum-set1) (enum-set-type enum-set2)))
  (update! (~ enum-set1'members)
           (cute bitvector-xor! <> (~ enum-set2'members)))
  enum-set1)

;;
;; Syntax
;;

(define-syntax define-enum
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ type-name (name-value ...) set-ctor)
        (let ([etype (make-enum-type name-value type-name)])
          (quasirename r
            `(begin
               (define-syntax ,type-name
                 (er-macro-transformer
                  (^[ff rr cc]
                    (match ff
                      [(_ name) (enum-name->enum ,etype name)]
                      [_ (error "enum type macro must take a symbol:" ff)]))))
               (define-syntax ,set-ctor
                 (er-macro-transformer
                  (^[ff rr cc]
                    (list->enum-set ,etype
                                    (map (cut enum-name->enum ,etype <>)
                                         (cdr ff))))))
               )))]
       ))))

(define-syntax define-enumeration       ;R6RS
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ type-name (name-value ...) set-ctor)
        (let ([etype (make-enum-type name-value type-name)])
          (quasirename r
            `(begin
               (define-syntax ,type-name
                 (er-macro-transformer
                  (^[ff rr cc]
                    (match ff
                      [(_ ,'name)
                       (if (enum-name->enum ,etype ,'name)
                         (quasirename rr `',,'name)
                         (errorf "enum ~s doesn't belong to enum type ~a"
                                 ,'name ',type-name))]
                      [_ (error "enum type macro must take a symbol:" ff)]))))
               (define-syntax ,set-ctor
                 (er-macro-transformer
                  (^[ff rr cc]
                    (list->enum-set ,etype
                                    (map (cut enum-name->enum ,etype <>)
                                         (cdr ff))))))
               )))]
       ))))
