;;;
;;; relation.scm - relational operations
;;;
;;;  Copyright (c) 2005-2019  Shiro Kawai  <shiro@acm.org>
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

;;; Given set of values S1, S2, ..., Sn, a relation R is a set of tuples
;;; such that the first element of a tuple is from S1, the second from
;;; S2, ..., and the n-th from Sn.  In another word, R is a subset of
;;; Cartesian product of S1, ..., Sn.
;;;
;;; (This is actually THE definition from the original Codd's 1970 paper,
;;; "A Relational Model of Data for Large Shared Data Banks", in
;;; CACM 13(6) pp.377--387.)
;;;
;;; This definition can be applied to various datasets: A set of Gauche
;;; instances is a relation, if you view each instance as a tuple and
;;; each slot value as the actual values.  A list of lists can be a
;;; relation.  A stream that reads from CSV table produces a relation.
;;; Thus it would be useful to provide a module that implements generic
;;; operations on relations, no matter how the actual representation is.
;;;
;;; From the operational point of view, we can treat any datastructure
;;; that provides the following two properties.
;;;
;;;  - a collection of tuples (rows), which should be a <collection> or
;;     one of its subclasses (e.g. <sequence>).
;;;  - metadata to retrieve individual column value from a given tuple.
;;;
;;; One can implement a relation by writing at least four methods,
;;; relation-column-names, relation-column-getter and relation-column-setter,
;;; and relation-rows.
;;;
;;; On top of that abstraction, we can implement a bunch of useful stuff.
;;; We'll add them, but here are some ideas.
;;;
;;;  - Codd's relational operations
;;;  - A relation that wraps existing relations, adding index operation.

(define-module util.relation
  (use gauche.sequence)
  (export <relation>
          relation-column-names relation-column-name?
          relation-column-getter relation-column-setter
          relation-rows
          relation-accessor relation-modifier
          relation-ref relation-set! relation-coercer
          relation-column-getters relation-column-setters
          relation-insertable? relation-insert!
          relation-deletable? relation-delete!
          relation-fold
          <simple-relation> <object-set-relation>
          ))
(select-module util.relation)

;; An abstract base class of <relation>.
(define-class <relation> () ())

;;;-----------------------------------------------------------------
;;; Minimal set of generic functions.  Concrete implementations must
;;; implement the followings.

;; Should return a procedure that can be used as
;;   (REF <row> <column> &optional <value>)
(define-method relation-accessor ((r <relation>))
  (error "accessor method isn't defined for the relation" r))

;; Should return a procedure that can be used as
;;   (SET! <row> <column> <value>)
;; If the relation is read-only, this method can return #f.
(define-method relation-modifier ((r <relation>))
  #f)

;; Should return a sequence of column names.
(define-method relation-column-names ((r <relation>)) '())

;; Should return a <collection> of rows.  (For some relations,
;; the returned object can be <sequence>.)
(define-method relation-rows ((r <relation>)) '())

;;;-----------------------------------------------------------------
;;; Optional methods.  They can be implemented by the minimal methods
;;; above, but a concrete implementation may override these methods
;;; for efficiency.

;; Queries if COLUMN is a valid column name in a relation.
;; The default method assumes eq? comparison of the column name.
(define-method relation-column-name? ((r <relation>) column)
  (memq column (relation-column-names r)))

;; Returns a procedure that extracts the value of the column from
;; the given row.
(define-method relation-column-getter ((r <relation>) column)
  (let1 accessor (relation-accessor r)
    (lambda (row) (accessor row column))))

;; Returns a procedure that takes a row and a value, and set row's
;; column by value.
(define-method relation-column-setter ((r <relation>) column)
  (let1 modifier (relation-modifier r)
    (lambda (row v) (modifier row column v))))

;; Direct referencer
(define-method relation-ref ((r <relation>) row column . default)
  (apply (relation-accessor r) row column default))

;; Direct modifier
(define-method relation-set! ((r <relation>) row column val)
  ((relation-modifier r) row column val))

(define-method (setter relation-ref) ((r <relation>) row column val)
  (relation-set! r row column val))

;; Returns full list of getters and setters.  Usually the default
;; method is sufficient, but the implementation may want to cache
;; the list of getters, for example.
(define-method relation-column-getters ((r <relation>))
  (let1 accessor (relation-accessor r)
    (map (^c (lambda (row) (accessor row c)))
         (relation-column-names r))))

(define-method relation-column-setters ((r <relation>))
  (let1 modifier (relation-modifier r)
    (map (^c (lambda (row val) (modifier row c val)))
         (relation-column-names r))))

;; Returns a procedure that coerces a row into a sequence.
;; If the relation already uses a sequence to represent a row, it can
;; return row as is.  The default method is generic but inefficient.
(define-method relation-coercer ((r <relation>))
  (let1 getters (relation-column-getters r)
    (lambda (row) (map (cut <> row) getters))))

;; Optionally a subclass may provide them.
(define-method relation-insertable? ((r <relation>)) #f)
(define-method relation-insert! ((r <relation>) row) #f)

(define-method relation-deletable? ((r <relation>)) #f)
(define-method relation-delete! ((r <relation>) row) #f)

;;;
;;; Generic stuff
;;;

;; Cf. E.F.Codd: Extending the database relational model to capture
;; more meaning, ACM trans. on database systems 4(4) pp.397--434, Dec. 1979.

;; project
;; select
;; join

(define-method relation-fold ((r <relation>) proc seed . columns)
  (let1 getters (map (cut relation-column-getter r <>) columns)
    (fold (lambda (row seed)
            (apply proc (fold-right (lambda (getter seed)
                                      (cons (getter row) seed))
                                    (list seed)
                                    getters)))
          seed (relation-rows r))))

;;;=============================================================
;;; Concrete implementations
;;;

;;
;; <simple-relation>
;;

(define-class <simple-relation> (<relation> <collection>)
  ((columns :init-keyword :columns :init-value '()) ;; sequence of symbols
   (rows    :init-keyword :rows :init-value '())) ;; list of sequences
  )

(define-method call-with-iterator ((r <simple-relation>) proc . keys)
  (apply call-with-iterator (ref r 'rows) proc keys))

(define-method relation-rows ((r <simple-relation>))
  (slot-ref r 'rows))

(define-method relation-column-names ((r <simple-relation>))
  (ref r 'columns))

(define-method relation-accessor ((r <simple-relation>))
  (let1 columns (ref r 'columns)
    (lambda (row column . maybe-default)
      (cond
       ((find-index (cut eq? <> column) columns) => (cut ref row <>))
       ((pair? maybe-default) (car maybe-default))
       (else (error "simple-relation: invalid column:" column))))))

(define-method relation-modifier ((r <simple-relation>))
  (let1 columns (ref r 'columns)
    (lambda (row column val)
      (cond
       ((find-index (cut eq? <> column) columns)
        => (cut (setter ref) row <> val))
       (else (error "simple-relation: invalid column:" column))))))

(define-method relation-column-getter ((r <simple-relation>) column)
  (let1 ind (find-index (cut eq? <> column) (ref r 'columns))
    (unless ind (error "simple-relation: invalid column:" column))
    (lambda (row) (ref row ind))))

(define-method relation-column-setter ((r <simple-relation>) column)
  (let1 ind (find-index (cut eq? <> column) (ref r 'columns))
    (unless ind
      (error "simple-relation: invalid column:" column))
    (lambda (row v) (set! (ref row ind) v))))

(define-method relation-coercer ((r <simple-relation>)) identity)

(define-method relation-insertable? ((r <simple-relation>)) #t)
(define-method relation-insert! ((r <simple-relation>) (row <sequence>))
  (unless (= (size-of row) (length (ref r 'columns)))
    (error "simple-relation: attempt to insert a row that doesn't match the relation:" row))
  (push! (ref r 'rows) row))

(define-method relation-deletable? ((r <simple-relation>)) #t)
(define-method relation-delete! ((r <simple-relation>) (row <sequence>))
  (update! (ref r 'rows) (cut delete! row <> equal?)))

;;
;; <object-set-relation>
;;

(define-class <object-set-relation> (<relation> <collection>)
  ((class :init-keyword :class :init-value <object>)
   (rows  :init-keyword :rows  :init-value '())  ;; list of instances of class
   ))

(define-method call-with-iterator ((r <object-set-relation>) proc . keys)
  (apply call-with-iterator (ref r 'rows) proc keys))

(define-method relation-rows ((r <object-set-relation>))
  (slot-ref r 'rows))

(define-method relation-column-names ((r <object-set-relation>))
  (map slot-definition-name (class-slots (ref r 'class))))

(define-method relation-column-name? ((r <object-set-relation>) column)
  (find (lambda (slot) (eq? column (slot-definition-name slot)))
        (class-slots (ref r 'class))))

(define-method relation-column-getter ((r <object-set-relation>) column)
  (unless (relation-column-name? r column)
    (error "object-set-relation: invalid column name:" column))
  (lambda (row) (slot-ref row column)))

(define-method relation-column-setter ((r <object-set-relation>) column)
  (unless (relation-column-name? r column)
    (error "object-set-relation: invalid column name:" column))
  (lambda (row v) (slot-set! row column v)))

(define-method relation-accessor ((r <object-set-relation>))
  slot-ref)

(define-method relation-modifier ((r <object-set-relation>))
  slot-set!)

(define-method relation-coercer ((r <object-set-relation>))
  (lambda (row)
    (map (^s (slot-ref row s)) (relation-column-names r))))

(define-method relation-insertable? ((r <object-set-relation>)) #t)
(define-method relation-insert! ((r <object-set-relation>) row)
  (unless (is-a? row (ref r 'class))
    (error "object-set-relation: attempt to insert an object of mismatched class:" row))
  (push! (ref r 'rows) row))

(define-method relation-deletable? ((r <object-set-relation>)) #t)
(define-method relation-delete! ((r <object-set-relation>) row)
  (update! (ref r 'rows) (cut delete! row <> equal?)))



