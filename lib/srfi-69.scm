;;;
;;; srfi-69  Basic Hash Tables
;;;

;; This is a thin wrapper to the Gauche's native hashtable support.

(define-module srfi-69
  (use srfi-13 :prefix srfi-13:) ; string-hash
  (export make-hash-table hash-table? alist->hash-table
          hash-table-equivalence-function hash-table-hash-function
          hash-table-ref hash-table-ref/default
          hash-table-set! hash-table-delete!
          hash-table-exists? hash-table-update!
          hash-table-update!/default
          hash-table-size hash-table-keys hash-table-values
          hash-table-walk hash-table-fold hash-table->alist
          hash-table-copy hash-table-merge!
          hash string-hash string-ci-hash hash-by-identity))
(select-module srfi-69)

;; These procedures are the same as Gauche's built-in:
;; hash-table?       hash-table-delete!   hash-table-exists?
;; hash-table-keys   hash-table-values    hash-table-fold
;;hash-table->alist hash-table-copy 

(define-constant *hasher-range* (+ (greatest-fixnum) 1))

(define (%choose-comparator equal hasher) ; equal never be #f.
  (if hasher
    (make-comparator #t equal #f (^[obj] (hasher obj *hasher-range*)))
    (cond [(eq? equal equal?)      equal-comparator]
          [(eq? equal eqv?)        eqv-comparator]
          [(eq? equal eq?)         eq-comparator]
          [(eq? equal string=?)    string-comparator]
          [(eq? equal string-ci=?) string-ci-comparator]
          [else (make-comparator #t equal #f (with-module gauche hash))])))

(define (make-hash-table :optional (equal equal?) (hasher #f) :rest opts)
  ((with-module gauche make-hash-table)
   (%choose-comparator equal hasher)))

(define (alist->hash-table alist :optional (equal equal?) (hasher #f) :rest opts)
  ((with-module gauche alist->hash-table)
   alist (%choose-comparator equal hasher)))

(define (hash-table-equivalence-function ht)
  (comparator-equality-predicate (hash-table-comparator ht)))

;; NB: srfi-69's hash function must take second argument.
(define (hash-table-hash-function ht)
  (let1 h (comparator-hash-function (hash-table-comparator ht))
    (^[obj bound] (modulo (h obj) bound))))

(define *unique* (list #f))

(define (no-key-thunk)
  (error "Hashtable has no key"))  ; maybe custom condition?

(define (hash-table-ref ht key :optional (thunk no-key-thunk))
  (let1 r (hash-table-get ht key *unique*)
    (if (eq? r *unique*)
      (thunk)
      r)))

(define (hash-table-ref/default ht key default)
  (hash-table-get ht key default))

(define (hash-table-set! ht key val)
  (hash-table-put! ht key val))

(define (hash-table-update! ht key proc :optional (thunk no-key-thunk))
  ((with-module gauche hash-table-update!)
   ht key
   (^[v] (if (eq? v *unique*)
           (thunk)
           (proc v)))
   *unique*))

(define (hash-table-update!/default ht key proc default)
  ((with-module gauche hash-table-update!) ht key proc default))

(define hash-table-size hash-table-num-entries)

(define (hash-table-walk ht proc) (hash-table-for-each ht proc))

(define (hash-table-merge! ht1 ht2)
  (hash-table-for-each ht2 (^[k v] (hash-table-put! ht1 k v)))
  ht1)

(define (%maybe-bounded proc obj bound)
  (let1 h (proc obj)
    (if bound (modulo h bound) h)))

(define (hash obj :optional (bound #f))
  (%maybe-bounded (with-module gauche hash) obj bound))

(define (string-hash obj :optional (bound #f))
  (if bound (srfi-13:string-hash obj bound) (srfi-13:string-hash obj)))

(define (string-ci-hash obj :optional (bound #f))
  (if bound (srfi-13:string-hash-ci obj bound) (srfi-13:string-hash-ci obj)))

(define (hash-by-identity obj :optional (bound #f))
  (%maybe-bounded eq-hash obj bound))
