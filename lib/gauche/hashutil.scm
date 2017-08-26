;;;
;;; auxiliary hashtable utilities.  to be autoloaded.
;;;
;;;   Copyright (c) 2000-2017  Shiro Kawai  <shiro@acm.org>
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

(define-module gauche.hashutil
  (export hash-table hash-table-from-pairs hash-table-r7
          hash-table-empty? hash-table-contains?
          hash-table-for-each hash-table-for-each-r7
          hash-table-map hash-table-map-r7 hash-table-map!-r7
          hash-table-fold hash-table-fold-r7
          hash-table-prune!-r7
          hash-table-seek hash-table-find
          hash-table-compare-as-sets hash-table=?
          boolean-hash char-hash char-ci-hash string-hash string-ci-hash
          symbol-hash number-hash default-hash
          hash-bound hash-salt))
(select-module gauche.hashutil)

;; TRANSIENT: Precompiling with 0.9.5 doesn't load assume-type yet.
;; Remove this after 0.9.6 release.
(cond-expand
 [gauche-0.9.5 (define-syntax assume-type
                 (syntax-rules ()
                   [(_ x type) (check-arg (cut is-a? <> type) x)]))]
 [else])

;; vararg constructor 'hash-table' has conflicting API between
;; legacy Gauche and R7RS scheme.hash-table (srfi-125).
;; R7RS's API is more convenient to use, so we eventually
;; switch to it.  Meantime, we provide original hash-table
;; with a different name, hash-table-from-pairs, and R7RS
;; hash-table as hash-table-r7.
(define (hash-table-from-pairs cmpr . kvs)
  (rlet1 h (make-hash-table cmpr)
    (for-each (^[kv] (hash-table-put! h (car kv) (cdr kv))) kvs)))

(define hash-table hash-table-from-pairs) ; transient

(define (hash-table-r7 cmpr . kvs) ; r7rs's hash-table
  (rlet1 h (make-hash-table cmpr)
    (doplist [(k v) kvs] (hash-table-put! h k v))))


(define (hash-table-empty? h) (zero? (hash-table-num-entries h))) ; r7rs

(define hash-table-contains? hash-table-exists?) ; r7rs

;; hash-table-map also disagree between Gauche and R7RS.
;; We keep Gauche API, for we have consistent interface with other *-map
;; procedures.  We add a check in case the caller mistook this with R7RS one.
(define (hash-table-map ht proc :optional arg)
  (when (not (undefined? arg))
    (error "Gauche's bulit-in hash-table-map is called with R7RS interface. \
            Use hash-table-map-r7, or say (use scheme.hash-table)."))
  (assume-type ht <hash-table>)
  (let ([eof (cons #f #f)]              ;marker
        [i   ((with-module gauche.internal %hash-table-iter) ht)])
    (let loop ([r '()])
      (receive [k v] (i eof)
        (if (eq? k eof)
          r
          (loop (cons (proc k v) r)))))))

;; This is R7RS version of hash-table-map
(define (hash-table-map-r7 proc cmpr ht) ; r7rs
  (assume-type cmpr <comparator>)
  (assume-type ht <hash-table>)
  (rlet1 r (make-hash-table cmpr)
    (hash-table-for-each ht (^[k v] (hash-table-put! r k (proc v))))))

(define (hash-table-map!-r7 proc ht) ; r7rs
  (assume-type ht <hash-table>)
  (hash-table-for-each ht (^[k v] (hash-table-put! ht k (proc v)))))

(define (hash-table-for-each ht proc)
  (assume-type ht <hash-table>)
  (let ([eof (cons #f #f)]              ;marker
        [i ((with-module gauche.internal %hash-table-iter) ht)])
    (let loop ()
      (receive [k v] (i eof)
        (unless (eq? k eof)
          (proc k v) (loop))))))

(define (hash-table-for-each-r7 proc ht) ; r7rs
  (hash-table-for-each ht proc))

(define (hash-table-fold ht kons knil)
  (assume-type ht <hash-table>)
  (let ([eof (cons #f #f)]              ;marker
        [i ((with-module gauche.internal %hash-table-iter) ht)])
    (let loop ([r knil])
      (receive [k v] (i eof)
        (if (eq? k eof)
          r
          (loop (kons k v r)))))))

(define (hash-table-fold-r7 kons knil ht) ; r7rs
  (hash-table-fold ht cons knil))

(define (hash-table-prune!-r7 proc ht) ; r7rs
  (hash-table-for-each ht (^[k v] (when (proc k v) (hash-table-delete! ht k)))))

(define (hash-table-seek hash pred succ fail)
  (assume-type hash <hash-table>)
  (let ([eof (cons #f #f)]              ;marker
        [i ((with-module gauche.internal %hash-table-iter) hash)])
    (let loop ()
      (receive [k v] (i eof)
        (cond [(eq? k eof) (fail)]
              [(pred k v) => (^r (succ r k v))]
              [else (loop)])))))
  
;; R7RS.  this doesn't align with other '*-find' API in a way that
;; it returns the result of PRED.
(define (hash-table-find hash pred :optional (failure (^[] #f)))
  (hash-table-seek hash pred (^[r k v] r) failure))

;; We delegate most hash calculation to the built-in default-hash.
;; These type-specific hash functions are mostly
;; for the compatibility of srfi-128.

(define (boolean-hash obj)
  (assume-type obj <boolean>)
  (default-hash obj))

(define (char-hash obj)
  (assume-type obj <char>)
  (default-hash obj))

(define (char-ci-hash obj)
  (assume-type obj <char>)
  (default-hash (char-foldcase obj)))

(define (string-hash obj)
  (assume-type obj <string>)
  (default-hash obj))

(autoload gauche.unicode string-foldcase)

(define (string-ci-hash obj)
  (assume-type obj <string>)
  (default-hash (string-foldcase obj)))

(define (symbol-hash obj)
  (assume-type obj <symbol>)
  (default-hash obj))

(define (number-hash obj)
  (assume-type obj <number>)
  (default-hash obj))

;; This is a placeholder to conform srfi-128.
(define (hash-bound) (greatest-fixnum))

;; Compare two hash-tables as sets.
(define (hash-table-compare-as-sets h1 h2
                                    :optional (value=? equal?)
                                              (fallback (undefined)))
  (define unique (cons #f #f))
  (define (fail)
    (if (undefined? fallback)
      (error "hash-tables can't be ordered:" h1 h2)
      fallback))
  ;; Returns #t iff smaller is a subset of larger.
  (define (subset? smaller larger)
    (hash-table-seek smaller
                     (^[k v] (let1 w (hash-table-get larger k unique)
                               (or (eq? unique w)
                                   (not (value=? v w)))))
                     (^[r k v] #f)
                     (^[] #t)))

  ;; Check comparator compatibility
  (let ([c1 (hash-table-comparator h1)]
        [c2 (hash-table-comparator h2)])
    (cond [(and c1 c2 (equal? c1 c2)) c1]
          [(or c1 c2) (error "hash-tables with different comparators can't be \
                              compared:" h1 h2)]
          [else (error "hash-tables don't have comparators and can't be \
                        compared:" h1 h2)]))  
  ;; Let's start
  (if (eq? h1 h2)
    0                 ;fast path
    (let ([n1 (hash-table-num-entries h1)]
          [n2 (hash-table-num-entries h2)])
      (cond
       [(= n1 n2) (if (subset? h1 h2) 0 (fail))]
       [(< n1 n2) (if (subset? h1 h2) -1 (fail))]
       [else      (if (subset? h2 h1) 1 (fail))]))))

(define (hash-table=? value-cmpr h1 h2)
  (or (eq? h1 h2)
      (and (= (hash-table-num-entries h1) (hash-table-num-entries h2))
           (zero? (hash-table-compare-as-sets h1 h2
                                              (cut =? value-cmpr <> <>)
                                              100))))) ;; any non-zero number
