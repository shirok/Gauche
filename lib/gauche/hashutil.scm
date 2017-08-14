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
  (export hash-table hash-table-empty?
          hash-table-for-each hash-table-map hash-table-fold
          hash-table-seek hash-table-find hash-table-compare-as-sets
          boolean-hash char-hash char-ci-hash string-hash string-ci-hash
          symbol-hash number-hash default-hash
          hash-bound hash-salt))
(select-module gauche.hashutil)

;; TRANSIENT: Precompiling with 0.9.5 doesn't load assume-type yet.
;; Remove this after 0.9.6 release.
(unless (global-variable-bound? 'gauche 'assume-type)
  (eval '(define-syntax assume-type
          (syntax-rules ()
            [(_ x type) (check-arg (cut is-a? <> type) x)]))
        (current-module)))

(define (hash-table cmpr . kvs)
  (rlet1 h (make-hash-table cmpr)
    (for-each (^[kv] (hash-table-put! h (car kv) (cdr kv))) kvs)))

(define (hash-table-empty? h) (zero? (hash-table-num-entries h)))

(define (hash-table-map hash proc)
  (assume-type hash <hash-table>)
  (let ([eof (cons #f #f)]              ;marker
        [i   ((with-module gauche.internal %hash-table-iter) hash)])
    (let loop ([r '()])
      (receive [k v] (i eof)
        (if (eq? k eof)
          r
          (loop (cons (proc k v) r)))))))

(define (hash-table-for-each hash proc)
  (assume-type hash <hash-table>)
  (let ([eof (cons #f #f)]              ;marker
        [i ((with-module gauche.internal %hash-table-iter) hash)])
    (let loop ()
      (receive [k v] (i eof)
        (unless (eq? k eof)
          (proc k v) (loop))))))

(define (hash-table-fold hash kons knil)
  (assume-type hash <hash-table>)
  (let ([eof (cons #f #f)]              ;marker
        [i ((with-module gauche.internal %hash-table-iter) hash)])
    (let loop ([r knil])
      (receive [k v] (i eof)
        (if (eq? k eof)
          r
          (loop (kons k v r)))))))

(define (hash-table-seek hash pred succ fail)
  (assume-type hash <hash-table>)
  (let ([eof (cons #f #f)]              ;marker
        [i ((with-module gauche.internal %hash-table-iter) hash)])
    (let loop ()
      (receive [k v] (i eof)
        (cond [(eq? k eof) (fail)]
              [(pred k v) => (^r (succ r k v))]
              [else (loop)])))))
  
;; srfi-125.  this doesn't align with other '*-find' API in a way that
;; it returns the result of PRED.
(define (hash-table-find hash pred :optional (failure (^[] #f)))
  (hash-table-seek hash pred (^[r k v] r) failure))

;; We delegate most hash calculation to the built-in default-hash.
;; These type-specific hash functions are mostly
;;  for the compatibility of srfi-128.

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
;; Lots of dupes from tree-map-compare-as-sets, and probably we should
;;  
(define (hash-table-compare-as-sets h1 h2
                                    :optional (value=? equal?)
                                    (fallback (undefined)))
  (define eof (cons #f #f))
  (define (fail)
    (if (undefined? fallback)
      (error "hash-tables can't be ordered:" h1 h2)
      fallback))
  (define key-cmp
    (let ([c1 (tree-map-comparator h1)]
          [c2 (tree-map-comparator h2)])
      (cond [(and c1 c2 (equal? c1 c2)) c1]
            [(or c1 c2) (error "hash-tables with different comparators can't be \
                                compared:" h1 h2)]
            [else (error "hash-tables don't have comparators and can't be \
                          compared:" h1 h2)])))
  (if (eq? h1 h2)
    0                                   ;fast path
    (let ([i1 ((with-module gauche.internal %hash-table-iter) h1)]
          [i2 ((with-module gauche.internal %hash-table-iter) h2)])
      (define (loop k1 v1 k2 v2 r)
        (if (eq? k1 eof)
          (if (eq? k2 eof)
            r
            (if (<= r 0) -1 (fail)))
          (if (eq? k2 eof)
            (if (>= r 0) 1 (fail))
            (case (comparator-compare key-cmp k1 k2)
              [(0)  (if (value=? v1 v2)
                      (receive (k1 v1) (i1 eof)
                        (receive (k2 v2) (i2 eof)
                          (loop k1 v1 k2 v2 r)))
                      (fail))]
              [(-1) (if (>= r 0)
                      (receive (k1 v1) (i1 eof)
                        (loop k1 v1 k2 v2 1))
                      (fail))]
              [else (if (<= r 0)
                      (receive (k2 v2) (i2 eof)
                        (loop k1 v1 k2 v2 -1))
                      (fail))]))))
      (receive (k1 v1) (i1 eof)
        (receive (k2 v2) (i2 eof)
          (loop k1 v1 k2 v2 0))))))

