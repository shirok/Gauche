;;;
;;; auxiliary hashtable utilities.  to be autoloaded.
;;;
;;;   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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
  (export hash-table hash-table-for-each hash-table-map hash-table-fold
          boolean-hash char-hash char-ci-hash string-hash string-ci-hash
          symbol-hash number-hash default-hash
          hash-bound hash-salt))
(select-module gauche.hashutil)

(define (hash-table cmpr . kvs)
  (rlet1 h (make-hash-table cmpr)
    (for-each (^[kv] (hash-table-put! h (car kv) (cdr kv))) kvs)))

(define (hash-table-map hash proc)
  (check-arg hash-table? hash)
  (let ([eof (cons #f #f)]              ;marker
        [i   (%hash-table-iter hash)])
    (let loop ([r '()])
      (receive [k v] (i eof)
        (if (eq? k eof)
          r
          (loop (cons (proc k v) r)))))))

(define (hash-table-for-each hash proc)
  (check-arg hash-table? hash)
  (let ([eof (cons #f #f)]              ;marker
        [i (%hash-table-iter hash)])
    (let loop ()
      (receive [k v] (i eof)
        (unless (eq? k eof)
          (proc k v) (loop))))))

(define (hash-table-fold hash kons knil)
  (check-arg hash-table? hash)
  (let ([eof (cons #f #f)]              ;marker
        [i (%hash-table-iter hash)])
    (let loop ([r knil])
      (receive [k v] (i eof)
        (if (eq? k eof)
          r
          (loop (kons k v r)))))))

;; We delegate most hash calculation to the built-in 'hash'.  These
;; functions are mostly for the compatibility of srfi-128.

(define (boolean-hash obj)
  (check-arg boolean? obj)
  (hash obj))

(define (char-hash obj)
  (check-arg char? obj)
  (hash obj))

(define (char-ci-hash obj)
  (check-arg char? obj)
  (hash (char-foldcase obj)))

(define (string-hash obj)
  ((with-module gauche.internal %hash-string) obj))

(autoload gauche.unicode string-foldcase)

(define (string-ci-hash obj)
  (check-arg string? obj)
  ((with-module gauche.internal %hash-string) (string-foldcase obj)))

(define (symbol-hash obj)
  (check-arg symbol? obj)
  (hash obj))

(define (number-hash obj)
  (check-arg number? obj)
  (hash obj))

(define default-hash hash)

;; These are placeholder to conform srfi-128.  We'll change the
;; internals to honor hash-salt eventually.
(define (hash-bound) (greatest-fixnum))
(define (hash-salt)  0)
