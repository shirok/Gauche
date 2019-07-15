;;;
;;; srfi-125 - intermediate hash tables
;;;
;;;   Copyright (c) 2017-2019  Shiro Kawai  <shiro@acm.org>
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

;; Most of procedures are built-in, though some of them have different names
;; to avoid conflict with traditional Gauche procedures.

(define-module srfi-125
  (use gauche.hashutil)
  (export make-hash-table                   ; extended for compatibility
          hash-table                        ; extended for compatibility
          hash-table-unfold                 ; builtin
          alist->hash-table                 ; extended for compatibility
          
          hash-table?                   ; builtin
          hash-table-contains?          ; builtin
          hash-table-exists?            ; builtin
          hash-table-empty?             ; builtin
          hash-table=?                  ; builtin
          hash-table-mutable?           ; builtin
          
          hash-table-ref                ; builtin
          hash-table-ref/default        ; builtin
          
          hash-table-set!               ; builtin
          (rename hash-table-delete!-r7 hash-table-delete!) ; builtin
          (rename hash-table-intern!-r7 hash-table-intern!) ; builtin
          (rename hash-table-update!-r7 hash-table-update!) ; builtin
          hash-table-update!/default                        ; builtin
          (rename hash-table-pop!-r7 hash-table-pop!)       ; builtin
          hash-table-clear!                                 ; builtin
          
          hash-table-size               ; builtin
          hash-table-keys               ; builtin
          hash-table-values             ; builtin
          hash-table-entries            ; builtin
          (rename hash-table-find-r7 hash-table-find) ; builtin
          (rename hash-table-count-r7 hash-table-count) ; builtin
          
          (rename hash-table-map-r7 hash-table-map) ; builtin
          (rename hash-table-for-each-r7 hash-table-for-each) ; builtin
          hash-table-walk               ; compatibility
          (rename hash-table-map!-r7 hash-table-map!) ; builtin
          (rename hash-table-map->list-r7 hash-table-map->list) ; builtin
          hash-table-fold               ; extended for compatibility
          (rename hash-table-prune!-r7 hash-table-prune!) ; builtin
          
          hash-table-copy               ; builtin
          hash-table-empty-copy         ; builtin
          hash-table->alist             ; builtin
          
          hash-table-union!             ; builtin
          hash-table-merge!             ; compatibility
          hash-table-intersection!      ; builtin
          hash-table-difference!        ; builtin
          hash-table-xor!               ; builtin
          
          hash                          ; compatibility
          string-hash                   ; compatibility
          string-ci-hash                ; compatibility
          hash-by-identity              ; compatibility
          hash-table-equivalence-function ; compatibility
          hash-table-hash-function      ; compatibility
          ))
(select-module srfi-125)

;; srfi-69 compatibility layer

(define %make-hash-table (with-module gauche make-hash-table))
(define %alist->hash-table (with-module gauche alist->hash-table))

(define (make-hash-table cmpr . args)
  (if (procedure? cmpr)                 ; srfi-69
    (if (and (pair? args) (procedure? (car args)))
      (apply %make-hash-table
             (make-comparator #t cmpr #f (car args)) (cdr args))
      (apply %make-hash-table
             (make-comparator #t cmpr #f default-hash) args))
    (apply %make-hash-table cmpr args)))

(define (hash-table cmpr . kvs)
  (cond [(comparator? cmpr) (apply hash-table-r7 cmpr kvs)]
        [(procedure? cmpr)
         (if (pair? kvs)
           (apply hash-table-r7 (make-comparator #t cmpr #f (cadr kvs))
                  (cdr kvs))
           (error "missing hash function in hash-table (srfi-69 compatibility):"
                  (list 'hash-table cmpr)))]
        [else (error "comparator or procedure expected, but got:" cmpr)]))

(define (alist->hash-table cmpr . pairs)
  (cond [(comparator? cmpr)
         (apply (with-module gauche alist->hash-table) cmpr pairs)]
        [(procedure? cmpr)
         (if (pair? pairs)
           (apply (with-module gauche alist->hash-table)
                  (make-comparator #t cmpr #f (cadr pairs))
                  (cdr pairs))
           (error "missing hash function in alist->hash-table (srfi-69 compatibility):"
                  (list 'alist->hash-table cmpr)))]
        [else (error "comparator or procedure expected, but got:" cmpr)]))

(define (hash-table-walk ht proc) (hash-table-for-each ht proc))

(define (hash-table-fold proc seed ht)
  (if (hash-table? proc)
    ((with-module gauche hash-table-fold) proc seed ht)
    (hash-table-fold-r7 proc seed ht)))

(define (hash-table-merge! ht1 ht2) (hash-table-union! ht1 ht2))

(define (hash obj :optional ignore)
  (warn "srfi-69 `hash' is deprecated.  Use `default-hash' instead.")
  (default-hash obj))
(define (string-hash obj :optional ignore)
  ((with-module gauche string-hash) obj))
(define (string-ci-hash obj :optional ignore)
  ((with-module gauche string-ci-hash) obj))
(define (hash-by-identity obj :optional ignore)
  (warn "srfi-69 `hash-by-identity' is deprecated and does not work compatible way with Gauche's eq-hash.")
  (default-hash obj))

(define (hash-table-equivalence-function ht)
  (assume-type ht <hash-table>)
  (comparator-equality-predicate (hash-table-comparator ht)))
(define (hash-table-hash-function ht)
  (assume-type ht <hash-table>)
  (comparator-hash-function (hash-table-comparator ht)))

  
