;;;
;;; gauche.dictionary - dictionary generics
;;;  
;;;   Copyright (c) 2007  Shiro Kawai  <shiro@acm.org>
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
;;;  $Id: dictionary.scm,v 1.1 2007-03-16 12:14:53 shirok Exp $
;;;

;; EXPERIMENTAL

(define-module gauche.dictionary
  (use gauche.collection)
  (export dict-fold dict-fold-right
          dict-for-each dict-map dict-map-to
          dict-keys dict-values)
  )
(select-module gauche.dictionary)

;;-----------------------------------------------
;; dict-fold, dict-fold-right
;;

(define-method dict-fold ((dict <dictionary>) proc seed)
  (fold dict (lambda (kv seed) (proc (car kv) (cdr kv) seed)) seed))

(define-method dict-fold ((dict <hash-table>) proc seed)
  (hash-table-fold dict proc seed))

(define-method dict-fold ((dict <tree-map>) proc seed)
  (tree-map-fold dict proc seed))


(define-method dict-fold-right ((dict <ordered-dictionary>) proc seed)
  (fold-right dict (lambda (kv seed) (proc (car kv) (cdr kv) seed)) seed))

(define-method dict-fold-right ((dict <tree-map>) proc seed)
  (tree-map-fold-right dict proc seed))

;;-----------------------------------------------
;; dict-for-each, dict-map
;;

(define-method dict-for-each ((dict <dictionary>) proc)
  (dict-fold dict (lambda (k v _) (proc k v)) #f))

(define-method dict-for-each ((dict <hash-table>) proc)
  (hash-table-for-each dict proc))


(define-method dict-map ((dict <dictionary>) proc)
  (dict-fold dict (lambda (k v r) (cons (proc k v) r)) '()))

(define-method dict-map ((dict <ordered-dictionary>) proc)
  (dict-fold-right dict (lambda (k v r) (cons (proc k v) r)) '()))

;;-----------------------------------------------
;; dict-keys, dict-values
;;

(define-method dict-keys ((dict <dictionary>))
  (dict-fold dict (lambda (k v r) (cons k r)) '()))

(define-method dict-keys ((dict <ordered-dictionary>))
  (dict-fold-right dict (lambda (k v r) (cons k r)) '()))

(define-method dict-keys ((dict <hash-table>))
  (hash-table-keys dict))

(define-method dict-keys ((dict <tree-map>))
  (tree-map-keys dict))

(define-method dict-values ((dict <dictionary>))
  (dict-fold dict (lambda (k v r) (cons v r)) '()))

(define-method dict-values ((dict <ordered-dictionary>))
  (dict-fold-right dict (lambda (k v r) (cons v r)) '()))

(define-method dict-values ((dict <hash-table>))
  (hash-table-values dict))

(define-method dict-values ((dict <tree-map>))
  (tree-map-values dict))


(provide "gauche/dictionary")

