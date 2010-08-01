;;;
;;; gauche.dictionary - dictionary generics
;;;  
;;;   Copyright (c) 2007-2010  Shiro Kawai  <shiro@acm.org>
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

(define-module gauche.dictionary
  (use gauche.collection)
  (export dict-get dict-put! |setter of dict-get|
          dict-exists? dict-delete!
          dict-fold dict-fold-right
          dict-for-each dict-map
          dict-keys dict-values
          <bimap> make-bimap bimap-put!
          bimap-left bimap-left-get bimap-left-exists? bimap-left-delete!
          bimap-right bimap-right-get bimap-right-exists? bimap-right-delete!
          ))
(select-module gauche.dictionary)

;;; Generic dictionary interface.
;;; The minimal requirements for dictionary framework implementors:
;;;
;;;    dict-get dict key [default]
;;;    dict-put! dict key value
;;;    dict-exists? dict key
;;;    dict-delete! dict key


;;-----------------------------------------------
;; Basic accessors
;;

;; NB: avoid using apply for performance
(define-method dict-get ((dict <hash-table>) key . maybe-default)
  (if (null? maybe-default)
    (hash-table-get dict key)
    (hash-table-get dict key (car maybe-default))))
(define-method dict-get ((dict <tree-map>) key . maybe-default)
  (if (null? maybe-default)
    (tree-map-get dict key)
    (tree-map-get dict key (car maybe-default))))

(define-method dict-put! ((dict <hash-table>) key val)
  (hash-table-put! dict key val))
(define-method dict-put! ((dict <tree-map>) key val)
  (tree-map-put! dict key val))

(define-method (setter dict-get) (dict key val)
  (dict-put! dict key val))

(define-method dict-delete! ((dict <hash-table>) key)
  (hash-table-delete! dict key))
(define-method dict-delete! ((dict <tree-map>) key)
  (tree-map-delete! dict key))

(define-method dict-exists? ((dict <hash-table>) key)
  (hash-table-exists? dict key))
(define-method dict-exists? ((dict <tree-map>) key)
  (tree-map-exists? dict key))

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

;;;
;;; Bidirectional map
;;;

(define-class <bimap-meta> (<class>) ())

;; Currently we only support strict one-to-one mapping.
(define-class <bimap> (<dictionary>)
  ((left  :init-keyword :left)    ; x -> y
   (right :init-keyword :right)   ; y -> x
   )
  :metaclass <bimap-meta>)

(define (make-bimap left right)
  (make <bimap> :left left :right right))

(define (bimap-left bm)  (slot-ref bm 'left))
(define (bimap-right bm) (slot-ref bm 'right))

(define-macro (define-bimap-ops lr fwd rev)
  (define (N templ) (string->symbol (format templ lr)))
  `(begin
     (define (,(N "bimap-~a-get") bm key . maybe-default)
       (apply dict-get (,fwd bm) key maybe-default))
     (define (,(N "bimap-~a-exists?") bm key)
       (dict-exists? (,fwd bm) key))
     (define (,(N "bimap-~a-delete!") bm key)
       (let ([f (,fwd bm)]
             [r (,rev bm)])
         (and (dict-exists? f key)
              (let1 val (dict-get f key)
                (dict-delete! f key)
                (dict-delete! r val)))))
     ))

(define-bimap-ops left  bimap-left bimap-right)
(define-bimap-ops right bimap-right bimap-left)

(define (bimap-put! bm x y :key (on-conflict :supersede))
  (let ([x-exists? (dict-exists? (bimap-left bm) x)]
        [y-exists? (dict-exists? (bimap-right bm) y)])
    (case on-conflict
      [(:error)
       (if x-exists?
         (error "attempt to insert duplicate left-key into bimap: " x)
         (error "attempt to insert duplicate right-key into bimap: " y))]
      [(#f) #f]
      [(:supersede)
       (when x-exists?
         (dict-delete! (bimap-right bm) (dict-get (bimap-left bm) x)))
       (when y-exists?
         (dict-delete! (bimap-left bm) (dict-get (bimap-right bm) y)))
       (dict-put! (bimap-left bm) x y)
       (dict-put! (bimap-right bm) y x)
       #t]
      [else
       (error "bimap-put!: on-conflict argument must be either one of \
              :supersede, :error or #f, but got:" on-conflict)])))

;; the normal ref/set! uses left map
(define-method dict-get ((dict <bimap>) key . maybe-default)
  (apply bimap-left-get dict key maybe-default))
(define-method dict-put! ((dict <bimap>) key val)
  (bimap-put! dict key val))
(define-method dict-exists? ((dict <bimap>) key)
  (bimap-left-exists? dict key))
(define-method dict-delete! ((dict <bimap>) key)
  (bimap-left-delete! dict key))
(define-method dict-fold ((dict <bimap>) proc seed)
  (dict-fold (bimap-left dict) proc seed))

;; Collection protocol.   We just redirect methods to left map.
(define-method call-with-iterator ((coll <bimap>) proc . args)
  (apply call-with-iterator (bimap-left coll) proc args))

