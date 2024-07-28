;;;
;;; libgdict.scm - generic dictionary base API
;;;
;;;   Copyright (c) 2000-2024  Shiro Kawai  <shiro@acm.org>
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

(declare) ;; a dummy form to suppress generation of "sci" file

;; NOTE: This must be initialized after macro and object systems are booted.
;;
;; The module gauche.libdict is not meant to be used directly; the user
;; program needs to use gauche.dictionary.
;;
;; We define basic generic functions for dictionary API, and attach
;; methods specialized to hashtable and treemap here.  Built-in writer
;; refers to them.
;;
;;
;;

(define-module gauche.libdict
  (export define-dict-interface
          dict-get dict-put! |setter of dict-get|
          dict-immutable? dict-transparent? dict-exists?
          dict-delete!
          dict-seek dict-find dict-any
          dict-fold dict-fold-right
          dict-for-each dict-map
          dict-keys dict-values dict-comparator
          dict->alist dict-pop! dict-push! dict-update! dict-clear!
          ))
(select-module gauche.libdict)

;; A convenient macro to define dictionary methods.

(define-macro (define-dict-interface class . clauses)
  (define (gen-def kind specific)
    (let ([dict (gensym)]
          [key (gensym)]
          [val (gensym)]
          [default (gensym)]
          [proc (gensym)]
          [seed (gensym)]
          [succ (gensym)]
          [fail (gensym)])
      (case kind
        [(:get)
         `(define-method dict-get ((,dict ,class) ,key . ,default)
            (if (null? ,default)
              (,specific ,dict ,key)
              (,specific ,dict ,key (car ,default))))]
        [(:put!)
         `(define-method dict-put! ((,dict ,class) ,key ,val)
            (,specific ,dict ,key ,val))]
        [(:exists?)
         `(define-method dict-exists? ((,dict ,class) ,key)
            (,specific ,dict ,key))]
        [(:immutable?)
         `(define-method dict-immutable? ((,dict ,class)) (,specific ,dict))]
        [(:transparent?)
         `(define-method dict-transparent? ((,dict ,class)) (,specific ,dict))]
        [(:delete!)
         `(define-method dict-delete! ((,dict ,class) ,key)
            (,specific ,dict ,key))]
        [(:clear!)
         `(define-method dict-clear! ((,dict ,class))
            (,specific ,dict))]
        [(:fold)
         `(define-method dict-fold ((,dict ,class) ,proc ,seed)
            (,specific ,dict ,proc ,seed))]
        [(:fold-right)
         `(define-method dict-fold-right ((,dict ,class) ,proc ,seed)
            (,specific ,dict ,proc ,seed))]
        [(:map)
         `(define-method dict-map ((,dict ,class) ,proc)
            (,specific ,dict ,proc))]
        [(:for-each)
         `(define-method dict-for-each ((,dict ,class) ,proc)
            (,specific ,dict ,proc))]
        [(:keys)
         `(define-method dict-keys ((,dict ,class))
            (,specific ,dict))]
        [(:values)
         `(define-method dict-values ((,dict ,class))
            (,specific ,dict))]
        [(:push!)
         `(define-method dict-push! ((,dict ,class) ,key ,val)
            (,specific ,dict ,key ,val))]
        [(:pop!)
         `(define-method dict-pop! ((,dict ,class) ,key . ,default)
            (apply ,specific ,dict ,key ,default))]
        [(:update!)
         `(define-method dict-update! ((,dict ,class) ,key . ,default)
            (apply ,specific ,dict ,key ,default))]
        [(:->alist)
         `(define-method dict->alist ((,dict ,class))
            (,specific ,dict))]
        [(:comparator)
         `(define-method dict-comparator ((,dict ,class))
            (,specific ,dict))]
        [(:seek)
         `(define-method dict-seek ((,dict ,class) ,proc ,fail ,succ)
            (,specific ,dict ,proc ,fail ,succ))]
        [else (error "invalid kind in define-dict-interface:" kind)])))
  `(begin
     ,@(map (^p (gen-def (car p) (cadr p))) (slices clauses 2))))

;;-----------------------------------------------
;; Methods for hash-table, tree-map
;;

(define-dict-interface <hash-table>
  :get        hash-table-get
  :put!       hash-table-put!
  :delete!    hash-table-delete!
  :clear!     hash-table-clear!
  :exists?    hash-table-exists?
  :seek       hash-table-seek
  :fold       hash-table-fold
  :for-each   hash-table-for-each
  :map        hash-table-map
  :keys       hash-table-keys
  :values     hash-table-values
  :pop!       hash-table-pop!
  :push!      hash-table-push!
  :update!    hash-table-update!
  :->alist    hash-table->alist
  :comparator hash-table-comparator
  :transparent? (^_ #t))

(define-dict-interface <tree-map>
  :get        tree-map-get
  :put!       tree-map-put!
  :delete!    tree-map-delete!
  :clear!     tree-map-clear!
  :exists?    tree-map-exists?
  :seek       tree-map-seek
  :fold       tree-map-fold
  :fold-right tree-map-fold-right
  :for-each   tree-map-for-each
  :map        tree-map-map
  :keys       tree-map-keys
  :values     tree-map-values
  :pop!       tree-map-pop!
  :push!      tree-map-push!
  :update!    tree-map-update!
  :->alist    tree-map->alist
  :comparator tree-map-comparator
  :transparent? (^_ #t))
