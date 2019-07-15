;;;
;;; gauche.dictionary - dictionary generics
;;;
;;;   Copyright (c) 2007-2019  Shiro Kawai  <shiro@acm.org>
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
          dict-immutable? dict-exists?
          dict-delete!
          dict-seek dict-find dict-any
          dict-fold dict-fold-right
          dict-for-each dict-map
          dict-keys dict-values dict-comparator
          dict->alist dict-pop! dict-push! dict-update! dict-clear!
          define-dict-interface

          <bimap> make-bimap bimap-put!
          bimap-left bimap-left-get bimap-left-exists? bimap-left-delete!
          bimap-right bimap-right-get bimap-right-exists? bimap-right-delete!

          <stacked-map> make-stacked-map
          stacked-map-push! stacked-map-pop! stacked-map-depth
          ))
(select-module gauche.dictionary)

;; Generic dictionary interface.
;; Required methods:
;;
;;    dict-get dict key [default]
;;    dict-put! dict key value
;;    dict-delete! dict key             ; for deletable dictionary
;;
;; Quasi-required methods (if not provided, returns #f.  It may hinder
;; the dict to be used in certain context.):
;;
;;    dict-comparator dict
;;
;; Optional methods (if not provided, the default method works, though
;; maybe inefficient.):
;;
;;    dict-immutable? dict-seek
;;    dict-fold dict proc seed
;;    dict-fold-right dict proc seed    ; for ordered dictionary
;;    dict-exists? dict key
;;    dict-map dict proc
;;    dict-for-each dict proc
;;    dict-keys dict
;;    dict-values dict
;;    dict-pop! dict key [default]
;;    dict-push! dict key value
;;    dict-update! dict key proc [default]
;;    dict->alist

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
         `(define-method dict-immutable ((,dict ,class)) (,specific ,dict))]
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
         `(define-method dict-seek ((,dict ,class) ,proc ,succ ,fail)
            (,specific ,dict ,proc ,succ ,fail))]
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
  :comparator hash-table-comparator)

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
  :comparator tree-map-comparator)

;;-----------------------------------------------
;; Fallback methods
;;

(define %unique (list #f))

;; Default is mutable.  Immutable dict should override.
(define-method dict-immutable? ((dict <dictionary>)) #f)

(define-method dict-exists? ((dict <dictionary>) key)
  (not (eq? (dict-get dict key %unique) %unique)))

(define-method dict-fold ((dict <dictionary>) proc seed)
  ;; This depends on the fact that a dictionary is also a collection.
  (fold (^[kv seed] (proc (car kv) (cdr kv) seed)) dict seed))

(define-method dict-fold-right ((dict <ordered-dictionary>) proc seed)
  (fold-right (^[kv seed] (proc (car kv) (cdr kv) seed)) dict seed))

(define-method dict-seek ((dict <dictionary>) pred succ fail)
  (let/cc return
    (dict-fold dict
               (^[k v _] (if-let1 r (pred k v)
                           (receive rs (succ r k v)
                             (apply return rs))
                           #f))
               #f)
    (fail)))

(define-method dict-find ((dict <dictionary>) pred
                          :optional (fail (^[] (values #f #f))))
  (dict-seek dict pred (^[r k v] (values k v)) fail))

(define-method dict-any ((dict <dictionary>) pred)
  (dict-seek dict pred (^[r k v] r) (^[] #f)))

(define-method dict-map ((dict <dictionary>) proc)
  (reverse (dict-fold dict (^[k v s] (cons (proc k v) s)) '())))

(define-method dict-for-each ((dict <dictionary>) proc)
  (dict-fold dict (^[k v s] (proc k v) #f) '()))

(define-method dict-keys ((dict <dictionary>))
  (dict-fold dict (^[k v s] (cons k s)) '()))

(define-method dict-keys ((dict <ordered-dictionary>))
  (reverse (dict-fold dict (^[k v s] (cons k s)) '())))

(define-method dict-values ((dict <dictionary>))
  (dict-fold dict (^[k v s] (cons v s)) '()))

(define-method dict-values ((dict <ordered-dictionary>))
  (reverse (dict-fold dict (^[k v r] (cons v r)) '())))

(define-method dict->alist ((dict <dictionary>))
  (dict-fold dict acons '()))

(define-method dict->alist ((dict <ordered-dictionary>))
  (reverse (dict-fold dict acons '())))

(define-method dict-delete! ((dict <dictionary>) key) ;fallback
  (error "You can't delete entry from a dictionary ~s" dict))

(define-method dict-clear! ((dict <dictionary>))
  (dolist [key (dict-keys dict)]
    (dict-delete! dict key)))

(define-method dict-pop! ((dict <dictionary>) key . maybe-default)
  (let1 r (dict-get dict key %unique)
    (cond [(eq? r %unique)
           (if (pair? maybe-default)
             (car maybe-default)
             (errorf "dict-pop!: no value for key ~s in ~s" key dict))]
          [(pair? r)
           (dict-put! dict key (cdr r))
           (car r)]
          [else
           (errorf "dict-pop!: value for key ~s is not a pair: ~s" key r)])))

(define-method dict-push! ((dict <dictionary>) key value)
  (dict-put! dict key (cons value (dict-get dict key '()))))

(define-method dict-update! ((dict <dictionary>) key proc . maybe-default)
  (let1 r (apply dict-get dict key maybe-default)
    (dict-put! dict key (proc r))))

(define-method (setter dict-get) (dict key val)
  (dict-put! dict key val))

(define-method dict-comparator ((dict <dictionary>)) #f)

;;;
;;; Bidirectional map
;;;

(define-class <bimap-meta> (<class>) ())

;; Currently we only support strict one-to-one mapping.
(define-class <bimap> (<dictionary>)
  ((left  :init-keyword :left)    ; x -> y
   (right :init-keyword :right)   ; y -> x
   (on-conflict :init-keyword :on-conflict
                :init-value :supersede)
   )
  :metaclass <bimap-meta>)

(define (make-bimap left right :key (on-conflict :supersede))
  (unless (memv on-conflict '(#f :error :supersede))
    (error "got invalid on-conflict value; possible values are "
           ":supersede, :error or #f, but got" on-conflict))
  (make <bimap> :left left :right right :on-conflict on-conflict))

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

(define (bimap-put! bm x y :key (on-conflict (~ bm 'on-conflict)))
  (let ([x-exists? (dict-exists? (bimap-left bm) x)]
        [y-exists? (dict-exists? (bimap-right bm) y)])
    (if (or x-exists? y-exists?)
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
              :supersede, :error or #f, but got:" on-conflict)])
      (begin
        (dict-put! (bimap-left bm) x y)
        (dict-put! (bimap-right bm) y x)))))

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
(define-method dict-comparator ((dict <bimap>))
  (dict-comparator (bimap-left dict)))

;; Collection protocol.   We just redirect methods to left map.
(define-method call-with-iterator ((coll <bimap>) proc . args)
  (apply call-with-iterator (bimap-left coll) proc args))

;;;
;;; Stacked map
;;;

;; Stacked map allows you to stack (layer) multiple maps and treat
;; as if they are a single map.  The top map "shadows" the bottom maps.

(define-class <stacked-map-meta> (<class>) ())

;; Unique-dict-maker is a thunk that creates a new dictionary
;; to be used to check key uniqueness.  When a stacked map is
;; traversed (e.g. by dict-fold), a new unique-dict is created
;; and keys are stored as the traversal progresses, and used to
;; filter out already-seen keys.  You can give it #f to turn off
;; unique-key check.

(define-class <stacked-map> (<dictionary>)
  ((stack     :init-keyword :stack :init-value '())
   (unique-dict-maker :init-keyword :unique-dict-maker
                      :init-value (cut make-hash-table 'eqv?)))
  :metaclass <stacked-map-meta>)

(define (make-stacked-map . maps)
  (make <stacked-map> :stack maps))

;; TODO: Fix ugly name.
(define (make-stacked-map-with-unique-dict-maker thunk . maps)
  (make <stacked-map> :stack maps :unique-dict-maker thunk))

(define-method stacked-map-push! ((smap <stacked-map>) (dict <dictionary>))
  (slot-push! smap 'stack dict))
(define-method stacked-map-pop! ((smap <stacked-map>))
  (slot-pop! smap 'stack))
(define-method stacked-map-depth ((smap <stacked-map>))
  (length (slot-ref smap 'stack)))

(define-method dict-exists? ((smap <stacked-map>) key)
  (any (cut dict-exists? <> key) (slot-ref smap 'stack)))

(define-method dict-get ((smap <stacked-map>) key . maybe-default)
  (let1 maybe-r (any (^m (let1 r (dict-get m key %unique)
                           (and (not (eq? r %unique)) (list r))))
                     (slot-ref smap 'stack))
    (cond [maybe-r (car maybe-r)]
          [(pair? maybe-default) (car maybe-default)]
          [else (errorf "~s doesn't have an entry for key ~s" smap key)])))

;; This puts entry to the topmost dict.
(define-method dict-put! ((smap <stacked-map>) key val)
  (let1 maps (slot-ref smap 'stack)
    (if (null? maps)
      (error "You can't add an entry to an empty stacked map:" smap)
      (dict-put! (car maps) key val))))

;; This deletes entry from *all* dicts.
(define-method dict-delete! ((smap <stacked-map>) key)
  (for-each (cut dict-delete! <> key) (slot-ref smap 'stack)))

(define-method dict-fold ((smap <stacked-map>) proc seed)
  (let1 udict (and-let* ([thunk (slot-ref smap 'unique-dict-maker)])
                (thunk))
    (if udict
      (fold (^[m s]
              (dict-fold m (^[k v s] (if (dict-exists? udict k)
                                       s
                                       (begin (dict-put! udict k #t)
                                              (proc k v s))))
                         s))
            seed (slot-ref smap 'stack))
      (fold (^[m s] (dict-fold m proc s)) seed (slot-ref smap 'stack)))))
