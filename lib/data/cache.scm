;;;
;;;  data.cache - Implementation of various cache strategies
;;;
;;;   Copyright (c) 2015  Shiro Kawai  <shiro@acm.org>
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

;; This class takes care of various cache algorithm.
;; The cache works like a dictionary that you can associate value
;; to a key.  The entry may disappear silently, when it is expired
;; according to the cache policy.  This module is inspired by Clojure's
;; core.cache, but note that the API spec is different from Clojure's.
;;
;; User-side API
;;
;;  (make-***-cache [args ...] :key storage comparator ...)   procedure
;;     Arguments varies depending on the alrogithm.
;;
;;  (cache-lookup! cache key [default]) => value              procedure
;;  (cache-through! cache key value) => value                 procedure
;;  (cache-evict! cache key) => void                             method*
;;  (cache-clear! cache) => void                                 method*
;;  (cache-write! cache key value) => void                       method
;;
;;  (cache-storage cache) => dictionary                       procedure
;;  (cache-compartor cache) => comparator                     procedure
;;
;; Implementor-side API
;;
;;  An implementation of cache algorithm must provide the methods marked
;;  by '*' above.  Providing other methods are optional.
;;
;;  Besides that, these two methods are essential:
;;
;;  (cache-check! cache key) => Maybe (key . value)
;;     Check if key has a value in cache.   If not, returns #f.
;;     If it does, returns a cons of the key and the value.
;;
;;  (cache-register! cache key value) => (key . value)
;;     Called when cache doesn't have an entry for the key, in order
;;     to insert the value with the key.
;;     This inserts the key-value pair into the cache, and returns
;;     cons of them.
;;
;;  The initialize method must take care of set up the internal state.
;;  It should also handle the case when the existing dictionary is given.
;;    

(define-module data.cache
  (use gauche.collection)
  (use gauche.dictionary)
  (use data.queue)
  (use data.heap)
  (use srfi-114)
  (export <cache>
          cache-storage cache-comparator
          cache-lookup! cache-through!
          cache-check! cache-register! cache-write!
          cache-evict! cache-clear!

          ;; Concrete implementations
          make-basic-cache
          make-fifo-cache
          make-ttl-cache
          make-ttlr-cache))
(select-module data.cache)

;; storage and comparator 

(define-class <cache> ()
  (;; private.  must be treated read-only.
   [storage :init-keyword :storage :init-value #f]
   [comparator :init-keyword :comparator :init-value #f]
   ))

(define-inline (cache-storage cache)    (slot-ref cache 'storage))
(define-inline (cache-comparator cache) (slot-ref cache 'comparator))

(define-method initialize ((c <cache>) initargs)
  (next-method)
  (if (cache-storage c)
    (unless (cache-comparator c)
      (slot-set! c 'comparator (dict-comparator (cache-storage c))))
    (begin
      (unless (cache-comparator c)
        (slot-set! c 'comparator default-comparator))
      (slot-set! c 'storage (make-hash-table (cache-comparator c))))))

;;;
;;; Cache external API
;;;

(define (cache-lookup! cache key . maybe-default)
  (if-let1 kv (cache-check! cache key)
    (cdr kv)
    (if (pair? maybe-default)
      (car maybe-default)
      (errorf "cache ~s doesn't have an entry for key ~s" cache key))))

(define (cache-through! cache key value-fn)
  (cdr (or (cache-check! cache key)
           (cache-register! cache key (value-fn key)))))

(define-method cache-write! ((cache <cache>) key value)
  (cache-evict! cache key)
  (cache-register! cache key value))

;;;
;;; Cache implementations
;;;

;; Basic Cache

(define-class <basic-cache> (<cache>)
  ())

(define (make-basic-cache :key (storage #f) (comparator #f))
  (make <basic-cache> :storage storage :comparator comparator))

(define *none* (list #f))

(define-method cache-check! ((cache <basic-cache>) key)
  (let1 v (dict-get (cache-storage cache) key *none*)
    (and (not (eq? v *none*))
         (cons key v))))
      
(define-method cache-register! ((cache <basic-cache>) key value)
  (dict-put! (cache-storage cache) key value)
  (cons key value))

(define-method cache-write! ((cache <basic-cache>) key value)
  (dict-put! (cache-storage cache) key value))

(define-method cache-evict! ((cache <basic-cache>) key)
  (dict-delete! (cache-storage cache) key))

(define-method cache-clear! ((cache <basic-cache>))
  (dict-clear! (cache-storage cache)))

;; FIFO Cache
;;  - To recover the order from dict, we keep (<n> . <value>) in the dict.
;;    <n> being increasing order of nonnegative integers.
;;  - A queue holds (<key> . <n>)
;;  - We don't want <n> to become bignums in long-running process, so
;;    when we see <n> gets too big, we renumber entries.

(define-class <fifo-cache> (<cache>)
  ([capacity :init-keyword :capacity]
   ;; private
   [queue :init-form (make-queue)]
   [counter :init-value 0]))

(define (make-fifo-cache capacity :key (storage #f) (comparator #f))
  (make <fifo-cache> :storage storage :comparator comparator
        :capacity capacity))

(define-method initialize ((c <fifo-cache>) initargs)
  (next-method)
  (let1 entries (dict->alist (cache-storage c))  ; entries :: [(key n . val)]
    (unless (null? entries)
      (let ([queue (~ c'queue)]
            [maxn 0])
        (dolist [entry (sort entries < cadr)]
          (enqueue! queue (cons (car entry) (cadr entry))) ; (key . n)
          (when (> (cadr entry) maxn) (set! maxn (cadr entry))))
        (set! (~ c'counter) (+ maxn 1))))))

(define-method cache-check! ((cache <fifo-cache>) key)
  (and-let1 nv (dict-get (cache-storage cache) key #f)
    (cons key (cdr nv))))

(define (%fifo-add! cache key value)
  (let ([dict (cache-storage cache)]
        [queue (~ cache'queue)])
    (when (>= (size-of dict) (~ cache'capacity))
      ;; NB: The queue may have multiple entries for the key.  We only
      ;; concern the newest entry, so we loop if the queue head is old.
      ;; The queue should never be empty during this loop, because size of
      ;; queue >= size of dict.
      (let loop ([kn (dequeue! queue)])
        (let1 nv (dict-get dict (car kn) #f)
          (if (and (pair? nv) (= (cdr kn) (car nv)))
            (dict-delete! dict (car kn))
            (loop (dequeue! queue))))))
    (let1 n (~ cache'counter)
      (dict-put! dict key (cons n value))
      (enqueue! queue (cons key n))
      (if (= n (greatest-fixnum))
        (%fifo-renumber cache)
        (set! (~ cache'counter) (+ n 1))))))

(define (%fifo-renumber cache)
  (let ([dict (cache-storage cache)]
        [queue (~ cache'queue)]
        [seen (make-hash-table (~ cache'comparator))])
    (define cnt (- (size-of dict) 1))
    (dolist [kn (sort (dequeue-all! queue) > cdr)]
      (unless (hash-table-exists? seen (car kn))
        (hash-table-put! seen (car kn) #t)
        (queue-push! queue (cons (car kn) cnt))
        (dict-update! dict (car kn) (^[nv] (cons cnt (cdr nv))))
        (dec! cnt)))
    (set! (~ cache'counter) (size-of dict))))

(define-method cache-register! ((cache <fifo-cache>) key value)
  (%fifo-add! cache key value))

(define-method cache-write! ((cache <fifo-cache>) key value)
  (%fifo-add! cache key value))

(define-method cache-evict! ((cache <fifo-cache>) key)
  ;; We leave queue entry; they'll be cleared eventually.
  (dict-delete! (cache-storage cache) key))

(define-method cache-clear! ((cache <fifo-cache>))
  (dict-clear! (cache-storage cache))
  (dequeue-all! (~ cache'queue))
  (undefined))
  

;; TTL Cache
;;  - Timestamps is a heap with (<timestamp> . <key>).   There can
;;    be multiple entries with the same <key>.
;;  - The dictionary holds (<timestamp> . <value>).

(define-class <ttl-cache> (<cache>)
  ([ttl :init-keyword :ttl] ; time to live, in seconds
   [timestamper :init-keyword :timestamper] ; a thunk to return a timestamp
   [timestamps] ; heap of (timestamp . key), sorted by timestamp.
   ))

(define (make-ttl-cache ttl :key (storage #f) (comparator #f)
                                 (timestamper sys-time))
  (make <ttl-cache> :storage storage :comparator comparator
        :ttl ttl :timestamper timestamper))

(define-method initialize ((c <ttl-cache>) initargs)
  (next-method)
  (let1 heap (make-binary-heap :comparator number-comparator :key car)
    (set! (~ c'timestamps) heap)
    ;; Set up for prefilled dict
    (dict-for-each (cache-storage c)
                   (^[k ts] (binary-heap-push! heap (cons (car ts) k)))))
  )

(define (%ttl-sweep! c)
  (let ([cutoff (- ((~ c'timestamper)) (~ c'ttl))]
        [ts  (~ c'timestamps)]
        [tab (cache-storage c)])
    (let loop ()
      (when (> (binary-heap-num-entries ts) 0)
         (let1 p (binary-heap-find-min ts)
           (when (< (car p) cutoff)
             (binary-heap-pop-min! ts)
             ;; The dict-get below may return #f, if there's more than one
             ;; entry of the key in the heap with the same timestamp.
             (and-let* ([tv (dict-get tab (cdr p) #f)]
                        [ (<= (car tv) (car p)) ])
               (dict-delete! tab (cdr p)))
             (loop)))))))

(define (%ttl-timestamp c) ((~ c'timestamper)))

(define-method cache-check! ((c <ttl-cache>) key)
  (%ttl-sweep! c)
  (and-let1 tv (dict-get (cache-storage c) key #f)
    (cons key (cdr tv))))

(define-method cache-register! ((c <ttl-cache>) key val)
  (let1 t (%ttl-timestamp c)
    (dict-put! (cache-storage c) key (cons t val))
    (binary-heap-push! (~ c'timestamps) (cons t key)))
  (cons key val))

(define-method cache-evict! ((c <ttl-cache>) key)
  (dict-delete! (cache-storage c) key)
  (let1 cmp (~ c'comparator)
    (binary-heap-remove! (~ c'timestamps)
                         (^e (comparator-equal? cmp (cdr e) key)))))

(define-method cache-clear! ((c <ttl-cache>))
  (dict-clear! (cache-storage c))
  (binary-heap-clear! (~ c'timestamps)))

(define-method cache-write! ((c <ttl-cache>) key val)
  (if-let1 tv (dict-get (cache-storage c) key #f)
    (let1 t (%ttl-timestamp c)
      (set! (car tv) t)
      (set! (cdr tv) val)
      (binary-heap-push! (~ c'timestamps) (cons t key)))
    (cache-register! c key val))
  (undefined))

;; TTLR Cache
;; - TTL with refreshing.  The timestamp of the entry is updated every
;;   time it is read.  It's kind of combination of LRU + TTL.
;; - Most methods are inherited from TTL cache.  The only difference
;;   is cache-check!, in which we 'touch' the entry.
(define-class <ttlr-cache> (<ttl-cache>)
  ())
  
(define (make-ttlr-cache ttl :key (storage #f) (comparator #f)
                                  (timestamper sys-time))
  (make <ttlr-cache> :storage storage :comparator comparator
        :ttl ttl :timestamper timestamper))

(define-method cache-check! ((c <ttlr-cache>) key)
  (%ttl-sweep! c)
  (and-let* ([tv (dict-get (cache-storage c) key #f)]
             [t  (%ttl-timestamp c)])
    (set! (car tv) t)
    (binary-heap-push! (~ c'timestamps) (cons t key))
    (cons key (cdr tv))))
