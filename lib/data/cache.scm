;;;
;;;  data.cache - Implementation of various cache strategies
;;;
;;;   Copyright (c) 2015-2019  Shiro Kawai  <shiro@acm.org>
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

;; This class takes care of various cache algorithms.
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
;;  (cache-through! cache key value-fn) => value              procedure
;;  (cache-evict! cache key) => void                             method
;;  (cache-clear! cache) => void                                 method
;;  (cache-write! cache key value) => void                       method
;;
;;  (cache-storage cache) => dictionary                       procedure
;;  (cache-comparator cache) => comparator                     procedure
;;
;; Implementor-side API
;;
;;  An implementation of cache algorithm must provide at least the
;;  following two methods:
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
;;  The default method of cache-evict! and cache-clear! only takes
;;  care of the storage.  If the algorithm needs to modify other internal
;;  states, it should override those methods.
;;
;;  The default method of cache-write! is evict! + register!.  In most
;;  cases there exists a better way.

(define-module data.cache
  (use gauche.collection)
  (use gauche.dictionary)
  (use data.queue)
  (use data.heap)
  (use srfi-114)
  (export <cache>
          ;; Protocol
          cache-storage cache-comparator
          cache-lookup! cache-through!
          cache-check! cache-register! cache-write!
          cache-evict! cache-clear!

          ;; Some auxiliary procedures for implementors
          cache-populate-queue! cache-compact-queue!
          cache-renumber-entries!

          ;; Concrete implementations
          make-fifo-cache
          make-ttl-cache
          make-ttlr-cache
          make-lru-cache
          make-counting-cache cache-stats))
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

(define-method cache-evict! ((cache <cache>) key)
  (dict-delete! (cache-storage cache) key))

(define-method cache-clear! ((cache <cache>))
  (dict-clear! (cache-storage cache)))

(define-method cache-write! ((cache <cache>) key value)
  (cache-evict! cache key)
  (cache-register! cache key value))

;; Some common routines
;;  Most of cache implementations uses a queue as an auxiliary struct.
;;  They keep <key> -> (<n> . <value>) in the storage, and
;;  pushd (<key> . <n>) into the queue, where <n> is a number and
;;  monotonically increasing within the queue.   There may be multiple
;;  entries with the same <key> in the queue.
;;  Although the meaning of <n> depends on the algorithm, some
;;  operations can be common.

;; Queue must be an empty queue.  Fills queue according to the contents of
;; storage.  This is called from initialize, in order to set up a cache
;; with existing storage.  Returns maximum <n>.
(define (cache-populate-queue! queue storage)
  (let1 entries (dict->alist storage)  ; entries :: [(key n . val)]
    (fold (^[entry maxn]
            (enqueue! queue (cons (car entry) (cadr entry))) ; (key . n)
            (if (> (cadr entry) maxn)
              (cadr entry)
              maxn))
          0
          (sort entries < cadr))))

;; When the algorithm increments <n> monotonically, it can eventually fell
;; to bignum, but we don't want that.  This routine renumbers entries,
;; while removing duplicate keys in the queue.
(define (cache-renumber-entries! queue storage)
  (let1 seen (make-hash-table (dict-comparator storage))
    (define cnt (- (size-of storage) 1))
    (dolist [kn (sort (dequeue-all! queue) > cdr)]
      (unless (hash-table-exists? seen (car kn))
        (hash-table-put! seen (car kn) #t)
        (queue-push! queue (cons (car kn) cnt))
        (dict-update! storage (car kn) (^[nv] (cons cnt (cdr nv))))
        (dec! cnt)))))

;; We allow duplicate keys in the queue, but we don't want the queue to
;; get too long (e.g. Repeatedly hitting the same key in LRU cache would
;; quickly pile up entries in the queue).  So occasionally we want to
;; compact the queue, by removing the duplicate entries.
;; This routine is quite similar to cache-renumber-entries! but we don't
;; change <n>'s, so we don't need to mutate the storage.
(define (cache-compact-queue! queue storage)
  (let1 seen (make-hash-table (dict-comparator storage))
    (dolist [kn (sort (dequeue-all! queue) > cdr)]
      (unless (hash-table-exists? seen (car kn))
        (hash-table-put! seen (car kn) #t)
        (queue-push! queue kn)))))

;;;
;;; Cache implementations
;;;

;; FIFO Cache
;;  - To recover the order from dict, we keep (<n> . <value>) in the storage.
;;    <n> being increasing order of nonnegative integers.
;;  - The queue holds (<key> . <n>)
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
  (set! (~ c'counter)
        (+ 1 (cache-populate-queue! (~ c'queue) (cache-storage c)))))

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
    (dict-put! dict key (cons (~ cache'counter) value))
    (%fifo-touch! cache queue key)))

(define (%fifo-touch! cache queue key)
  (let ([n (~ cache'counter)]
        [storage (cache-storage cache)])
    (enqueue! queue (cons key n))
    (cond [(= n (greatest-fixnum))
           (cache-renumber-entries! queue storage)
           (set! (~ cache'counter) (size-of storage))]
          [(> (queue-length queue) (* 3 (size-of storage)))
           (cache-compact-queue! queue storage)
           (set! (~ cache'counter) (+ n 1))]
          [else
           (set! (~ cache'counter) (+ n 1))])))

(define-method cache-register! ((cache <fifo-cache>) key value)
  (%fifo-add! cache key value)
  (cons key value))

(define-method cache-write! ((cache <fifo-cache>) key value)
  (%fifo-add! cache key value))

;; We don't provide cache-evict!.  Leaving stale entry in the queue
;; doesn't do harm.

(define-method cache-clear! ((cache <fifo-cache>))
  (dict-clear! (cache-storage cache))
  (dequeue-all! (~ cache'queue))
  (undefined))

;; LRU Cache
;;  - LRU cache is actually a variation of FIFO cache, except that
;;    we touch the entry on read operaion as well.
(define-class <lru-cache> (<fifo-cache>) ())

(define (make-lru-cache capacity :key (storage #f) (comparator #f))
  (make <lru-cache> :storage storage :comparator comparator
        :capacity capacity))

(define-method cache-check! ((cache <lru-cache>) key)
  (and-let* ([nv (dict-get (cache-storage cache) key #f)]
             [nv2 (cons (~ cache'counter) (cdr nv))])
    (dict-put! (cache-storage cache) key nv2)
    (%fifo-touch! cache (~ cache'queue) key)
    (cons key (cdr nv))))
  
;; TTL Cache
;;  - Timestamps is a heap with (<timestamp> . <key>).   There can
;;    be multiple entries with the same <key>.
;;  - The storage holds (<timestamp> . <value>).

(define-class <ttl-cache> (<cache>)
  ([ttl :init-keyword :ttl] ; time to live, in seconds
   [timestamper :init-keyword :timestamper] ; a thunk to return a timestamp
   [timestamps :init-form (make-queue)])) ; (<key> . <timestamp>)

(define (make-ttl-cache ttl :key (storage #f) (comparator #f)
                                 (timestamper sys-time))
  (make <ttl-cache> :storage storage :comparator comparator
        :ttl ttl :timestamper timestamper))

(define-method initialize ((c <ttl-cache>) initargs)
  (next-method)
  (cache-populate-queue! (~ c'timestamps) (cache-storage c)))

(define (%ttl-sweep! c)
  (let ([cutoff (- ((~ c'timestamper)) (~ c'ttl))]
        [ts  (~ c'timestamps)]
        [tab (cache-storage c)])
    (let loop ()
      (and-let* ([kt (queue-front ts #f)]
                 [ (< (cdr kt) cutoff) ])
        (dequeue! ts)
        ;; The dict-get below may return #f, if there's more than one
        ;; entry of the key in the heap with the same timestamp.
        (and-let* ([tv (dict-get tab (car kt) #f)]
                   [ (<= (car tv) (cdr kt)) ])
          (dict-delete! tab (car kt)))
        (loop)))))

(define (%ttl-timestamp c) ((~ c'timestamper)))

(define-method cache-check! ((c <ttl-cache>) key)
  (%ttl-sweep! c)
  (and-let1 tv (dict-get (cache-storage c) key #f)
    (cons key (cdr tv))))

(define-method cache-register! ((c <ttl-cache>) key val)
  (let1 t (%ttl-timestamp c)
    (dict-put! (cache-storage c) key (cons t val))
    (enqueue! (~ c'timestamps) (cons key t)))
  (cons key val))

(define-method cache-clear! ((c <ttl-cache>))
  (dict-clear! (cache-storage c))
  (dequeue-all! (~ c'timestamps)))

(define-method cache-write! ((c <ttl-cache>) key val)
  (if-let1 tv (dict-get (cache-storage c) key #f)
    (let1 t (%ttl-timestamp c)
      (set! (car tv) t)
      (set! (cdr tv) val)
      (enqueue! (~ c'timestamps) (cons key t)))
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
    (enqueue! (~ c'timestamps) (cons key t))
    (when (> (queue-length (~ c'timestamps)) (* 3 (size-of (cache-storage c))))
      (cache-compact-queue! (~ c'timestamps) (cache-storage c)))
    (cons key (cdr tv))))

;; Counting cache
;; - This is a wrapper cache to count cache misses/hits
;; NB: counting cache's storage directly points to inner-cache's storage.
;; We assume that any modification on the storage is done via inner-cache's
;; method, so that it won't cause inconsistency.

(define-class <counting-cache> (<cache>)
  ([inner-cache :init-keyword :inner-cache]
   [hits  :init-value 0]
   [misses :init-value 0]))

(define (make-counting-cache inner-cache)
  (make <counting-cache>
    :inner-cache inner-cache
    :storage (cache-storage inner-cache)
    :comparator (cache-comparator inner-cache)))

(define-method cache-check! ((cache <counting-cache>) key)
  (rlet1 r (cache-check! (~ cache'inner-cache) key)
    (if r
      (inc! (~ cache'hits))
      (inc! (~ cache'misses)))))

(define-method cache-register! ((cache <counting-cache>) key value)
  (cache-register! (~ cache'inner-cache) key value))

(define-method cache-write! ((cache <counting-cache>) key value)
  (cache-write! (~ cache'inner-cache) key value))

(define-method cache-evict! ((cache <counting-cache>) key)
  (cache-evict! (~ cache'inner-cache) key))

(define-method cache-clear! ((cache <counting-cache>))
  (cache-clear! (~ cache'inner-cache)))

(define-method cache-stats ((cache <counting-cache>))
  `(:hits ,(~ cache'hits) :misses ,(~ cache'misses)))
