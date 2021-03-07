;;;
;;;  data.ring-buffer - Ring buffers
;;;
;;;   Copyright (c) 2015-2020  Shiro Kawai  <shiro@acm.org>
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

(define-module data.ring-buffer
  (use gauche.sequence)
  (use gauche.uvector)
  (use gauche.record)
  (use scheme.vector)
  (use util.match)
  (use srfi-42)
  (export make-ring-buffer
          make-overflow-doubler
          ring-buffer?
          ring-buffer-empty? ring-buffer-num-entries ring-buffer-capacity
          ring-buffer-full?
          ring-buffer-front ring-buffer-back
          ring-buffer-add-front! ring-buffer-add-back!
          ring-buffer-remove-front! ring-buffer-remove-back!
          ring-buffer-ref ring-buffer-set! ring-buffer-insert-all!

          ring-buffer->xsubvectors
          ring-buffer->xvector ring-buffer->xvector!))
(select-module data.ring-buffer)

;;
;;          +-----------+            +-----------+
;;          |  (vacant) |            |///////////|
;;          |           |            |///////////|
;;          +-----------+            |///////////|
;;    head >|///////////|            +-----------+
;;          |///////////|      tail >|  (vacant) |
;;          |///////////|            |           |
;;          |///////////|            |           |
;;          +-----------+            |           |
;;    tail >|  (vacant) |            +-----------+
;;          |           |      head >|///////////|
;;          +-----------+            +-----------+ < capacity

;; Although we define ring-buffer as a record-type, we don't export
;; accessors/constructors.  Users of this module should use exported
;; public APIs.
;;
;; Originally, ring-buffer is only supposed to grow by one item
;; at a time, so overflow-handler only needs to expand a storage at least one.
;; However, we decdied to use ring-buffer as a basis of flexvector (srfi-214)
;; and needs some additional features, including the ability to extend
;; the storage in arbitrary length.  We think it is slightly out-of-scope
;; as a feature of a ring-buffer, so for now we keep it unofficial.

(define-record-type (ring-buffer <record> :mixins (<sequence>))
  %make-ring-buffer ring-buffer?
  (storage)
  overflow-handler
  room-maker                            ;only used by ring-buffer-insert-all!
  (head)
  (tail)
  (capacity)
  (num-entries))

;; ref/set! dispatcher
;; We avoid using generic dispatch of '~', for ring buffers
;; may be used in speed-conscious/allocation-conscious situation.
;; It is kind of dumb that we have to roll our own dispatcher, though;
;; there should be a built-in support.
(define *dispatch-table*
  ($ hash-table 'eq?
     `(,<vector>    ,vector-length  ,vector-ref    ,vector-set!
                    ,make-vector    . ,vector-copy!)
     `(,<u8vector>  ,uvector-length ,u8vector-ref  ,u8vector-set!
                    ,make-u8vector  . ,uvector-copy!)
     `(,<s8vector>  ,uvector-length ,s8vector-ref  ,s8vector-set!
                    ,make-s8vector  . ,uvector-copy!)
     `(,<u16vector> ,uvector-length ,u16vector-ref ,u16vector-set!
                    ,make-u16vector . ,uvector-copy!)
     `(,<s16vector> ,uvector-length ,s16vector-ref ,s16vector-set!
                    ,make-s16vector . ,uvector-copy!)
     `(,<u32vector> ,uvector-length ,u32vector-ref ,u32vector-set!
                    ,make-u32vector . ,uvector-copy!)
     `(,<s32vector> ,uvector-length ,s32vector-ref ,s32vector-set!
                    ,make-s32vector . ,uvector-copy!)
     `(,<u64vector> ,uvector-length ,u64vector-ref ,u64vector-set!
                    ,make-u64vector . ,uvector-copy!)
     `(,<s64vector> ,uvector-length ,s64vector-ref ,s64vector-set!
                    ,make-s64vector . ,uvector-copy!)
     `(,<f16vector> ,uvector-length ,f16vector-ref ,f16vector-set!
                    ,make-f16vector . ,uvector-copy!)
     `(,<f32vector> ,uvector-length ,f32vector-ref ,f32vector-set!
                    ,make-f32vector . ,uvector-copy!)
     `(,<f64vector> ,uvector-length ,f64vector-ref ,f64vector-set!
                    ,make-f64vector . ,uvector-copy!)
     ))

;; dprocs is (<length> <ref> <set!> <alloc> . <copy!>)
(define-inline (%dprocs storage)
  (hash-table-get *dispatch-table* (class-of storage)))

(define-inline (%rb-size dprocs storage)
  ((car dprocs) storage))

(define-inline (%rb-mod-index dprocs storage index)
  (modulo index (%rb-size dprocs storage)))

(define-inline (%rb-ref dprocs storage index)
  ((cadr dprocs) storage index))

(define-inline (%rb-ref-1 dprocs storage index)
  ((cadr dprocs) storage (%rb-mod-index dprocs storage (- index 1))))

(define-inline (%rb-set! dprocs storage index val)
  ((caddr dprocs) storage index val))

(define-inline (%rb-alloc dprocs size)
  ((cadddr dprocs) size))

(define-inline (%rb-copy! dprocs dest dstart src sstart send)
  ((cddddr dprocs) dest dstart src sstart send))

(define (%rb-copy-contents! rb newvec oldvec)
  (let1 dprocs (%dprocs oldvec)
    (let ([h (ring-buffer-head rb)]
          [t (ring-buffer-tail rb)])
      (if (<= t h)
        (let1 c (ring-buffer-capacity rb)
          (%rb-copy! dprocs newvec 0 oldvec h c)
          (%rb-copy! dprocs newvec (- c h) oldvec 0 t))
        (%rb-copy! dprocs newvec 0 oldvec h t)))))

(define-inline (%rb-head-inc! rb dprocs storage delta)
  ($ ring-buffer-head-set! rb
     (%rb-mod-index dprocs (ring-buffer-storage rb)
                    (+ (ring-buffer-head rb) delta))))

(define-inline (%rb-tail-inc! rb dprocs storage delta)
  ($ ring-buffer-tail-set! rb
     (%rb-mod-index dprocs (ring-buffer-storage rb)
                    (+ (ring-buffer-tail rb) delta))))

;; predefined overflow handlers
(define (overflow-error rb v) 'error)
(define (overflow-overwrite rb v) 'overwrite)

;; API
(define (make-overflow-doubler :key (max-increase +inf.0)
                                    (max-capacity +inf.0))
  (^[rb v]
    (let1 size (ring-buffer-capacity rb)
      (cond [(>= size max-capacity) 'error]
            [(>= size max-increase)
             (%rb-alloc (%dprocs v) (+ size max-increase))]
            [else
             (%rb-alloc (%dprocs v) (* size 2))]))))

;; API
;; make-ring-buffer
;;  Returns a ring buffer.
;;  STORAGE can be vector-like objects.
;;  OVERFLOW-HANDLER must be a procedure that takes ring buffer instance
;;    and the current backing storage.  It can perform one of the following
;;    ops.
;;
;;    Returns 'error      - causes the API to throws an error
;;    Returns 'overwrite  - overwrite existing entries.
;;    Allocates larger backing storage
(define (make-ring-buffer :optional (storage (make-vector 4))
                          :key (overflow-handler (make-overflow-doubler))
                               (room-maker #f)
                               (initial-head-index 0)
                               (initial-tail-index 0))
  (assume (exact-integer? initial-head-index))
  (assume (exact-integer? initial-tail-index))
  (assume (or (vector? storage) (uvector? storage))
          "Ring buffer storage must be a vector-like object, but got:" storage)
  (assume (<= 0 initial-head-index (- (size-of storage) 1))
          "initial-head-index out of range:" initial-head-index)
  (assume (<= initial-head-index initial-tail-index (size-of storage))
          "initial-tail-index out of range:" initial-tail-index)
  (let1 ov-handler
      (case overflow-handler
        [(error) overflow-error]
        [(overwrite) overflow-overwrite]
        [else (unless (applicable? overflow-handler ring-buffer <top>)
                (error "overflow-handler must be a procedure, or \
                              a symbol error of overwrite, but got"
                       overflow-handler))
              overflow-handler])
    (%make-ring-buffer storage ov-handler room-maker
                       initial-head-index
                       (modulo initial-tail-index (size-of storage))
                       (size-of storage)
                       (- initial-tail-index initial-head-index))))

;; API
(define (ring-buffer-empty? rb) (zero? (ring-buffer-num-entries rb)))

;; API
(define (ring-buffer-full? rb)
  (= (ring-buffer-num-entries rb) (ring-buffer-capacity rb)))

(define (%ensure-nonempty rb)
  (when (ring-buffer-empty? rb)
    (error "Ring buffer is empty:" rb)))

;; Make sure there's room for at least one entry in rb
(define (%ensure-room! rb dir) ; dir = 'forward | 'backward
  (when (= (ring-buffer-num-entries rb) (ring-buffer-capacity rb))
    (let1 v ((ring-buffer-overflow-handler rb) rb (ring-buffer-storage rb))
      (case v
        [(error) (error "Ring buffer overflow:" rb)]
        [(overwrite)
         ;; pop the last item so that we can fill it
         (ecase dir
           [(forward)
            (let1 s (ring-buffer-storage rb)
              (%rb-head-inc! rb (%dprocs s) s 1))]
           [(backward)
            (let1 s (ring-buffer-storage rb)
              (%rb-tail-inc! rb (%dprocs s) s -1))])
         (dec! (ring-buffer-num-entries rb))]
        [else
         (unless (or (vector? v) (uvector? v))
           (error "Ring buffer overflow handler returned invalid object:" v))
         (%rb-copy-contents! rb v (ring-buffer-storage rb))
         (ring-buffer-head-set! rb 0)
         (ring-buffer-tail-set! rb (ring-buffer-num-entries rb))
         (ring-buffer-capacity-set! rb (size-of v))
         (ring-buffer-storage-set! rb v)]))))

;; API
(define (ring-buffer-front rb)
  (%ensure-nonempty rb)
  (let1 s (ring-buffer-storage rb)
    (%rb-ref (%dprocs s) s (ring-buffer-head rb))))

;; API
(define (ring-buffer-back rb)
  (%ensure-nonempty rb)
  (let1 s (ring-buffer-storage rb)
    (%rb-ref-1 (%dprocs s) s (ring-buffer-tail rb))))

;; API
(define (ring-buffer-add-front! rb elt)
  (%ensure-room! rb 'backward)
  (let* ([s (ring-buffer-storage rb)]
         [dprocs (%dprocs s)])
    (%rb-head-inc! rb dprocs s -1)
    (%rb-set! dprocs s (ring-buffer-head rb) elt)
    (inc! (ring-buffer-num-entries rb))
    (undefined)))

;; API
(define (ring-buffer-add-back! rb elt)
  (%ensure-room! rb 'forward)
  (let* ([s (ring-buffer-storage rb)]
         [dprocs (%dprocs s)])
    (%rb-set! dprocs s (ring-buffer-tail rb) elt)
    (%rb-tail-inc! rb dprocs s 1)
    (inc! (ring-buffer-num-entries rb))
    (undefined)))

;; API
(define (ring-buffer-remove-front! rb)
  (%ensure-nonempty rb)
  (let* ([s (ring-buffer-storage rb)]
         [dprocs (%dprocs s)])
    (rlet1 v (%rb-ref dprocs s (ring-buffer-head rb))
      (%rb-head-inc! rb dprocs s 1)
      (dec! (ring-buffer-num-entries rb)))))

;; API
(define (ring-buffer-remove-back! rb)
  (%ensure-nonempty rb)
  (let* ([s (ring-buffer-storage rb)]
         [dprocs (%dprocs s)])
    (%rb-tail-inc! rb dprocs s -1)
    (dec! (ring-buffer-num-entries rb))
    (%rb-ref dprocs s (ring-buffer-tail rb))))

;; API
(define (ring-buffer-ref rb n :optional fallback)
  (if (<= 0 n (- (ring-buffer-num-entries rb) 1))
    (let* ([s (ring-buffer-storage rb)]
           [dprocs (%dprocs s)])
      (%rb-ref dprocs s (%rb-mod-index dprocs s (+ (ring-buffer-head rb) n))))
    (if (undefined? fallback)
      (errorf "index out of range (~s) for a ring buffer ~s" n rb)
      fallback)))

;; API
(define (ring-buffer-set! rb n val)
  (unless (<= 0 n (- (ring-buffer-num-entries rb) 1))
    (errorf "index out of range (~s) for a ring buffer ~s" n rb))
  (let* ([s (ring-buffer-storage rb)]
         [dprocs (%dprocs s)])
    (%rb-set! dprocs s (%rb-mod-index dprocs s (+ (ring-buffer-head rb) n))
              val)))

;; API (Unofficial)
;;  Insert ITEMS (a list or a (u)vector) at N-th position of RB, shifting
;;  the rest elements.
;;  This is to implement flexvector.  We're not sure if this is appropriate
;;  as a feature of ring-buffer, though, so for now we keep this unofficial.
;   Note: The ring buffer needs room-maker callback, which takes
(define (ring-buffer-insert-all! rb n items)
  (assume (<= 0 n (ring-buffer-num-entries rb))
          "index out of range:"n)
  (let1 size (size-of items)
    (if (>= (+ (ring-buffer-num-entries rb) size) (ring-buffer-capacity rb))
      (if-let1 room-maker (ring-buffer-room-maker rb)
        (let1 newbuf ($ room-maker rb
                        (ring-buffer-storage rb)
                        (+ size (ring-buffer-num-entries rb)))
          (ring-buffer->xvector! newbuf 0 rb 0 n)
          (ring-buffer->xvector! newbuf (+ n size) rb n)
          (do-ec (: item (index i) items)
                 (set! (~ newbuf (+ n i)) item))
          (ring-buffer-storage-set! rb newbuf)
          (ring-buffer-num-entries-set! rb (+ size (ring-buffer-num-entries rb)))
          (ring-buffer-capacity-set! rb (size-of newbuf))
          (ring-buffer-head-set! rb 0)
          (ring-buffer-tail-set! rb (ring-buffer-num-entries rb)))
        (errorf "ring buffer overflow (capacity=~s, required=~s)"
                (ring-buffer-capacity rb)
                (+ size (ring-buffer-num-entries rb))))
      (let ([storage (ring-buffer-storage rb)]
            [cap (ring-buffer-capacity rb)])
        (do-ec (: i size 0 -1)
               (set! (~ storage (modulo (+ n i -1) cap))
                     (~ storage (modulo (+ n size i -1) cap))))
        (do-ec (: item (index i) items)
               (set! (~ storage (modulo (+ n i) cap)) item)))))
  (undefined))

;; API
;;  Returns (vec1 start1 end1 vec2 start2 end2 ...)
;;  The list is compatbile to the argument list of *vector-append-subvectors.
;;  With the current implementation, the number of subvector ranges are
;;  0, 1 or 2, but the caller shouldn't assume that.
(define (ring-buffer->xsubvectors rb
                                  :optional (start 0)
                                            (end (ring-buffer-num-entries rb)))
  (define storage (ring-buffer-storage rb))
  (assume (<= 0 start end (ring-buffer-num-entries rb))
          "start/end index out of range:" (list start end))
  (if (= start end)
    '()
    (let ([h (modulo (+ (ring-buffer-head rb) start)
                     (ring-buffer-capacity rb))]
          [t (modulo (+ (ring-buffer-head rb) end)
                     (ring-buffer-capacity rb))])
      (cond [(< h t) `(,storage ,h ,t)]
            [(= t 0) `(,storage ,h ,(ring-buffer-capacity rb))]
            [else `(,storage ,h ,(ring-buffer-capacity rb) ,storage ,0 ,t)]))))

;; API
(define (ring-buffer->xvector rb
                              :optional (start 0)
                                        (end (ring-buffer-num-entries rb)))
  (define storage (ring-buffer-storage rb))
  (define appender
    ;; there should be better way
    (cond [(vector? storage) vector-append-subvectors]
          [(u8vector? storage) u8vector-append-subvectors]
          [(s8vector? storage) s8vector-append-subvectors]
          [(u16vector? storage) u16vector-append-subvectors]
          [(s16vector? storage) s16vector-append-subvectors]
          [(u32vector? storage) u32vector-append-subvectors]
          [(s32vector? storage) s32vector-append-subvectors]
          [(u64vector? storage) u64vector-append-subvectors]
          [(s64vector? storage) s64vector-append-subvectors]
          [(f16vector? storage) f16vector-append-subvectors]
          [(f32vector? storage) f32vector-append-subvectors]
          [(f64vector? storage) f64vector-append-subvectors]
          [(c32vector? storage) c32vector-append-subvectors]
          [(c64vector? storage) c64vector-append-subvectors]
          [(c128vector? storage) c128vector-append-subvectors]
          [else (error "Unsupported backing storage:" storage)]))
  (apply appender (ring-buffer->xsubvectors rb start end)))

;; API
(define (ring-buffer->xvector! target tstart rb
                               :optional (start 0)
                                         (end (ring-buffer-num-entries rb)))
  (define storage (ring-buffer-storage rb))
  (define fill! (if (vector? storage) vector-copy! uvector-copy!))

  (assume (eqv? (class-of storage) (class-of target))
          "target must be the same class as ring-buffer's backing storage:"
          (list (class-of target) (class-of storage)))
  (assume (<= 0 start end (ring-buffer-num-entries rb))
          "start/end index out of range:" (list start end))
  (assume (and (<= 0 tstart)
               (<= (+ tstart (- end start)) (size-of target)))
          "tstart out of range:" tstart)

  (let loop ([k tstart]
             [subvecs (ring-buffer->xsubvectors rb start end)])
    (match subvecs
      [() (undefined)]
      [(vec s e . rest) (fill! target k vec s e) (loop (+ k (- e s)) rest)])))

;;
;; Sequence protocol
;;

(define-method call-with-iterator ((rb ring-buffer) proc :key (start 0))
  (define len (ring-buffer-num-entries rb))
  (define k start)
  (proc (^[] (>= k len))
        (^[] (rlet1 v (ring-buffer-ref rb k)
               (inc! k)))))

(define-method call-with-reverse-iterator ((rb ring-buffer) proc
                                           :key
                                           (start 0)
                                           (end (ring-buffer-num-entries rb))
                                           :allow-other-keys)
  (define k (- end 1))
  (proc (^[] (<= start k))
        (^[] (rlet1 v (ring-buffer-ref rb k)
               (dec! k)))))

(define-method referencer ((rb ring-buffer)) ring-buffer-ref)
(define-method modifier ((rb ring-buffer)) ring-buffer-set!)
