;;;
;;;  data.ring-buffer - Ring buffers
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

(define-module data.ring-buffer
  (use gauche.sequence)
  (use gauche.uvector)
  (use gauche.record)
  (use srfi-43)
  (export make-ring-buffer
          make-overflow-doubler
          ring-buffer?
          ring-buffer-empty? ring-buffer-num-entries ring-buffer-capacity
          ring-buffer-full?
          ring-buffer-front ring-buffer-back
          ring-buffer-add-front! ring-buffer-add-back!
          ring-buffer-remove-front! ring-buffer-remove-back!
          ring-buffer-ref ring-buffer-set!))
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
;;          +-----------+            +-----------+

;; Although we define ring-buffer as a record-type, we don't export
;; accessors/constructors.  Users of this module should use exported
;; public APIs.
(define-record-type ring-buffer %make-ring-buffer ring-buffer?
  (storage)
  overflow-handler
  (head)
  (tail)
  (capacity)
  (num-entries))

;; ref/set! dispatcher
;; We avoid using generic dispatch of '~', for ring buffers
;; may be used in speed-conscious/allocation-conscious situation.
;; It is kind of dumb that we have to roll our own dispatcher, though;
;; there should be a built-in support.
(define-constant *dispatch-table*
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
                          :key (overflow-handler (make-overflow-doubler)))
  (unless (or (vector? storage) (uvector? storage))
    (error "Ring buffer storage must be a vector-like object, but got:" storage))
  (let1 h (case overflow-handler
            [(error) overflow-error]
            [(overwrite) overflow-overwrite]
            [else (unless (applicable? overflow-handler ring-buffer <top>)
                    (error "overflow-handler must be a procedure, or a symbol error of overwrite, but got" overflow-handler))
                  overflow-handler])
    (%make-ring-buffer storage h 0 0 (size-of storage) 0)))

;; API
(define (ring-buffer-empty? rb) (zero? (ring-buffer-num-entries rb)))

;; API
(define (ring-buffer-full? rb)
  (= (ring-buffer-num-entries rb) (ring-buffer-capacity rb)))

(define (%ensure-nonempty rb)
  (when (ring-buffer-empty? rb)
    (error "Ring buffer is empty:" rb)))

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

