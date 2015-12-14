;;;
;;; gauche.vport - virtual port
;;;
;;;   Copyright (c) 2004-2015  Shiro Kawai  <shiro@acm.org>
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

(define-module gauche.vport
  (use gauche.uvector)
  (use gauche.generator)
  (use util.match)
  (export <virtual-input-port>
          <virtual-output-port>
          <buffered-input-port>
          <buffered-output-port>
          open-input-uvector
          open-output-uvector get-output-uvector
          open-input-limited-length-port
          open-input-char-list open-input-byte-list get-remaining-input-list
          open-input-char-generator open-input-byte-generator
          get-remaining-input-generator
          ))
(select-module gauche.vport)

(dynamic-load "gauche--vport")

;;=======================================================
;; A port backed up by an uniform vector
;;

(define (open-input-uvector uvector)
  (let* ([src (if (u8vector? uvector)
                uvector
                (uvector-alias <u8vector> uvector))]
         [index 0]
         [len (u8vector-length src)])
    (define (filler buf)
      (if (>= index len)
        #f
        (let1 req (u8vector-length buf)
          (if (>= req (- len index))
            (rlet1 count (- len index)
              (u8vector-copy! buf 0 src index)
              (inc! index count))
            (begin
              (u8vector-copy! buf 0 src index (+ index req))
              (inc! index req)
              req)))))
    (define (seeker offset whence)
      (cond [(= whence SEEK_SET)
             (set! index (clamp offset 0 len))]
            [(= whence SEEK_CUR)
             (set! index (clamp (+ index offset) 0 len))]
            [(= whence SEEK_END)
             (set! index (clamp (+ len offset) 0 len))])
      index)
    (make <buffered-input-port> :fill filler :seek seeker)))

;; For output uvector, we keep backing storage info in port attributes so that
;; we can retrieve it by get-output-uvector.
;; The info is #(<buffer> <max-index> <target-class>), where <buffer> is
;; u8vector (possibly aliased), <max-index> is the maximum index during
;; write operation, and <target-class> is the original u8vector class.
(define (%make-uvector-storage uvector)
  (vector (if (u8vector? uvector) uvector (uvector-alias <u8vector> uvector))
          0 (class-of uvector)))

;; Ensure the backing storage can contain at least next-index octets.
;; We adopt allocate-and-copy strategy.  Although this is amortized O(1),
;; it may not be ideal for our GC; using chained chunks may behave better.
;; Let's see.
(define (%extend-uvector-storage! storage next-index)
  (let* ([cur-buffer (vector-ref storage 0)]
         [cur-size (uvector-size cur-buffer)])
    (when (> next-index cur-size)
      (let* ([next-size (ash 1 (integer-length (- next-index 1)))] ;round up
             [next-buffer (make-uvector <u8vector> next-size)])
        (u8vector-copy! next-buffer 0 cur-buffer)
        (set! (vector-ref storage 0) next-buffer)))))

(define (%storage-buffer storage) (vector-ref storage 0))
(define (%storage-size storage) (uvector-size (%storage-buffer storage)))
(define (%storage-get-uvector storage)
  (match-let1 #(buffer max-index class) storage
    (let ([aligned-index (if (memv class (list <u8vector> <s8vector>))
                           max-index
                           (let1 align (uvector-class-element-size class)
                             (* (quotient max-index align) align)))])
      (uvector-alias class buffer 0 aligned-index))))
(define (%storage-max-index storage) (vector-ref storage 1))

(define (%update-storage-max-index! storage max-index)
  (update! (vector-ref storage 1) (cut max <> max-index)))

(define (open-output-uvector :optional (uvector #f) :key (extendable #f))
  ;; If uvector isn't provided, extendable is #t.
  ;; If uvector is provided, extendable defaults to #f.
  ;; This is for the backward compatibility, though confusing.
  (let* ([extendable (if uvector extendable #t)]
         [uvector (or uvector '#u8())]
         [storage (%make-uvector-storage uvector)]
         [index 0])
    (define (set-index! val)
      (%update-storage-max-index! storage val)
      (set! index val))
    (define (flusher buf flush?)
      (rlet1 req (u8vector-length buf)
        (if (> req (- (%storage-size storage) index))
          ;; buffer is too small.  if we're extendable, realloc the buffer.
          ;; otherwise we just discard the excess data.
          (if extendable
            (begin
              (%extend-uvector-storage! storage (+ index req))
              (flusher buf flush?))
            (let1 count (- (%storage-size storage) index)
              (u8vector-copy! (%storage-buffer storage) index buf 0 count)
              (set-index! (+ index count))))
          (begin ; normal path
            (u8vector-copy! (%storage-buffer storage) index buf 0 req)
            (set-index! (+ index req))))))
    (define (seeker offset whence)
      (define (do-seek new-index)
        (when extendable (%extend-uvector-storage! storage new-index))
        (set-index! (clamp new-index 0 (%storage-size storage))))
      (cond [(= whence SEEK_SET) (do-seek offset)]
            [(= whence SEEK_CUR) (do-seek (+ index offset))]
            [(= whence SEEK_END)
             ;; the meaning of 'end' differ if the storage is extendable or not.
             ;; if it's extendable, the rightmost end of the data ever written
             ;; is the 'end' whence; if fixed sized storage, end is the
             ;; end of the given storage.
             (if extendable
               (do-seek (+ (%storage-max-index storage) offset))
               (do-seek (+ (%storage-size storage) offset)))])
      index)
    (rlet1 port (make <buffered-output-port> :flush flusher :seek seeker)
      (port-attribute-set! port 'uvector-port-backing-storage storage))))

(define (get-output-uvector port :key (shared #f))
  (with-port-locking port
    (^[]
      (and-let* ([storage
                  (port-attribute-ref port 'uvector-port-backing-storage #f)])
        (flush port) ; ensure buffered output is written to the storage
        (let1 vec (%storage-get-uvector storage)
          (if shared
            vec
            (uvector-copy vec)))))))

;;=======================================================
;; A port with limited-length input/output
;;

(define (open-input-limited-length-port source limit
                                        :key (limit-reached #f)
                                             (eof-reached #f) (closed #f))
  (let ([nrest limit]
        [eof #f])
    (define (filler buf)
      (if (or (<= nrest 0) eof)
        (if limit-reached (limit-reached buf) 0)
        (let* ([len   (u8vector-length buf)]
               [nread (read-block! buf source 0 (min nrest len))])
          (cond [(eof-object? nread)
                 (set! eof #t)
                 (if eof-reached (eof-reached buf) 0)]
                [else
                 (dec! nrest nread)
                 nread]))))
    (define (closer)
      (when closed (closed)))
    (make <buffered-input-port> :fill filler :close closer)))

;;=======================================================
;; A port read from list of chars or bytes
;;

(define (open-input-char-list lis)
  (define (getc)
    (if (null? lis)
      (eof-object)
      (rlet1 c (pop! lis)
        (unless (char? c)
          (error "input-char-list: source contains non-char value:" c)))))
  (rlet1 p (make <virtual-input-port> :getc getc)
    (define (getlist)
      (let1 cc ((with-module gauche.internal %port-ungotten-chars) p)
        (append cc lis)))
    (port-attribute-set! p 'get-input-list getlist)))

(define (open-input-byte-list lis)
  (define (getb)
    (if (null? lis)
      (eof-object)
      (rlet1 b (pop! lis)
        (unless (and (integer? b) (<= 0 b 255))
          (error "input-byte-list: source contains other than bytes:" b)))))
  (rlet1 p (make <virtual-input-port> :getb getb)
    (define (getlist)
      (let ([bb ((with-module gauche.internal %port-ungotten-bytes) p)]
            [cc ((with-module gauche.internal %port-ungotten-chars) p)])
        (concatenate `(,@(map %char->bytes cc) ,bb ,lis))))
    (port-attribute-set! p 'get-input-list getlist)))

(define (get-remaining-input-list p)
  (if-let1 getlist (port-attribute-ref p 'get-input-list)
    (getlist)
    '()))

;; Common utility used to convert ungotten char to byte list
(define (%char->bytes c)
  (let1 b (char->integer c)
    (if (< b #x80)
      (list b)
      ;; this is inefficient, but we assume it's a rare path
      (u8vector->list (string->u8vector (string c))))))  


;;=======================================================
;; A port read from generators
;;

(define (open-input-char-generator gen)
  (define (getc)
    (rlet1 c (gen)
      (unless (or (char? c) (eof-object? c))
        (error "input-char-generator: source yields non-char value:" c))))
  (rlet1 p (make <virtual-input-port> :getc getc)
    (define (getlist)
      (let1 cc ((with-module gauche.internal %port-ungotten-chars) p)
        (%gprepend cc gen)))
    (port-attribute-set! p 'get-input-generator getlist)))

(define (open-input-byte-generator gen)
  (define (getb)
    (rlet1 b (gen)
      (unless (or (eof-object? b) (and (integer? b) (<= 0 b 255)))
        (error "input-byte-generator: source yields other than bytes:" b))))
  (rlet1 p (make <virtual-input-port> :getb getb)
    (define (getgen)
      (let ([bb ((with-module gauche.internal %port-ungotten-bytes) p)]
            [cc ((with-module gauche.internal %port-ungotten-chars) p)])
        (%gprepend (append (append-map %char->bytes cc) bb) gen)))
    (port-attribute-set! p 'get-input-generator getgen)))

(define (get-remaining-input-generator p)
  (if-let1 getgen (port-attribute-ref p 'get-input-generator)
    (getgen)
    eof-object))

(define (%gprepend lis gen) (gunfold null? car cdr lis (^_ gen)))
