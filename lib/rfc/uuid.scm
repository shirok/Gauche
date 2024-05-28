;;;
;;; rfc.uuid - UUID
;;;
;;;   Copyright (c) 2020-2024  Shiro Kawai  <shiro@acm.org>
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

;; RFC9562 https://www.rfc-editor.org/rfc/rfc9562.html

(define-module rfc.uuid
  (use binary.io)
  (use gauche.record)
  (use gauche.threads)
  (use gauche.uvector)
  (use srfi.27)
  (export <uuid> uuid-value uuid-version
          make-uuid1-generator uuid1 make-uuid4-generator uuid4
          make-uuid6-generator uuid6 make-uuid7-generator uuid7
          nil-uuid
          uuid-random-source
          uuid-random-source-set!
          uuid-comparator
          write-uuid uuid->string parse-uuid)
  )
(select-module rfc.uuid)

;; value is 16-element u8vector
(define-record-type <uuid> %make-uuid uuid? (value uuid-value))

(define-method write-object ((u <uuid>) out)
  (format out "#<uuid v~a ~s>" (uuid-version u) (uuid->string u)))

;;;
;;;I/O
;;;

(define (write-uuid uuid :optional (port (current-output-port)))
  ;; speed-conscious code
  (define (writeb x)
    (write-char (integer->digit (ash x -4) 16) port)
    (write-char (integer->digit (logand x #xf) 16) port))
  (assume-type uuid <uuid>)
  (let1 v (uuid-value uuid)
    (writeb (u8vector-ref v 0))
    (writeb (u8vector-ref v 1))
    (writeb (u8vector-ref v 2))
    (writeb (u8vector-ref v 3))
    (write-char #\- port)
    (writeb (u8vector-ref v 4))
    (writeb (u8vector-ref v 5))
    (write-char #\- port)
    (writeb (u8vector-ref v 6))
    (writeb (u8vector-ref v 7))
    (write-char #\- port)
    (writeb (u8vector-ref v 8))
    (writeb (u8vector-ref v 9))
    (write-char #\- port)
    (writeb (u8vector-ref v 10))
    (writeb (u8vector-ref v 11))
    (writeb (u8vector-ref v 12))
    (writeb (u8vector-ref v 13))
    (writeb (u8vector-ref v 14))
    (writeb (u8vector-ref v 15))))

(define (uuid->string uuid) (call-with-output-string (cut write-uuid uuid <>)))

(define (parse-uuid str :key (if-invalid :error))
  (define (make-uuidx tl tm th cl nd)
    (let1 v (make-u8vector 16)
      (put-u32be! v 0 (string->number tl 16))
      (put-u16be! v 4 (string->number tm 16))
      (put-u16be! v 6 (string->number th 16))
      (put-u16be! v 8 (string->number cl 16))
      (let1 nnd (string->number nd 16)
        (put-u16be! v 10 (ash nnd -32))
        (put-u32be! v 12 (logand nnd #xffffffff)))
      (%make-uuid v)))

  (assume (memq if-invalid '(#f :error))
          "if-invalid argument must be either #f or :error, but got:" if-invalid)

  (rxmatch-case str
    [#/^(?:urn:uuid:)?([[:xdigit:]]{8})-([[:xdigit:]]{4})-([[:xdigit:]]{4})-([[:xdigit:]]{4})-([[:xdigit:]]{12})$/
     (_ tl tm th cl nd)
     (make-uuidx tl tm th cl nd)]
    [#/^\{([[:xdigit:]]{8})-([[:xdigit:]]{4})-([[:xdigit:]]{4})-([[:xdigit:]]{4})-([[:xdigit:]]{12})\}$/
     (_ tl tm th cl nd)
     (make-uuidx tl tm th cl nd)]
    [#/^[[:xdigit:]]{32}$/ (xx)
     (let1 v (make-u8vector 16)
       (put-uint! 16 v 0 (string->number xx 16) 'big-endian)
       (%make-uuid v))]
    ;; invalid input
    [else
     (if if-invalid
       (error "Invalid UUID format: " str)
       #f)]))

;;;
;;;Generation
;;;

(define uuid-random-source
  (make-parameter
   (rlet1 s (make-random-source)
     (random-source-randomize! s))
   (^x (assume (random-source? x) "SRFI-27 random source required, but got:" x)
       x)))

(define (%make-uuid-random-int)
  (random-source-make-integers (uuid-random-source)))

;; Deprecated.  Use parameter.
(define (uuid-random-source-set! s) (uuid-random-source s))

(define (make-timestate) (atom 0 -1))  ; last-time and clock-seq
(define (make-utimestate) (atom 0 -1)) ; like timestate, but for uuidv7

;; Returns a fake node id, when the user doesn't provide one.
(define (make-pseudo-node)
  (logior ((%make-uuid-random-int) (ash 1 47))
          (ash 1 40)))                  ; multicast bit

(define-constant jd-origin 2299160)     ; julian day of 15 Oct 1582
(define-constant jd-unix   2440587)     ; julian day of 1 Jan 1970
(define-constant ts-offset              ; 100ns count of offset
  (* (- jd-unix jd-origin) 86400 #e1e7))

;; returns 60bit timestamp, 100ns resolution since 00:00:00 15 Oct 1582
;; and 14bit clock sequence
(define (%ts timestate random-int)
  (atomic-update! timestate
                  (^[ts0 cl0]
                    (let* ([cl (if (negative? cl0)
                                 (random-int (ash 1 14)) ;initial
                                 cl0)]
                           [now (current-time)]
                           ;; count by 100ns tick
                           [ts (+ (* (~ now'second) #e1e7)
                                  (quotient (~ now'nanosecond) 100)
                                  ts-offset)])
                      (if (<= ts ts0)
                        ;; got clock skew; increment clock seq
                        (values ts (logand (+ cl 1) #x3fff))
                        (values ts cl))))))

;; Similar to %ts, but the timestamp is 48bit millisecond resolution since
;; unix epoch, plus 74bits monotonically increasing sequence number.
;; This is for uuidv7.  We use the latter for rand_a and rand_b fields.
;; The increment step of sequence number is a random
;;
;; for the next millisecond before wrap around, to ensure monotonicity.
(define (%uts utimestate random-int incr-bits)
  (define (wait-a-bit) (sys-nanosleep 500000))
  (define (update ts0 cl0)
    (let* ([cl (if (negative? cl0)
                 (random-int (ash 1 74)) ;initial
                 cl0)]
           [now (current-time)]
           [ts (+ (* (~ now'second) 1000)
                  (quotient (~ now'nanosecond) #e1e6))]
           [cl1 (logand (+ cl 1 (random-int (- (ash 1 incr-bits) 1)))
                        (- (ash 1 74) 1))])
      (cond [(> ts ts0)    ;timestamp differ, so we just use cl1
             (values ts cl1)]
            [(< cl1 cl)    ;counter wraparound.  wait for next millisec
             (wait-a-bit)
             (update ts0 cl0)]
            [else
             (values ts cl1)])))
  (atomic-update! utimestate update))

(define nil-uuid
  (let1 u (%make-uuid (make-u8vector 16 0))
    (^[] u)))

;; UUID v1 and v6
;;  They use the same info, just the order of timestamp bits differs.
(define (make-uuid1-generator :optional (default-node-id #f))
  (define nid (or default-node-id (make-pseudo-node)))
  (define tstate (make-timestate))
  (define random-int (%make-uuid-random-int))
  (^[:optional (node-id #f)]
    (let ([id (or node-id nid)]
          [v (make-u8vector 16)])
      (receive (ts cs) (%ts tstate random-int)
        (put-u32be! v 0 (logand ts #xffffffff))
        (put-u16be! v 4 (logand (ash ts -32) #xffff))
        (put-u16be! v 6 (logior (logand (ash ts -48) #x0fff)
                                #x1000))    ; Version 1
        (put-u16be! v 8 (logior cs #x8000)) ; Variant 10
        (put-u16be! v 10 (ash nid -32))
        (put-u32be! v 12 (logand nid #xffffffff)))
      (%make-uuid v))))

(define uuid1 (make-uuid1-generator))

(define (make-uuid6-generator :optional (default-node-id #f))
  (define nid (or default-node-id (make-pseudo-node)))
  (define tstate (make-timestate))
  (define random-int (%make-uuid-random-int))
  (^[:optional (node-id #f)]
    (let ([id (or node-id nid)]
          [v (make-u8vector 16)])
      (receive (ts cs) (%ts tstate random-int)
        (put-u32be! v 0 (logand (ash ts -28) #xffffffff))
        (put-u16be! v 4 (logand (ash ts -12) #xffff))
        (put-u16be! v 6 (logior (logand ts #x0fff)
                                #x6000))    ; Version 6
        (put-u16be! v 8 (logior cs #x8000)) ; Variant 10
        (put-u16be! v 10 (ash nid -32))
        (put-u32be! v 12 (logand nid #xffffffff)))
      (%make-uuid v))))

(define uuid6 (make-uuid6-generator))

;; UUID v4
;;   Mostly random bits.
(define (make-uuid4-generator)
  (define random-int (%make-uuid-random-int))
  (^[]
    (let1 v (make-u8vector 16)
      (put-u32be! v 0  (random-int (ash 1 32)))
      (put-u32be! v 4  (logior (logand (random-int (ash 1 32))
                                       #xffff0fff)
                               #x4000))  ;Verson 4
      (put-u32be! v 8  (copy-bit-field (random-int (ash 1 32))
                                       2 30 32)) ;Variant 10
      (put-u32be! v 12 (random-int (ash 1 32)))
      (%make-uuid v))))

(define uuid4 (make-uuid4-generator))

;; UUID v7
;;   Monotonically increasing over time.  Similar to ULID.
;;   To guarantee monotonicity, we combine rand_a and rand_b fields
;;   to store monotonically increasing sequence with random increments.
;;   If it wraparounds, we wait till next milliseconds boundary.
(define (make-uuid7-generator :optional (increment-bits 64))
  (define utstate (make-utimestate))
  (define random-int (%make-uuid-random-int))
  (assume (>= increment-bits 1))
  (^[]
    (let1 v (make-u8vector 16)
      (receive (ts cs) (%uts utstate random-int increment-bits)
        (put-u32be! v 0 (ash ts -16))
        (put-u16be! v 4 (logand ts #xffff))
        (put-u16be! v 6 (logior (logand (ash cs -62) #x0fff)
                                #x7000))   ;Version 7
        (put-u32be! v 8 (logior #x8000     ;Variant 10
                                (logand (ash cs -32) #x3fffffff)))
        (put-u32be! v 12 (logand cs #xffffffff)))
      (%make-uuid v))))

(define uuid7 (make-uuid7-generator))

;;;
;;;Meta info
;;;

(define (uuid-version uuid)
  (assume-type uuid <uuid>)
  (ash (u8vector-ref (uuid-value uuid) 6) -4))

(define-method object-equal? ((u1 <uuid>) (u2 <uuid>))
  (equal? (uuid-value u1) (uuid-value u2)))
(define-method object-compare ((u1 <uuid>) (u2 <uuid>))
  (u8vector-compare (uuid-value u1) (uuid-value u2)))
(define-method object-hash ((u <uuid>) rec)
  (default-hash (uuid-value u)))

(define-inline uuid-comparator
  (make-comparator/compare uuid? object-equal? object-compare object-hash))
