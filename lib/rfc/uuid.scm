;;;
;;; rfc.uuid - UUID
;;;
;;;   Copyright (c) 2020-2022  Shiro Kawai  <shiro@acm.org>
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

;; RFC4122 https://tools.ietf.org/html/rfc4122

(define-module rfc.uuid
  (use gauche.record)
  (use gauche.uvector)
  (use gauche.threads)
  (use gauche.parameter)                ; we use hooks
  (use binary.io)
  (use srfi-27)
  (export <uuid> uuid-value uuid-version
          uuid1 uuid4 nil-uuid
          uuid-random-source
          uuid-random-source-set!
          uuid-comparator
          write-uuid uuid->string parse-uuid)
  )
(select-module rfc.uuid)

;; value is 16-element u8vector
(define-record-type <uuid> %make-uuid uuid? (value uuid-value))

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

(define (parse-uuid str)
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
    [else (error "Invalid UUID format: " str)]))

;;;
;;;Generation
;;;

(define uuid-random-source
  (make-parameter
   (rlet1 s (make-random-source)
     (random-source-randomize! s))
   (^x (assume random-source? "srfi-27 random source required, but got:" x) x)))

(define %uuid-random-int
  (random-source-make-integers (uuid-random-source)))

(parameter-observer-add! uuid-random-source
                         (^[old new]
                           (set! %uuid-random-int
                                 (random-source-make-integers new))))

;; Deprecated.  Use parameter.
(define (uuid-random-source-set! s) (uuid-random-source s))

(define timestate (atom 0 -1)) ; last-time and clock-seq
(define pseudo-node (atom #f)) ; fake node id

(define-constant jd-origin 2299160)     ; julian day of 15 Oct 1582
(define-constant jd-unix   2440587)     ; julian day of 1 Jan 1970
(define-constant ts-offset              ; 100ns count of offset
  (* (- jd-unix jd-origin) 86400 #e1e7))

;; returns 60bit timestamp, 100ns resolution since 00:00:00 15 Oct 1582
;; and 14bit clock sequence
(define (%ts)
  (atomic-update! timestate
                  (^[ts0 cl0]
                    (let* ([cl (if (negative? cl0)
                                 (%uuid-random-int (ash 1 14)) ;initial
                                 cl0)]
                           [now (current-time)]
                           [ts (+ (* (~ now'second) #x1e9)
                                  (quotient (~ now'nanosecond) 100)
                                  ts-offset)])
                      (if (<= ts ts0)
                        ;; got clock skew; increment clock seq
                        (values ts (logand (+ cl 1) #x3fff))
                        (values ts cl))))))

(define nil-uuid
  (let1 u (%make-uuid (make-u8vector 16 0))
    (^[] u)))

(define (%pseudo-node)                  ; see Section 4.5
  (logior (%uuid-random-int (ash 1 47))
          (ash 1 40)))                  ; multicast bit

(define (uuid1 :optional (node-id #f))
  (let ([nid (or node-id
                 (atomic-update! pseudo-node
                                 (^[n] (or n (%pseudo-node)))))]
        [v (make-u8vector 16)])
    (receive (ts cs) (%ts)
      (put-u32be! v 0 (logand ts #xffffffff))
      (put-u16be! v 4 (logand (ash ts -32) #xffff))
      (put-u16be! v 6 (logior (logand (ash ts -48) #x0fff)
                              #x1000))    ; Version 1
      (put-u16be! v 8 (logior cs #x8000)) ; Variant 10
      (put-u16be! v 10 (ash nid -32))
      (put-u32be! v 12 (logand nid #xffffffff)))
    (%make-uuid v)))

(define (uuid4)
  (let1 v (make-u8vector 16)
    (put-u32be! v 0  (%uuid-random-int (ash 1 32)))
    (put-u32be! v 4  (logior (logand (%uuid-random-int (ash 1 32))
                                     #xffff0fff)
                             #x4000))  ;Verson 4
    (put-u32be! v 8  (copy-bit-field (%uuid-random-int (ash 1 32))
                                     2 30 32)) ;Variant 10
    (put-u32be! v 12 (%uuid-random-int (ash 1 32)))
    (%make-uuid v)))

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
