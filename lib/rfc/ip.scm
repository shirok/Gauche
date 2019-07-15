;;;
;;; rfc.ip - Internet Protocol
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

;; EXPERIMENTAL

(define-module rfc.ip
  (use gauche.uvector)
  (use binary.io)
  (use util.match)
  (use srfi-1)
  (use srfi-13)
  (export ip-version ip-header-length ip-protocol
          ip-source-address ip-destination-address
          ipv4-global-address?
          ))
(select-module rfc.ip)

;;============================================================
;; Packet accessors
;;

(define (ip-version packet offset)
  (ash (get-u8 packet offset) -4))

;; returns the final protocol and offset
(define (%ipv6-skip-header-extensions packet offset)
  (let loop ((nexthdr (get-u8 packet (+ 6 offset)))
             (off (+ 40 offset)))
    (if (memv nexthdr '(0        ; hop-by-hop options
                        43       ; routing options
                        60       ; destination options
                        ))
      (loop (get-u8 packet off)
            (+ off (* (+ (get-u8 packet (+ off 1)) 1) 8)))
      (values nexthdr (- off offset)))))

(define-macro (if-v4 packet offset v4 v6)
  `(case (ip-version ,packet ,offset)
     ((4) ,v4)
     ((6) ,v6)
     (else
      (errorf "unknown IP protocol version ~a in packet ~s"
              (ip-version ,packet ,offset) ,packet))))

(define (ip-header-length packet offset)
  (if-v4 packet offset
         (* (logand (get-u8 packet offset) #x0f) 4)
         (values-ref (%ipv6-skip-header-extensions packet offset) 1)))

(define (ip-protocol packet offset)
  (if-v4 packet offset
         (get-u8 packet (+ 9 offset))
         (values-ref (%ipv6-skip-header-extensions packet offset) 0)))

;; These may allocate.  Should we have non-allocation version?
(define (ip-source-address packet offset)
  (if-v4 packet offset
         (get-u32be packet (+ 12 offset))
         (+ (ash (get-u64be packet (+ 8 offset)) 64)
            (get-u64be packet (+ 16 offset)))))

(define (ip-destination-address packet offset)
  (if-v4 packet offset
         (get-u32be packet (+ 16 offset))
         (+ (ash (get-u64be packet (+ 24 offset)) 64)
            (get-u64be packet (+ 32 offset)))))

;;============================================================
;; Adderss utility
;;

;; addr :: Integer
;;  http://www.iana.org/assignments/ipv4-address-space/
(define (ipv4-global-address? addr)
  (let ((x/24 (ash addr -8))
        (x/16 (ash addr -16))
        (x/12 (ash addr -20))
        (x/8  (ash addr -24)))
    (not (or (memv x/8  '(0           ; 0.0.0.0/8 self-identification
                          10          ; 10.0.0.0/8 private
                          127))       ; 127.0.0.0/8 loopback
             (eqv? x/12 #xac1)        ; 172.16.0.0/12 private
             (memv x/16 '(#xa9fe      ; 169.254.0.0/16 link local
                          #xc0a8      ; 192.168.0.0/16 private
                          #xc612      ; 198.18.0.0/15 benchmark
                          #xc613))    ; 198.18.0.0/15 benchmark
             (memv x/24 '(#xc00002    ; 192.0.2.0/24 test-net
                          #xc05863))) ; 192.88.99.0/24 6to4 anycast relay
         )))

