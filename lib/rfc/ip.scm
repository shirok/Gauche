;;;
;;; rfc.ip - Internet Protocol
;;;  
;;;   Copyright (c) 2007-2008  Shiro Kawai  <shiro@acm.org>
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
;;;  $Id: ip.scm,v 1.7 2008-05-10 13:36:08 shirok Exp $
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
          ))
(select-module rfc.ip)

;;============================================================
;; Packet accessors
;;

(define (ip-version packet)
  (ash (get-u8 packet 0) -4))

;; returns the final protocol and offset
(define (%ipv6-skip-header-extensions packet)
  (let loop ((nexthdr (get-u8 packet 6))
             (off 40))
    (if (memv nexthdr '(0        ; hop-by-hop options
                        43       ; routing options
                        60       ; destination options
                        ))
      (loop (get-u8 packet off)
            (+ off (* (+ (get-u8 packet (+ off 1)) 1) 8)))
      (values nexthdr off))))

(define-macro (if-v4 packet v4 v6)
  `(case (ip-version ,packet)
     ((4) ,v4)
     ((6) ,v6)
     (else 
      (errorf "unknown IP protocol version ~a in packet ~s"
              (ip-version ,packet) ,packet))))
  
(define (ip-header-length packet)
  (if-v4 packet
         (* (logand (get-u8 packet 0) #x0f) 4)
         (values-ref (%ipv6-skip-header-extensions packet) 1)))

(define (ip-protocol packet)
  (if-v4 packet
         (get-u8 packet 9)
         (values-ref (%ipv6-skip-header-extensions packet) 0)))

;; These may allocate.  Should we have non-allocation version?
(define (ip-source-address packet)
  (if-v4 packet
         (get-u32be packet 12)
         (+ (ash (get-u64be packet 8) 64)
            (get-u64be packet 16))))

(define (ip-destination-address packet)
  (if-v4 packet
         (get-u32be packet 16)
         (+ (ash (get-u64be packet 24) 64)
            (get-u64be packet 32))))

(provide "rfc/ip")
