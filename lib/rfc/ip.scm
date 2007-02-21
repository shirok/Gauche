;;;
;;; rfc.ip - Internet Protocol
;;;  
;;;   Copyright (c) 2007 Shiro Kawai, All rights reserved.
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
;;;  $Id: ip.scm,v 1.1 2007-02-21 22:27:38 shirok Exp $
;;;


;; EXPERIMENTAL

(define-module rfc.ip
  (use gauche.uvector)
  (use binary.io)
  (export ip-version ip-header-length ip-protocol))
(select-module rfc.ip)

;;============================================================
;; Constants
;;

;;============================================================
;; Packet accessors
;;

(define (ip-version packet)
  (ash (get-u8 packet 0) -4))

(define (ip-header-length packet)
  (case (ip-version packet)
    ((4) (* (logand (get-u8 packet 0) #x0f) 5))
    ((6) 40)
    (else
     (errorf "unknown IP protocol version ~a in packet ~s"
             (ip-version packet) packet))))

(define (ip-protocol packet)
  (case (ip-version packet)
    ((4) (get-u8 packet 9))
    ((6) (let loop ((nexthdr (get-u8 packet 6))
                    (off 40))
           (if (memv nexthdr '(0        ; hop-by-hop options
                               43       ; routing options
                               60       ; destination options
                               ))
             (loop (get-u8 packet off)
                   (+ off (* (+ (get-u8 packet (+ off 1)) 1) 8)))
             nexthdr)))
    (else
     (errorf "unknown IP protocol version ~a in packet ~s"
             (ip-version packet) packet))))


(provide "rfc/ip")
