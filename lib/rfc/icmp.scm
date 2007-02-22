;;;
;;; rfc.icmp - Internet Control Message Protocol
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
;;;  $Id: icmp.scm,v 1.3 2007-02-22 01:36:19 shirok Exp $
;;;


;; EXPERIMENTAL

;; RFC 792  Internet Control Message Protocol
;; RFC 1256 ICMP Router Discovery Messages
;; RFC 1393 Traceroute Using an IP Option
;; RFC 1788 ICMP Domain Name Messages
;; RFC 1812 Requirements for IP Version 4 Routers
;; RFC 2002 IP Mobility Supprot
;; RFC 2463 ICMPv6
;; RFC 2521 ICMP Security Failures Messages
;; Also useful:
;;  http://www.iana.org/assignments/icmp-parameters

(define-module rfc.icmp
  (use gauche.uvector)
  (use gauche.net)
  (use binary.io)
  (export icmp-message-type->string
          ICMP_ECHOREPLY ICMP_DEST_UNREACH ICMP_SOURCE_QUENCH
          ICMP_REDIRECT ICMP_ECHO ICMP_ROUTER_ADVERT ICMP_ROUTER_SOLICIT
          ICMP_TIME_EXCEEDED ICMP_PARAMETERPROB ICMP_TIMESTAMP
          ICMP_TIMESTAMPREPLY ICMP_INFO_REQUEST ICMP_INFO_REPLY ICMP_ADDRESS
          ICMP_ADDRESSREPLY ICMP_TRACEROUTE ICMP_DGRAMCONVERROR
          ICMP_DOMAIN_REQUEST ICMP_DOMAIN_REPLY ICMP_SECURITYFAILURE

          icmp-unreach-code->string
          ICMP_NET_UNREACH ICMP_HOST_UNREACH ICMP_PROT_UNREACH
          ICMP_PORT_UNREACH ICMP_FRAG_NEEDED ICMP_SR_FAILED ICMP_NET_UNKNOWN
          ICMP_HOST_UNKNOWN ICMP_HOST_ISOLATED ICMP_NET_ANO ICMP_HOST_ANO
          ICMP_NET_UNR_TOS ICMP_HOST_UNR_TOS ICMP_PKT_FILTERED
          ICMP_PREC_VIOLATION ICMP_PREC_CUTOFF

          icmp-redirect-code->string
          ICMP_REDIR_NET ICMP_REDIR_HOST ICMP_REDIR_NETTOS ICMP_REDIR_HOSTTOS

          icmp-router-code->string
          ICMP_ROUTER_NORMAL ICMP_ROUTER_NOCOMMON

          icmp-exceeded-code->string
          ICMP_EXC_TTL ICMP_EXC_FRAGTIME

          icmp-parameter-code->string
          ICMP_PARAM_PTR ICMP_PARAM_MISSING ICMP_PARAM_BADLENGTH

          icmp-security-code->string
          ICMP_SEC_BADSPI ICMP_SEC_AUTHFAILED ICMP_SEC_DECOMPFAILED
          ICMP_SEC_DECRYPTFAILED ICMP_SEC_NEEDAUTHENTICATION
          ICMP_SEC_NEEDAUTHORIZATION

          icmp-fill-header! icmp-checksum icmp-fill-checksum!
          icmp-fill-echo!

          icmp-packet-type icmp-packet-code
          icmp-echo-ident icmp-echo-sequence
          icmp-describe-packet
          ))
(select-module rfc.icmp)

;;============================================================
;; Constants
;;
;;  NB: it seems that these symbols in C slightly differ among the 
;;  underlying ICMP library, e.g. between GNU/Linux and BSD.
;;  I follow the GNU convention, for it is a bit more descriptive.

(define-syntax define-named-code
  (syntax-rules ()
    ((_ name ->string (sym val expl) ...)
     (begin
       (define-constant sym val) ...
       (define (->string c)
         (case c
           ((val) expl) ...
           (else (format "Unknown code ~s for ~a" c name))))))))

;; ICMP message types.
(define-named-code "ICMP message types"
  icmp-message-type->string
  (ICMP_ECHOREPLY          0   "Echo Reply")
  (ICMP_DEST_UNREACH       3   "Destination Unreachable")
  (ICMP_SOURCE_QUENCH      4   "Source Quench      ")
  (ICMP_REDIRECT           5   "Redirect (change route)")
  (ICMP_ECHO               8   "Echo Request")
  (ICMP_ROUTER_ADVERT      9   "Router Advertisement")
  (ICMP_ROUTER_SOLICIT     10  "Router solicitation")
  (ICMP_TIME_EXCEEDED      11  "Time Exceeded")
  (ICMP_PARAMETERPROB      12  "Parameter Problem")
  (ICMP_TIMESTAMP          13  "Timestamp Request")
  (ICMP_TIMESTAMPREPLY     14  "Timestamp Reply")
  (ICMP_INFO_REQUEST       15  "Information Request")
  (ICMP_INFO_REPLY         16  "Information Reply")
  (ICMP_ADDRESS            17  "Address Mask Request")
  (ICMP_ADDRESSREPLY       18  "Address Mask Reply")
  (ICMP_TRACEROUTE         30  "Traceroute")
  (ICMP_DGRAMCONVERROR     31  "Datagram Conversion Error")
  (ICMP_DOMAIN_REQUEST     37  "Domain Name Request")
  (ICMP_DOMAIN_REPLY       38  "Domain Name Reply")
  (ICMP_SECURITYFAILURE    40  "Security Failures")
  )

;; Codes
(define-named-code "ICMP UNREACH codes"
  icmp-unreach-code->string
  (ICMP_NET_UNREACH        0   "Network Unreachable")
  (ICMP_HOST_UNREACH       1   "Host Unreachable")
  (ICMP_PROT_UNREACH       2   "Protocol Unreachable")
  (ICMP_PORT_UNREACH       3   "Port Unreachable")
  (ICMP_FRAG_NEEDED        4   "Fragmentation Needed/DF set")
  (ICMP_SR_FAILED          5   "Source Route failed")
  (ICMP_NET_UNKNOWN        6   "Unknown Net")
  (ICMP_HOST_UNKNOWN       7   "Unknown Host")
  (ICMP_HOST_ISOLATED      8   "Source Host Isolated")
  (ICMP_NET_ANO            9   "Dest Network Prohibited")
  (ICMP_HOST_ANO           10  "Dest Host Prohibited")
  (ICMP_NET_UNR_TOS        11  "Dest Netowork Unreachable for TOS")
  (ICMP_HOST_UNR_TOS       12  "Dest Host Unreachable for TOS")
  (ICMP_PKT_FILTERED       13  "Packet Filtered")
  (ICMP_PREC_VIOLATION     14  "Precedence Violation")
  (ICMP_PREC_CUTOFF        15  "Precedence Cut Off")
  )

(define-named-code "ICMP REDIRECT codes"
  icmp-redirect-code->string
  (ICMP_REDIR_NET          0   "Redirect Net")
  (ICMP_REDIR_HOST         1   "Redirect Host")
  (ICMP_REDIR_NETTOS       2   "Redirect Net for TOS")
  (ICMP_REDIR_HOSTTOS      3   "Redirect Host for TOS")
  )

(define-named-code "ICMP ROUTER ADVERTISE codes"
  icmp-router-code->string
  (ICMP_ROUTER_NORMAL      0   "Normal Router Advertisement")
  (ICMP_ROUTER_NOCOMMON    16  "Does not route common traffic")
  )

(define-named-code "ICMP TIME EXCEEDED codes"
  icmp-exceeded-code->string
  (ICMP_EXC_TTL            0   "TTL count exceeded")
  (ICMP_EXC_FRAGTIME       1   "Fragment Reass time exceeded")
  )

(define-named-code "ICMP PARAMETER PROBLEM codes"
  icmp-parameter-code->string
  (ICMP_PARAM_PTR          0   "Pointer indicates the error")
  (ICMP_PARAM_MISSING      1   "Missing Required Option")
  (ICMP_PARAM_BADLENGTH    2   "Bad Length")
  )

(define-named-code "ICMP SECURITY FAILURE codes"
  icmp-security-code->string
  (ICMP_SEC_BADSPI         0   "Bad SPI")
  (ICMP_SEC_AUTHFAILED     1   "Authentication Failed")
  (ICMP_SEC_DECOMPFAILED   2   "Decompression Failed")
  (ICMP_SEC_DECRYPTFAILED  3   "Decryption Failed")
  (ICMP_SEC_NEEDAUTHENTICATION 4 "Need Authentication")
  (ICMP_SEC_NEEDAUTHORIZATION  5 "Need Authorization")
  )

;;============================================================
;; Packet construction
;;

;; The low-level 'fill' routines operates on pre-allocated u8vector
;; as buffer.  They don't allocate, so that they can be used
;; in inner loops.

;; Fill ICMP packet header (except checksum)
(define-inline (icmp-fill-header! buf type code)
  (put-u8! buf 0 type)
  (put-u8! buf 1 code)
  (put-u16be! buf 2 0))

(define (icmp-fill-checksum! buf size)
  (put-u16be! buf 2 (inet-checksum buf size)))

(define (icmp-fill-echo! buf ident seq data)
  (icmp-fill-header! buf ICMP_ECHO 0)
  (put-u16be! buf 4 ident)
  (put-u16be! buf 6 seq)
  (u8vector-copy! buf 8 data)
  (icmp-fill-checksum! buf (+ (u8vector-length data) 8)))

;;============================================================
;; Packet decomposition
;;

(define (icmp-packet-type buf offset)
  (get-u8 buf (+ 0 offset)))

(define (icmp-packet-code buf offset)
  (get-u8 buf (+ 1 offset)))

(define (icmp-echo-ident buf offset)
  (get-u16be buf (+ 4 offset)))

(define (icmp-echo-sequence buf offset)
  (get-u16be buf (+ 6 offset)))

(define (icmp-describe-packet buf offset)
  (let ((type (icmp-packet-type buf offset))
        (code (icmp-packet-code buf offset)))
    (format #t "ICMP packet type=~a(~a)\n"
            type (icmp-message-type->string type))
    (cond
     ((= type ICMP_ECHOREPLY)
      (format #t "  ident=~2,'0x seq=~2,'0x\n"
              (icmp-echo-ident buf offset) (icmp-echo-sequence buf offset)))
     ((assv type `((,ICMP_DEST_UNREACH . ,icmp-unreach-code->string)
                   (,ICMP_REDIRECT     . ,icmp-redirect-code->string)
                   (,ICMP_ROUTER_ADVERT . ,icmp-router-code->string)
                   (,ICMP_TYPE_EXCEEDED . ,icmp-exceeded-code->string)
                   (,ICMP_PARAMETERPROB . ,icmp-parameter-code->string)
                   (,ICMP_SECURITYFAILURE . ,icmp-security-code->string)))
      => (lambda (p)
           (format #t "  code=~a(~a)\n" code ((cdr p) code))))
     )))

;; TODO: ICMPv6 support

(provide "rfc/icmp")
