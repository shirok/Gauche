;;;
;;; rfc.icmp - Internet Control Message Protocol
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
  (export icmp4-message-type->string
          ICMP4_ECHOREPLY ICMP4_DEST_UNREACH ICMP4_SOURCE_QUENCH
          ICMP4_REDIRECT ICMP4_ECHO ICMP4_ROUTER_ADVERT ICMP4_ROUTER_SOLICIT
          ICMP4_TIME_EXCEEDED ICMP4_PARAMETERPROB ICMP4_TIMESTAMP
          ICMP4_TIMESTAMPREPLY ICMP4_INFO_REQUEST ICMP4_INFO_REPLY
          ICMP4_ADDRESS ICMP4_ADDRESSREPLY ICMP4_TRACEROUTE
          ICMP4_DGRAMCONVERROR ICMP4_DOMAIN_REQUEST ICMP4_DOMAIN_REPLY
          ICMP4_SECURITYFAILURE

          icmp4-unreach-code->string
          ICMP4_NET_UNREACH ICMP4_HOST_UNREACH ICMP4_PROT_UNREACH
          ICMP4_PORT_UNREACH ICMP4_FRAG_NEEDED ICMP4_SR_FAILED
          ICMP4_NET_UNKNOWN ICMP4_HOST_UNKNOWN ICMP4_HOST_ISOLATED
          ICMP4_NET_ANO ICMP4_HOST_ANO ICMP4_NET_UNR_TOS ICMP4_HOST_UNR_TOS
          ICMP4_PKT_FILTERED ICMP4_PREC_VIOLATION ICMP4_PREC_CUTOFF

          icmp4-redirect-code->string
          ICMP4_REDIR_NET ICMP4_REDIR_HOST ICMP4_REDIR_NETTOS
          ICMP4_REDIR_HOSTTOS

          icmp4-router-code->string
          ICMP4_ROUTER_NORMAL ICMP4_ROUTER_NOCOMMON

          icmp4-exceeded-code->string
          ICMP4_EXC_TTL ICMP4_EXC_FRAGTIME

          icmp4-parameter-code->string
          ICMP4_PARAM_PTR ICMP4_PARAM_MISSING ICMP4_PARAM_BADLENGTH

          icmp4-security-code->string
          ICMP4_SEC_BADSPI ICMP4_SEC_AUTHFAILED ICMP4_SEC_DECOMPFAILED
          ICMP4_SEC_DECRYPTFAILED ICMP4_SEC_NEEDAUTHENTICATION
          ICMP4_SEC_NEEDAUTHORIZATION

          icmp6-message-type->string
          ICMP6_DEST_UNREACH ICMP6_PACKET_TOO_BIG ICMP6_TIME_EXCEEDED
          ICMP6_PARAMETERPROB ICMP6_ECHO ICMP6_ECHOREPLY

          icmp6-unreach-code->string
          ICMP6_UNREACH_NOROUTE ICMP6_UNREACH_ADMIN ICMP6_UNREACH_BEYONDSCOPE
          ICMP6_UNREACH_ADDR ICMP6_UNREACH_NOPORT

          icmp6-exceeded-code->string
          ICMP6_EXC_TRANSIT ICMP6_EXC_REASSEMBLY

          icmp6-parameter-code->string
          ICMP6_PARAM_HEADER ICMP6_PARAM_NEXTHEADER ICMP6_PARAM_OPTION

          icmp-fill-header!
          icmp4-fill-checksum! icmp4-fill-echo!
          icmp6-fill-echo!

          icmp-packet-type icmp-packet-code
          icmp-echo-ident icmp-echo-sequence
          icmp4-describe-packet icmp6-describe-packet
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

;;-------------------------------------------------------------
;; ICMPv4
;;

;; ICMP message types.
(define-named-code "ICMP message types"
  icmp4-message-type->string
  (ICMP4_ECHOREPLY          0   "Echo Reply")
  (ICMP4_DEST_UNREACH       3   "Destination Unreachable")
  (ICMP4_SOURCE_QUENCH      4   "Source Quench      ")
  (ICMP4_REDIRECT           5   "Redirect (change route)")
  (ICMP4_ECHO               8   "Echo Request")
  (ICMP4_ROUTER_ADVERT      9   "Router Advertisement")
  (ICMP4_ROUTER_SOLICIT     10  "Router solicitation")
  (ICMP4_TIME_EXCEEDED      11  "Time Exceeded")
  (ICMP4_PARAMETERPROB      12  "Parameter Problem")
  (ICMP4_TIMESTAMP          13  "Timestamp Request")
  (ICMP4_TIMESTAMPREPLY     14  "Timestamp Reply")
  (ICMP4_INFO_REQUEST       15  "Information Request")
  (ICMP4_INFO_REPLY         16  "Information Reply")
  (ICMP4_ADDRESS            17  "Address Mask Request")
  (ICMP4_ADDRESSREPLY       18  "Address Mask Reply")
  (ICMP4_TRACEROUTE         30  "Traceroute")
  (ICMP4_DGRAMCONVERROR     31  "Datagram Conversion Error")
  (ICMP4_DOMAIN_REQUEST     37  "Domain Name Request")
  (ICMP4_DOMAIN_REPLY       38  "Domain Name Reply")
  (ICMP4_SECURITYFAILURE    40  "Security Failures")
  )

;; Codes
(define-named-code "ICMP UNREACH codes"
  icmp4-unreach-code->string
  (ICMP4_NET_UNREACH        0   "Network Unreachable")
  (ICMP4_HOST_UNREACH       1   "Host Unreachable")
  (ICMP4_PROT_UNREACH       2   "Protocol Unreachable")
  (ICMP4_PORT_UNREACH       3   "Port Unreachable")
  (ICMP4_FRAG_NEEDED        4   "Fragmentation Needed/DF set")
  (ICMP4_SR_FAILED          5   "Source Route failed")
  (ICMP4_NET_UNKNOWN        6   "Unknown Net")
  (ICMP4_HOST_UNKNOWN       7   "Unknown Host")
  (ICMP4_HOST_ISOLATED      8   "Source Host Isolated")
  (ICMP4_NET_ANO            9   "Dest Network Prohibited")
  (ICMP4_HOST_ANO           10  "Dest Host Prohibited")
  (ICMP4_NET_UNR_TOS        11  "Dest Netowork Unreachable for TOS")
  (ICMP4_HOST_UNR_TOS       12  "Dest Host Unreachable for TOS")
  (ICMP4_PKT_FILTERED       13  "Packet Filtered")
  (ICMP4_PREC_VIOLATION     14  "Precedence Violation")
  (ICMP4_PREC_CUTOFF        15  "Precedence Cut Off")
  )

(define-named-code "ICMP REDIRECT codes"
  icmp4-redirect-code->string
  (ICMP4_REDIR_NET          0   "Redirect Net")
  (ICMP4_REDIR_HOST         1   "Redirect Host")
  (ICMP4_REDIR_NETTOS       2   "Redirect Net for TOS")
  (ICMP4_REDIR_HOSTTOS      3   "Redirect Host for TOS")
  )

(define-named-code "ICMP ROUTER ADVERTISE codes"
  icmp4-router-code->string
  (ICMP4_ROUTER_NORMAL      0   "Normal Router Advertisement")
  (ICMP4_ROUTER_NOCOMMON    16  "Does not route common traffic")
  )

(define-named-code "ICMP TIME EXCEEDED codes"
  icmp4-exceeded-code->string
  (ICMP4_EXC_TTL            0   "TTL count exceeded")
  (ICMP4_EXC_FRAGTIME       1   "Fragment Reass time exceeded")
  )

(define-named-code "ICMP PARAMETER PROBLEM codes"
  icmp4-parameter-code->string
  (ICMP4_PARAM_PTR          0   "Pointer indicates the error")
  (ICMP4_PARAM_MISSING      1   "Missing Required Option")
  (ICMP4_PARAM_BADLENGTH    2   "Bad Length")
  )

(define-named-code "ICMP SECURITY FAILURE codes"
  icmp4-security-code->string
  (ICMP4_SEC_BADSPI         0   "Bad SPI")
  (ICMP4_SEC_AUTHFAILED     1   "Authentication Failed")
  (ICMP4_SEC_DECOMPFAILED   2   "Decompression Failed")
  (ICMP4_SEC_DECRYPTFAILED  3   "Decryption Failed")
  (ICMP4_SEC_NEEDAUTHENTICATION 4 "Need Authentication")
  (ICMP4_SEC_NEEDAUTHORIZATION  5 "Need Authorization")
  )

;;-------------------------------------------------------------
;; ICMPv6
;;
(define-named-code "ICMP6 message types"
  icmp6-message-type->string
  (ICMP6_DEST_UNREACH      1   "Destination unreachable")
  (ICMP6_PACKET_TOO_BIG    2   "Packet Too Big")
  (ICMP6_TIME_EXCEEDED     3   "Time Exceeded")
  (ICMP6_PARAMETERPROB     4   "Parameter problem")

  (ICMP6_ECHO            128   "Echo request")
  (ICMP6_ECHOREPLY       129   "Echo reply")
  )

(define-named-code "ICMP6 UNREACH codes"
  icmp6-unreach-code->string
  (ICMP6_UNREACH_NOROUTE      0 "No route to destination")
  (ICMP6_UNREACH_ADMIN        1 "Communication administratively prohibited")
  (ICMP6_UNREACH_BEYONDSCOPE  2 "Beyond scope of source address")
  (ICMP6_UNREACH_ADDR         3 "Address unreachable")
  (ICMP6_UNREACH_NOPORT       4 "Bad port")
  )

(define-named-code "ICMP6 TIME EXCEEDED codes"
  icmp6-exceeded-code->string
  (ICMP6_EXC_TRANSIT          0 "Hop limit == 0 in transit")
  (ICMP6_EXC_REASSEMBLY       1 "Reassembly time out")
  )

(define-named-code "ICMP6 PARAMETER PROBLEM codes"
  icmp6-parameter-code->string
  (ICMP6_PARAM_HEADER         0 "Erroneous header field")
  (ICMP6_PARAM_NEXTHEADER     1 "Unrecognized next header")
  (ICMP6_PARAM_OPTION         2 "Unrecognized IPv6 option")
  )

;;============================================================
;; Packet construction
;;

;; The low-level 'fill' routines operates on pre-allocated u8vector
;; as buffer.  They don't allocate, so that they can be used
;; in inner loops.

;; Fill ICMP/ICMP6 packet header (except checksum)
(define-inline (icmp-fill-header! buf type code)
  (put-u8! buf 0 type)
  (put-u8! buf 1 code)
  (put-u16be! buf 2 0))

(define (icmp4-fill-checksum! buf size)
  (put-u16be! buf 2 (inet-checksum buf size)))

(define (icmp4-fill-echo! buf ident seq data)
  (icmp-fill-header! buf ICMP4_ECHO 0)
  (put-u16be! buf 4 ident)
  (put-u16be! buf 6 seq)
  (u8vector-copy! buf 8 data)
  (icmp4-fill-checksum! buf (+ (u8vector-length data) 8)))

;; We don't need to fill out checksum fields, for the kernel calculates it.
(define (icmp6-fill-echo! buf ident seq data)
  (icmp-fill-header! buf ICMP6_ECHO 0)
  (put-u16be! buf 4 ident)
  (put-u16be! buf 6 seq)
  (u8vector-copy! buf 8 data))

;;============================================================
;; Packet decomposition
;;

;; common to v4/v6
(define (icmp-packet-type buf offset)
  (get-u8 buf (+ 0 offset)))

;; common to v4/v6
(define (icmp-packet-code buf offset)
  (get-u8 buf (+ 1 offset)))

;; common to v4/v6
(define (icmp-echo-ident buf offset)
  (get-u16be buf (+ 4 offset)))

;; common to v4/v6
(define (icmp-echo-sequence buf offset)
  (get-u16be buf (+ 6 offset)))

(define (icmp4-describe-packet buf offset)
  (let ((type (icmp-packet-type buf offset))
        (code (icmp-packet-code buf offset)))
    (format #t "ICMP packet type=~a(~a)\n"
            type (icmp4-message-type->string type))
    (cond
     ((memv type `(,ICMP4_ECHOREPLY ,ICMP4_ECHO))
      (format #t "  ident=~2,'0x seq=~2,'0x\n"
              (icmp-echo-ident buf offset) (icmp-echo-sequence buf offset)))
     ((assv type `((,ICMP4_DEST_UNREACH . ,icmp4-unreach-code->string)
                   (,ICMP4_REDIRECT     . ,icmp4-redirect-code->string)
                   (,ICMP4_ROUTER_ADVERT . ,icmp4-router-code->string)
                   (,ICMP4_TIME_EXCEEDED . ,icmp4-exceeded-code->string)
                   (,ICMP4_PARAMETERPROB . ,icmp4-parameter-code->string)
                   (,ICMP4_SECURITYFAILURE . ,icmp4-security-code->string)))
      => (lambda (p)
           (format #t "  code=~a(~a)\n" code ((cdr p) code))))
     )))

(define (icmp6-describe-packet buf offset)
  (let ((type (icmp-packet-type buf offset))
        (code (icmp-packet-code buf offset)))
    (format #t "ICMPv6 packet type=~a(~a)\n"
            type (icmp6-message-type->string type))
    (cond
     ((= type ICMP6_ECHOREPLY)
      (format #t "  ident=~2,'0x seq=~2,'0x\n"
              (icmp-echo-ident buf offset) (icmp-echo-sequence buf offset)))
     ((assv type `((,ICMP6_DEST_UNREACH . ,icmp6-unreach-code->string)
                   (,ICMP6_TIME_EXCEEDED . ,icmp6-exceeded-code->string)
                   (,ICMP6_PARAMETERPROB . ,icmp6-parameter-code->string)))
      => (lambda (p)
           (format #t "  code=~a(~a)\n" code ((cdr p) code))))
     )))

