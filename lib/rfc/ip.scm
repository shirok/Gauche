;;;
;;; rfc.ip - Internet Protocol
;;;  
;;;   Copyright (c) 2007  Shiro Kawai  <shiro@acm.org>
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
;;;  $Id: ip.scm,v 1.4 2007-03-02 07:39:10 shirok Exp $
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
          ip-parse-address ip-parse-address!))
(select-module rfc.ip)

;;============================================================
;; Constants
;;

;;============================================================
;; IP sddress utilities
;;

;; IP address parser.  Can deal with both v4 and v6 addresses.
;; Two variations: ip-parse-address returns an integer address
;; and version; ip-parse-address! fills the given uvector with
;; parsed address and returns a version.
;; We could use <sockaddr-in> and <sockaddr-in6>, giving STRING
;; to :host argument in the constructor and extract the address
;; value, but the host argument also accepts hostnames, which we
;; want to avoid.  So we parse the address by ourselves.

(define (ip-parse-address string)
  (receive (ns vers) (%ip-parse-address string)
    (case vers
      ((4) (values (%fold-addr-to-integer ns 8) 4))
      ((6) (values (%fold-addr-to-integer ns 16) 6))
      (else (values #f #f)))))

(define (ip-parse-address! string uv)
  (receive (ns vers) (%ip-parse-address string)
    (case vers
      ((4) (%fill-addr-to-buf! ns uv put-u8! 1) 4)
      ((6) (%fill-addr-to-buf! ns uv put-u16be! 2) 6)
      (else #f))))

(define (%ip-parse-address string)
  (define (try-ipv6 s)
    (let loop ((ss (map (lambda (s) (or (string-null? s) s))
                        (string-split s #\:)))
               (r '())
               (abbrev? #f))
      (match ss
        ((#t #t)                        ; ends with '::'
         (and (not abbrev?)
              (finish-ipv6 (reverse (cons '* r)) #t)))
        ((#t #t . rest)                 ; begins with '::'
         (and (null? r)
              (loop rest '(*) #t)))
        ((#t . rest)                    ; '::' in the middle
         (and (not (null? r))
              (not abbrev?)
              (loop rest (cons '* r) #t)))
        ((part)                         ; end
         (cond
          ((try-ipv4 part) =>           ;   ipv6-mapped-ipv4
           (lambda (ns)
             (finish-ipv6 (reverse (cons* (+ (* (caddr ns) 256) (cadddr ns))
                                          (+ (* (car ns) 256) (cadr ns))
                                          r))
                          abbrev?)))
          ((string->number part 16) =>
           (lambda (x) (finish-ipv6 (reverse (cons x r)) abbrev?)))
          (else #f)))
        ((part . rest)
         (and-let* ((x (string->number part 16))
                    ( (<= 0 x 65535) ))
           (loop rest (cons x r) abbrev?)))
        (_ #f))))
  (define (finish-ipv6 parts abbrev?)
    (if abbrev?
      (and-let* ((zeropart-length (- 8 (length parts) -1))
                 ( (< 0 zeropart-length 8) )
                 (zeropart (make-list zeropart-length 0)))
        (receive (pre post) (break (cut eq? <> '*) parts)
          (append pre zeropart (cdr post))))
      (and (= (length parts) 8) parts)))
  (define (try-ipv4 s)
    (let1 ss (string-split s #\.)
      (and (= (length ss) 4)
           (let1 ns (map string->number ss)
             (and (every (lambda (n) (and n (<= 0 n 255))) ns)
                  ns)))))

  (cond
   ((try-ipv6 string) => (cut values <> 6))
   ((try-ipv4 string) => (cut values <> 4))
   (else (values #f #f))))

(define (%fold-addr-to-integer ns digbits)
  (let loop ((shift (* (- (length ns) 1) digbits))
             (ns ns)
             (v  0))
    (if (null? ns)
      v
      (loop (- shift digbits) (cdr ns) (logior (ash (car ns) shift) v)))))

(define (%fill-addr-to-buf! ns buf filler incr)
  (fold (lambda (n pos) (filler buf pos n) (+ pos incr)) 0 ns))

;;============================================================
;; Packet accessors
;;

(define (ip-version packet)
  (ash (get-u8 packet 0) -4))

;; returns the final protocl and offset
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
