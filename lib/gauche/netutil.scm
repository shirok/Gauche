;;;
;;; netutil.scm - Auxiliary network utilities
;;;
;;;   Copyright (c) 2000-2021  Shiro Kawai  <shiro@acm.org>
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

;; This file is autoloaded
(select-module gauche.net)

;; Although many systems support this feature (e.g. inet_ntop/inet_pton
;; or WSAAdressToString/WSAStringToAddress), it would be too cumbersome
;; to check availability of those and switch the implementation.  So we
;; provide them in Scheme.

;; API
(define-method sockaddr-name ((addr <sockaddr-in>))
  #"~(inet-address->string (sockaddr-addr addr) AF_INET):~(sockaddr-port addr)")

;; NB: this should be conditionally defined by cond-expand at compile-time,
;; instead of load-time dispatch.  We need to clean up cond feature management
;; more to do so.
(if (global-variable-bound? (find-module 'gauche.net) '<sockaddr-in6>)
  ;; API
  (define-method sockaddr-name ((addr <sockaddr-in6>))
    #"[~(inet-address->string (sockaddr-addr addr) AF_INET6)]:~(sockaddr-port addr)"))

;; connection protocol
(define-method connection-self-address ((s <socket>))
  (socket-getsockname s))
(define-method connection-peer-address ((s <socket>))
  (socket-getpeername s))
(define-method connection-input-port ((s <socket>))
  (socket-input-port s))
(define-method connection-output-port ((s <socket>))
  (socket-output-port s))
(define-method connection-shutdown ((s <socket>) how)
  (socket-shutdown s (ecase how
                       [(read)  SHUT_RD]
                       [(write) SHUT_WR]
                       [(both)  SHUT_RDWR])))
(define-method connection-close ((s <socket>))
  (socket-close s))
(define-method connection-address-name ((a <sockaddr>))
  (sockaddr-name a))