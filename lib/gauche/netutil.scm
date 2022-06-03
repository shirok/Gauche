;;;
;;; netutil.scm - Auxiliary network utilities
;;;
;;;   Copyright (c) 2000-2022  Shiro Kawai  <shiro@acm.org>
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

;;
;; Connection protocol
;;
;; <connection> class is built-in.
;; It defines a generic interface of a connection capable of
;; bidirectional communication to another entity.
;;
;; The data communication is abstracted as a pair of input and
;; output ports.  They can be accessed by connection-input-port
;; and connection-output-port.
;;
;; A connected connection has two endpoints; self and peer.  Each
;; endpoint has an address.  We don't have any assumption on address,
;; except that it must respond to connection-address-name method.
;; For sockets, addresses are subclass of <sockaddr>.
;; If the connection allows unconnected state, the endpoint
;; addresses can be #f.
;;
;; Two methods to finish up the connection; connection-shutdown
;; breaks the connection to the peer---you can shutdown just one
;; of reading or writing channel, or both.  The connection-close
;; method destroys the resources in our side.  Note that close doesn't
;; imply shutdown---if you fork the process after establishing the
;; connection, you might want to close the endpoint in one process
;; but still want co keep communication from another process.
;;
;; We don't provide a generic interface of establishing connections;
;; that would vary greatly depending on the actual underlying mechanism,
;; and must be handled with such knowledge.  Connection interface
;; is useful for generic code that deals with established connections.


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
;; for the convenience
(define-method connection-shutdown ((c <connection>))
  (connection-shutdown c 'both))

(define-method connection-close ((s <socket>))
  (socket-close s))

(define-method connection-address-name ((a <sockaddr>))
  (sockaddr-name a))
(define-method connection-address-name ((a <top>))
  (write-to-string a display))
