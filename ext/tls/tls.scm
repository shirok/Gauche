;;;
;;; tls - TLS secure connection interface
;;;
;;;   Copyright (c) 2011 Kirill Zorin <k.zorin@me.com>
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

#!no-fold-case

(define-module rfc.tls
  (use gauche.vport)
  (export <tls> make-tls tls-destroy tls-connect tls-accept tls-close
          tls-load-object tls-read tls-write
          tls-input-port tls-output-port
          tls-ca-bundle-path
          default-tls-class

          SSL_SERVER_VERIFY_LATER SSL_CLIENT_AUTHENTICATION
          SSL_DISPLAY_BYTES SSL_DISPLAY_STATES SSL_DISPLAY_CERTS
          SSL_DISPLAY_RSA SSL_CONNECT_IN_PARTS SSL_NO_DEFAULT_KEY
          SSL_OBJ_X509_CERT SSL_OBJ_X509_CACERT SSL_OBJ_RSA_KEY
          SSL_OBJ_PKCS8 SSL_OBJ_PKCS12)
  )
(select-module rfc.tls)

(export-if-defined <ax-tls>)

(without-precompiling
 (cond-expand
  [gauche.net.tls.mbedtls 
   (autoload rfc.tls.mbed <mbed-tls>)
   (export <mbed-tls>)]
  [else]))

(inline-stub
 (declcode "#include \"gauche-tls.h\" ")

 (define-type <tls> "ScmTLS*")

 (define-enum SSL_SERVER_VERIFY_LATER)
 (define-enum SSL_CLIENT_AUTHENTICATION)
 (define-enum SSL_NO_DEFAULT_KEY)
 (define-enum SSL_DISPLAY_BYTES)
 (define-enum SSL_DISPLAY_STATES)
 (define-enum SSL_DISPLAY_CERTS)
 (define-enum SSL_DISPLAY_RSA)
 (define-enum SSL_CONNECT_IN_PARTS)
 (define-enum SSL_OBJ_X509_CERT)
 (define-enum SSL_OBJ_X509_CACERT)
 (define-enum SSL_OBJ_RSA_KEY)
 (define-enum SSL_OBJ_PKCS8)
 (define-enum SSL_OBJ_PKCS12)
 
 (define-cproc make-tls (:rest initargs) Scm_MakeTLS)
 (define-cproc tls-load-object (tls::<tls> obj-type filename::<const-cstring>
                                           :optional (password::<const-cstring>? #f)) Scm_TLSLoadObject)
 (define-cproc tls-destroy (tls::<tls>) Scm_TLSDestroy)
 (define-cproc %tls-connect (tls::<tls> fd::<long>) Scm_TLSConnect)
 (define-cproc %tls-accept (tls::<tls> fd::<long>) Scm_TLSAccept)
 (define-cproc %tls-close (tls::<tls>) Scm_TLSClose)
 (define-cproc tls-read (tls::<tls>) Scm_TLSRead)
 (define-cproc tls-write (tls::<tls> msg) Scm_TLSWrite)
 (define-cproc tls-input-port (tls::<tls>) Scm_TLSInputPort)
 (define-cproc tls-output-port (tls::<tls>) Scm_TLSOutputPort)
 ;; internal
 (define-cproc tls-input-port-set! (tls::<tls> port) Scm_TLSInputPortSet)
 (define-cproc tls-output-port-set! (tls::<tls> port) Scm_TLSOutputPortSet)

 (declcode "void Scm_Init_tls(ScmModule *);")
 (initcode "Scm_Init_tls(Scm_CurrentModule());")
 )

;; API
(define (tls-connect tls fd)
  (%tls-connect tls fd) ;; done before ports in case of connect failure.
  (tls-input-port-set! tls (make-tls-input-port tls))
  (tls-output-port-set! tls (make-tls-output-port tls))
  tls)

;; API
(define (tls-accept tls fd)
  (%tls-accept tls fd) ;; done before ports in case of connect failure.
  (tls-input-port-set! tls (make-tls-input-port tls))
  (tls-output-port-set! tls (make-tls-output-port tls))
  tls)

;; API
(define (tls-close t)
  (when (input-port? (tls-input-port t))
    (close-input-port (tls-input-port t)))
  (when (output-port? (tls-output-port t))
    (close-output-port (tls-output-port t)))
  (%tls-close t))

(define (make-tls-input-port tls)
  (rlet1 ip (make <virtual-input-port>)
    (set! (~ ip'getb)
          (let ((buf #f) (pos 0) (size 0))
            (^[]
              (unless buf
                (set! buf (tls-read tls))
                (set! size (string-size buf))
                (set! pos 0))
              (rlet1 r (string-byte-ref buf pos)
                (set! pos (+ pos 1))
                (when (= pos size) (set! buf #f))))))))

(define (make-tls-output-port tls)
  (rlet1 op (make <virtual-output-port>)
    (set! (~ op'puts) (^[msg] (tls-write tls msg)))
    (set! (~ op'putb) (^[b] (tls-write tls (make-byte-string 1 b))))))
