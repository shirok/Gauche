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
  (use gauche.connection)
  (use gauche.net)
  (use util.match)
  (export <tls> make-tls tls-connect
          tls-bind tls-accept tls-poll tls-close
          tls-load-certificate tls-load-private-key
          tls-read tls-write
          tls-input-port tls-output-port
          tls-ca-bundle-path tls-debug-level-set!
          default-tls-class

          ;; connection interface
          connection-self-address connection-peer-address
          connection-input-port connection-output-port
          connection-shutdown connection-close

          ;; deprecated interface
          SSL_SERVER_VERIFY_LATER SSL_CLIENT_AUTHENTICATION
          SSL_DISPLAY_BYTES SSL_DISPLAY_STATES SSL_DISPLAY_CERTS
          SSL_DISPLAY_RSA SSL_CONNECT_IN_PARTS SSL_NO_DEFAULT_KEY
          SSL_OBJ_X509_CERT SSL_OBJ_X509_CACERT SSL_OBJ_RSA_KEY
          SSL_OBJ_PKCS8 SSL_OBJ_PKCS12
          tls-load-object tls-destroy
)
  )
(select-module rfc.tls)

;; The initialization of default-tls-class depends on the availability
;; of classes and tls-ca-bundle-path.

(without-precompiling
 (cond-expand
  [gauche.net.tls.mbedtls
   ;; Set rfc.tls.mbed to be autoladed when <mbed-tls> is used.
   (autoload rfc.tls.mbed <mbed-tls>)
   (export <mbed-tls>)
   (when (eq? (tls-ca-bundle-path) 'check)
     (if (%tls-system-ca-bundle-available?)
       (tls-ca-bundle-path 'system)
       (tls-ca-bundle-path #f)))
   (default-tls-class (delay <mbed-tls>))
   ]
  [else
   (when (eq? (tls-ca-bundle-path) 'check)
     (tls-ca-bundle-path #f))
   (default-tls-class #f)]))

(inline-stub
 (declcode
  (.include "gauche-tls.h"))

 (declare-stub-type <tls> "ScmTLS*")

 (define-cproc make-tls (:rest initargs) Scm_MakeTLS)
 (define-cproc tls-load-certificate (tls::<tls> filename::<const-cstring>)
   Scm_TLSLoadCertificate)
 (define-cproc tls-load-private-key (tls::<tls>
                                     filename::<const-cstring>
                                     password::<const-cstring>?)
   Scm_TLSLoadPrivateKey)
 (define-cproc %tls-connect (tls::<tls>
                             host::<const-cstring>
                             port::<const-cstring>
                             proto)
   Scm_TLSConnect)
 (define-cproc %tls-bind (tls::<tls>
                          ip::<const-cstring>?
                          port::<const-cstring>
                          proto)
   Scm_TLSBind)
 (define-cproc %tls-accept (tls::<tls>) Scm_TLSAccept)
 (define-cproc %tls-close (tls::<tls>) Scm_TLSClose)
 (define-cproc tls-read (tls::<tls>) Scm_TLSRead)
 (define-cproc tls-write (tls::<tls> msg) Scm_TLSWrite)
 (define-cproc tls-input-port (tls::<tls>) Scm_TLSInputPort)
 (define-cproc tls-output-port (tls::<tls>) Scm_TLSOutputPort)
 (define-cproc tls-poll (tls::<tls> rwflags::<list> :optional (timeout #f))
   (let* ([ts::ScmTimeSpec]
          [pts::ScmTimeSpec* (Scm_GetTimeSpec timeout (& ts))]
          [iflags::u_long 0])
     (for-each (lambda (f)
                 (cond
                  [(SCM_EQ f 'read) (logior= iflags TLS_POLL_READ)]
                  [(SCM_EQ f 'write) (logior= iflags TLS_POLL_WRITE)]
                  [else
                   (Scm_Error "List of 'read and 'write expected, but got: %S"
                              rwflags)]))
               rwflags)
     (let* ([r::u_long (Scm_TLSPoll tls iflags pts)]
            [result SCM_NIL])
       (when (logand r TLS_POLL_READ)
         (set! result (Scm_Cons 'read result)))
       (when (logand r TLS_POLL_WRITE)
         (set! result (Scm_Cons 'write result)))
       (return result))))
 (define-cproc tls-debug-level-set! (level::<int>) ::<void>
   (Scm_TLSSetDebugLevel level))
 ;; internal routine to regiseter debug level setter
 (define-cproc %tls-register-debug-level-callback (setter) ::<void>
   (Scm_TLSRegisterDebugLevelCallback setter))

 ;; internal
 (define-cproc %tls-input-port-set! (tls::<tls> port) Scm_TLSInputPortSet)
 (define-cproc %tls-output-port-set! (tls::<tls> port) Scm_TLSOutputPortSet)
 (define-cproc %tls-get-self-address (tls::<tls>)
   (return (Scm_TLSGetConnectionAddress tls TLS_SELF_ADDRESS)))
 (define-cproc %tls-get-peer-address (tls::<tls>)
   (return (Scm_TLSGetConnectionAddress tls TLS_PEER_ADDRESS)))

 ;; DEPRECATED APIs
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
 (define-cproc tls-load-object (tls::<tls>
                                obj-type::<fixnum>
                                filename::<const-cstring>
                                :optional (password::<const-cstring>? #f))
   ::<void>
   (Scm_Warn "tls-load-object is deprecated.  Use tls-load-certificate or \
              tls-load-private-key.\n")
   (case obj-type
     [(SSL_OBJ_X509_CERT SSL_OBJ_X509_CACERT)
      (Scm_TLSLoadCertificate tls filename)]
     [(SSL_OBJ_RSA_KEY SSL_OBJ_PKCS8 SSL_OBJ_PKCS12)
      (Scm_TLSLoadPrivateKey tls filename password)]
     [else (Scm_Error "Invalid obj-type: %d" obj-type)]))

 (declcode "void Scm_Init_tls(ScmModule *);")
 (initcode "Scm_Init_tls(Scm_CurrentModule());")
 )

;; API
(define (tls-connect tls host port :optional (proto 'tcp))
  (assume-type port (</> <string> <integer>))
  (let1 p (if (integer? port)
            (if (or (inexact? port) (negative? port))
              (error "Nonnegative exact integer or string expected, but got:"
                     port)
              (x->string port))
            port)
    (%tls-connect tls host p proto)
    (%tls-input-port-set! tls (make-tls-input-port tls))
    (%tls-output-port-set! tls (make-tls-output-port tls)))
  tls)

;; API
(define (tls-bind tls host port :optional (proto 'tcp))
  (assume-type port (</> <string> <integer>))
  (let1 p (if (integer? port)
            (if (or (inexact? port) (negative? port))
              (error "Nonnegative exact integer or string expected, but got:"
                     port)
              (x->string port))
            port)
    (%tls-bind tls host p proto)))

;; API
(define (tls-accept tls)
  (rlet1 new-tls (%tls-accept tls)
    (%tls-input-port-set! new-tls (make-tls-input-port new-tls))
    (%tls-output-port-set! new-tls (make-tls-output-port new-tls))))

;; API
(define (tls-close t)
  (when (input-port? (tls-input-port t))
    (close-input-port (tls-input-port t)))
  (when (output-port? (tls-output-port t))
    (close-output-port (tls-output-port t)))
  (%tls-close t))

;; Internal
(define-cproc %tls-system-ca-bundle-available? () ::<boolean>
  Scm_TLSSystemCABundleAvailable)

(define (make-tls-input-port tls)
  (rlet1 ip (make <virtual-input-port>)
    (set! (~ ip'getb)
          (let ((buf #f) (pos 0) (size 0))
            (rec (reader)
              (if buf
                (rlet1 r (string-byte-ref buf pos)
                  (set! pos (+ pos 1))
                  (when (= pos size) (set! buf #f)))
                (let1 data (tls-read tls)
                  (if (eof-object? data)
                    data
                    (begin
                      (set! buf data)
                      (set! size (string-size buf))
                      (set! pos 0)
                      (reader))))))))))

(define (make-tls-output-port tls)
  (rlet1 op (make <virtual-output-port>)
    (set! (~ op'puts) (^[msg] (tls-write tls msg)))
    (set! (~ op'putb) (^[b] (tls-write tls (make-byte-string 1 b))))))

;; Deprecated
(define (tls-destroy tls) (tls-close tls))

(define-method connection-self-address ((s <tls>)) (%tls-get-self-address s))
(define-method connection-peer-address ((s <tls>)) (%tls-get-peer-address s))
(define-method connection-input-port ((s <tls>))
  (tls-input-port s))
(define-method connection-output-port ((s <tls>))
  (tls-output-port s))
(define-method connection-shutdown ((s <tls>) how)
  ;; for now, we shutdown connection entirely regardless of HOW argument.
  (tls-close s))
(define-method connection-close ((s <tls>))
  (tls-destroy s))
