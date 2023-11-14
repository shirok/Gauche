(use gauche.test)
(use gauche.threads)
(use gauche.net)

(use file.util)

(test-start "rfc.tls")

(use rfc.tls)
(test-module 'rfc.tls)
(use rfc.tls.mbed)
(test-module 'rfc.tls.mbed)

(cond-expand
 [gauche.net.tls

  (test-section "communication")

  (define (make-server-thread-1 bound-tls)
    (^[]
      (guard (e [else (report-error e) #f])
        (let1 clnt (tls-accept bound-tls)
          (tls-close clnt)))))

  (let ((serv (make <mbed-tls> :server-name "localhost"))
        (serv-thread #f))
    (unwind-protect
        (begin
          (test* "simple communication" #t
                 (is-a? (tls-bind serv #f "8087" 'tcp) <mbed-tls>))
          (test* "loading private key" #t
                 (boolean
                  (tls-load-private-key serv "data/test-key.pem" "cafebabe")))
          (test* "loading server cert" #t
                 (boolean
                  (tls-load-certificate serv "data/test-cert.pem")))
          ;; Not working yet
          ;; (set! serv-thread (make-thread (make-server-thread-1 serv)))
          ;; (thread-start! serv-thread)
          ;; (test* "connect" #t
          ;;        (parameterize ((tls-ca-bundle-path "data/test-cert.pem"))
          ;;          (let1 clnt (make <mbed-tls> :server-name "localhost")
          ;;            (tls-connect clnt "localhost" "8087" 'tcp))))
          )
      (tls-close serv)))
  ]
 [else])

(test-end)
