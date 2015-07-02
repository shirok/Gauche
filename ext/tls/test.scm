(use gauche.test)
(use gauche.process)
(use file.util)

(test-start "rfc.tls")

(use rfc.tls)
(test-module 'rfc.tls)

(cond-expand
 ;; ssltest program needs thread support, so we don't build it if we don't
 ;; have threads.
 [(and gauche.net.tls.axtls gauche.sys.threads)
  ;; Run ssltest program for sanity-check of axTLS build.
  ;; Some tests requires openssl command.  We've checked its availability
  ;; in toplevel configure.
  (define (have-openssl-command?)
    (any #/S\["OPENSSL"\]=\"openssl\"/
         (file->string-list "../../config.status")))

  (sys-unlink "axTLS/ssl/openssl.pid")
  (test* "ssltest" 0
         (process-exit-status
          (run-process `(./ssltest
                         ,@(cond-list [(have-openssl-command?) "--exttest"]))
                       :directory "axTLS/ssl"
                       :output "ssltest.log"
                       :wait #t)))
  ]
 [else])

(test-end)
