(use gauche.test)
(use gauche.process)
(use file.util)

(test-start "rfc.tls")

(use rfc.tls)
(test-module 'rfc.tls)

(cond-expand
 [gauche.net.tls.axtls
  ;; Run ssltest program for sanity-check of axTLS build.
  ;; Some tests requires opensll command.  We've checked its availability
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
