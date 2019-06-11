(use gauche.test)
(use gauche.process)
(use gauche.version)
(use file.util)

(test-start "rfc.tls")

(use rfc.tls)
(test-module 'rfc.tls)
(use rfc.tls.mbed)
(test-module 'rfc.tls.mbed)


(cond-expand
 ;; ssltest program needs thread support, so we don't build it if we don't
 ;; have threads.
 [(and gauche.net.tls.axtls gauche.sys.threads)
  ;; Run ssltest program for sanity-check of axTLS build.
  ;; Some tests requires openssl command.  We've checked its availability
  ;; in toplevel configure.
  ;; NB: We assume we're running in $top_builddir/ext/tls.
  (define openssl-cmd
    (and-let1 m (any #/S\["OPENSSL"\]=\"(.+)\"/
                     (file->string-list "../../config.status"))
      (m 1)))
  (define (no-openssl msg)
    (warn #"~|msg|: some tests are skipped.\n")
    (set! openssl-cmd #f))

  ;; MinGW's openssl command needs winpty.
  ;; (MSYS's openssl command doesn't need it.)
  (define mingw-detected
    (cond-expand
     [gauche.os.windows
      (and-let1 msystem (sys-getenv "MSYSTEM")
        (boolean (#/MINGW(64|32)/ msystem)))]
     [else #f]))
  (define openssl-path
    (and mingw-detected
         openssl-cmd
         (guard (e [(<process-abnormal-exit> e) #f])
           (process-output->string
            `("cmd.exe" "/c" "which" ,openssl-cmd)))))
  (define winpty-needed
    (and mingw-detected
         openssl-path
         (boolean (#/\/mingw(64|32)/ openssl-path))))

  (sys-unlink "axTLS/ssl/openssl.pid")
  (sys-unlink "kick_openssl.sh")

  (cond
   [(not openssl-cmd)
    (no-openssl "openssl command not available")]
   [(and mingw-detected (not openssl-path))
    (no-openssl "couldn't get openssl command path")]
   [(and winpty-needed
         (not (find-file-in-paths "winpty" :extensions '("exe"))))
    (no-openssl "winpty not found. (MinGW's openssl command needs it.)")]
   [else
    ;; Check openssl version.  OSX and MinGW32 ship with old openssl
    ;; that's unusable.
    (guard (e [(<process-abnormal-exit> e)
               (no-openssl "couldn't run openssl command")])
      (if-let1 m ($ #/(?:OpenSSL|LibreSSL)\s+([\d\.]+\w*)/
                    $ process-output->string
                      (cond-expand
                       ;; for MSYS (mintty)
                       [gauche.os.windows `("cmd.exe" "/c" ,openssl-cmd "version")]
                       [else              `(,openssl-cmd "version")]))
        (let1 vers (m 1)
          (unless (version>=? vers "1.0.1")
            (no-openssl #"openssl version is too old (~vers)")))
        (no-openssl "couldn't get openssl version")))])

  (when openssl-cmd
    (with-output-to-file "kick_openssl.sh"
      (^[]
        (print "#!/bin/sh")
        (print "set -e")
        (print #"echo \"$$\" \"~|openssl-cmd|\" >> openssl.pid")
        (cond
         [winpty-needed
          ;; MinGW's openssl command needs winpty only when stdin is terminal.
          ;; (MSYS's openssl command doesn't need this workaround.)
          (print  "if [ -t 0 ]; then")
          (print #"    exec winpty -Xallow-non-tty -Xplain \"~|openssl-cmd|\" \"$@\"")
          (print  "else")
          (print #"    exec \"~|openssl-cmd|\" \"$@\"")
          (print  "fi")]
         [else
          (print #"exec \"~|openssl-cmd|\" \"$@\"")])))
    (sys-chmod "kick_openssl.sh" #o755))

  (test* "ssltest" 0
         (process-exit-status
          (run-process `(./ssltest
                         ,@(cond-list [openssl-cmd "--exttest"]))
                       :directory "axTLS/ssl"
                       :output "ssltest.log"
                       :wait #t)))

  ;; On MSYS (mintty), winpty with '-Xallow-non-tty' option changes tty
  ;; setting, so that we should reset it.
  (when (and openssl-cmd winpty-needed)
    (sys-system "stty sane"))
  ]
 [else])



(test-end)
