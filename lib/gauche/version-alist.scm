;;;
;;; srfi-176 version-alist - autoloaded
;;;

(define-module gauche.version-alist
  (use gauche.config)
  (use gauche.process))
(select-module gauche.version-alist)

(define-in-module gauche (version-alist)
  (let1 fs ((with-module gauche.internal cond-features))
    `((version ,(gauche-version))
      (command "gosh")
      (scheme.id gauche)
      (languages scheme r5rs r7rs)
      (encodings ,(gauche-character-encoding))
      (website "https://practical-scheme.net/gauche")
      (build.platform ,(gauche-architecture))
      (build.configure ,@($ cdr $ shell-tokenize-string
                            $ gauche-config "--reconfigure"))
      (scheme.path ,@*load-path*)
      (gauche.threads ,(cond
                        [(assq 'gauche.sys.pthreads fs) 'pthreads]
                        [(assq 'gauche.sys.wthreads fs) 'wthreads]
                        [else 'none]))
      (gauche.net.tls ,@(cond-list
                         [(assq 'gauche.net.tls.axtls fs) 'axtls]
                         [(assq 'gauche.net.tls.mbedtls fs) 'mbedtls])))))
