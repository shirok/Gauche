;;;
;;; SRFI-176 version-alist - autoloaded
;;;

(define-module gauche.version-alist
  (use gauche.config)
  (use gauche.process))
(select-module gauche.version-alist)

(define-in-module gauche (version-alist :key (full #f))
  (let1 fs ((with-module gauche.internal cond-features))
    (remove
     (if full (constantly #f) version-info-excluded?)
     `((version ,(gauche-version))
       (command "gosh")
       (scheme.id gauche)
       (languages scheme r5rs r7rs)
       (encodings ,(gauche-character-encoding))
       (website "https://practical-scheme.net/gauche")
       (build.platform ,(gauche-architecture))
       (build.configure ,@($ cdr $ shell-tokenize-string
                             $ gauche-config "--reconfigure"))
       (build.gosh-version ,((with-module gauche.internal %build-gosh-version)))
       (scheme.path ,@*load-path*)
       (threads ,(cond
                  [(assq 'gauche.sys.pthreads fs) 'pthreads]
                  [(assq 'gauche.sys.wthreads fs) 'wthreads]
                  [else 'none]))
       (gauche.net.tls ,@(cond-list
                          [(assq 'gauche.net.tls.axtls fs) 'axtls]
                          [(assq 'gauche.net.tls.mbedtls fs) 'mbedtls]))))))

;; If the user doesn't want to include certain info in the output of
;; `gosh -V`, they can set an environment variable
;; GAUCHE_VERSION_INFO_EXCLUSION, which lists a comma-separated
;; list of keys.  Elements in the version-alist whose key matches
;; any of them will be excluded.
;;
;; For example, elements of scheme.path and build.configure may contain
;; the username and project name in the path, and the user might want
;; to exclude it so that the casual copy&paste of `gosh -V` output to
;; the public bug report accidentally reveals them.  They can set the
;; environment variable as follows:
;;
;;  GAUCHE_VERSION_INFO_EXCLUSION=scheme.path,build.configure
;;
;; For futuer addition: Maybe we can incorporate wildcard match, e.g.
;; 'build.*'.  For now, the number of elements in version-alist isn't
;; that big to need such feature, but if it grows in future, something
;; to consider.

(define version-info-excluded?
  (let1 excludes (if-let1 v (sys-getenv "GAUCHE_VERSION_INFO_EXCLUSION")
                   (map string->symbol (string-split v #/,/))
                   '())
    (^p (memq (car p) excludes))))
