;;;
;;; Utility of configure
;;;
(define-module util-config
  (use gauche.configure)
  (use gauche.version)
  (export cf-init-gauche-extension))
(select-module util-config)

;; For the compatibility to Gauche 0.9.7 - 0.9.9
(define (cf-init-gauche-extension)

  ;; Call original procedure
  (if (global-variable-bound? 'gauche.configure 'cf-init-gauche-extension)
    ((with-module gauche.configure cf-init-gauche-extension))
    (error "procedure 'cf-init-gauche-extension' not found.\n\
           Gauche 0.9.7 or later is required.  Aborting."))

  ;; For Gauche 0.9.9 or earlier
  (when (version<=? (gauche-version) "0.9.9")
    ;; C build settings
    (unless (cf-have-subst? 'CFLAGS)
      (cf-subst 'CFLAGS (gauche-config "--default-cflags")))
    (unless (cf-have-subst? 'CPPFLAGS) (cf-subst 'CPPFLAGS ""))
    (unless (cf-have-subst? 'LDFLAGS)  (cf-subst 'LDFLAGS  ""))
    (unless (cf-have-subst? 'LIBS)     (cf-subst 'LIBS     ""))

    ;; For Windows Unicode support
    (cond-expand
     [(and gauche.os.windows gauche.ces.utf8)
      (unless (#/(^-|\s+-)DUNICODE\b/ (cf$ 'CPPFLAGS))
        (cf-subst-append 'CPPFLAGS "-DUNICODE"))]
     [else])
    )
  )

