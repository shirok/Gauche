;; examples/windows/errordialog.scm
;; Public domain

(use os.windows)

;; It is a good idea for Windows scripts to catch an error at the outermost
;; level, since the error console will be immediately gone after program
;; exits in non-console mode.
(define (main args)
  (guard (e [else (sys-message-box #f (format "ERROR: ~a" (~ e'message))
                                   (sys-basename *program-name*)
                                   (logior MB_OK MB_ICONERROR))])
    (sys-sleep 1)
    (error "We got an error!"))
  0)
