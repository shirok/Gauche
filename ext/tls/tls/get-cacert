#!/usr/bin/env gosh
;;;
;;; Download cacert.pem file
;;;

(use gauche.process)
(use file.util)

(define (usage)
  (print "Usage: gosh rfc/tls/get-cacert")
  (print "  Download cacert.pem from curl official site (curl.haxx.se) and")
  (print "  Saves it to installed Gauche's library directory.")
  (print "  (If you installed Gauche with root priviledge, you're asked to")
  (print "  enter your password by sudo when the file is copied.)")
  (exit 1))

(define *cacert-url* "https://curl.haxx.se/ca/cacert.pem")

(define *curl*
  (or (find-file-in-paths "curl")
      (exit 1 "Cannot find 'curl' in PATH.  We need to use curl to download cacert file.")))

(define *destdir* (sys-dirname (gauche-library-directory)))

(define (main args)
  (when (not (null? (cdr args)))
    (usage))
  (call-with-temporary-directory do-it :prefix "gauche-get-cacert")
  0)

(define (do-it tmpdir)
  (let1 file (build-path tmpdir "cacert.pem")
    (print #"Donwnloading ~*cacert-url*")
    (or (do-process `(,*curl* -o ,file ,*cacert-url*))
        (exit 1 "curl failed"))
    (if (sys-access *destdir* W_OK)
      (copy-file file (build-path *destdir* "cacert.pem")
                 :if-exists :supersede)
      (begin
        (print #"We're going to run 'sudo gauche-install ~file ~*destdir*'")
        (do-process `(sudo gauche-install ,file ,*destdir*))))))

;; Local variables:
;; mode: scheme
;; end:
