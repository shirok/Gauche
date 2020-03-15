#!/usr/bin/gosh
;;
;; Scoop manifest ganerator
;;
;; Usage:
;;  $ gosh make-scoop-manifasto.scm --version="version number" --msi64="64bit MSI file" --msi32="32bit MSI file" > gauche.json
;;
;;
;; NB: Output JSON file uses commpressed format. Use JSON pretty printer for
;;     human-readable form.
;;  $ emacs --batch --file=gauche.json --load=json \
;;          --funcall=json-pretty-print-buffer \
;;          --funcall=save-buffer
;;

(use gauche.parseopt)
(use rfc.json)
(use rfc.sha)
(use text.tr)

(define (hex-digest-file file)
  (with-input-from-file
      file
    (lambda ()
      (digest-hexify (sha256-digest)))))

(define (print-manifesto version msi64 msi32)
  (let ((underscoreVersion (string-tr version "." "_"))
	(hash64            (hex-digest-file msi64))
	(hash32            (hex-digest-file msi32)))

    (construct-json
     `(
       ("##" . "This file is generated from Gauche source tree")

       ("description" . "Scheme Scripting Engine")
       ("homepage"    . "https://practical-scheme.net/gauche/")
       ("version"     . ,version)

       ("license" .
	(("identifier" . "BSD-3-Clause")
	 ("url" . "https://github.com/shirok/Gauche/blob/master/COPYING")))

       ("architecture" .
	(("64bit" .
	  (("url"  . ,#"https://github.com/shirok/Gauche/releases/download/release~|underscoreVersion|/Gauche-mingw-~|version|-64bit.msi")
	   ("hash" . ,hash64)))
	 ("32bit" .
	  (("url"  . ,#"https://github.com/shirok/Gauche/releases/download/release~|underscoreVersion|/Gauche-mingw-~|version|-32bit.msi")
	   ("hash" . ,hash32)))))

       ("bin" .
	#("Gauche/bin/gauche-cesconv.exe"
	  "Gauche/bin/gauche-config.exe"
	  "Gauche/bin/gauche-install.exe"
	  "Gauche/bin/gauche-package.exe"
	  "Gauche/bin/gosh.exe"
	  "Gauche/bin/gosh-noconsole.exe"))

       ("checkver" .
	(("github" . "https://github.com/shirok/Gauche")))

       ("autoupdate" .
	(("architecture" .
	  (("64bit" .
	    (("url" . "https://github.com/shirok/Gauche/releases/download/release$underscoreVersion/Gauche-mingw-$version-64bit.msi")))
	   ("32bit" .
	    (("url" . "https://github.com/shirok/Gauche/releases/download/release$underscoreVersion/Gauche-mingw-$version-32bit.msi")))))))
       ))
    ))

(define (main args)
  (let-args (cdr args)
	    ((version "version=s")
	     (msi64   "msi64=s")
	     (msi32   "msi32=s"))
	    (print-manifesto version msi64 msi32)
	    )
  0)
