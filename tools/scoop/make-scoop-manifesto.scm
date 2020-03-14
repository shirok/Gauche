#!/usr/bin/gosh

(use rfc.json)
(use rfc.sha)
(use gauche.parseopt)
(use text.tr)

(define (get-sha256 file)
  (with-input-from-file
      file
    (lambda ()
      (digest-hexify (sha256-digest))
      )))

(define (print-manifesto version msi64 msi32)
  (let ((underscoreVersion (string-tr version "." "_"))
	(hash64            (get-sha256 msi64))
	(hash32            (get-sha256 msi32)))
    (construct-json
     `(
       ("##" . "This file is generated from Gauche source tree")

       ("description" . "Scheme Scripting Engine")
       ("license"     . "BSD-3-Clause")
       ("homepage"    . "https://practical-scheme.net/gauche/")
       ("version"     . ,version)

       ("checkver" .
	(("github" . "https://github.com/shirok/Gauche")))

       ("autoupdate" .
	(("architecture" .
	  (("64bit" .
	    (("url" . "https://github.com/shirok/Gauche/releases/download/release$underscoreVersion/Gauche-mingw-$version-64bit.msi")))
	   ("32bit" .
	    (("url" . "https://github.com/shirok/Gauche/releases/download/release$underscoreVersion/Gauche-mingw-$version-32bit.msi")))))))

       ("architecture" .
	(("64bit" .
	  (("url" .  ,#"https://github.com/shirok/Gauche/releases/download/release~|underscoreVersion|/Gauche-mingw-~|version|-64bit.msi")
	   ("hash" . ,hash64)))
	 ("32bit" .
	  (("url" .  ,#"https://github.com/shirok/Gauche/releases/download/release~|underscoreVersion|/Gauche-mingw-~|version|-32bit.msi")
	   ("hash" . ,hash32)))))

       ("bin" .
	#("Gauche/bin/gauche-cesconv.exe"
	  "Gauche/bin/gauche-config.exe"
	  "Gauche/bin/gauche-install.exe"
	  "Gauche/bin/gauche-package.exe"
	  "Gauche/bin/gosh.exe"
	  "Gauche/bin/gosh-noconsole.exe"))
       ))))

(define (main args)
  (let-args (cdr args)
	    ((version "version=s")
	     (msi64   "msi64=s")
	     (msi32   "msi32=s"))
	    (print-manifesto version msi64 msi32)
	    )
  0)
