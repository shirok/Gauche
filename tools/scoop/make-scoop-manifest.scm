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
       ("description" . "Scheme Scripting Engine")
       ("homepage"    . "https://practical-scheme.net/gauche/")
       ("version"     . ,version)

       ("license" . "BSD-3-Clause")

       ("architecture" .
	(("64bit" .
	  (("url"  . ,#"https://github.com/shirok/Gauche/releases/download/release~|underscoreVersion|/Gauche-mingw-~|version|-64bit.msi")
	   ("hash" . ,hash64)))
	 ("32bit" .
	  (("url"  . ,#"https://github.com/shirok/Gauche/releases/download/release~|underscoreVersion|/Gauche-mingw-~|version|-32bit.msi")
	   ("hash" . ,hash32)))))

       ("extract_dir" . "Gauche")
       ("bin" .
	#("bin\\gauche-cesconv.exe"
	  "bin\\gauche-config.exe"
	  "bin\\gauche-install.exe"
	  "bin\\gauche-package.exe"
	  "bin\\gosh.exe"
	  "bin\\gosh-noconsole.exe"))

       ("shortcuts" .
	#(
	  #("bin\\gosh.exe" "Gosh")
	  ))

       ("checkver" .
	(("github" . "https://github.com/shirok/Gauche")
	 ("regex" . "mingw-([\\d.]+)-\\d+bit\\.msi")))

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

	    (if (not (and version msi64 msi32))
		(error "Required option (version,msi64,msi32) is missing"))

	    (print-manifesto version msi64 msi32)
	    )
  0)
