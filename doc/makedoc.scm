;; -*- coding:utf-8 -*-

(use gauche.process)
(use gauche.version)
(use file.util)
(use util.match)
(use srfi-13)

;;;
;;;  Call makeinfo to generate various document output
;;;

(define (main args)
  (cond-expand
   ;; do not let LANG setting affect makeinfo
   [gauche.sys.setenv (sys-putenv "LANG=C")]
   [else])
  (if (match (cdr args)
        [("info" input makeinfo gzip)     (do-info input makeinfo gzip)]
        [("html" input makeinfo)          (do-html input makeinfo)]
        [("htmls" input makeinfo version) (do-htmls input makeinfo version)]
        [("pdf" input makeinfo)           (do-pdf input makeinfo)]
        [("dvi" input makeinfo)           (do-dvi input makeinfo)]
        [_ (usage)])
    0 1))

(define (usage)
  (print "Usage: gosh makedoc.scm command args ...")
  (print "Valid commands (and args):")
  (print "  info input MAKEINFO GZIP            - generate info doc")
  (print "  html input MAKEINFO                 - generate single html")
  (print "  htmls input MAKEINFO VERSION-STRING - generate html files in subdir")
  (print "  pdf input MAKEINFO                  - generate pdf")
  (print "  dvi input MAKEINFO                  - generate dvi")
  #f)

(define (make-cmd cmd-list)
  (cond-expand
   [gauche.os.windows
    ;; for MSYS (mintty)
    (if-let1 sh (sys-getenv "SHELL")
      `("cmd.exe" "/c" ,sh "-c" ,(string-join
                                  (map (^c (shell-escape-string (x->string c) 'posix))
                                       cmd-list)
                                  " "))
      `("cmd.exe" "/c" ,@cmd-list))]
   [else cmd-list]))

(define (check-makeinfo-version makeinfo min-version)
  (and-let* ([ makeinfo ]
             [msg (process-output->string (make-cmd `(,makeinfo --version)))]
             [vers (rxmatch->string #/\d+\.\d+(\.\d+)?/ msg)])
    (rlet1 b (version<=? min-version vers)
      (unless b
        (warn "makeinfo version ~a or greater is required, but ~a's \
               version is ~a.  Skipping.\n" min-version makeinfo vers)))))

(define (do-info input makeinfo gzip)
  (define info (path-swap-extension input "info"))
  (or (string-null? makeinfo)
      (string-null? gzip)
      (and (do-process (make-cmd `(,makeinfo ,input)))
           (begin (remove-files (glob #"~|info|*.gz"))
                  (do-process (make-cmd `(,gzip ,info ,@(glob #"~|info|-[0-9]*"))))))))

(define (do-html input makeinfo)
  (or (string-null? makeinfo)
      (not (check-makeinfo-version makeinfo "5.0"))
      (do-process (make-cmd
                   `(,makeinfo "--html"
                               "--no-split"
                               "--set-customization-variable"
                               "TOP_NODE_UP_URL=https://practical-scheme.net/gauche"
                               "-"))
                  :redirects `((<< 0 ,(alter-top-node input))))))

(define (do-htmls input makeinfo version-info)
  (define top-link (if (#/j\.texi$/ input)
                     "https://practical-scheme.net/gauche/memo-j.html"
                     "https://practical-scheme.net/gauche/memo.html"))
  (or (string-null? makeinfo)
      (not (check-makeinfo-version makeinfo "5.0"))
      (do-process (make-cmd
                   `(,makeinfo "--html"
                               "--split=section"
                               "--set-customization-variable"
                               ,#"AFTER_BODY_OPEN=<div style=\"width:100%\" class=\"header\"><p style=\"text-align:center\"><a href=\"~|top-link|\">For ~|version-info|</a></p></div><hr>"
                               "--set-customization-variable"
                               ,#"PRE_BODY_CLOSE=<hr><div style=\"width:100%\" class=\"footer\"><p style=\"text-align:center\"><a href=\"~|top-link|\">For ~|version-info|</a></p></div>"
                               "--set-customization-variable"
                               ,#"TOP_NODE_UP_URL=~|top-link|"
                               "-"))
                  :redirects `((<< 0 ,(alter-top-node input))))))

;; For html, makeinfo generates "(dir)" link in the top node, which we don't
;; need.  TOP_NODE_UP_URL doesn't alter link for "Previous".
;; So we employ this kludge to modify the Top node definition
(define (alter-top-node input)
  ($ (cut string-join <> "\n" 'suffix)
     $ map (^l (if (#/^@node Top,/ l) "@node Top" l))
     $ file->string-list input))

;; For pdf and dvi: We need newer luatex (0.95 or later) to process Japanese.
;; But even if you don't have that newer luatex, TeX can handle English
;; version.  So, for now, we conditionally switch TeX based on the input
;; file name.  We won't need the switching once everybody has newer versions.
;; (NB: Also not you need texinfo-ja to process Japanese version; they've
;; merged only recently (May 2016) and may not be available unless you're
;; using bleeding edge.  Cf. http://www.trueroad.jp/2016/05/14-01.html)

(define (do-pdf input makeinfo)
  (cond-expand
   [gauche.sys.setenv
    (when (#/j\.texi$/ input)
      (sys-putenv "PDFTEX=luatex"))]
   [else])
  (do-process (make-cmd `(,makeinfo "--pdf" "--Xopt" "--tidy" ,input))))

(define (do-dvi input makeinfo)
  (cond-expand
   [gauche.sys.setenv
    (when (#/j\.texi$/ input)
      (sys-putenv "TEX=dviluatex"))]
   [else])
  (do-process (make-cmd `(,makeinfo "--dvi" "--Xopt" "--tidy" ,input))))

