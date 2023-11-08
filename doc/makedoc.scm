;; -*- coding:utf-8 -*-

(use file.util)
(use gauche.process)
(use gauche.version)
(use srfi.13)
(use util.match)

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
        [("epub" input makeinfo)          (do-epub input makeinfo)]
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
  (print "  epub input MAKEINFO                 - generate ePub")
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

(define (check-makeinfo-version makeinfo min-version :optional (quiet? #f))
  (and-let* ([ makeinfo ]
             [msg (process-output->string (make-cmd `(,makeinfo --version)))]
             [vers (rxmatch->string #/\d+\.\d+(\.\d+)?/ msg)])
    (rlet1 b (version<=? min-version vers)
      (when (and (not b) (not quiet?))
        (warn "makeinfo version ~a or greater is required, but ~a's \
               version is ~a.  Skipping.\n" min-version makeinfo vers)))))

(define (do-info input makeinfo gzip)
  (define info (path-swap-extension input "info"))
  (or (string-null? makeinfo)
      (string-null? gzip)
      (not (check-makeinfo-version makeinfo "5.0"))
      (and (do-process (make-cmd `(,makeinfo ,input)))
           (begin (remove-files (glob #"~|info|*.gz"))
                  (do-process (make-cmd `(,gzip "-n" ,info ,@(glob #"~|info|-[0-9]*"))))))))

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
  (define draft? (#/DRAFT/ version-info))
  (define ja? (#/j\.texi$/ input))
  (define top-link
    (cond [draft? "https://github.com/shirok/Gauche"]
          [ja? "https://practical-scheme.net/gauche/memo-j.html"]
          [else "https://practical-scheme.net/gauche/memo.html"]))
  (define header-style (if draft?
                         "width:100%;background-color:#f88;"
                         "width:100%;background-color:#cfc;"))
  (define lang (if ja? "ja" "en"))
  (define draft-search (if draft?
                         "<input type=\"hidden\" name=\"v\" value=\"draft\">"
                         ""))
  (define header-div #"<div style=\"~|header-style|\">\
                        <form action=\"https://practical-scheme.net/gauche/man/\"\
                              style=\"padding:5px 10px\">\
                          <a href=\"~|top-link|\">For ~|version-info|</a>\
                          <span style=\"float: right\">\
                            Search (procedure/syntax/module): \
                            <input type=\"text\" name=\"p\">\
                            <input type=\"hidden\" name=\"l\" value=\"~|lang|\">\
                            ~|draft-search|\
                          </span>\
                        </form>\
                      </div>")
  (define draft-mark (if draft?
                       "<div style=\"position:fixed;top:150px;right:0px;\
                                     font:bold 70px Helvetica,sans-serif;\
                                     color:#f00;opacity:0.4;\
                                     transform:rotate(-90deg)\">\
                         DRAFT\
                       </div>"
                       ""))
  (or (string-null? makeinfo)
      (not (check-makeinfo-version makeinfo "5.0"))
      (do-process (make-cmd
                   `(,makeinfo "--html"
                               "--split=section"
                               "--set-customization-variable"
                               ,#"AFTER_BODY_OPEN=~|header-div|<hr>"
                               "--set-customization-variable"
                               ,#"PRE_BODY_CLOSE=<hr>~|header-div|~|draft-mark|"
                               "--set-customization-variable"
                               ,#"TOP_NODE_UP_URL=~|top-link|"
                               ,@(cond-list
                                  [(check-makeinfo-version makeinfo "6.8" #t) @
                                   `("--set-customization-variable"
                                     "FORMAT_MENU=menu"
                                     "--set-customization-variable"
                                     "CONTENTS_OUTPUT_LOCATION=inline")]
                                  [draft? @
                                   `("-o"
                                     ,#"~(path-sans-extension input)-draft")])
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

(define (do-epub input makeinfo)
  (do-process (make-cmd `(,makeinfo "--epub3" ,input))))
