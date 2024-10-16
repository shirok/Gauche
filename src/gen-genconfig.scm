;; -*- coding: utf-8 -*-
;;;
;;; Generate genconfig.in
;;;

(use srfi.13)
(use util.match)

;;
;; gen-genconfig.scm
;;         + <---- genconfig.in.in : template
;;         |
;;         |<== release tarball creation
;;         |
;;         +-----------------+---------------------------\
;;         |                 |                           |
;;         v                 v                           v
;;    genconfig.in    doc/gauche-config.1.in  doc/man-gauche-config.texi
;;         |                 |
;;         |<== configure    |
;;         v                 v
;;     genconfig       doc/gauche/config.1
;;         |
;;         |<==  make
;;         v
;;   gauche-config.c
;;    paths_arch.c
;;  lib/gauche/config.scm
;;

;; This script should depend on _released_ Gauche.  Do not use pre-release
;; features.

;; To add a new option to gauche-config, edit *config-parameters*.
;;
;;  <config-parameter> : <section> | <entry>
;;  <section> : (section <string> [<string-ja>])  ; used in help message
;;  <entry> : (((<option> <source-string>) ...) <help-message> [<help-message-ja>])
(define *config-parameters*
  '((section
     "General parameter"
     "一般のパラメータ")
    ((("-V" "$gauche_version"))
     "The current Gauche version."
     "現在のGaucheのバージョン")

    (section
     "Parameters to compile applications using Gauche"
     "Gaucheを使ったアプリケーションのコンパイルのためのパラメータ")
    ((("-I" "-I$gauche_incdir"))
     "Include path options required to compile programs using Gauche\n\
      (Note: This doesn't work if Gauche installation directory\n\
      path contains whitespaces.  See --incdirs below.)"
     "Gaucheを使ったプログラムをコンパイルするのに必要なインクルードパスオプション\n\
      (Gaucheのインストール先のパスが空白を含んでいる場合、これは
      使えません。下の--incdirsを見てください。)")
    ((("-L" "-L$gauche_archdir $local_lib"))
     "Library path options required to link programs using Gauche\n\
      (Note: This doesn't work if Gauche installation directory\n\
      path contains whitespaces.  See --archdirs below.)"
     "Gaucheを使ったプログラムをコンパイルするのに必要なライブラリパスオプション\n\
      (Gaucheのインストール先のパスが空白を含んでいる場合、これは
      使えません。下の--incdirsを見てください。)")
    ((("-l" "-l$garchabi $gauche_libs"))
     "Link library options required to link programs using Gauche."
     "Gaucheを使ったプログラムをリンクする時に必要なライブラリオプション")
    ((("--cc" "$cc"))
     "The name of the C compiler used to compile this Gauche."
     "このGaucheをコンパイルする時に使われたCコンパイラ名")
    ((("--cpp" "$cpp"))
     "The command to run the C preprocessor."
     "Cプリプロセッサを走らせるコマンド")
    ((("--ac" "$gauche_aclocaldir"))
     "The directory that contains Gauche-specific autoconf macros."
     "Gauche用のautoconfマクロがインストールされているディレクトリ名")
    ((("--reconfigure" "./configure $gauche_configure_args"))
     "The command line used to configure the current installation."
     "現在インストールされているGaucheに使われたconfigureのオプション")
    ((("--arch" "$arch"))
     "The autoconf-style architecture signature (cpu-vendor-kernel-os)."
     "autoconf形式のアーキテクチャシグネチャ (cpu-vendor-kernel-os)")
    ((("--incdirs" "$incdirs")
      ("--archdirs" "$archdirs"))
     "The list of directory names to be looked for include files and\n\
      libraries, respectively.  Each directory name may be quoted if\n\
      it contains whitespaces, and separated by ':' on Unix platforms, or\n\
      by ';' on Windows platforms."
     "それぞれ、インクルードファイルとライブラリファイルが探されるディレクトリのリスト。\n\
      各ディレクトリ名は、それが空白を含んでいる場合はクオートされています。また、\n\
      それぞれのディレクトリ名の間はUnixでは ':'、Windowsでは ';' で区切られています。")
    ((("--local-incdir" "$local_inc")
      ("--local-libdir" "$local_lib"))
     "These are '-I' and '-L' flags for additional local headers/libraries\n\
      to search, given by '--with-local' configure flags.  Note that those\n\
      are also included in '-I', '-L', '--incdirs', and '--archdirs'."
     "これらは、追加のローカルヘッダファイルやライブラリファイルを探すために必要な\n\
      '-I' および '-L' フラグのリストです。これらはconfigure時の '--with-local' \n\
      オプションにより設定されます。追加のローカルディレクトリは、'-I'、'-L'、'--incdirs'\n\
      '--arcdirs' の出力にも含まれます。")

    (section
     "Parameters to install files"
     "ファイルをインストールするためのパラメータ")
    ((("--prefix" "$prefix"))
     "The directory prefix set by configure."
     "configure時に設定されたディレクトリプレフィクス")
    ((("--sysincdir" "$gauche_incdir")
      ("--siteincdir" "$gauche_siteincdir")
      ("--pkgincdir" "$gauche_pkgincdir"))
     "Directories where system|site|package header files of extensions go."
     "それぞれ、システム/サイト/パッケージレベルの拡張モジュールヘッダファイルが\n\
      インストールされるべきディレクトリ")
    ((("--syslibdir" "$gauche_libdir")
      ("--sitelibdir" "$gauche_sitelibdir")
      ("--pkglibdir" "$gauche_pkglibdir"))
     "Directories where system|site|package scheme files go."
     "それぞれ、システム/サイト/パッケージのSchemeファイルがインストールされる\n\
      べきディレクトリ")
    ((("--sysarchdir" "$gauche_archdir")
      ("--sitearchdir" "$gauche_sitearchdir")
      ("--pkgarchdir" "$gauche_pkgarchdir"))
     "Directories where system|site|package DSO files go."
     "それぞれ、システム/サイト/パッケージのDSOファイルがインストールされる\n\
      べきディレクトリ")
    ((("--mandir" "$mandir")
      ("--infodir" "$infodir"))
     "Directories where gauche manpage and info docs are installed."
     "それぞれ、Gaucheのmanpageとinfoドキュメントがインストールされるべきディレクトリ")

    (section
     "Parameters to help building extensions"
     "拡張モジュールをビルドするためのパラメータ")
    ((("--object-suffix" "$object_suffix"))
     "The extension of the compiled objects (e.g. 'o' or 'obj')."
     "コンパイルされたオブジェクトファイルの拡張子 (例: 'o' や 'obj')")
    ((("--executable-suffix" "$executable_suffix"))
     "The extension of the executable including a period (empty on Unix systems,\n\
      '.exe' on Windows."
     "実行可能ファイルの拡張氏(ピリオドを含む)。Unixシステムでは空文字列、
      Windowsでは '.exe'。")
    ((("--so-suffix" "$shlib_so_suffix"))
     "The extension for dynamically loadable (dlopen-able) modules (e.g. 'so')."
     "動的ロード(dlopen)可能なファイルの拡張子 (例: 'so')")
    ((("--so-cflags" "$shlib_so_cflags"))
     "Additional CFLAGS to create dynamically loadable modules."
     "動的ロード可能なファイルを作るために追加で必要なCFLAGS")
    ((("--so-ldflags" "$shlib_so_ldflags"))
     "Additional LDFLAGS to create dynamically loadable modules."
     "動的ロード可能なファイルを作るために追加で必要なLDFLAGS")
    ((("--so-libs" "$shlib_so_libs"))
     "Additional libraries required to create dynamically loadable modules."
     "動的ロード可能なファイルを作るために追加で必要なライブラリ")
    ((("--dylib-suffix" "$shlib_dylib_suffix"))
     "The extension for dynamically linked libraries (as opposed to dlopen()ed)\n\
      Usually the same as --so-suffix, but OSX wants 'dylib'."
     "動的リンクされる (dlopenされるのではない) ライブラリファイルの拡張子。\n\
      通常は--so-suffixと同じだが、OSXでは 'dylib'。")
    ((("--dylib-ldflags" "$shlib_dylib_ldflags"))
     "LDFLAGS to create dynamically linked libraries."
     "動的リンクされるライブラリファイルを作るのに必要なLDFLAGS。")
    ((("--rpath-flag" "$rpath_flag"))
     "Compiler flag(s) to embed RPATH"
     "RPATHを埋め込むためのコンパイラフラグ")
    ((("--default-cflags" "$default_cflags"))
     "This is the CFLAGS used to compile Gauche, and to be used to compile\n\
      extensions as well."
     "Gaucheをコンパイルする時に使われたCFLAGS。拡張モジュールをコンパイルする時にも\n\
      同じフラグを使うべき。")
    ((("--cppflags" "$cppflags"))
     "Compiler flag(s) passed to C preprocessor"
     "Cプリプロセッサに渡されるコンパイラフラグ")
    ((("--static-libs" "$static_libs"))
     "List of library link flags ('-llib') required to link Gauche statically.\n\
      Similar to '-l', but this includes the libraries that are used\n\
      for extension modules, and also the static library itself,\n\
      that is -lgauche-static-X.X."
     "Gaucheを静的リンクする時に必要なリンクフラグ ('-llib') のリスト。\n\
     '-l' と似ているが、これは拡張モジュールが必要とするライブラリ、\n\
     及びGaucheの静的ライブラリ本体('-lgauche-static-X.X')を含んでいる。")
    ((("--libgauche-so" "$libgauche_so"))
     "The base name of dynamically linked libgauche."
     "動的リンクされるlibgaucheのbasename")

    )) ; end of *config-parameters*

;;
;; Generating genconfig.in from genconfig.in.in
;;

(define (generate genconfig.in.in)
  (define (process-line line)
    (cond
     [(equal? line "@@@USAGE@@@") (gen-usage)]
     [(equal? line "@@@COMMAND_TABLE@@@") (gen-command-table)]
     [(equal? line "@@@COMMAND_LIST@@@") (gen-command-list)]
     [(string-prefix? "@@@" line)]
     [else (print line)]))

  (call-with-input-file genconfig.in.in
    (^p (for-each process-line (port->string-list p)))))

(define (gen-usage)
  (dolist [e *config-parameters*]
    (match e
      [('section section-e . _)
       (format #t "    \"\\n\"\n    \"~a\\n\"\n" section-e)]
      [(((opt src) ...) help . _)
       ;; TRANSIENT: After 1.0 release, we can switch to use text.fill
       ;; (let1 s `(,(map (cut format "  ~a\n" <>) (drop-right opt 1))
       ;;           ,($ tree->string $ text->filled-stree help
       ;;               :indent 8
       ;;               :lead-in (format "  ~a" (last opt))
       ;;               :width 78))
       ;;   (dolist [h (string-split s #\newline)]
       ;;     (format #t "    \"~a\\n\"\n" h)))
       (dolist [o opt]
         (format #t "    \"  ~a\\n\"\n" o))
       (dolist [h (string-split help #\newline)]
         (format #t "    \"        ~a\\n\"\n" h))])))

(define (gen-command-table)
  (dolist [e *config-parameters*]
    (match e
      [('section . _) #f]
      [(((opt src) ...) help . _)
       (for-each (^[o s] (format #t "    {~s, ~s},\n" o s)) opt src)])))

(define (gen-command-list)
  (dolist [e *config-parameters*]
    (match e
      [('section . _) #f]
      [(((opt src) ...) help . _)
       (for-each (^[o s] (format #t "    (~s ~s)\n" o s)) opt src)])))

;; Generate info fragment to be included from doc/program.texi
(define (gen-info)
  (print "@c To be included from program.texi")
  (print "@c Generated by gen-genconfig.scm.  DO NOT EDIT.")
  (fold (^[e state]
          (match e
            [('section section-en section-ja)
             (unless (eq? state 'beginning)
               (print "@end table") (print))
             (print "@c EN")
             (print section-en ":")
             (print "@c JP")
             (print section-ja ":")
             (print "@c COMMON")
             (print "@table @option")
             'cont]
            [(((opt src) ...) help-en help-ja)
             (format #t "@item ~a\n" (car opt))
             (dolist [o (cdr opt)]
               (format #t "@itemx ~a\n" o))
             (print "@c EN")
             (print (regexp-replace #/@/ help-en "@@"))
             (print "@c JP")
             (print (regexp-replace #/@/ (or help-ja help-en) "@@"))
             (print "@c COMMON")
             'cont]))
        'beginning *config-parameters*)
  (print "@end table"))

;; Generate manpage
(define (gen-man)
  (print ".\\\" -*-nroff-*-")
  (print ".TH GAUCHE\\-CONFIG \"1\" \"\" \"Gauche @GAUCHE_VERSION@\" \"Gauche Commands\"")
  (print ".SH NAME")
  (print "gauche-config \\- retrieve configuration parameters of Gauche")
  (print ".SH SYNOPSIS")
  (print ".B gauche-config")
  (print ".I option")
  (print ".br")
  (print ".sp 0.3")
  (print ".SH DESCRIPTION")
  (print ".I Gauche-config")
  (print "displays various parameters specified at the configuration time")
  (print "of the Gauche Scheme implementation.  It can be used in Makefile")
  (print "and other configuration scripts that uses Gauche.")
  (print)
  (print ".SH OPTIONS")

  (dolist [e *config-parameters*]
    (match e
      [('section section-e . _)
       (print)
       (print ".SS " section-e)]
      [(((opt src) ...) help . _)
       (print ".TP")
       (print ".B " (string-join opt ", "))
       (print help)]))

  (print)
  (print ".SH AUTHORS")
  (print "Shiro Kawai (shiro @ acm . org)")
  (print)
  (print ".SH SEE ALSO")
  (print "gosh(1), gauche-package(1)")
  (print ".PP")
  (print "Gauche Scheme script engine:")
  (print ".br")
  (print "https://practical-scheme.net/gauche/"))


(define (main args)
  (match (cdr args)
    [(file)
     (with-output-to-file "genconfig.in"
       (cut generate file))
     (with-output-to-file "../doc/man-gauche-config.texi"
       (cut gen-info))
     (with-output-to-file "../doc/gauche-config.1.in"
       (cut gen-man))
     0]
    [_ (exit 1 "Usage: gosh gen-genconfig.scm <genconfig-template>")]))
