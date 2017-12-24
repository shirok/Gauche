;; Scan extensions directories and generate staticinit.c
;; gosh gen-staticinit.scm $(top_srcdir) $(top_builddir)

;; NB: At this moment, we assume each extension DSO has single
;; initfn, with the default name.  It is the case in all bundled
;; extensions now.  However, Gauche allows to precompile multiple
;; modules into one DSO with multiple initfns for each module.
;; If we use that in modules under ext/*, we need to change this file.

;; NB: We treat gdbm specially, for statically linking it causes
;; the entire binary to be under GPL.  Here's what we do:
;;   - We split initializer function for gdbm libs into separate file
;;   - If the use defines GAUCHE_STATIC_LINK_GDBM before SCM_INIT_STATIC,
;;     we don't call gdbm initializer function, which causes object files
;;     related to gdbm would be excluded from the final executable.
;; This is kinda ad-hockery but we expect we won't add more dependencies
;; to GPL libs in the main distribution.

(use gauche.generator)
(use gauche.parameter)
(use gauche.process)
(use gauche.config)
(use gauche.cgen.precomp)
(use file.util)
(use util.match)
(use gauche.cgen)
(use text.tr)
(use srfi-13)
(use srfi-42)

(define top-srcdir   (make-parameter ".."))
(define top-builddir (make-parameter ".."))

(define (usage)
  (exit 1
   "Usage: gen-staticinit.scm $(top_srcdir) $(top_builddir)\n\
    Create statically linked version of libgauche, with all Scheme libraries\n\
    and extensions packaged in.  After building and installing Gauche as\n\
    usual, invoke this script via 'make static' in src directory.\n\
    If you want to exclude some libraries/extensions to keep the binary\n\
    size down, set the environment variable LIBGAUCHE_STATIC_EXCLUDES\n\
    with the module name delimited by comma, colon or space, e.g.:\n\
    \n\
    LIBGAUCHE_STATIC_EXCLUDES=rfc.tls,os.windows make static\n\
    \n"))

(define read-line/continuation
  (gbuffer-filter (^[v s]
                    (if-let1 m (#/\\$/ v)
                      (values '() (cons (m 'before) s))
                      (values `(,(string-concatenate-reverse (cons v s))) '())))
                  '()
                  read-line
                  (^[s] `(,(string-concatenate-reverse s)))))

;; This returns ("") if the definition is empty.  get-scheme-paths counts on it.
(define (mfvar-ref makefile var :optional default)
  (if-let1 line (with-input-from-file (build-path (top-builddir) makefile)
                  (cute generator-find (string->regexp #"^~|var|\\b")
                        read-line/continuation))
    (remove string-null?
            (string-split
             (rxmatch->string (string->regexp #"^~|var|\\s*=\\s*") line 'after)
             #[\s+]))
    (if (undefined? default)
      (errorf "Cannot find ~a definition in ~a" var makefile)
      default)))

;;
;; Scan ext/* directories to gather extention initfns, and generate
;; Scm_InitPrelinked() function.
;;

;; returns a list of shared library file names in ext/SUBDIR
(define (get-dso-names subdir)
  ($ map path-sans-extension
     $ mfvar-ref (build-path "ext" subdir "Makefile") "LIBFILES" '()))

;; Returns ((<dso> <module-name>) ...)
(define (gather-dsos modules-to-exclude)
  (list-ec [: subdir (mfvar-ref "ext/Makefile" "SUBDIRS")]
           [: dso (get-dso-names subdir)]
           [if (not (string-null? dso))]
           ;; NB: This relies on the current DSO naming scheme.
           [:let module-name (string->symbol (regexp-replace-all "--" dso "."))]
           [if (not (and (any (cut eq? module-name <>) modules-to-exclude)
                         (print "INFO: Skipping " dso)
                         #t))]
           (list dso module-name)))

;; Classify dsos
;; Caveat: If we use gdbm_compat, dbm.ndbm and dbm.odbm is likely to be
;; linked with gdbm.
(define (classify-dsos dsos&mods)
  (define gdbm-linked?
    (if (#/-lgdbm_compat/ (gauche-config "--static-libs"))
      (^m (memq m '(dbm.gdbm dbm.ndbm dbm.odbm)))
      (^m (eq? m 'dbm.gdbm))))
  (partition (^p (gdbm-linked? (cadr p))) dsos&mods))

(define (generate-staticinit dsos&mods)
  (do-ec [: dso&mod dsos&mods]
         [:let dso (car dso&mod)]
         (let ([initfn (cgen-c-file->initfn dso)]
               [str    (cgen-literal dso)])
           (cgen-decl #"extern void ~initfn(void);")
           (cgen-init
            #"  {"
            #"    const char *initfn_names[] = { ~(cgen-safe-string (string-append \"_\" initfn)), NULL };"
            #"    void (*initfns[])(void) = { ~initfn, NULL };"
            
            #"    Scm_RegisterPrelinked(SCM_STRING(~(cgen-cexpr str)), initfn_names, initfns);"
            #"   }"))))

;;
;; Gather *.scm and *.sci files
;;

;; Returns ((<module-name> <partial-path> <path-to-the-file>) ...)
;; <module-name>      : symbol
;; <partial-path>     : "parser/peg.sci" etc.
;; <path-to-the-file> : "../ext/peg/peg.sci" etc.
(define (get-scheme-paths)
  (append-map
   find-scm-source
   (append (map (^p (cons p (build-path "lib" p)))
                (mfvar-ref "lib/Makefile" "SCMFILES"))
           (append-ec [: subdir (mfvar-ref "ext/Makefile" "SUBDIRS")]
                      [:let mf (build-path "ext"subdir"Makefile")]
                      [:let cat (let1 v (mfvar-ref mf "SCM_CATEGORY")
                                  (if (null? v) "" (car v)))]
                      [:let files (mfvar-ref mf "SCMFILES")]
                      (map (^s (cons (build-path cat s)
                                     (build-path "ext" subdir s)))
                           files)))))

;; p : ((<partial-path> . <path-to-look-for>) ...)
;; expand glob pattern in the basename part
;; returns ((<module-name> <partial-path> <path-to-the-file>) ...)
(define (find-scm-source p)
  (map (^[file] (let* ([dir (sys-dirname (car p))]
                       [partial-path (string-tr
                                      (if (equal? dir ".")
                                        (sys-basename file)
                                        (build-path dir (sys-basename file)))
                                      "\\\\" "/")]
                       [modname ($ path->module-name
                                   $ path-sans-extension partial-path)])
                  (list modname partial-path file)))
       (delete-duplicates
        (glob (list (build-path (top-srcdir) (cdr p))
                    (build-path (top-builddir) (cdr p)))))))

;; Classify scheme files
;; scms : ((module partial-path path) ...)
;; dsos&mods-gdbm : ((dso module) ...)
(define (classify-scms scms dsos&mods-gdbm)
  (define gdbm-modules (map cadr dsos&mods-gdbm))
  (define gdbm-file?
    (^p (memq (car p) gdbm-modules)))
  (partition gdbm-file? scms))

(define (get-scm-content path)
  ;; NB: We can just do (file->string p), but the code below eliminates
  ;; comments and unnecessary spaces.  Using read/write would break if
  ;; the source code contains weird read-time constructor, though.  We
  ;; know Gauche sources don't have one, but should be careful if we ever
  ;; extend this functionality to cover other libraries.
  ;; NB: unwrap-syntax is to strip identifier info inserted by reader macros,
  ;; if any.
  (with-output-to-string
    (cute for-each ($ write $ unwrap-syntax $) (file->sexp-list path))))

;; This code fragment is evaluated in Scm_InitPrelinked() to set up a load
;; hook so that the standard libraries would be load from memory instead of
;; files.  Be careful! This code shouldn't trigger any autoloads.
(define (hook-source tab)
  `(%add-load-path-hook!
    (lambda (archive-file name suffixes)
      (and (equal? archive-file "")
           (let ([fn (any (lambda (sfx)
                            (let ([n (string-append name sfx)])
                              (and (hash-table-exists? ,tab n)
                                   n)))
                          suffixes)])
             (and fn
                  (let ([content (hash-table-get ,tab fn)])
                    (and content
                         (cons fn (lambda (_)
                                    (open-input-string content)))))))))))


(define (embed-scm name scmfiles)
  (cgen-decl "static ScmHashTable *scmtab;")
  (cgen-init "scmtab = SCM_HASH_TABLE(Scm_MakeHashTableSimple(SCM_HASH_STRING, 0));")
  (cgen-init "SCM_DEFINE(SCM_FIND_MODULE(\"gauche.internal\", 0),"
             #"           \"*embedded-scm-table-~|name|*\","
             "           SCM_OBJ(scmtab));")
  ;; Set up load hook
  (cgen-decl "static const char *embedded_load_hook = "
             ($ cgen-safe-string $ write-to-string 
                $ hook-source $ symbol-append '*embedded-scm-table- name '*)
             ";")
  (cgen-init "Scm_AddLoadPath(\"\", FALSE);")
  (cgen-init "Scm_EvalCStringRec(embedded_load_hook,"
             "                SCM_OBJ(SCM_FIND_MODULE(\"gauche.internal\", 0))"
             "                );")

  (do-ec [: scmfile scmfiles]
         [:let name (cgen-literal (cadr scmfile))]
         [:let lit  (cgen-literal (get-scm-content (caddr scmfile)))]
         (cgen-init (format "Scm_HashTableSet(scmtab, ~a, ~a, 0);"
                            (cgen-cexpr name)
                            (cgen-cexpr lit))))
  )

;; MinGW specific stuff
;; The extension modules are compiled to be linked against libgauche.dll,
;; so they refer to use import stubs (e.g. the call to Scm_Cons is actually
;; compiled to an indirect call via _imp_Scm_Cons).  We don't want to recompile
;; extension modules again, so we fake those import entries.
(define (generate-imp-stub libgauche.dll)
  (define exptab (make-hash-table 'equal?))
  (dolist [entry (process-output->string-list `("nm" ,libgauche.dll))]
    (rxmatch-case entry
      [#/[TD] (Scm\w+)/ (_ sym) (set! (~ exptab sym) #t)]))
  (hash-table-for-each exptab (^[k v] (cgen-decl #"void *__imp_~|k|;")))
  (cgen-body "static void unexported_procedure() {"
             "  fprintf(stderr, \"[Gauche Internal Error] Unexported procedure is called.  It is likely that the code is calling obsoleted C API.\");"
             "  exit(1);"
             "}")
  (cgen-body "static void *get_proc_addr(HMODULE m, const char *name) {"
             "  void *p = (void*)GetProcAddress(m, name);"
             "  if (p == NULL) p = unexported_procedure;"
             "  return p;"
             "}")
  (cgen-body "static void populate_imp_pointers() {"
             "  HMODULE m = GetModuleHandle(NULL);"
             "  if (m == NULL) {"
             "    printf(\"Couldn't get module handle.  Aborting.\");"
             "    exit(1);"
             "  }")
  ($ hash-table-for-each exptab
     (^[k v] (cgen-body #"  __imp_~|k| = get_proc_addr(m, \"~|k|\");")))
  (cgen-body "}")
  (cgen-init "  populate_imp_pointers();"))

(define (generate-c name initfn main? scmfiles dsos&mods)
  (cgen-current-unit (make <cgen-unit>
                       :name name
                       :init-prologue #"void ~initfn() {\n"
                       :init-epilogue "}\n"))
  (cgen-decl "#include <gauche.h>")
  (cgen-decl "extern void Scm_RegisterPrelinked(ScmString*, const char *ns[], void (*fns[])(void));")
  (when main?
    (cond-expand
     [gauche.os.windows (generate-imp-stub "libgauche-0.9.dll")]
     [else]))
  (embed-scm name scmfiles)
  (generate-staticinit dsos&mods)
  (cgen-emit-c (cgen-current-unit)))

(define (do-everything)
  ;; TRANSIENT: Rewrite this using let*-values after 0.9.6 release.
  ;; We need to avoid using it, since let*-values is moved from srfi-11
  ;; to core in 0.9.6, and we can't refer to srfi-11 during building 0.9.6
  ;; while using 0.9.5.
  (let1 modules-to-exclude
      (if-let1 x (sys-getenv "LIBGAUCHE_STATIC_EXCLUDES")
        (map string->symbol (string-split x #[\s:,]))
        '())
    (receive (dsos&mods-gdbm dsos&mods-other)
        (classify-dsos (gather-dsos modules-to-exclude))
      (receive (scms-gdbm scms-other)
          (classify-scms (get-scheme-paths) dsos&mods-gdbm)
        (generate-c "staticinit" "Scm_InitPrelinked" #t 
                    scms-other dsos&mods-other)
        (generate-c "staticinit_gdbm" "Scm_InitPrelinked_gdbm" #f
                    scms-gdbm dsos&mods-gdbm)
        ))))

(define (main args)
  (match (cdr args)
    [(%top-srcdir %top-builddir)
     (parameterize ([top-srcdir   %top-srcdir]
                    [top-builddir %top-builddir])
       (do-everything))]
    [_ (usage)])
  0)
