;; Scan extensions directories and generate staticinit.c
;; gosh gen-staticinit.scm $(top_srcdir) $(top_builddir)

;; NB: At this moment, we assume each extension DSO has single
;; initfn, with the default name.  It is the case in all bundled
;; extensions now.  However, Gauche allows to precompile multiple
;; modules into one DSO with multiple initfns for each module.
;; If we use that in modules under ext/*, we need to change this file.

(use gauche.generator)
(use file.util)
(use gauche.cgen)
(use text.tr)
(use srfi-13)
(use srfi-42)

(define *top-srcdir*   (ref (command-line) 1 ".."))
(define *top-builddir* (ref (command-line) 2 ".."))

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
  (if-let1 line (with-input-from-file (build-path *top-builddir* makefile)
                  (cute generator-find (string->regexp #`"^,|var|\\b")
                        read-line/continuation))
    (string-split
     (rxmatch->string (string->regexp #`"^,|var|\\s*=\\s*") line 'after)
     #[\s+])
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

;; given shared library name, derive the init function name
;; (see the comment above about consideration of multiple initfns).
(define (initfn-name dso-name)
  ;; NB: This is in sync with name transformatin in cgen.precomp.
  (let1 n (string-tr dso-name "-+." "___")
    #`"Scm_Init_,n"))

(define (generate-staticinit)
  (do-ec [: subdir (mfvar-ref "ext/Makefile" "SUBDIRS")]
         [: dso (get-dso-names subdir)]
         (let ([initfn (initfn-name dso)]
               [str    (cgen-literal dso)])
           (cgen-decl #`"extern void ,initfn(void);")
           (cgen-init
            #`"  {"
            #`"    const char *initfn_names[] = { ,(c-safe-string-literal (string-append \"_\" initfn)), NULL };"
            #`"    void (*initfns[])(void) = { ,initfn, NULL };"
            
            #`"    Scm_RegisterPrelinked(SCM_STRING(,(cgen-cexpr str)), initfn_names, initfns);"
            #`"   }"))))

;;
;; Gather *.scm and *.sci files
;;

;; ((<partial-path> . <path-to-look-for>) ...)
(define (get-scheme-paths)
  (append (map (^p (cons p (build-path "lib" p)))
               (mfvar-ref "lib/Makefile" "SCMFILES"))
          (append-ec [: subdir (mfvar-ref "ext/Makefile" "SUBDIRS")]
                     [:let mf (build-path "ext"subdir"Makefile")]
                     [:let cat (car (mfvar-ref mf "SCM_CATEGORY"))]
                     [:let files (mfvar-ref mf "SCMFILES")]
                     [if (not (equal? files '("")))]
                     (map (^s (cons (build-path cat s)
                                    (build-path "ext" subdir s)))
                          files))))

(define (get-scm-content path-to-look-for)
  (if-let1 p (find-file-in-paths path-to-look-for
                                 :paths (list *top-srcdir* *top-builddir*)
                                 :pred file-is-readable?)
    ;; NB: we can just do (file->string p), but the code below eliminates
    ;; comments and unnecessary spaces.  Using read/write would break if
    ;; the source code contains weird read-time constructor, though.  We
    ;; know Gauche sources don't have one, but should be careful if we ever
    ;; extend this functionality to cover other libraries.
    (with-output-to-string (cute for-each write (file->sexp-list p)))
    (errorf "couldn't find ~a" path-to-look-for)))

;; This code fragment is evaluated in Scm_InitPrelinked() to set up a load
;; hook so that the standard libraries would be load from memory instead of
;; files.  Be careful! This code shouldn't trigger any autoloads.
(define *hook-source*
  '(%add-load-path-hook!
    (lambda (archive-file name suffixes)
      (and (equal? archive-file "")
           (let ([fn (any (lambda (sfx)
                            (let ([n (string-append name sfx)])
                              (and (hash-table-exists? *embedded-scm-table* n)
                                   n)))
                          suffixes)])
             (and fn
                  (let ([content (hash-table-get *embedded-scm-table* fn)])
                    (and content
                         (cons fn (lambda (_) (open-input-string content)))))))))))

(define (embed-scm)
  ;; Table of module path -> source
  (cgen-decl "static ScmHashTable *scmtab;")
  (cgen-init "scmtab = SCM_HASH_TABLE(Scm_MakeHashTableSimple(SCM_HASH_STRING, 0));")
  (cgen-init "SCM_DEFINE(SCM_FIND_MODULE(\"gauche.internal\", 0),"
             "           \"*embedded-scm-table*\","
             "           SCM_OBJ(scmtab));")
  ;; Set up load hook
  (cgen-decl "static const char *embedded_load_hook = "
             (c-safe-string-literal (write-to-string *hook-source*))
             ";")
  (cgen-init "Scm_AddLoadPath(\"\", FALSE);")
  (cgen-init "Scm_EvalCStringRec(embedded_load_hook,"
             "                SCM_OBJ(SCM_FIND_MODULE(\"gauche.internal\", 0))"
             "                );")
  (cgen-init "Scm_EvalCStringRec(\" (define-macro (use module . options) `(begin (require ,(module-name->path module)) (import (,module ,@options))))\","
             "                 SCM_OBJ(Scm_GaucheModule()));")

  
  ;; Hash table setup
  (do-ec [: scmfile (get-scheme-paths)]
         [:let name (cgen-literal (car scmfile))]
         [:let lit  (cgen-literal (get-scm-content (cdr scmfile)))]
         (cgen-init (format "Scm_HashTableSet(scmtab, ~a, ~a, 0);"
                            (cgen-cexpr name)
                            (cgen-cexpr lit))))
  )

(define (main args)
  (cgen-current-unit (make <cgen-unit>
                       :name "staticinit"
                       :init-prologue "void Scm_InitPrelinked() {\n"
                       :init-epilogue "}\n"))
  (cgen-decl #`"#include <gauche.h>")
  (cgen-decl "extern void Scm_RegisterPrelinked(ScmString*, const char *ns[], void (*fns[])(void));")
  (embed-scm)
  (generate-staticinit)
  (cgen-emit-c (cgen-current-unit))
  0)

