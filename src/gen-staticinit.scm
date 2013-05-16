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

;; returns all SUBDIRS under ext/
(define (ext-subdirs)
  (if-let1 line (with-input-from-file (build-path *top-builddir* "ext/Makefile")
                  (cut generator-find #/^SUBDIRS\b/ read-line/continuation))
    (string-split (rxmatch->string #/^SUBDIRS\s*=\s*/ line 'after) #[\s+])
    (error "Cannot find SUBDIRS definition in ext/Makefile")))

;; returns a list of shared library file names in ext/SUBDIR
(define (get-dso-names subdir)
  (if-let1 line (with-input-from-file
                    (build-path *top-builddir* "ext" subdir "Makefile")
                  (cut generator-find #/^LIBFILES\b/ read-line/continuation))
    ($ map path-sans-extension
       $ string-split (rxmatch->string #/^LIBFILES\s*=\s*/ line 'after) #[\s+])
    '()))

;; given shared library name, derive the init function name
;; (see the comment above about consideration of multiple initfns).
(define (initfn-name dso-name)
  ;; NB: This is in sync with name transformatin in cgen.precomp.
  (let1 n (string-tr dso-name "-+." "___")
    #`"Scm_Init_,n"))

(define (main args)
  (cgen-current-unit (make <cgen-unit>
                       :name "staticinit"
                       :init-prologue "void Scm_InitPrelinked() {\n"
                       :init-epilogue "}\n"))
  (cgen-decl #`"#include <gauche.h>")
  (cgen-decl "extern void Scm_RegisterPrelinked(ScmString*);")
  (do-ec [: subdir (ext-subdirs)]
         [: dso (get-dso-names subdir)]
         (let ([initfn (initfn-name dso)]
               [str    (cgen-literal dso)])
           (cgen-decl #`"extern void ,initfn(void);")
           (cgen-init #`"  Scm_RegisterPrelinked(SCM_STRING(,(cgen-cexpr str)));"
                      #`"  ,initfn();")))
  (cgen-emit-c (cgen-current-unit))
  0)


