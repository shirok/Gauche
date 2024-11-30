;;;
;;;  Generate buildinfo.c
;;;
;;;  This script should be executed by BUILD_GOSH
;;;

(use gauche.cgen)

(cgen-current-unit
 (make <cgen-unit>
   :name "buildinfo"
   :preamble "/* Generated from autoloads.scm.  DO NOT EDIT */\
           \n /* To be included from core.c  */"
   :init-prologue ""
   :init-epilogue ""
   ))

(define (main args)
  (cgen-decl "static const char *build_gosh_version = "
             (cgen-safe-string (gauche-version))
             ";")
  (cgen-emit-c (cgen-current-unit))
  0)
