;;;
;;; configure.scm - configuring Gauche extensions
;;;
;;;   Copyright (c) 2013-2024  Shiro Kawai  <shiro@acm.org>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

;; This is a utility library to write 'configure' script, a replacement
;; of autotool-generated 'configure' shell script.  See ext/template.configure
;; for an example.
;;
;; The biggest advantage of using autotool's 'configure' is that it runs
;; on most vanilla unix, for it only uses minimal shell features and
;; basic unix commands.   However, when you configure Gauche extension,
;; you sure have Gauche already.  Why not use full power of Gauche
;; to do the configuration work?
;;
;; If we use Gauche to write 'configure', we no longer need an extra step
;; to generate configure from configure.ac, for the interpreter (gosh)
;; is powerful enough to allow extension writers to do any abstraction
;; he needs.   So the author can check in 'configure' script itself
;; to the source tree, and anyone who checks it out can directly run
;; ./configure, without worrying running autoconf (and free from headache
;; of autoconf version mismatch)
;;
;; The core feature of gauche.configure is the ability to generate files
;; (e.g. Makefile) from templates (e.g. Makefile.in) with replacing
;; parameters.  We follow autoconf convention, so the replacement variables
;; in a template is written like @VAR@.
;;
;; The API is roughly corresponds to autoconf's AC_* macros, while we use
;; 'cf-' suffix instead.
;;
;; The simplest configure scripts can be just the following 3 expressions:
;;
;;  (use gauche.configure)
;;  (cf-init-gauche-extension)
;;  (cf-output-default)
;;
;; It takes package name and version from package.scm file, sets several
;; substitution variables, and creates Makefile from Makefile.in along
;; the gpd (Gauche package description) file.

;; TODO: Caching test results

(define-module gauche.configure
  (use gauche.configure.base)
  (use gauche.configure.lang)
  (use gauche.configure.prog)
  (use gauche.configure.output)
  (use gauche.configure.compile)
  (use gauche.configure.lib)
  (use gauche.configure.init)

  ;; Allow configure scripts to use gauche-config
  (extend gauche.config)

  (export
   ;; gauche.configure.base
   cf-msg-checking cf-msg-result cf-msg-warn cf-msg-error cf-msg-notice
   cf-echo
   cf-define cf-defined? cf-subst cf-subst-append cf-subst-prepend
   cf-arg-var cf-have-subst? cf-ref cf$
   with-cf-subst

   ;; gauche.configure.init
   cf-init cf-init-gauche-extension
   cf-arg-enable cf-arg-with cf-feature-ref cf-package-ref
   cf-help-string

   ;; gauche.configure.output
   cf-config-headers cf-output cf-output-default cf-show-substs
   cf-make-gpd

   ;; gauche.configure.prog
   cf-check-prog cf-path-prog cf-check-tool

   ;; gauche.configure.lang
   cf-lang cf-lang-C cf-lang-C++
   cf-lang-program cf-lang-io-program cf-lang-call

   ;; gauche.configure.compile
   cf-call-with-cpp cf-try-compile cf-try-compile-and-link
   cf-prog-cxx

   cf-includes-default
   cf-check-header cf-header-available? cf-check-headers
   cf-check-type cf-type-available? cf-check-types
   cf-check-decl cf-decl-available? cf-check-decls
   cf-check-member cf-member-available? cf-check-members
   cf-check-func cf-func-available? cf-check-funcs
   cf-check-lib cf-lib-available? cf-search-libs

   ;; gauche.configure.lib
   cf-path-x cf-path-xtra
   ))
