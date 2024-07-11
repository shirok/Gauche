;;;
;;; gauche.configure.compile - Test with compiling
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

(define-module gauche.configure.compile
  (use gauche.configure.base)
  (use gauche.configure.lang)
  (use gauche.configure.prog)
  (use gauche.generator)
  (use gauche.logger)
  (use gauche.process)
  (use file.util)
  (use text.tree)
  (use text.tr)
  (use srfi.13)
  (export cf-call-with-cpp cf-try-compile cf-try-compile-and-link
          cf-prog-cxx
          cf-includes-default cf-header-available?
          cf-check-header cf-check-headers
          cf-type-available? cf-check-type cf-check-types
          cf-decl-available? cf-check-decl cf-check-decls
          cf-member-available? cf-check-member cf-check-members
          cf-func-available? cf-check-func cf-check-funcs
          cf-lib-available? cf-check-lib cf-search-libs)
  )
(select-module gauche.configure.compile)

(define (safe-variable-name s)
  (string-tr (string-upcase s) "A-Z0-9" "_*" :complement #t))

;;;
;;; Tests - compilation
;;;

;; Dump CONTENT to a file conftext.$(cf-lang-ext) and run COMMAND.
;; The output and error goes to config.log.  Returns #t on success,
;; #f on failure.  Make sure to clean temporary files.
(define (run-compiler-with-content command content)
  (define (clean)
    (remove-files (glob "conftest.err*")
                  #"conftest.~(cf-lang-ext)"
                  #"conftest.~(cf$'OBJEXT)"
                  #"conftest~(cf$'EXEEXT)"))
  (define cmd
    (if (string? command)
      (shell-tokenize-string command)
      command))
  (unwind-protect
      (receive (errout erroutfile) (sys-mkstemp "conftest.err.")
        (log-format "configure: ~s" cmd)
        (with-output-to-file #"conftest.~(cf-lang-ext)"
          (^[] (write-tree content)))
        (let1 st ($ process-exit-status
                    (run-process cmd :wait #t
                                 :redirects `((> 1 ,errout) (> 2 ,errout))))
          (close-port errout)
          ($ generator-for-each (cut log-format "~a" <>)
             $ file->line-generator erroutfile)
          (log-format "configure: $? = ~s" (sys-wait-exit-status st))
          (unless (zero? st)
            (log-format "configure: failed program was:")
            ($ generator-for-each (cut log-format "| ~a" <>)
               $ file->line-generator #"conftest.~(cf-lang-ext)"))
          (zero? st)))
    (clean)))

;; API (no autoconf equivalent)
;; Run preprocessor and calls proc with an input port receiving the output
;; of the preprocessor.
(define (cf-call-with-cpp prologue body proc)
  (define file #"conftest.~(cf-lang-ext)")
  (define cmd `(,@(shell-tokenize-string (cf-lang-cpp-m (cf-lang))) ,file))
  (define (clean)
    (remove-files (glob "conftest.err*") file))
  (define process #f)
  (unwind-protect
      (begin
        (log-format "configure: ~s" cmd)
        (with-output-to-file file
          (cut write-tree (cf-lang-program prologue body)))
        (set! process (run-process cmd :output :pipe))
        (proc (process-output process)))
    (when process (process-kill process))
    (clean)))

;; API
;; Try compile BODY as the current language.
;; Returns #t on success, #f on failure.
(define (cf-try-compile prologue body)
  ($ run-compiler-with-content
     (cf-lang-compile-m (cf-lang))
     (cf-lang-program prologue body)))

;; API
;; Try compile and link BODY as the current language.
;; Returns #t on success, #f on failure.
(define (cf-try-compile-and-link prologue body)
  ($ run-compiler-with-content
     (cf-lang-link-m (cf-lang))
     (cf-lang-program prologue body)))

;; Try to produce executable from
;; This emits message---must be called in feature test api
(define (compiler-can-produce-executable?)
  (cf-msg-checking "whether the ~a compiler works" (~ (cf-lang)'name))
  (rlet1 result ($ run-compiler-with-content
                   (cf-lang-link-m (cf-lang))
                   (cf-lang-null-program-m (cf-lang)))
    (cf-msg-result (if result "yes" "no"))))

;; Feature Test API
;; Find c++ compiler.  Actually, we need the one that generates compatible
;; binary with which Gauche was compiled, but there's no reliable way
;; (except building an extension and trying to load into Gauche, but that's
;; a bit too much.)
(define (cf-prog-cxx :optional (compilers '("g++" "c++" "gpp" "aCC" "CC"
                                            "cxx" "cc++" "cl.exe" "FCC"
                                            "KCC" "RCC" "xlC_r" "xlC")))
  (cf-arg-var 'CXX)
  (cf-arg-var 'CXXFLAGS)
  (cf-arg-var 'CCC)
  (or (not (string-null? (cf-ref 'CXX)))
      (and-let* ([ccc (cf-ref 'CCC)]
                 [ (not (string-null? ccc)) ])
        (cf-subst 'CXX ccc)
        #t)
      (cf-check-tool 'CXX compilers :default "g++"))
  (parameterize ([cf-lang (cf-lang-C++)])
    (compiler-can-produce-executable?)))

;;;
;;; Tests - headers
;;;

;; API
;; Returns a string tree
;; Unlike AC_INCLUDES_DEFAULT, we don't accept argument.  The
;; behavior of AC_INCLUDES_DEFAULT is convenient for m4 macros,
;; but makes little sense for Scheme.
(define cf-includes-default
  (let* ([defaults '("#include <stdio.h>\n"
                     "#ifdef HAVE_SYS_TYPES_H"
                     "# include <sys/types.h>"
                     "#endif"
                     "#ifdef HAVE_SYS_STAT_H"
                     "# include <sys/stat.h>"
                     "#endif"
                     "#ifdef STDC_HEADERS"
                     "# include <stdlib.h>"
                     "# include <stddef.h>"
                     "#else"
                     "# ifdef HAVE_STDLIB_H"
                     "#  include <stdlib.h>"
                     "# endif"
                     "#endif"
                     "#ifdef HAVE_STRING_H"
                     "# if !defined STDC_HEADERS && defined HAVE_MEMORY_H"
                     "#  include <memory.h>"
                     "# endif"
                     "# include <string.h>"
                     "#endif"
                     "#ifdef HAVE_STRINGS_H"
                     "# include <strings.h>"
                     "#endif"
                     "#ifdef HAVE_INTTYPES_H"
                     "# include <inttypes.h>"
                     "#endif"
                     "#ifdef HAVE_STDINT_H"
                     "# include <stdint.h>"
                     "#endif"
                     "#ifdef HAVE_UNISTD_H"
                     "# include <unistd.h>"
                     "#endif")]
         [requires (delay
                     (begin (cf-check-headers '("sys/types.h" "sys/stat.h"
                                                "stdlib.h" "string.h" "memory.h"
                                                "strings.h" "inttypes.h"
                                                "stdint.h" "unistd.h")
                                              :includes defaults)
                            defaults))])
    (^[] (force requires))))

;; internal API
;; Common processing of :includes keyword argument.  If it is #f,
;; we use cf-includes-default.  It is supposed to be a list of lines,
;; but we allow a single string.
(define (default-includes includes)
  (cond [(not includes) (cf-includes-default)]
        [(list? includes) includes]
        [(string? includes) (list includes)]
        [else (error "String list required for :includes, but got" includes)]))

;; Feature Test API
;; Like AC_CHECK_HEADER.
;; Returns #t on success, #f on failure.
(define (cf-header-available? header-file :key (includes #f))
  (let1 includes (default-includes includes)
    (cf-msg-checking "~a usability" header-file)
    (rlet1 result (cf-try-compile `(,@includes
                                    "/* Testing compilability */"
                                    ,#"#include <~|header-file|>")
                                  "")
      (cf-msg-result (if result "yes" "no")))))
(define cf-check-header cf-header-available?) ;; autoconf compatible name

;; Feature Test API
;; Like AC_CHECK_HEADERS.  Besides the check, it defines HAVE_<header-file>
;; definition.
(define (cf-check-headers header-files
                          :key (includes #f) (if-found #f) (if-not-found #f))
  (dolist [h header-files]
    (if (cf-check-header h :includes includes)
      (begin (cf-define (string->symbol #"HAVE_~(safe-variable-name h)"))
             (when if-found (if-found h)))
      (when if-not-found (if-not-found h)))))

;; Feature Test API
;; Like AC_CHECK_TYPE.
;; Returns #t on success, #f on failure.
;; If TYPE is a valid type, sizeof(TYPE) compiles and sizeof((TYPE)) fails.
;; The second test is needed in case TYPE happens to be a variable.
(define (cf-type-available? type :key (includes #f))
  (let1 includes (default-includes includes)
    (cf-msg-checking "for ~a" type)
    (rlet1 result
        (and (cf-try-compile includes
                             #"if (sizeof (~|type|)) return 0;")
             (not (cf-try-compile includes
                                  #"if (sizeof ((~|type|))) return 0;")))
      (cf-msg-result (if result "yes" "no")))))
(define cf-check-type cf-type-available?)  ; autoconf-compatible name

;; Feature Test API
;; Like AC_CHECK_TYPES.
;; For each type in types, run cf-check-type and define HAVE_type if found.
(define (cf-check-types types :key (includes #f)
                                   (if-found identity)
                                   (if-not-found identity))
  (dolist [type types]
    (if (cf-check-type type :includes includes)
      (begin (cf-define (string->symbol #"HAVE_~(safe-variable-name type)"))
             (if-found type))
      (if-not-found type))))

;; Feature Test API
;; Like AC_CHECK_DECL
;; Returns #t on success, #f on failure.
;; Check SYMBOL is declared as a macro, a constant, a variable or a function.
(define (cf-decl-available? symbol :key (includes #f))
  (let1 includes (default-includes includes)
    (cf-msg-checking "whether ~a is declared" symbol)
    (rlet1 result
        (cf-try-compile includes
                        (list #"#ifndef ~|symbol|"
                              #" (void)~|symbol|;"
                              #"#endif"
                              "return 0;"))
      (cf-msg-result (if result "yes" "no")))))
(define cf-check-decl cf-decl-available?)  ;autoconf-compatible name

;; Feature Test API
;; Like AC_CHECK_DECLS
;; For each symbol in symbols, run cf-check-decl and define HAVE_DECL_symbol
;; to 1 (found) or 0 (not found).
(define (cf-check-decls symbols :key (includes #f)
                                     (if-found identity)
                                     (if-not-found identity))
  (dolist [symbol symbols]
    (let1 nam (string->symbol #"HAVE_DECL_~(safe-variable-name symbol)")
      (if (cf-check-decl symbol :includes includes)
        (begin (cf-define nam 1)
               (if-found symbol))
        (begin (cf-define nam 0)
               (if-not-found symbol))))))

;; Feature Test API
;; Like AC_CHECK_MEMBER
;; Works as a predicate
(define (cf-member-available? aggregate.member :key (includes #f))
  (receive (aggr memb) (string-scan aggregate.member #\. 'both)
    (unless (and aggr memb)
      (error "cf-check-member: argument doesn't contain a dot:"
             aggregate.member))
    (cf-msg-checking "`~a' is a member of `~a'" memb aggr)
    (let1 includes (default-includes includes)
      (rlet1 result
          (or (cf-try-compile includes
                              (list #"static ~aggr ac_aggr;"
                                    #"if (ac_aggr.~|memb|) return 0;"))
              (cf-try-compile includes
                              (list #"static ~aggr ac_aggr;"
                                    #"if (sizeof ac_aggr.~|memb|) return 0;")))
        (cf-msg-result (if result "yes" "no"))))))
(define cf-check-member cf-member-available?) ;autoconf-compatible name

;; Feature Test API
;; Like AC_CHECK_MEMBERS
(define (cf-check-members members :key (includes #f)
                                       (if-found identity)
                                       (if-not-found identity))
  (dolist [mem members]
    (if (cf-check-member mem :includes includes)
      (begin (cf-define (string->symbol #"HAVE_~(safe-variable-name mem)"))
             (if-found mem))
      (if-not-found mem))))

;; Feature Test API
;; Like AC_CHECK_FUNC
;; NB: autoconf has language-dependent methods (AC_LANG_FUNC_LINK_TRY)
;; For now, we hardcode C.
(define (cf-func-available? func)
  (let1 includes (cf-includes-default)
    (cf-msg-checking #"for ~func")
    (rlet1 result ($ cf-try-compile-and-link
                     `(,#"#define ~func innocuous_~func"
                       "#ifdef __STDC__"
                       "# include <limits.h>"
                       "#else"
                       "# include <assert.h>"
                       "#endif"
                       ,#"#undef ~func"
                       "#ifdef __cplusplus"
                       "extern \"C\""
                       "#endif"
                       ,#"char ~func ();")
                     `(,#"return ~func ();"))
      (cf-msg-result (if result "yes" "no")))))
(define cf-check-func cf-func-available?)  ;autoconf-compatible name

;; Feature Test API
;; Like AC_CHECK_FUNCS
(define (cf-check-funcs funcs :key (if-found identity)
                                   (if-not-found identity))
  (dolist [f funcs]
    (if (cf-check-func f)
      (begin (cf-define (string->symbol #"HAVE_~(safe-variable-name f)"))
             (if-found f))
      (if-not-found f))))


(define (default-lib-found libname)
  (when libname
    (cf-subst-prepend 'LIBS #"-l~|libname|")
    (cf-define (string->symbol #"HAVE_LIB~(safe-variable-name libname)")))
  #t)

(define (default-lib-not-found libname) #f)

;; Feature Test API
;; Like AC_CHECK_LIB
(define (cf-lib-available? lib fn
                        :key (other-libs '())
                        (if-found default-lib-found)
                        (if-not-found default-lib-not-found))
  (let1 includes (cf-includes-default)
    (cf-msg-checking "for ~a in -l~a" fn lib)
    (if (with-cf-subst
         ([LIBS #"-l~|lib| ~(string-join other-libs \" \")" +])
         (cf-try-compile-and-link includes
                                  (format "extern void ~a(); ~a();" fn fn)))
      (begin
        (cf-msg-result "yes")
        (if-found lib))
      (begin
        (cf-msg-result "no")
        (if-not-found lib)))))
(define cf-check-lib cf-lib-available?)    ;autoconf-compatible name

(define (default-lib-search-found libname)
  (when libname
    (cf-subst-prepend 'LIBS #"-l~|libname|"))
  #t)

;; Feature test API
;; Like AC_CHECK_LIBS
(define (cf-search-libs fn libs
                        :key (other-libs '())
                             (if-found default-lib-search-found)
                             (if-not-found default-lib-not-found))
  (let ([includes (cf-includes-default)]
        [xlibs #"~(string-join other-libs \" \") ~(cf$'LIBS)"])
    (define (try lib)
      (with-cf-subst
       ([LIBS (if (eq? lib 'none) xlibs #"-l~|lib| ~xlibs")])
       (cf-try-compile-and-link includes
                                (format "extern void ~a(); ~a();"
                                        fn fn))))
    (cf-msg-checking "for ~a" fn)
    (if-let1 lib (find try (cons 'none libs))
      (begin
        (cf-msg-result (if (eq? lib 'none) "found" #"found in -l~|lib|"))
        (if-found (if (eq? lib 'none) #f lib)))
      (begin
        (cf-msg-result "no")
        (if-not-found #f)))))
