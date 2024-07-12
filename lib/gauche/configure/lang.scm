;;;
;;; gauche.configure.lang - language environment setup
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

(define-module gauche.configure.lang
  (use gauche.configure.base)
  (use gauche.mop.singleton)
  (export <cf-language>
          cf-lang
          cf-lang-ext cf-lang-ext-m
          cf-lang-program cf-lang-program-m
          cf-lang-io-program cf-lang-io-program-m
          cf-lang-call cf-lang-call-m

          cf-lang-cpp-m
          cf-lang-compile-m
          cf-lang-link-m
          cf-lang-null-program-m

          cf-lang-C cf-lang-C++
          ))
(select-module gauche.configure.lang)

;; utility
;; we accept a single line, or a list of lines, for testing program.
(define (join-lines line-or-lines)
  (cond [(string? line-or-lines) line-or-lines]
        [(list? line-or-lines) (string-join line-or-lines "\n" 'suffix)]
        [else (error "String or list of strings required, but got" line-or-lines)]))


;;;
;;; Target languages
;;;

;; Various language-specific operations are defined as methods on
;; the language singleton object inheriting <cf-language>.
;; Methods a named with -m suffix to distinguish from cf- API, which
;; is a usual procedure dispatches according to the current
;; language.
(define-class <cf-language> ()
  ((name :init-keyword :name :init-value "(none)"))
  :metaclass <singleton-meta>)

;; API
;; Parameter cf-lang holds a singleton instance of the current language.
(define cf-lang (make-parameter (make <cf-language>)))

(define (cf-lang-ext) (cf-lang-ext-m (cf-lang)))

;; API
;; cf-lang-program <prologue> <body>
;; Returns a string tree that consists a stand-alone program for the
;; current language.  <prologue> and <body> both are string tree.
(define (cf-lang-program prologue body)
  (cf-lang-program-m (cf-lang) (join-lines prologue) (join-lines body)))
(define-method cf-lang-program-m ((lang <cf-language>) prologue body)
  (error "No language is selected."))

;; API
;; cf-lang-io-program
;; Returns a string tree of a program that creates "conftest.out"
;; in the current language.
(define (cf-lang-io-program)
  (cf-lang-io-program-m (cf-lang)))
(define-method cf-lang-io-program-m ((lang <cf-language>))
  (error "No language is selected."))

;; API
;; cf-lang-call
(define (cf-lang-call prologue func-name)
  (cf-lang-call-m (cf-lang) (join-lines prologue) func-name))
(define-method cf-lang-call-m ((lang <cf-language>) prologue func-name)
  (error "No language is selected."))

;;
;; C
;;
(define-class <c-language> (<cf-language>) ((name :init-value "C")))
(define (cf-lang-C) (instance-of <c-language>))

(define-method cf-lang-ext-m ((lang <c-language>)) "c")
(define-method cf-lang-cpp-m ((lang <c-language>))
  #"~(cf$'CPP) ~(cf$'CPPFLAGS)")
(define-method cf-lang-compile-m ((lang <c-language>))
  #"~(cf$'CC) -c ~(cf$'CFLAGS) ~(cf$'CPPFLAGS) conftest.~(cf-lang-ext-m lang)")
(define-method cf-lang-link-m ((lang <c-language>))
  #"~(cf$'CC) -o conftest~(cf$'EXEEXT) ~(cf$'CFLAGS) ~(cf$'CPPFLAGS) ~(cf$'LDFLAGS) conftest.~(cf-lang-ext-m lang) ~(cf$'LIBS)")
(define-method cf-lang-null-program-m ((lang <c-language>))
  (cf-lang-program-m lang "" ""))

(define-method cf-lang-program-m ((lang <c-language>) prologue body)
  `(,prologue
    "\nint main(){\n"
    ,body
    "\n; return 0;\n}\n"))

(define-method cf-lang-io-program-m ((lang <c-language>))
  (cf-lang-program-m lang "#include <stdio.h>"
                     '("FILE *f = fopen(\"conftest.out\", \"w\");"
                       "return ferror(f) || fclose(f) != 0;")))

(define-method cf-lang-call-m ((lang <c-language>) prologue func-name)
  ($ cf-lang-program-m lang
     (if (equal? func-name "main")
       `(,prologue
         "\n#ifdef __cplusplus\nextern \"C\"\n#endif\nchar main();\n")
       prologue)
     `("return" ,func-name "();\n")))

;; C++
(define-class <c++-language> (<c-language>) ((name :init-value "C++")))
(define (cf-lang-C++) (instance-of <c++-language>))

(define-method cf-lang-ext-m ((lang <c++-language>)) "c")
(define-method cf-lang-cpp-m ((lang <c++-language>))
  #"~(cf$'CXXCPP) ~(cf$'CPPFLAGS)")
(define-method cf-lang-compile-m ((lang <c++-language>))
  #"~(cf$'CXX) -c ~(cf$'CXXFLAGS) ~(cf$'CPPFLAGS) conftest.~(cf-lang-ext-m lang)")
(define-method cf-lang-link-m ((lang <c++-language>))
  #"~(cf$'CXX) -o conftest~(cf$'EXEEXT) ~(cf$'CXXFLAGS) ~(cf$'CPPFLAGS) ~(cf$'LDFLAGS) conftest.~(cf-lang-ext-m lang) ~(cf$'LIBS)")
