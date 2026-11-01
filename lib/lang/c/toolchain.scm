;;;
;;; Utility for C toolchain
;;;
;;;   Copyright (c) 2026  Shiro Kawai  <shiro@acm.org>
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

;; This is a submodule of other lang.c.* modules.  Not for general public use.
(define-module lang.c.toolchain
  (use gauche.config)
  (use gauche.process)
  (use file.util)
  (use scheme.list)
  (use srfi.13)
  (use util.match)
  (export call-with-cpp
          cpp-include-paths
          c-find-header))
(select-module lang.c.toolchain)

;; Returns command line list to invoke cc.
;;
;; Kludge - gauche-config --cc may return something like "gcc --std=gnu23".
;; We need to split the command and the argument.  However, the compiler
;; pathname may include whitespaces, e.g. "c:\\Program Files\\...\\gcc.exe",
;; so we use heuristics.
(define cc
  (let1 cmd (delay (string-split (gauche-config "--cc") #/\s(?=-)/))
    (^[] (force cmd))))

;; NB: We may eventually implement our own preprocessor, for we can do
;; more things (such as caching), but for the time being, we call cpp.
;;
;; gcc exits with non-zero status if output isn't fully read, which happens
;; when PROC throws an error.  We don't want to lose that error, so we set
;; :on-abnormal-exit to :ignore.
;;
;; INCLUDES is a list of include paths, or #f to use the system default
;; (gauche-config --incdirs).  DEFS is an list of (var val ...) which will
;; be used as -Dvar=val -Dvar=val ..., or just var for -Dvar.

(define (call-with-cpp file proc
                       :key (includes #f)
                            (defs '()))
  (define Is
    (cond [(list? includes)
           (map (^d (shell-escape-string #"-I~d")) includes)]
          [(not includes)
           (map (^d #"-I~d")
                (string-split (gauche-config "--incdirs") #\:))]
          [else (error "includes keyword arg must be a list of \
                        directories or #f, but got:" includes)]))
  (define Ds
    (append-map (^[def]
                  (match def
                    [(var val ...)
                     (map (^[val] (shell-escape-string #"-D~|var|=~val")) val)]
                    [var `(,#"-D~var")]))
                defs))

  (call-with-input-process `(,@(cc) "-E" ,@Is ,@Ds ,file)
    proc
    :on-abnormal-exit :ignore))

;; API
;; Obtain built-in include paths for the system's compiler.
;; This isn't guaranteed to work universally, but works for
;; gcc and clang family.   If the info cannot be obtained, returns ().
(define cpp-include-paths
  (let1 paths (delay (%try-get-include-paths))
    (^[] (force paths))))

(define (%try-get-include-paths)
  (let* ([p (run-process `(,@(cc) -E -x c - -v)
                         :input :null :output :null :error :pipe)]
         [paths ($ map string-trim-both
                   $ take-while (complement #/End of search list/)
                   $ cdr
                   $ drop-while (complement #/include <...> search starts here/)
                   $ port->string-list (process-error p))])
    (process-wait p)
    (let1 status (process-exit-status p)
      (if (and (sys-wait-exited? status)
               (zero? (sys-wait-exit-status status)))
        paths
        '()))))

;; API
(define (c-find-header name :optional (includes '()))
  (find-file-in-paths name
                      :paths (append includes (cpp-include-paths))
                      :pred file-is-readable?))
