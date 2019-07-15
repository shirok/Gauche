;;;
;;; gauche.package.util - internal utilities used in package manager
;;;
;;;   Copyright (c) 2004-2019  Shiro Kawai  <shiro@acm.org>
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

;;; NB: this module is not intended for external use.

(define-module gauche.package.util
  (use gauche.process)
  (use gauche.parameter)
  (use gauche.termios)
  (use gauche.package)
  (use util.match)
  (use file.util)
  (use srfi-13)
  (use srfi-14)
  (export run dry-run verbose-run get-password
          find-package-name-and-version))
(select-module gauche.package.util)

(define dry-run     (make-parameter #f))
(define verbose-run (make-parameter #f))

(define (run cmdline :key (stdin-string #f))
  (when (or (dry-run) (verbose-run))
    (print cmdline))
  (unless (dry-run)
    (let1 p (run-process (cond-expand
                          [gauche.os.windows (shell-tokenize-string cmdline 'posix)]
                          [else `("/bin/sh" "-c" ,cmdline)])
                         :input (if stdin-string :pipe :null)
                         :wait #f)
      (when stdin-string
        (let1 pi (process-input p)
          (display stdin-string pi)
          (flush pi)
          (close-output-port pi)))
      (process-wait p)
      (unless (zero? (process-exit-status p))
        (errorf "command execution failed: ~a" cmdline)))))

;; Read password from the terminal without echoing
(define (get-password)
  (with-output-to-file
      (cond-expand [gauche.os.windows "CON"] [else "/dev/tty"])
    (lambda () (display "Password: ") (flush)))
  (without-echoing #f read-line))

;; Determine package name and version heuristically
;; - If we have package.scm, just use it.
;; - If we have autoconf configure.ac, look for AC_INIT.
;; - If we have Scheme configure script, look for cf-init.
;; - Otherwise, look at the current directory name and VERSION file.
(define (find-package-name-and-version :key (top-srcdir "."))
  (or (and-let* ([f (build-path top-srcdir "package.scm")]
                 [ (file-exists? f) ]
                 [desc (path->gauche-package-description f)])
        (list (~ desc 'name)
              (~ desc 'version)))
      (and-let* ([f (build-path top-srcdir "configure.ac")]
                 [ (file-exists? f) ]
                 [x (find #/^\s*AC_INIT\(/ (file->string-list f))])
        (rxmatch-case x
          [#/AC_INIT\(\s*([\w-.]+)\s*,(?:\s*([\w-.]+))?/ (_ pkg ver)
           (list pkg ver)]
          [else #f]))
      (and-let* ([f (build-path top-srcdir "configure")]
                 [ (file-exists? f) ]
                 [s (guard (e [else #f]) (file->sexp-list f))]
                 [t (find (^x (and (pair? x) (eq? (car x) 'cf-init))) s)])
        (match t
          [('cf-init pkg ver . _) (list pkg ver)]
          [_ #f]))
      (and-let* ([vf (build-path top-srcdir "VERSION")]
                 [ (file-exists? vf) ]
                 [ver (string-trim-both (file->string vf))])
        (list ($ sys-basename $ sys-dirname $ simplify-path
                 $ sys-normalize-pathname vf :absolute #t)
              ver))))
