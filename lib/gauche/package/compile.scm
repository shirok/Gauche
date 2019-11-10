;;;
;;; gauche.package.compile - compile extensions
;;;
;;;   Copyright (c) 2005-2019  Shiro Kawai  <shiro@acm.org>
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

;; *EXPERIMENTAL*
;; gauche.package.compile kicks C compiler/linker to compile an
;; extension file.

(define-module gauche.package.compile
  (use srfi-1)
  (use gauche.package)
  (use gauche.package.util)
  (use gauche.config)
  (use gauche.parameter)
  (use gauche.cgen.cise)
  (use gauche.cgen.stub)
  (use file.util)
  (export gauche-package-compile-and-link
          gauche-package-compile
          gauche-package-link
          gauche-package-clean))
(select-module gauche.package.compile)

;; If we use Gauche that's not installed yet, this parameter contains
;; its top builddir.  We intercept INCDIR and LIBDIR
(define in-place-dir (make-parameter #f))

(define CC       (gauche-config "--cc"))
(define CFLAGS   (gauche-config "--so-cflags"))
(define (INCDIR) (filter-dir "I" (gauche-config "--incdirs") "--sysincdir"))
(define (LIBDIR) (filter-dir "L" (gauche-config "--archdirs") "--sysarchdir"))
(define LIBS     (gauche-config "-l"))
(define OBJEXT   (gauche-config "--object-suffix"))
(define SOEXT    (gauche-config "--so-suffix"))
(define LDFLAGS  (gauche-config "--so-ldflags"))

(define (gauche-package-compile file :key (output #f)
                                          (cppflags #f)
                                          (cflags   #f)
                                          (cc #f)
                                          (gauche-builddir #f)
                                          (keep-c #f)
                                          (no-line #f)
                                          (ld #f)      ; dummy
                                          (ldflags #f) ; dummy
                                          (libs #f)    ; dummy
                                          ((:dry-run dry?) #f)
                                          ((:verbose verb?) #f))
  (parameterize ([dry-run dry?]
                 [verbose-run verb?]
                 [in-place-dir gauche-builddir]
                 [cise-emit-source-line (not no-line)])
    (let1 ofile (or output (sys-basename (path-swap-extension file OBJEXT)))
      (unless (and (file-exists? ofile)
                   (file-mtime>? ofile file))
        (if (equal? (path-extension file) "stub")
          (let1 cfile (path-swap-extension file "c")
            (unwind-protect
                (begin (cgen-genstub file)
                       (do-compile (or cc CC) cfile ofile
                                   (or cppflags "") (or cflags "")))
              (unless keep-c (sys-unlink cfile))))
          (do-compile (or cc CC) file ofile
                      (or cppflags "") (or cflags "")))))))

(define (do-compile cc cfile ofile cppflags cflags)
  (run #"~cc -c ~cppflags ~(INCDIR) ~cflags ~CFLAGS -o '~ofile' '~cfile'"))

(define (gauche-package-link sofile ofiles :key (ldflags #f)
                                                (libs #f)
                                                (ld #f)
                                                (gauche-builddir #f)
                                                (keep-c #f)   ; dummy
                                                (no-line #f)  ; dummy
                                                (output #f)   ; dummy
                                                (cppflags #f) ; dummy
                                                (cflags #f)   ; dummy
                                                (cc #f)       ; dummy
                                                ((:dry-run dry?) #f)
                                                ((:verbose verb?) #f))
  (parameterize ([dry-run dry?]
                 [verbose-run verb?]
                 [in-place-dir gauche-builddir])
    (unless (and (file-exists? sofile)
                 (every (cut file-mtime>? sofile <>) ofiles))
      (let1 all-ofiles (string-join (map (^f #"'~f'") ofiles) " ")
        (run #"~(or ld CC) ~(or ldflags \"\") ~(LIBDIR) ~LDFLAGS ~sofile ~all-ofiles ~LIBS ~(or libs \"\")")))))

(define (gauche-package-compile-and-link module-name files . args)
  (let1 sofile (or (get-keyword :output args #f)
                   #"~|module-name|.~|SOEXT|")
    (parameterize ([dry-run (get-keyword :dry-run args #f)]
                   [verbose-run (get-keyword :verbose args #f)])
      (guard (e [else (sys-unlink sofile)
                      (raise e)])
        (let1 objs (map (lambda (src)
                          (cond
                           [(equal? (path-extension src) OBJEXT) src]
                           [else (apply gauche-package-compile src
                                        (delete-keyword :output args))
                                 (sys-basename (path-swap-extension src OBJEXT))]))
                        files)
          (apply gauche-package-link sofile objs args)
          sofile)))))

(define (gauche-package-clean module-name files :key (output #f))
  (when module-name
    (sys-unlink #"~|module-name|.~|SOEXT|"))
  (when output
    (sys-unlink output))
  (dolist (f files)
    (unless (equal? (path-extension f) OBJEXT)
      (sys-unlink (sys-basename (path-swap-extension f OBJEXT))))))

;; Adjust pathnames in INCDIR and LIBDIR
;;   If we're running compiler in-place during testing, we replace
;;   the paths 'gauche-config --incdir' or 'gauche-config --libdir' returns
;;   with the source tree directories.
(define (filter-dir flag olddirs dir-key)
  (define sep (cond-expand [gauche.os.windows ";"][else ":"]))
  (define dirs
    (or (and-let* ([to-dir   (in-place-dir)]
                   [orig-dir (regexp-quote (gauche-config dir-key))]
                   ;; NB: we wrap to-dir by closure to for the case if
                   ;; to-dir includes submatch replacement spec.
                   [new (regexp-replace-all (string->regexp orig-dir) olddirs
                                            (^m #"~|to-dir|/src"))])
          (if (equal? dir-key "--sysincdir")
            #"~|new|~|sep|~|to-dir|/gc/include"
            new))
        olddirs))
  ;; Return "-I<path> -I<path> ..." or "-L<path> -L<path> ...".
  ;; We exclude nonexistent paths, for OSX complains about it.
  (string-join (map (^s #"'-~|flag|~|s|'")
                    (filter file-exists? (string-split dirs sep)))
               " "))
