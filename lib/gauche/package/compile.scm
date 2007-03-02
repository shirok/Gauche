;;;
;;; gauche.package.compile - compile extensions
;;;  
;;;   Copyright (c) 2005-2007  Shiro Kawai  <shiro@acm.org>
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
;;;  $Id: compile.scm,v 1.8 2007-03-02 07:39:09 shirok Exp $
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
  (use file.util)
  (export gauche-package-compile-and-link
          gauche-package-compile
          gauche-package-link
          gauche-package-clean))
(select-module gauche.package.compile)

(define GOSH     (build-path (gauche-architecture-directory) "gosh"))
(define CONFIG   (build-path (gauche-architecture-directory) "gauche-config"))
(define CC       (gauche-config "--cc"))
(define CFLAGS   (gauche-config "--so-cflags"))
(define INCDIR   (gauche-config "-I"))
(define LIBDIR   (gauche-config "-L"))
(define LIBS     (gauche-config "-l"))
(define OBJEXT   (gauche-config "--object-suffix"))
(define SOEXT    (gauche-config "--so-suffix"))
(define LDFLAGS  (gauche-config "--so-ldflags"))

(define (gauche-package-compile file . args)
  (let-keywords args ((output #f)
                      (cppflags #f)
                      (cflags   #f)
                      (cc #f)
                      (ld #f)           ;; dummy
                      (ldflags #f)      ;; dummy
                      (libs #f)         ;; dummy
                      (dry? :dry-run #f)
                      (verb? :verbose #f))
    (parameterize ((dry-run dry?)
                   (verbose-run verb?))
      (let1 ofile (or output (path-swap-extension file OBJEXT))
        (unless (and (file-exists? ofile)
                     (file-mtime>? ofile file))
          (if (equal? (path-extension file) "stub")
            (let1 cfile (path-swap-extension file "c")
              (unwind-protect
                  (begin
                    (do-genstub file)
                    (do-compile (or cc CC) cfile ofile
                                (or cppflags "") (or cflags "")))
                (sys-unlink cfile)))
            (do-compile (or cc CC) file ofile
                        (or cppflags "") (or cflags ""))))))))

(define (do-genstub stubfile)
  (run #`"',GOSH' genstub ,stubfile"))
      
(define (do-compile cc cfile ofile cppflags cflags)
  (run #`",cc -c ,cppflags ,INCDIR ,cflags ,CFLAGS -o ',ofile' ',cfile'"))

(define (gauche-package-link sofile ofiles . args)
  (let-keywords args ((ldflags #f)
                      (libs #f)
                      (ld #f)
                      (output #f)       ;; dummy
                      (cppflags #f)     ;; dummy
                      (cflags #f)       ;; dummy
                      (cc #f)           ;; dummy
                      (dry? :dry-run #f)
                      (verb? :verbose #f))
    (parameterize ((dry-run dry?)
                   (verbose-run verb?))
      (unless (and (file-exists? sofile)
                   (every (cut file-mtime>? sofile <>) ofiles))
        (let1 all-ofiles (string-join (map (lambda (f) #`"',f'") ofiles) " ")
          (run #`",(or ld CC) ,(or ldflags \"\") ,LIBDIR ,LDFLAGS ,sofile ,all-ofiles ,LIBS ,(or libs \"\")"))))))

(define (gauche-package-compile-and-link module-name files . args)
  (let ((head.c #`",|module-name|_head.c")
        (tail.c #`",|module-name|_tail.c")
        (sofile (or (get-keyword :output args #f)
                    #`",|module-name|.,|SOEXT|")))
    (parameterize ((dry-run (get-keyword :dry-run args #f))
                   (verbose-run (get-keyword :verbose args #f)))
      (guard (e (else (sys-unlink head.c)
                      (sys-unlink tail.c)
                      (sys-unlink sofile)
                      (raise e)))
        (run #`"',CONFIG' --fixup-extension ',module-name'")
        (let1 objs (map (lambda (src)
                          (cond
                           ((equal? (path-extension src) OBJEXT) src)
                           (else (apply gauche-package-compile src args)
                                 (path-swap-extension src OBJEXT))))
                        `(,head.c ,@files ,tail.c))
          (apply gauche-package-link sofile objs args)
          sofile)))))

(define (gauche-package-clean module-name files . args)
  (let-keywords args ((output #f))
    (when module-name
      (sys-unlink #`",|module-name|_head.c")
      (sys-unlink #`",|module-name|_tail.c")
      (sys-unlink #`",|module-name|_head.,|OBJEXT|")
      (sys-unlink #`",|module-name|_tail.,|OBJEXT|")
      (sys-unlink #`",|module-name|.,|SOEXT|"))
    (when output
      (sys-unlink output))
    (dolist (f files)
      (unless (equal? (path-extension f) OBJEXT)
        (sys-unlink (path-swap-extension f OBJEXT))))))

(provide "gauche/package/compile")
