;;;
;;; gauche.package.build - build a package
;;;  
;;;   Copyright (c) 2004 Shiro Kawai, All rights reserved.
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
;;;  $Id: build.scm,v 1.3 2004-04-23 06:17:46 shirok Exp $
;;;

;; *EXPERIMENTAL*
;; gauche.package.build module is intended to automate standard building
;; process (untar+configure+make+make install).

(define-module gauche.package.build
  (use srfi-1)
  (use srfi-2)
  (use gauche.package)
  (use gauche.package.util)
  (use gauche.package.fetch)
  (use gauche.parameter)
  (use file.util)
  (use util.list)
  (export gauche-package-build))
(select-module gauche.package.build)

;; Default programs
(define *cat-program*   (find-file-in-paths "cat"))
(define *tar-program*   (find-file-in-paths "tar"))
(define *gzip-program*  (find-file-in-paths "gzip"))
(define *bzip2-program* (find-file-in-paths "bzip2"))
(define *make-program*  (or (find-file-in-paths "gmake")
                            (find-file-in-paths "make")))
(define *rm-program*    (find-file-in-paths "rm"))

(define (untar config file)
  (let ((build-dir (assq-ref config 'build-dir "."))
        (cat   (assq-ref config 'cat   *cat-program*))
        (tar   (assq-ref config 'tar   *tar-program*))
        (gzip  (assq-ref config 'gzip  *gzip-program*))
        (bzip2 (assq-ref config 'bzip2 *bzip2-program*)))
    (rxmatch-case (sys-basename file)
      (#/(?:\.tar\.gz|\.tgz|\.taz)$/ (#f)
       (run #`"\",cat\" \",file\" | \",gzip\" -d | \",tar\" xfC - \",build-dir\""))
      (#/(?:\.tar\.bz|\.tar\.bz2|\.tbz|\.tbz2)$/ (#f)
       (run #`"\",cat\" \",file\" | \",bzip2\" -d | \",tar\" xfC - \",build-dir\""))
      (#/\.tar$/ (#f)
       (run #`"\",tar\" xfC \",file\" \",build-dir\""))
      (else
       (error "can't decide the package format of " file)))))

(define (tarball->package-directory file)
  (cond
   ((#/(.*)(?:\.tar\.gz|\.tgz|\.taz|\.tar\.bz|\.tar\.bz2|\.tbz|\.tbz2|\.tar)$/
       (sys-basename file))
    => (lambda (m) (m 1)))
   (else
    (error "can't determine package directory from tarball " file))))

;; when reconfiguring, give package name.
(define (configure config dir package-name configure-options)
  (let1 conf-cmd
      (cond
       (configure-options #`"./configure ,configure-options")
       ((and-let* ((package-name)
                   (gpd (find-gauche-package-description package-name)))
          (ref gpd 'configure)))
       (else "./configure"))
    (run #`"cd \",dir\"; ,conf-cmd")))

(define (make config dir)
  (let ((make (assq-ref config 'make *make-program*)))
    (run #`"cd \",dir\"; \",make\"")))

(define (make-check config dir)
  (let ((make (assq-ref config 'make *make-program*)))
    (run #`"cd \",dir\"; \",make\" check")))

(define (make-install config dir)
  (let ((make (assq-ref config 'make *make-program*)))
    (run #`"cd \",dir\"; \",make\" install")))

(define (clean config dir)
  (let ((rm (assq-ref config 'rm *rm-program*)))
    (run #`"\",rm\" -rf \",dir\"")))

;; extracting package name from directory name (PACKAGE-VERSION)
(define (package-name basename)
  (let1 s (string-split basename #\-)
    (when (or (null? s) (null? (cdr s)))
      (error "weird package directory name:" basename))
    (string-join (drop-right s 1) "-")))

;;;
;;; Driver
;;;

(define (gauche-package-build uri . opts)
  (let-keywords* opts ((config  '())
                       (configure-options #f)
                       (install-only? :install-only #f)
                       (dry?         :dry-run #f)
                       (reconfigure? :reconfigure #f)
                       (check?       :check #t)
                       (install?     :install #f)
                       (clean?       :clean #f))
    (parameterize ((dry-run dry?))
      (let* ((tarball   (gauche-package-ensure uri :config config))
             (build-dir (assq-ref config 'build-dir "."))
             (basename  (tarball->package-directory tarball))
             (dir       (build-path build-dir basename))
             (packname  (package-name basename)))
        (unless install-only?
          (untar config tarball)
          (configure config dir packname configure-options)
          (make config dir)
          (when check?   (make-check config dir)))
        (when (or install? install-only?)
          (make-install config dir))
        (when clean?   (clean config dir))))))

(provide "gauche/package/build")
