;;;
;;; gauche.package - package management
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
;;;  $Id: package.scm,v 1.3 2004-04-23 04:46:37 shirok Exp $
;;;

;; *EXPERIMENTAL*
;; The spec might be changed in future.  Do not count on the current version.
;;
;; This module maintains information about the Gauche "packages" installed
;; in the system.  A Gauche package is a collection of files such as
;; Scheme library files, DSO files, scripts, etc. to add a specific
;; feature to Gauche.  Examples of packages are Gauche-gl and Gauche-gtk.
;;
;; Information of a package foo is kept in a package description file.
;; It is put in "$(SCM_INSTALL_DIR)/.packages/foo.gpd", where SCM_INSTALL_DIR
;; is the directory where Scheme library files of foo are installed.
;; (The suffix .gpd stands for "Gauche package description").
;;
;; The package description file contains a bunch of S-exprs that records
;; how the package was built, what it provides and depends on,
;; and which files it owns.
;;
;; Note that this module doesn't handle build and installation process.
;; This module just provides a means to create and access the package
;; description files; gauche.package.build module will handle the build and
;; installation, and gauche.package.fetch module will handle downloading
;; package tarballs, and so on.
;;
;; [Package description file]
;;   For the time being, it contains just one S-expression.
;;
;;   (define-gauche-package NAME
;;     :version VERSION
;;     ...)
;;
;;   NAME _must_ match the filename sans suffix.
;;

(define-module gauche.package
  (use srfi-1)
  (use srfi-2)
  (use gauche.collection)
  (use file.util)
  (export <gauche-package-description>
          path->gauche-package-description
          gauche-package-description-paths
          write-gauche-package-description
          find-gauche-package-description
          ))
(select-module gauche.package)

;;;
;;; Reading .gpd contents
;;;

(define-class <gauche-package-description> ()
  (;; package name, e.g. "Gauche-gtk"
   (name           :init-keyword :name)
   ;; package version, e.g. "0.4.1"
   (version        :init-keyword :version
                   :init-value "0.0")
   ;; gauche version with which this package was built
   (gauche-version :init-keyword :gauche-version
                   :init-value (gauche-version))
   ;; configure command line with which this package was built
   (configure      :init-keyword :configure
                   :init-value #f)
   ))

(define (path->gauche-package-description path)
  (with-error-handler
      (lambda (e)
        (error "couldn't read the package description ~s: ~a" path
               (ref e 'message)))
    (lambda ()
      (call-with-input-file path
        (lambda (in)
          (let ((f (read in)))
            (if (and (list? f)
                     (>= (length f) 2)
                     (eq? (car f) 'define-gauche-package)
                     (string? (cadr f)))
              (apply make <gauche-package-description>
                     :name (cadr f) (cddr f))
              (error "malformed define-gauche-package"))))))
    ))

(define-method write-gauche-package-description
    ((desc <gauche-package-description>) . maybe-port)
  (let ((out (get-optional maybe-port (current-output-port))))
    (format out ";; -*-Scheme-*-\n")
    (format out ";; Gauche Package Description\n\n")
    (format out "(define-gauche-package ~s\n" (ref desc 'name))
    (for-each (lambda (slot)
                (unless (eq? slot 'name)
                  (format out "  :~a ~s\n" slot (ref desc slot))))
              (map slot-definition-name
                   (class-slots <gauche-package-description>)))
    (format out ")\n")))

;;;
;;; Searching .gpd files
;;;

;;; e.g.  (find (lambda (path) ....) (gauche-package-description-paths))
;;;       (coerce-to <list> (gauche-package-description-paths))
;;;       (map path->gauche-package-description
;;;            (gauche-package-description-paths))

;; A pseudo collection to traverse package description filenames.
(define-class <gauche-package-description-paths> (<collection>)
  ())

;; Returns a singleton of <gauche-package-descriptions>.
(define gauche-package-description-paths
  (let ((singleton #f))
    (lambda ()
      (or singleton
          (begin
            (set! singleton (make <gauche-package-description-paths>))
            singleton)))))

(define-method call-with-iterator ((gpd <gauche-package-description-paths>)
                                   proc . args)
  (let ((paths *load-path*)
        (files '()))
    (define (pick-next)
      (if (null? files)
        (let loop ()
          (if (null? paths)
            #f
            (let ((dir (build-path (pop! paths) ".packages")))
              (cond ((file-is-directory? dir)
                     (set! files
                           (directory-list dir
                                           :add-path? #t :filter #/\.gpd$/))
                     (pick-next))
                    (else (loop))))))
        (pop! files)))
    (let ((buf (pick-next)))
      (define (end?) (not buf))
      (define (next) (begin0 buf (set! buf (pick-next))))
      (proc end? next))))

;; utility
(define (find-gauche-package-description name)
  (and-let* ((path (find (string->regexp #`"/,|name|\\.gpd$")
                         (gauche-package-description-paths))))
    (path->gauche-package-description path)))

(provide "gauche/package")
