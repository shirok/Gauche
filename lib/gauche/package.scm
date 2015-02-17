;;;
;;; gauche.package - package management
;;;
;;;   Copyright (c) 2004-2015  Shiro Kawai  <shiro@acm.org>
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
  (use gauche.collection)
  (use gauche.version)
  (use gauche.lazy)
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

;; API
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

;; API
(define (path->gauche-package-description path)
  (guard (e [(or (<io-error> e) (<read-error> e))
             (errorf "couldn't read the package description ~s: ~a"
                     path (ref e 'message))])
    (call-with-input-file path
      (^[in]
        (let1 f (read in)
          (if (and (list? f)
                   (>= (length f) 2)
                   (eq? (car f) 'define-gauche-package)
                   (string? (cadr f)))
            (apply make <gauche-package-description>
                   :name (cadr f) (cddr f))
            (error "malformed define-gauche-package")))))))

;; API
(define-method write-gauche-package-description
    ((desc <gauche-package-description>) :optional (out (current-output-port)))
  (format out ";; -*-Scheme-*-\n")
  (format out ";; Gauche Package Description\n\n")
  (format out "(define-gauche-package ~s\n" (ref desc 'name))
  (for-each (^[slot] (unless (eq? slot 'name)
                       (format out "  :~a ~s\n" slot (ref desc slot))))
            (map slot-definition-name
                 (class-slots <gauche-package-description>)))
  (format out ")\n"))

;;;
;;; Searching .gpd files
;;;

;; API.  Returns a lazy list.
(define (gauche-package-description-paths :key (all-versions #f))
  (let1 g (%gpd-path-generator (if all-versions
                                 (%get-all-version-paths)
                                 *load-path*))
    (generator->lseq g)))

;; scan the directory to find older verison of Gauche library directories.
(define (%get-all-version-paths)
  (apply append
         *load-path*
         (filter-map
          (^p (and-let* ([m (#/\/\d+(\.\d+)*[^\/]*\/lib\/?$/ p)]
                         [base (m 'before)]
                         [ (file-is-directory? base) ]
                         [dirs (directory-list base
                                               :children? #t :add-path? #t
                                               :filter #/^\d+(\.\d+)*[^\/]*$/)]
                         [pdirs (map (cut string-append <> "/lib") dirs)])
                (sort-by (delete p pdirs) sys-basename version>?)))
          *load-path*)))

(define (%gpd-path-generator paths)
  (let ([files '()]
        [visited (make-hash-table 'string=?)])
    (define (interesting? path)
      (and (#/\.gpd$/ path)
           (not (hash-table-get visited (sys-basename path) #f))
           (begin (hash-table-put! visited (sys-basename path) #t)
                  #t)))
    (define (pick-next)
      (if (null? files)
        (let loop ()
          (if (null? paths)
            #f
            (let1 dir (build-path (pop! paths) ".packages")
              (cond [(file-is-directory? dir)
                     (set! files
                           (directory-list dir
                                           :add-path? #t :filter interesting?))
                     (pick-next)]
                    [else (loop)]))))
        (pop! files)))
    (let1 buf (pick-next)
      (^[] (if (not buf)
             (eof-object)
             (begin0 buf (set! buf (pick-next))))))))

;; API.  utility
(define (find-gauche-package-description name :key (all-versions #f))
  (and-let* ([path (find (string->regexp #`"/,|name|\\.gpd$")
                         (gauche-package-description-paths
                          :all-versions all-versions))])
    (path->gauche-package-description path)))
