;;;
;;; gauche.package - package management
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

;; *EXPERIMENTAL*
;; The spec might be changed in future.  Do not count on the current version.
;;
;; This module handles package meta-information.
;;
;; Module source can have package.scm in the toplevel directory of
;; the source.  It contains define-gauche-package form, that defines
;; dependencies, where to find source, etc.
;;
;; When the package is built, the configure script will generate
;; a Gauche package description file, which combines the information
;; in package.scm and the build information.  It is installed as
;; "$(SCM_INSTALL_DIR)/.packages/$(PACKAGE).gpd", where SCM_INSTALL_DIR
;; is the directory where Scheme library files of foo are installed.
;; (The suffix .gpd stands for "Gauche package description").
;;
;; Note that this module doesn't handle build and installation process.
;; This module just provides a means to create and access the package
;; description files; gauche.package.build module will handle the build and
;; installation, and gauche.package.fetch module will handle downloading
;; package tarballs, and so on.
;;
;; package.scm and *.gpd file contains a single define-gauche-package
;; form.  It takes the name of the package, followed by keyword-value
;; style attribute specifications.  Attributes marked by '*' can
;; be specified zero or more times; '?' can be zero or one time.
;; The order of attributes doesn't matter, except that the order of
;; the same attribute is preserved (e.g. if package.scm has
;; :author "foo" :author "bar", they appear in the same order in
;; the package's installed gpd file.)
;;
;;   (define-gauche-package NAME
;;     :version VERSION                     ; version of this module
;;     :require ((PACKAGE VERSION-SPEC)...) ; dependency
;;     :maintainers (STRING ...)            ;
;;     :authors (STRING ...)
;;     :licenses (STRING ...)
;;     :homepage URI-STRING
;;     :repository URI-STRING
;;     :description STRING
;;     :providing-modules (SYMBOL ..)     ; list of providing modules
;;
;;     ;; The following attributes are added when *.gpd file is generated.
;;     :gauche-version VERSION            ; Gauche version used to build
;;     :configure STRING                  ; configure option given
;;
;;     ;; In the *.gpd file, the attributes marked with '*' above
;;     ;; are consolidated to the following plural attributes.
;;     
;;     )
;;
;;   NAME _must_ match the filename sans suffix.
;;   VERSION-SPEC is as specified in gauche.version

;; NB: We intentionally use long names for the APIs, since this module isn't
;; for general public consumption.

(define-module gauche.package
  (use gauche.collection)
  (use gauche.version)
  (use gauche.lazy)
  (use file.util)
  (use util.match)
  (export <gauche-package-description>
          make-gauche-package-description
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
;; NB: All slots are supposed to be initialized with init-keyword, by
;; make-gauche-package-description.  We provide init-values only for
;; the backward compatibility with the configure scripts written for
;; 0.9.4.
(define-class <gauche-package-description> ()
  ((name           :init-keyword :name        :init-value #f)
   (version        :init-keyword :version     :init-value #f)
   (description    :init-keyword :description :init-value #f)
   (require        :init-keyword :require     :init-value '())
   (maintainers    :init-keyword :maintainers :init-value '())
   (authors        :init-keyword :authors     :init-value '())
   (licenses       :init-keyword :licenses    :init-value '())
   (homepage       :init-keyword :homepage    :init-value #f)
   (repository     :init-keyword :repository  :init-value #f)
   (providing-modules :init-keyword :providing-modules :init-value '())
   ;; The slots filled by cf-make-gpd
   (gauche-version :init-keyword :gauche-version :init-form (gauche-version))
   (configure      :init-keyword :configure   :init-value #f)
   ))

;; API
(define (make-gauche-package-description name
                                         :key (version "0.0")
                                              (description #f)
                                              (require '())
                                              (maintainers '())
                                              (authors '())
                                              (licenses '())
                                              (homepage #f)
                                              (repository #f)
                                              (gauche-version (gauche-version))
                                              (configure #f)
                                              (providing-modules '())
                                         :allow-other-keys unknown-keys)
  (when (not (null? unknown-keys))
    (warn "Package description has unrecognized key-value pairs: ~s\n"
          unknown-keys))
  (check-require-syntax require)
  (check-string-list 'maintainers maintainers)
  (check-string-list 'authors authors)
  (check-string-list 'licenses licenses)
  (check-maybe-string 'homepage homepage)
  (check-maybe-string 'repository repository)
  (check-maybe-string 'description description)
  (check-symbol-list 'providing-modules providing-modules)
  (make <gauche-package-description>
    :name name :version version :require require :maintainers maintainers
    :authors authors :licenses licenses :homepage homepage
    :repository repository :description description
    :gauche-version gauche-version :configure configure
    :providing-modules providing-modules))

(define (check-maybe-string key val)
  (unless (or (string? val) (not val))
    (errorf "String or #f is required for ~a, but got: ~s" key val)))

(define (check-string-list key val)
  (unless (every string? val)
    (errorf "String list is required for ~a, but got: ~s" key val)))

(define (check-symbol-list key val)
  (unless (every symbol? val)
    (errorf "Symbol list is required for ~a, but got: ~s" key val)))

(define (check-require-syntax req)
  (define (check-require-1 clause)
    (match clause
      [((? string?) v)
       (unless (valid-version-spec? v)
         (error "Invalid version spec in require clause:" clause))]
      [else (error "Invalid require clause:" clause)]))
  (match req
    [(x ...) (for-each check-require-1 x)]
    [else (error "Invalid require form:" req)]))

;; API
(define (path->gauche-package-description path)
  (guard (e [(or (<io-error> e) (<read-error> e))
             (errorf "couldn't read the package description ~s: ~a"
                     path (ref e 'message))])
    (match (file->sexp-list path)
      [(('define-gauche-package (? string? name) . attrs))
       (apply make-gauche-package-description name attrs)]
      [_ (errorf "malformed gauche package description file. \
                  It must contain a define-gauche-package form and \
                  nothing else: ~a" path)])))

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

;; scan the directory to find older version of Gauche library directories.
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
  (and-let* ([gpdfile #"~|name|.gpd"]
             [path (find (^p (equal? gpdfile (sys-basename p)))
                         (gauche-package-description-paths
                          :all-versions all-versions))])
    (path->gauche-package-description path)))
