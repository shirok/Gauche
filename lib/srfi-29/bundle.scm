;;;
;;; srfi-29 - Localization
;;;
;;;  Alex Shinn
;;;

;; This module provides basic support for srfi-29 API.
;;
;; load-bundle! and store-bundle! don't actually load/store the bundle
;; and they return #f (permitted in srfi-29 spec).

(define-module srfi-29.bundle
  (use srfi-13)
  (use gauche.parameter)
  (export current-language current-country current-locale-details
          load-bundle! store-bundle! declare-bundle! localized-template))
(select-module srfi-29.bundle)

;; bundle specifiers are (package ... [country] lang)
(define *bundles* (make-hash-table 'equal?))

;; implement as parameters (default to something other than en?)
(define current-language (make-parameter 'en))
(define current-country (make-parameter 'us))
(define current-locale-details (make-parameter '()))

;; initialize locale from LANG env variable if defined
(let ((lang (sys-getenv "LANG")))
  (cond ((and lang (#/^(\w+)(?:[-_](\w+))?(?:\.(.*))?$/ lang))
         => (lambda (m)
              (current-language (string->symbol (m 1)))
              (cond ((m 2)
                     => (lambda (x)
                          (current-country
                           (string->symbol (string-downcase x))))))
              (cond ((m 3)
                     => (lambda (x)
                          (current-locale-details
                           (list (string->symbol (string-downcase x)))))))))))

;; possibly tie these in with text.gettext
(define (load-bundle! bundle-specifier) #f)
(define (store-bundle! bundle-specifier) #f)

;; could also use read/write, but doesn't have tool support like gettext
; (use file.util)
; (define (bundle->file x)
;   (apply build-path (gauche-library-directory) "srfi-29/bundles" x))
; (define (read-from-file file)
;   (and (file-exists? file)
;        (with-error-handler (lambda (err . opt) (warn err) #f)
;          (with-input-from-file file read))))
; (define (write-to-file file obj)
;   (with-error-handler (lambda (err . opt) (warn err) #f)
;     (with-output-to-file file (cut write obj))))
; (define (load-bundle! bundle-specifier)
;   (cond ((read-from-file (bundle->file bundle-specifier))
;          => (lambda (ls)
;               (hash-table-put! *bundles* bundle-specifier
;                                (alist->hash-table ls))))))
; (define (store-bundle! bundle-specifier)
;   (write-to-file (bundle->file bundle-specifier)
;                  (hash-table-get *bundles* bundle-specifier)))

;; declare a bundle of templates with a given bundle specifier
(define (declare-bundle! bundle-specifier bundle-assoc-list)
  (hash-table-put! *bundles* bundle-specifier
                   (alist->hash-table bundle-assoc-list)))

;; lookup a name in a given package
(define (localized-template package-name template-name)
  (define (rdc ls)
    (cond ((null? ls) '())
          ((null? (cdr ls)) '())
          (else (cons (car ls) (rdc (cdr ls))))))
  (let loop ((name (list package-name (current-language) (current-country))))
    (let ((bundle (hash-table-get *bundles* name #f)))
      (or (and bundle (hash-table-get bundle template-name #f))
          (let ((next (rdc name)))
            (and (pair? next) (loop next)))))))

