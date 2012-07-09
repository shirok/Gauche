;;;
;;; reload.scm - reloading modules
;;;
;;;  Copyright(C) 2002 by Alex Shinn <foof@synthcode.com>
;;;
;;;  Permission to use, copy, modify, distribute this software and
;;;  accompanying documentation for any purpose is hereby granted,
;;;  provided that existing copyright notices are retained in all
;;;  copies and that this notice is included verbatim in all
;;;  distributions.
;;;  This software is provided as is, without express or implied
;;;  warranty.  In no circumstances the author(s) shall be liable
;;;  for any damages arising out of the use of this software.
;;;

;;; Created:    <2002-11-06 16:02:55 foof>
;;; Time-stamp: <2002-11-13 10:11:58 foof>
;;; Author:     Alex Shinn <foof@synthcode.com>
;;; Slightly modified by Shiro Kawai

;; Development-only utility to reload modules while retaining data.

(define-module gauche.reload
  (use file.util)
  (use srfi-13)
  (use gauche.libutil)
  (use gauche.parameter)
  (export reload reload-modified-modules
          module-reload-rules reload-verbose)
  )
(select-module gauche.reload)

;; share the internal utility
(define module-glob-pattern->regexp
  (with-module gauche.libutil module-glob-pattern->regexp))

;; parameter reload-verbose
;;   If true, reload and reload-modified-modules reports what they are
;;   doing.
(define reload-verbose (make-parameter #f))

;; parameter module-reload-rules
;;   <rules>       : (<mod-rule> ...)
;;   <mod-rule>    : (<mod-pattern> <rule> ...)
;;   <mod-pattern> : symbol module name, or symbol containing glob pattern
;;   <rule>        : procedure | symbol | regexp
;;                 | (and <rule> ...)
;;                 | (or  <rule> ...)
;;                 | (not <rule>)
(define module-reload-rules (make-parameter '()))

;; find-file-in-paths from file.util doesn't work on foo/bar searches
(define (find-in-path file path)
  (find file-is-regular?
        (map (^d (sys-normalize-pathname (string-append d "/" file))) path)))

;; procedure reload <module-name> &optional <rule> ...
;;
(define (reload module-name . predicates)
  (let1 mod (find-module module-name)
    (if (not mod)
      (warn "no such module: ~S" module-name)
      (let ([table (module-table mod)]
            [saves (make-hash-table 'eq?)])
        ;; remember data
        (if (not (null? predicates))
          (hash-table-for-each
           table
           (^[sym gloc]
             (let1 value (eval sym mod)
               (define (check keep?)
                 (cond [(procedure? keep?) (keep? value)]
                       [(symbol? keep?) (eq? keep? sym)]
                       [(regexp? keep?)
                        (rxmatch keep? (symbol->string sym))]
                       [(null? keep?) #f]
                       [(pair? keep?)
                        (case (car keep?)
                          [(and) (every check (cdr keep?))]
                          [(or)  (any check (cdr keep?))]
                          [(not) (not (check (cdr keep?)))]
                          [else ;; just recurse on other lists
                           (or (check (car keep?))
                               (check (cdr keep?)))])]
                       [else (error "invalid predicate: " keep?)]))
               (let loop ([preds predicates])
                 (if (not (null? preds))
                   (if (check (car preds))
                     (begin
                       (when (reload-verbose)
                         (format #t "keeping value of ~S\n" sym))
                       (hash-table-put! saves sym value))
                     (loop (cdr preds)))))))))
        ;; reload
        (load (module-name->path module-name))
        ;; restore any remembered data
        (hash-table-for-each
         saves
         (^[sym value] (eval `(set! ,sym (quote ,value)) mod)))))))

;; procedure reload-modified-modules &optional <reload-rules>
;;   Reloads modules that are modified after this module is loaded.
(define reload-modified-modules
  (let ([init (sys-time)]
        [mod-times (make-hash-table 'eq?)])
    (lambda rl
      ;; get default rules from module-reload-rules, and convert to
      ;; regexps up front
      (define rules
        (map (^x (cons (module-glob-pattern->regexp (symbol->string (car x)))
                       (cdr x)))
             (if (pair? rl) (car rl) (module-reload-rules))))
      ;; search for the module name in ls
      (define (find-rule name ls)
        (cond [(null? ls) '()]
              [(rxmatch (caar ls) name) (cdar ls)]
              [else (find-rule name (cdr ls))]))
      ;; check each loaded module to see if it has changed
      (let1 now (sys-time)
        (for-each
         (^[mod]
           (and-let* ([name (module-name mod)]
                      [last-load (hash-table-get mod-times name init)]
                      [p1 (string-append (module-name->path name) ".scm")]
                      [path (find-in-path p1 *load-path*)]
                      [last-mod (slot-ref (sys-stat path) 'mtime)]
                      [rule (find-rule (symbol->string name) rules)])
             (when (> last-mod last-load)
               (when (reload-verbose)
                 (format #t "reloading: ~S\n" name))
               (hash-table-put! mod-times name now)
               (reload name rule))))
         (all-modules))))))


