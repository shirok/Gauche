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
;;;  $Id: reload.scm,v 1.1 2002-11-09 23:42:33 shirok Exp $
;;;

;;; Created:    <2002-11-06 16:02:55 foof>
;;; Time-stamp: <2002-11-07 13:51:04 foof>
;;; Author:     Alex Shinn <foof@synthcode.com>
;;; Modfied by Shiro Kawai

;; Development-only utility to reload modules while retaining data.

(define-module gauche.reload
  (use srfi-1)
  (use srfi-2)
  (use file.util)
  (export reload reload-modified-modules)
  )
(select-module gauche.reload)

(define (reload module-name . predicates)
  (let ((keep? (apply any-pred predicates))
        (mod (find-module module-name)))
    (if (not mod)
        (warn "no such module: ~S" module-name)
        (let ((table (module-table mod))
              (saves (make-hash-table 'eq?)))
          ;; remember data
          (hash-table-for-each
           table
           (lambda (sym gloc)
             (let1 value (eval sym mod)
               (when (keep? sym value)
                 ;;(format #t "keeping ~S (~S)\n" sym (class-of value))
                 (hash-table-put! saves sym value)))))
          ;; reload
          (load (%module-name->path module-name))
          ;; restore data
          (hash-table-for-each
           saves
           (lambda (sym value)
             (eval `(set! ,sym ',value) mod)))))))

(define (find-in-path file path)
  (find file-is-regular?
        (map (lambda (d)
               (sys-normalize-pathname (string-append d "/" file)))
             path)))

(define reload-modified-modules
  (let ((init (sys-time))
        (mod-times (make-hash-table 'eq?)))
    (lambda args
      (let1 now (sys-time)
        (for-each
         (lambda (mod)
           (and-let* ((name (module-name mod))
                      (last-load (hash-table-get mod-times name init))
                      (p1 (string-append (%module-name->path name) ".scm"))
                      (path (find-in-path p1 *load-path*))
                      (last-mod (slot-ref (sys-stat path) 'mtime)))
             (when (> last-mod last-load)
               (hash-table-put! mod-times name now)
               ;;(format #t "reloading: ~S\n" name)
               (apply reload (cons name args)))))
         (all-modules))))))

(provide "gauche/reload")

