;;;
;;; interactive/info.scm - online helper
;;;
;;;  Copyright(C) 2002 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: info.scm,v 1.3 2002-07-11 20:07:41 shirok Exp $
;;;

(define-module gauche.interactive.info
  (use srfi-1)
  (use srfi-13)
  (use text.info)
  (use file.util)
  (use gauche.process)
  (export info)
  )
(select-module gauche.interactive.info)

(define *info-file* "gauche-refe.info")
(define *info* #f)
(define *info-index* (make-hash-table 'string=?))

(define *pager* (or (sys-getenv "PAGER")
                    (find-file-in-paths "less")
                    (find-file-in-paths "more")))
(define viewer
  (if (or (equal? (sys-getenv "TERM") "emacs")
          (not (sys-isatty (current-output-port)))
          (not *pager*))
      display
      (lambda (s)
        (let1 mask (sys-sigset-add! (make <sys-sigset>) SIGPIPE)
          (dynamic-wind
           (lambda () (sys-sigmask SIG_BLOCK mask))
           (lambda ()
             (let1 p (run-process *pager* :input :pipe)
               (display s (process-input p))
               (close-output-port (process-input p))
               (process-wait p)))
           (lambda () (sys-sigmask SIG_UNBLOCK mask)))))
      ))

(define (get-info-paths)
  (let* ((syspath (cond ((sys-getenv "INFOPATH") => (cut string-split <> #\:))
                        (else '())))
         (instpath 
          (let1 pathcomps (string-split (gauche-library-directory) #\/)
            (if (> (length pathcomps) 3)
                (list (apply build-path
                             (append (drop-right pathcomps 3) '("info"))))
                '())))
         (in-place (list "../doc")))
    (append syspath instpath in-place)))

(define (find-info-file)
  (let ((paths (get-info-paths)))
    (or (find-file-in-paths *info-file*
                            :paths paths
                            :pred (lambda (p)
                                    (or (file-is-readable? p)
                                        (file-is-readable? #`",|p|.gz"))))
        (errorf "couldn't find info file ~s in paths: ~s" *info-file* paths))
    ))

(define (info fn)
  (unless *info*
    (set! *info* (open-info-file (find-info-file)))
    (for-each (lambda (p)
                (hash-table-put! *info-index* (car p) (cdr p)))
              (info-parse-menu (info-get-node *info*
                                              "Function and Syntax Index"))))
  (let1 nodename (hash-table-get *info-index* (x->string fn) #f)
    (unless nodename (errorf "no info document for ~a" fn))
    (viewer (ref (info-get-node *info* nodename) 'content)))
  (values))

(provide "gauche/interactive/info")

