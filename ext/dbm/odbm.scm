;;;
;;; odbm - original dbm interface
;;;
;;;  Copyright(C) 2001 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: odbm.scm,v 1.3 2001-10-29 00:50:20 shirok Exp $
;;;

(define-module dbm.odbm
  (use dbm)
  (export <odbm>
          ;; low level funcions
          odbm-init    odbm-close    odbm-closed?
          odbm-store   odbm-fetch   odbm-delete
          obdm-firstkey  odbm-nextkey)
  )
(select-module dbm.odbm)
(dynamic-load "odbm")

;; Legacy DBM has some limitations.
;;  * only one dbm file can be opened at a time.
;;     (this is checked in C code)
;;  * no flags (read-only, read-write or create)
;;  * dbm changes file mode to #o000 when opened, to prevent
;;    other processes to access to the same file (I guess).
;;    this won't work well if the process dies before changing
;;    back the permission.  We reset the permission after
;;    opening the file.

;;
;; Initialize
;;

(define-class <odbm> (<dbm>)
  ())

(define-method dbm-open ((self <odbm>))
  (next-method)
  (unless (slot-bound? self 'path)
    (errorf "path must be set to open dbm database"))
  (let* ((path   (slot-ref self 'path))
         (rwmode (slot-ref self 'rw-mode))
         (fmode  (slot-ref self 'file-mode))
         (dirfile (string-append path ".dir"))
         (pagfile (string-append path ".pag"))
         (exists? (and (file-exists? dirfile) (file-exists? pagfile)))
         (create  (lambda ()
                    (with-output-to-file dirfile (lambda () #f))
                    (with-output-to-file pagfile (lambda () #f)))))
    (case rwmode
      ((:read)
       (unless exists?
         (errorf "dbm-open: no database file ~s.dir or ~s.pag" path path)))
      ((:write)
       ;; dir and pag files must exist
       (unless exists? (create)))
      ((:create)
       ;; a trick: remove dir and pag file first if :create
       (when exists?
         (posix-unlink dirfile) (posix-unlink pagfile))
       (create)))
    (odbm-init path)
    ;; adjust mode properly
    (sys-chmod dirfile fmode)
    (sys-chmod pagfile fmode)
    self))

;;
;; close operation
;;

(define-method dbm-close ((self <odbm>))
  (odbm-close))

(define-method dbm-closed? ((self <odbm>))
  (odbm-closed?))

;;
;; common operations
;;

(define-method dbm-put! ((self <odbm>) key value)
  (next-method)
  (when (positive? (odbm-store (%dbm-k2s self key) (%dbm-v2s self value)))
    (errorf "dbm-put!: put failed")))

(define-method dbm-get ((self <odbm>) key . args)
  (next-method)
  (cond ((odbm-fetch (%dbm-k2s self key))
         => (lambda (v) (%dbm-s2v self v)))
        ((pair? args) (car args))     ;fall-back value
        (else  (errorf "odbm: no data for key ~s in database ~s"
                       key self))))

(define-method dbm-exists? ((self <odbm>) key)
  (next-method)
  (if (odbm-fetch (%dbm-k2s self key)) #t #f))

(define-method dbm-delete! ((self <odbm>) key)
  (next-method)
  (when (positive? (odbm-delete (%dbm-k2s self key)))
    (errorf "dbm-delete!: deleteting key ~s from ~s failed" key self)))

(define-method dbm-fold ((self <odbm>) proc knil)
  (next-method)
  (let loop ((key (odbm-firstkey))
             (r   '()))
    (if key
        (let ((val (odbm-fetch key)))
          (loop (odbm-nextkey key)
                (proc (%dbm-s2k self key) (%dbm-s2v self val) r)))
        r)))
  
(provide "dbm/odbm")
