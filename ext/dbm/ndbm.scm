;;;
;;; ndbm - ndbm interface
;;;
;;;  Copyright(C) 2001-2003 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: ndbm.scm,v 1.3 2003-01-09 11:41:14 shirok Exp $
;;;

(define-module dbm.ndbm
  (use dbm)
  (export <ndbm>
          ;; low level funcions
          ndbm-open           ndbm-close            ndbm-closed?
          ndbm-store          ndbm-fetch            ndbm-exists?
          ndbm-delete
          ndbm-firstkey       ndbm-nextkey          ndbm-error
          ndbm-clearerror
          |DBM_INSERT|        |DBM_REPLACE|)
  )
(select-module dbm.ndbm)
(dynamic-load "ndbm")

;;
;; Initialize
;;

(define-class <ndbm> (<dbm>)
  ((ndbm-file :accessor ndbm-file-of :initform #f)
   ))

(define-method dbm-open ((self <ndbm>))
  (next-method)
  (unless (slot-bound? self 'path)
    (errorf "path must be set to open ndbm database"))
  (when (ndbm-file-of self)
    (errorf "ndbm ~S already opened" gdbm))
  (let* ((path   (slot-ref self 'path))
         (rwmode (slot-ref self 'rw-mode))
         (rwopt  (case rwmode
                   ((:read)   |O_RDONLY|)
                   ((:write)  (+ |O_RDWR| |O_CREAT|))
                   ((:create) (+ |O_RDWR| |O_CREAT| |O_TRUNC|))))
         (fp     (ndbm-open path
                            rwopt
                            (slot-ref self 'file-mode))))
    (slot-set! self 'ndbm-file fp)
    self))

;;
;; close operation
;;

(define-method dbm-close ((self <ndbm>))
  (let ((ndbm (ndbm-file-of self)))
    (and ndbm (ndbm-close ndbm))))

(define-method dbm-closed? ((self <ndbm>))
  (let ((ndbm (ndbm-file-of self)))
    (or (not ndbm) (ndbm-closed? ndbm))))

;;
;; common operations
;;

(define-method dbm-put! ((self <ndbm>) key value)
  (next-method)
  (when (positive? (ndbm-store (ndbm-file-of self)
                               (%dbm-k2s self key)
                               (%dbm-v2s self value)
                               |DBM_REPLACE|))
    (error "dbm-put! failed" self)))

(define-method dbm-get ((self <ndbm>) key . args)
  (next-method)
  (cond ((ndbm-fetch (ndbm-file-of self) (%dbm-k2s self key))
         => (lambda (v) (%dbm-s2v self v)))
        ((pair? args) (car args))     ;fall-back value
        (else  (errorf "ndbm: no data for key ~s in database ~s"
                       key (ndbm-file-of self)))))

(define-method dbm-exists? ((self <ndbm>) key)
  (next-method)
  (ndbm-exists? (ndbm-file-of self) (%dbm-k2s self key)))

(define-method dbm-delete! ((self <ndbm>) key)
  (next-method)
  (when (positive? (ndbm-delete (ndbm-file-of self) (%dbm-k2s self key)))
    (errorf "dbm-delete!: deleteting key ~s from ~s failed" key self)))

(define-method dbm-fold ((self <ndbm>) proc knil)
  (let ((ndbm (ndbm-file-of self)))
    (let loop ((key (ndbm-firstkey ndbm))
               (r   knil))
      (if key
          (let ((val (ndbm-fetch ndbm key)))
            (loop (ndbm-nextkey ndbm)
                  (proc (%dbm-s2k self key) (%dbm-s2v self val) r)))
          r))
    ))

(provide "dbm/ndbm")
