;;;
;;; gdbm - gdbm interface
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
;;;  $Id: gdbm.scm,v 1.4 2001-10-28 11:38:14 shirok Exp $
;;;

(define-module dbm.gdbm
  (use dbm)
  (export <gdbm>
          ;; low-level functions
          gdbm-open          gdbm-close          gdbm-closed?
          gdbm-store         gdbm-fetch          gdbm-delete
          gdbm-firstkey      gdbm-nextkey        gdbm-reorganize
          gdbm-sync          gdbm-exists?        gdbm-strerror
          gdbm-setopt        gdbm-version        gdbm-file-of
          |GDBM_READER|      |GDBM_WRITER|       |GDBM_WRCREAT|
          |GDBM_NEWDB|       |GDBM_FAST|         |GDBM_SYNC|
          |GDBM_NOLOCK|      |GDBM_INSERT|       |GDBM_REPLACE|
          |GDBM_CACHESIZE|   |GDBM_FASTMODE|     |GDBM_CENTFREE|
          |GDBM_COALESCEBLKS|)
  )
(select-module dbm.gdbm)
(dynamic-load "gdbm")

;;
;; Initialize
;;

(define-class <gdbm> (<dbm>)
  ((gdbm-file :accessor gdbm-file-of :initform #f)
   (sync      :init-keyword :sync   :initform #f)
   (nolock    :init-keyword :nolock :initform #f)
   (bsize     :init-keyword :bsize  :initform 0)
   ))

(define-method dbm-open ((self <gdbm>))
  (next-method)
  (unless (slot-bound? self 'path)
    (errorf "path must be set to open gdbm database"))
  (when (gdbm-file-of self)
    (errorf "gdbm ~S already opened" gdbm))
  (let* ((path   (slot-ref self 'path))
         (rwmode (slot-ref self 'rw-mode))
         (sync   (slot-ref self 'sync))
         (nolock (slot-ref self 'nolock))
         (rwopt  (case rwmode
                   ((:read) |GDBM_READER|)
                   ((:write) (+ |GDBM_WRCREAT|
                                 (if sync |GDBM_SYNC| 0)
                                 (if nolock |GDBM_NOLOCK| 0)))
                   ((:create) (+ |GDBM_NEWDB|
                                 (if sync |GDBM_SYNC| 0)
                                 (if nolock |GDBM_NOLOCK| 0)))))
         (fp     (gdbm-open path
                            (slot-ref self 'bsize)
                            rwopt
                            (slot-ref self 'file-mode))))
    (slot-set! self 'gdbm-file fp)
    self))

;;
;; close operation
;;

(define-method dbm-close ((self <gdbm>))
  (let ((gdbm (gdbm-file-of self)))
    (and gdbm (gdbm-close gdbm))))

(define-method dbm-closed? ((self <gdbm>))
  (let ((gdbm (gdbm-file-of self)))
    (or (not gdbm) (gdbm-closed? gdbm))))

;;
;; accessors
;;

(define-method dbm-put! ((self <gdbm>) key value)
  (next-method)
  (when (positive? (gdbm-store (gdbm-file-of self)
                               (%dbm-k2s self key)
                               (%dbm-v2s self value)
                               |GDBM_REPLACE|))
    (error "dbm-put! failed" self)))

(define-method dbm-get ((self <gdbm>) key . args)
  (next-method)
  (cond ((gdbm-fetch (gdbm-file-of self) (%dbm-k2s self key))
         => (lambda (v) (%dbm-s2v self v)))
        ((pair? args) (car args))     ;fall-back value
        (else  (errorf "gdbm: no data for key ~s in database ~s"
                       key (gdbm-file-of self)))))

(define-method dbm-exists? ((self <gdbm>) key)
  (next-method)
  (gdbm-exists? (gdbm-file-of self) (%dbm-k2s self key)))

(define-method dbm-delete! ((self <gdbm>) key)
  (next-method)
  (when (positive? (gdbm-delete (gdbm-file-of self) (%dbm-k2s self key)))
    (errorf "dbm-delete!: deleteting key ~s from ~s failed" key self)))

;;
;; Iterations
;;

(define-method dbm-fold ((self <gdbm>) proc knil)
  (let ((gdbm (gdbm-file-of self)))
    (let loop ((key (gdbm-firstkey gdbm))
               (r   knil))
      (if key
          (let ((val (gdbm-fetch gdbm key)))
            (loop (gdbm-nextkey gdbm key)
                  (proc (%dbm-s2k self key) (%dbm-s2k self val) r)))
          r))))

(provide "dbm/gdbm")


                       
                               

           
           