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
;;;  $Id: gdbm.scm,v 1.2 2001-10-20 11:23:00 shirok Exp $
;;;

(define-module dbm.gdbm
  (use dbm)
  (export <gdbm>
          ;; low-level functions
          gdbm-open          gdbm-close          gdbm-closed?
          gdbm-store         gdbm-fetch          gdbm-delete
          gdbm-firstkey      gdbm-nextkey        gdbm-reorganize
          gdbm-sync          gdbm-exists         gdbm-strerror
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
  ((gdbm-file :accessor gdbm-file-of)
   (sync      :init-keyword :sync   :initform #f)
   (nolock    :init-keyword :nolock :initform #f)
   (bsize     :init-keyword :bsize  :initform 0)
   ))

(define-method initialize ((self <gdbm>) initargs)
  (next-method)
  (let* ((path   (slot-ref self 'path))
         (rwmode (slot-ref self 'rw-mode))
         (sync   (slot-ref self 'sync))
         (nolock (slot-ref self 'nolock))
         (rwopt  (case rwmode
                   (:read |GDBM_READER|)
                   (:write (+ |GDBM_WRITER|
                               (if sync |GDBM_SYNC| 0)
                               (if nolock |GDBM_NOLOCK| 0)))
                   (:create (+ |GDBM_NEWDB|
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
  (gdbm-close (gdbm-file-of self)))

(define-method dbm-closed? ((self <gdbm>))
  (gdbm-closed? (gdbm-file-of self)))

;;
;; accessors
;;

(define-method dbm-put! ((self <gdbm>) key value)
  (next-method)
  (let ((stringify (slot-ref self '->string)))
    (if (> 0 (gdbm-store (gdbm-file-of self)
                         (stringify key)
                         (stringify value)
                         |GDBM_REPLACE|))
        (errorf "dbm-put!: database ~s is read only" self))))

(define-method dbm-get ((self <gdbm>) key . args)
  (next-method)
  (let ((stringify   (slot-ref self '->string))
        (unstringify (slot-ref self 'string->)))
    (cond ((gdbm-fetch (gdbm-file-of self) (stringify key))
           => (lambda (v) (unstringify v)))
          ((pair? args) (car args))     ;fall-back value
          (else  (errorf "gdbm: no data for key ~s in database ~s"
                         key (gdbm-file-of self))))))

(define-method dbm-exists? ((self <gdbm>) key)
  (next-method)
  (gdbm-exists (gdbm-file-of self) ((slot-ref self '->string) key)))

(define-method dbm-delete! ((self <gdbm>) key)
  (next-method)
  (if (> 0 (gdbm-delete (gdbm-file-of self) ((slot-ref self '->string) key)))
      (errorf "dbm-delete!: deleteting key ~s from ~s failed" key self)))

;;
;; Iterations
;;

(define-method dbm-for-each ((self <gdbm>) proc)
  (next-method)
  (let ((gdbm        (gdbm-file-of self))
        (unstringify (slot-ref self 'string->)))
    (let loop ((key (gdbm-firstkey gdbm)))
      (when key
        (let ((val (gdbm-fetch gdbm key)))
          (proc (unstringify key) (unstringify val))
          (loop (gdbm-nextkey gdbm key)))))))

(define-method dbm-map ((self <gdbm>) proc)
  (next-method)
  (let ((gdbm        (gdbm-file-of self))
        (unstringify (slot-ref self 'string->)))
    (let loop ((key (gdbm-firstkey gdbm))
               (r   '()))
      (if key
          (let ((val (gdbm-fetch gdbm key)))
            (loop (gdbm-nextkey gdbm key)
                  (cons (proc (unstringify key) (unstringify val)) r)))
          (reverse r)))))
  
(provide "dbm/gdbm")


                       
                               

           
           