;;;
;;; dbm - abstract base class for dbm interface
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
;;;  $Id: dbm.scm,v 1.3 2001-10-24 10:31:38 shirok Exp $
;;;

(define-module dbm
  (export <dbm>
          dbm-open    dbm-close     dbm-closed?    dbm-get
          dbm-put!    dbm-delete!   dbm-exists?
          dbm-for-each  dbm-map     dbm-error))
(select-module dbm)

(define-class <dbm> ()
  ((path       :init-keyword :path)
   (rw-mode    :init-keyword :rw-mode    :initform :write)
   (file-mode  :init-keyword :file-mode  :initform #o664)
   (key-serializer :init-keyword :key-serializer :initform #f)
   (value-serializer :init-keyword :value-serializer :initform #f)
   ))

(define-method dbm-open ((class <class>) . initargs)
  (dbm-open (apply make class initargs)))



;;
;; Method prototypes.  Actual method should be defined in subclasses.
;;

(define-method dbm-put! ((dbm <dbm>) key value)
  (when (dbm-closed? dbm) (errorf "dbm-put!: dbm already closed: ~s" dbm)))

(define-method dbm-get ((dbm <dbm>) key . args)
  (when (dbm-closed? dbm) (errorf "dbm-get: dbm already closed: ~s" dbm)))

(define-method dbm-exists? ((dbm <dbm>) key)
  (when (dbm-closed? dbm) (errorf "dbm-exists?: dbm already closed: ~s" dbm)))

(define-method dbm-delete! ((dbm <dbm>) key)
  (when (dbm-closed? dbm) (errorf "dbm-delete!: dbm already closed: ~s" dbm)))

(define-method dbm-for-each ((dbm <dbm>) proc)
  (when (dbm-closed? dbm) (errorf "dbm-for-each: dbm already closed: ~s" dbm))
  (unless (procedure? proc) (errorf "dbm-for-each: bad procedure: ~s" proc)))

(define-method dbm-map ((dbm <dbm>) proc)
  (when (dbm-closed? dbm) (errorf "dbm-map: dbm already closed: ~s" dbm))
  (unless (procedure? proc) (errorf "dbm-map: bad procedure: ~s" proc)))

(define-method dbm-close ((dbm <dbm>)) #f)

(define-method dbm-closed? ((dbm <dbm>)) #f)


(provide "dbm")
