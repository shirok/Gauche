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
;;;  $Id: dbm.scm,v 1.1 2001-10-20 21:18:36 shirok Exp $
;;;

(define-module dbm
  (export <dbm>
          dbm-open
          dbm-close
          dbm-closed?
          dbm-get
          dbm-put!
          dbm-delete!
          dbm-exists?
          dbm-for-each
          dbm-map
          dbm-error))
(select-module dbm)

(define-class <DBM> ()
  ((path       :init-keyword :path)
   (rw-mode    :init-keyword :rw-mode    :initform :write)
   (file-mode  :init-keyword :file-mode  :initform #o664)
   (serializer :init-keyword :serializer :initform #f)
   ;; internal slots
   ->string
   string->
   ))

(define-method initialize ((self <dbm>) initargs)
  (next-method)
  (let ((ser (slot-ref self 'serializer)))
    (slot-set! self '->string
               (cond ((eq? ser #f) (lambda (o) o))
                     ((eq? ser #t)
                      (lambda (o)
                        (with-output-to-string (lambda () (write* o)))))
                     (else
                      (lambda (o)
                        (write-to-string-with-serializer ser o :preserve-equality #t)))))
    (slot-set! self 'string->
               (cond ((eq? ser #f) (lambda (o) o))
                     ((eq? ser #t)
                      (lambda (o)
                        (with-input-from-string o read)))
                     (else
                      (lambda (o)
                        (read-from-string-with-serializer ser o :preserve-equality #t)))))
    ))

;;
;; Method prototypes.  Actual method should be defined in subclasses.
;;

(define-method DBM-PUT! ((dbm <dbm>) key value)
  (when (dbm-closed? dbm) (errorf "dbm-put!: dbm already closed: ~s" dbm)))

(define-method DBM-GET ((dbm <dbm>) key . args)
  (when (dbm-closed? dbm) (errorf "dbm-get: dbm already closed: ~s" dbm)))

(define-method DBM-EXISTS? ((dbm <dbm>) key)
  (when (dbm-closed? dbm) (errorf "dbm-exists?: dbm already closed: ~s" dbm)))

(define-method DBM-DELETE! ((dbm <dbm>) key)
  (when (dbm-closed? dbm) (errorf "dbm-delete!: dbm already closed: ~s" dbm)))

(define-method DBM-FOR-EACH ((dbm <dbm>) proc)
  (when (dbm-closed? dbm) (errorf "dbm-for-each: dbm already closed: ~s" dbm))
  (unless (procedure? proc) (errorf "dbm-for-each: bad procedure: ~s" proc)))

(define-method DBM-MAP ((dbm <dbm>) proc)
  (when (dbm-closed? dbm) (errorf "dbm-map: dbm already closed: ~s" dbm))
  (unless (procedure? proc) (errorf "dbm-map: bad procedure: ~s" proc)))

(define-method DBM-CLOSE ((dbm <dbm>)) #f)

(define-method DBM-CLOSED? ((dbm <dbm>)) #f)


(provide "dbm")
