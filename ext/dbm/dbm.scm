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
;;;  $Id: dbm.scm,v 1.5 2001-10-30 20:03:07 shirok Exp $
;;;

(define-module dbm
  (export <dbm>
          dbm-open    dbm-close   dbm-closed? dbm-get
          dbm-put!    dbm-delete! dbm-exists?
          dbm-fold    dbm-for-each  dbm-map
          %dbm-k2s    %dbm-s2k    %dbm-v2s    %dbm-s2v)
  )
(select-module dbm)

(define-class <dbm> ()
  ((path       :init-keyword :path)
   (rw-mode    :init-keyword :rw-mode    :initform :write)
   (file-mode  :init-keyword :file-mode  :initform #o664)
   (key-convert   :init-keyword :key-convert :initform #f)
   (value-convert :init-keyword :value-convert :initform #f)
   ;; internal.  set up by dbm-open
   k2s s2k v2s s2v
   ))

;; Macros that can be used by implementation modules
(define-syntax %dbm-k2s
  (syntax-rules ()
    ((_ self key) ((slot-ref self 'k2s) key))))

(define-syntax %dbm-s2k
  (syntax-rules ()
    ((_ self key) ((slot-ref self 's2k) key))))

(define-syntax %dbm-v2s
  (syntax-rules ()
    ((_ self key) ((slot-ref self 'v2s) key))))

(define-syntax %dbm-s2v
  (syntax-rules ()
    ((_ self key) ((slot-ref self 's2v) key))))

;;
;; DBM-OPEN
;;

(define-method dbm-open ((class <class>) . initargs)
  (dbm-open (apply make class initargs)))

(define-method dbm-open ((self <dbm>))
  (define (pick-proc slot default custom)
    (let ((spec (slot-ref self slot)))
      (cond ((eq? spec #f) identity)
            ((eq? spec #t) default)
            ((and (pair? spec)
                  (null? (cddr spec))
                  (procedure? (car spec))
                  (procedure? (cadr spec)))
             spec)
            (else (errorf "bad value for ~s: has to be boolean or a list of two procedures, but got ~s" slot spec)))))

  (slot-set! self 'k2s (pick-proc 'key-convert write-to-string car))
  (slot-set! self 's2k (pick-proc 'key-convert read-from-string cadr))
  (slot-set! self 'v2s (pick-proc 'value-convert write-to-string car))
  (slot-set! self 's2v (pick-proc 'value-convert read-from-string cadr))
  self)

;;
;; Method prototypes.  Actual method should be defined in subclasses.
;;

(define-method dbm-put! ((dbm <dbm>) key value)
  (when (dbm-closed? dbm) (errorf "dbm-put!: dbm already closed: ~s" dbm))
  (when (eqv? (slot-ref dbm 'rw-mode) :read)
    (errorf "dbm-put!: dbm is read only: ~s" dbm)))

(define-method dbm-get ((dbm <dbm>) key . args)
  (when (dbm-closed? dbm) (errorf "dbm-get: dbm already closed: ~s" dbm)))

(define-method dbm-exists? ((dbm <dbm>) key)
  (when (dbm-closed? dbm) (errorf "dbm-exists?: dbm already closed: ~s" dbm)))

(define-method dbm-delete! ((dbm <dbm>) key)
  (when (dbm-closed? dbm) (errorf "dbm-delete!: dbm already closed: ~s" dbm))
  (when (eqv? (slot-ref dbm 'rw-mode) :read)
    (errorf "dbm-put!: dbm is read only: ~s" dbm)))

(define-method dbm-fold ((dbm <dbm>) proc knil) #f)

(define-method dbm-close ((dbm <dbm>)) #f)

(define-method dbm-closed? ((dbm <dbm>)) #f)

;;
;; These work if dbm-fold is defined.
;;

(define-method dbm-for-each ((dbm <dbm>) proc)
  (when (dbm-closed? dbm) (errorf "dbm-for-each: dbm already closed: ~s" dbm))
  (unless (procedure? proc) (errorf "dbm-for-each: bad procedure: ~s" proc))
  (dbm-fold dbm (lambda (key value r) (proc key value)) #f))

(define-method dbm-map ((dbm <dbm>) proc)
  (when (dbm-closed? dbm) (errorf "dbm-map: dbm already closed: ~s" dbm))
  (unless (procedure? proc) (errorf "dbm-map: bad procedure: ~s" proc))
  (reverse (dbm-fold dbm (lambda (key value r) (cons (proc key value) r)) '())))

(provide "dbm")
