;;;
;;; ndbm - ndbm interface
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
;;;  $Id: ndbm.scm,v 1.1 2001-10-20 11:29:00 shirok Exp $
;;;

(define-module dbm.ndbm
  (use dbm)
  (export <ndbm>
          ;; low level funcions
          ndbm-open           ndbm-close            ndbm-closed?
          ndbm-store          ndbm-fetch            ndbm-delete
          ndbm-firstkey       ndbm-nextkey          ndbm-error
          ndbm-clear-error
          |DBM_INSERT|        |DBM_REPLACE|)
  )
(select-module dbm.ndbm)
(dynamic-load "ndbm")

;;
;; Initialize
;;

(define-class <ndbm> (<dbm>)
  ((ndbm-file :accessor ndbm-file-of)
   ))

(define-method initialize ((self <ndbm>) initargs)
  (next-method)
  (let* ((path   (slot-ref self 'path))
         (rwmode (slot-ref self 'rw-mode))
         (rwopt  (case rwmode
                   (:read   |O_RDONLY|)
                   (:write  (+ |O_RDWR| |O_CREAT|))
                   (:create (+ |O_RDWR| |O_CREAT| |O_TRUNC|))))
         (fp     (ndbm-open path
                            rwopt
                            (slot-ref self 'file-mode))))
    (slot-set! self 'ndbm-file fp)
    self))

;;
;; close operation
;;

(define-method dbm-close ((self <ndbm>))
  (ndbm-close (ndbm-file-of self)))

(define-method dbm-closed? ((self <ndbm>))
  (ndbm-closed? (ndbm-file-of self)))

;;
;; common operations
;;

(define-method dbm-put! ((self <ndbm>) key value)
  (next-method)
  (let ((stringify (slot-ref self '->string)))
    (if (> 0 (ndbm-store (ndbm-file-of self)
                         (stringify key)
                         (stringify value)
                         |DBM_REPLACE|))
        (errorf "dbm-put!: database ~s is read only" self))))

(define-method dbm-get ((self <ndbm>) key . args)
  (next-method)
  (let ((stringify   (slot-ref self '->string))
        (unstringify (slot-ref self 'string->)))
    (cond ((ndbm-fetch (ndbm-file-of self) (stringify key))
           => (lambda (v) (unstringify v)))
          ((pair? args) (car args))     ;fall-back value
          (else  (errorf "ndbm: no data for key ~s in database ~s"
                         key (ndbm-file-of self))))))

(define-method dbm-exists? ((self <ndbm>) key)
  (next-method)
  (let ((stringify   (slot-ref self '->string)))
    (if (ndbm-fetch (ndbm-file-of self) (stringify key))
        #t
        #f)))

(define-method dbm-delete! ((self <ndbm>) key)
  (next-method)
  (if (> 0 (ndbm-delete (ndbm-file-of self) ((slot-ref self '->string) key)))
      (errorf "dbm-delete!: deleteting key ~s from ~s failed" key self)))

(define-method dbm-for-each ((self <ndbm>) proc)
  (next-method)
  (let ((ndbm        (ndbm-file-of self))
        (unstringify (slot-ref self 'string->)))
    (let loop ((key (ndbm-firstkey ndbm)))
      (when key
        (let ((val (ndbm-fetch ndbm key)))
          (proc (unstringify key) (unstringify val))
          (loop (ndbm-nextkey ndbm)))))
    ))

(define-method dbm-map ((self <ndbm>) proc)
  (next-method)
  (let ((ndbm        (ndbm-file-of self))
        (unstringify (slot-ref self 'string->)))
    (let loop ((key (ndbm-firstkey ndbm))
               (r   '()))
      (if key
          (let ((val (ndbm-fetch ndbm key)))
            (loop (ndbm-nextkey ndbm)
                  (cons (proc (unstringify key) (unstringify val)) r)))
          (reverse r)))))
  
(provide "dbm/ndbm")
