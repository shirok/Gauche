;;;
;;; auxiliary hashtable utilities.  to be autoloaded.
;;;
;;;  Copyright(C) 2003 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: hashutil.scm,v 1.1 2003-01-06 04:06:07 shirok Exp $
;;;

(define-module gauche.hashutil
  (export hash-table hash-table-for-each hash-table-map hash-table-fold)
  )
(select-module gauche.hashutil)

(define (hash-table type . kvs)
  (let ((h (make-hash-table type)))
    (for-each (lambda (kv) (hash-table-put! h (car kv) (cdr kv))) kvs)
    h))

(define (hash-table-map hash proc)
  (check-arg hash-table? hash)
  (let ((eof (cons #f #f))              ;marker
        (i   (%hash-table-iter hash)))
    (let loop ((r '()))
      (receive (k v) (i eof)
        (if (eq? k eof)
            r
            (loop (cons (proc k v) r)))))))

(define (hash-table-for-each hash proc)
  (check-arg hash-table? hash)
  (let ((eof (cons #f #f))              ;marker
        (i (%hash-table-iter hash)))
    (let loop ()
      (receive (k v) (i eof)
        (unless (eq? k eof)
          (proc k v) (loop))))))

(define (hash-table-fold hash kons knil)
  (check-arg hash-table? hash)
  (let ((eof (cons #f #f))              ;marker
        (i (%hash-table-iter hash)))        
    (let loop ((r knil))
      (receive (k v) (i eof)
        (if (eq? k eof)
            r
            (loop (kons k v r)))))))

(provide "gauche/hashutil")
