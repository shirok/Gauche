;;;
;;; port related utility functions.  to be autoloaded.
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
;;;  $Id: port.scm,v 1.1 2001-10-04 10:45:40 shirok Exp $
;;;

(select-module gauche)

;; TODO: allow caller to specify reading units
(define (port->string port)
  (with-output-to-string
    (lambda ()
      (let loop ((ch (read-char port)))
        (unless (eof-object? ch) (write-char ch) (loop (read-char port)))))))

;; TODO: optimize
(define (port->list reader port)
  (let loop ((obj (reader port))
             (result '()))
    (if (eof-object? obj)
        (reverse! result)
        (loop (reader port) (cons obj result)))))

(define (port->string-list port)
  (port->list read-line port))

(define (port->sexp-list port)
  (port->list read port))

;; Iterators on the input stream
;;   These constructs take reader function that expects to return
;;   new data for each invocation, and EOF whe exhausted.  It can
;;   be any function; need not be bound to a port.

(define (port-fold fn knil reader)
  (let loop ((item (reader))
             (r    knil))
    (if (eof-object? item)
        r
        (begin (loop (reader) (fn item r))))))

(define (port-fold-right fn knil reader)
  (let loop ((item (reader))
             (r    knil))
    (if (eof-object? item)
        r
        (begin (loop (reader) (fn r item))))))

(define (port-for-each fn reader)
  (let loop ((item (reader)))
    (unless (eof-object? item)
      (fn item)
      (loop (reader)))))

(define (port-map fn reader)
  (let loop ((item (reader))
             (r    '()))
    (if (eof-object? item)
        (reverse! r)
        (loop (reader) (cons (fn item) r)))))

