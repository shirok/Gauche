;;;
;;; with-* and call-with-* functions.  to be autoloaded.
;;;
;;;  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: with.scm,v 1.2 2001-06-24 23:06:10 shirok Exp $
;;;

(select-module gauche)

(define (with-output-to-string thunk)
  (let ((out (open-output-string)))
    (with-output-to-port out thunk)
    (get-output-string out)))

(define (with-input-from-string str thunk)
  (with-input-from-port (open-input-string str) thunk))

(define (call-with-output-string proc)
  (let ((out (open-output-string)))
    (proc out)
    (get-output-string out)))

(define (call-with-input-string str proc)
  (let ((in (open-input-string str)))
    (proc in)))

;; Convenient port input utilities.

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
