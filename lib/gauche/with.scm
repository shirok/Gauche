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
;;;  $Id: with.scm,v 1.12 2002-02-23 07:37:45 shirok Exp $
;;;

(select-module gauche)

;; File ports

(define (call-with-input-file filename proc . flags)
  (let ((port (apply open-input-file filename flags)))
    (dynamic-wind
     (lambda () #f)
     (lambda () (proc port))
     (lambda () (if port (close-input-port port))))))

(define (call-with-output-file filename proc . flags)
  (let ((port (apply open-output-file filename flags)))
    (dynamic-wind
     (lambda () #f)
     (lambda () (proc port))
     (lambda () (if port (close-output-port port))))))

(define (with-input-from-file filename thunk . flags)
  (let ((port (apply open-input-file filename flags)))
    (and port
         (dynamic-wind
          (lambda () #f)
          (lambda () ((with-module gauche with-input-from-port) port thunk))
          (lambda () (close-input-port port))))))

(define (with-output-to-file filename thunk . flags)
  (let ((port (apply open-output-file filename flags)))
    (and port
         (dynamic-wind
          (lambda () #f)
          (lambda () ((with-module gauche with-output-to-port) port thunk))
          (lambda () (close-output-port port))))))

;; intern R5RS procedures in scheme module
(with-module scheme
  (define call-with-input-file  (with-module gauche call-with-input-file))
  (define call-with-output-file (with-module gauche call-with-output-file))
  (define with-input-from-file  (with-module gauche with-input-from-file))
  (define with-output-to-file   (with-module gauche with-output-to-file))
  )

;; String ports

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

(define (call-with-string-io str proc)
  (let ((out (open-output-string))
        (in  (open-input-string str)))
    (proc in out)
    (get-output-string out)))

(define (with-string-io str thunk)
  (with-output-to-string
    (lambda ()
      (with-input-from-string str
        thunk))))

(define (write-to-string obj . args)
  (with-output-to-string
    (lambda () ((if (pair? args) (car args) write) obj))))

(define (read-from-string string . args)
  (with-input-from-string
      (if (null? args) string (apply %maybe-substring string args))
    read))

(provide "gauche/with")
