;;;
;;; charconv - character code conversion module
;;;
;;;  Copyright(C) 2001-2003 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: charconv.scm,v 1.10 2003-01-09 11:34:21 shirok Exp $
;;;

(define-module gauche.charconv
  (export open-input-conversion-port
          open-output-conversion-port
          open-input-file open-output-file
          call-with-input-file call-with-output-file
          with-input-from-file with-output-to-file
          ces-conversion-supported?
          ces-convert
          ces-guess-from-string))

(select-module gauche.charconv)

(dynamic-load "libcharconv" :export-symbols #t)

(define (ces-convert string fromcode . args)
  (let-optionals* args ((tocode #f))
    (let ((out (open-output-string)))
      (copy-port
       (open-input-conversion-port (open-input-string string) fromcode
                                   :to-code tocode
                                   :buffer-size (string-size string))
       out :unit 'byte)
      (get-output-string out))))

;; Replace system's open-*-file to accept :encoding option
(define (open-input-file name . args)
  (cond ((get-keyword :encoding args #f)
         => (lambda (from-code)
              (open-input-conversion-port
               (with-module scheme (apply open-input-file name args))
               from-code
               :buffer-size (get-keyword :conversion-buffer-size args 0)
               :owner #t)))
        (else (with-module scheme (apply open-input-file name args)))))

(define (open-output-file name . args)
  (cond ((get-keyword :encoding args #f)
         => (lambda (to-code)
              (open-output-conversion-port
               (with-module scheme (apply open-output-file name args))
               to-code
               :buffer-size (get-keyword :conversion-buffer-size args 0)
               :owner #t)))
        (else (with-module scheme (apply open-output-file name args)))))

(define (call-with-input-file filename proc . flags)
  (let ((port (apply open-input-file filename flags)))
    (dynamic-wind
     (lambda () #f)
     (lambda () (proc port))
     (lambda () (close-input-port port)))))

(define (call-with-output-file filename proc . flags)
  (let ((port (apply open-output-file filename flags)))
    (dynamic-wind
     (lambda () #f)
     (lambda () (proc port))
     (lambda () (close-output-port port)))))

(define (with-input-from-file filename thunk . flags)
  (let ((port (apply open-input-file filename flags)))
    (dynamic-wind
     (lambda () #f)
     (lambda () ((with-module gauche with-input-from-port) port thunk))
     (lambda () (close-input-port port)))))

(define (with-output-to-file filename thunk . flags)
  (let ((port (apply open-output-file filename flags)))
    (dynamic-wind
     (lambda () #f)
     (lambda () ((with-module gauche with-output-to-port) port thunk))
     (lambda () (close-output-port port)))))

(provide "gauche/charconv")
