;;;
;;; interpolate.scm - string interpolation; to be autoloaded
;;;
;;;  Copyright(C) 2002 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: interpolate.scm,v 1.2 2002-02-19 10:08:56 shirok Exp $
;;;

;;; #`"The value is ,|foo|." => (string-append "The value is " foo ".")
;;; 

(define-module gauche.interpolate
  (export string-interpolate)
  )
(select-module gauche.interpolate)

(define-macro string-interpolate (lambda (str)
  (if (string? str)
      (%string-interpolate str)
      (errorf "malformed string-interpolate: ~s"
              (cons 'string-interpolate str)))
  ))

(define (%string-interpolate str)
  (define (accum c acc)
    (cond ((eof-object? c) (list (get-output-string acc)))
          ((char=? c #\,)
           (let ((c2 (peek-char)))
             (cond ((eof-object? c2) (write-char c acc) (accum c2 acc))
                   ((char=? c2 #\,)
                    (write-char (read-char) acc) (accum (read-char) acc))
                   ((char-set-contains? #[\x00-\x20\),\;\[\\\]\{\}\7f] c2)
                    (write-char c acc) (accum (read-char) acc))
                   (else
                    (cons (get-output-string acc) (insert))))))
          (else
           (write-char c acc) (accum (read-char) acc))))
  (define (insert)
    (let* ((item
            (with-error-handler
             (lambda (e)
               (error "unmatched parenthesis in interpolating string: ~s" str))
             (lambda () (read))))
           (rest
            (accum (read-char) (open-output-string))))
      (cons `(,x->string ,item) rest)))
  (cons 'string-append
        (with-input-from-string str
          (lambda () (accum (read-char) (open-output-string)))))
  )

(provide "gauche/interpolate")
