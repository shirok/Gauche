;;;
;;; auxiliary string utilities.  to be autoloaded.
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
;;;  $Id: stringutil.scm,v 1.1 2002-12-06 12:46:55 shirok Exp $
;;;

(define-module gauche.stringutil
  (export string-split)
  )
(select-module gauche.stringutil)

;; trick to delay loading of srfi-13 until needed
(autoload srfi-13 string-tokenize)

;; Generic string-split
;;   splitter can be a character, a char-set, a string, or a regexp.
(define (string-split string splitter)
  (cond ((char? splitter) (%string-split-by-char string splitter))
        ((and (string? splitter) (= (string-length splitter) 1))
         (%string-split-by-char string (string-ref splitter 0)))
        (else (%string-split string (%string-split-scanner splitter)))
        ))

;; aux fns
(define (%string-split-scanner splitter)
  (cond ((string? splitter)
         (lambda (s)
           (receive (before after) (string-scan s splitter 'both)
             (if before (values before after) (values s #f)))))
        ((char-set? splitter)
         (%string-split-scanner-each-char
          (cut char-set-contains? splitter <>)))
        ((regexp? splitter)
         (lambda (s)
           (cond ((rxmatch splitter s)
                  => (lambda (m)
                       (let ((before (m 'before))
                             (after  (m 'after)))
                         (when (string=? s after)
                           (errorf "splitting string by regexp #/~a/ would cause infinite loop"
                                   (regexp->string splitter)))
                         (values before after))))
                 (else (values s #f)))))
        (else ;; assume splitter is a predicate
         (%string-split-scanner-each-char splitter))
        ))

(define (%string-split-scanner-each-char pred)
  (define (scan-in p)
    (let ((c (string-pointer-ref p)))
      (cond ((eof-object? c) (values (string-pointer-substring p) #f))
            ((pred c)
             (let1 before (string-pointer-substring p)
               (string-pointer-next! p)
               (scan-out p before)))
            (else (string-pointer-next! p) (scan-in p)))))
  (define (scan-out p before)
    (let ((c (string-pointer-ref p)))
      (cond ((eof-object? c) (values before ""))
            ((pred c) (string-pointer-next! p) (scan-out p before))
            (else (values before (string-pointer-substring p :after #t))))))
  (lambda (s) (scan-in (make-string-pointer s))))

(define (%string-split string scanner)
  (let loop ((s string)
             (r '()))
    (receive (before after) (scanner s)
      (if after
          (loop after (cons before r))
          (reverse! (cons before r))))))

(provide "gauche/stringutil")
