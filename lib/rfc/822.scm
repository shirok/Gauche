;;;
;;; 822.scm - parsing RFC2822 style message
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
;;;  $Id: 822.scm,v 1.1 2001-06-02 06:36:22 shirok Exp $
;;;

;; Parser and constructor of the message defined in
;; RFC2822 Internet Message Format <ftp://ftp.isi.edu/in-notes/rfc2822.txt>

(define-module rfc.822
  (use srfi-13)
  (use gauche.regexp)
  (export rfc822-header->list)
  )

(select-module rfc.822)

(define (rfc822-header->list iport . args)
  (let-optional* args ((strict? #f))
    (let loop ((r '())
               (line (read-line iport)))
      (receive (head body next)
          (read-single-field iport line strict?)
        (if next
            (loop (cons (list head body) r) next)
            (reverse (cons (list head body) r))))
      )))

;; Internal routine.  read a single header field.
;;  iport - input port
;;  pline  - prefetched line.  #f if none.
;; Returns three values: field-name, field-body, and the prefetched line
;; (to process folded field, the routne need to prefetch one line ahead).
;; If it sees the end of the header, returns (values name body #f)
(define (read-single-field input line strict?)
  (rxmatch-case line
    (test eof-object? (values #t #t #f))
    (#/^([\x21-\x39\x3b-\x7e]+):\s*(.*)$/ (#f name body)
     (let ((name (string-downcase name)))
       (let loop ((nline (read-line input))
                  (bodies (list body)))
         (cond ((eof-object? nline)
                ;; maybe premature end of the message
                (if strict?
                    (error "premature end of message header")
                    (values name (string-concatenate-reverse bodies) #f)))
               ((string-null? nline)     ;; end of the header
                (values name (string-concatenate-reverse bodies) #f))
               ((char-set-contains? #[ \t] (string-ref nline 0))
                (loop (read-line input) (cons nline bodies)))
               (else
                (values name (string-concatenate-reverse bodies) nline)))
         )
       ))
    (else
     (if strict?
         (error "bad header line: ~s" line)
         (read-single-field input (read-line input) #f)))))


(provide "rfc/822")
