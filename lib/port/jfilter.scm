;;;
;;; port.jfilter - jfilter compatibility interface
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
;;;  $Id: jfilter.scm,v 1.3 2001-06-30 09:42:38 shirok Exp $
;;;

;; This file defines a set of character-code conversion routine with
;; the same API as Dai INUKAI's Jfilter module
;;  http://www.sci.toyama-u.ac.jp/~iwao/Scheme/Jfilter/index.html
;;
;; The intention is to ease porting the existing code using Jfilter
;; to Gauche.
;;
;; The handling of string differs between Gauche and other non-multibyte
;; Scheme implementation, and most of lower-level routines in Jfilter
;; is irrelevant.  I only implement the higher-level routines.

(define-module port.jfilter
  (use gauche.let-opt)
  (use gauche.charconv)
  (use srfi-13)
  (export cv-file
          cv-string
          judge-file))

(select-module port.jfilter)

(define (ces-name->symbol name)
  (let ((n (string-upcase (string-delete name #[-_]))))
    (cond ((equal? n "EUCJP") 'eucj)
          ((member n '("CSISO2022JP" "ISO2022JP")) 'jis)
          ((member n '("SJIS" "SHIFTJIS")) 'sjis)
          ((equal? n "UTF8") 'utf8)
          (else (error "unsupported encoding name:" name)))))

(define (ces-symbol->name sym default)
  (case sym
    ((eucj) "EUCJP")
    ((sjis) "SJIS")
    ((utf8) "UTF-8")
    ((jis)  "CSISO2022JP")
    ((() #f) default)
    (else (error "unsupported encoding symbol:" sym))))

(define (judge-file input . args)
  (let-optionals* args ((prefetch 5000))

    (define (judge-port port)
      (let ((str (read-block prefetch port)))
        (ces-name->symbol (ces-guess-from-string str "*JP"))))
    
    (cond ((string? input)
           (call-with-input-file input judge-port))
          ((input-port? input)
           (judge-port input))
          (else
           (error "input must be a file name or an input port, but got" input)))
    ))

(define (cv-string str from-code to-code)
  (ces-convert str
               (ces-symbol->name from-code "*JP")
               (ces-symbol->name to-code   "EUCJP")))

(define (cv-file input output from-code to-code . args)
  (let-optionals* args ((remove-cr #f)
                        (add-cr #f)
                        (check-length 5000))
    (let ((from (ces-symbol->name from-code "*JP"))
          (to   (ces-symbol->name from-code "EUCJP")))

      (define (cv-block iport oport)
        (copy-port (open-input-conversion-port iport from
                                               :to-code to
                                               :buffer-size check-length)
                   oport))

      (define (cv-line iport oport)
        (let loop ((line (read-line iport)))
          (if (eof-object? line)
              (flush oport)
              (begin
                (display (ces-convert line from to) oport)
                (when add-cr (display #\return oport))
                (newline oport)
                (loop (read-line))))))

      (define (cv-out iport)
        (cond ((string? output)
               (call-with-output-file output
                 (lambda (out)
                   ((if add-cr cv-line cv-block) iport out))))
              ((output-port? output)
               ((if add-cr cv-line cv-block) iport output))
              (else "output must be a file name or an output port: ~s"
                    output)))

      (cond ((string? input)
             (call-with-input-file input cv-out))
            ((input-port? input)
             (cv-out input))
            (else "input must be a file name or an input port: ~s"
                  input))
      )))

(provide "port/jfilter")
