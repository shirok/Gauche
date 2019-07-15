;;;
;;; compat.jfilter - jfilter compatibility interface
;;;
;;;   Copyright (c) 2000-2019  Shiro Kawai  <shiro@acm.org>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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

(define-module compat.jfilter
  (use gauche.charconv)
  (use srfi-13)
  (export cv-file
          cv-string
          judge-file))

(select-module compat.jfilter)

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

(define (judge-file input :optional (prefetch 5000))
  (define (judge-port port)
    (let ((str (read-block prefetch port)))
      (ces-name->symbol (ces-guess-from-string str "*JP"))))

  (cond ((string? input)
         (call-with-input-file input judge-port))
        ((input-port? input)
         (judge-port input))
        (else
         (error "input must be a file name or an input port, but got" input)))
  )

(define (cv-string str from-code to-code)
  (ces-convert str
               (ces-symbol->name from-code "*JP")
               (ces-symbol->name to-code   "EUCJP")))

(define (cv-file input output from-code to-code :optional
                 (remove-cr #f) (add-cr #f) (check-length 5000))
  (let ((from (ces-symbol->name from-code "*JP"))
        (to   (ces-symbol->name to-code "EUCJP")))

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
    ))

