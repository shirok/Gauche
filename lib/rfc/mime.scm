;;;
;;; mime.scm - parsing MIME (rfc2045) message
;;;  
;;;   Copyright (c) 2000-2003 Shiro Kawai, All rights reserved.
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
;;;  $Id: mime.scm,v 1.2 2003-12-13 03:12:19 shirok Exp $
;;;

;; RFC2045 Multipurpose Internet Mail Extensions (MIME)
;;  Part One: Format of Internet Message Bodies
;; RFC2046 Multipurpose Internet Mail Extensions (MIME)
;;  Part Two: Media Types
;; RFC2047 Multipurpose Internet Mail Extensions (MIME)
;;  Part Three: Message Header Extensions for Non-ASCII Text
;; RFC2048 
;; RFC2049 

(define-module rfc.mime
  (use srfi-1)
  (use srfi-2)
  (use srfi-13)
  (use srfi-14)
  (use rfc.822)
  (use util.list)
  (export mime-parse-version mime-parse-content-type
          mime-decode-word
          )
  )
(select-module rfc.mime)

(autoload rfc.quoted-printable quoted-printable-decode-string)
(autoload rfc.base64 base64-decode-string)
(autoload gauche.charconv ces-conversion-supported? ces-convert)

;;===============================================================
;; Basic utility
;;

;; returns list of major and minor versions in integers
(define (mime-parse-version field)
  (and field
       (let1 s
           (string-concatenate (map x->string (rfc822-field->tokens field)))
         (cond ((#/^(\d+)\.(\d+)$/ s) =>
                (lambda (m) (map (lambda (d) (x->integer (m d))) '(1 2))))
               (else #f)))))

;; returns (<type> <subtype> (<attribute> . <value>) ...)
(define (mime-parse-content-type field)
  (define token-chars
    (char-set-difference #[\x21-\x7e] #[()<>@,\;:\\\"/\[\]?=]))
  (define (get-attributes input r)
    (cond ((and-let* (((eqv? #\; (rfc822-next-token input '())))
                      (attr (rfc822-next-token input `(,token-chars)))
                      ((string? attr))
                      ((eqv? #\= (rfc822-next-token input '())))
                      (val  (rfc822-next-token
                             input
                             `(,token-chars
                               (#[\"] . ,rfc822-quoted-string))))
                      ((string? val)))
             (cons attr val))
           => (lambda (p) (get-attributes input (cons p r))))
          (else (reverse! r))))

  (and field
       (call-with-input-string field
         (lambda (input)
           (let* ((type    (rfc822-next-token input `(,token-chars)))
                  (slash   (rfc822-next-token input '()))
                  (subtype (rfc822-next-token input `(,token-chars))))
             (and (string? type)
                  (eqv? #\/ slash)
                  (string? subtype)
                  (list* (string-downcase type)
                         (string-downcase subtype)
                         (get-attributes input '()))))
           ))))

;; decode rfc2047-encoded word, i.e. "=?...?="
;; if word isn't legal encoded word, it is returned as is.
(define (mime-decode-word word)
  (rxmatch-case word
    (test string-incomplete? word) ;; safety net
    (#/^=\?([-!#-'*+\w\^-~]+)\?([-!#-'*+\w\^-~]+)\?([!->@-~]+)\?=$/
     (#f charset encoding body)
     (if (ces-conversion-supported? charset #f)
       (cond ((string-ci=? encoding "q")
              (ces-convert (quoted-printable-decode-string body) charset #f))
             ((string-ci=? encoding "b")
              (ces-convert (base64-decode-string body) charset #f))
             (else word)) ;; unsupported encoding
       word))
    (else word)))

;;===============================================================
;; Basic streaming parser
;;

(define-constant *eof-object* (read-from-string ""))

;; message information packet
(define-class <mime-message> ()
  ((type     :init-keyword :type)
   (subtype  :init-keyword :subtype)
   (parameters :init-keyword :parameters)
   (transfer-encoding :init-keyword :transfer-encoding)
   (headers  :init-keyword :headers)
   (parent-headers :init-keyword :parent-headers)
   (contents :init-value #f) ;; up to the <proc>
   ))

(define (mime-port-fold port headers proc seed)
  (let* ((ctype (or (mime-parse-content-type
                     (rfc822-header-ref headers "content-type"))
                    '("text" "plain" ("charset" . "us-ascii"))))
         (enc   (rfc822-header-ref headers "content-transfer-encoding" "7bit"))
         (packet (make <mime-message>
                   :type (car ctype)
                   :subtype (cadr ctype)
                   :parameter (cddr ctype)
                   :transfer-encoding enc
                   :headers headers))
         )
    (cond
     ((equal? (car ctype) "multipart")
      (multipart-fold port packet proc seed))
     ((equal? (car ctype) "message")
      (message-fold port packet proc seed))
     (else
      ;; normal body
      (proc port packet read-line seed))
     )))

(define (multipart-fold port packet proc seed)
  (let* ((boundary (or (assoc-ref (ref message 'parameters) "boundary")
                       (error "No boundary given for multipart message")))
         (--boundary   (string-append "--" boundary))
         (--boundary-- (string-append --boundary "--"))
         (state 'prologue))
    (define (line-reader port)
      (let1 l (read-line port #t)
        (cond ((eof-object? l)
               (set! state 'eof) *eof-object*)
              ((equal? l --boundary)
               (set! state 'body) *eof-object*)
              ((equal? l --boundary--)
               (set! state 'epilogue) *eof-object*)
              (else l))))
    ;; skip prologue
    (do ()
        ((not (eq? state 'prologue)))
      (line-reader port))
    (let loop ((seed seed))
      (let* ((headers (rfc822-header->list port :reader line-reader))
             (r (mime-port-fold port headers proc seed)))
        (if (eq? state 'epilogue)
          r
          (loop r))))
    ))

;(define (message-fold port packet proc seed)
;  (

(provide "rfc/mime")
