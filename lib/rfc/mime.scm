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
;;;  $Id: mime.scm,v 1.4 2003-12-16 05:31:12 shirok Exp $
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
          <mime-part>
          mime-parse-message mime-retrieve-body
          mime-body->string mime-body->file
          )
  )
(select-module rfc.mime)

(autoload rfc.quoted-printable quoted-printable-decode-string)
(autoload rfc.base64 base64-decode-string base64-decode)
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
(define-class <mime-part> ()
  ((type     :init-keyword :type)
   (subtype  :init-keyword :subtype)
   (parameters :init-keyword :parameters)
   (transfer-encoding :init-keyword :transfer-encoding)
   (headers  :init-keyword :headers)
   (parent   :init-keyword :parent :init-value #f)
   (index    :init-keyword :index :init-value 0)
   (content  :init-value #f)
   ))

(define (mime-parse-message port headers handler)
  (internal-parse port headers handler (cut read-line <> #t) #f 0
                  '("text" "plain" ("charset" . "us-ascii"))))

(define (internal-parse port headers handler reader parent index default-type)
  (let* ((ctype (or (mime-parse-content-type
                     (rfc822-header-ref headers "content-type"))
                    default-type))
         (enc   (rfc822-header-ref headers "content-transfer-encoding" "7bit"))
         (packet (make <mime-part>
                   :type (car ctype)
                   :subtype (cadr ctype)
                   :parameters (cddr ctype)
                   :transfer-encoding enc
                   :parent parent
                   :index index
                   :headers headers))
         )
    (cond
     ((equal? (car ctype) "multipart")
      (multipart-parse port packet handler reader))
     ((equal? (car ctype) "message")
      (message-parse port packet handler reader))
     (else
      ;; normal body
      (slot-set! packet 'content (handler packet reader))
      packet)
     )))

(define (multipart-parse port packet handler reader)
  (let* ((boundary (or (assoc-ref (ref packet 'parameters) "boundary")
                       (error "No boundary given for multipart message")))
         (--boundary   (string-append "--" boundary))
         (--boundary-- (string-append --boundary "--"))
         (state 'prologue)
         (default-type (if (equal? (ref packet 'subtype) "digest")
                         '("message" "rfc822")
                         '("text" "plain" ("charset" . "us-ascii"))))
         )
    (define (line-reader port)
      (let1 l (reader port)
        (cond ((eof-object? l)
               (set! state 'eof) *eof-object*)
              ((equal? l --boundary)
               (set! state 'body) *eof-object*)
              ((equal? l --boundary--)
               (set! state 'epilogue) *eof-object*)
              (else l))))
    ;; skip prologue
    (do () ((not (eq? state 'prologue))) (line-reader port))
    ;; main part
    (let loop ((index 0)
               (contents '()))
      (let* ((headers (rfc822-header->list port :reader line-reader))
             (r (internal-parse port headers handler
                                line-reader packet index
                                default-type))
             )
        (case state
          ((epilogue)
           ;; skip epilogue
           (do () ((eq? state 'eof)) (line-reader port))
           (set! (ref packet 'content) (reverse! (cons r contents)))
           packet)
          ((eof)
           ;; this level of multipart body is incomplete
           (set! (ref packet 'content) (reverse! (cons r contents)))
           packet)
          (else
           (loop (+ index 1) (cons r contents))))))
    ))

(define (message-parse port packet handler reader)
  (let* ((headers (rfc822-header->list port)))
    (set! (ref packet 'content)
          (list
           (internal-parse port headers handler reader packet 0
                           '("text" "plain" ("charset" . "us-ascii")))))
    packet))

;;===============================================================
;; Body readers
;;

(define (mime-retrieve-body packet reader inp outp)

  (define (read-text decoder)
    (let loop ((line (reader inp))
               (crlf #f))
      (unless (eof-object? line)
        (when crlf (newline outp))
        (display (decoder line) outp)
        (loop (reader inp) #t))))

  (define (read-base64)
    (let ((lines (port->list reader inp)))
      (with-output-to-port outp
        (lambda ()
          (with-input-from-string (string-concatenate lines)
            (lambda ()
              (with-port-locking (current-input-port)
                base64-decode)))))))

  (with-port-locking inp
    (lambda ()
      (let ((enc (ref packet 'transfer-encoding)))
        (cond
         ((string-ci=? enc "base64") (read-base64))
         ((string-ci=? enc "quoted-printable")
          (read-text quoted-printable-decode-string))
         ((member enc '("7bit" "8bit" "binary"))
          (read-text identity))))))
  )

(define (mime-body->string packet reader inp)
  (let ((s (open-output-string/private)))
    (mime-retrieve-body packet reader inp s)
    (get-output-string s)))

(define (mime-body->file packet reader inp filename)
  (call-with-output-file filename
    (lambda (outp)
      (with-port-locking outp
        (cut mime-retrieve-body packet reader inp outp))))
  filename)

(provide "rfc/mime")
