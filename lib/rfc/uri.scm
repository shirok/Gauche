;;;
;;; uri.scm - parse and construct URIs
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
;;;  $Id: uri.scm,v 1.6 2001-08-29 11:28:31 shirok Exp $
;;;

;; Main reference:
;; RFC2396 Uniform Resource Identifiers (URI): Generic Syntax
;;  <ftp://ftp.isi.edu/in-notes/rfc2396.txt>

;; Historical:
;; RFC1738 Uniform Resource Locators
;;  <ftp://ftp.isi.edu/in-notes/rfc1738.txt>
;; RFC1808 Relative Uniform Resource Locators
;;  <ftp://ftp.isi.edu/in-notes/rfc1808.txt>
;; RFC2368 The mailto URL Scheme
;;  <ftp://ftp.isi.edu/in-notes/rfc2368.txt>

(define-module rfc.uri
  (use srfi-13)
  (use gauche.regexp)
  (use gauche.let-opt)
  (export uri-scheme&specific uri-decompose-hierarchical
          uri-decompose-authority
          uri-decode uri-decode-string
          uri-encode uri-encode-string
          )
  )
(select-module rfc.uri)

;;==============================================================
;; Generic parser
;;

;; Splits URI scheme and the scheme specific part from given URI.
;; If URI doesn't follow the generic URI syntax, it is regarded
;; as a relative URI and #f is returned for the scheme.
;; The escaped characters of the scheme specific part is not unescaped;
;; their interpretation is dependent on the scheme.

(define (uri-scheme&specific uri)
  (cond ((rxmatch #/^([A-Za-z][A-Za-z0-9+.-]*):/ uri)
         => (lambda (match)
              (values (string-downcase (rxmatch-substring match 1))
                      (rxmatch-after match))))
        (else (values #f uri))))

(define (uri-decompose-hierarchical specific)
  (rxmatch-if
      (rxmatch #/^(\/\/([^\/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?$/ specific)
      (#f #f authority path #f query #f fragment)
    (values authority path query fragment)
    (values #f #f #f #f)))

(define (uri-decompose-authority authority)
  (rxmatch-if
      (rxmatch #/^([^@]*@)?([^:]*)(:(\d*))?$/ authority)
      (#f userinfo host #f port)
    (values userinfo host port)
    (values #f #f #f)))



;;==============================================================
;; Relative -> Absolute
;;


;;==============================================================
;; Encoding & decoding
;;
;;  NB. Which character to encode, and when to encode/decode depend on
;;  the semantics of specific URI scheme.
;;  These procedures provides basic building components.

(define (uri-decode)
  (define (hex c)
    (cond ((not (char? c)) #f)
          ((char<?  c #\0) #f)
          ((char<=? c #\9) (- (char->integer c) #x30))
          ((char<?  c #\A) #f)
          ((char<=? c #\F) (- (char->integer c) #x37))
          ((char<?  c #\a) #f)
          ((char<=? c #\f) (- (char->integer c) #x57))
          (else #f)))
  (let loop ((c (read-char)))
    (cond ((eof-object? c))
          ((char=? c #\%)
           (let ((c1 (read-char)))
             (cond ((hex c1)
                    => (lambda (i1)
                         (let ((c2 (read-char)))
                           (cond ((hex c2)
                                  => (lambda (i2)
                                       (write-byte (+ (* i1 16) i2))
                                       (loop (read-char))))
                                 (else (write-char c)
                                       (write-char c1)
                                       (loop c2))))))
                   (else (write-char c)
                         (loop c1)))))
          (else (write-char c)
                (loop (read-char)))
          )))

(define (uri-decode-string string)
  (with-string-io string uri-decode))

;; Default set of characters to be escaped
;; See 2.4.3 "Excluded US-ASCII characters" of RFC 2396
(define *uri-special-char-set* #[\x00-\x20<>#%\"{}|\\^\`\[\]])

(define (uri-encode . args)
  (let-optionals* args ((echars *uri-special-char-set*))
    (let loop ((c (read-char)))
      (cond ((eof-object? c))
            ((char-set-contains? echars c)
             (format #t "%~2,'0x" (char->integer c))
             (loop (read-char)))
            ((char>=? c #\del)
             (let loop1 ((i (char->integer c)))
               (if (< i #x100)
                   (format #t "%~2,'0x" i)
                   (begin
                     (loop1 (quotient i 16))
                     (format #t "%~2,'0x" (modulo i 16)))))
             (loop (read-char)))
            (else (write-char c) (loop (read-char)))))))

(define (uri-encode-string string . args)
  (with-string-io string (lambda () (apply uri-encode args))))

(provide "rfc/uri")
