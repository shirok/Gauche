;;;
;;; uri.scm - parse and construct URIs
;;;
;;;   Copyright (c) 2000-2013  Shiro Kawai  <shiro@acm.org>
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
  (use gauche.charconv)
  (export uri-scheme&specific uri-decompose-hierarchical
          uri-decompose-authority uri-parse
          uri-merge uri-compose
          uri-decode uri-decode-string
          uri-encode uri-encode-string
          *rfc2396-unreserved-char-set*
          *rfc3986-unreserved-char-set*
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
  (cond [(#/^([A-Za-z][A-Za-z0-9+.-]*):/ uri)
         => (lambda (m) (values (string-downcase (m 1)) (m 'after)))]
        [else (values #f uri)]))

(define (uri-decompose-hierarchical specific)
  (cond
   [(and (string? specific)
         (#/^(?:\/\/([^\/?#]*))?([^?#]+)?(?:\?([^#]*))?(?:#(.*))?$/ specific))
    => (lambda (m) (values (m 1) (m 2) (m 3) (m 4)))]
   [else (values #f #f #f #f)]))

(define (uri-decompose-authority authority)
  (cond
   [(and (string? authority)
         (#/^(?:(.*?)@)?([^:]*)(?::(\d*))?$/ authority))
    => (lambda (m) (values (m 1) (m 2) (m 3)))]
   [else (values #f #f #f)]))

;; A common cliche (suggested by Kouhei Sutou)
;; Returns: scheme, user-info, host, port, path, query, fragment
(define (uri-parse uri)
  (define (filter-non-empty-string str)
    (and (string? str)
         (not (string-null? str))
         str))
  (receive (scheme specific)
      (uri-scheme&specific uri)
    (receive (authority path query fragment)
        (uri-decompose-hierarchical specific)
      (receive (user-info host port)
          (uri-decompose-authority authority)
        (values scheme
                user-info
                (filter-non-empty-string host)
                (and port (string->number port))
                (filter-non-empty-string path)
                query
                fragment)))))

;;==============================================================
;; Generic constructor
;;

(define (uri-compose :key (scheme #f) (userinfo #f) (host #f) (port #f)
                     (authority #f) (path  #f) (path* #f) (query #f)
                     (fragment #f) (specific #f))
  (with-output-to-string
    (lambda ()
      (when scheme (display scheme) (display ":"))
      (if specific
        (display specific)
        (begin
          (display "//")
          (if authority
            (begin (display authority))
            (begin
              (when userinfo (display userinfo) (display "@"))
              (when host     (display host))
              (when port     (display ":") (display port))))
          (if path*
            (begin
              (unless (string-prefix? "/" path*) (display "/"))
              (display path*))
            (begin
              (if path
                (begin (unless (string-prefix? "/" path) (display "/"))
                       (display path))
                (display "/"))
              (when query (display "?") (display query))
              (when fragment (display "#") (display fragment))))
          ))
      )))

;;==============================================================
;; Relative -> Absolute
;;

;; This function is originally implemented by teppey
;; (from http://d.hatena.ne.jp/teppey/20110517/1305613493).
;; By the author's permission I took it and modified to include here.
;; NB: CL has (merge-pathname <path> <base>).  I always felt it
;; counterintuitive, since <path> is likely to be concatenated
;; after (some part of) <base>, unless <path> fully specifies all components.
;; Here we reverse the arguments, so that we can merge multiple fragments
;; of uris.
(define (uri-merge base-uri rel-uri . more)
  (define (rec base-uri rel-uri more initial?)
    (if (null? more)
      (uri-merge-1 base-uri rel-uri initial?)
      (rec (uri-merge-1 base-uri rel-uri initial?) (car more) (cdr more) #f)))
  (rec base-uri rel-uri more #t))

(define (uri-merge-1 base-uri rel-uri pre-normalize?)
  (define (split uri)
    (receive (scheme specific)
        (uri-scheme&specific uri)
      (receive (authority path query fragment)
          (uri-decompose-hierarchical specific)
        (values scheme authority path query fragment))))

  ;; RFC3986, Section 5.2.3
  ;;  we do prenormalize base-path, so it never be #f.
  (define (merge base-path rel-path base-authority)
    (if (and base-authority (string-null? base-path))
      (string-append "/" rel-path)
      (if-let1 m (#/^(.*)\/[^\/]*$/ base-path)
        (string-append (m 1) "/" rel-path)
        rel-path)))

  ;; RFC3986, Section 5.2.4
  (define (canonicalize path)
    (let loop ([input path] [output '()])
      (cond [(not input) (string-concatenate (reverse! output))]
            ;; RFC3986, Section 5.2.4, 2A
            [(#/^\.{1,2}\/(.*)$/ input) => (^m (loop (m 1) output))]
            ;; RFC3986, Section 5.2.4, 2B
            [(#/^(?:\/\.\/(.*)|\/\.)$/ input)
             => (^m (loop (string-append "/" (or (m 1) "")) output))]
            ;; RFC3986, Section 5.2.4, 2C
            [(#/^(?:\/\.\.\/(.*)|\/\.\.)$/ input)
             => (^m (loop (string-append "/" (or (m 1) ""))
                          (if (not (null? output)) (cdr output) output)))]
            ;; RFC3986, Section 5.2.4, 2D
            [(#/^\.{1,2}$/ input) (loop #f output)]
            ;; RFC3986, Section 5.2.4, 2E
            [(#/^(\/?[^\/]+)(\/.*)?$/ input)
             => (^m (loop (m 2) (cons (m 1) output)))]
            [else (loop #f (cons input output))])))

  ;; RFC3986, Section 5.3
  (define (recompose scheme authority path query fragment)
    (with-output-to-string
      (lambda ()
        (when scheme    (display scheme) (display ":"))
        (when authority (display "//") (display authority))
        (display path)
        (when query     (display "?") (display query))
        (when fragment  (display "#") (display fragment)))))

  (receive (b.scheme b.authority b.path b.query b.fragment) (split base-uri)
    (when pre-normalize?
      (set! b.path (canonicalize b.path))) ;pre-normalization (section 5.2.1)
    (receive (r.scheme r.authority r.path r.query r.fragment) (split rel-uri)
      (recompose (or r.scheme b.scheme)
                 (if r.scheme r.authority (or r.authority b.authority))
                 (cond [(or r.scheme r.authority) (canonicalize r.path)]
                       [(not r.path) b.path]
                       [(string-prefix? "/" r.path) (canonicalize r.path)]
                       [else (canonicalize (merge b.path r.path b.authority))])
                 (if (or r.scheme r.authority r.path r.query) r.query b.query)
                 r.fragment))))

;;==============================================================
;; Encoding & decoding
;;
;;  NB. Which character to encode, and when to encode/decode depend on
;;  the semantics of specific URI scheme.
;;  These procedures provides basic building components.

(define (uri-decode :key (cgi-decode #f))
  (let loop ((c (read-char)))
    (cond [(eof-object? c)]
          [(char=? c #\%)
           (let1 c1 (read-char)
             (cond
              [(eof-object? c1) (write-char c)] ;; just be permissive
              [(digit->integer c1 16)
               => (lambda (i1)
                    (let1 c2 (read-char)
                      (cond [(eof-object? c2) (write-char c) (write-char c1)]
                            [(digit->integer c2 16)
                             => (lambda (i2)
                                  (write-byte (+ (* i1 16) i2))
                                  (loop (read-char)))]
                            [else (write-char c) (write-char c1) (loop c2)])))]
                   [else (write-char c) (loop c1)]))]
          [(char=? c #\+)
           (if cgi-decode (write-char #\space) (write-char #\+))
           (loop (read-char))]
          [else (write-char c) (loop (read-char))])))

(define (uri-decode-string string :key (encoding (gauche-character-encoding))
                           :allow-other-keys args)
  (define (wrap out)
    (wrap-with-output-conversion out (gauche-character-encoding)
                                 :from-code encoding))
  (call-with-string-io string
    (lambda (in out)
      (with-ports in (wrap out) (current-error-port)
        (lambda ()
          (apply uri-decode args)
          (close-output-port (current-output-port)))))))

;; Default set of characters that can be passed without escaping.
;; See 2.3 "Unreserved Characters" of RFC 2396.  It is slightly
;; revised in RFC3986.
(define-constant *rfc2396-unreserved-char-set* #[-_.!~*'()0-9A-Za-z])
(define-constant *rfc3986-unreserved-char-set* #[-_.~0-9A-Za-z])

;; NB: Converts byte by byte, instead of chars, to avoid complexity
;; from different character encodings (suggested by Fumitoshi UKAI).
;; 'noescape' char-set is only valid in ASCII range.  All bytes
;; larger than #x80 are encoded unconditionally.
(define (uri-encode :key ((:noescape echars) *rfc3986-unreserved-char-set*))
  (let loop ((b (read-byte)))
    (unless (eof-object? b)
      (if (and (< b #x80)
               (char-set-contains? echars (integer->char b)))
        (write-byte b)
        (format #t "%~2,'0x" b))
      (loop (read-byte)))))

(define (uri-encode-string string :key (encoding (gauche-character-encoding))
                           :allow-other-keys args)
  (define (wrap in)
    (wrap-with-input-conversion in (gauche-character-encoding)
                                :to-code encoding))
  (call-with-string-io string
    (lambda (in out)
      (with-ports (wrap in) out (current-error-port)
        (cut apply uri-encode args)))))

