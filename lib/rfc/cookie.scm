;;;
;;; cookie.scm - parse and construct http state information
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
;;;  $Id: cookie.scm,v 1.1 2001-09-22 23:47:57 shirok Exp $
;;;

;; Parser and constructor of http "Cookies" defined in
;; RFC 2965 HTTP state managemnet mechanism
;;   <ftp://ftp.isi.edu/in-notes/rfc2965.txt>
;; See also
;; RFC 2964 Use of HTTP state management
;;   <ftp://ftp.isi.edu/in-notes/rfc2964.txt>
;; The parser also supports the old Netscape spec
;;   <http://www.netscape.com/newsref/std/cookie_spec.html>

(define-module rfc.cookie
  (use srfi-1)
  (use srfi-2)
  (use srfi-13)
  (export parse-cookie)
  )
(select-module rfc.cookie)

;; utility fn.  breaks  ``attr=value;attr=value ... '' into alist.
;; version is a cookie version.  if version>0, we allow comma as the
;; delimiter as well as semicolon.
(define (parse-av-pairs input version)
  (define attr-regexp
    (if (= version 0)
        #/\s*([\w$_]+)\s*([=\;]\s*)?/
        #/\s*([\w$_]+)\s*([=\;,]\s*)?/))
  (define attr-delim
    (if (= version 0) #\; #[,\;]))
  
  (define (read-attr input r)
    (cond ((string-null? input) (reverse! r))
          ((rxmatch attr-regexp input)
           => (lambda (m)
                (if (and-let* ((delimiter (rxmatch-substring m 2)))
                      (string-prefix? "=" delimiter))
                    (let ((attr (rxmatch-substring m 1))
                          (rest (rxmatch-after m)))
                      (if (string-prefix? "\"" rest)
                          (read-token-quoted attr (string-drop rest 1) r)
                          (read-token attr rest r)))
                    (read-attr (rxmatch-after m)
                               (acons (rxmatch-substring m 1) #f r)))))
          (else
           ;; the input is broken; for now, we ignore the rest.
           (reverse! r))))
  (define (read-token attr input r)
    (cond ((string-index input attr-delim)
           => (lambda (i)
                (read-attr (string-drop input (+ i 1))
                           (acons attr
                                  (string-trim-right (string-take input i))
                                  r))))
          (else
           (reverse! (acons attr (string-trim-right input) r)))))
  (define (read-token-quoted attr input r)
    (let loop ((input input)
               (partial '()))
      (cond ((string-index input #[\\\"])
             => (lambda (i)
                  (let ((c (string-ref input i)))
                    (if (char=? c #\\)
                        (if (< (string-length input) (+ i 1))
                            (error-unterminated attr)
                            (loop (string-drop input (+ i 2))
                                  (list* (string (string-ref input (+ i 1)))
                                         (string-take input i)
                                         partial)))
                        (read-attr (string-drop input (+ i 1))
                                   (acons attr
                                          (string-concatenate-reverse
                                           (cons (string-take input i)
                                                 partial))
                                          r))))))
            (else (error-unterminated attr)))))
  (define (error-unterminated attr)
    (error "Unterminated quoted value given for attribute" attr))

  (read-attr input '()))

;; Parses the header value of "Cookie" request header.
;; If cookie version is known by "Cookie2" request header, it should
;; be passed to version (as integer).  Otherwise, it figures out
;; the cookie version from input.
;;
;; Returns the following format.
;;   ((<name> <value> [:path <path>] [:domain <domain>] [:port <port>])
;;    ...)

(define (parse-cookie input . version)
  (let ((ver (cond ((and (pair? version) (integer? (car version)))
                    (car version))
                   ((rxmatch #/^\s*$Version\s*=\s*(\d+)/ input)
                    => (lambda (m)
                         (string->number (rxmatch-substring m 1))))
                   (else 0))))
    (let loop ((av-pairs (parse-av-pairs input ver))
               (r '())
               (current '()))
      (cond ((null? av-pairs)
             (if (null? current)
                 (reverse r)
                 (reverse (cons (reverse current) r))))
            ((string-ci=? "$path" (caar av-pairs))
             (loop (cdr av-pairs) r (list* (cdar av-pairs) :path current)))
            ((string-ci=? "$domain" (caar av-pairs))
             (loop (cdr av-pairs) r (list* (cdar av-pairs) :domain current)))
            ((string-ci=? "$port" (caar av-pairs))
             (loop (cdr av-pairs) r (list* (cdar av-pairs) :port current)))
            (else
             (if (null? current)
                 (loop (cdr av-pairs) r (list (cdar av-pairs) (caar av-pairs)))
                 (loop (cdr av-pairs)
                       (cons (reverse current) r)
                       (list (cdar av-pairs) (caar av-pairs)))))))
    ))

(provide "rfc/cookie")
