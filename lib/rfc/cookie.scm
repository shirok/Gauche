;;;
;;; cookie.scm - parse and construct http state information
;;;
;;;   Copyright (c) 2000-2022  Shiro Kawai  <shiro@acm.org>
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

;; Parser and constructor of http "Cookies" defined in
;; RFC 6265 HTTP state managemnet mechanism
;;   http://tools.ietf.org/html/rfc6265
;; See also
;; RFC 2964 Use of HTTP state management
;;   <ftp://ftp.isi.edu/in-notes/rfc2964.txt>
;; The parser also supports the old Netscape spec
;;   <http://www.netscape.com/newsref/std/cookie_spec.html>

(define-module rfc.cookie
  (use scheme.list)
  (use gauche.threads)
  (use gauche.sequence)
  (use srfi-13)
  (use srfi-19)
  (use util.match)
  (export parse-cookie-string
          construct-cookie-string

          parse-cookie-date

          make-cookie-jar
          cookie-jar-put!
          cookie-jar-get
          cookie-jar-purge-ephemeral!
          )
  )
(select-module rfc.cookie)

;;==============================================================
;; Cookie header parser and constructor
;;

;; utility fn.  breaks  ``attr=value;attr=value ... '' into alist.
;; version is a cookie version.  if version>0, we allow comma as the
;; delimiter as well as semicolon.
(define (parse-av-pairs input version)
  (define attr-regexp
    (if (= version 0)
      #/\s*([\w$_.-]+)\s*([=\;]\s*)?/
      #/\s*([\w$_.-]+)\s*([=\;,]\s*)?/))
  (define attr-delim
    (if (= version 0) #\; #[,\;]))

  (define (read-attr input r)
    (cond [(string-null? input) (reverse! r)]
          [(rxmatch attr-regexp input)
           => (^m (if (and-let* ([delimiter (rxmatch-substring m 2)])
                        (string-prefix? "=" delimiter))
                    (let ([attr (rxmatch-substring m 1)]
                          [rest (rxmatch-after m)])
                      (if (string-prefix? "\"" rest)
                        (read-token-quoted attr (string-drop rest 1) r)
                        (read-token attr rest r)))
                    (read-attr (rxmatch-after m)
                               (acons (rxmatch-substring m 1) #f r))))]
          [else
           ;; the input is broken; for now, we ignore the rest.
           (reverse! r)]))
  (define (read-token attr input r)
    (cond [(string-index input attr-delim)
           => (^i (read-attr (string-drop input (+ i 1))
                             (acons attr
                                    (string-trim-right (string-take input i))
                                    r)))]
          [else
           (reverse! (acons attr (string-trim-right input) r))]))
  (define (read-token-quoted attr input r)
    (let loop ([input input]
               [partial '()])
      (cond ([string-index input #[\\\"]]
             => (^i (let1 c (string-ref input i)
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
            [else (error-unterminated attr)])))
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

(define (parse-cookie-string input . version)
  (let1 ver (cond [(and (pair? version) (integer? (car version)))
                   (car version)]
                  [(rxmatch #/^\s*$Version\s*=\s*(\d+)/ input)
                   => (^m (string->number (rxmatch-substring m 1)))]
                  [else 0])
    (let loop ([av-pairs (parse-av-pairs input ver)]
               [r '()]
               [current '()])
      (cond [(null? av-pairs)
             (if (null? current)
               (reverse r)
               (reverse (cons (reverse current) r)))]
            [(string-ci=? "$path" (caar av-pairs))
             (loop (cdr av-pairs) r (list* (cdar av-pairs) :path current))]
            [(string-ci=? "$domain" (caar av-pairs))
             (loop (cdr av-pairs) r (list* (cdar av-pairs) :domain current))]
            [(string-ci=? "$port" (caar av-pairs))
             (loop (cdr av-pairs) r (list* (cdar av-pairs) :port current))]
            [else
             (if (null? current)
               (loop (cdr av-pairs) r (list (cdar av-pairs) (caar av-pairs)))
               (loop (cdr av-pairs)
                     (cons (reverse current) r)
                     (list (cdar av-pairs) (caar av-pairs))))]))
    ))

;; Construct a cookie string suitable for Set-Cookie or Set-Cookie2 header.
;; specs is the following format.
;;
;;   ((<name> <value> [:comment <comment>] [:comment-url <comment-url>]
;;                    [:discard <bool>] [:domain <domain>] [:http-only <bool>]
;;                    [:max-age <age>] [:path <value>] [:port <port-list>]
;;                    [:secure <bool>] [:version <version>] [:expires <date>]
;;    ) ...)
;;
;; Returns a list of cookie strings for each <name>=<value> pair.  In the
;; ``new cookie'' implementation, you can join them by comma and send it
;; at once with Set-cookie2 header.  For the old netscape protocol, you
;; must send each of them by Set-cookie header.

(define (construct-cookie-string specs . version)
  (let1 ver (if (and (pair? version) (integer? (car version)))
              (car version)
              1)
    (map (^[spec] (construct-cookie-string-1 spec ver)) specs)))

(define (construct-cookie-string-1 spec ver)
  (when (< (length spec) 2)
    (error "bad cookie spec: at least <name> and <value> required" spec))
  (let ([name (car spec)]
        [value (cadr spec)])
    (let loop ([attr (cddr spec)]
               [r    (list (if value
                             (string-append name "="
                                            (quote-if-needed value))
                             name))])
      (define (next s) (loop (cddr attr) (cons s r)))
      (define (ignore) (loop (cddr attr) r))
      (cond
       [(null? attr) (string-join (reverse r) ";")]
       [(null? (cdr attr))
        (errorf "bad cookie spec: attribute ~s requires value" (car attr))]
       [(eqv? :comment (car attr))
        (if (> ver 0)
          (next (string-append "Comment=" (quote-if-needed (cadr attr))))
          (ignore))]
       [(eqv? :comment-url (car attr))
        (if (> ver 0)
          (next (string-append "CommentURL=" (quote-value (cadr attr))))
          (ignore))]
       [(eqv? :discard (car attr))
        (if (and (> ver 0) (cadr attr)) (next "Discard") (ignore))]
       [(eqv? :domain (car attr))
        (next (string-append "Domain=" (cadr attr)))]
       [(eqv? :max-age (car attr))
        (if (> ver 0)
          (next (format #f "Max-Age=~a" (cadr attr)))
          (ignore))]
       [(eqv? :path (car attr))
        (next (string-append "Path=" (quote-if-needed (cadr attr))))]
       [(eqv? :port (car attr))
        (if (> ver 0)
          (next (string-append "Port=" (quote-value (cadr attr))))
          (ignore))]
       [(eqv? :secure (car attr))
        (if (cadr attr) (next "Secure") (ignore))]
       [(eqv? :http-only (car attr))
        (if (cadr attr) (next "HttpOnly") (ignore))]
       [(eqv? :version (car attr))
        (if (> ver 0)
          (next (format #f "Version=~a" (cadr attr)))
          (ignore))]
       [(eqv? :expires (car attr))
        (if (> ver 0)
          (ignore)
          (next (make-expires-attr (cadr attr))))]
       [else (error "Unknown cookie attribute" (car attr))])
      ))
  )

;; aux. function to quote value
(define (quote-value value)
  (string-append "\"" (regexp-replace-all #/\"|\\/ value "\\\\\\0") "\""))

(define (quote-if-needed value)
  (if (rxmatch #/[\",\;\\ \t\n]/ value)
    (quote-value value)
    value))

(define (make-expires-attr time)
  (define (ensure-time-string time)
    (cond
     [(number? time)
      (sys-strftime "%a, %d-%b-%Y %H:%M:%S GMT" (sys-gmtime time))]
     [(is-a? time <date>)
      (date->string time "~a, ~d-~@b-~Y ~H:~M:~S GMT")]
     [(is-a? time <time>)
      (case (time-type time)
        [(time-utc) (ensure-time-string (time-utc->date time 0))]
        [(time-tai) (ensure-time-string (time-tai->date time 0))]
        [(time-monotonic) (ensure-time-string (time-monotonic->date time 0))]
        [else (errorf "Don't know how to convert a time object ~s to string."
                      time)])]
     [else time]))

    (format #f "Expires=~a" (ensure-time-string time)))

;;==============================================================
;; Cookie-jar, a client-side storage of cookies used by <http-connection>
;;

;; From outside, <cookie-jar> should be treated as an opaque object.
;; The opeartions on <cookie-jar> is thread-safe, so that it can easily
;; be shared by multiple connections.

;; Client-side cookie representation.  Immutable.
;;
;; Max-Age is converted to the absolute time when the cookie shall be
;; discarded.
;;
;; request-host is not an attribute in the cookie string, but should be
;; passed by the caller in client side to set which host the cookie
;; string is sent from.  It is used only when the cookie is missing Domain
;; attribute.
(define-class <http-cookie> ()
  ((name    :init-keyword :name)
   (value   :init-keyword :value)
   (domain  :init-keyword :domain :init-value #f) ;lower case
   (request-host :init-keyword :source-domain :init-value #f)
   (path    :init-keyword :path :init-value #f)
   (lifetime :init-keyword :lifetime :init-value #f) ; #f or <time>
   (port    :init-keyword :port :init-value '())  ; list of port numbers
   (secure  :init-keyword :secure :init-value #f)
   (version :init-keyword :version :init-value 1)
   (comment :init-keyword :comment :init-value #f)
   (comment-url :init-keyword :comment-url :init-value #f)
   ))

;; For now, we just use a linear list.  May change if performance becomes
;; the issue.   The user should treat <cookie-jar> an opaque object.
(define-class <cookie-jar> (<collection>)
  ((%table :init-keyword :%table)))     ;private


;; RFC6265 5.1.1
(define (parse-cookie-date str)
  (let loop ([tokens ($ string-split str
                        #[\x09\x20-\x2f\x3b-\x40\x5b-\x60\x7b-\x7e])]
             [hour #f]
             [minute #f]
             [second #f]
             [day #f]
             [month #f]
             [year #f])
    (cond [(null? tokens)
           (and hour minute second day month year
                (<= 0 hour 23)
                (<= 0 minute 59)
                (<= 0 second 59)
                (<= 1 day 31)
                (<= 1601 year)
                (make-date 0 second minute hour day month year 0))] ;UTC
          [(#/^(\d{1,2}):(\d{1,2}):(\d{1,2})\D?/ (car tokens))
           => (^m (if hour
                   (loop (cdr tokens) hour minute second day month year)
                   (loop (cdr tokens)
                         (string->number (m 1))
                         (string->number (m 2))
                         (string->number (m 3))
                         day month year)))]
          [(#/^(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)/i (car tokens))
           => (^m (loop
                   (cdr tokens) hour minute second day
                   (or month
                       (find-index (cute string-ci=? <> (m 1))
                                   '("" "jan" "feb" "mar" "apr" "may" "jun"
                                     "jul" "aug" "sep" "oct" "nov" "dec")))
                   year))]
          [(#/^\d{1,4}/ (car tokens))
           => (^m (let1 n (string->number (m 0))
                    (cond [(< n 10)
                           (loop (cdr tokens) hour minute second
                                 (or day n) month year)]
                          [(<= 10 n 99)
                           (if day
                             (loop (cdr tokens) hour minute second day
                                   month (or year
                                             (if (<= 70 n)
                                               (+ 1900 n)
                                               (+ 2000 n))))
                             (loop (cdr tokens) hour minute second
                                   n month year))]
                          [else
                           (loop (cdr tokens) hour minute second day
                                 month (or year n))])))]
          [else (loop (cdr tokens) hour minute second day month year)])))


(define (%put-cookie! jar cookie)
  ;; ok, this isn't ideal, but just for now...
  ($ atomic-update! (~ jar'%table)
     (^[table]
       (cons cookie
             (remove (^e (and (equal? (~ e'domain) (~ cookie'domain))
                              (equal? (~ e'name) (~ cookie'name))))
                     table)))))

(define (%remove-cookie! jar domain name)
  ($ atomic-update! (~ jar'%table)
     (^[table] (remove (^e (and (equal? (~ e'domain) domain)
                                (equal? (~ e'name) name)))
                       table))))

(define (%compute-lifetime max-age expires)
  ;; max-age has precedence (RFC6265 4.1.2.2)
  (cond [(and-let* ([  max-age ]
                    [secs (string->number max-age)])
           (if (< secs 0)
             (make-time time-utc 0 0)
             (add-duration (current-time)
                           (make-time time-duration 0 secs))))]
        [expires (date->time-utc (parse-cookie-date expires))]
        [else #f]))

;; 5.1.4
(define (%default-path uri-path)
  (if (or (equal? uri-path "")
          (not (eqv? (~ uri-path 0) #\/)))
    "/"
    (let1 xs (drop-right (string-split uri-path "/" 'prefix) 1)
      (if (null? xs)
        "/"
        (string-join xs "/" 'prefix)))))

;; 5.1.4
(define (%path-match request-path cookie-path)
  (or (equal? cookie-path request-path)
      (and (string-prefix? cookie-path request-path)
           (or (boolean (#/\/$/ cookie-path))
               (eqv? (string-ref request-path (string-length cookie-path))
                     #\/)))))


(define (%domain-belongs-to? sub parent)
  (let loop ([sub-components (reverse (string-split sub #\.))]
             [par-components (reverse (string-split parent #\.))])
    (cond [(null? sub-components) (null? par-components)]
          [(null? par-components)]
          [(string-ci=? (car sub-components) (car par-components))
           (loop (cdr sub-components) (cdr par-components))]
          [else #f])))

;; API
(define (make-cookie-jar :optional (proto #f))
  (assume-type proto (<?> <cookie-jar>))
  (make <cookie-jar>
    :%table (if proto
              (atom (list-copy (atom-ref (~ proto'%table))))
              (atom '()))))

;; API
(define (cookie-jar-put! jar request-host request-path parsed-cookies)
  (define (put-1 parsed-cookie)
    (match parsed-cookie
      [(name value . args)
       (let-keywords args ([path #f] [domain #f] [port #f]
                           [discard #f] [max-age #f] [expires #f]
                           [secure #f] [http-only #f] [version #f]
                           [comment #f] [comment-url #f]
                           . other)
         (unless (null? other)
           (error "Invalid cookie attribute in ~s" parsed-cookie))
         (when (or (not domain)
                   ;; if request-host doesn't belong to domain, just ignore
                   (%domain-belongs-to? request-host domain))
           (if (or (not value) (equal? value ""))
             (%remove-cookie! jar (or domain request-host) name)
             (%put-cookie! jar
                           (make <http-cookie>
                             :name name
                             :value value
                             :domain (string-downcase (or domain request-host))
                             :request-host request-host
                             :path (or path (%default-path request-path))
                             :lifetime (%compute-lifetime max-age expires)
                             :port (if port
                                     (string-tokenize port)
                                     '())
                             :secure secure
                             :version version
                             :comment comment
                             :comment-url comment-url)))))]
      [_ (error "Invalid cookie format: ~s" parsed-cookie)]))

  (for-each put-1 parsed-cookies))

;; API
(define (cookie-jar-get jar request-host request-port request-path)
  (define now (current-time))
  (define (alive? c) (or (not (~ c'lifetime))
                         (time<=? (~ c'lifetime) now)))
  (define (match? c) (and (%domain-belongs-to? request-host (~ c'domain))
                          (%path-match request-path (~ c'path))
                          (or (null? (~ c'port))
                              (memv request-port (~ c'port)))))

  (values-ref
   ($ atomic-update! (~ jar'%table)
      (^[cookies]
        (let loop ([keep '()] [matched '()] [cookies cookies])
          (match cookies
            [() (values (reverse keep) (reverse matched))]
            [(c . cookies)
             (if (alive? c)
               (loop (cons c keep)
                     (if (match? c) (cons c matched) matched)
                     cookies)
               (loop keep matched cookies))]))))
   1))

;; API
;; Discard non-persistent cookies
(define (cookie-jar-purge-ephemeral! jar)
  (let1 now (current-time)
    (atomic-update! (~ jar'%table)
                    (^[table] (filter (^e (~ e'lifetime)) table)))))
