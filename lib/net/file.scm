;;;; net.file -- universal file access

(define-module net.file
  (use srfi-2)      ;; and-let*
  (use rfc.uri)
  (extend net.file.base)
  (export uri->object))
(select-module net.file)

;;; schemas (see rfc1630, rfc1738 and rfc2396)

;;; standard

;; file://<host>/<path>
;;   file:///<path> is localhost
;;   file:<path> is not valid but might be allowed if path has 0 or 1 leading /'s
;; http://<host>:<port>/<path>?<query>#<fragment> (or https)
;; ftp://<user>:<password>@<host>:port/path/to/file;type (type = A or I) ()
;; mailto:addr1,addr2?headers (rfc2368)
;; news:<newsgroup> (also snews)
;; news:<message-id>
;; nntp://<host>:<port>/<newsgroup-name>/<article-number>
;; mid:<message-id> (no general way to search for mid, need app help)
;; cid:<relative> (relative to a given message only)
;; data: (rfc2397)
;; ldap: (rfc2255)
;; pop: (rfc2384)
;; rtsp: (rfc2326) (also rtspu)
;; rsync: (http://rsync.samba.org/)
;; sip: (3261) (also sips)
;; urn: Uniform Resource Names (rfc2141)
;; urn:isbn: International Standard Book Numbers (rfc3187)
;; urn:oid: Object Identifiers (rfc3061)

;;; old stuff

;; gopher://<host>:<port>/file ()
;; wais://<host>:<port>/<database>
;; wais://<host>:<port>/<database>?<search>
;; wais://<host>:<port>/<database>/<wtype>/<wpath>

;;; shells (command form can be used as source, file form as source or sink)

;; telnet://<user>:<password>@<host>:<port>/
;; rlogin://<user>:<password>@<host>:<port>/
;; ssh://<user>:<password>@<host>:<port>/
;;   * ssh://<user>:<password>@<host>:<port>/<path> (like scp)
;;   * ssh://<user>:<password>@<host>:<port>/?<opt>#<command>
;; tn3270 (like telnet but for IBM mainframes)

;;; extensions

;; cvs:$CVSROOT#<path>
;;   * cvs:/usr/local/cvsroot;mymodule/path/to/file#version
;;   * cvs::pserver:user@host:/path/to/repos;mymodule/path/to/file
;; man:<man-page>#section
;; info:<info-node>

;; allow for dynamic addition and management of new uri methods
(define (uri->object uri)
  (receive (scheme specific) (uri-scheme&specific uri)
    (unless scheme (set! scheme "file"))
    (and-let*
        ((path-name (string-append "net/file/" scheme))
         (mod-name (string->symbol (string-append "net.file." scheme)))
         (proc-name (string->symbol (string-append "uri->" scheme
                                                   "-object"))))
      (eval `(require ,path-name) (current-module))
      (and-let* ((module (find-module mod-name))
                 (proc (eval proc-name module)))
        (proc specific)))))

(define-method uri-scheme-of ((uri <string>))
  (receive (scheme specific) (uri-scheme&specific uri) scheme))

(define-method object->uri ((uri <uri-object>))
  (string-append (uri-scheme-of uri) ":" (slot-ref uri 'path)))

(define-method open-input-uri ((uri <string>))
  (and-let* ((obj (uri->object uri)))
    (unless (string? obj)
      (open-input-uri (uri->object uri)))))

(define-method fetch-uri ((uri <string>))
  (fetch-uri (uri->object uri)))

(define-method fetch-uri ((uri <uri-object>))
  (and-let* ((p (open-input-uri uri)))
    (port->string p)))

(define-method open-output-uri ((uri <string>))
  (open-output-uri (uri->object uri)))

(define-method put-uri ((uri <string>) data)
  (put-uri (uri->object uri) data))

(define-method put-uri ((uri <uri-object>) data)
  (and-let* ((p (open-output-uri uri)))
    (display data p)))

(define-method uri-directory-objects ((uri <string>))
  (uri-directory-objects (uri->object uri)))

(define-method uri-directory-objects ((uri <uri-object>))
  '())

(define-method uri-directory-uris ((uri <string>))
  (uri-directory-uris (uri->object uri)))

(define-method uri-directory-uris ((uri <uri-object>))
  (map object->uri (uri-directory-objects uri)))
