;;;; gettext.scm -- gettext variant implemented in scheme

;;; Created:    <2002-04-14 11:29:21 foof>
;;; Time-stamp: <2003-01-14 08:47:34 foof>
;;; Author:     Alex Shinn <foof@synthcode.com>

;;; Commentary:

;;;   This is *not* gettext, nor does it use the C gettext library.
;;; This is a simple gettext-like tool I wrote for my own use which
;;; addresses certain shortcomings of gettext, namely:
;;;
;;;   * gettext is single-user, single-app specific
;;;   * gettext only supports one locale/domain at a time
;;;   * gettext is C-centric
;;;
;;; Of course, being C-centric isn't enough of a reason to write my own
;;; library, since we can always just wrap a more convenient interface
;;; around the C functions.  However, the other two issues are real
;;; problems because I would like to use gettext for server
;;; applications, which may be handling multiple users in different
;;; locales.

;;; Currently only the editable (.po) gettext format is supported.

;;; TODO:

;;; Add .mo format support (well documented in gettext info), more for
;;; the convenience of being able to easily use existing messages from
;;; other applications, intermixed with your own messages, rather than
;;; for speed (since messages are cached anyway).

;;; sync/write options so you can use the set functionality to offer
;;; other interfaces to editing messages and easily save the results.

(define-module text.gettext
  (use srfi-2)    ;; and-let*
  (use srfi-13)   ;; string library
  (use rfc.822)   ;; message headers parsing (same syntax for .po meta-data)
  (use file.util) ;; file-is-readable?
  (use gauche.charconv)
  (use gauche.parameter)
  (export
   ;;; standard gettext interface
   gettext textdomain dgettext dcgettext bindtextdomain
   ;;; more flexible interface for building lookups
   make-gettext
   ;;; not yet supported
   ;; ngettext dngettext dcngettext
   ;;; no need for this in gauche
   ;; bind-textdomain-codeset
   ))
(select-module text.gettext)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; store meta info for gettext files

(define-class <gettext-file> ()
  ((filename :init-keyword :filename :initform #f :accessor gettext-file-filename)
   (locale   :init-keyword :locale   :initform #f :accessor gettext-file-locale)
   (encoding :init-keyword :encoding :initform #f :accessor gettext-file-encoding)
   (properties :init-keyword :properties :initform #f :accessor gettext-file-properties)
   ))

(define (make-gettext-file filename locale)
  (make <gettext-file> :filename filename :locale locale))

(define (gettext-file-guess-encoding gfile)
  (let ((locale (gettext-file-locale gfile)))
    (rxmatch-cond
     ;; explicit encoding used in locale name
     ((rxmatch #/\.(.*)$/ locale)
        (#f encoding)
       encoding)
     ;; no encoding, but Japanese so we can auto-detect
     ((rxmatch #/^ja.*/ locale)
        (#f)
       "*JP")
     ;; otherwise use internal encoding (maybe default to utf-8?)
     (else (gauche-character-encoding)))))

(define (gettext-file-update-properties! f)
  (let ((filename (gettext-file-filename f))
        (encoding (gettext-file-guess-encoding f))
        (properties '()))
    ;;(format #t "gettext: file: ~S guess-encoding: ~S\n" filename encoding)
    (when (file-is-readable? filename)
      (and-let* ((property-msg (lookup-message filename "" encoding)))
        (set! properties
              (call-with-input-string property-msg rfc822-header->list))
        (and-let* ((type-ls (assoc "content-type" properties))
                   (type (cadr type-ls))
                   (m (rxmatch "charset=([^\s]+)" type)))
          ;;(format #t "gettext: found encoding: ~S\n" (rxmatch-substring m 1))
          (set! encoding (rxmatch-substring m 1)))
        ;;(format #t "gettext: file: ~S encoding: ~S\n" filename encoding)
        (set! (gettext-file-encoding f) encoding)))
    ;;(format #t "gettext: properties: ~S\n" properties)
    (set! (gettext-file-properties f) properties)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; take a list or a single argument which is interpretted as a one
;; element list
(define (listify arg)
  (if (pair? arg) arg (list arg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the default gettext lookup

(define message-path (make-parameter '("/usr/share/locale")))
(define domain-message-paths (make-hash-table 'equal?))

(define default-accessor (make-parameter #f))
(define (gettext msgid)
  ((default-accessor) 'get msgid))

;; rebind the default domain
(define (textdomain domain)
  (let ((accessor (make-gettext domain)))
    (default-accessor accessor)
    accessor))

(define (bindtextdomain domain dirs)
  (hash-table-put! domain-message-paths domain (listify dirs)))

;; other interfaces
(define (dgettext domain msgid)
  (let ((accessor (make-gettext domain)))
    (accessor 'get msgid)))
(define (dcgettext domain msgid locale)
  (let ((accessor (make-gettext domain (list locale))))
    (accessor 'get msgid)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the gettext .po parser

(define (lookup-message file msg encoding)
  ;; utilities
  (define (tail-str str)
    (call-with-input-string str (lambda (p) (read p) (read p))))
  (define (read-str str p)
    (if (equal? (peek-char p) #\")
      (let reader ((s str)
                   (line (read-line p)))
        (cond ((eof-object? line)
               s)
              ((rxmatch #/^\s*".*"/ line)
               (let ((obj (call-with-input-string line read)))
                 (if (equal? (peek-char p) #\")
                   (reader (string-append s obj) (read-line p))
                   (string-append s obj))))
              (else
               s)))
      str))
  ;; read from the file if it exists
  (if (file-is-readable? file)
    (call-with-input-file file
      (lambda (p)
        ;;(format #t "reading: ~S\n" file)
        (let loop ((line (read-line p)))
          (cond ((eof-object? line)
                 #f)
                ((string-prefix? "msgid " line)
                 ;;(display "msgid\n")
                 (let ((msgid (read-str (tail-str line) p)))
                   ;;(format #t "msgid: ~S\n\n" msgid)
                   (cond ((string=? msgid msg)
                          ;; skip msgstr
                          ;;(display "msgstr\n")
                          (let loop2 ((line (read-line p)))
                            (cond ((eof-object? line)
                                   #f)
                                  ((string-prefix? "msgstr " line)
                                   (read-str (tail-str line) p))
                                  (else (loop2 (read-line p))))))
                         (else
                          (loop (read-line p))))))
                (else
                 (loop (read-line p))))))
      :encoding encoding)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the internal routines for building/caching files and lookups

;; only .po files for now
(define (expand-lang-files dir cdir domain lang)
  (let ((res (list (make-gettext-file (string-append dir "/" lang "/"
                                                     cdir "/" domain ".po")
                                      lang))))
    (for-each
     (lambda (sep)
       (cond ((string-index lang sep) =>
              (lambda (i)
                (let* ((la (substring lang 0 i))
                       (file (string-append dir "/" la "/" cdir "/"
                                           domain ".po")))
                  (set! res (cons (make-gettext-file file la) res)))))))
     ;; LANG_REGION.CHARSET
     '(#\. #\_))
    (reverse res)))

;; (make-gettext-interal domain locale dirs cdir cached?)
(define (make-gettext-internal domain locale dirs cdir cached?)
  ;;(format #t "make-gettext: LANG => ~S\n" (sys-getenv "LANG"))
  (letrec
      ((files '())
       (cache #f)
       (make-file-list
        (lambda ()
          (let ((res '()))
            (for-each
             (lambda (la)
               (for-each
                (lambda (dmn)
                  (for-each
                   (lambda (d)
                     (set! res (append (expand-lang-files d cdir dmn la) res)))
                   (reverse dirs)))
                (reverse domain)))
             (reverse locale))
            (set! files (reverse res))
            ;;(format #t "gettext files: ~S\n" files)
            )))
       (get
        (lambda (msg)
          ;;(format #t "get ~S\n" msg)
          (or (and cached? (hash-table-get cache msg #f))
              (let ((res #f))
                (let loop ((l files))
                  (if (pair? l)
                    (let ((f (car l)))
                      (unless (gettext-file-properties f)
                        (gettext-file-update-properties! f))
                      (let ((m (lookup-message (gettext-file-filename f)
                                               msg (gettext-file-encoding f))))
                        (if m (set! res m) (loop (cdr l)))))))
                (if (and cached? res) (hash-table-put! cache msg res))
                res))))
       (set
        (lambda (msg val)
          ;; will trigger an error if caching was not set
          (hash-table-put! cache msg val)))
       (clear-cache
        (lambda () (set! cache (make-hash-table 'string=?)))))
    ;; initialize file-list from prefs and build dir list
    (make-file-list)
    (clear-cache)
    ;; the dispatcher
    (lambda (dispatch . args)
      (case dispatch
        ((getter) get)
        ((setter) set)
        ((get) (apply get args))
        ((set!) (apply set args))
        ((locale) locale)
        ((domain) domain)
        ((dirs) dirs)
        ((set-locale!)
         (set! locale (listify (car args))) (make-file-list) (clear-cache))
        ((set-domain!)
         (set! domain (listify (car args))) (make-file-list) (clear-cache))
        ((set-dirs!)
         (set! dirs (listify (car args))) (make-file-list) (clear-cache))
        ((use-cache)
         (set! cached? (car args)))
        ((clear)
         (clear-cache))
        ))))

(define gettext-lookup-cache (make-hash-table 'equal?))

;; cache the lookups and provide a more friendly interface.  should this
;; take keyword arguments?
(define (make-gettext . args)
  (let* ((locale (list (or (sys-getenv "LANG") (sys-getenv "LC_ALL") "C")))
         (dirs (cond ((sys-getenv "GETTEXT_PATH")
                      => (cut string-split <> ":"))
                     (else '())))
         (cdir (or (sys-getenv "LC_CATEGORY") "LC_MESSAGES"))
         (domain '("default"))
         (gettext-cached? #t)
         (lookup-cached? #t))
    ;; optional domain & locale & dirs & cached?
    (when (pair? args)
      (if (car args) (set! domain (listify (car args))))
      (set! args (cdr args))
      (when (pair? args)
        (if (car args) (set! locale (listify (car args))))
        (set! args (cdr args))
        (when (pair? args)
          (if (car args) (set! dirs (listify (car args))))
          (set! args (cdr args))
          (when (pair? args)
            (set! gettext-cached? (car args))
            (set! args (cdr args))
            (if (pair? args) (set! lookup-cached? (car args)))))))
    ;; prepend default dirs based on domain
    (set! dirs (append (hash-table-get domain-message-paths domain
                                       (message-path))
                       dirs))
    ;; optionally lookup from cache
    (if lookup-cached?
      (let* ((key (list domain locale dirs cdir gettext-cached?))
             (lookup (hash-table-get gettext-lookup-cache key #f)))
        (unless lookup
          (set! lookup (make-gettext-internal domain locale dirs
                                              cdir gettext-cached?))
          (hash-table-put! gettext-lookup-cache key lookup))
        lookup)
      (make-gettext-internal domain locale dirs cdir gettext-cached?))))

(provide "text/gettext")

