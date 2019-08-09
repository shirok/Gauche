;;; "slib.scm" configuration template of slib:features for Scheme -*-scheme-*-
;;; Author: Shiro Kawai
;;; based on the "Template.scm" by Aubrey Jaffer
;;;
;;; This code is in the public domain.

;; SLIB module exports all symbols for compatibility.
(define-module slib
  (use srfi-0)
  (use srfi-13)
  (use srfi-111)        ;; For make-exchanger
  (extend util.record)  ;; SLIB-compatible make-record-type
  (use file.util)
  (use gauche.uvector)  ;; Used to implement 'byte' API
  (use gauche.threads)  ;; For make-exchanger
  (export-all))
(select-module slib)

;;@ (software-type) should be set to the generic operating system type.
;;; unix, vms, macos, amiga and ms-dos are supported.

(define (software-type)
  (cond-expand
   [gauche.windows 'ms-dos]
   [else 'unix]))

;;@ (scheme-implementation-type) should return the name of the scheme
;;; implementation loading this file.
(define (scheme-implementation-type) 'gauche)

;;@ (scheme-implementation-home-page) should return a (string) URI
;;; (Uniform Resource Identifier) for this scheme implementation's home
;;; page; or false if there isn't one.
(define (scheme-implementation-home-page)
  "http://practical-scheme.net/gauche/")

;;@ (scheme-implementation-version) should return a string describing
;;; the version the scheme implementation loading this file.
(define (scheme-implementation-version) (gauche-version))

;;@ (implementation-vicinity) should be defined to be the pathname of
;;; the directory where any auxillary files to your Scheme
;;; implementation reside.
(define (implementation-vicinity)
  (string-append (gauche-library-directory) "/"))

;;@ (library-vicinity) should be defined to be the pathname of the
;;; directory where files of Scheme library functions reside.
(define library-vicinity
  (let ((library-path
         (or
          ;; Use this getenv if your implementation supports it.
          (and-let1 p (sys-getenv "SCHEME_LIBRARY_PATH")
            (string-append p "/"))
          ;; Use this path if your scheme does not support GETENV
          ;; or if SCHEME_LIBRARY_PATH is not set.
          (case (software-type)
            ((unix) (regexp-replace "[^\/]$"
                                    (with-module gauche.internal SLIB_DIR)
                                    (^m #"~(m 0)/")))
            ((vms) "lib$scheme:")
            ((ms-dos) "C:\\SLIB\\")
            (else "")))))
    (lambda () library-path)))

;;@ (home-vicinity) should return the vicinity of the user's HOME
;;; directory, the directory which typically contains files which
;;; customize a computer environment for a user.
(define (home-vicinity)
  (let ((home (or (sys-getenv "HOME")
                  (home-directory))))
    (and home
         (case (software-type)
           ((unix coherent ms-dos)      ;V7 unix has a / on HOME
            (if (eqv? #\/ (string-ref home (+ -1 (string-length home))))
              home
              (string-append home "/")))
           (else home)))))

;@
(define in-vicinity string-append)
;@
(define (user-vicinity)
  (case (software-type)
    ((vms)     "[.]")
    (else       "")))

(define *load-pathname* #f)
;@
(define vicinity:suffix?
  (let ((suffi
        (case (software-type)
          ((amiga)                             '(#\: #\/))
          ((macos thinkc)                      '(#\:))
          ((ms-dos windows atarist os/2)       '(#\\ #\/))
          ((nosve)                             '(#\: #\.))
          ((unix coherent plan9)               '(#\/))
          ((vms)                               '(#\: #\]))
          (else
           (slib:warn "require.scm" 'unknown 'software-type (software-type))
           "/"))))
    (lambda (chr) (and (memv chr suffi) #t))))
;@
(define (pathname->vicinity pathname)
  (let loop ((i (- (string-length pathname) 1)))
    (cond ((negative? i) "")
         ((vicinity:suffix? (string-ref pathname i))
          (substring pathname 0 (+ i 1)))
         (else (loop (- i 1))))))
(define (program-vicinity)
  (if *load-pathname*
      (pathname->vicinity *load-pathname*)
      (slib:error 'program-vicinity " called; use slib:load to load")))
;@
(define sub-vicinity
  (case (software-type)
    ((vms) (lambda
               (vic name)
             (let ((l (string-length vic)))
               (if (or (zero? (string-length vic))
                       (not (char=? #\] (string-ref vic (- l 1)))))
                 (string-append vic "[" name "]")
                 (string-append (substring vic 0 (- l 1))
                                "." name "]")))))
    (else (let ((*vicinity-suffix*
                 (case (software-type)
                   ((nosve) ".")
                   ((macos thinkc) ":")
                   ((ms-dos windows atarist os/2) "\\")
                   ((unix coherent plan9 amiga) "/"))))
            (lambda (vic name)
              (string-append vic name *vicinity-suffix*))))))
;@
(define (make-vicinity <pathname>) <pathname>)
;@
(define with-load-pathname
  (let ((exchange
         (lambda (new)
           (let ((old *load-pathname*))
             (set! *load-pathname* new)
             old))))
    (lambda (path thunk)
      (let ((old #f))
        (dynamic-wind
            (lambda () (set! old (exchange path)))
            thunk
            (lambda () (exchange old)))))))

;;@ SLIB:FEATURES is a list of symbols naming the (SLIB) features
;;; initially supported by this implementation.
(define slib:features
  '(
    source                          ;can load scheme source files
                                        ;(SLIB:LOAD-SOURCE "filename")
;;;    compiled                        ;can load compiled files
                                        ;(SLIB:LOAD-COMPILED "filename")
    vicinity
    srfi-59
    srfi-96

    ;; Scheme report features
    ;; R5RS-compliant implementations should provide all 9 features.

    r5rs                            ;conforms to
    eval                            ;R5RS two-argument eval
    values                          ;R5RS multiple values
    dynamic-wind                    ;R5RS dynamic-wind
    macro                           ;R5RS high level macros
    delay                           ;has DELAY and FORCE
    multiarg-apply                  ;APPLY can take more than 2 args.
    char-ready?
    rev4-optional-procedures        ;LIST-TAIL, STRING-COPY,
                                        ;STRING-FILL!, and VECTOR-FILL!

    ;; These four features are optional in both R4RS and R5RS

    multiarg/and-                   ;/ and - can take more than 2 args.
    rationalize
;;;    transcript                      ;TRANSCRIPT-ON and TRANSCRIPT-OFF
    with-file                       ;has WITH-INPUT-FROM-FILE and
                                        ;WITH-OUTPUT-TO-FILE

    r4rs                            ;conforms to

    ieee-p1178                      ;conforms to

;;;    r3rs                            ;conforms to

;;;    rev2-procedures                 ;SUBSTRING-MOVE-LEFT!,
                                        ;SUBSTRING-MOVE-RIGHT!,
                                        ;SUBSTRING-FILL!,
                                        ;STRING-NULL?, APPEND!, 1+,
                                        ;-1+, <?, <=?, =?, >?, >=?
;;;    object-hash                     ;has OBJECT-HASH
    ;; NB: Gauche's object-hash is different from SLIB wants, which
    ;; is Gauche's eqv-hash.  Providing it in the name of object-hash
    ;; would be confusing, so we don't provide this.

    full-continuation               ;can return multiple times
    ieee-floating-point             ;conforms to IEEE Standard 754-1985
                                        ;IEEE Standard for Binary
                                        ;Floating-Point Arithmetic.

    ;; Other common features

    ;; NB: we turned off srfi here, since if this is on, slib tries
    ;; to import all available srfis, including srfi-29, which defines
    ;; incompatible 'format'.
    srfi-0                           ;srfi-0, COND-EXPAND finds all srfi-*
;;;    sicp                            ;runs code from Structure and
                                        ;Interpretation of Computer
                                        ;Programs by Abelson and Sussman.
    defmacro                        ;has Common Lisp DEFMACRO
;;;	syntax-case			;has syncase:eval and syncase:load
    record                          ;has user defined data structures
    string-port                     ;has CALL-WITH-INPUT-STRING and
                                        ;CALL-WITH-OUTPUT-STRING
    sort
;;;    pretty-print
    object->string
;;;    format                          ;Common-lisp output formatting
;;;    trace                           ;has macros: TRACE and UNTRACE
;;;    compiler                        ;has (COMPILER)
;;;    ed                              ;(ED) is editor
    system                          ;posix (system <string>)
    getenv                          ;posix (getenv <string>)
    program-arguments               ;returns list of strings (argv)
    current-time                    ;returns time in seconds since 1/1/1970

    ;; Implementation Specific features
    byte                            ;byte string manipulation
    ))

;;@ (FILE-POSITION <port> . <k>)
(define (file-position . args) #f)

;;@ (OUTPUT-PORT-WIDTH <port>)
(define (output-port-width . arg) 79)

;;@ (OUTPUT-PORT-HEIGHT <port>)
(define (output-port-height . arg) 24)

;;@ (CURRENT-ERROR-PORT) - Gauche has it
;(define current-error-port
;  (let ((port (current-output-port)))
;    (lambda () port)))

;;@ (TMPNAM) makes a temporary file name.
(define tmpnam sys-tmpnam)

;;@ SYSTEM
(define system sys-system)

;;@ GETENV
(define getenv sys-getenv)

;;@ (FILE-EXISTS? <string>)
; Gauche has this

;;@ (DELETE-FILE <string>)
(define delete-file (with-module file.util delete-file))

;;@ FORCE-OUTPUT flushes any pending output on optional arg output port
;;; use this definition if your system doesn't have such a procedure.
(define force-output flush)

;;@ CURRENT-TIME
(define current-time sys-time)

;;@ PROGRAM-ARGUMENTS
(define (program-arguments) (with-module user *argv*))

;;; CALL-WITH-INPUT-STRING and CALL-WITH-OUTPUT-STRING are the string
;;; port versions of CALL-WITH-*PUT-FILE.

;;@ "rationalize" adjunct procedures.
(define (find-ratio x e)
  (let ((rat (rationalize x e)))
    (list (numerator rat) (denominator rat))))
(define (find-ratio-between x y)
  (find-ratio (/ (+ x y) 2) (/ (- x y) 2)))

;;@ CHAR-CODE-LIMIT is one greater than the largest integer which can
;;; be returned by CHAR->INTEGER.
(define char-code-limit (+ *char-code-max* 1))

;;@ MOST-POSITIVE-FIXNUM is used in modular.scm
(define most-positive-fixnum (greatest-fixnum))

;;@ Return argument
;(define (identity x) x) ; Gauche has this.

;;@ SLIB:EVAL is single argument eval using the top-level (user) environment.
(define (slib:eval expr) (eval expr (interaction-environment)))

;; If your implementation provides R4RS macros:
(define macro:eval slib:eval)
;;@ %SLIB-LOAD loads file in slib module.
(define (%slib-load file)
  (load file :environment (current-module)))
(define macro:load %slib-load)

(define-syntax defmacro
  (syntax-rules ()
    ((_ name params . body) (define-macro (name . params) . body))))

;; Gauche has these
; macroexpand-1
; macroexpand
;@
(define (gentemp) (gensym "slib:G"))

(define base:eval slib:eval)
;@
(define (defmacro:eval x) (eval x (current-module)))
(define (macro:expand x) (macroexpand x))
;(define (defmacro:expand* x)
;  (require 'defmacroexpand) (apply defmacro:expand* x '()))
;@
(define defmacro:load %slib-load)
;; slib:eval-load definition moved to "require.scm"
;@
(define slib:warn
  (lambda args
    (let ((cep (current-error-port)))
      ;;(if (provided? 'trace) (print-call-stack cep))
      (display "Warn: " cep)
      (for-each (lambda (x) (display #\space cep) (write x cep)) args)
      (newline cep))))

;;@ define an error procedure for the library
(define slib:error error)
;@
(define (make-exchanger obj)
  (let1 a (atom (box obj))
    (lambda (new) (atomic a (^[b] (begin0 (unbox b) (set-box! b new)))))))
(define (open-file filename modes)
  (case modes
    ((r rb) (open-input-file filename))
    ((w wb) (open-output-file filename))
    (else (slib:error 'open-file 'mode? modes))))
(define (port? obj) (or (input-port? obj) (output-port? obj)))
(define (call-with-open-ports . ports)
  (define proc (car ports))
  (cond ((procedure? proc) (set! ports (cdr ports)))
        (else (set! ports (reverse ports))
              (set! proc (car ports))
              (set! ports (reverse (cdr ports)))))
  (let ((ans (apply proc ports)))
    (for-each close-port ports)
    ans))
(define (close-port port)
  (cond ((input-port? port)
         (close-input-port port)
         (if (output-port? port) (close-output-port port)))
        ((output-port? port) (close-output-port port))
        (else (slib:error 'close-port 'port? port))))
;@
(define (browse-url url)
  (define (try cmd end) (zero? (system (string-append cmd url end))))
  (or (try "xdg-open '" "' &")
      (try "firefox '"  "' &")))

;@
(define object->string write-to-string)

;;@ define these as appropriate for your system.
(define slib:tab (integer->char 9))
(define slib:form-feed (integer->char 12))

;;@ Support for older versions of Scheme.  Not enough code for its own file.
;(define (last-pair l) (if (pair? (cdr l)) (last-pair (cdr l)) l)) ; Gauche has this.
(define t #t)
(define nil #f)

;;; byte string operators.  SLIB's byte module uses slib array.
;;; It's more efficient to use u8vector in Gauche.

(define (byte-ref s k) (u8vector-ref s k))
(define (byte-set! s k b) (u8vector-set! s k b))
(define make-bytes make-u8vector)
(define (bytes-length s) (u8vector-length s))
(define bytes u8vector)
(define bytes->list u8vector->list)
(define list->bytes list->u8vector)
(define bytes->string u8vector->string)
(define string->bytes string->u8vector)
(define bytes-copy u8vector-copy)
;; write-byte - Gauche has this
;; read-byte  - Gauche has this

;;@ Define these if your implementation's syntax can support it and if
;;; they are not already defined.
(define (1+ n) (+ n 1))
(define (-1+ n) (+ n -1))
(define 1- -1+)

;;@ Define SLIB:EXIT to be the implementation procedure to exit or
;;; return if exiting not supported.
(define slib:exit exit)

;;@ Here for backward compatability
(define scheme-file-suffix
  (let ((suffix (case (software-type)
                 ((nosve) "_scm")
                  (else ".scm"))))
    (lambda () suffix)))

;;@ (SLIB:LOAD-SOURCE "foo") should load "foo.scm" or with whatever
;;; suffix all the module files in SLIB have.  See feature 'SOURCE.
(define (slib:load-source f) (load (string-append f ".scm")))

;;@ (SLIB:LOAD-COMPILED "foo") should load the file that was produced
;;; by compiling "foo.scm" if this implementation can compile files.
;;; See feature 'COMPILED.
(define slib:load-compiled load)

;;@ At this point SLIB:LOAD must be able to load SLIB files.
(define slib:load load)

;; We have to provide slib-related srfis now, since slib's require.scm
;; tries to load all available srfis.
(provide "srfi-59")
(define-module srfi-59)
(provide "srfi-96")
(define-module srfi-96)

;; [SK] Emit comprehensive message in case we can't find SLIB
;(slib:load (in-vicinity (library-vicinity) "require"))
(unless (load (in-vicinity (library-vicinity) "require")
              :error-if-not-found #f)
  (error #"Couldn't load SLIB's require.scm in `~(library-vicinity)'.  \
           Either SLIB is not installed, or it is installed in a different \
           location.  Try setting the environment variable \
           SCHEME_LIBRARY_PATH to point to the SLIB directory; or you can \
           reconfigure Gauche with --with-slib option and reinstall." ))

;;; A trick to make require work both on Gauche files and slib files.
;;; The hint is taken from STk.

(define-macro (require feature)         ;redefine
  (if (string? feature)
    `',(%require feature)            ;gauche version
    `(slib:require ,feature)))    ;slib version

(define (provide feature)
  (if (string? feature)
    (with-module gauche (provide feature)) ;gauche version
    (slib:provide feature)))       ;slib version

(define (provided? feature)
  (if (string? feature)
    (with-module gauche (provided? feature)) ;gauche version
    (slib:provided? feature)))     ;slib version
