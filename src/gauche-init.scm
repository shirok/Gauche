;;;
;;; gauche-init.scm - initialize standard environment
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
;;;  $Id: gauche-init.scm,v 1.111 2003-10-22 01:38:05 shirok Exp $
;;;

(select-module gauche)

;;
;; Loading, require and provide
;;

;; Load path needs to be dealt with at the compile time.  this is a
;; hack to do so.   Don't modify *load-path* directly, since it causes
;; weird compiler-evaluator problem.
;; I don't like the current name "add-load-path", though---looks like
;; more a procedure than a compiler syntax---any ideas?
(define-macro (add-load-path path)
  `',(%add-load-path path))

;; Same as above.
(define-macro (require feature)
  `',(%require feature))

(define-macro (export-all)
  `',(%export-all))

(define-macro (export-if-defined . symbols)
  ;; CAVEAT: this form sees whether the given symbols are defined or not
  ;; _at_compile_time_.  So the definitions of symbols have to appear
  ;; before this form.   Furthermore, the semantics of this form is ambigous
  ;; when used except top-level.  It's not very nice, so you should
  ;; avoid this form unless you really need it.
  ;; NB: filter is in srfi-1, and we don't want to load it here.  Ugh.
  `(export
    ,@(let loop ((syms symbols) (r '()))
        (cond ((null? syms) (reverse! r))
              ((not (symbol? (car syms)))
               (error "non-symbol in export-if-defined form:" (car syms)))
              ((symbol-bound? (car syms))
               (loop (cdr syms) (cons (car syms) r)))
              (else (loop (cdr syms) r))))))

;; Preferred way
;;  (use x.y.z) ==> (require "x/y/z") (import x.y.z)

(define-macro (use module)
  `(begin
     (with-module gauche
       (require ,(module-name->path module)))
     (import ,module)))

(define-macro (extend . modules)
  `',(%extend (map (lambda (m)
                     (or (find-module m)
                         (begin
                           (%require (module-name->path m))
                           (find-module m))
                         (error "undefined module" m)))
                   modules)))

;; Inter-version compatibility.
(define-macro (use-version version)
  (let ((compat (string-append "gauche/compat/" version)))
    (unless (provided? compat)
      (let ((path (string-append (gauche-library-directory) "/" compat ".scm")))
        (when (file-exists? path)
          (let ((module (string->symbol (string-append "gauche-" version))))
            `(begin
               (require ,compat)
               (import ,module))))))))

;; create built-in modules, so that (use srfi-6) won't complain, for example.
(define-module srfi-6 )
(define-module srfi-8 )
(define-module srfi-10 )
(define-module srfi-17 )

;;
;; Auxiliary definitions
;;

(define-in-module scheme call/cc call-with-current-continuation)

(define-in-module scheme (call-with-values producer consumer)
  (receive vals (producer) (apply consumer vals)))

;;
;; Autoload
;;

(define-macro (autoload file . vars)
  (define (bad)
    (error "bad autoload spec" (list* 'autoload file vars)))
  (define (macrodef? v)
    (and (pair? v) (eq? (car v) :macro) (symbol? (cadr v))))
  (receive (path module)
      (cond ((string? file) (values file #f))
            ((symbol? file) (values (module-name->path file) file))
            (else (bad)))
    `(begin ,@(map (lambda (v)
                     (cond ((symbol? v)
                            `(define ,v (%make-autoload ',v ,path ',module)))
                           ((macrodef? v)
                            `(define-macro ,(cadr v)
                               ,(%make-autoload (cadr v) path module)))
                           (else (bad))))
                   vars))))

;; Some r5rs stuff are actually autoloaded
(let-syntax ((%autoload-scheme
              (syntax-rules ()
                ((%autoload-scheme file v ...)
                 (begin
                   (define-in-module scheme v (%make-autoload 'v file))
                   ...))))
             )
  (%autoload-scheme "gauche/listutil"
                    caaar caadr cadar caddr cdaar cdadr cddar cdddr
                    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
                    cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr)
  (%autoload-scheme "gauche/with"
                    call-with-input-file call-with-output-file
                    with-input-from-file with-output-to-file)
  (%autoload-scheme "gauche/numerical"
                    exp log sqrt expt cos sin tan asin acos atan
                    gcd lcm numerator denominator
                    real-part imag-part)
  )

(autoload "gauche/with"
          with-output-to-string call-with-output-string
          with-input-from-string call-with-input-string
          with-string-io call-with-string-io
          write-to-string read-from-string)

(autoload "gauche/signal"
          (:macro with-signal-handlers))

(autoload gauche.portutil
          port->string port->list port->string-list port->sexp-list
          copy-port port-fold port-fold-right port-for-each port-map 
          port-position-prefix port-tell)

(autoload "gauche/numerical"
          sinh cosh tanh asinh acosh atanh)

(autoload "gauche/logical"
          logtest logbit? copy-bit bit-field copy-bit-field logcount
          integer-length)

(autoload "gauche/common-macros"
          (:macro syntax-error) (:macro syntax-errorf) unwrap-syntax
          (:macro push!) (:macro pop!) (:macro inc!) (:macro dec!)
          (:macro update!)
          (:macro check-arg) (:macro get-keyword*)
          (:macro let1) (:macro begin0) (:macro fluid-let)
          (:macro dotimes) (:macro dolist) (:macro while) (:macro until))

(autoload gauche.regexp
          (:macro rxmatch-let) (:macro rxmatch-if)
          (:macro rxmatch-cond) (:macro rxmatch-case)
          regexp-replace regexp-replace-all regexp-quote)

(autoload gauche.procedure
          compose pa$ map$ for-each$ apply$ any-pred every-pred
          (:macro let-optionals*) (:macro let-keywords*)
          (:macro get-optional)
          arity procedure-arity-includes?
          <arity-at-least> arity-at-least? arity-at-least-value)

(autoload gauche.vm.debugger
          enable-debug disable-debug (:macro debug-print))

(autoload srfi-0 (:macro cond-expand))
(autoload srfi-26 (:macro cut) (:macro cute))
(autoload srfi-31 (:macro rec))

(autoload gauche.interpolate string-interpolate)

(define-reader-ctor 'string-interpolate
  (lambda (s) (string-interpolate s))) ;;lambda is required to delay loading

(autoload gauche.auxsys
          fmod frexp modf ldexp
          sys-abort sys-mkfifo
          sys-setgid sys-setpgid sys-getpgid sys-getpgrp
          sys-setsid sys-setuid sys-times sys-uname sys-ctermid
          sys-gethostname sys-getdomainname sys-putenv
          sys-gettimeofday sys-chown sys-utime
          sys-getgroups sys-getlogin sys-localeconv)

(autoload gauche.defvalues
          (:macro define-values) (:macro set!-values))

(autoload gauche.stringutil string-split)

(autoload gauche.hashutil hash-table hash-table-fold
                          hash-table-for-each hash-table-map)

(autoload gauche.libutil  library-fold library-map library-for-each
                          library-exists? library-has-module?)

(autoload gauche.sortutil sort sort! merge merge! sorted?)

;; these are so useful that I couldn't resist to add...
(define (file-exists? path)
  (sys-access path |F_OK|))
(define (file-is-regular? path)
  (and (sys-access path |F_OK|)
       (eq? (slot-ref (sys-stat path) 'type) 'regular)))
(define (file-is-directory? path)
  (and (sys-access path |F_OK|)
       (eq? (slot-ref (sys-stat path) 'type) 'directory)))

;; srfi-17
(define (getter-with-setter get set)
  (let ((proc (lambda x (apply get x))))
    (set! (setter proc) set)
    proc))

;; print (from SCM, Chicken)
(define (print . args)
  (for-each display args) (newline))

;; system object accessors (for backward compatibility)
(define (sys-stat->file-type s)  (slot-ref s 'type))
(define (sys-stat->mode s)  (slot-ref s 'mode))
(define (sys-stat->ino s)   (slot-ref s 'ino))
(define (sys-stat->dev s)   (slot-ref s 'dev))
(define (sys-stat->rdev s)  (slot-ref s 'rdev))
(define (sys-stat->nlink s) (slot-ref s 'nlink))
(define (sys-stat->size s)  (slot-ref s 'size))
(define (sys-stat->uid s)   (slot-ref s 'uid))
(define (sys-stat->gid s)   (slot-ref s 'gid))
(define (sys-stat->atime s) (slot-ref s 'atime))
(define (sys-stat->mtime s) (slot-ref s 'mtime))
(define (sys-stat->ctime s) (slot-ref s 'ctime))
(define (sys-stat->type s)  (slot-ref s 'type))

(define (sys-tm->alist tm)
  (map (lambda (n s) (cons n (slot-ref tm s)))
       '(tm_sec tm_min tm_hour tm_mday tm_mon tm_year tm_wday tm_yday tm_isdst)
       '(sec min hour mday mon year wday yday isdst)))

;;
;; Load object system
;;

(require "gauche/object")

;;
;; For convenience
;;

(let ((dotfile (sys-normalize-pathname "~/.gaucherc" :expand #t)))
  (when (sys-access dotfile |F_OK|)
    (load dotfile :environment (find-module 'user))))
