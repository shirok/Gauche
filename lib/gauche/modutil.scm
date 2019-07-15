;;;
;;; module related utility functions.  to be autoloaded.
;;;
;;;   Copyright (c) 2000-2019  Shiro Kawai  <shiro@acm.org>
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

(define-module gauche.modutil
  (export export-if-defined use-version describe-symbol-bindings)
  )
(select-module gauche.modutil)

(define-macro (export-if-defined . symbols)
  ;; CAVEAT: this form sees whether the given symbols are defined or not
  ;; _at_compile_time_.  So the definitions of symbols have to appear
  ;; before this form.   Furthermore, the semantics of this form is ambigous
  ;; when used except top-level.  It's not very nice, so you should
  ;; avoid this form unless you really need it.
  ;; NB: filter is in srfi-1, and we don't want to load it here.  Ugh.
  `(export
    ,@(let loop ([syms symbols] [r '()])
        (cond [(null? syms) (reverse! r)]
              [(not (symbol? (car syms)))
               (error "non-symbol in export-if-defined form:" (car syms))]
              [(global-variable-bound? #f (car syms))
               (loop (cdr syms) (cons (car syms) r))]
              [else (loop (cdr syms) r)]))))

;; Inter-version compatibility.
(define-macro (use-version version)
  (let1 compat (string-append "gauche/compat/" version)
    (unless (provided? compat)
      (let1 path (string-append (gauche-library-directory) "/" compat ".scm")
        (when (file-exists? path)
          (let1 module (string->symbol (string-append "gauche-" version))
            `(begin
               (require ,compat)
               (import ,module))))))))

;; Called when you describe a symbol in REPL.  Look for the symbol
;; in all named modules.
(define (describe-symbol-bindings sym)
  (define const?     (with-module gauche.internal gloc-const?))
  (define inlinable? (with-module gauche.internal gloc-inlinable?))
  (define find-b     (with-module gauche.internal find-binding))
  (define (describe-binding mod gloc val)
    (let1 attrs (cond-list [(const? gloc) 'const]
                           [(inlinable? gloc) 'inlinable])
      (format #t "  In module `~s'" (module-name mod))
      (unless (null? attrs) (format #t " ~s" attrs))
      (format #t ":\n    ~a\n"
              (guard [e (else "#<unprintable>")]
                (format "~,,,,50:a" val)))))
  (let1 bindings (filter-map (^m (and-let* ([g (find-b m sym #t)]
                                            [ (global-variable-bound? m sym) ])
                                   (list m g (global-variable-ref m sym))))
                             (all-modules))
    (if (null? bindings)
      (format #t "No known bindings for variable ~a.\n" sym)
      (begin (format #t "Known binding~a for variable ~a:\n"
                     (if (null? (cdr bindings)) "" "s") sym)
             (for-each (cut apply describe-binding <>) bindings))))
  (values))
