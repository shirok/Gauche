;;;
;;; gauche.interactive - useful stuff in the interactive session
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

#!no-fold-case

(define-module gauche.interactive
  (export apropos describe d read-eval-print-loop
          ;; autoloaded symbols follow
          info reload reload-modified-modules module-reload-rules
          reload-verbose)
  )
(select-module gauche.interactive)

;;;
;;; Apropos - search bound symbols matching given pattern
;;;
;;;  (apropos 'open)             print bound symbols that contains "open"
;;;                              in its name
;;;  (apropos #/^(open|close)/)  you can use regexp
;;;
;;;  (apropos 'open 'scheme)     search symbols only in a single module
;;;
;;; Apropos is implemented as macro, for it requires to get the current
;;; module which is only available at the compile time.

(define-syntax apropos
  (syntax-rules ()
    [(_ item) (%apropos item (current-module) #f)]
    [(_ item module) (%apropos item module #t)]
    ))

(define (%apropos item module stay-in-module)
  (let ([module (cond [(module? module) module]
                      [(symbol? module)
                       (or (find-module module)
                           (error "No such module: " module))]
                      [else (error "Bad object for module: " module)])]
        [matcher (cond [(symbol? item)
                        (let1 substr (symbol->string item)
                          (^[name] (string-scan name substr)))]
                       [(string? item)
                        ;; Note: future extention
                        (error "Bad object for item: " item)]
                       [(is-a? item <regexp>) (^[name] (rxmatch item name))]
                       [else
                        (error "Bad object for item: " item)])]
        [result '()]
        [searched '()])

    (define (search mod)
      (unless (memq mod searched)
        (set! searched (cons mod searched))
        (hash-table-for-each
         (module-table mod)
         (^[symbol value]
           (when (matcher (symbol->string symbol))
             (found mod symbol))))))

    (define (found module symbol)
      (set! result
            (cons (format #f "~30s (~a)~%" symbol (module-name module))
                  result)))

    ;; mimics the Scm_FindBinding
    (if stay-in-module
      (search module)
      (begin (for-each (^m (for-each search (module-precedence-list m)))
                       (module-imports module))
             (for-each search (module-precedence-list module))))
    (for-each display (sort result))
    (values)
    ))

;;;
;;; Describe - describe object
;;;

(define-method describe (object)
  (let* ([class (class-of object)]
         [slots (class-slots class)])
    (format #t "~s is an instance of class ~a\n" object (class-name class))
    (unless (null? slots)
      (format #t "slots:\n")
      (dolist [s (map slot-definition-name slots)]
        (format #t "  ~10s: ~a\n" s
                (if (slot-bound? object s)
                  (with-output-to-string
                    (^[] (write-limited (slot-ref object s) 60)))
                  "#<unbound>"))))
    (values)))

(define-method describe ((s <symbol>))
  (format #t "~s is an instance of class ~a\n" s (class-name (class-of s)))
  (describe-symbol-bindings s)) ;; autoloaded from gauche.modutil

(define d describe)

;;;
;;; Enhanced REPL
;;;

;; Evaluation history.
;; Kludge: We want the history variables to be visible not only in
;; #<module user> but in most other modules, so that the user can switch
;; modules in REPL without losing access to the history.  So we "inject"
;; those variables into #<module gauche>.  It is not generally recommended
;; way, though.
;; We also export those history variables, so the modules that does not
;; inherit gauche can still use them by (import gauche :only (*1 ...)).
(define-in-module gauche *1 #f)
(define-in-module gauche *1+ '())
(define-in-module gauche *2 #f)
(define-in-module gauche *2+ '())
(define-in-module gauche *3 #f)
(define-in-module gauche *3+ '())
(define-in-module gauche *e #f)
(define-in-module gauche (*history)
  (display "*1: ") (repl-print *1) (newline)
  (display "*2: ") (repl-print *2) (newline)
  (display "*3: ") (repl-print *3) (newline)
  (values))
(with-module gauche
  (export *1 *1+ *2 *2+ *3 *3+ *e *history))

(define (%set-history-expr! r)
  (unless (null? r)
    (set! *3 *2) (set! *3+ *2+)
    (set! *2 *1) (set! *2+ *1+)
    (set! *1 (car r)) (set! *1+ r)))

(define (%set-history-exception! e) (set! *e e))

;; Will be extended for fancier printer
(define (repl-print x) (write/ss x) (flush))

(define *repl-name* "gosh")

(define %prompter
  (let1 user-module (find-module 'user)
    (^[] (let1 m ((with-module gauche.internal vm-current-module))
           (if (eq? m user-module)
             (format #t "~a> " *repl-name*)
             (format #t "~a[~a]> " *repl-name* (module-name m)))
           (flush)))))

;; error printing will be handled by the original read-eval-print-loop
(define (%evaluator expr env)
  (guard (e [else (%set-history-exception! e) (raise e)])
    (receive r (eval expr env)
      (%set-history-expr! r)
      (apply values r))))

;; This shadows gauche#read-eval-print-loop
(define (read-eval-print-loop :optional (reader #f)
                                        (evaluator #f)
                                        (printer #f)
                                        (prompter #f))
  (let ([evaluator (or evaluator %evaluator)]
        [prompter (or prompter %prompter)])
    ((with-module gauche read-eval-print-loop)
     reader evaluator printer prompter)))

;;;
;;; Misc. setup
;;;

;; Autoload online info viewer
(autoload gauche.interactive.info info)

;; Autoload module reloader
(autoload gauche.reload reload reload-modified-modules
                        module-reload-rules reload-verbose)

;; See (describe <symbol>) above
(autoload gauche.modutil describe-symbol-bindings)

;; Kludge - if gosh is invoked with R7RS mode, import r7rs-small libraries
;; into user module.  There should be better way to detect whether we started
;; with r7rs mode.
(when (memq (find-module 'r7rs)
            (and-let* ([u (find-module 'user)])
              (module-precedence-list u)))
  (set! *repl-name* "gosh-r7rs") ; for the convenience
  (eval '(import (scheme base) (scheme case-lambda) (scheme char)
                 (scheme complex) (scheme cxr) (scheme eval)
                 (scheme file) (scheme inexact) (scheme lazy)
                 (scheme load) (scheme process-context) (scheme read)
                 (scheme repl) (scheme time) (scheme write)
                 (only (gauche) *1 *1+ *2 *2+ *3 *3+ *e *history))
        (find-module 'user)))

;; For convenience
(let ((dotfile (sys-normalize-pathname "~/.gaucherc" :expand #t)))
  (when (sys-access dotfile F_OK)
    (load dotfile :environment (find-module 'user))))

