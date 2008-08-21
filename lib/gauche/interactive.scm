;;;
;;; interactive.scm - useful stuff in the interactive session
;;;  
;;;   Copyright (c) 2000-2008  Shiro Kawai  <shiro@acm.org>
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
;;;  $Id: interactive.scm,v 1.16 2008-05-10 13:35:55 shirok Exp $
;;;

#!no-fold-case

(define-module gauche.interactive
  (export apropos describe d
          ;; autoloaded symbols follow
          info reload reload-modified-modules module-reload-rules
          reload-verbose)
  )
(select-module gauche.interactive)

;; Apropos - search bound symbols matching given pattern
;;
;;  (apropos 'open)             print bound symbols that contains "open"
;;                              in its name
;;  (apropos #/^(open|close)/)  you can use regexp
;;
;;  (apropos 'open 'scheme)     search symbols only in a single module
;;
;; Apropos is implemented as macro, for it requires to get the current
;; module which is only available at the compile time.

(define-syntax apropos
  (syntax-rules ()
    ((_ item) (%apropos item (current-module) #f))
    ((_ item module) (%apropos item module #t))
    ))

(define (%apropos item module stay-in-module)
  (let ((module (cond ((module? module) module)
                      ((symbol? module)
                       (or (find-module module)
                           (error "No such module: " module)))
                      (else (error "Bad object for module: " module))))
        (matcher (cond ((symbol? item)
                        (let ((substr (symbol->string item)))
                          (lambda (name) (string-scan name substr))))
                       ((string? item)
                        ;; Note: future extention
                        (error "Bad object for item: " item))
                       ((is-a? item <regexp>)
                        (lambda (name) (rxmatch item name)))
                       (else
                        (error "Bad object for item: " item))))
        (result '())
        (searched '()))

    (define (search mod)
      (unless (memq mod searched)
        (set! searched (cons mod searched))
        (hash-table-for-each
         (module-table mod)
         (lambda (symbol value)
           (when (matcher (symbol->string symbol))
             (found mod symbol))))))

    (define (found module symbol)
      (set! result
            (cons (format #f "~30s (~a)~%" symbol (module-name module))
                  result)))

    ;; mimics the Scm_FindBinding
    (if stay-in-module
        (search module)
        (begin
          (for-each (lambda (m)
                      (for-each search (module-precedence-list m)))
                    (module-imports module))
          (for-each search (module-precedence-list module))))
    (for-each display (sort result))
    (values)
    ))

;; Describe - describe object
;;

(define-method describe (object)
  (let* ((class (class-of object))
         (slots (class-slots class)))
    (format #t "~s is an instance of class ~a\n"
            object (class-name class))
    (unless (null? slots)
      (format #t "slots:\n")
      (for-each (lambda (s)
                  (format #t "  ~10s: ~a\n" s
                          (if (slot-bound? object s)
                              (with-output-to-string
                                (lambda () (write-limited (slot-ref object s)
                                                          60)))
                              "#<unbound>")))
                (map slot-definition-name slots)))
    (values)
    ))

;; For convenience
;; This may interfere with other code.
;(define-syntax a
;  (syntax-rules ()
;    ((_ . ?args) (apropos . ?args))))

(define d describe)

;; Turn on debugger
;(enable-debug)

;; Autoload online info viewer
(autoload gauche.interactive.info info)

;; Autoload module reloader
(autoload gauche.reload reload reload-modified-modules
                        module-reload-rules reload-verbose)

;; For convenience
(let ((dotfile (sys-normalize-pathname "~/.gaucherc" :expand #t)))
  (when (sys-access dotfile F_OK)
    (load dotfile :environment (find-module 'user))))

(provide "gauche/interactive")
