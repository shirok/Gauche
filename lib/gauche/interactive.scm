;;;
;;; interactive.scm - useful stuff in the interactive session
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
;;;  $Id: interactive.scm,v 1.2 2001-09-06 07:05:43 shirok Exp $
;;;

(define-module gauche.interactive
  (export apropos a describe d)
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
                          (lambda (name) (string-contains name substr))))
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
          (for-each search (module-imports module))
          (let loop ((mod module))
            (when mod
              (search mod)
              (loop (module-parent mod))))))
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
                              "#unbound")))
                (map slot-definition-name slots)))
    (values)
    ))

;; For convenience
(define-syntax a
  (syntax-rules ()
    ((_ . ?args) (apropos . ?args))))

(define d describe)

(provide "gauche/interactive")
