;;;
;;; Debugger - terminal base debugger
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
;;;  $Id: debugger.scm,v 1.1 2001-09-28 10:00:44 shirok Exp $
;;;

(define-module gauche.vm.debugger
  (use srfi-1)
  (use srfi-13)
  (use gauche.vm.disasm)
  (export enable-debug disable-debug)
  )
(select-module gauche.vm.debugger)

(define *stack-show-depth* 20)
(define *expr-show-length* 65)

(define (write-limit expr port)
  (let ((s (with-output-to-string (lambda () (write expr)))))
    (if (> (string-length s) *expr-show-length*)
        (begin (display (string-take s *expr-show-length*) port)
               (display " ...\n" port))
        (begin (display s port) (newline port)))))

;; Called when uncaptured error occurs
(define (debug exn)
  ;; TODO: we need to gain terminal I/O.
  (disable-debug)
  (let ((inp   (current-input-port))
        (outp  (current-error-port))
        (stack (vm-get-stack-trace)))
    (if (is-a? exn <exception>)
        (format outp "*** Error: ~a\n" (slot-ref exn 'message))
        (format outp "*** Error: ~a\n" exn))
    (format outp "Stack trace\n")
    (format outp "________________________________________________\n")
    (do ((s stack (cdr s))
         (i 0     (+ i 1)))
        ((or (null? s) (> i *stack-show-depth*))
         (unless (null? s)
           (format outp "...\n")))
      (format outp "~3d   " i)
      (write-limit (caar s) outp))
    (format outp "Entering debugger\n")
    )
  (enable-debug))

(define (enable-debug)
  (vm-set-default-exception-handler (current-vm) debug))

(define (disable-debug)
  (vm-set-default-exception-handler (current-vm) #f))

(provide "gauche/vm/debugger")

