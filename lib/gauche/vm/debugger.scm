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
;;;  $Id: debugger.scm,v 1.3 2001-10-04 11:20:03 shirok Exp $
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
  (let ((inp   (standard-input-port))
        (outp  (standard-error-port))
        (stack (cdddr (vm-get-stack-trace))) ;remove our stack frames
        )

    (define (current-stack level)
      (let loop ((n 0) (stack stack))
        (cond ((null? stack) #f)
              ((= n level) (car stack))
              (else (loop (+ n 1) (cdr stack))))))

    (define (show-stack s level)
      (let* ((env (cdr s))
             (vals (vector-ref env 0)))
        (format outp "~3d: " level)
        (write-limit (car s) outp)
        (if (not (= (length vals) (- (vector-length env) 1)))
            (format outp "[Unrecognized env; compiler error?]\n")
            (do ((i 1 (+ i 1))
                 (vals vals (cdr vals)))
                ((null? vals))
              (format outp " ~10@s = " (car vals))
              (write-limit (vector-ref env i) outp)))))
    
    (define (loop level)
      (format outp "debug$ ")
      (flush outp)
      (let ((cmd (read inp)))
        (cond ((eqv? cmd :show) (show level))
              ((eqv? cmd :up)   (up   level))
              ((eqv? cmd :down) (down level))
              ((eqv? cmd :quit))
              ((eof-object? cmd) (newline outp))
              (else (help level)))))

    (define (show level)
      (show-stack (current-stack level) level)
      (loop level))

    (define (up level)
      (cond ((current-stack (+ level 1))
             => (lambda (s) (show-stack s (+ level 1)) (loop (+ level 1))))
            (else
             (format outp "No more stack.\n")
             (loop level))))

    (define (down level)
      (if (= level 0)
          (begin (format outp "You're already at stack bottom.\n")
                 (loop level))
          (begin (show-stack (current-stack (- level 1)) (- level 1))
                 (loop (- level 1)))))

    (define (help level)
      (format outp "Commands:\n")
      (format outp "  :show       show current frame\n")
      (format outp "  :up         up one frame\n")
      (format outp "  :down       down one frame\n")
      (format outp "  :quit       return to toplevel\n")
      (loop level))
    
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
    (format outp "Entering debugger.  Type :help for help.\n")
    (loop 0)
    )
  (enable-debug))

(define (enable-debug)
  (vm-set-default-exception-handler (current-vm) debug))

(define (disable-debug)
  (vm-set-default-exception-handler (current-vm) #f))

(provide "gauche/vm/debugger")

