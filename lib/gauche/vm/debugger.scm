;;;
;;; Debugger - terminal base debugger
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
;;;  $Id: debugger.scm,v 1.16.2.2 2005-01-01 07:22:18 shirok Exp $
;;;

;; NB: this is still a working version.  

(define-module gauche.vm.debugger
  (use srfi-1)
  (use srfi-2)
  (use srfi-13)
  (use text.parse)
  (extend gauche.internal)
  (export enable-debug disable-debug debug-print)
  )
(select-module gauche.vm.debugger)

(define *stack-show-depth* 20)
(define *expr-show-length* 65)

;; Debug print stub ------------------------------------------
;; (this is temporary implementation)
(define-syntax debug-print
  (syntax-rules ()
    ((_ ?form)
     (let1 f '?form
       (or (and-let* ((info (and (pair? f)
                                 (pair-attribute-get f 'source-info #f)))
                      ((pair? info))
                      ((pair? (cdr info))))
             (format/ss (current-error-port) "#?=~s:~a:~,,,,65:s\n"
                        (car info) (cadr info) f)
             #t)
           (format (current-error-port) "#?=~,,,,65:s\n" f))
       (receive vals ?form
         (if (null? vals)
             (format (current-error-port) "#?-<void>\n")
             (begin
               (format/ss (current-error-port) "#?-    ~,,,,65:s\n" (car vals))
               (for-each (lambda (elt)
                           (format/ss (current-error-port)
                                      "#?+    ~,,,,65:s\n" elt))
                         (cdr vals))))
         (apply values vals))))))

;; Print stack trace -----------------------------------------
;; NB: the same code is in vm.c.  Should be refactored.
(define (debug-print-stack stack max-depth)
  (let ((outp (current-error-port)))
    (format outp "Stack trace\n")
    (format outp "________________________________________________\n")
    (do ((s stack (cdr s))
         (i 0     (+ i 1)))
        ((or (null? s) (> i max-depth))
         (unless (null? s) (format outp "...\n")))
      (let ((code (caar s)))
        (format/ss outp "~3d  ~,,,,v:s\n" i *expr-show-length* code)
        (or (and-let* (((pair? code))
                       (info (pair-attribute-get code 'source-info #f))
                       ((pair? info))
                       ((pair? (cdr info))))
              (format outp "       At line ~a of ~s\n" (cadr info) (car info))
              #t)
            (format outp "       [unknown location]\n"))))
    ))

;; Debugger entry point ---------------------------------------
;; Called when an error occurs
(define (debug exception)
  (disable-debug) ;; prevent to enter debugger again
  (with-error-handler
      (lambda (e)
        (format (current-error-port) "Oops, error in debugger.\n"))
    (lambda ()
      (let ((stack (cdddr (vm-get-stack-trace))) ;remove our stack frames
            (outp  (current-error-port)))
        (if (is-a? exception <exception>)
            (format outp "*** Error: ~a\n" (slot-ref exception 'message))
            (format outp "*** Error: ~a\n" exception))
        (debug-print-stack stack *stack-show-depth*)
        (format outp "Entering debugger.  Type help for help.\n")
        (debug-loop stack))))
  (enable-debug))

;; Internal debug command loop --------------------------------
;; 
(define (debug-loop stack)
  ;; TODO: we need to gain terminal I/O.
  (let ((inp   (standard-input-port))
        (outp  (standard-error-port)))

    (define (current-stack level)
      (let loop ((n 0) (stack stack))
        (cond ((null? stack) #f)
              ((= n level) (car stack))
              (else (loop (+ n 1) (cdr stack))))))

    (define (show-env env)
      (let* ((up   (vector-ref env 0))
             (vals (vector-ref env 1)))
        (cond ((not (list? vals))
               (when (vector? up) (show-env up)))
              ((not (= (length vals) (- (vector-length env) 2)))
               (format outp "[Unrecognized env; compiler error?]\n" env))
              (else
               (do ((i 2 (+ i 1))
                    (vals vals (cdr vals)))
                   ((null? vals) (when (vector? up) (show-env up)))
                 (format outp " ~10@s = ~,,,,v:s\n" (car vals)
                         *expr-show-length*  (vector-ref env i))))
              )))

    (define (show-stack s level)
      (when (pair? s)
        (format/ss outp "~3d:  ~,,,,v:s\n" level *expr-show-length* (car s))
        (and-let* (((pair? (car s)))
                   (info (pair-attribute-get (car s) 'source-info #f))
                   ((pair? info))
                   ((pair? (cdr info))))
          (format outp "       At line ~a of ~s\n" (cadr info) (car info)))
        (when (vector? (cdr s)) 
          (show-env (cdr s)))))
    
    (define (loop level)
      (format outp "debug$ ")
      (flush outp)
      (let ((line (read-line inp)))
        (if (eof-object? line)
            (newline outp)
            (let ((cmd (with-input-from-string line
                         (lambda () (next-token #[\s] '(#[\s] *eof*))))))
              (cond
               ((equal? cmd "") (loop level))
               ((member cmd '("s" "sh" "sho" "show")) (show level))
               ((member cmd '("u" "up"))              (up   level))
               ((member cmd '("d" "do" "dow" "down")) (down level))
               ((member cmd '("q" "qu" "qui" "quit")))
               (else (help level)))))))

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
      (format outp "  show       show current frame\n")
      (format outp "  up         up one frame\n")
      (format outp "  down       down one frame\n")
      (format outp "  quit       return to toplevel\n")
      (loop level))

    (loop 0)
    ))

(define (enable-debug)
  (vm-set-default-exception-handler (current-thread) debug))

(define (disable-debug)
  (vm-set-default-exception-handler (current-thread) #f))

(provide "gauche/vm/debugger")

