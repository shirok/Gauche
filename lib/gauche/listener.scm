;;;
;;; gauche/listener - listerner utility
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
;;;  $Id: listener.scm,v 1.5 2003-07-05 03:29:11 shirok Exp $
;;;

;; provides functions useful to implement a repl listener

(define-module gauche.listener
  (use srfi-13)
  (export <listener>
          listener-read-handler
          listener-show-prompt
          complete-sexp?)
  )
(select-module gauche.listener)

;; Listener class
;;
;;   <listener> is a single-buffered, that is, only the input is
;;   buffered until it consists a valid S-expression.  The output
;;   is directly sent to the output port.  It's enough to handle
;;   usual situation.
;;
;;   A possible variant of <listener> is a double-buffered listener,
;;   that buffers output as well, which will be sent to the output
;;   port whenever it is ready.

(define-class <listener> ()
  ((input-port  :init-keyword :input-port  :init-form (current-input-port))
   (ouptut-port :init-keyword :output-port :init-form (current-output-port))
   (error-port  :init-keyword :error-port  :init-form (current-error-port))
   (reader      :init-keyword :reader    :init-form read)
   (evaluator   :init-keyword :evaluator :init-form eval)
   (printer     :init-keyword :printer
                :init-form (lambda args
                             (for-each (lambda (r) (write r) (newline)) args)))
   (prompter    :init-keyword :prompter
                :init-form (lambda () (display "listener> ")))
   (environment :init-keyword :environment
                :init-form (interaction-environment))
   (finalizer   :init-keyword :finalizer :init-form #f)
   (error-handler :init-keyword :error-handler
                :init-form (lambda (e) (report-error e)))
   (rbuf        :init-value "")
   ))

(define-method listener-show-prompt ((self <listener>))
  (with-output-to-port (ref self 'ouptut-port)
    (lambda ()
      ((ref self 'prompter))
      (flush))))

(define-method listener-read-handler ((self <listener>))
  (define (repl)
    (with-error-handler
     (lambda (e)
       (set! (ref self 'rbuf) "")
       ((ref self 'error-handler) e)
       (listener-show-prompt self))
     (lambda ()
       (update! (ref self 'rbuf) (cut string-trim <> #[\s]))
       (when (and (not (string-null? (ref self 'rbuf)))
                  (complete-sexp? (ref self 'rbuf)))
         (with-input-from-string (ref self 'rbuf)
           (lambda ()
             (let* ((env  (ref self 'environment))
                    (expr ((ref self 'reader))))
               (unless (eof-object? expr)
                 (with-output-to-port (ref self 'ouptut-port)
                   (lambda ()
                     (call-with-values
                      (lambda () ((ref self 'evaluator) expr env))
                      (ref self 'printer))))
                 (listener-show-prompt self)))
             (set! (ref self 'rbuf) (port->string (current-input-port)))))
         (repl)))))

  (lambda ()
    (let ((chunk (read-block 8192 (ref self 'input-port))))
      (if (eof-object? chunk)
          (cond ((ref self 'finalizer) => (lambda (f) (f))))
          (begin
            (update! (ref self 'rbuf) (cut string-append <> chunk))
            (string-incomplete->complete! (ref self 'rbuf))
            (with-error-to-port (ref self 'error-port) repl)))))
  )

;; Check if the given string can be parsed as a complete sexp.
;; Note that this test doesn't rule out all invalid sexprs.
(define (complete-sexp? str)
  (with-input-from-string str
    (lambda ()
      ;; charset that delimits token
      (define special-chars #[\x00-\x20\"\'()\,\;\[\\\]\`{|}\x7f])

      ;; main loop
      (define (rec closer)
        (let1 ch (read-char)
          (cond ((eof-object? ch) (if closer #f #t))
                ((eqv? closer ch) #t)
                ((eqv? #\( ch) (and (rec #\) ) (rec closer)))
                ((eqv? #\[ ch) (and (rec #\] ) (rec closer)))
                ((eqv? #\{ ch) (and (rec #\} ) (rec closer)))
                ((eqv? #\" ch) (and (rec-escaped #\") (rec closer)))
                ((eqv? #\| ch) (and (rec-escaped #\|) (rec closer)))
                ((eqv? #\; ch) (skip-to-nl) (rec closer))
                ((eqv? #\# ch)
                 (let1 c2 (read-char)
                   (cond ((eof-object? c2) #f)
                         ((eqv? c2 #\\)
                          (and (not (eof-object? (read-char)))
                               (begin (skip-token) (rec closer))))
                         ((eqv? c2 #\/) (and (rec-escaped #\/) (rec closer)))
                         ((eqv? c2 #\[) (and (rec-escaped #\]) (rec closer)))
                         ((eqv? c2 #\,)
                          (let1 c3 (skip-ws)
                            (cond ((eof-object? c3) #f)
                                  ((eqv? #\( c3) (and (rec #\) ) (rec closer)))
                                  ((eqv? #\[ c3) (and (rec #\] ) (rec closer)))
                                  ((eqv? #\{ c3) (and (rec #\} ) (rec closer)))
                                  (else (skip-token) (rec closer)))))
                         ((eqv? c2 #\() (and (rec #\)) (rec closer)))
                         ((eqv? c2 #\<)
                          (errorf "unreadable sequence #<~a..."
                                  (read-block 10)))
                         (else (rec closer)))))
                (else (rec closer)))))
      
      (define (rec-escaped closer)
        (let1 ch (read-char)
          (cond ((eof-object? ch) #f)
                ((eqv? closer ch) #t)
                ((eqv? #\\ ch) (read-char) (rec-escaped closer))
                (else (rec-escaped closer)))))

      (define (skip-token)
        (let loop ((ch (peek-char)))
          (unless (or (eof-object? ch)
                      (char-set-contains? special-chars ch))
            (read-char)
            (loop (peek-char)))))

      (define (skip-ws)
        (let loop ((ch (read-char)))
          (if (or (eof-object? ch)
                  (char-set-contains? #[\S] ch))
              ch
              (loop (read-char)))))

      (define (skip-to-nl)
        (let loop ((ch (read-char)))
          (unless (or (eof-object? ch)
                      (eqv? ch #\newline))
            (loop (read-char)))))

      ;; body
      (rec #f)
      )))

(provide "gauche/listener")
