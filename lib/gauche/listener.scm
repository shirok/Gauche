;;;
;;; gauche/listener - listerner utility
;;;
;;;  Copyright(C) 2002 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: listener.scm,v 1.1 2002-10-22 08:32:56 shirok Exp $
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
  ((iport     :init-keyword :input-port  :init-form (current-input-port))
   (oport     :init-keyword :output-port :init-form (current-output-port))
   (eport     :init-keyword :error-port  :init-form (current-error-port))
   (reader    :init-keyword :reader    :init-form read)
   (evaluator :init-keyword :evaluator :init-form eval)
   (printer   :init-keyword :printer
              :init-form (lambda args
                           (for-each (lambda (r) (write r) (newline)) args)))
   (prompter  :init-keyword :prompter
              :init-form (lambda () (display "listener> ")))
   (environment :init-keyword :environment
                :init-form (interaction-environment))
   (finalizer :init-keyword :finalizer :init-form #f)
   (rbuf      :init-value "")
   ))

(define-method listener-show-prompt ((self <listener>))
  (with-output-to-port (ref self 'oport)
    (lambda ()
      ((ref self 'prompter))
      (flush))))

(define-method listener-read-handler ((self <listener>))
  (define (repl)
    (let ((istr (ref self 'rbuf)))
      (string-incomplete->complete! istr)
      (when (complete-sexp? istr)
        (with-input-from-string istr
          (lambda ()
            ;; The dynamic-wind lets the current dynamic error handler
            ;; handle whatever error occurred inside read-eval-print
            ;; sequence.
            (dynamic-wind
             (lambda () #f)
             (lambda ()
               (let* ((env  (ref self 'environment))
                      (expr ((ref self 'reader))))
                 (with-output-to-port (ref self 'oport)
                   (lambda ()
                     (call-with-values
                      (lambda () ((ref self 'evaluator) expr env))
                      (ref self 'printer))))))
             (lambda ()
               (set! (ref self 'rbuf)
                     (port->string (current-input-port)))
               (listener-show-prompt self)))
            (when (string-skip (ref self 'rbuf) #[\s]) (repl))))
        )))

  (lambda ()
    (let ((chunk (read-block 8192 (ref self 'iport))))
      (if (eof-object? chunk)
          (cond ((ref self 'finalizer) => (lambda (f) (f))))
          (begin
            (update! (ref self 'rbuf) (cut string-append <> chunk))
            (with-error-to-port (ref self 'eport) repl)))))
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

      ;; body
      (rec #f)
      )))

(provide "gauche/listener")
