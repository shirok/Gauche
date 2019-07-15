;;;
;;; gauche/listener - listerner utility
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
   (output-port :init-keyword :output-port :init-form (current-output-port))
   (error-port  :init-keyword :error-port  :init-form (current-error-port))
   (reader      :init-keyword :reader    :init-form read)
   (evaluator   :init-keyword :evaluator :init-form eval)
   (printer     :init-keyword :printer
                :init-form (lambda args
                             (for-each (^r (write r) (newline)) args)))
   (prompter    :init-keyword :prompter
                :init-form (cut display "listener> "))
   (environment :init-keyword :environment
                :init-form (interaction-environment))
   (finalizer   :init-keyword :finalizer :init-form #f)
   (error-handler :init-keyword :error-handler
                  :init-form report-error)
   (fatal-handler :init-keyword :fatal-handler
                  :init-form #f)
   ;; Private
   (rbuf        :init-value "")
   (original-input-port)    ; capture std ports when listener-read-handler is
   (original-output-port)   ;   called.
   (original-error-port)    ;
   ))

(define-method listener-show-prompt ((self <listener>))
  (guard (e [(sigpipe? e)
             (or (listener-fatal self e)
                 (listener-finalize self))
             #f])
    (with-output-to-port (ref self 'output-port)
      (^[]
        ((ref self 'prompter))
        (flush)
        #t))))

;; Returns a thunk which should be called whenever input is
;; available.

;; Error handling is rather convoluted, for we have to deal with
;; a few different cases:
;;
;;  - I/O errors with reading/writing the client is regarded a
;;    fatal error.  The fatal-handler is called if the listener
;;    has any.  If the fatal-handler returns #f or the listener
;;    doesn't have one, the finalizer is called.  Usually these
;;    handlers removes listener to be called again.
;;
;;  - Errors during fatal handler or finalizer are "passed through"
;;    to the caller of the listener handler.  Usually it has a
;;    fatal consequence.  The fatal-handler and finalizer should
;;    be written so that foreseeable errors are properly handled
;;    within them.
;;
;;  - Other errors (e.g. read error while reading provided S-expr,
;;    or evaluation error) are handled by listener's error-handler.
;;    Double fault is regarded as a fatal error.

(define-method listener-read-handler ((self <listener>))

  (define (body return)
    (define (finish)
      (guard (e [else (return e)])
        (listener-finalize self)
        (return #f)))
    (define (abort e)
      (guard (e [else (return e)])
        (or (listener-fatal self e) (listener-finalize self))
        (return #f)))

    (let1 chunk (guard (e [else (abort e)])
                  (read-block 8192 (ref self 'input-port)))
      (when (eof-object? chunk) (finish))
      (with-ports
          (ref self 'input-port)
          (ref self 'output-port)
          (ref self 'error-port)
        (^[]
          (update! (ref self 'rbuf) (cut string-append <> chunk))
          (string-incomplete->complete (ref self 'rbuf))
          (guard (e [else
                     (set! (ref self 'rbuf) "")
                     (guard (e1 [else (abort e1)])
                       ((ref self 'error-handler) e)
                       (listener-show-prompt self)
                       #f)])
            (let loop ()
              (update! (ref self 'rbuf) (cut string-trim <> #[\s]))
              (and (not (string-null? (ref self 'rbuf)))
                   (complete-sexp? (ref self 'rbuf))
                   (begin
                     (with-input-from-string (ref self 'rbuf)
                       (^[]
                         (let1 expr ((ref self 'reader))
                           (when (eof-object? expr) (finish))
                           (receive r
                               ((ref self 'evaluator) expr
                                (ref self 'environment))
                             (guard (e [(sigpipe? e) (abort e)])
                               (apply (ref self 'printer) r)
                               (flush)))
                           (set! (ref self 'rbuf)
                                 (get-remaining-input-string
                                  (current-input-port))))))
                     (and (listener-show-prompt self)
                          (loop))))))))))

  ;; Capture std ports when the handler is created
  (set! (ref self 'original-input-port)  (current-input-port))
  (set! (ref self 'original-output-port) (current-output-port))
  (set! (ref self 'original-error-port)  (current-error-port))

  ;; Returns a handler closure.
  (^[]
    (cond [(call/cc body) => raise]
          [else #t]))
  )

;; Check if the given string can be parsed as a complete sexp.
;; Note that this test doesn't rule out all invalid sexprs.
;;
;; NB: This should eventually be folded into build-in read, so that
;; any nontrivial syntax can be handled consistently.

(define (complete-sexp? str)
  (with-input-from-string str
    (^[]
      ;; charset that delimits token
      (define special-chars #[\u0000-\u0020\"\'()\,\;\[\\\]\`{|}\u007f])

      ;; main loop
      (define (rec closer)
        (let1 ch (read-char)
          (cond [(eof-object? ch) (if closer #f #t)]
                [(eqv? closer ch) #t]
                [(eqv? #\( ch) (and (rec #\) ) (rec closer))]
                [(eqv? #\[ ch) (and (rec #\] ) (rec closer))]
                [(eqv? #\{ ch) (and (rec #\} ) (rec closer))]
                [(eqv? #\" ch) (and (rec-escaped #\") (rec closer))]
                [(eqv? #\| ch) (and (rec-escaped #\|) (rec closer))]
                [(eqv? #\; ch) (skip-to-nl) (rec closer)]
                [(eqv? #\# ch)
                 (let1 c2 (read-char)
                   (cond [(eof-object? c2) #f]
                         [(eqv? c2 #\\)
                          (and (not (eof-object? (read-char)))
                               (begin (skip-token) (rec closer)))]
                         [(eqv? c2 #\/) (and (rec-escaped #\/) (rec closer))]
                         [(eqv? c2 #\[) (and (rec-escaped #\]) (rec closer))]
                         [(eqv? c2 #\,)
                          (let1 c3 (skip-ws)
                            (cond [(eof-object? c3) #f]
                                  [(eqv? #\( c3) (and (rec #\) ) (rec closer))]
                                  [(eqv? #\[ c3) (and (rec #\] ) (rec closer))]
                                  [(eqv? #\{ c3) (and (rec #\} ) (rec closer))]
                                  [else (skip-token) (rec closer)]))]
                         [(eqv? c2 #\() (and (rec #\)) (rec closer))]
                         [(eqv? c2 #\<)
                          (errorf "unreadable sequence #<~a..."
                                  (read-block 10))]
                         [else (rec closer)]))]
                [else (rec closer)])))

      (define (rec-escaped closer)
        (let1 ch (read-char)
          (cond [(eof-object? ch) #f]
                [(eqv? closer ch) #t]
                [(eqv? #\\ ch) (read-char) (rec-escaped closer)]
                [else (rec-escaped closer)])))

      (define (skip-token)
        (let loop ([ch (peek-char)])
          (unless (or (eof-object? ch)
                      (char-set-contains? special-chars ch))
            (read-char)
            (loop (peek-char)))))

      (define (skip-ws)
        (let loop ([ch (read-char)])
          (if (or (eof-object? ch)
                  (char-set-contains? #[\S] ch))
            ch
            (loop (read-char)))))

      (define (skip-to-nl)
        (let loop ([ch (read-char)])
          (unless (or (eof-object? ch)
                      (eqv? ch #\newline))
            (loop (read-char)))))

      ;; body
      (rec #f)
      )))

;;;
;;; Private utils
;;;

(define-method listener-finalize ((self <listener>))
  (and-let* ([f (ref self 'finalizer)])
    (with-ports
        (ref self 'original-input-port)
        (ref self 'original-output-port)
        (ref self 'original-error-port)
      f)))

(define-method listener-fatal ((self <listener>) e)
  (and-let* ([f (ref self 'fatal-handler)])
    (with-ports
        (ref self 'original-input-port)
        (ref self 'original-output-port)
        (ref self 'original-error-port)
      (cut f e))))

(define (sigpipe? e)
  (cond-expand
   [gauche.os.windows #f]
   [else (and (<unhandled-signal-error> e)
              (eqv? (ref e 'signal) SIGPIPE))]))

