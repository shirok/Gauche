;;;
;;; gauche.interactive - useful stuff in the interactive session
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

#!no-fold-case

(define-module gauche.interactive
  (export apropos d read-eval-print-loop print-mode
          ;; autoloaded symbols follow
          info info-page info-search reload ed
          reload-modified-modules module-reload-rules reload-verbose)
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
                        ;; Note: future extension
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

;; NB: The base methods (describe (obj <top>)) and
;; (describe-slots (obj <top>)) are defined in src/libobj.scm

(define-method describe () (describe *1)) ; for convenience

(define-method describe ((s <symbol>))
  (describe-common s)
  (describe-symbol-bindings s) ;; autoloaded from gauche.modutil
  (values))

(define-method describe ((c <char>))
  (describe-common c)
  (format #t "  (U+~4,'0x, ~a)\n" (char->ucs c) (char-general-category c))
  (values))

(define-method describe ((n <integer>))
  (describe-common n)
  (when (exact? n)
    (format #t "  (#x~,,'_,8:x" n)
    (when (<= 1000 n #e1e26) ; 10^26 is approx to 2^89
      (let loop ([nn n] [unit '(_ Ki Mi Gi Ti Pi Ei Zi Yi)])
        (cond [(null? unit)]
              [(< nn 1000)
               (format #t ", ~~ ~,,,,3a~a" (floor nn) (car unit))]
              ;; I'm not sure how to round in binary-prefix system, but it's
              ;; approximation anyway, so here it goes.
              [(< nn 9950)
               (let* ([N (floor (+ nn 50))]
                      [N0 (quotient N 1000)]
                      [N1 (quotient (modulo N 1000) 100)])
                 (format #t ", ~~ ~d.~d~a" N0 N1 (cadr unit)))]
              [else (loop (/ nn 1024) (cdr unit))])))
    (when (and (<= 0 n #x10ffff)
               (let1 c (ucs->char n)
                 (or (memq (char-general-category c) '(Ll Lm Lo Lt Lu
                                                       Nd Nl No
                                                       Pc Pd Pe Pf Pi Po Ps
                                                       Sc Sk Sm So))
                     (memv c '(#\null #\alarm #\backspace #\tab #\newline
                               #\return #\escape #\space)))))
      (format #t ", ~s as char" (ucs->char n)))
    (when (and (<= 0 n (expt 2 31)))
      (format #t ", ~a as unix-time"
              (sys-strftime "%Y-%m-%dT%H:%M:%SZ" (sys-gmtime n))))
    (format #t ")\n")
    (values)))

(define-method describe ((g <generic>))
  (next-method) ; common object description
  (print "methods:")
  (dolist [m (~ g'methods)]
    (let ([spnames (map class-name (~ m'specializers))]
          [has-optional? (~ m'optional)])
      (format #t "  ~s\n"
              (if has-optional?
                (append spnames '_) ; this works even spnames is ()
                spnames))))
  (and-let1 dis ((with-module gauche.object generic-dispatcher-info) g)
    (format #t "dispatcher:\n  ~s\n" dis))
  (values))

(define-method describe ((p <procedure>))
  (describe-common p)
  (if-let1 source (source-location p)
    (format #t "Defined at ~s:~d\n" (car source) (cadr source)))
  (describe-slots p)
  (values))

(define d describe)

;;;
;;; Enhanced REPL
;;;

(autoload gauche.interactive.editable-reader make-editable-reader)

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

(define default-prompt-string
  (let1 user-module (find-module 'user)
    (^[:optional (delim ">")]
      (let1 m ((with-module gauche.internal vm-current-module))
        (if (eq? m user-module)
          (format "~a~a " *repl-name* delim)
          (format "~a[~a]~a " *repl-name* (module-name m) delim))))))

;; Returns a reader procedure that can handle toplevel command.
;; READ - reads one sexpr from the REPL input
;; READ-LINE - read to the EOL from REPL input and returns a string.
;;             The newline char is read but not included in the string.
;; SKIPPER - consumes trailing whitespaces from REPL input until either
;;           first newline is read, or encounters non-whitespace character.
(define (make-repl-reader read read-line skipper)
  (^[]
    (let1 expr (read)
      (if (and (pair? expr)      ; avoid depending on util.match yet
               (eq? (car expr) 'unquote)
               (pair? (cdr expr))
               (null? (cddr expr)))
        (handle-toplevel-command (cadr expr) (read-line))
        (begin
          (unless (eof-object? expr) (skipper))
          expr)))))

;; EXPERIMENTAL: Environment GAUCHE_READ_EDIT enables editing mode.
;; Note that, at this moment, text.line-edit isn't complete; it doesn't
;; handle multibyte characters nor the multiline expressions bigger
;; than the screen height.  Once we complete text.line-edit, we make
;; the feature available through command-line options of gosh.

(define-values (%prompter %reader %line-edit-ctx)
  (receive (r rl skipper ctx)
      (if (sys-getenv "GAUCHE_READ_EDIT")
        (make-editable-reader (^[] (default-prompt-string "$")))
        (values #f #f #f #f))
    (if (and r rl skipper ctx)
      (values (^[] #f)
              (make-repl-reader r rl skipper)
              ctx)
      (values (^[] (display (default-prompt-string)) (flush))
              (make-repl-reader read read-line
                                consume-trailing-whitespaces)
              #f))))

;; error printing will be handled by the original read-eval-print-loop
(define (%evaluator expr env)
  (guard (e [else (%set-history-exception! e) (raise e)])
    (receive r (eval expr env)
      (%set-history-expr! r)
      (apply values r))))

;; <write-controls> used for the printer.
(define-constant *default-controls*
  (make-write-controls :length 50 :level 10 :width 79
                       :pretty (not (sys-getenv "GAUCHE_REPL_NO_PPRINT"))))
(define *controls* *default-controls*)

(define (%printer . exprs)
  (dolist [expr exprs]
    (write expr *controls*)
    (newline)))

;; API
(define print-mode
  (case-lambda
    [() *controls*]                     ; return the current controls
    [(c)                                ; set controls directly
     (let1 c (if (eq? c 'default)
               *default-controls*
               c)
       (assume-type c <write-controls>)
       (rlet1 old *controls*
         (set! *controls* c)))]
    [kvs
     (rlet1 old *controls*
       (set! *controls* (apply write-controls-copy *controls* kvs)))]))

;; This shadows gauche#read-eval-print-loop
(define (read-eval-print-loop :optional (reader #f)
                                        (evaluator #f)
                                        (printer #f)
                                        (prompter #f))
  (let ([reader    (or reader %reader)]
        [evaluator (or evaluator %evaluator)]
        [printer   (or printer %printer)]
        [prompter  (or prompter %prompter)])
    ((with-module gauche read-eval-print-loop)
     reader evaluator printer prompter)))

;;;
;;; Misc. setup
;;;

;; EXPERIMENTAL: windows console code page support for text.line-edit
;; NB: ces (character encoding scheme) conversion is not implemented.
(cond-expand
 [gauche.os.windows
  (autoload os.windows sys-get-console-output-cp)
  ;; check if we have a windows console.
  (when (or (sys-isatty (standard-input-port))
            (sys-isatty (standard-output-port))
            (sys-isatty (standard-error-port)))
    ;; wide character settings for text.line-edit
    (if-let1 ctx %line-edit-ctx
      (case (sys-get-console-output-cp)
        [(65001)
         (set! (~ ctx 'wide-char-disp-setting 'mode) 'Surrogate)
         (set! (~ ctx 'wide-char-pos-setting  'mode) 'Surrogate)
         (set! (~ ctx 'wide-char-disp-setting 'wide-char-width) 2)
         (set! (~ ctx 'wide-char-pos-setting  'wide-char-width) 1)
         (set! (~ ctx 'wide-char-disp-setting 'surrogate-char-width) 2)
         (set! (~ ctx 'wide-char-pos-setting  'surrogate-char-width) 2)]
        [else ; 932 etc.
         (set! (~ ctx 'wide-char-disp-setting 'mode) 'Surrogate)
         (set! (~ ctx 'wide-char-pos-setting  'mode) 'Surrogate)
         (set! (~ ctx 'wide-char-disp-setting 'wide-char-width) 2)
         (set! (~ ctx 'wide-char-pos-setting  'wide-char-width) 2)
         (set! (~ ctx 'wide-char-disp-setting 'surrogate-char-width) 4)
         (set! (~ ctx 'wide-char-pos-setting  'surrogate-char-width) 4)])))]
 [else])

;; Autoload online info viewer
(autoload gauche.interactive.info info info-page info-search)

;; Autoload module reloader
(autoload gauche.reload reload reload-modified-modules
                        module-reload-rules reload-verbose)

;; Autoload editor invoker
(autoload gauche.interactive.ed ed ed-pick-file)

;; Autoload toplevel command handler
(autoload gauche.interactive.toplevel handle-toplevel-command)

;; See (describe <symbol>) above
(autoload gauche.modutil describe-symbol-bindings)

;; This might help first time users
(define-in-module user help "Type ,help (comma and help) for help")

