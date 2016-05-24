;;;
;;; interactive/toplevel.scm - toplevel commands
;;;
;;;   Copyright (c) 2015  Shiro Kawai  <shiro@acm.org>
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

;; This module is autoloaded from gauche.interactive.

;;; Scheme48-style toplevel commands
;;;
;;;   ,command arg ...
;;;
;;; Command is a word (Scheme identifier).  After the command, characters
;;; until EOL are read and splitted into arguments in similar way to shell
;;; command-line processing.
;;;
;;; The initial ,command part is read by read - which becomes (unquote command)
;;; - and then the control passed to handle-toplevel-command.

(define-module gauche.interactive.toplevel
  (use gauche.interactive)
  (use gauche.threads)
  (use gauche.generator)
  (use gauche.sequence)
  (use srfi-13)
  (use util.match)
  (use data.trie)
  (export handle-toplevel-command)
  )
(select-module gauche.interactive.toplevel)

(autoload file.util home-directory expand-path)

;; toplevel-commands
;; Map from symbol to (help-message proc)
(define *toplevel-commands* (atom (make-trie)))

(define (toplevel-command-add! key help handler)
  ($ atomic *toplevel-commands*
     (^t (trie-put! t (x->string key) `(,help ,handler)))))

;; Returns [(key help handler)]
(define (toplevel-command-lookup key)
  (let1 k (x->string key)
    ($ atomic *toplevel-commands*
       (^t (if-let1 h&h (trie-get t k #f)
             `((,k . ,h&h))
             (sort (trie-common-prefix t k) string<? car))))))

;; Returns [((key ...) help-string)]
(define (toplevel-command-keys)
  ($ atomic *toplevel-commands*
     (^t ($ map (^[grp] (cons (map car grp) (cdar grp)))
            $ group-collection
            ($ map (^p (cons (car p) (cadr p))) $ trie->list t)
            :key cdr))))

;; A handler return value that does nothing
(define *no-value* `(,(with-module gauche values)))

(define (ambiguous-command given possibilities)
  (print #"Ambiguous toplevel command: ~|given|")
  (print "Did you mean:")
  (dolist [p possibilities] (print #"  ~p")))

(define (toplevel-command-helper key)
  (^[]
    (match (toplevel-command-lookup key)
      [()    (print "Unknown toplevel command: " key)]
      [((cmd help handler)) (print "Usage: " help)]
      [((cmd help handler) ...) (ambiguous-command key cmd)])
    *no-value*))

;; (define-toplevel-command cmds helpmsg handler)
;;  cmds    - a symbol, or a list of symbols.
;;  helpmsg - one line help message, followed by multiline detailed description.
;;            command name(s) is/are automatically prepended at the beginning
;;            so no need to be included.
;;  handler - a procedure to be called.  with one parameter - a list of
;;            command arguments.
(define-syntax define-toplevel-command
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ keys help handler)
        (let* ([keys (if (list? keys) keys (list keys))]
               [help (string-append (string-join (map x->string keys) "|")
                                    help)])
          `(,(r'begin)
            ,@(map (^[key]
                     `(,(r'toplevel-command-add!) (,(r'quote) ,key) ,help
                       (,(r'let1) usage (,(r'toplevel-command-helper)
                                         (,(r'quote) ,key))
                        ,handler)))
                   keys)))]))))

;; API
;; Entry point - called by REPL reader.
;; Whatever the handler returns, it is treated as if it's read
;; from the input.  If you don't need the result to be evaluated,
;; you can return *no-value*.
(define (handle-toplevel-command command line)
  (unless (symbol? command)
    (error "Invalid REPL toplevel command:" command))
  (match (toplevel-command-lookup command)
    [() (print #"Unrecognized REPL toplevel command: ~command")
     (print "Type ,help for the list of available commands.") *no-value*]
    [((cmd help handler))
     (handler (generator->list (cute read (open-input-string line))))]
    [((cmd help handler) ...) (ambiguous-command command cmd) *no-value*]))

;;
;; Predefined commands
;;

(define-toplevel-command (apropos a)
  " regexp [module-name]\n\
 Show the names of global bindings that match the regexp.\n\
 If module-name (symbol) is given, the search is limited in the named module."
  (^[args]
    (define (->regexp x)
      (cond [(regexp? x) x]
            [else (string->regexp (x->string x))]))
    (match args
      [(word) `(apropos ,(->regexp word))]
      [(word mod) `(apropos ,(->regexp word) ',mod)]
      [_ (usage)])))

(define-toplevel-command (describe d)
  " [object]\n\
 Describe the object.\nWithout arguments, describe the last REPL result."
  (^[args]
    (match args
      [() `(,(with-module gauche.interactive describe))]
      [(obj) `(,(with-module gauche.interactive describe) ,obj)]
      [_ (usage)])))

(define-toplevel-command history
  "\n\
 Show REPL history."
  (^[args]
    (match args
      [() `(,(with-module gauche *history))]
      [_ (usage)])))

(define-toplevel-command (info doc)
  " name\n\
 Show info document for an entry of NAME.\n\
 NAME can be a name of a function, syntax, macro, module, or class."
  (^[args]
    (define (->name x) ; to preserve ':' of keyword
      (if (keyword? x) #":~x" (x->string x)))
    (match args
      [(name) `(,(with-module gauche.interactive info) ,(->name name))]
      [_ (usage)])))

(define-toplevel-command (help h)
  " [command]\n\
 Show the help message of the command.\n\
 Without arguments, show the list of all toplevel commands."
  (^[args]
    (define (get-cmd&help help-string)
      (let* ((ls   (call-with-input-string help-string port->string-lseq))
             (cmd  (or (rxmatch->string #/^\S*/ (list-ref ls 0 "")) ""))
             (help (list-ref ls 1 "")))
        (cons cmd help)))
    (match args
      [()
       (print "You're in REPL (read-eval-print-loop) of Gauche shell.")
       (print "Type a Scheme expression to evaluate.")
       (print "A word preceeded with comma has special meaning.  Type ,help <cmd> ")
       (print "to see the detailed help for <cmd>.")
       (print "Commands can be abbreviated as far as it is not ambiguous.")
       (print)
       (dolist [cmd&help
                (sort (map (^p (get-cmd&help (cdr p)))
                           (toplevel-command-keys))
                      string<? car)]
         (format #t (if (> (string-length (car cmd&help)) 10)
                      " ,~10a\n             ~a\n"
                      " ,~10a ~a\n")
                 (car cmd&help)
                 (cdr cmd&help)))
       *no-value*]
      [(cmd) ((toplevel-command-helper cmd)) *no-value*]
      [_ (usage)])))

(define-toplevel-command pwd
  "\n\
 Print working directory."
  (^[args]
    (match args
      [() (print (sys-getcwd)) *no-value*]
      [_ (usage)])))

(define-toplevel-command cd
  " [directory]\n\
 Change the current directory.\n\
 Without arguments, change to the home directory."
  (^[args]
    (let1 dir (match args
                [() (home-directory)]
                [(dir) (expand-path (x->string dir))]
                [_ #f])
      (if dir
        (begin (sys-chdir dir) (sys-getcwd))
        (usage)))))


;; This can be better - to make it work on generic functions,
;; show source location as well, etc.
(define-toplevel-command source
  " procedure\n\
 Show source code of the procedure if it's available."
  (^[args]
    (match args
      [(word) `(or (,(with-module gauche source-code) ,word)
                   (begin (print "No source code is available for: " ',word)
                          (values)))]
      [_ (usage)])))
