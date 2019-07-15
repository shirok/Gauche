;;;
;;; interactive/toplevel.scm - toplevel commands
;;;
;;;   Copyright (c) 2015-2019  Shiro Kawai  <shiro@acm.org>
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
  (use gauche.process)
  (use srfi-13)
  (use util.match)
  (use data.trie)
  (export handle-toplevel-command)
  )
(select-module gauche.interactive.toplevel)

(autoload file.util home-directory expand-path)

;; toplevel-commands
;; Map from symbol to (parser help-message proc)
(define *toplevel-commands* (atom (make-trie)))

(define (toplevel-command-add! key parser help handler)
  ($ atomic *toplevel-commands*
     (^t (trie-put! t (x->string key) `(,parser ,help ,handler)))))

;; Returns [(key parser help handler)]
(define (toplevel-command-lookup key)
  (let1 k (x->string key)
    ($ atomic *toplevel-commands*
       (^t (if-let1 v (trie-get t k #f)
             `((,k . ,v))
             (sort (trie-common-prefix t k) string<? car))))))

;; Returns [((key ...) help-string)]
(define (toplevel-command-keys)
  ($ atomic *toplevel-commands*
     (^t ($ map (^[grp] (cons (map car grp) (cdar grp)))
            $ group-collection
            ($ map (^p (cons (car p) (caddr p))) $ trie->list t)
            :key cdr :test equal?))))

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
      [((cmd _ help _)) (print "Usage: " help)]
      [((cmd _ help _) ...) (ambiguous-command key cmd)])
    *no-value*))

;; parser
(define (get-arg-parser parser-key)
  (case parser-key
    [(:read) (^[line] (generator->list (cute read (open-input-string line))))]
    [(:trim) (^[line] (string-trim-both line))]
    [else (errorf "[internal] Invalid parser spec in define-toplevel-command:\
                   `~s': must be oen of ~s."
                  parser-key '(:read :trim))]))

;; (define-toplevel-command cmds arg-parser helpmsg handler)
;;  cmds    - a symbol, or a list of symbols.
;;  arg-parser - how to parse the arguments.  :read - Scheme's read,
;;            :trim - just pass the remaining line, with trimming preceding
;;            and following whitespaces.
;;  helpmsg - one line help message, followed by multiline detailed description.
;;            command name(s) is/are automatically prepended at the beginning
;;            so no need to be included.
;;  handler - a procedure to be called.  with one parameter - a list of
;;            command arguments.
;;            Inside handler, local bindings 'usage' and 'rename' is visible;
;;            'usage' is bound to a thunk to display usage, and 'rename' is
;;            bound to a procedure to hygienically rename identifiers.
(define-syntax define-toplevel-command
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ keys parser-spec help handler)
        (let* ([keys (if (list? keys) keys (list keys))]
               [parser (get-arg-parser parser-spec)]
               [help `(,(r'string-append)
                       (,(r'string-join)
                        (,(r'map) ,(r'x->string) ',keys) "|")
                       ,help)])
          `(,(r'begin)
            ,@(map (^[key]
                     (quasirename r
                       `(toplevel-command-add! ',key ',parser ,help
                          (let ([,'usage (toplevel-command-helper ',key)]
                                [,'rename ,r])
                            ,handler))))
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
    [((cmd parser help handler)) (handler (parser line))]
    [((cmd _ _ _) ...) (ambiguous-command command cmd) *no-value*]))

;;
;; Predefined commands
;;

(define-toplevel-command (apropos a) :read
  " regexp [module-name]\
 \nShow the names of global bindings that match the regexp.\
 \nIf module-name (symbol) is given, the search is limited in the named module."
  (^[args]
    (define (->regexp x)
      (cond [(regexp? x) x]
            [else (string->regexp (x->string x))]))
    (match args
      [(word) `(,(rename 'apropos) ,(->regexp word))]
      [(word mod) `(,(rename 'apropos) ,(->regexp word) ',mod)]
      [_ (usage)])))

(define-toplevel-command (describe d) :read
  " [object]\
 \nDescribe the object.\nWithout arguments, describe the last REPL result."
  (^[args]
    (match args
      [() `(,(rename 'describe))]
      [(obj) `(,(rename 'describe) ,obj)]
      [_ (usage)])))

(define-toplevel-command history :read
  "\
 \nShow REPL history."
  (^[args]
    (match args
      [() `(,(rename '*history))]
      [_ (usage)])))

(define-toplevel-command (info doc) :read
  " name-or-regexp\
 \nShow info document for an entry of NAME, or search entries by REGEXP.\
 \nNAME can be a name of a function, syntax, macro, module, or class.\
 \n  Example:  ,info cons\
 \nIf REGEXP is given instead of NAME, it lists the matching entry names.\
 \n  Example:  ,info #/cons/"
  (^[args]
    (define (->name x) ; to preserve ':' of keyword
      (if (keyword? x) #":~x" (x->string x)))
    (match args
      [(name)
       (if (regexp? name)
         `(,(rename 'info-search) ,name)
         `(,(rename 'info) ,(->name name)))]
      [_ (usage)])))

(define-toplevel-command (help h) :read
  " [command]\
 \nShow the help message of the command.\
 \nWithout arguments, show the list of all toplevel commands."
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
       (print "Evaluate (exit) to exit REPL.")
       (print "A word preceded with comma has special meaning.  Type ,help <cmd> ")
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
      [(('unquote cmd)) ((toplevel-command-helper cmd)) *no-value*]
      [(cmd) ((toplevel-command-helper cmd)) *no-value*]
      [_ (usage)])))

(define-toplevel-command pwd :read
  "\
 \nPrint working directory."
  (^[args]
    (match args
      [() (print (sys-getcwd)) *no-value*]
      [_ (usage)])))

(define-toplevel-command cd :read
  " [directory]\
 \nChange the current directory.\
 \nWithout arguments, change to the home directory."
  (^[args]
    (let1 dir (match args
                [() (home-directory)]
                [(dir) (expand-path (x->string dir))]
                [_ #f])
      (if dir
        (begin (sys-chdir dir) (sys-getcwd))
        (usage)))))

;; Run shell command.
;; A tradition to use '!' for shell escape, but "comma - exclamation-mark"
;; combination is a bit awkward to type.  "comma - s - h" is much easier.
(define-toplevel-command sh :trim
  "  command args ...\
 \nRun command via shell.\
 \nShell is taken from the environment variable SHELL, or /bin/sh if it's empty.\
 \nThe command line COMMAND ARGS ... are passed to the shell as is."
  (^[line]
    (cond-expand
     [gauche.os.windows
      ;; for MSYS (mintty)
      (if-let1 sh (sys-getenv "SHELL")
        (run-process `("cmd.exe" "/c" ,sh "-c" ,line) :wait #t)
        (run-process `("cmd.exe" "/c" ,line) :wait #t))]
     [else
      (let1 sh (or (sys-getenv "SHELL") "/bin/sh")
        (run-process `(,sh "-c" ,line) :wait #t))])
    *no-value*))

;; This can be better - to make it work on generic functions,
;; show source location as well, etc.
(define-toplevel-command source :read
  " procedure\
 \nShow source code of the procedure if it's available."
  (^[args]
    (match args
      [(word) (quasirename rename
                `(or (source-code ,word)
                     (begin (print "No source code is available for: " ',word)
                            (values))))]
      [_ (usage)])))

(define-toplevel-command (use u) :read
  " module [option ...]\
 \nUse the specified module.  Same as (use module option ...).\
 \nAllowed options:\
 \n  :only (symbol ...)\
 \n  :except (symbol ...)\
 \n  :prefix symbol\
 \n  :rename ((orig-name new-name) ...)\
 \nFor the details of options, type \",info import\" and select the first one."
  (^[args]
    (match args
      [(module . rest) `(,(rename 'use) ,module ,@rest)]
      [_ (usage)])))

(define-toplevel-command (reload r) :read
  " module\
 \nReload the specified module, using gauche.reload."
  (^[args]
    (match args
      [(module) `(,(rename 'reload) ',module)]
      [_ (usage)])))

(define-toplevel-command (load l) :read
  " file\
 \nLoad the specified file."
  (^[args]
    (match args
      [(file) `(,(rename 'load) ,(x->string file))]
      [_ (usage)])))

(define-constant default-pm (print-mode))

(define-toplevel-command (print-mode pm) :read
  #" default | [key value ...]\
  \nView/set print-mode of REPL.\
  \nWithout arguments, it shows the current print mode.\
  \nWith a single argument, 'default', resets the print mode to the default.\
  \nOtherwise, the arguments must be a key-value list where keys are symbols,\
  \nand values are either integers or booleans.\
  \nThe following keys are recognized.\
  \n  pretty <boolean>    - Use pretty printer [default: ~(~ default-pm'pretty)]\
  \n  length <integer>|#f - Max # of items shown in list/vector before abbreviated.\
  \n                        #f for unlimited.  [default: ~(~ default-pm'length)]\
  \n  level <integer>|#f  - Max # of nestings shown before abbreviated.\
  \n                        #f for unlimited.  [default: ~(~ default-pm'level)]\
  \n  width <integer>|#f  - Column width before line is wrapped.  Only used in\
  \n                        the pretty printer.  #f for unlimited.  [default: ~(~ default-pm'width)]\
  \n  base <integer>      - Base radix of showing whole numbers.  [default: ~(~ default-pm'base)]\
  \n  radix <boolean>     - Add radix prefix ('#x' etc.) befors whole numbers.\
  \n                        [default: ~(~ default-pm'radix)]"
  (^[args]
    (match args
      [() #f]
      [(default) (print-mode 'default)]
      [(kvs ...)
       (unless (even? (length kvs))
         (error "print-mode: given key-value list isn't even:" kvs))
       (apply print-mode
              (append-map
               (^[kv]
                 `(,(if (memq (car kv) '(pretty length level width base radix))
                      (make-keyword (car kv))
                      (error "print-mode: unrecognized key:" (car kv)))
                   ,(cadr kv)))
               (slices kvs 2)))])
    (let1 c (print-mode)
      (format #t "Current print mode:\n")
      (format #t "  length : ~3d\n"  (~ c'length))
      (format #t "   level : ~3d\n"  (~ c'level))
      (format #t "  pretty : ~3@a\n" (~ c'pretty))
      (format #t "   width : ~3d\n"  (~ c'width))
      (format #t "    base : ~3d\n"  (~ c'base))
      (format #t "   radix : ~3d\n"  (~ c'radix)))
    *no-value*))

(define-toplevel-command (print-all pa) :read
  "\
 \nPrint previous result (*1) without abbreviation.\
 \nThis is useful when you want the entire S-expression printed out in the\
 \nbuffer, for the copy&paste, etc."
  (^[exprs]
    (match exprs
      [() (write *1 (make-write-controls)) (newline)]
      [_ (usage)])
    *no-value*))

