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
  (use srfi-13)
  (use util.match)
  (export handle-toplevel-command)
  )
(select-module gauche.interactive.toplevel)

(autoload file.util home-directory expand-path)

(define *toplevel-commands* (atom (make-hash-table 'eq?)))

(define (toplevel-command-add! key handler)
  (atomic *toplevel-commands* (^t (hash-table-put! t key handler))))

(define (toplevel-command-lookup key)
  (atomic *toplevel-commands* (^t (hash-table-get t key #f))))

;; A handler return value that does nothing
(define *no-value* `(,(with-module gauche values)))

;; API
;; Entry point - called by REPL reader.
;; Whatever the handler returns, it is treated as if it's read
;; from the input.  If you don't need the result to be evaluated,
;; you can return *no-value*.
(define (handle-toplevel-command command line)
  (unless (symbol? command)
    (error "Invalid REPL toplevel command:" command))
  (if-let1 handler (toplevel-command-lookup command)
    ;; Just for now - we'll employ more sophisticated parser later
    (let* ([argline (string-trim-both line)]
           [args (if (equal? argline "")
                   '()
                   (string-split (string-trim-both line) #/\s+/))])
      (handler args))
    (error "Unrecognized REPL toplevel command:" command)))

;;
;; Predefined commands
;;

($ toplevel-command-add! 'a
   (^[args]
     (match args
       [() (print "Usage: ,a <word>") *no-value*]
       [(word) `(apropos ',(string->symbol word))])))

($ toplevel-command-add! 'd
   (^[args]
     `(,(with-module gauche.interactive describe)
       ,@(map read-from-string args))))

($ toplevel-command-add! 'pwd
   (^[args]
     (match args
       [() (print (sys-getcwd)) *no-value*]
       [_ (print "Usage: ,pwd") *no-value*])))

($ toplevel-command-add! 'cd
   (^[args]
     (let1 dir (match args
                 [() (home-directory)]
                 [(dir) (expand-path dir)]
                 [_ #f])
       (if dir
         (begin (sys-chdir dir) dir)
         (begin (print "Usage: ,cd [directory]") *no-value*)))))

       
   
    

