;;;
;;; text.pager - display text using appropriate pager
;;;
;;;   Copyright (c) 2020  Shiro Kawai  <shiro@acm.org>
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

;; This module uses some heuristics to present a long text nicely to the user

(define-module text.pager
  (use gauche.process)
  (use gauche.parameter)
  (use gauche.termios)
  (use file.util)
  (export display/pager
          pager-program))
(select-module text.pager)

(define *default-pager*
  (or (and-let1 s (sys-getenv "PAGER")
        (shell-tokenize-string s))
      (and-let1 cmd (or (find-file-in-paths "less")
                        (find-file-in-paths "more"))
        (list cmd))))

;; Parameter: pager-program
;; pager program and arguments.
;; can be #f.
(define pager-program (make-parameter *default-pager*))

;; This is to display info document safely on ascii-only output.  We can
;; throw in more heuristics as needed.  The goal isn't a perfect solution,
;; but something that works most of the time.
(define (unicode->ascii s)
  ($ regexp-replace-all* s
     #/\u21d2/ "==>"      ; @result{}
     #/\u2026/ "..."      ; @dots{}
     #/\u2018/ "`"        ; @code{}
     #/\u2019/ "'"        ; @code{}
     #/\u201C/ "``"       ; ``         (e.g. ,i do)
     #/\u201D/ "''"       ; ''         (e.g. ,i do)
     #/\u2261/ "=="       ; @equiv{}   (e.g. ,i cut)
     #/\u2212/ "-"        ; @minus     (e.g. ,i modulo)
     #/\u2022/ "*"        ; @bullet    (e.g. ,i lambda)
     #/\u2013/ "--"       ; --         (e.g. ,i utf8-length)
     #/\u2014/ "---"      ; ---        (e.g. ,i lambda)
     #/\u00df/ "[Eszett]" ; eszett     (e.g. ,i char-upcase)
     ))

(define (pager-unavailable?)
  (or (equal? (sys-getenv "TERM") "emacs")
      (equal? (sys-getenv "TERM") "dumb")
      (not (sys-isatty (current-output-port)))
      (not (pager-program))))

(define (limited-output?)
  (cond-expand
   [(and gauche.os.windows
         (not gauche.ces.none))
    (use os.windows)
    (guard (e [else #f])
      (and (sys-isatty 1)
           (sys-get-console-mode 
            (sys-get-std-handle STD_OUTPUT_HANDLE))
           (not (= (sys-get-console-output-cp) 65001))))] ;unicode cp
   [else #f]))

(define (pager-filter s)
  (if (limited-output?)
    (unicode->ascii s)
    s))

(define (run-pager s)
  ;; We need to ensure the terminal mode is restored even the pager process
  ;; died unexpectedly.
  (with-terminal-mode
   (standard-output-port) 'cooked
   (^_
    ;; During running the pager, we block SIGINT and let the pager handle
    ;; (or ignore) it, otherwise the terminal would be taken with both the
    ;; pager and gosh process and cause a mess.  Note that the sent SIGINT
    ;; will be handled as soon as we restore the sigmask.
    (let* ([omask (sys-sigmask SIG_BLOCK (sys-sigset SIGINT))]
           [p (run-process (pager-program) :input :pipe :sigmask (sys-sigset))])
      (unwind-protect
          (display s (process-input p))
        (close-output-port (process-input p))
        (process-wait p)
        (sys-sigmask SIG_SETMASK omask))))))

;; API
(define (display/pager s)
  (if (pager-unavailable?)
    (display (pager-filter s))
    (run-pager (pager-filter s))))

