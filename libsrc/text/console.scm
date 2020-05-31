;;;
;;; text.console - basic console control
;;;
;;;   Copyright (c) 2014-2019  Shiro Kawai  <shiro@acm.org>
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

(define-module text.console
  (use gauche.termios)
  (export <vt100> <windows-console>

          call-with-console
          putch putstr getch get-raw-chars chready? beep
          query-screen-size query-cursor-position move-cursor-to
          hide-cursor show-cursor cursor-down/scroll-up cursor-up/scroll-down
          reset-terminal clear-screen clear-to-eol clear-to-eos
          set-character-attribute reset-character-attribute
          with-character-attribute
          ensure-bottom-room

          make-default-console vt100-compatible?))
(select-module text.console)

;; If make-default-console returns #f, it doesn't make sense to load these.
(autoload text.console.generic
          <vt100> <windows-console>
          call-with-console
          putch putstr getch get-raw-chars chready? beep
          query-screen-size query-cursor-position move-cursor-to
          hide-cursor show-cursor cursor-down/scroll-up cursor-up/scroll-down
          reset-terminal clear-screen clear-to-eol clear-to-eos
          set-character-attribute reset-character-attribute
          with-character-attribute
          ensure-bottom-room)

;;;
;;; Select appropriate console
;;;

;; Regexp that matches $TERM that behaves as if it's vt100.
;; Strict compatibility isn't necessary; we're using just a common
;; denominator.  We expand this list as we discover other terminal types
;; that works.
;; See https://github.com/shirok/Gauche/issues/179 about 'screen'.
(define-constant *vt100-compatible-terminals*
  #/^(vt10[02]|vt220|xterm.*|rxvt.*|screen.*)$/)

;; Convenience API
(define (vt100-compatible? term)
  (boolean (*vt100-compatible-terminals* term)))

;; Convenience API
;; Inspect the runtime environment and returns a console object with
;; appropriate setting.
;; If it cannot find supported console type, an error is signalled,
;; with a message describing why.
;; We use some heuristics to recognize vt100 compatible terminals.
(define (make-default-console :key (if-not-available :error))
  (define (e s)
    (if (eq? if-not-available :error)
      (error s)
      #f))
  (cond [(not (memv if-not-available '(#f :error)))
         (error "if-not-available argument must be either #f or :error, \
                 but got:" if-not-available)]
        [(has-windows-console?) (make <windows-console>)]
        [(and-let1 t (sys-getenv "TERM")
           (vt100-compatible? t))
         (if (or (sys-isatty 0)
                 ((with-module gauche.internal %sys-mintty?) 0))
           (make <vt100>)
           #f)]
        [(sys-getenv "TERM")
         => (^t (e #"Unsupported terminal type: ~t"))]
        [else
         (e "TERM isn't set and we don't know how to control the terminal.")]))
