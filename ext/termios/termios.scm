;;;
;;; termios - termios interface
;;;
;;;   Copyright (c) 2000-2013  Shiro Kawai  <shiro@acm.org>
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

(define-module gauche.termios
  (use gauche.uvector)
  (export-all)
  )
(select-module gauche.termios)

(dynamic-load "gauche--termios")

(cond-expand
 [gauche.os.windows (use os.windows)]
 [else])

;;
;; High-level utilities
;;

;; NB: on windows, this only works with iport==#f.
(define (without-echoing iport proc)
  (cond [(not iport) ;; open tty
         (call-with-input-file
             (cond-expand [gauche.os.windows "CON"] [else "/dev/tty"])
           (cut without-echoing <> proc))]
        [(sys-isatty iport)
         (let ()
           (cond-expand
            [gauche.os.windows
             (define ihandle (sys-get-std-handle STD_INPUT_HANDLE))
             (define orig-mode (sys-get-console-mode ihandle))
             (define (echo-off)
               (sys-set-console-mode ihandle
                                     (logand orig-mode
                                             (lognot ENABLE_ECHO_INPUT))))
             (define (echo-on)
               (sys-set-console-mode ihandle orig-mode))]
            [else
             (begin
               (define attr (sys-tcgetattr iport))
               (define lflag-save (ref attr'lflag))
               (define (echo-off)
                 (set! (ref attr'lflag)
                       (logand (ref attr'lflag)
                               (lognot (logior ECHO ICANON ISIG))))
                 (sys-tcsetattr iport TCSANOW attr))
               (define (echo-on)
                 (set! (ref attr'lflag) lflag-save)
                 (sys-tcsetattr iport TCSANOW attr)))])
           (unwind-protect (begin (echo-off) (proc iport)) (echo-on)))]
        [else (proc iport)]))

#|
;; sample
(define (get-password)
  (with-output-to-file
      (cond-expand [gauche.os.windows "CON"] [else "/dev/tty"])
    (lambda () (display "Password: ") (flush)))
  (without-echoing #f read-line))
|#




