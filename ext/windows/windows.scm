;;;
;;; os.windows - Windows-specific procedures
;;;
;;;   Copyright (c) 2010-2022  Shiro Kawai  <shiro@acm.org>
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

(cond-expand [gauche.os.windows

(define-module os.windows
  (export
   ;; Fileapi
   sys-get-disk-free-space-ex

   ;; MessageBox
   MB_ABORTRETRYIGNORE MB_CANCELTRYCONTINUE MB_HELP MB_OK MB_OKCANCEL
   MB_RETRYCANCEL MB_YESNO MB_YESNOCANCEL
   MB_ICONEXCLAMATION MB_ICONWARNING MB_ICONINFORMATION MB_ICONASTERISK
   MB_ICONQUESTION MB_ICONSTOP MB_ICONERROR MB_ICONHAND
   MB_DEFBUTTON1 MB_DEFBUTTON2 MB_DEFBUTTON3 MB_DEFBUTTON4
   MB_APPLMODAL MB_SYSTEMMODAL MB_TASKMODAL
   MB_DEFAULT_DESKTOP_ONLY MB_RIGHT MB_RTLREADING MB_SETFOREGROUND
   MB_TOPMOST MB_SERVICE_NOTIFICATION
   IDABORT IDCANCEL IDCONTINUE IDIGNORE IDNO IDOK IDRETRY IDTRYAGAIN IDYES
   sys-message-box

   ;; Console-related (see the comment in console.stub)
   sys-alloc-console sys-free-console
   CTRL_C_EVENT CTRL_BREAK_EVENT sys-generate-console-ctrl-event
   GENERIC_READ GENERIC_WRITE FILE_SHARE_READ FILE_SHARE_WRITE
   sys-create-console-screen-buffer
   sys-set-console-active-screen-buffer
   sys-scroll-console-screen-buffer
   sys-get-console-cp sys-get-console-output-cp
   sys-set-console-cp sys-set-console-output-cp
   sys-get-console-cursor-info
   sys-set-console-cursor-info
   sys-set-console-cursor-position
   ENABLE_LINE_INPUT ENABLE_ECHO_INPUT ENABLE_PROCESSED_INPUT
   ENABLE_WINDOW_INPUT ENABLE_MOUSE_INPUT ENABLE_PROCESSED_OUTPUT
   ENABLE_WRAP_AT_EOL_OUTPUT
   sys-get-console-mode sys-set-console-mode
   <win:console-screen-buffer-info>
   FOREGROUND_BLUE
   FOREGROUND_GREEN
   FOREGROUND_RED
   FOREGROUND_INTENSITY
   BACKGROUND_BLUE
   BACKGROUND_GREEN
   BACKGROUND_RED
   BACKGROUND_INTENSITY
   sys-get-console-screen-buffer-info
   sys-get-largest-console-window-size
   sys-set-screen-buffer-size
   <win:input-record>
   sys-get-number-of-console-input-events
   sys-get-number-of-console-mouse-buttons
   sys-peek-console-input
   sys-read-console-input
   sys-read-console
   sys-read-console-output
   sys-read-console-output-attribute
   sys-read-console-output-character
   sys-set-console-text-attribute
   sys-set-console-window-info
   sys-write-console
   sys-write-console-output-character
   sys-fill-console-output-character
   sys-fill-console-output-attribute
   sys-flush-console-input-buffer
   sys-get-console-title
   sys-set-console-title
   STD_INPUT_HANDLE STD_OUTPUT_HANDLE STD_ERROR_HANDLE
   sys-get-std-handle sys-set-std-handle

   ;; windows console related
   sys-has-windows-console?
   sys-windows-terminal?
   sys-windows-console-legacy?
   ))
(select-module os.windows)

(dynamic-load "os--windows")

;; check if we have a windows console
;;
;; NB: this procedure returns #f on MSYS (mintty).
;; see %sys-mintty? in src/libsys.scm for details.
;;
(define (sys-has-windows-console?)
  (boolean (or (sys-isatty (standard-input-port))
               (sys-isatty (standard-output-port))
               (sys-isatty (standard-error-port)))))

;; check if running on windows terminal (windows 10)
(define (sys-windows-terminal?)
  (boolean (and (not (sys-getenv "GAUCHE_WINDOWS_TERMINAL_OFF"))
                (sys-getenv "WT_SESSION"))))

;; check if windows console is in legacy mode (windows 10)
(define (sys-windows-console-legacy?)
  (let* ([hout (sys-get-std-handle STD_OUTPUT_HANDLE)]
         [mode (sys-get-console-mode hout)]
         [ret  (guard (e [(<system-error> e) #t])
                 (sys-set-console-mode hout #x0005) ; try VT mode
                 #f)])
    (sys-set-console-mode hout mode) ; restore mode
    ret))

] ; gauche.os.windows
[else
 (errorf "os.windows module is not available on this platform: ~a"
         (gauche-architecture))
])
