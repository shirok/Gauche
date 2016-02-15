;;;
;;; text.console.windows - windows console control
;;;
;;;   Copyright (c) 2015  Hamayama  https://github.com/Hamayama
;;;   Copyright (c) 2016  Shiro Kawai  <shiro@acm.org>
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

;; This is conditionally included from text.console; you don't need
;; to 'use' or 'require' this file.
;; The code is based on the works by Hamayama
;; https://github.com/Hamayama/line-editor-gw

(use os.windows)

;; Class <windows-console> is defined in text.console.
(define-method initialize ((con <windows-console>) initargs)
  (next-method)
  (set! (~ con'ihandle) (sys-get-std-handle STD_INPUT_HANDLE))
  (set! (~ con'ohandle) (sys-get-std-handle STD_OUTPUT_HANDLE)))

(define-method call-with-console ((con <windows-console>) proc)
  (unwind-protect (proc con)
    (reset-terminal con)))

(define *win-virtual-key-table*
  (alist->hash-table
   '([38  . KEY_UP]
     [40  . KEY_DOWN]
     [39  . KEY_RIGHT]
     [37  . KEY_LEFT]
     [35  . KEY_END]
     [36  . KEY_HOME]
     [45  . KEY_INS]
     [46  . KEY_DEL]
     [34  . KEY_PGDN]
     [33  . KEY_PGUP]
     [112 . KEY_F1]
     [113 . KEY_F2]
     [114 . KEY_F3]
     [115 . KEY_F4]
     [116 . KEY_F5]
     [117 . KEY_F6]
     [118 . KEY_F7]
     [119 . KEY_F8]
     [120 . KEY_F9]
     [121 . KEY_F10]
     [122 . KEY_F11]
     [123 . KEY_F12])
   eqv-comparator))

(define-method putch ((con <windows-console>) c)
  (sys-write-console (~ con'ohandle) (string c)))
(define-method putstr ((con <windows-console>) s)
  (sys-write-console (~ con'ohandle) s))

;; some keyboard event constants
(define-constant KEY_EVENT 1)
(define-constant RIGHT_ALT_PRESSED  #x01)
(define-constant LEFT_ALT_PRESSED   #x02)
(define-constant RIGHT_CTRL_PRESSED #x04)
(define-constant LEFT_CTRL_PRESSED  #x08)
(define-constant SHIFT_PRESSED      #x10)
(define-constant ALT_PRESSED (logior RIGHT_ALT_PRESSED LEFT_ALT_PRESSED))
(define-constant CTRL_PRESSED (logior RIGHT_CTRL_PRESSED LEFT_CTRL_PRESSED))

;; Obtain keyboard status
(define (win-keystate hdl)
  (let ([cmode   (sys-get-console-mode hdl)]
        [kslist  (make-queue)])
    (sys-set-console-mode hdl 0)
    (let loop ([irlist (sys-peek-console-input hdl)])
      (unless (null? irlist)
        (sys-read-console-input hdl)
        (dolist [ir irlist]
          (if (= (slot-ref ir 'event-type) KEY_EVENT)
            (let* ([kdown (if (slot-ref ir 'key.down) 1 0)]
                   [ch    (slot-ref ir 'key.unicode-char)]
                   [vk    (slot-ref ir 'key.virtual-key-code)]
                   [ctls  (slot-ref ir 'key.control-key-state)])
              (enqueue! kslist (list kdown ch vk ctls))
              )))
        (loop (sys-peek-console-input hdl))))
    (sys-set-console-mode hdl cmode)
    (dequeue-all! kslist)))

(define (%getch-sub con)
  (define ignorevk '(#x10 ; VK_SHIFT
                     #x11 ; VK_CONTROL
                     #x12 ; VK_MENU
                     #x5b ; VK_LWIN
                     #x5c ; VK_RWIN
                     #x5d ; VK_APPS
                     #xa0 ; VK_LSHIFT
                     #xa1 ; VK_RSHIFT
                     #xa2 ; VK_LCONTROL
                     #xa3 ; VK_RCONTROL
                     #xa4 ; VK_LMENU
                     #xa5 ; VK_RMENU
                     ))
  (define (get-ctrl-char vk)
    (cond [(or (and (>= vk #x41) (<= vk #x5a)) ; #\A-#\Z
               (= vk 32))  ; #\space
           (integer->char (- (logand vk (lognot #x20)) #x40))]
          [else (case vk
                  [(192) #\x00] ; #\@
                  [(219) #\x1b] ; #\[
                  [(220) #\x1c] ; #\\
                  [(221) #\x1d] ; #\]
                  [(222) #\x1e] ; #\^
                  [(226) #\x1f] ; #\_
                  [else  #\x00])]))
  (dolist [ks (win-keystate (~ con'ihandle))]
    (match-let1 (kdown ch vk ctls) ks
      (if (and (= kdown 1) (not (memv vk ignorevk)))
        (cond
         [(hash-table-get *win-virtual-key-table* vk #f)
          => (cut enqueue! (~ con 'keybuf) <>)]
         [(and (logtest ctls ALT_PRESSED) (logtest ctls CTRL_PRESSED))
          (enqueue! (~ con 'keybuf) `(ALT ,(get-ctrl-char vk)))]
         [(logtest ctls ALT_PRESSED)
          (enqueue! (~ con 'keybuf) `(ALT ,(integer->char ch)))]
         [(logtest ctls CTRL_PRESSED)
          (enqueue! (~ con 'keybuf) (get-ctrl-char vk))]
         [else
          (enqueue! (~ con 'keybuf) (integer->char ch))])))))

;; Default - gray foreground, black background
(define *win-default-cattr*
  (logior FOREGROUND_BLUE FOREGROUND_GREEN FOREGROUND_RED))

(define-method getch ((con <windows-console>))
  (while (queue-empty? (~ con 'keybuf))
    (sys-nanosleep #e10e6) ; 10msec
    (%getch-sub con))
  (dequeue! (~ con 'keybuf)))

(define-method chready? ((con <windows-console>))
  (%getch-sub con)
  (not (queue-empty? (~ con 'keybuf))))

(define-method query-cursor-position ((con <windows-console>))
  (let* ([hdl   (~ con'ohandle)]
         [cinfo (sys-get-console-screen-buffer-info hdl)])
    (values (slot-ref cinfo 'cursor-position.y)
            (slot-ref cinfo 'cursor-position.x))))

(define-method move-cursor-to ((con <windows-console>) y x)
  (sys-set-console-cursor-position (~ con'ohandle) x y))

(define-method reset-terminal ((con <windows-console>))
  (reset-character-attribute con)
  (show-cursor con))

(define-method clear-screen ((con <windows-console>))
  (let* ([hdl   (~ con'ohandle)]
         [cinfo (sys-get-console-screen-buffer-info hdl)]
         [bw    (slot-ref cinfo 'size.x)]
         [bh    (slot-ref cinfo 'size.y)]
         [cattr *win-default-cattr*])
    (sys-fill-console-output-attribute hdl cattr   (* bw bh) 0 0)
    (sys-fill-console-output-character hdl #\space (* bw bh) 0 0)
    (sys-set-console-cursor-position hdl 0 0)))

(define-method clear-to-eol ((con <windows-console>))
  (let* ([hdl   (~ con'ohandle)]
         [cinfo (sys-get-console-screen-buffer-info hdl)]
         [x     (slot-ref cinfo'cursor-position.x)]
         [y     (slot-ref cinfo'cursor-position.y)]
         [sbw   (slot-ref cinfo'size.x)])
    (let1 n (- sbw x)
      (sys-fill-console-output-attribute hdl *win-default-cattr* n x y)
      (sys-write-console-output-character hdl (make-string n #\space) x y))))

(define-method clear-to-eos ((con <windows-console>))
  (let* ([hdl   (~ con'ohandle)]
         [cinfo (sys-get-console-screen-buffer-info hdl)]
         [x     (slot-ref cinfo'cursor-position.x)]
         [y     (slot-ref cinfo'cursor-position.y)]
         [sr    (slot-ref cinfo'window.right)]
         [sb    (slot-ref cinfo'window.bottom)]
         [sbw   (slot-ref cinfo'size.x)])
    (let1 n (+ (* (- sb y) sbw) (- x) sr 1)
      (sys-fill-console-output-attribute hdl *win-default-cattr* n x y)
      (sys-write-console-output-character hdl (make-string n #\space) x y))))

(define-method hide-cursor ((con <windows-console>))
  (let1 hdl (~ con'ohandle)
    (receive (sz v) (sys-get-console-cursor-info hdl)
      (sys-set-console-cursor-info hdl sz #f))))

(define-method show-cursor ((con <windows-console>))
  (let1 hdl (~ con'ohandle)
    (receive (sz v) (sys-get-console-cursor-info hdl)
      (sys-set-console-cursor-info hdl sz #t))))

(define-method last-scroll ((con <windows-console>))
  (receive (y x) (query-cursor-position con)
    (let ([sbw (screen-buffer-width)]
          [sbh (screen-buffer-height)])
      (cond
       [(>= y (- sbh 1))
        (display (make-string sbw) (~ con'oport)) (flush (~ con'oport))
        (move-cursor-to con (- sbh 2) x)]))))

(define-method cursor-down/scroll-up ((con <windows-console>))
  (receive (y x) (query-cursor-position con)
    (last-scroll con)
    (move-cursor-to con (+ y 1) x)))

(define-method cursor-up/scroll-down ((con <windows-console>))
  (receive (y x) (query-cursor-position con)
    (move-cursor-to con (- y 1) x)))

(define-method query-screen-size ((con <windows-console>))
  (let* ([hdl   (~ con'ohandle)]
         [cinfo (sys-get-console-screen-buffer-info hdl)])
    (values (+ 1 (- (slot-ref cinfo'window.bottom)
                    (slot-ref cinfo'window.top)))
            (+ 1 (- (slot-ref cinfo'window.right)
                    (slot-ref cinfo'window.left))))))

(define-method set-character-attribute ((con <windows-console>) spec)
  (define B 1)
  (define G 2)
  (define R 4)
  (define I 8) ; intensity
  (define (get-color-code color default-color)
    (case color
      [(black)      0]
      [(red)        R]
      [(green)      G]
      [(yellow)     (logior R G)]
      [(blue)       B]
      [(magenta)    (logior B R)]
      [(cyan)       (logior B G)]
      [(white)      (logior R G B)]
      [else         default-color]))
  (define (get-optional-code opt)
    (case opt
      [(bright)     I]
      [else         0]))  ; underscore not supported
  (define (get-color-attr fc bc)
    (logior (if (logtest fc B) FOREGROUND_BLUE 0)
            (if (logtest fc G) FOREGROUND_GREEN 0)
            (if (logtest fc R) FOREGROUND_RED 0)
            (if (logtest fc I) FOREGROUND_INTENSITY 0)
            (if (logtest bc B) BACKGROUND_BLUE 0)
            (if (logtest bc G) BACKGROUND_GREEN 0)
            (if (logtest bc R) BACKGROUND_RED 0)
            (if (logtest bc I) BACKGROUND_INTENSITY 0)))
  (define hdl (~ con'ohandle))
  (match-let1 (fgcolor bgcolor . opts) spec
    (receive (fg bg) (if (memq 'reverse opts)
                       (values bgcolor fgcolor)
                       (values fgcolor bgcolor))
      (let ([fc (get-color-code fg (logior R G B))]
            [bc (get-color-code bg 0)])
        (dolist [opt opts]
          (set! fc (logior fc (get-optional-code opt))))
        (sys-set-console-text-attribute hdl (get-color-attr fc bc))))))

(define-method reset-character-attribute ((con <windows-console>))
  (sys-set-console-text-attribute (~ con'ohandle) *win-default-cattr*))

(define-method with-character-attribute ((con <windows-console>) attrs thunk)
  (unwind-protect
      (begin
        (set-character-attribute con attrs)
        (thunk))
    (reset-character-attribute con)))

(define-method beep ((con <windows-console>))
  (putch con #\alarm))
