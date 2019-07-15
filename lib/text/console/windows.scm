;;;
;;; text.console.windows - windows console control
;;;
;;;   Copyright (c) 2015  Hamayama  https://github.com/Hamayama
;;;   Copyright (c) 2016-2019  Shiro Kawai  <shiro@acm.org>
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

;; These handles should not be cached.
(define (get-ihandle) (sys-get-std-handle STD_INPUT_HANDLE))
(define (get-ohandle) (sys-get-std-handle STD_OUTPUT_HANDLE))

;; Class <windows-console> is defined in text.console.
(define-method initialize ((con <windows-console>) initargs)
  (next-method)
  (set! (~ con'high-surrogate) 0))

(define-method call-with-console ((con <windows-console>) proc
                                  :allow-other-keys)
  (unwind-protect (proc con)
    (reset-character-attribute con)
    (show-cursor con)))

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
  (sys-write-console (get-ohandle) (string c)))
(define-method putstr ((con <windows-console>) s)
  (sys-write-console (get-ohandle) s))

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
(define (win-keystate)
  (let ([hdl    (get-ihandle)]
        [kslist (make-queue)])
    (let loop ([irlist (sys-peek-console-input hdl)])
      (unless (null? irlist)
        (sys-read-console-input hdl)
        (dolist [ir irlist]
          (if (= (slot-ref ir'event-type) KEY_EVENT)
            (let* ([kdown (if (slot-ref ir'key.down) 1 0)]
                   [ch    (slot-ref ir'key.unicode-char)]
                   [vk    (slot-ref ir'key.virtual-key-code)]
                   [ctls  (slot-ref ir'key.control-key-state)])
              (enqueue! kslist (list kdown ch vk ctls))
              )))
        (loop (sys-peek-console-input hdl))))
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
    (cond [(<= #x41 vk #x5a) ; #\A-#\Z
           (integer->char (- (logand vk (lognot #x20)) #x40))]
          [else (case vk
                  [(192) #\x00] ; #\@
                  [(219) #\x1b] ; #\[
                  [(220) #\x1c] ; #\\
                  [(221) #\x1d] ; #\]
                  [(222) #\x1e] ; #\^
                  [(226) #\x1f] ; #\_
                  [else  #\x00])]))
  (define (enqueue-keybuffer ch vk ctls)
    (cond
     [(hash-table-get *win-virtual-key-table* vk #f)
      => (cut enqueue! (~ con'keybuf) <>)]
     [(and (logtest ctls ALT_PRESSED) (logtest ctls CTRL_PRESSED))
      (enqueue! (~ con'keybuf) `(ALT ,(get-ctrl-char vk)))]
     [(logtest ctls ALT_PRESSED)
      (enqueue! (~ con'keybuf) `(ALT ,(integer->char ch)))]
     [(logtest ctls CTRL_PRESSED)
      (enqueue! (~ con'keybuf) (get-ctrl-char vk))]
     [(eqv? ch #x0a)] ; drop a newline character
     [(eqv? ch #x0d)  ; convert a return character to a newline character
      (enqueue! (~ con'keybuf) #\x0a)]
     [else
      (enqueue! (~ con'keybuf) (integer->char ch))]))
  (dolist [ks (win-keystate)]
    (match-let1 (kdown ch vk ctls) ks
      (if (and (= kdown 1) (not (memv vk ignorevk)))
        (cond-expand
         [gauche.ces.utf8
          ;; process a surrogate pair
          (case (logand ch #xfc00)
            [(#xd800) ; high surrogate
             (set! (~ con'high-surrogate) ch)]
            [(#xdc00) ; low surrogate
             (unless (= (~ con'high-surrogate) 0)
               (set! ch (+ #x10000
                           (* (- (~ con'high-surrogate) #xd800) #x400)
                           (- ch #xdc00)))
               (enqueue-keybuffer ch vk ctls)
               (set! (~ con'high-surrogate) 0))]
            [else
             (enqueue-keybuffer ch vk ctls)
             (set! (~ con'high-surrogate) 0)])]
         [else
          (enqueue-keybuffer ch vk ctls)])
        ))))

;; Default - gray foreground, black background
(define *win-default-cattr*
  (logior FOREGROUND_BLUE FOREGROUND_GREEN FOREGROUND_RED))

;; Get a char; returns a char, or #f on timeout.
;; The timeout argument is in us.
(define-method getch ((con <windows-console>) :optional (timeout #f))
  ;; Windows timer has rather coarse resolution (10ms)
  (define resolution-us 10000)
  (let loop ([t 0])
    (%getch-sub con)
    (if (not (queue-empty? (~ con'keybuf)))
      (dequeue! (~ con'keybuf))
      (if (and timeout (>= t timeout))
        #f ; timeout
        (begin (sys-nanosleep (* resolution-us 1000))
               (if timeout
                 (loop (+ t resolution-us))
                 (loop 0)))))))

(define-method get-raw-chars ((con <windows-console>))
  ;; Windows timer has rather coarse resolution (10ms)
  (define resolution-us 10000)
  (define q (make-queue))
  (while (queue-empty? q)
    (sys-nanosleep (* resolution-us 1000))
    (dolist [ks (win-keystate)]
      (match-let1 (kdown ch vk ctls) ks
        (when (= kdown 1)
          (enqueue! q (list (integer->char ch) vk (logand ctls #x1f)))))))
  (dequeue-all! q))

(define-method chready? ((con <windows-console>))
  (%getch-sub con)
  (not (queue-empty? (~ con'keybuf))))

(define-method query-cursor-position ((con <windows-console>))
  (let1 cinfo (sys-get-console-screen-buffer-info (get-ohandle))
    (values (slot-ref cinfo'cursor-position.y)
            (slot-ref cinfo'cursor-position.x))))

(define-method move-cursor-to ((con <windows-console>) y x)
  (sys-set-console-cursor-position (get-ohandle) x y))

(define-method reset-terminal ((con <windows-console>))
  (clear-screen con)
  (reset-character-attribute con)
  (show-cursor con))

(define-method clear-screen ((con <windows-console>))
  (let* ([hdl   (get-ohandle)]
         [cinfo (sys-get-console-screen-buffer-info hdl)]
         [sbw   (slot-ref cinfo'size.x)]
         [sbh   (slot-ref cinfo'size.y)])
    (let1 n (* sbw sbh)
      (sys-fill-console-output-attribute hdl *win-default-cattr* n 0 0)
      (sys-fill-console-output-character hdl #\space n 0 0))
    (sys-set-console-cursor-position hdl 0 0)))

(define-method clear-to-eol ((con <windows-console>))
  (let* ([hdl   (get-ohandle)]
         [cinfo (sys-get-console-screen-buffer-info hdl)]
         [x     (slot-ref cinfo'cursor-position.x)]
         [y     (slot-ref cinfo'cursor-position.y)]
         [sbw   (slot-ref cinfo'size.x)])
    (let1 n (- sbw x)
      (sys-fill-console-output-attribute hdl *win-default-cattr* n x y)
      (sys-fill-console-output-character hdl #\space n x y))))

(define-method clear-to-eos ((con <windows-console>))
  (let* ([hdl   (get-ohandle)]
         [cinfo (sys-get-console-screen-buffer-info hdl)]
         [x     (slot-ref cinfo'cursor-position.x)]
         [y     (slot-ref cinfo'cursor-position.y)]
         [sbw   (slot-ref cinfo'size.x)]
         [sbh   (slot-ref cinfo'size.y)])
    (let1 n (* sbw (- sbh y))
      (sys-fill-console-output-attribute hdl *win-default-cattr* n x y)
      (sys-fill-console-output-character hdl #\space n x y))))

(define-method hide-cursor ((con <windows-console>))
  (let1 hdl (get-ohandle)
    (receive (sz v) (sys-get-console-cursor-info hdl)
      (sys-set-console-cursor-info hdl sz #f))))

(define-method show-cursor ((con <windows-console>))
  (let1 hdl (get-ohandle)
    (receive (sz v) (sys-get-console-cursor-info hdl)
      (sys-set-console-cursor-info hdl sz #t))))

;; If the cursor is on the last line, scroll up and make a room
;; at the bottom.  This is to workaround windows IME bug:
;; When windows ime is on, writing a newline character
;; to the last line causes a system error.
(define-method ensure-bottom-room ((con <windows-console>)
                                   :optional (full-column-flag #f))
  (let* ([hdl   (get-ohandle)]
         [cinfo (sys-get-console-screen-buffer-info hdl)]
         [sbw   (slot-ref cinfo'size.x)]
         [sbh   (slot-ref cinfo'size.y)])
    (receive (y1 x1) (query-cursor-position con)
      (when (>= y1 (- sbh 1))
        (guard (e [(<system-error> e)
                   ;; When windows ime is on, a full column wrapping
                   ;; causes one more line scroll-up.
                   ;; So we don't write a newline character in this case.
                   (if (not full-column-flag)
                     ;; When windows ime is on, the space character
                     ;; before a newline character is important
                     ;; in order to avoid a system error.
                     (sys-write-console hdl " \n"))])
          (sys-write-console hdl "\n"))
        (receive (y2 x2) (query-cursor-position con)
          (move-cursor-to con (- y2 1) x1))
        ))))

(define-method cursor-down/scroll-up ((con <windows-console>)
                                      :optional (y #f) (height #f)
                                      (full-column-flag #f))
  ;; When windows ime is on, a full column wrapping
  ;; causes one more line scroll-up.
  ;; So we must deal with this problem.
  (ensure-bottom-room con full-column-flag)

  ;; move cursor to the next line
  (receive (y1 x1) (query-cursor-position con)
    (move-cursor-to con (+ y1 1) x1))

  ;; We have to make a room on the last line of console,
  ;; because windows ime overwrites the last line and causes
  ;; a system error.
  (ensure-bottom-room con full-column-flag)

  ;; return the difference of the cursor position y
  (receive (y2 x2) (query-cursor-position con)
    (if y (- y2 y) 1)))

(define-method cursor-up/scroll-down ((con <windows-console>)
                                      :optional (y #f))
  ;; move cursor to the previous line
  (receive (y1 x1) (query-cursor-position con)
    (move-cursor-to con (max (- y1 1) 0) x1))

  ;; return the difference of the cursor position y
  (if (and y (<= y 0)) 0 -1))

(define-method query-screen-size ((con <windows-console>))
  (let1 cinfo (sys-get-console-screen-buffer-info (get-ohandle))
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
  (match-let1 (fgcolor bgcolor . opts) spec
    (let ([fc (get-color-code fgcolor (logior R G B))]
          [bc (get-color-code bgcolor 0)])
      (dolist [opt opts]
        (set! fc (logior fc (get-optional-code opt))))
      (if (memq 'reverse opts)
        (set!-values (fc bc) (values bc fc)))
      (sys-set-console-text-attribute (get-ohandle)
                                      (get-color-attr fc bc)))))

(define-method reset-character-attribute ((con <windows-console>))
  (sys-set-console-text-attribute (get-ohandle) *win-default-cattr*))

(define-method with-character-attribute ((con <windows-console>)
                                         attrs thunk)
  (unwind-protect
      (begin
        (set-character-attribute con attrs)
        (thunk))
    (reset-character-attribute con)))

(define-method beep ((con <windows-console>))
  (putch con #\alarm))
