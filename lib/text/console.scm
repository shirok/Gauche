;;;
;;; text.console - basic console control
;;;
;;;   Copyright (c) 2014-2015  Shiro Kawai  <shiro@acm.org>
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

;; Provides primitive full-screen console I/O.
;; This kind of stuff used to be the job of curses/terminfo/termcap, but
;; the world has somewhat moved on.
;; In the lower layer (terminfo), nowadays we pretty much need to handle
;; either vt100 compatible terminal or Windows console; the latter isn't
;; even covered by terminfo.
;; In the upper layer, we only need something like cursor positioning
;; and raw input, and curses is a bit overkill.
;; On the other hand, curses would bring another build-time dependency
;; that differ among platforms; so we decided to avoid it.
;; Let's see how far we can get without involving curses.

;; NB: Most full-screen UI should be done during setting the terminal
;; in "raw" mode.  Such means are provided by gauche.termios module.

(define-module text.console
  (use gauche.generator)
  (use gauche.sequence)
  (use gauche.termios)
  (use data.trie)
  (use data.queue)
  (use util.match)
  (use srfi-42)
  (export <vt100> <windows-console>

          call-with-console
          putch putstr getch get-raw-chars chready? beep
          query-screen-size query-cursor-position move-cursor-to
          hide-cursor show-cursor cursor-down/scroll-up cursor-up/scroll-down
          reset-terminal clear-screen clear-to-eol clear-to-eos
          set-character-attribute reset-character-attribute
          with-character-attribute

          make-default-console))
(select-module text.console)

;; Portable character attributes - can be supported both on
;; vt100 color terminal and on Windows console.
;;
;; (<fgcolor> [<bgcolor> . <option> ...])
;; <fgcolor> : <color> | #f     ; #f means default
;; <bgcolor> : <color> | #f
;; <color>  : black | red | green | yellow | blue | magenta | cyan | white
;; <option> : bright | reverse | underscore

;; Internal character attributes
(define-class <char-attrs> ()
  ((fgcolor :init-keyword :fgcolor :init-value #f)
   (bgcolor :init-keyword :bgcolor :init-value #f)
   (bright  :init-keyword :bright  :init-value #f)
   (reverse :init-keyword :reverse :init-value #f)
   (underscore :init-keyword :underscore :init-value #f)))

(define (parse-char-attrs spec :optional (storage #f))
  (define (check-color color)
    (or (memq color '(#f black red green yellow blue magenta cyan white))
        (error "Invalid color name:" color)))
  (define (check-option opt)
    (or (memq opt '(bright underscore reverse))
        (error "Invalid atttribute option name:" opt)))
  (match spec
    [(fgcolor bgcolor . opts)
     (check-color fgcolor)
     (check-color bgcolor)
     (for-each check-option opts)
     (let ([bright     (boolean (memq 'bright opts))]
           [reverse    (boolean (memq 'reverse opts))]
           [underscore (boolean (memq 'underscore opts))])
       (if storage
         (begin
           (set! (~ storage'fgcolor) fgcolor)
           (set! (~ storage'bgcolor) bgcolor)
           (set! (~ storage'bright) bright)
           (set! (~ storage'reverse) reverse)
           (set! (~ storage'underscore) underscore)
           storage)
         (make <char-attrs> :fgcolor fgcolor :bgcolor bgcolor
               :bright bright :reverse reverse :underscore underscore)))]))

(define (copy-char-attrs attrs)
  (make <char-attrs> :fgcolor (~ attrs'fgcolor) :bgcolor (~ attrs'bgcolor)
        :bright (~ attrs'bright) :reverse (~ attrs'reverse)
        :underscore (~ attrs'underscore)))

;;;
;;; Console class implementation - vt100
;;;

(define-class <vt100> ()
  ((iport :init-keyword :iport :initform (standard-input-port))
   (oport :init-keyword :oport :initform (standard-output-port))
   (input-delay :init-keyword :input-delay :init-value 1000); us, can be #f
   ;; private
   (current-attrs :init-form (make <char-attrs>))
   (input-buffer :init-form (make-queue))
   (screen-size :init-value #f)   ; (width . height)
   ))

(define-method call-with-console ((con <vt100>) proc :key (mode 'rare))
  (with-terminal-mode (~ con'iport) mode
                      (^_ (proc con))
                      (^[]
                        (reset-character-attribute con)
                        (show-cursor con))))

(define-method putch ((con <vt100>) c)
  (display c (~ con'oport)) (flush (~ con'oport)))
(define-method putstr ((con <vt100>) s)
  (display s (~ con'oport)) (flush (~ con'oport)))
(define-method chready? ((con <vt100>))
  (or (not (queue-empty? (~ con'input-buffer)))
      (char-ready? (~ con'iport))))

(define-method beep ((con <vt100>)) (putch con #\alarm))

;; We try to interpret input escape sequences as much as possible.
;; To distinguish from real ESC key and the escape sequence, we time
;; the input---if they comes immediately one after, we take it as a chunk.
;; <vt100>'input-delay sets the timeout.

;; Read a char; returns a char, or #f on timeout.  May return EOF.
(define (%read-char/timeout con)
  (receive (nfds rfds wfds xfds)
      (sys-select! (sys-fdset (~ con'iport)) #f #f (~ con'input-delay))
    (if (= nfds 0)
      #f ; timeout
      (read-char (~ con'iport)))))

(define *input-escape-sequence*
  '([(#\[ #\A)         . KEY_UP]
    [(#\[ #\B)         . KEY_DOWN]
    [(#\[ #\C)         . KEY_RIGHT]
    [(#\[ #\D)         . KEY_LEFT]
    [(#\O #\A)         . KEY_UP]  ; variation
    [(#\O #\B)         . KEY_DOWN]
    [(#\O #\C)         . KEY_RIGHT]
    [(#\O #\D)         . KEY_LEFT]
    [(#\[ #\F)         . KEY_END]
    [(#\[ #\H)         . KEY_HOME]
    [(#\[ #\2 #\~)     . KEY_INS]
    [(#\[ #\3 #\~)     . KEY_DEL]
    [(#\[ #\5 #\~)     . KEY_PGDN]
    [(#\[ #\6 #\~)     . KEY_PGUP]
    [(#\O #\P)         . KEY_F1]  ; original vt100
    [(#\O #\Q)         . KEY_F2]
    [(#\O #\R)         . KEY_F3]
    [(#\O #\S)         . KEY_F4]
    [(#\[ #\1 #\1 #\~) . KEY_F1]  ; some version of xterm
    [(#\[ #\1 #\2 #\~) . KEY_F2]
    [(#\[ #\1 #\3 #\~) . KEY_F3]
    [(#\[ #\1 #\4 #\~) . KEY_F4]
    [(#\[ #\1 #\5 #\~) . KEY_F5]
    [(#\[ #\1 #\7 #\~) . KEY_F6]  ; vt220
    [(#\[ #\1 #\8 #\~) . KEY_F7]
    [(#\[ #\1 #\9 #\~) . KEY_F8]
    [(#\[ #\2 #\0 #\~) . KEY_F9]
    [(#\[ #\2 #\1 #\~) . KEY_F10]
    [(#\[ #\2 #\3 #\~) . KEY_F11]
    [(#\[ #\2 #\4 #\~) . KEY_F12]
    ))

(define *input-escape-sequence-trie*
  (delay (rlet1 t (apply trie '() *input-escape-sequence*)
            (do-ec (: n 128)
                  (trie-put! t `(,(integer->char n))
                             `(ALT ,(integer->char n)))))))

(define-method getch ((con <vt100>))
  (define tab (force *input-escape-sequence-trie*))
  (define (fetch q)
    (let1 ch (%read-char/timeout con)
      (if (char? ch)
        (begin (enqueue! q ch)
               (if (trie-partial-key? tab (queue-internal-list q))
                 (fetch q)
                 (finish q)))
        (finish q))))
  (define (finish q)
    (if (trie-exists? tab (queue-internal-list q))
      (trie-get tab (dequeue-all! q))
      #\escape))
  (let1 q (~ con'input-buffer)
    (if (queue-empty? q)
      (let1 ch (read-char (~ con'iport))
        (if (eqv? ch #\escape)
          (fetch q)
          ch))
      (dequeue! q))))

(define-method get-raw-chars ((con <vt100>))  ; no translation
  (let1 q (~ con'input-buffer)
    (when (queue-empty? q) (enqueue! q (read-char (~ con'iport))))
    (let loop ()
      (let1 ch (%read-char/timeout con)
        (if (char? ch)
          (begin (enqueue! q ch) (loop))
          (dequeue-all! q))))))

(define-method query-cursor-position ((con <vt100>))
  (define (r) (read-char (~ con'iport))) ; we bypass getch buffering
  (define q (~ con'input-buffer))
  (putstr con "\x1b;[6n")
  (until (r) (cut eqv? <> #\x1b) => ch (enqueue! q ch))
  (unless (eqv? #\[   (r)) (error "terminal error"))
  (rxmatch-case ($ list->string $ generator->list
                   $ gtake-while (^c (not (eqv? #\R c))) r)
    [#/^(\d+)\;(\d+)$/ (_ row col)
     (values (- (x->integer row) 1) (- (x->integer col) 1))]
    [else (error "terminal error")]))

;; we're zero-origin as curses; vt100 takes 1-origin.
(define-method move-cursor-to ((con <vt100>) y x)
  (putstr con (format "\x1b;[~d;~dH" (+ y 1) (+ x 1))))

(define-method reset-terminal ((con <vt100>)) (putstr con "\x1b;c"))
(define-method clear-screen ((con <vt100>)) (putstr con "\x1b;[2J"))
(define-method clear-to-eol ((con <vt100>)) (putstr con "\x1b;[K"))
(define-method clear-to-eos ((con <vt100>)) (putstr con "\x1b;[J"))

(define-method hide-cursor ((con <vt100>)) (putstr con "\x1b;[?25l"))
(define-method show-cursor ((con <vt100>)) (putstr con "\x1b;[?25h"))

;; Move cursor; if cursor is already top or bottom, scroll.
(define-method cursor-down/scroll-up ((con <vt100>))
  (putstr con "\x1b;D"))
(define-method cursor-up/scroll-down ((con <vt100>))
  (putstr con "\x1b;M"))

;; No portable way to directly query it, so we take a kind of heuristic approach.
(define-method query-screen-size ((con <vt100>))
  (define *max-dim* 2000)
  (putstr con "\x1b;[s") ; save cursor pos
  (move-cursor-to con *max-dim* *max-dim*)
  (unwind-protect (receive (h w) (query-cursor-position con)
                    (values (+ h 1) (+ w 1)))
    (putstr con "\x1b;[u"))) ;restore cursor pos

(define-method set-character-attribute ((con <vt100>) spec)
  (define (color->n color)
    (or (find-index (cut eq? <> color)
                    '(black red green yellow blue magenta cyan white))
        9)) ; default
  (let* ([chattr (parse-char-attrs spec (~ con'current-attrs))]
         [opts (cond-list [(~ chattr'bright) 1]
                          [(~ chattr'underscore) 4]
                          [(~ chattr'reverse) 7])]
         [fgcolor (+ (color->n (~ chattr'fgcolor)) 30)]
         [bgcolor (+ (color->n (~ chattr'bgcolor)) 40)]
         [nlist `(0 ,fgcolor ,bgcolor ,@opts)])
    ($ putstr con
       $ format "\x1b;[~am"
       $ string-join (map number->string nlist) ";")))

(define-method reset-character-attribute ((con <vt100>))
  (putstr con "\x1b;[0m"))

;; NB: This should actually save the attr and restores it, so that
;; we can nest this.  However, somehow I can't make "Save/restore cursor&attrs"
;; sequences (ESC 7 and ESC 8) on xterm.  For the time being, don't nest this.
(define-method with-character-attribute ((con <vt100>) attrs thunk)
  (unwind-protect
      (begin
        (set-character-attribute con attrs)
        (thunk))
    (reset-character-attribute con)))

;;;
;;; Console class implementation - windows cosole
;;;

;; This part incorporates works by Hamayama
;; https://github.com/Hamayama/line-editor-gw

;; We define <windows-console> class unconditionally so that
;; the user doesn't need to use cond-expand (instead they may
;; use ordinary conditionals).
(define-class <windows-console> ()
  ((oport :init-keyword :oport :init-form (standard-output-port))
   (keybuf :init-form (make-queue))))

;; The actual method definitions depend on os.windows
(cond-expand
  [gauche.os.windows
   (use os.windows)

(define-method call-with-console ((con <windows-console>) proc)
  (unwind-protect
      (proc con)
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
  (display c (~ con'oport)) (flush (~ con'oport)))
(define-method putstr ((con <windows-console>) s)
  (display s (~ con'oport)) (flush (~ con'oport)))

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
  (let* ([hdl     (sys-get-std-handle STD_INPUT_HANDLE)]
         [cmode   (sys-get-console-mode hdl)]
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

;; return (keydown unicode-char virtual-key-code control-key-state shift ctrl alt)
;; 

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
  (dolist [ks (win-keystate)]
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
  (if (not (queue-empty? (~ con 'keybuf))) #t #f))

(define-method query-cursor-position ((con <windows-console>))
  (let* ([hdl   (sys-get-std-handle STD_OUTPUT_HANDLE)]
         [cinfo (sys-get-console-screen-buffer-info hdl)])
    (values (slot-ref cinfo 'cursor-position.y)
            (slot-ref cinfo 'cursor-position.x))))

(define-method move-cursor-to ((con <windows-console>) y x)
  (let1 hdl (sys-get-std-handle STD_OUTPUT_HANDLE)
    (sys-set-console-cursor-position hdl x y)))

(define-method reset-terminal ((con <windows-console>))
  (reset-character-attribute con)
  (show-cursor con))

(define-method clear-screen ((con <windows-console>))
  (let* ([hdl   (sys-get-std-handle STD_OUTPUT_HANDLE)]
         [cinfo (sys-get-console-screen-buffer-info hdl)]
         [bw    (slot-ref cinfo 'size.x)]
         [bh    (slot-ref cinfo 'size.y)]
         [cattr *win-default-cattr*])
    (sys-fill-console-output-attribute hdl cattr   (* bw bh) 0 0)
    (sys-fill-console-output-character hdl #\space (* bw bh) 0 0)
    (sys-set-console-cursor-position hdl 0 0)))

(define-method clear-to-eol ((con <windows-console>))
  (let* ([hdl   (sys-get-std-handle STD_OUTPUT_HANDLE)]
         [cinfo (sys-get-console-screen-buffer-info hdl)]
         [x     (slot-ref cinfo'cursor-position.x)]
         [y     (slot-ref cinfo'cursor-position.y)]
         [sbw   (slot-ref cinfo'size.x)])
    (let1 n (- sbw x)
      (sys-fill-console-output-attribute hdl n *win-default-cattr* x y)
      (sys-write-console-output-character hdl (make-string n #\space) x y))))

(define-method clear-to-eos ((con <windows-console>))
  (let* ([hdl   (sys-get-std-handle STD_OUTPUT_HANDLE)]
         [cinfo (sys-get-console-screen-buffer-info hdl)]
         [x     (slot-ref cinfo'cursor-position.x)]
         [y     (slot-ref cinfo'cursor-position.y)]
         [sr    (slot-ref cinfo'window.right)]
         [sb    (slot-ref cinfo'window.bottom)]
         [sbw   (slot-ref cinfo'size.x)])
    (let1 n (+ (* (- sb y) sbw) (- x) sr 1)
      (sys-fill-console-output-attribute hdl n *win-default-cattr* x y)
      (sys-write-console-output-character hdl (make-string n #\space) x y))))

(define-method hide-cursor ((con <windows-console>))
  (let1 hdl (sys-get-std-handle STD_OUTPUT_HANDLE)
    (receive (sz v) (sys-get-console-cursor-info hdl)
      (sys-set-console-cursor-info hdl sz #f))))

(define-method show-cursor ((con <windows-console>))
  (let1 hdl (sys-get-std-handle STD_OUTPUT_HANDLE)
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
  (let* ([hdl   (sys-get-std-handle STD_OUTPUT_HANDLE)]
         [cinfo (sys-get-console-screen-buffer-info hdl)])
    (values (+ 1 (- (slot-ref cinfo'window.bottom)
                    (slot-ref cinfo'window.top)))
            (+ 1 (- (slot-ref cinfo'window.left)
                    (slot-ref cinfo'window.right))))))

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
      [else         0]))  ; reverse and underscore not supported
  (define (get-color-attr fc bc)
    (logior (if (logtest fc B) FOREGROUND_BLUE 0)
            (if (logtest fc G) FOREGROUND_GREEN 0)
            (if (logtest fc R) FOREGROUND_RED 0)
            (if (logtest fc I) FOREGROUND_INTENSITY 0)
            (if (logtest bc B) BACKGROUND_BLUE 0)
            (if (logtest bc G) BACKGROUND_GREEN 0)
            (if (logtest bc R) BACKGROUND_RED 0)
            (if (logtest bc I) BACKGROUND_INTENSITY 0)))
  (define hdl (sys-get-std-handle STD_OUTPUT_HANDLE))
  (match-let1 (fgcolor bgcolor . opts) spec
    (let ([fc (get-color-code fgcolor (logior R G B))]
          [bc (get-color-code bgcolor (logior R G B I))])
      (dolist [opt opts]
        (set! fc (logior fc (get-optional-code opt))))
      (sys-set-console-text-attribute hdl (get-color-attr fc bc)))))

(define-method reset-character-attribute ((con <windows-console>))
  (sys-set-console-text-attribute (sys-get-std-handle STD_OUTPUT_HANDLE)
                                  *win-default-cattr*))

(define-method with-character-attribute ((con <windows-console>) attrs thunk)
  (unwind-protect
      (begin
        (set-character-attribute con attrs)
        (thunk))
    (reset-character-attribute con)))

(define-method beep ((con <windows-console>))
  (putch con #\alarm))

] ; end of gauche.os.windows
[else])


;;;
;;; Select appropriate console
;;;

;; Convenience API
;; Inspect the runtime environment and returns a console object with
;; appropriate setting.
;; If it cannot find supported console type, an error is signalled,
;; with a message describing why.
;; We use some heuristics to recognize vt100 compatible terminals.
(define (make-default-console)
  (cond [(and-let1 t (sys-getenv "TERM")
           (any (cut <> t) '(#/^vt10[02]$/ #/^vt220$/ #/^xterm.*/ #/^rxvt$/)))
         (make <vt100>)]
        [(has-windows-console?) (make <windows-console>)]
        [(sys-getenv "TERM")
         => (^t (error #"Unsupported terminal type: ~t"))]
        [else
         (error "TERM isn't set and don't know how to control the terminal.")]))

(cond-expand
 [gauche.os.windows
  ;; Heuristics - check if we have a console, and it's not MSYS one.
  (define (has-windows-console?)
    (and (guard (e [else #f])
           (sys-get-console-title))
         (not (sys-getenv "MSYSCON"))))]
 [else
  (define (has-windows-console?) #f)])
         
  
