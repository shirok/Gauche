;;;
;;; text.console.generic - basic console control (autoloaded)
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

;; The user must (use text.console).
;; This file is splitted to be autoloaded so that it won't be loaded
;; if console control isn't available.

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

(define-module text.console.generic
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
          ensure-bottom-room))
(select-module text.console.generic)

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
;; The timeout argument is in us.
(define (%read-char/timeout con :optional (timeout (~ con'input-delay)))
  (cond-expand
   [gauche.os.windows
    ;; This code is used with mintty on MSYS
    ;; Windows timer has rather coarse resolution (10ms)
    (define resolution-us 10000)
    (if (or (char-ready? (~ con'iport)) (not timeout))
      (read-char (~ con'iport))
      (let loop ([t 0])
        (if (>= t timeout)
          #f ; timeout
          (begin (sys-nanosleep (* resolution-us 1000))
                 (if (char-ready? (~ con'iport))
                   (read-char (~ con'iport))
                   (loop (+ t resolution-us)))))))]
   [else
    (receive (nfds rfds wfds xfds)
        (sys-select! (sys-fdset (~ con'iport)) #f #f timeout)
      (if (= nfds 0)
        #f ; timeout
        (read-char (~ con'iport))))]))

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

;; Get a char; returns a char, or #f on timeout.
;; The timeout argument is in us.
(define-method getch ((con <vt100>) :optional (timeout #f))
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
      (begin (dequeue-all! q) #\escape)))
  (let1 q (~ con'input-buffer)
    (if (queue-empty? q)
      (let1 ch (if timeout
                 (%read-char/timeout con timeout)
                 (read-char (~ con'iport)))
        (cond [(eqv? ch #\escape) (fetch q)]
              [(not ch) #f]             ;timeout
              [else ch]))
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
  ;; Read an esc seq terminted by #\R and return it as a char list
  ;; (excluding leading ESC).  Buffer any other seq in the console's
  ;; input buffer.
  (define (get-ESC-R)
    (let loop ([ch (r)] [chars '()])
      (cond [(eqv? ch #\escape)
             ;; buffer previous escape sequence
             (apply enqueue! (~ con'input-buffer) #\escape (reverse chars))
             (loop (r) '())]
            [(eqv? ch #\R) ; we got it
             (reverse (cons ch chars))]
            [(char? ch) (loop (r) (cons ch chars))]
            [else
             (apply enqueue! (~ con'input-buffer) #\escape (reverse chars))
             (e)])))
  (define (e) (error "terminal error when receiving cursor position"))
    
  (putstr con "\x1b;[6n")
  (until (r) (cut eqv? <> #\x1b) => ch (enqueue! (~ con'input-buffer) ch))
  (rxmatch-case (list->string (get-ESC-R))
    [#/^\[(\d+)\;(\d+)R$/ (_ row col)
     (values (- (x->integer row) 1) (- (x->integer col) 1))]
    [else (e)]))

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
;; If optional arguments are specified, these methods return
;; the difference of the cursor position y.
;; NB: full-column-flag is a dummy argument for compatibility with
;; <windows-console>.
(define-method cursor-down/scroll-up ((con <vt100>)
                                      :optional (y #f) (height #f)
                                      (full-column-flag #f))
  (putstr con "\x1b;D")
  (if (and y height (>= y (- height 1))) 0 1))
(define-method cursor-up/scroll-down ((con <vt100>)
                                      :optional (y #f))
  (putstr con "\x1b;M")
  (if (and y (<= y 0)) 0 -1))

;; No portable way to directly query it, so we take a kind of heuristic approach.
(define-method query-screen-size ((con <vt100>))
  (define *max-dim* 2000)
  (putstr con "\x1b;7") ; save cursor pos
  (move-cursor-to con *max-dim* *max-dim*)
  (unwind-protect (receive (h w) (query-cursor-position con)
                    (values (+ h 1) (+ w 1)))
    (putstr con "\x1b;8"))) ;restore cursor pos

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

;; This method is for workaround of windows IME bug.  For vt100,
;; we don't need to do anything.  See console/windows.scm for the details.
(define-method ensure-bottom-room ((con <vt100>) :optional full-column-flag)
  #f)

;;;
;;; Console class implementation - windows cosole
;;;

;; We define <windows-console> class unconditionally so that
;; the user doesn't need to use cond-expand (instead they may
;; use ordinary conditionals).
(define-class <windows-console> ()
  (;; all slots are private
   (keybuf :init-form (make-queue))
   (high-surrogate)
   ))

;; The actual method definitions depend on os.windows
(cond-expand
  [gauche.os.windows (include "windows")]
  [else])
