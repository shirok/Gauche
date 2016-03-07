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
          last-scroll ; for windows ime bug

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
  (cond-expand
   [gauche.os.windows
    (cond
     [(or (char-ready? (~ con'iport)) (not (~ con'input-delay)))
      (read-char (~ con'iport))]
     [else
      (let1 ch #f
        (do ((i 0 (+ i 1)))
            ((>= i 10) ch)
          (sys-nanosleep (* (~ con'input-delay) 1000))
          (when (char-ready? (~ con'iport))
            (set! ch (read-char (~ con'iport)))
            (set! i 10))
          ))])]
   [else
    (receive (nfds rfds wfds xfds)
        (sys-select! (sys-fdset (~ con'iport)) #f #f (~ con'input-delay))
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
      (begin (dequeue-all! q) #\escape)))
  (let1 q (~ con'input-buffer)
    (if (queue-empty? q)
      (let1 ch (read-char (~ con'iport))
        (if (eqv? ch #\escape)
          (fetch q)
          ch))
      (dequeue! q))))

(define-method get-raw-chars ((con <vt100>))  ; no translation
  (let1 q (~ con'input-buffer)
    ;(when (queue-empty? q) (enqueue! q (read-char (~ con'iport))))
    (dequeue-all! q)
    (enqueue! q (read-char (~ con'iport)))
    (let loop ()
      (let1 ch (%read-char/timeout con)
        (if (char? ch)
          (begin (enqueue! q ch) (loop))
          (dequeue-all! q))))))

(define-method query-cursor-position ((con <vt100>))
  (define q (~ con'input-buffer))
  (define q2 (make-queue)) ; query-cursor-buffer
  (define (fetch q2)
    ;(let1 ch (%read-char/timeout con)
    (let1 ch (read-char (~ con'iport))
      (cond
       [(eqv? ch #\escape)
        (dequeue-all! q2) ; drop other escape sequences
        (fetch q2)]
       [(eqv? ch #\R)
        (enqueue! q2 ch)]
       [(char? ch)
        (enqueue! q2 ch)
        (fetch q2)])))
  (putstr con "\x1b;[6n")
  (let loop ((ch (read-char (~ con'iport))))
    (cond
     [(eqv? ch #\escape)
      (fetch q2)]
     [(char? ch)
      (enqueue! q ch) ; queuing other characters
      (loop (read-char (~ con'iport)))]))
  (rxmatch-case (list->string (queue->list q2))
    [#/\[(\d+)\;(\d+)R/ (_ row col)
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

;; For windows ime bug:
;;   This is a dummy method.
(define-method last-scroll ((con <vt100>) :optional (full-column-flag #f)))

;;;
;;; Console class implementation - windows cosole
;;;

;; We define <windows-console> class unconditionally so that
;; the user doesn't need to use cond-expand (instead they may
;; use ordinary conditionals).
(define-class <windows-console> ()
  (;; all slots are private
   (keybuf :init-form (make-queue))
   (ihandle)
   (ohandle)
   (high-surrogate)
   ))

;; The actual method definitions depend on os.windows
(cond-expand
  [gauche.os.windows (include "console/windows")]
  [else])

;;;
;;; Select appropriate console
;;;

;; Regexp that matches $TERM that behaves as if it's vt100.
;; Strict compatibility isn't necessary; we're using just a common
;; denominator.  We expand this list as we discover other terminal types
;; that works.
;; See https://github.com/shirok/Gauche/issues/179 about 'screen'.
(define-constant *vt100-compatible-terminals*
  #/^(vt10[02]|vt220|xterm.*|rxvt.*|screen)$/)

;; Convenience API
;; Inspect the runtime environment and returns a console object with
;; appropriate setting.
;; If it cannot find supported console type, an error is signalled,
;; with a message describing why.
;; We use some heuristics to recognize vt100 compatible terminals.
(define (make-default-console)
  (cond [((with-module gauche.termios has-windows-console?))
         (make <windows-console>)]
        [(and-let1 t (sys-getenv "TERM")
           (*vt100-compatible-terminals* t))
         (make <vt100>)]
        [(sys-getenv "TERM")
         => (^t (error #"Unsupported terminal type: ~t"))]
        [else
         (error "TERM isn't set and we don't know how to control the terminal.")]))
