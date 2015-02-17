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
  (use util.match)
  (export <vt100>

          putch putstr getch chready?
          query-screen-size query-cursor-position move-cursor-to
          hide-cursor show-cursor
          reset-terminal clear-screen clear-to-eol
          set-character-attribute with-character-attribute))
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
   (reverse :init-keyword :bright  :init-value #f)
   (underscore :init-keyword :bright  :init-value #f)))

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
   ;; private
   (current-attrs :init-form (make <char-attrs>))))

(define-method putch ((con <vt100>) c)
  (display c (~ con'oport)) (flush (~ con'oport)))
(define-method putstr ((con <vt100>) s)
  (display s (~ con'oport)) (flush (~ con'oport)))
(define-method getch ((con <vt100>))
  (read-char (~ con'iport)))
(define-method chready? ((con <vt100>))
  (char-ready? (~ con'iport)))

(define-method query-cursor-position ((con <vt100>))
  (define (r) (getch con))
  (putstr con "\x1b;[6n")
  (unless (eqv? #\x1b (r)) (error "terminal error"))
  (unless (eqv? #\[   (r)) (error "terminal error"))
  (rxmatch-case ($ list->string $ generator->list
                   $ gtake-while (^c (not (eqv? #\R c))) r)
    [#/^(\d+)\;(\d+)$/ (_ row col) (values (x->integer row) (x->integer col))]
    [else (error "terminal error")]))

;; we're zero-origin as curses; vt100 takes 1-origin.
(define-method move-cursor-to ((con <vt100>) y x)
  (putstr con (format "\x1b;[~d;~dH" (+ y 1) (+ x 1))))

(define-method reset-terminal ((con <vt100>)) (putstr con "\x1b;c"))
(define-method clear-screen ((con <vt100>)) (putstr con "\x1b;[2J"))
(define-method clear-to-eol ((con <vt100>)) (putstr con "\x1b;[K"))

(define-method hide-cursor ((con <vt100>)) (putstr con "\x1b;[?25l"))
(define-method show-cursor ((con <vt100>)) (putstr con "\x1b;[?25h"))

;; No portable way to directly query it, so we take a kind of heuristic approach.
(define-method query-screen-size ((con <vt100>))
  (define *max-dim* 2000)
  (putstr con "\x1b;[s") ; save cursor pos
  (move-cursor-to con *max-dim* *max-dim*)
  (unwind-protect (query-cursor-position con)
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

;TODO:
;(define-class <windows-console> () ())
