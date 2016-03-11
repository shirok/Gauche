;;;
;;; text.line-edit - line editing
;;;
;;;   Copyright (c) 2015  Shiro Kawai  <shiro@acm.org>
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

(define-module text.line-edit
  (use gauche.generator)
  (use data.ring-buffer)
  (use data.queue)
  (use util.match)
  (use text.console)
  (use text.gap-buffer)
  (export <line-edit-context> read-line/edit)
  )
(select-module text.line-edit)

(define *kill-ring-size* 60)
(define *history-size* 200)

;; <line-edit-context>
;; Initializable slots:
;;   console - <console> object to use.  if omitted, make-default-console
;;             is called (see text.console)
;;   prompt  - a string or a thunk that returns a string.
;;   keymap  - a hashtable to keycode -> command.  not much useful now,
;;             for we haven't exported commands.
;;   input-continues - if this is #f, commit-or-newline always
;;             commit the line---that read-line/edit strictly works as
;;             line editor.  Otherwise, this should be a procedure that
;;             takes the current buffer content as a string, and should
;;             return #f if the input is complete, or #t otherwise.
;;             If the procedure returns #t, commit-or-newline inserts a
;;             newline to the buffer and enters multiline edit mode.

(define-class <line-edit-context> ()
  ((console :init-keyword :console :init-form (make-default-console))
   (prompt  :init-keyword :prompt :init-value "")
   (keymap  :init-keyword :keymap :init-form (default-keymap))
   (input-continues :init-keyword :input-continues :init-form #f)
   ;; for wide characters support
   (wide-char-disp-width :init-keyword :wide-char-disp-width :init-value 2)
   (wide-char-pos-width  :init-keyword :wide-char-pos-width  :init-value 2)
   (surrogate-char-disp-width :init-keyword :surrogate-char-disp-width :init-value 2)
   (surrogate-char-pos-width  :init-keyword :surrogate-char-pos-width  :init-value 2)

   ;; Following slots are private.
   (initpos-y)
   (initpos-x)
   (screen-height)
   (screen-width)

   ;; Selection
   ;; selection is between marker-pos and the current cursor pos.
   ;; maker-pos == #f means no selection.
   (marker-pos :init-value #f)

   ;; Kill and yank
   ;; Kill ring survives across sessions.  Last-yank-* is reset after
   ;; every command except yank and yank-pop.
   (kill-ring :init-form (make-ring-buffer
                          (make-vector *kill-ring-size*)
                          :overflow-handler 'overwrite))
   (last-yank :init-value -1) ; index into the kill-ring buffer.  -1 means
                              ; last op wasn't yank.
   (last-yank-pos :init-value 0) ; index into buffer where last yank is put.
   (last-yank-size :init-value 0)

   ;; Undo
   ;; See the comment at the bottom.  In the normal state, redo-queue is
   ;; empty and undo-stack has the stack of edit commands to undo the changes.
   ;; Redo-queue is non-empty when we're in the middle of undo sequence.
   (undo-stack :init-form (make-queue))
   (redo-queue :init-form (make-queue))

   ;; History
   ;; Global history is kept in the ring buffer, index 0 being the most recent.
   ;; History-pos is the index to the last recalled history; -1 indicates
   ;; we're editing the fresh line instead of the one recalled from history.
   ;; The history-transient is a hashtable indexed by history-pos and holds
   ;; the line and undo-stack; see below.
   (history :init-form (make-ring-buffer
                        (make-vector *history-size*)
                        :overflow-handler 'overwrite))
   (history-pos :init-value -1)
   (history-transient :init-value #f)
   ))

;; Entry point API
;(define (read-line/edit ctx)
(define (read-line/edit ctx :optional (use-call-with-console #t))
  (reset-undo-info! ctx)
  (reset-last-yank! ctx)
  (if use-call-with-console
    (call-with-console (~ ctx'console) (%read-line/edit-sub ctx))
    ((%read-line/edit-sub ctx) (~ ctx'console))))

(define (%read-line/edit-sub ctx)
     (^[con]
       (define buffer (make-gap-buffer))
       (define (eofread)
         (if (> (gap-buffer-content-length buffer) 0)
           (commit-history ctx buffer)
           (eof-object)))

       ;; For windows ime bug:
       ;;   We have to make a space on the last line of console,
       ;;   because windows ime overwrites the last line and causes
       ;;   a system error.
       (last-scroll con)

       (show-prompt ctx)
       (init-screen-params ctx)
       ;; Main loop.  Get a key and invoke associated command.
       ;; The command may return one of the following values.
       ;;   #f     - nothing visible is done
       ;;   #<eof> - end of input - either input port is closed, or
       ;;            the user typed EOT char when the buffer is empty.
       ;;   commit - record the current buffer to the history, and
       ;;            returns it.
       ;;   undone - the command undid the change.  we record the fact,
       ;;            for the consecutive undo behaves differently than
       ;;            other commands.
       ;;   move   - the command only moved cursor pos.  requires redisplay,
       ;;            but we keep selection.
       ;;   #t     - the command changed something so we need to redisplay,
       ;;            but the buffer contents is intact.  the selection is
       ;;            cleared.
       ;;   <edit-command> - the commad changed the buffer contents,
       ;;        and the return value is an edit command to undo the
       ;;        change.
       ;;   (yanked <edit-command>) - this is only returned by yank and yank-pop
       ;;        command.
       ;;
       ;; If the key is a character and it is not registered in the table,
       ;; we treat as if it is associated to the self-insert-command.
       ;; Given the large character set, it is a reasonable compromise.
       (let loop ([redisp #f])
         (when redisp

           ;; for speed-up of pasting a text
           ;(redisplay ctx buffer))
           (if (not (chready? con)) (redisplay ctx buffer)))

         (let* ([ch (getch con)]
                [h (hash-table-get (~ ctx'keymap) ch ch)])
           (cond
            [(eof-object? h) (eofread)]
            [(char? h)
             (reset-last-yank! ctx)
             (break-undo-sequence! ctx)
             (clear-mark! ctx buffer)
             (push-undo! ctx (self-insert-command ctx buffer h))
             (loop #t)]
            [(procedure? h)
             (match (h ctx buffer ch)
               [(? boolean? b)
                (reset-last-yank! ctx)
                (clear-mark! ctx buffer)
                (break-undo-sequence! ctx)
                (loop b)]
               [(? eof-object?)
                (if (> (gap-buffer-content-length buffer) 0)
                  (commit-history ctx buffer)
                  (eof-object))]

               ;; To aboid overwriting input lines:
               ;;   When multiple input lines are commited and a cursor
               ;;   is on the first line, the echo back will overwite
               ;;   the input lines.
               ;;   So we redisplay the input lines and move a cursor
               ;;   to the last line.
               ;['commit (commit-history ctx buffer)]
               ['commit (redisplay ctx buffer #t)
                        (commit-history ctx buffer)]

               ['undone (reset-last-yank! ctx)
                        (clear-mark! ctx buffer)
                        (loop #t)] ; don't break undo sequence
               ['move   (reset-last-yank! ctx)
                        (break-undo-sequence! ctx)
                        (loop #t)]
               [('yanked edit-command)
                (break-undo-sequence! ctx)
                (clear-mark! ctx buffer)
                (push-undo! ctx edit-command)
                (loop #t)]
               [(? list? edit-command)
                (reset-last-yank! ctx)
                (break-undo-sequence! ctx)
                (clear-mark! ctx buffer)
                (push-undo! ctx edit-command)
                (loop #t)]
               [x (error "[internal] invalid return value from a command:" x)])]
            [else
             (error "[internal] do not know how to handle key:" h)])))))

;; Check some parameters of screen.
(define (init-screen-params ctx)
  (receive (y x) (query-cursor-position (~ ctx'console))
    (set! (~ ctx'initpos-y) y)
    (set! (~ ctx'initpos-x) x))
  (receive (h w) (query-screen-size (~ ctx'console))
    (set! (~ ctx'screen-height) h)
    (set! (~ ctx'screen-width) w)))

;; Show prompt.  Returns the current column.
(define (show-prompt ctx)
  (let* ([p (~ ctx'prompt)]
         [s (if (applicable? p) (with-output-to-string p) (x->string p))])
    (putstr (~ ctx'console) s)))

;; Show secondary prompt
;; TODO: make this customizable
(define (show-secondary-prompt ctx)
  (when (> (~ ctx'initpos-x) 0)
    (putstr (~ ctx'console) (make-string (~ ctx'initpos-x) #\.))))

;; TODO: Mind char-width correctly.
;(define (redisplay ctx buffer)
(define (redisplay ctx buffer :optional (pos-to-end-flag #f))
  (define (get-tab-width x) (- 8 (modulo x 8)))
  (define (get-char-width ch x wide-char-width surrogate-char-width)
    (let1 chcode (char->integer ch)
      (cond
       [(eqv? ch #\tab)
        (get-tab-width x)]
       [(and (>= chcode 0) (<= chcode #x7f))
        1]
       [(>= chcode #x10000)
        (cond-expand
         [gauche.ces.utf8
          surrogate-char-width]
         [else
          wide-char-width])]
       [else
        wide-char-width])))

  (define use-windows-console?
    (eq? (class-name (class-of (~ ctx'console))) '<windows-console>))

  ;; check a initial position
  (if (< (~ ctx'initpos-y) 0)
    (set! (~ ctx'initpos-y) 0))

  (let* ([con (~ ctx'console)]
         [y   (~ ctx'initpos-y)]
         [x   (~ ctx'initpos-x)]
         [w   (~ ctx'screen-width)]
         [h   (~ ctx'screen-height)]
         [sel (selected-range ctx buffer)]
         [disp-x x]
         [pos-x  x]
         [pos-y  y]
         [pos-set-flag #f]
         [pos (gap-buffer-pos buffer)]
         [g   (gap-buffer->generator buffer)]
         [maxy   #f]
         [sel-flag #f])

    (define (line-wrapping disp-x1 w :optional (full-column-flag #f))
      (when (>= disp-x1 w)
        (set! x      0)
        (set! disp-x 0)
        (move-cursor-to con y x)

        (cond
         [use-windows-console?

          ;; For windows ime bug:
          ;;   If a full column wrapping is done when windows ime is on,
          ;;   one more line scroll-up may occur.
          ;;   So we must deal with this problem.
          (last-scroll con full-column-flag)

          ;; Console buffer scroll-up:
          ;;   The cursor position may be changed.
          (cursor-down/scroll-up con)
          (receive (y2 x2) (query-cursor-position con)
            (set! (~ ctx'initpos-y) (+ (~ ctx'initpos-y) (- y2 y 1)))
            (if pos-set-flag (set! pos-y (max (+ pos-y (- y2 y 1)) 0)))
            (set! y y2))

          ;; For windows ime bug:
          ;;   We have to make a space on the last line of console,
          ;;   because windows ime overwrites the last line and causes
          ;;   a system error.
          (last-scroll con full-column-flag)
          (receive (y2 x2) (query-cursor-position con)
            (set! (~ ctx'initpos-y) (+ (~ ctx'initpos-y) (- y2 y)))
            (if pos-set-flag (set! pos-y (max (+ pos-y (- y2 y)) 0)))
            (set! y y2))

          ]
         [else

          ;; Console buffer scroll-up:
          ;;   The cursor position may be changed.
          (when (or (or (not maxy) (<= y maxy)) pos-to-end-flag)
            (cursor-down/scroll-up con)
            (cond
             [(>= y (- h 1))
              (dec! (~ ctx'initpos-y))
              (if pos-set-flag (dec! pos-y))]
             [else
              (inc! y)]))

          ;; check a cursor position for clipping a display area
          (if (and (<= pos-y 0) pos-set-flag)
            (set! maxy (- h 2)))

          ])
        ))

    (reset-character-attribute con)
    (move-cursor-to con y 0)
    (show-prompt ctx)
    (clear-to-eos con)
    (let loop ([n 1])
      (glet1 ch (g)

        ;; set a region color
        (when (and sel (not (eqv? (car sel) (cdr sel))) (not pos-to-end-flag))
          (cond [(eqv? (- n 1) (car sel))
                 (set! sel-flag #t)
                 (set-character-attribute con '(#f #f bright underscore))]
                [(eqv? (- n 1) (cdr sel))
                 (set! sel-flag #f)
                 (reset-character-attribute con)]))

        ;; print a character
        (case ch
          [(#\newline)]
          [(#\tab)
           (let1 tw (get-tab-width disp-x)
             (if (>= (+ disp-x tw) w)
               (set! tw (- w disp-x)))
             (when (or (and (>= y 0) (or (not maxy) (<= y maxy))) pos-to-end-flag)
               (move-cursor-to con y x)
               (putstr con (make-string tw #\space))))]
          [else
           (line-wrapping (+ disp-x (get-char-width ch
                                                    disp-x
                                                    (~ ctx'wide-char-disp-width)
                                                    (~ ctx'surrogate-char-disp-width)))
                          (+ w 1))
           (when (or (and (>= y 0) (or (not maxy) (<= y maxy))) pos-to-end-flag)
             (move-cursor-to con y x)
             (putch con ch))])

        ;; line wrapping
        (set! x      (+ x      (get-char-width ch
                                               x
                                               (~ ctx'wide-char-pos-width)
                                               (~ ctx'surrogate-char-pos-width))))
        (set! disp-x (+ disp-x (get-char-width ch
                                               disp-x
                                               (~ ctx'wide-char-disp-width)
                                               (~ ctx'surrogate-char-disp-width))))
        (case ch
          [(#\newline)
           (line-wrapping w w)
           (when (or (and (>= y 0) (or (not maxy) (<= y maxy))) pos-to-end-flag)
             (if sel-flag (reset-character-attribute con))
             (show-secondary-prompt ctx)
             (if sel-flag (set-character-attribute con '(#f #f bright underscore))))
           (set! x (~ ctx'initpos-x))
           (set! disp-x x)]
          [else
           (line-wrapping disp-x w #t)])

        ;; set a cursor position
        (if (not pos-set-flag)
          (cond
           [(= pos 0)
            (set! pos-set-flag #t)
            (set! pos-x (~ ctx'initpos-x))
            (set! pos-y y)]
           [(= pos n)
            (set! pos-set-flag #t)
            (set! pos-x x)
            (set! pos-y y)]))

        (loop (+ n 1))))
    (when pos-to-end-flag
      (set! pos-x x)
      (set! pos-y y))
    (move-cursor-to con pos-y pos-x)))

;;
;; Key combinations
;;

(define (ctrl k)
  (integer->char (- (logand (char->integer k) (lognot #x20)) #x40)))
(define (alt k) `(ALT ,k))

;;
;; Selection
;;

(define (set-mark! ctx buffer)
  (set! (~ ctx'marker-pos) (gap-buffer-pos buffer)))

(define (clear-mark! ctx buffer) (set! (~ ctx'marker-pos) #f))

(define (selected-range ctx buffer) ; returns (start . end), end exclusive
  (and-let1 s (~ ctx'marker-pos)
    (let1 p (gap-buffer-pos buffer)
      (if (< p s) (cons p s) (cons s p)))))

;;
;; Undo stuff
;;

(define (break-undo-sequence! ctx)
  (unless (queue-empty? (~ ctx'redo-queue))
    (apply queue-push! (~ ctx'undo-stack) (dequeue-all! (~ ctx'redo-queue)))))

(define (push-undo! ctx edit-command)
  (queue-push! (~ ctx'undo-stack) edit-command))

(define (reset-undo-info! ctx)
  (dequeue-all! (~ ctx'redo-queue))
  (dequeue-all! (~ ctx'undo-stack)))

(define (set-undo-info! ctx undo-list)
  (reset-undo-info! ctx)
  (unless (null? undo-list)
    (apply enqueue! (~ ctx'undo-stack) undo-list)))

;;
;; History
;;

;; Enter the current buffer contents as the history, and discard any
;; transient info.  Returns the current buffer content.
(define (commit-history ctx buffer)
  (rlet1 str (gap-buffer->string buffer)
    (ring-buffer-add-front! (~ ctx'history) str)
    (set! (~ ctx'history-pos) -1)
    (set! (~ ctx'history-transient) #f)))

;; (~ ctx'history-transient) is a hashtable, indexed by history position
;; (-1 being the fresh line), and its value is a pair of string and
;; undo stack.  When history is recalled and edited, and then the user
;; moves history position, we save the edited line and its undo stack
;; in this table.  When the user comes back to the history position,
;; we present the saved one instead of the actual history.
;; Note that when the user recalls history for the first time of the session,
;; we save the fresh line and its undo info in the transient table
;; as the history position -1.
;; The table is reset when the user commits the input.
(define (ensure-history-transient ctx)
  (or (~ ctx'history-transient)
      (rlet1 tab (make-hash-table 'eqv?)
        (set! (~ ctx'history-transient) tab))))

;; Save the current editing line to the transient table if necessary
(define (save-history-transient ctx buffer)
  (break-undo-sequence! ctx) ; flush redo queue
  (when (or (= (~ ctx'history-pos) -1)
            (not (queue-empty? (~ ctx'undo-stack))))
    (hash-table-put! (ensure-history-transient ctx)
                     (~ ctx'history-pos)
                     (cons (gap-buffer->string buffer)
                           (dequeue-all! (~ ctx'undo-stack))))))

;; returns (<string> . <undo-list>)
(define (get-history ctx)
  (let1 tab (ensure-history-transient ctx)
    (or (hash-table-get tab (~ ctx'history-pos) #f)
        (cons (ring-buffer-ref (~ ctx'history) (~ ctx'history-pos) "")
              '()))))

(define (history-pos ctx) (~ ctx'history-pos))
(define (history-size ctx) (ring-buffer-num-entries (~ ctx'history)))

;;
;; Kill rings
;;

(define (reset-last-yank! ctx)
  (set! (~ ctx'last-yank) -1)
  (set! (~ ctx'last-yank-pos) 0)
  (set! (~ ctx'last-yank-size) 0))

(define (get-yank-line ctx)
  (let* ([rb (~ ctx'kill-ring)]
         [n  (ring-buffer-num-entries rb)])
    (if (zero? n)
      ""  ; no yank line
      (ring-buffer-ref rb (modulo (~ ctx'last-yank) n)))))

(define (save-kill-ring ctx str)
  (ring-buffer-add-front! (~ ctx'kill-ring) str))

(define (pop-yank-line ctx)
  (if (zero? (ring-buffer-num-entries (~ ctx'kill-ring)))
    ""
    (begin (inc! (~ ctx'last-yank))
           (get-yank-line ctx))))

;;
;; Some buffer utilities
;;

(define (buffer-current-line&col buf)
  (generator-fold (^[ch p]
                    (match-let1 (row . col) p
                      (if (eqv? ch #\newline)
                        (cons (+ row 1) 0)
                        (cons row (+ col 1)))))
                  '(0 . 0)
                  (gap-buffer->generator buf 0 (gap-buffer-pos buf))))

;; returns the # of newline chars in buffer
(define (buffer-num-lines buf)
  (generator-fold (^[ch cnt] (if (eqv? ch #\newline) (+ cnt 1) cnt))
                  0 (gap-buffer->generator buf)))

;; Tries to set the buffer position in line & column.  If the line
;; isn't wide enough, set the position at the end of the line.
(define (buffer-set-line&col! buf line col)
  (define nchars (gap-buffer-content-length buf))
  (let skip-lines ([pos 0] [lin 0])
    (cond [(= nchars pos) (gap-buffer-move! buf pos)]
          [(= lin line)
           (let skip-chars ([pos pos] [chr 0])
             (if (or (= nchars pos)
                     (eqv? (gap-buffer-ref buf pos) #\newline)
                     (= chr col))
               (gap-buffer-move! buf pos)
               (skip-chars (+ pos 1) (+ chr 1))))]
          [(eqv? (gap-buffer-ref buf pos) #\newline)
           (skip-lines (+ pos 1) (+ lin 1))]
          [else (skip-lines (+ pos 1) lin)])))

;;;
;;; Commands
;;;

;; A command is a procedure that takes <line-edit-context>, <gap-buffer>,
;; and <key>.
;; Return value must be one of #f, #t, #<eof>, commit, undone, or
;; <edit-command>.  See the read-line/edit above for the details.

(define (self-insert-command ctx buf key)
  (gap-buffer-edit! buf `(i #f ,(x->string key))))

(define (commit-input ctx buf key)
  'commit)

(define (commit-or-newline ctx buf key)
  (or (and-let* ([pred (~ ctx'input-continues)]
                 [ (pred (gap-buffer->string buf)) ])
        (gap-buffer-edit! buf '(i #f "\n")))
      'commit))

(define (delete-char ctx buf key)
  (and (not (gap-buffer-gap-at? buf 'end))
       (gap-buffer-edit! buf '(d #f 1))))

(define (eot-or-delete-char ctx buf key)
  (if (zero? (gap-buffer-content-length buf))
    (eof-object)
    (delete-char ctx buf key)))

(define (delete-backward-char ctx buf key)
  (and (not (gap-buffer-gap-at? buf 'beginning))
       (let1 p-1 (- (gap-buffer-pos buf) 1)
         (gap-buffer-edit! buf `(d ,p-1 1)))))

(define (backward-char ctx buf key)
  (if (not (gap-buffer-gap-at? buf 'beginning))
    (gap-buffer-move! buf -1 'current))
  'move)

(define (forward-char ctx buf key)
  (if (not (gap-buffer-gap-at? buf 'end))
    (gap-buffer-move! buf 1 'current))
  'move)

(define (move-beginning-of-line ctx buf key)
  (if (not (gap-buffer-gap-at? buf 'beginning))
    (gap-buffer-move! buf 0 'beginning))
  'move)

(define (move-end-of-line ctx buf key)
  (if (not (gap-buffer-gap-at? buf 'end))
    (gap-buffer-move! buf 0 'end))
  'move)

(define (set-mark-command ctx buf key)
  (set-mark! ctx buf)
  'move)

(define (kill-line ctx buf key)
  (and (not (gap-buffer-gap-at? buf 'end))
       (let* ([len (- (gap-buffer-content-length buf) (gap-buffer-pos buf))]
              [e (gap-buffer-edit! buf `(d #f ,len))])
         ;; e contiains (i <pos> <killed-string>)
         (save-kill-ring ctx (caddr e))
         e)))

(define (kill-region ctx buf key)
  (match (selected-range ctx buf)
    [(start . end)
     ;; NB: the cursor is either on start or on end.  either way,
     ;; after operation the cursor's be at start.
     (rlet1 e (gap-buffer-edit! buf `(d ,start ,(- end start)))
       ;; e contiains (i <pos> <killed-string>)
       (save-kill-ring ctx (caddr e)))]
    [_ #t]))

(define (kill-ring-save ctx buf key)
  (match (selected-range ctx buf)
    [(start . end)
     (save-kill-ring ctx (gap-buffer->string buf start end))
     #t] ; this clears selection
    [_ #t]))

(define (refresh-display ctx buf key)
  (reset-terminal (~ ctx'console))
  (move-cursor-to (~ ctx'console) 0 0)
  (show-prompt ctx)
  (init-screen-params ctx)
  #t)

;; NB: This command may modify undo queue
(define (prev-history ctx buf key)
  (and (< (history-pos ctx) (- (history-size ctx) 1))
       (begin
         (save-history-transient ctx buf)
         (inc! (~ ctx'history-pos))
         (let1 p (get-history ctx)
           (set-undo-info! ctx (cdr p))
           (gap-buffer-clear! buf)
           (gap-buffer-insert! buf (car p))
           #t))))

;; NB: This command may modify undo queue
(define (next-history ctx buf key)
  (and (> (history-pos ctx) -1)
       (begin
         (save-history-transient ctx buf)
         (dec! (~ ctx'history-pos))
         (let1 p (get-history ctx)
           (set-undo-info! ctx (cdr p))
           (gap-buffer-clear! buf)
           (gap-buffer-insert! buf (car p))
           #t))))

(define (prev-line-or-history ctx buf key)
  (match-let1 (lines . col) (buffer-current-line&col buf)
    (if (zero? lines)
      (prev-history ctx buf key)
      (begin (buffer-set-line&col! buf (- lines 1) col) 'move))))

(define (next-line-or-history ctx buf key)
  (match-let1 (lines . col) (buffer-current-line&col buf)
    (if (= lines (buffer-num-lines buf))
      (next-history ctx buf key)
      (begin (buffer-set-line&col! buf (+ lines 1) col) 'move))))
  
       
(define (transpose-chars ctx buf key)
  (cond [(gap-buffer-gap-at? buf 'beginning) #f]
        [(= (gap-buffer-content-length buf) 1) ; special case
         (gap-buffer-move! buf 0)
         'move]
        [else
         (gap-buffer-move! buf
                           (if (gap-buffer-gap-at? buf 'end) -2 -1)
                           'current)
         (let ([cur  (gap-buffer-ref buf (+ (gap-buffer-pos buf) 1))]
               [prev (gap-buffer-ref buf (gap-buffer-pos buf))])
           (gap-buffer-edit! buf `(c #f 2 ,(string cur prev))))]))

(define (yank ctx buf key)
  (and (> (ring-buffer-num-entries (~ ctx'kill-ring)) 0)
       (begin
         (set! (~ ctx'last-yank) 0)
         (set! (~ ctx'last-yank-pos) (gap-buffer-gap-start buf))
         (let1 yanked-text (get-yank-line ctx)
           (set! (~ ctx'last-yank-size) (string-length yanked-text))
           `(yanked ,(gap-buffer-edit! buf `(i #f ,yanked-text)))))))

(define (yank-pop ctx buf key)
  (and (> (ring-buffer-num-entries (~ ctx'kill-ring)) 0)
       (>= (~ ctx'last-yank) 0)
       (let* ([text (pop-yank-line ctx)]
              [pos  (~ ctx'last-yank-pos)]
              [len  (~ ctx'last-yank-size)]
              [edit (gap-buffer-edit! buf `(c ,pos ,len ,text))])
         (set! (~ ctx'last-yank-size) (string-length text))
         `(yanked ,edit))))

(define (undo ctx buf key)
  (and (not (queue-empty? (~ ctx'undo-stack)))
       (let* ([undo-command (queue-pop! (~ ctx'undo-stack))]
              [redo-command (gap-buffer-edit! buf undo-command)])
         (queue-push! (~ ctx'redo-queue) undo-command)
         (enqueue! (~ ctx'redo-queue) redo-command)
         'undone)))

(define (keyboard-quit ctx buf key)
  (beep (~ ctx'console))
  #t)

(define (undefined-command ctx buf key)
  (beep (~ ctx'console))
  #f)

(define (nop-command ctx buf key)
  #f)

(define (default-keymap)
  (hash-table 'equal?
              `(,(ctrl #\@) . ,set-mark-command)
              `(,(ctrl #\a) . ,move-beginning-of-line)
              `(,(ctrl #\b) . ,backward-char)
              `(,(ctrl #\c) . ,undefined-command)
              `(,(ctrl #\d) . ,eot-or-delete-char)
              `(,(ctrl #\e) . ,move-end-of-line)
              `(,(ctrl #\f) . ,forward-char)
              `(,(ctrl #\g) . ,keyboard-quit)
              `(,(ctrl #\h) . ,delete-backward-char)
              `(,(ctrl #\i) . ,self-insert-command) ; tab
              `(,(ctrl #\j) . ,commit-or-newline)   ; newline
              `(,(ctrl #\k) . ,kill-line)
              `(,(ctrl #\l) . ,refresh-display)
              `(,(ctrl #\m) . ,commit-or-newline) ; return
              `(,(ctrl #\n) . ,next-line-or-history)
              `(,(ctrl #\o) . ,undefined-command); todo
              `(,(ctrl #\p) . ,prev-line-or-history)
              `(,(ctrl #\q) . ,undefined-command); todo
              `(,(ctrl #\r) . ,undefined-command); todo
              `(,(ctrl #\s) . ,undefined-command); todo
              `(,(ctrl #\t) . ,transpose-chars)
              `(,(ctrl #\u) . ,undefined-command); todo
              `(,(ctrl #\v) . ,undefined-command); todo
              `(,(ctrl #\w) . ,kill-region)
              `(,(ctrl #\x) . ,undefined-command); todo
              `(,(ctrl #\y) . ,yank)
              `(,(ctrl #\z) . ,undefined-command); todo
              `(,(ctrl #\[) . ,undefined-command)
              `(,(ctrl #\\) . ,undefined-command)
              `(,(ctrl #\]) . ,undefined-command)
              `(,(ctrl #\^) . ,undefined-command)
              `(,(ctrl #\_) . ,undo)

              `(,(alt #\null) . ,nop-command) ; for windows (ime on/off)
              `(,(alt #\space) . ,undefined-command) ; should be set-mark
              `(,(alt #\!) . ,undefined-command)
              `(,(alt #\") . ,undefined-command)
              `(,(alt #\#) . ,undefined-command)
              `(,(alt #\$) . ,undefined-command)
              `(,(alt #\%) . ,undefined-command)
              `(,(alt #\&) . ,undefined-command)
              `(,(alt #\') . ,undefined-command)
              `(,(alt #\() . ,undefined-command)
              `(,(alt #\)) . ,undefined-command)
              `(,(alt #\*) . ,undefined-command)
              `(,(alt #\+) . ,undefined-command)
              `(,(alt #\,) . ,undefined-command)
              `(,(alt #\-) . ,undefined-command)
              `(,(alt #\.) . ,undefined-command)
              `(,(alt #\/) . ,undefined-command)
              `(,(alt #\0) . ,undefined-command)
              `(,(alt #\1) . ,undefined-command)
              `(,(alt #\2) . ,undefined-command)
              `(,(alt #\3) . ,undefined-command)
              `(,(alt #\4) . ,undefined-command)
              `(,(alt #\5) . ,undefined-command)
              `(,(alt #\6) . ,undefined-command)
              `(,(alt #\7) . ,undefined-command)
              `(,(alt #\8) . ,undefined-command)
              `(,(alt #\9) . ,undefined-command)
              `(,(alt #\:) . ,undefined-command)
              `(,(alt #\;) . ,undefined-command)
              `(,(alt #\<) . ,undefined-command)
              `(,(alt #\=) . ,undefined-command)
              `(,(alt #\>) . ,undefined-command)
              `(,(alt #\?) . ,undefined-command)

              
              `(,(alt #\@) . ,undefined-command)
              `(,(alt #\A) . ,undefined-command)
              `(,(alt #\B) . ,undefined-command)
              `(,(alt #\C) . ,undefined-command)
              `(,(alt #\D) . ,undefined-command)
              `(,(alt #\E) . ,undefined-command)
              `(,(alt #\F) . ,undefined-command)
              `(,(alt #\G) . ,undefined-command)
              `(,(alt #\H) . ,undefined-command)
              `(,(alt #\I) . ,undefined-command)
              `(,(alt #\J) . ,undefined-command)
              `(,(alt #\K) . ,undefined-command)
              `(,(alt #\L) . ,undefined-command)
              `(,(alt #\M) . ,undefined-command)
              `(,(alt #\N) . ,undefined-command)
              `(,(alt #\O) . ,undefined-command)
              `(,(alt #\P) . ,undefined-command)
              `(,(alt #\Q) . ,undefined-command)
              `(,(alt #\R) . ,undefined-command)
              `(,(alt #\S) . ,undefined-command)
              `(,(alt #\T) . ,undefined-command)
              `(,(alt #\U) . ,undefined-command)
              `(,(alt #\V) . ,undefined-command)
              `(,(alt #\W) . ,undefined-command)
              `(,(alt #\X) . ,undefined-command)
              `(,(alt #\Y) . ,yank-pop)
              `(,(alt #\Z) . ,undefined-command)
              `(,(alt #\[) . ,undefined-command)
              `(,(alt #\\) . ,undefined-command)
              `(,(alt #\]) . ,undefined-command)
              `(,(alt #\^) . ,undefined-command)
              `(,(alt #\_) . ,undefined-command)

              `(,(alt #\`) . ,undefined-command)
              `(,(alt #\a) . ,undefined-command)
              `(,(alt #\b) . ,undefined-command)
              `(,(alt #\c) . ,undefined-command)
              `(,(alt #\d) . ,undefined-command)
              `(,(alt #\e) . ,undefined-command)
              `(,(alt #\f) . ,undefined-command)
              `(,(alt #\g) . ,undefined-command)
              `(,(alt #\h) . ,undefined-command)
              `(,(alt #\i) . ,undefined-command)
              `(,(alt #\j) . ,undefined-command)
              `(,(alt #\k) . ,undefined-command)
              `(,(alt #\l) . ,undefined-command)
              `(,(alt #\m) . ,undefined-command)
              `(,(alt #\n) . ,next-history)
              `(,(alt #\o) . ,undefined-command)
              `(,(alt #\p) . ,prev-history)
              `(,(alt #\q) . ,undefined-command)
              `(,(alt #\r) . ,undefined-command)
              `(,(alt #\s) . ,undefined-command)
              `(,(alt #\t) . ,undefined-command)
              `(,(alt #\u) . ,undefined-command)
              `(,(alt #\v) . ,undefined-command)
              `(,(alt #\w) . ,kill-ring-save)
              `(,(alt #\x) . ,undefined-command)
              `(,(alt #\y) . ,yank-pop)
              `(,(alt #\z) . ,undefined-command)
              `(,(alt #\{) . ,undefined-command)
              `(,(alt #\|) . ,undefined-command)
              `(,(alt #\}) . ,undefined-command)
              `(,(alt #\)) . ,undefined-command)
              `(,(alt #\_) . ,undefined-command)
              `(,(alt #\x7f) . ,undefined-command)

              `(#\x7f . ,delete-backward-char)

              `(,(alt (ctrl #\space)) . ,undefined-command)
              `(,(alt (ctrl #\a)) . ,undefined-command)
              `(,(alt (ctrl #\b)) . ,undefined-command)
              `(,(alt (ctrl #\c)) . ,undefined-command)
              `(,(alt (ctrl #\d)) . ,undefined-command)
              `(,(alt (ctrl #\e)) . ,undefined-command)
              `(,(alt (ctrl #\f)) . ,undefined-command)
              `(,(alt (ctrl #\g)) . ,undefined-command)
              `(,(alt (ctrl #\h)) . ,undefined-command)
              `(,(alt (ctrl #\i)) . ,undefined-command)
              `(,(alt (ctrl #\j)) . ,undefined-command)
              `(,(alt (ctrl #\k)) . ,undefined-command)
              `(,(alt (ctrl #\l)) . ,undefined-command)
              `(,(alt (ctrl #\m)) . ,undefined-command)
              `(,(alt (ctrl #\n)) . ,undefined-command)
              `(,(alt (ctrl #\o)) . ,undefined-command)
              `(,(alt (ctrl #\p)) . ,undefined-command)
              `(,(alt (ctrl #\q)) . ,undefined-command)
              `(,(alt (ctrl #\r)) . ,undefined-command)
              `(,(alt (ctrl #\s)) . ,undefined-command)
              `(,(alt (ctrl #\t)) . ,undefined-command)
              `(,(alt (ctrl #\u)) . ,undefined-command)
              `(,(alt (ctrl #\v)) . ,undefined-command)
              `(,(alt (ctrl #\w)) . ,undefined-command)
              `(,(alt (ctrl #\x)) . ,undefined-command)
              `(,(alt (ctrl #\y)) . ,undefined-command)
              `(,(alt (ctrl #\z)) . ,undefined-command)
              `(,(alt (ctrl #\[)) . ,undefined-command)
              `(,(alt (ctrl #\\)) . ,undefined-command)
              `(,(alt (ctrl #\])) . ,undefined-command)
              `(,(alt (ctrl #\^)) . ,undefined-command)
              `(,(alt (ctrl #\_)) . ,undefined-command)

              `(KEY_UP    . ,prev-line-or-history)
              `(KEY_DOWN  . ,next-line-or-history)
              `(KEY_LEFT  . ,backward-char)
              `(KEY_RIGHT . ,forward-char)
              `(KEY_INS   . ,undefined-command)
              `(KEY_DEL   . ,delete-char)
              `(KEY_HOME  . ,undefined-command)
              `(KEY_END   . ,undefined-command)
              `(KEY_PGDN  . ,undefined-command)
              `(KEY_PGUP  . ,undefined-command)
              `(KEY_F1    . ,undefined-command)
              `(KEY_F2    . ,undefined-command)
              `(KEY_F3    . ,undefined-command)
              `(KEY_F4    . ,undefined-command)
              `(KEY_F5    . ,undefined-command)
              `(KEY_F6    . ,undefined-command)
              `(KEY_F7    . ,undefined-command)
              `(KEY_F8    . ,undefined-command)
              `(KEY_F9    . ,undefined-command)
              `(KEY_F10   . ,undefined-command)
              `(KEY_F11   . ,undefined-command)
              `(KEY_F12   . ,undefined-command)
              ))

#|

Undo semantics

 We emulate Emacs undo semantics, which allows "undoing undo".
 Here's the description of algorithm.

 - A, B, C, ... is an edit commans
 - Applying command A to buffer v is v*A.  
 - ^X is an inversion of command X.  X*^X = ^X*X = I (identity)

 1. Suppose the user did three buffer-changing operations, A, B and C.
 For every such operation, we push "undo procedure" into undo-stack.
 If we write undo procedure of A as ^A, the undo stack would look
 like this:

         buffer: v*A*B*C
     undo stack: ^A ^B ^C
     redo queue:

 2. If the user invoke undo command, we pop one undo procedure and applies
 to the buffer, then (1) append the undo op to the front of redo queue,
 and (2) push the redo op to the rear of redo queue.

         buffer: v*A*B
     undo stack: ^A ^B
     redo queue: ^C C

 3. If the user invoke undo command again immediately after that (that is,
 the user does not do any operation, including cursor movement),
 we keep doing the same op.

         buffer: v*A
     undo stack: ^A
     redo queue: ^B ^C C B

 4. If the user does anything but undo command here, we flush the redo queue
 and append the contents to undo stack.

         buffer: v*A
     undo stack: ^A ^B ^C C B
     redo queue:

 5. If the user does undo again, it actually redoes the last undo.

         buffer: v*A*B
     undo stack: ^A ^B ^C C
     redo queue: B ^B

 6. Contiguous undo follows the same pattern, until the undo stack get
    empty.

         buffer: v*A*B*C
     undo stack: 
     redo queue: ^A ^B ^C C B ^B ^C C B A
|#


