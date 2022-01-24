;;;
;;; text.line-edit - line editing
;;;
;;;   Copyright (c) 2015-2021  Shiro Kawai  <shiro@acm.org>
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
  (use gauche.threads)
  (use gauche.sequence)
  (use gauche.unicode)
  (use data.ring-buffer)
  (use data.queue)
  (use file.util)
  (use util.match)
  (use text.console)
  (use text.console.wide-char-setting)
  (use text.gap-buffer)
  (use text.pager)
  (export <line-edit-context> read-line/edit
          save-line-edit-history
          load-line-edit-history

          default-keymap
          command-name->keystrokes
          command-name->keystroke-string)
  )
(select-module text.line-edit)

(autoload text.external-editor ed-string)
(autoload text.console.framebuffer <framebuffer-console>
                                   init-framebuffer
                                   draw-framebuffer)
(autoload text.multicolumn display-multicolumn)

(define *kill-ring-size* 60)
(define *history-size* 200)

;; <line-edit-context>
;; Initializable slots:
;;   console - <console> object to use.  if omitted, make-default-console
;;             is called (see text.console)
;;   prompt  - a string or a thunk that returns a string.
;;   keymap  - a keymap to map keystroke -> command.
;;   input-continues - if this is #f, commit-or-newline always
;;             commit the line---that read-line/edit strictly works as
;;             line editor.  Otherwise, this should be a procedure that
;;             takes the current buffer content as a string, and should
;;             return #f if the input is complete, or #t otherwise.
;;             If the procedure returns #t, commit-or-newline inserts a
;;             newline to the buffer and enters multiline edit mode.
;;   tab-char-width - a tab character width.
;;   wide-char-disp-setting - <wide-char-setting> object to configure
;;             a wide character setting for display.
;;   wide-char-pos-setting - <wide-char-setting> object to configure
;;             a wide character setting for cursor movement.
;;
(define-class <line-edit-context> ()
  ((console :init-keyword :console :init-form (make-default-console))
   (prompt  :init-keyword :prompt :init-value "")
   (keymap  :init-keyword :keymap :init-form (default-keymap))
   (input-continues :init-keyword :input-continues :init-form #f)
   (tab-char-width :init-keyword :tab-char-width :init-value 8)
   (wide-char-disp-setting :init-keyword :wide-char-disp-setting
                           :init-form (make <wide-char-setting>))
   (wide-char-pos-setting :init-keyword :wide-char-pos-setting
                          :init-form (make <wide-char-setting>))

   ;; A predicate or a char-set to determine word constituent characters
   (completion-word-constituent? :init-keyword :completion-word-constituent?
                                 :init-value #[-\w])
   ;; <^ <string> <gap-buffer> <integer> <integer> -> <List <string>>>
   ;; Returns list of completion candidates that match the given word.
   (completion-lister :init-keyword :completion-lister :init-value #f)

   ;; Following slots are private.
   ;; When we enter read/edit, we record the cursor position and screen size.
   (initpos-y)
   (initpos-x)
   (screen-height)
   (screen-width)

   ;; These are the cursor position immediately before a command is invoked
   ;; (or, more precisely, the cursor position after the last redraw).
   (lastpos-y)
   (lastpos-x)

   ;; The last executed editor command.  Completion command behaves differently
   ;; if invoked more than once immediately.
   (last-command :init-value #f)

   ;; Experimental framebuffer support.  Only enabled by the env var.
   (framebuffer :init-form (and (sys-getenv "GAUCHE_READ_EDIT_WITH_FB")
                                (make <framebuffer-console>)))

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

   ;; Keystroke queue
   (keystroke-queue :init-form (make-queue))

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

(define (%load-history ctx path)
  (with-input-from-file path
    (lambda ()
      (read)                            ; ignore version for now
      (do-generator [line read-line]    ; for now, one sexpr per line
        (guard (e [else #f])
          (commit-history ctx
                          (string->gap-buffer
                           (call-with-input-string line read))))))
    :if-does-not-exist #f))

(define (load-line-edit-history ctx path)
  (guard (e [else
             (display "failed to read history: " (current-error-port))
             (display e (current-error-port))
             (display #\newline (current-error-port))])
    (%load-history ctx path)))

(define (%save-history ctx path)
  (call-with-temporary-file
    (lambda (port name)
      (write '(gauche-history-version 1) port)
      (display #\newline port)
      (for-each
       (lambda (i)
         (write (ring-buffer-ref (~ ctx'history) i) port)
         (display "\n" port))
       (reverse (iota (history-size ctx))))
      (close-output-port port) ;; necessary on MinGW
      (sys-rename name path))
    :directory (sys-dirname path)))

(define (save-line-edit-history ctx path)
  (guard (e [else
             (display "failed to save history: " (current-error-port))
             (display e (current-error-port))
             (display #\newline (current-error-port))])
    (%save-history ctx path)))

;; Global SIGCONT handler
;;  Since we, as a library, don't have control of sigmasks in all threads,
;;  so we don't know which thread the sigcont handler is invoked.

;;  *sigcont-observer* is an atom of these values:
;;   - list of threads running read-line/edit
;;   - list of threads that haven't handled SIGCONT
;;
;;  When SIGCONT handler is called, it copies the first value to the second.
;;  When read-line/edit attempt to read from console, it checks the second
;;  value to see if the calling thread is in it.  If so, remove the thread
;;  from it, and reset the console.

;;  NB: First call to read-line/edit installs global SIGCONT handler.
;;  We can't localize the effect.  If you want to handle SIGCONT yourself,
;;  you can't use read-line/edit.  (We may be able to offer an option to
;;  handle SIGCONT or not, but it'll be pretty hairy to make it safe.)

(define *sigcont-observers* (atom '() '()))

;; Windows doesn't define SIGCONT, and we precompile this module so we can't
;; use cond-expand.
(define *sigcont* (global-variable-ref (find-module 'gauche) 'SIGCONT 0))

(define (%sigcont-handler sig)
  (when (eqv? sig *sigcont*)
    (atomic-update! *sigcont-observers* (^[ts _] (values ts ts)))))

(define (%sigcont-observe)
  (let1 t (current-thread)
    (atomic-update! *sigcont-observers*
                    (^[ts notifiee]
                      (when (null? ts)
                        (set-signal-handler! *sigcont* %sigcont-handler))
                      (values (if (memq t ts) ts (cons t ts))
                              notifiee)))))

(define (%sigcont-unobserve)
  (let1 t (current-thread)
    (atomic-update! *sigcont-observers*
                    (^[ts notifiee]
                      (let1 ts. (delete t ts)
                        (when (null? ts)
                          (set-signal-handler! *sigcont* #t))
                        (values (delete t ts) notifiee))))))

(define (%sigcont-received?)
  (let1 t (current-thread)
    (values-ref (atomic-update! *sigcont-observers*
                                (^[ts notifiee]
                                  (values ts (delete t notifiee)
                                          (boolean (memq t notifiee)))))
                2)))

;; Entry point API
;; NB: For the consistency with read-line, the returned string won't include
;; the final newline.
(define (read-line/edit ctx)
  (define buffer (make-gap-buffer))
  (define (eofread)
    (if (> (gap-buffer-content-length buffer) 0)
      (commit-history ctx buffer)
      (eof-object)))
  ;; Command handler.
  ;; The command may return one of the following values.
  ;;   visible - the command changed something visible so we need to
  ;;            redisplay, but we don't need to save the change in
  ;;            the actual buffer.
  ;;            The selection is cleared.
  ;;   unchanged - no change visually and internally; we break
  ;;            undo sequence and reset last yank, but not clear
  ;;            selection.
  ;;            This occurs, for example, backward-char at the
  ;;            beginning of the input.
  ;;   nop    - totally ignore the key input.
  ;;   redraw - we won't change the state of the editor, but
  ;;            we reobtain the cursor position before redrawing
  ;;            the buffer content.  This is necessary when the command
  ;;            wrote something on the screen and we don't want to clobber
  ;;            it.
  ;;   moved  - the command only moved cursor pos.  requires redisplay,
  ;;            but we keep selection.
  ;;   #<eof> - end of input - either input port is closed, or
  ;;            the user typed EOT char when the buffer is empty.
  ;;   commit - record the current buffer to the history, and
  ;;            returns it.
  ;;   undone - the command undid the change.  we record the fact,
  ;;            for the consecutive undo behaves differently than
  ;;            other commands.
  ;;   <edit-command> - the command changed the buffer contents,
  ;;            and the return value is an edit command to undo the
  ;;            change.
  ;;   (yanked <edit-command>) - this is only returned by yank and yank-pop
  ;;            command.
  ;;
  ;; If the key is a character and it is not registered in the table,
  ;; we treat as if it is associated to the self-insert-command.
  ;; Given the large character set, it is a reasonable compromise.
  (define (handle-command h ch loop redisp)
    (cond
     [(eof-object? h) (eofread)]
     [(applicable? h <top> <top> <top>)
      (match (h ctx buffer ch)
        [(? eof-object?) (eofread)]
        ['nop       (loop redisp h)]
        ['visible   (reset-last-yank! ctx)
                    (clear-mark! ctx buffer)
                    (break-undo-sequence! ctx)
                    (loop #t h)]
        ['unchanged (reset-last-yank! ctx)
                    (break-undo-sequence! ctx)
                    (loop redisp h)]
        ['redraw    (reset-cursor-pos ctx)
                    (loop #t h)]
        ['moved     (reset-last-yank! ctx)
                    (break-undo-sequence! ctx)
                    (loop #t h)]
        ['commit
         ;; We move the cursor to the last of input and redisplay,
         ;; so that the output of the client program won't overwrite
         ;; the existing input.
         (gap-buffer-move! buffer 0 'end)
         (redisplay ctx buffer)
         (putstr (~ ctx'console) "\r\n")
         (commit-history ctx buffer)]
        ['undone (reset-last-yank! ctx)
                 (clear-mark! ctx buffer)
                 (loop #t h)] ; don't break undo sequence
        [('yanked edit-command)
         (break-undo-sequence! ctx)
         (clear-mark! ctx buffer)
         (push-undo! ctx edit-command)
         (loop #t h)]
        [(? list? edit-command)
         (reset-last-yank! ctx)
         (break-undo-sequence! ctx)
         (clear-mark! ctx buffer)
         (push-undo! ctx edit-command)
         (loop #t h)]
        [x (error "[internal] invalid return value from a command:" x)])]
     [(is-a? h <keymap>)
      (let* ([ch (next-keystroke ctx)]
             [h (keymap-ref h ch)])
        (handle-command h ch loop redisp))]
     [else
      (error "[internal] do not know how to handle key:" h)]))

  ;; Internal loop
  ;; Pass true to redisp to cause buffer to be redisplayed before
  ;; accepting a new edit command.
  ;; Returns a string or #<eof>
  (define (edit-loop redisp last-command)
    ;; NB: If next char is ready, don't bother to redisplay and
    ;; just carry over redisp flag.
    (let* ([redisp (if (and redisp (not (chready? (~ ctx'console))))
                     (begin (redisplay ctx buffer) #f)
                     redisp)]
           [ch (next-keystroke ctx)]
           [h (keymap-ref (~ ctx'keymap) ch)])
      (set! (~ ctx'last-command) last-command)
      (handle-command h ch edit-loop redisp)))

  ;; The 'main' routine of the editor.  Called while the console is set up
  ;; for the editing.  Returns edited string.   Can bail out by an error.
  ;; This routine may be reentered if SIGCONT happens.
  (define (edit-main redisp)
    (dynamic-wind
      %sigcont-observe
      (^[]
        (ensure-bottom-room (~ ctx'console)) ; workaround for windows IME glitch
        (when redisp (reset-terminal (~ ctx'console)))
        (show-prompt ctx)
        (init-screen-params ctx)
        (edit-loop redisp #f))
      %sigcont-unobserve))

  ;;
  ;; Main body
  ;;
  (reset-undo-info! ctx)
  (reset-last-yank! ctx)
  (let reenter ([refresh? #f])
    (let1 r (guard (e [(and (<unhandled-signal-error> e)
                            (eqv? (~ e'signal) SIGINT))
                       (display "\nInput interrupted\n")
                       ""]
                      [(eq? e 'sigcont-received) e]
                      [else (raise e)])
              (call-with-console (~ ctx'console) (^_ (edit-main refresh?))))
    (if (eq? r 'sigcont-received)
      (reenter #t)
      r))))

;; Check some parameters of screen.
(define (init-screen-params ctx)
  (receive (y x) (query-cursor-position (~ ctx'console))
    (set! (~ ctx'initpos-y) y)
    (set! (~ ctx'lastpos-y) y)
    (set! (~ ctx'initpos-x) x)
    (set! (~ ctx'lastpos-x) x))
  (receive (h w) (query-screen-size (~ ctx'console))
    (set! (~ ctx'screen-height) h)
    (set! (~ ctx'screen-width) w)
    (if (~ ctx'framebuffer)
      (init-framebuffer (~ ctx'framebuffer) w h (~ ctx'wide-char-pos-setting)))))

;; If the cursor position has been moved from the supposed position,
;; redisplay the prompt and reset initial position.  This is called
;; when something may be put on the screen and we don't want to
;; clobber it.
(define (reset-cursor-pos ctx)
  (receive (y x) (query-cursor-position (~ ctx'console))
    (unless (and (= (~ ctx'lastpos-y) y)
                 (= (~ ctx'lastpos-x) x))
      (move-cursor-to (~ ctx'console) y 0)
      (show-prompt ctx)
      (receive (y x) (query-cursor-position (~ ctx'console))
        (set! (~ ctx'initpos-y) y)
        (set! (~ ctx'initpos-x) x)))))

;; Fetch next keystroke.
;; Note: If we've received SIGCONT and the terminal has been turned back to
;; the canonical mode, it is likely that we've stopped and then resumed
;; by shell job control.  The user may have tried several keystrokes, but
;; they won't be sent to us until the user type Enter.  We'll discard all
;; those keys, then raises 'sigcont-received, which causes read-line/edit
;; to reset the terminal mode and redisplay the buffer.
(define (next-keystroke ctx)
  (if (queue-empty? (~ ctx'keystroke-queue))
    (let1 ch (getch (~ ctx'console))
      (if (and (%sigcont-received?)
               (canonical-mode? (~ ctx'console)))
        (let loop ()
          (let1 ch (getch (~ ctx'console) 10000) ;10ms wait
            (unless ch (raise 'sigcont-received))
            (loop)))
        ch))
    (queue-pop! (~ ctx'keystroke-queue))))

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

;; Get a tab character width
(define (get-tab-char-width ctx x w)
  (define tab-char-width (~ ctx'tab-char-width))
  (min (- w x) (- tab-char-width (modulo x tab-char-width))))

(define (redisplay ctx buffer)
  (if (~ ctx'framebuffer)
    (let ([real-console (~ ctx'console)])
      (set! (~ ctx'console) (~ ctx'framebuffer))
      (%redisplay ctx buffer)
      ;; maybe we should guard and reset ctx'console anyway to be safe...
      (set! (~ ctx'console) real-console)
      (draw-framebuffer (~ ctx'framebuffer) real-console (~ ctx'initpos-y) 0))
    (%redisplay ctx buffer)))

(define (%redisplay ctx buffer)

  ;; check a initial position
  (if (< (~ ctx'initpos-y) 0)
    (set! (~ ctx'initpos-y) 0))

  (let* ([con     (~ ctx'console)]
         [y       (~ ctx'initpos-y)]
         [x       (~ ctx'initpos-x)]
         [w       (~ ctx'screen-width)]
         [h       (~ ctx'screen-height)]
         [sel     (selected-range ctx buffer)]
         [oparen  (buffer-find-matching-paren-on-cursor buffer)]
         [oldattr '(#f #f)]
         [newattr '(#f #f)]
         [disp-x  x]
         [g       (gap-buffer->generator buffer)]
         [pos     (gap-buffer-pos buffer)]
         [pos-x   x]
         [pos-y   y]
         [pos-set-flag (= pos 0)]
         [maxy    #f])

    (define (display-area?)
      (and (>= y 0) (or (not maxy) (<= y maxy))))

    (define (line-wrapping disp-x1 w :optional (full-column-flag #f))
      (when (>= disp-x1 w)
        (set! x      0)
        (set! disp-x 0)
        (cond
         [(display-area?)
          (move-cursor-to con y 0)

          ;; move cursor to the next line
          (let1 dy (cursor-down/scroll-up con y h full-column-flag)
            (set! y (+ y dy))
            (set! (~ ctx'initpos-y) (+ (~ ctx'initpos-y) (- dy 1)))
            (when pos-set-flag
              (set! pos-y (+ pos-y (- dy 1)))
              ;; check a cursor position for clipping a display area
              (when (<= pos-y 0)
                (set! pos-y 0)
                (set! maxy  (- h 2)))))]

         [else
          (inc! y)])))

    (hide-cursor con)
    (reset-character-attribute con)
    (move-cursor-to con y 0)
    (show-prompt ctx)
    (clear-to-eos con)
    (let loop ([n 0])
      (glet1 ch (g)

        ;; set character attributes
        (set! newattr (current-char-attr n sel oparen))
        (switch-char-attr-when-needed con oldattr newattr)
        (set! oldattr newattr)

        ;; display a character and do line wrapping
        (case ch
          [(#\newline)
           (line-wrapping w w)
           (when (display-area?)
             (switch-char-attr-when-needed con newattr '(#f #f))
             (show-secondary-prompt ctx)
             (switch-char-attr-when-needed con '(#f #f) newattr))
           (set! x      (~ ctx'initpos-x))
           (set! disp-x x)]
          [(#\tab)
           (let1 tw (get-tab-char-width ctx disp-x w)
             (when (display-area?)
               (move-cursor-to con y x)
               (putstr con (make-string tw #\space)))
             (set! x      (+ x      tw))
             (set! disp-x (+ disp-x tw))
             (line-wrapping disp-x w #t))]
          [else
           (let ([dw (get-char-width (~ ctx'wide-char-disp-setting) ch)]
                 [pw (get-char-width (~ ctx'wide-char-pos-setting)  ch)])
             ;; wide characters need a check of line wrapping before
             ;; displaying them
             (if (> dw 1) (line-wrapping (+ disp-x dw) (+ w 1)))
             (when (display-area?)
               (move-cursor-to con y x)
               (putch con ch))
             (set! x      (+ x      pw))
             (set! disp-x (+ disp-x dw))
             (line-wrapping disp-x w #t))])

        ;; set a cursor position
        (when (= pos (+ n 1))
          (set! pos-set-flag #t)
          (set! pos-x x)
          (set! pos-y y))

        (loop (+ n 1))))
    (move-cursor-to con pos-y pos-x)
    (set! (~ ctx'lastpos-y) pos-y)
    (set! (~ ctx'lastpos-x) pos-x)
    (show-cursor con)))

(define (current-char-attr pos sel oparen)
  (cond-list
   [#t @ '(#f #f)]
   [(and sel (<= (car sel) pos) (< pos (cdr sel))) @ '(bright underscore)]
   [(eqv? pos oparen) 'reverse]))

(define (switch-char-attr-when-needed con oldattr newattr)
  (unless (equal? oldattr newattr)
    (if (equal? newattr '(#f #f))
      (reset-character-attribute con)
      (set-character-attribute con newattr))))

;; Show message at the starting point of the current input.
;; Redraw the current buffer below it.
;; (Existing input is overwritten.)
(define (show-message ctx buffer msg :optional (attr #f))
  (move-cursor-to (~ ctx'console) (~ ctx'initpos-y) 0)
  (%draw-lines-with-attr ctx msg attr))

;; Show message below the line where the cursor is at.
;; Redraw the entire input after that.
(define (show-message-below ctx buffer msg :optional (attr #f))
  (ensure-bottom-room (~ ctx'console))
  (cursor-down/scroll-up (~ ctx'console))
  (putch (~ ctx'console) #\return)
  (%draw-lines-with-attr ctx msg attr))

(define (%draw-lines-with-attr ctx msg attr)
  (when attr
    (set-character-attribute (~ ctx'console) attr))
  (unwind-protect
      (dolist [line (string-split msg #\newline)]
        (clear-to-eos (~ ctx'console))
        (putstr (~ ctx'console) line)
        (cursor-down/scroll-up (~ ctx'console))
        (putch (~ ctx'console) #\return)
        (ensure-bottom-room (~ ctx'console))
        (when (< (~ ctx'initpos-y) (- (~ ctx'screen-height) 1))
          (inc! (~ ctx'initpos-y))))
    (when attr
      (reset-character-attribute (~ ctx'console)))))

;;
;; Key combinations
;;

(define (ctrl k)
  (integer->char (- (logand (char->integer k) (lognot #x20)) #x40)))
(define (alt k) `(ALT ,k))

(define (macro! ctx . keys)
  (apply enqueue! (~ ctx'keystroke-queue) keys)
  'nop)

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

(define (duplicated-history? ctx str)
  (and (not (ring-buffer-empty? (~ ctx'history)))
       (string=? (ring-buffer-front (~ ctx'history)) str)))

;; Enter the current buffer contents as the history, and discard any
;; transient info.  Returns the current buffer content.
(define (commit-history ctx buffer)
  (rlet1 str (gap-buffer->string buffer)
    (unless (or (string=? str "\n")
                (string=? str "")
                (duplicated-history? ctx str))
      (ring-buffer-add-front! (~ ctx'history) str))
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

;; Scan open paren from START, and when found, search matching close
;; paren.  When found, call found-fn with to indexes, the
;; location of open paren and close paren.
;; Returns (values <index-of-close-paren> <found-fn-result>).
;; We'd scan recursively, but once found-fn returns a true value,
;; we stop scanning and return those values to the top.
(define (buffer-scan-matching-parens buf start end found-fn)
  (define parens '((#\( . #\)) (#\[ . #\]) (#\{ . #\})))
  (define (scan i open-pos closer)
    (if (= i end)
      (values #f #f)
      (let1 ch (gap-buffer-ref buf i)
        (cond [(eqv? ch closer) (values i (found-fn open-pos i closer))]
              [(eqv? ch #\\) (if (= i (- end 1))
                               (values #f #f)
                               (scan (+ i 2) open-pos closer))]
              [(eqv? ch #\") (in-string (+ i 1) #\" open-pos closer)]
              [(eqv? ch #\#)
               (if (= i (- end 1))
                 (values #f #f)
                 (case (gap-buffer-ref buf (+ i 1))
                   [(#\" #\/) => (^c (in-string (+ i 2) c open-pos closer))]
                   [else (scan (+ i 1) open-pos closer)]))]
              [(assq-ref parens ch) =>
               (^[closer2]
                 (receive (close-pos result) (scan (+ i 1) i closer2)
                   (cond [result (values close-pos result)]
                         [close-pos (scan (+ close-pos 1) open-pos closer)]
                         [else (values #f #f)])))]
              [else (scan (+ i 1) open-pos closer)]))))
  (define (in-string i delim open-pos closer)
    (if (= i end)
      (values #f #f)
      (let1 ch (gap-buffer-ref buf i)
        (cond [(eqv? ch delim) (scan (+ i 1) open-pos closer)]
              [(eqv? ch #\\) (if (= i (- end 1))
                               (values #f #f)
                               (in-string (+ i 2) delim open-pos closer))]
              [else (in-string (+ i 1) delim open-pos closer)]))))
  (scan start #f #f))

;; Given closing paren position, find matching opening paren.
;; START limits the search region (search performed between START and
;; CURRENT-POS).  The character at current-pos is used as the closer.
;; Returns an index of matching opening paren, or #f.
(define (buffer-find-matching-paren buf start current-pos)
  (receive (close-pos open-pos)
      ($ buffer-scan-matching-parens buf start (+ current-pos 1)
         (^[open-pos close-pos closer]
           (and (eq? close-pos current-pos) ; found
                open-pos)))
    open-pos))

;; Similar to above, but take the current cursor pos of the buffer.
(define (buffer-find-matching-paren-on-cursor buf)
  (and (not (gap-buffer-gap-at? buf 'end))
       (memv (gap-buffer-ref buf (gap-buffer-pos buf))
             '(#\) #\] #\}))
       (buffer-find-matching-paren buf 0 (gap-buffer-pos buf))))

;; Pick word at the current position of BUF.
;; Returns three values: The word, start position, and end position.
;; The first word can be #f if the word isn't found.
;; If the position doesn't have a word-constituent character, but
;; the immediate left position has, then we pick the word immediately
;; left on the position.
;; This is mainly to extract a word to be used for completion.  We don't
;; do precise word segmentation (considering grapheme clusters); we just
;; define 'word' as a consecutive characters in word-chars charset.
(define (buffer-find-word buf word-chars)
  (define (scan-backward pos)
    (cond [(= pos 0) 0]
          [(word-chars (gap-buffer-ref buf (- pos 1))) (scan-backward (- pos 1))]
          [else pos]))
  (define (scan-forward pos)
    (cond [(gap-buffer-pos-at-end? buf pos) pos]
          [(word-chars (gap-buffer-ref buf pos)) (scan-forward (+ pos 1))]
          [else pos]))

  (let* ([pos (gap-buffer-pos buf)]
         [start (scan-backward pos)]
         [end (scan-forward pos)])
    (values (if (= start end) #f (gap-buffer->string buf start end))
            start
            end)))

;;;
;;; Commands
;;;

;; Editor command, invoked by specific key sequence.
;; Its handler is a procedure that takes <line-edit-context>,
;; <gap-buffer>, and <key>.
;; Return value indicates the action to be taken by the main editor
;; loop.  See the read-line/edit above for the details.

(define-class <edit-command> ()
  ((name      :init-keyword :name)
   (handler   :init-keyword :handler) ; handler
   (docstring :init-keyword :docstring)     ; document string

   (all-commands :allocation :class
                 :init-form (make-hash-table 'eq?))
   ))
(define-method initialize ((c <edit-command>) initargs)
  (next-method)
  (hash-table-put! (~ c'all-commands) (~ c'name) c))
(define-method object-apply ((c <edit-command>) ctx buf key)
  ((~ c'handler) ctx buf key))

(define-syntax define-edit-command
  (syntax-rules ()
    [(_ (name . args) docstring . body)
     (define name (make <edit-command>
                    :name 'name
                    :handler (lambda args . body)
                    :docstring docstring))]))


(define-edit-command (self-insert-command ctx buf key)
  "Insert typed character at the cursor, and position the cursor after it."
  (reset-last-yank! ctx)
  (break-undo-sequence! ctx)
  (clear-mark! ctx buf)
  (gap-buffer-edit! buf `(i #f ,(x->string key))))

(define-edit-command (quoted-insert ctx buf key)
  "Read next keystroke and insert it into the buffer at the cursor."
  (let1 ch (next-keystroke ctx) ; TODO: octal digits input
    (gap-buffer-edit! buf `(i #f ,(x->string ch)))))

(define-edit-command (insert-parentheses ctx buf key)
  "Insert a pair of parentheses at the cursor, and position the cursor \
   between them."
  (macro! ctx #\( #\) (ctrl #\b)))

(define-edit-command (commit-input ctx buf key)
  "Commit the current buffer content as the finished input."
  'commit)

(define-edit-command (commit-or-newline ctx buf key)
  "If the current buffer content consists a complete input, \
   commit it as the finished input.  Otherwise, insert a newline."
  (or (and-let* ([pred (~ ctx'input-continues)]
                 [ (pred (gap-buffer->string buf)) ])
        (gap-buffer-edit! buf '(i #f "\n")))
      'commit))

(define-edit-command (delete-char ctx buf key)
  "Delete a single character at the cursor. \
   If the cursor is at the end of the buffer, do nothing."
  (if (not (gap-buffer-gap-at? buf 'end))
    (gap-buffer-edit! buf '(d #f 1))
    'unchanged))

(define-edit-command (eot-or-delete-char ctx buf key)
  "If the buffer is empty, commit input as EOF.  Otherwise, delete \
   one character at the cursor."
  (if (zero? (gap-buffer-content-length buf))
    (eof-object)
    (delete-char ctx buf key)))

(define-edit-command (delete-backward-char ctx buf key)
  "Delete one character right before the cursor. \
   If the current point is beginning of the buffer, do nothing."
  (if (not (gap-buffer-gap-at? buf 'beginning))
    (let1 p-1 (- (gap-buffer-pos buf) 1)
      (gap-buffer-edit! buf `(d ,p-1 1)))
    'unchanged))

(define-edit-command (backward-char ctx buf key)
  "Move the cursor one character backward.  If the cursor is at the beginning, \
   do nothing."
  (if (not (gap-buffer-gap-at? buf 'beginning))
    (begin (gap-buffer-move! buf -1 'current)
           'moved)
    'unchanged))

(define-edit-command (forward-char ctx buf key)
  "Move the cursor one character forward.  If the cursor is at the end of the \
   buffer, do nothing."
  (if (not (gap-buffer-gap-at? buf 'end))
    (begin (gap-buffer-move! buf 1 'current)
           'moved)
    'unchanged))

(define (move-word! buf move-offset cset peek-offset)
  (let loop ([result #f])
    (cond
     [(gap-buffer-gap-at? buf (if (negative? move-offset) 'beginning 'end))
      result]
     [(char-set-contains? cset
                          (gap-buffer-ref buf
                                          (+ (gap-buffer-pos buf) peek-offset)
                                          #\space))
      (begin
        (gap-buffer-move! buf move-offset 'current)
        (loop #t))]
     [else
      result])))

(define-edit-command (backward-word ctx buf key)
  "Move the cursor one word backward."
  (let* ([res1 (move-word! buf -1 #[\W] -1)]
         [res2 (move-word! buf -1 #[\w] -1)])
    (if (or res1 res2) 'moved 'unchanged)))

(define-edit-command (forward-word ctx buf key)
  "Move the cursor one word forward."
  (let* ([res1 (move-word! buf 1 #[\W] 0)]
         [res2 (move-word! buf 1 #[\w] 0)])
    (if (or res1 res2) 'moved 'unchanged)))

(define-edit-command (move-beginning-of-buffer ctx buf key)
  "Position the cursor at the beginning of the buffer."
  (if (not (gap-buffer-gap-at? buf 'beginning))
    (begin (gap-buffer-move! buf 0 'beginning)
           'moved)
    'unchanged))

(define-edit-command (move-end-of-buffer ctx buf key)
  "Position the cursor at the end of the buffer."
  (if (not (gap-buffer-gap-at? buf 'end))
    (begin (gap-buffer-move! buf 0 'end)
           'moved)
    'unchanged))

(define-edit-command (move-beginning-of-line ctx buf key)
  "Move the cursor at the beginning of the current line."
  (match-let1 (lines . col) (buffer-current-line&col buf)
    (if (zero? col)
      'unchanged
      (begin
        (buffer-set-line&col! buf lines 0)
        'moved))))

(define-edit-command (move-end-of-line ctx buf key)
  "Move the cursor at the end of the current line."
  (match-let1 (lines . col) (buffer-current-line&col buf)
    (buffer-set-line&col! buf lines 9999)
    'moved))

(define-edit-command (set-mark-command ctx buf key)
  "Set mark at the current cursor position."
  (set-mark! ctx buf)
  'moved)

(define-edit-command (kill-line ctx buf key)
  "If the cursor is at the end of line, delete the newline character and \
   combine the line with the next line.  Otherwise, delete characters from \
   the cursor to the end of the line."
  (cond
   [(gap-buffer-gap-at? buf 'end)
    'unchanged]
   [(eqv? (gap-buffer-ref buf (gap-buffer-pos buf)) #\newline)
    (macro! ctx (ctrl #\d))]
   [else
    (macro! ctx (ctrl #\@) (ctrl #\e) (ctrl #\w))]))

(define-edit-command (kill-region ctx buf key)
  "Delete characters between the cursor and the mark, and save them into \
   the kill ring.  If there's no active mark, do nothing."
  (match (selected-range ctx buf)
    [(start . end)
     ;; NB: the cursor is either on start or on end.  either way,
     ;; after operation the cursor's be at start.
     (rlet1 e (gap-buffer-edit! buf `(d ,start ,(- end start)))
       ;; e contains (i <pos> <killed-string>)
       (save-kill-ring ctx (caddr e)))]
    [_ 'unchanged]))

(define-edit-command (kill-ring-save ctx buf key)
  "Save the characters between the cursor and the mark into the kill-ring. \
   The contents of the buffer isn't changed, but the mark is cleared."
  (match (selected-range ctx buf)
    [(start . end)
     (save-kill-ring ctx (gap-buffer->string buf start end))
     'visible] ; this clears selection
    [_ 'unchanged]))

(define-edit-command (kill-word ctx buf key)
  "Delete characters from the cursor to the end of the word, and save them \
   into the kill ring.  The mark is cleared."
  (macro! ctx (ctrl #\@) (alt #\f) (ctrl #\w)))

(define-edit-command (backward-kill-word ctx buf key)
  "Delete characters from the cursor to the beginning of the word, and save \
   them into the kill ring.  The mark is cleared."
  (macro! ctx (ctrl #\@) (alt #\b) (ctrl #\w)))

(define-edit-command (refresh-display ctx buf key)
  "Redraw contents of the buffer."
  (reset-terminal (~ ctx'console))
  (move-cursor-to (~ ctx'console) 0 0) ;redundant, but mintty has problem without this
  (show-prompt ctx)
  (init-screen-params ctx)
  'visible)

;; NB: This command may modify undo queue
(define-edit-command (prev-history ctx buf key)
  "Replace the buffer content with the last item in the history currently \
   looking at."
  (if (< (history-pos ctx) (- (history-size ctx) 1))
    (begin
      (save-history-transient ctx buf)
      (inc! (~ ctx'history-pos))
      (let1 p (get-history ctx)
        (set-undo-info! ctx (cdr p))
        (gap-buffer-clear! buf)
        (gap-buffer-insert! buf (car p))
        'visible))
    'unchanged))

;; NB: This command may modify undo queue
(define-edit-command (next-history ctx buf key)
  "Replace the buffer content with the next item in the history currently \
   looking at."
  (if (> (history-pos ctx) -1)
    (begin
      (save-history-transient ctx buf)
      (dec! (~ ctx'history-pos))
      (let1 p (get-history ctx)
        (set-undo-info! ctx (cdr p))
        (gap-buffer-clear! buf)
        (gap-buffer-insert! buf (car p))
        'visible))
    'unchanged))

(define-edit-command (prev-line-or-history ctx buf key)
  "If the cursor is at the first line, do prev-sitory.  Otherwise, \
   move the cursor one line up."
  (match-let1 (lines . col) (buffer-current-line&col buf)
    (if (zero? lines)
      (prev-history ctx buf key)
      (begin (buffer-set-line&col! buf (- lines 1) col) 'moved))))

(define-edit-command (next-line-or-history ctx buf key)
  "If the cursor is at the last line, do next-sitory.  Otherwise, \
   move the cursor one line down."
  (match-let1 (lines . col) (buffer-current-line&col buf)
    (if (= lines (buffer-num-lines buf))
      (next-history ctx buf key)
      (begin (buffer-set-line&col! buf (+ lines 1) col) 'moved))))


(define-edit-command (transpose-chars ctx buf key)
  "Exchange characters on the cursor and before the cursor and position \
   the cursor one character before.  If the buffer is empty, do nothing. \
   If the buffer has only one character, just move the cursor."
  (cond [(gap-buffer-gap-at? buf 'beginning) 'unchanged]
        [(= (gap-buffer-content-length buf) 1) ; special case
         (gap-buffer-move! buf 0)
         'moved]
        [else
         (gap-buffer-move! buf
                           (if (gap-buffer-gap-at? buf 'end) -2 -1)
                           'current)
         (let ([cur  (gap-buffer-ref buf (+ (gap-buffer-pos buf) 1))]
               [prev (gap-buffer-ref buf (gap-buffer-pos buf))])
           (gap-buffer-edit! buf `(c #f 2 ,(string cur prev))))]))

(define-edit-command (yank ctx buf key)
  "Insert the last chunk in the kill ring at the cursor \
   position."
  (if (> (ring-buffer-num-entries (~ ctx'kill-ring)) 0)
    (begin
      (set! (~ ctx'last-yank) 0)
      (set! (~ ctx'last-yank-pos) (gap-buffer-pos buf))
      (let1 yanked-text (get-yank-line ctx)
        (set! (~ ctx'last-yank-size) (string-length yanked-text))
        `(yanked ,(gap-buffer-edit! buf `(i #f ,yanked-text)))))
    'unchanged))

(define-edit-command (yank-pop ctx buf key)
  "Replace just yanked chunk of characters with the other chunk in the kill \
   ring."
  (if (and (> (ring-buffer-num-entries (~ ctx'kill-ring)) 0)
           (>= (~ ctx'last-yank) 0))
    (let* ([text (pop-yank-line ctx)]
           [pos  (~ ctx'last-yank-pos)]
           [len  (~ ctx'last-yank-size)]
           [edit (gap-buffer-edit! buf `(c ,pos ,len ,text))])
      (set! (~ ctx'last-yank-size) (string-length text))
      `(yanked ,edit))
    'unchanged))

(define-edit-command (undo ctx buf key)
  "Undo the last change."
  (if (not (queue-empty? (~ ctx'undo-stack)))
    (let* ([undo-command (queue-pop! (~ ctx'undo-stack))]
           [redo-command (gap-buffer-edit! buf undo-command)])
      (queue-push! (~ ctx'redo-queue) undo-command)
      (enqueue! (~ ctx'redo-queue) redo-command)
      'undone)
    'unchanged))

(define-edit-command (keyboard-quit ctx buf key)
  "Reset yank sequence, clear a mark, and break undo sequence.  Basically, \
   break whatever work that spans for several commands."
  (beep (~ ctx'console))
  'visible)

(define-edit-command (edit-with-editor ctx buf key)
  "Edit the current input buffer with the external editor"
  (let* ([orig (gap-buffer->string buf)]
         [edited (ed-string orig)])
    (cond
     [(not edited)
      (show-message ctx buf "External editor is not available."
                    '(#f #f reverse))
      'redraw]
     [(equal? orig edited)
      'unchanged]
     [else
      (gap-buffer-clear! buf)
      (gap-buffer-insert! buf edited)
      'visible])))

(define-edit-command (undefined-command ctx buf key)
  "Placeholder for a keystroke that isn't assigned to any command."
  (beep (~ ctx'console))
  (show-message ctx buf
                #"Unknown keystroke ~(keys->string '() key). \
                  Type M-h b for the list of key bindings."
                '(#f #f reverse))
  'redraw)

(define-edit-command (undefined-or-self-insert-command ctx buf key)
  "Insert the key if its a character beyond ascii range, otherwise behave \
   as the undefined command."
  (if (and (char? key)
           (> (char->integer key) #x7f))
    (self-insert-command ctx buf key)
    (undefined-command ctx buf key)))

(define-edit-command (nop-command ctx buf key)
  "Do nothing."
  'nop)

(define-edit-command (completion-command ctx buf key)
  "Try to complete the (partial) word at the cursor."
  (receive (word start-pos end-pos)
      (buffer-find-word buf (~ ctx'completion-word-constituent?))
    (or (and-let* ([ word ]
                   [lister (~ ctx'completion-lister)]
                   [words (lister word buf start-pos end-pos)])
          (match words
            [() #f]
            [(w)
             (gap-buffer-move! buf start-pos)
             (gap-buffer-replace! buf (- end-pos start-pos) w)
             'redraw]
            [(w ws ...)
             (when (eq? (~ ctx'last-command) completion-command)
               (let1 candidates
                   (with-output-to-string
                     (cut display-multicolumn (cons w ws)))
                 ;; NB: output of display-multicolumn contains newline at the
                 ;; end.  We don't want to display it.
                 ($ show-message-below ctx buf
                    (substring candidates 0 (- (string-length candidates) 1)))))
             (let1 pre (fold common-prefix w ws)
               (gap-buffer-move! buf start-pos)
               (gap-buffer-replace! buf (- end-pos start-pos) pre)
               'redraw)]))
        'nop)))

(define-edit-command (help-binding-command ctx buf key)
  "Show key bindings."
  (display/pager
   (with-output-to-string
     (^[] (print "Line editor key bindings:") (print)
       (keymap-describe-recursively (~ ctx'keymap)))))
  'redraw)

(define-edit-command (help-summary-command ctx buf key)
  "General help string"
  (let ((dkm (default-keymap)))
    (define (lookup name)
      (command-name->keystroke-string dkm name))
    (define Help-b (lookup 'help-binding-command))
    (define Help-k (lookup 'help-keystroke-command))
    (define C-f    (lookup 'forward-char))
    (define C-b    (lookup 'backward-char))
    (define C-p    (lookup 'prev-line-or-history))
    (define C-n    (lookup 'next-line-or-history))
    (define C-a    (lookup 'move-beginning-of-line))
    (define C-e    (lookup 'move-end-of-line))
    (define M-f    (lookup 'forward-word))
    (define M-b    (lookup 'backward-word))
    (define M-p    (lookup 'prev-history))
    (define M-n    (lookup 'next-history))
    (define M-<    (lookup 'move-beginning-of-buffer))
    (define M->    (lookup 'move-end-of-buffer))
    (define C-@    (lookup 'set-mark-command))
    (define C-w    (lookup 'kill-region))
    (define C-k    (lookup 'kill-line))
    (define M-d    (lookup 'kill-word))
    (define C-y    (lookup 'yank))
    (define M-y    (lookup 'yank-pop))
    (define C-_    (lookup 'undo))
    (define M-C-x  (lookup 'commit-input))
    (define C-l    (lookup 'refresh-display))

    (display/pager
     #"\
 Gauche input editing quick cheat sheet:
  ~Help-b                  for keymap
  ~Help-k <keystroke> ...  for help of specific keystroke

  ~|C-f|/~C-b   forward/backward char        ~|M-f|/~M-b   forward/backward word
  ~|C-p|/~C-n   prev/next line or history    ~|M-p|/~M-n   prev/next history
  ~|C-a|/~C-e   beginning/end line           ~|M-<|/~M->   beginning/end buffer
  ~|C-@|       mark                         ~|C-w|       kill region
  ~|C-k|       kill line                    ~|M-d|       kill word
  ~|C-y|       yank                         ~|M-y|       yank pop
  ~|C-_|       undo                         ~|M-C-x|     commit input
  ~|C-l|       refresh disiplay
 To disable input editing: Type ,edit off")
    'redraw))

(define-edit-command (help-keystroke-command ctx buf key)
  "Show description of given keystroke(s)."
  (let loop ([ch (next-keystroke ctx)]
             [keymap (~ ctx'keymap)]
             [keystrokes '()])
    (let1 h (keymap-ref keymap ch)
      (cond [(is-a? h <edit-command>)
             (let1 msg (format "~22a ~a\n~a"
                               (keys->string (reverse keystrokes) ch)
                               (~ h'name)
                               (~ h'docstring))
               (show-message ctx buf msg '(#f #f bright)))]
            [(is-a? h <keymap>)
             (loop (next-keystroke ctx) h (cons ch keystrokes))]
            [else #f])))                ;can't happen, but just ignore
  'redraw)

;;;
;;; Keymaps
;;;

;; Keymap maps a keystroke (either <char> or (ALT <char>))
;; to a command or another keymap.
(define-class <keymap> ()
  ((name    :init-keyword :name)
   (table   :init-keyword :table)         ;hashtable
   (default :init-keyword :default)))

;; Stacked keymap is like <keymap>, except that default slot is
;; always a <keymap>, and if a keystroke is not in the table,
;; lookup is delegated to that keymap.
;; (It is distinguished from a <keymap> whose default value is
;; a secondary keymap - in that case, a keystroke falls on the default
;; replaces the current keymap to it.)
(define-class <stacked-keymap> (<keymap>) ())

(define (make-keymap :optional (name #f) (default undefined-command))
  (make <keymap> :name name :table (make-hash-table 'equal?) :default default))

(define (make-stacked-keymap base-keymap :optional (name #f))
  (make <stacked-keymap> :name name :table (make-hash-table 'equal?)
        :default base-keymap))

(define (copy-keymap km :optional (name #f))
  (make (class-of km)
    :name name
    :table (hash-table-copy (~ km'table))
    :default (~ km'default)))

(define-method keymap-ref ((km <keymap>) keystroke)
  (or (hash-table-get (~ km'table) keystroke #f)
      (~ km'default)))

(define-method keymap-ref ((km <stacked-keymap>) keystroke)
  (or (hash-table-ref (~ km'table) keystroke #f)
      (keymap-ref (~ km'default) keystroke)))

(define (command-name->keystrokes km command-name)
  (assume-type km <keymap>)
  (assume-type command-name <symbol>)
  (letrec ([pred (^[km k v]
                   (cond [(is-a? v <keymap>)
                          (and-let1 r (pick v)
                            (cons k r))]
                         [(is-a? v <edit-command>)
                          (and (eq? command-name (~ v'name))
                               (list k))]
                         [else (error "Broken keymap:" km)]))]
           [pick (^[km]
                   ;; NB: We take precedence in ordinary keys to special keys
                   (or ($ hash-table-find (~ km'table)
                          (^[k v] (and (not (symbol? k)) (pred km k v))))
                       ($ hash-table-find (~ km'table)
                          (^[k v] (and (symbol? k) (pred km k v))))))])
    (pick km)))

(define (command-name->keystroke-string km command-name)
  (and-let1 ks (command-name->keystrokes km command-name)
    (let1 s (keys->string ks #f)
      (substring s 0 (- (string-length s) 1))))) ;avoid depending srfi-13

(define-syntax define-key
  (syntax-rules ()
    [(_ km keystroke command)
     (hash-table-put! (~ km'table) keystroke command)]))

(define (keys->string prefixes key)
  (define (key->string k)
    (match k
      [('ALT x) #"M-~(key->string x)"]
      [(? symbol?) (x->string k)]
      [#\space "SPC"]
      [#\del   "DEL"]
      [_ (cond
          [(char=? k #\null) "C-@"]
          [(char<=? k #\x1a)  #"C-~(integer->char (+ (char->integer k) 96))"]
          [(char<? k #\space) #"C-~(integer->char (+ (char->integer k) 64))"]
          [else (string k)])]))
  (string-append (if (null? prefixes)
                   ""
                   (string-join (map (cut keys->string '() <>) prefixes)
                                " " 'suffix))
                 (if key
                   (key->string key)
                   "")))

(define (keymap-describe km :optional (prefix-keys '()))
  (define (show-key k)
    (match k
      [('ALT x) #"M-~(show-key x)"]
      [(? symbol?) (x->string k)]
      [#\space "SPC"]
      [#\del   "DEL"]
      [_ (cond
          [(char=? k #\null) "C-@"]
          [(char<=? k #\x1a)  #"C-~(integer->char (+ (char->integer k) 96))"]
          [(char<? k #\space) #"C-~(integer->char (+ (char->integer k) 64))"]
          [else (string k)])]))
  (define (key<? x y)
    (match x
      [('ALT xx)
       (match y
         [('ALT yy) (key<? xx yy)]
         [_ #f])]
      [(? symbol?)
       (match y
         [('ALT _) #t]
         [(? symbol?) (<? default-comparator x y)]
         [_ #f])]
      [_ (if (char? y)
           (char<? x y)
           #t)]))
  (define (show-value v)
    (cond [(is-a? v <edit-command>) (~ v'name)]
          [(is-a? v <keymap>) (or #"(~(~ v'name))"
                                  "(anonymous keymap)")]
          [else v]))      ;shouldn't happen
  (define (show-entry-1 k v)
    (format #t " ~20a  ~a\n" (keys->string prefix-keys k) (show-value v)))
  (define (show-entry-n k0 k1 v)
    (when k0
      (if (equal? k0 k1)
        (show-entry-1 k0 v)
        (format #t " ~20a  ~a\n"
                #"~(keys->string prefix-keys k0) .. ~(keys->string '() k1)"
                (show-value v)))))
  (let loop ([ks (sort (hash-table-keys (~ km'table)) key<?)]
             [group-start #f]
             [group-end #f])
    (if (null? ks)
      (show-entry-n group-start group-end self-insert-command)
      (let1 v (hash-table-get (~ km'table) (car ks))
        (if (eq? v self-insert-command)
          (loop (cdr ks) (or group-start (car ks)) (car ks))
          (begin
            (show-entry-n group-start group-end self-insert-command)
            (show-entry-1 (car ks) v)
            (loop (cdr ks) #f #f)))))))

(define (keymap-describe-recursively km)
  (define (rec km prefixes)
    (print (or (~ km'name) "(anonymous keymap)"))
    (keymap-describe km prefixes)
    (hash-table-for-each
     (~ km'table)
     (^[k v] (when (is-a? v <keymap>)
               (print)
               (rec v (append prefixes (list k)))))))
  (rec km '()))

;; M-h - help keymap
(define *help-keymap* (make-keymap "Help keymap"))

(define-key *help-keymap* #\b help-binding-command)
(define-key *help-keymap* #\h help-summary-command)
(define-key *help-keymap* #\k help-keystroke-command)

;; C-x - general prefix
(define *c-x-keymap* (make-keymap "C-x prefix"))

(define-key *c-x-keymap* #\e edit-with-editor)

;; Default keymap.
(define *default-keymap* (make-keymap "Default keymap"
                                       undefined-or-self-insert-command))

(define-key *default-keymap* (ctrl #\@) set-mark-command)
(define-key *default-keymap* (ctrl #\a) move-beginning-of-line)
(define-key *default-keymap* (ctrl #\b) backward-char)
;;(define-key *default-keymap* (ctrl #\c) undefined-command)
(define-key *default-keymap* (ctrl #\d) eot-or-delete-char)
(define-key *default-keymap* (ctrl #\e) move-end-of-line)
(define-key *default-keymap* (ctrl #\f) forward-char)
(define-key *default-keymap* (ctrl #\g) keyboard-quit)
(define-key *default-keymap* (ctrl #\h) delete-backward-char)
(define-key *default-keymap* (ctrl #\i) completion-command)  ; tab
(define-key *default-keymap* (ctrl #\j) self-insert-command) ; newline
(define-key *default-keymap* (ctrl #\k) kill-line)
(define-key *default-keymap* (ctrl #\l) refresh-display)
(define-key *default-keymap* (ctrl #\m) commit-or-newline) ; return
(define-key *default-keymap* (ctrl #\n) next-line-or-history)
;;(define-key *default-keymap* (ctrl #\o) undefined-command)
(define-key *default-keymap* (ctrl #\p) prev-line-or-history)
(define-key *default-keymap* (ctrl #\q) quoted-insert)
;;(define-key *default-keymap* (ctrl #\r) undefined-command)
;;(define-key *default-keymap* (ctrl #\s) undefined-command)
(define-key *default-keymap* (ctrl #\t) transpose-chars)
;;(define-key *default-keymap* (ctrl #\u) undefined-command)
;;(define-key *default-keymap* (ctrl #\v) undefined-command)
(define-key *default-keymap* (ctrl #\w) kill-region)
(define-key *default-keymap* (ctrl #\x) *c-x-keymap*)
(define-key *default-keymap* (ctrl #\y) yank)
;;(define-key *default-keymap* (ctrl #\z) undefined-command)
;;(define-key *default-keymap* (ctrl #\[) undefined-command)
;;(define-key *default-keymap* (ctrl #\\) undefined-command)
;;(define-key *default-keymap* (ctrl #\]) undefined-command)
;;(define-key *default-keymap* (ctrl #\^) undefined-command)
(define-key *default-keymap* (ctrl #\_) undo)

(do ([c #x20 (+ c 1)])
    [(= c #x7f)]
  (define-key *default-keymap* (integer->char c) self-insert-command))

(define-key *default-keymap* (alt #\null) nop-command) ; for windows (ime on/off)
;;(define-key *default-keymap* (alt #\space) undefined-command) ; should be set-mark
;;(define-key *default-keymap* (alt #\!) undefined-command)
;;(define-key *default-keymap* (alt #\") undefined-command)
;;(define-key *default-keymap* (alt #\#) undefined-command)
;;(define-key *default-keymap* (alt #\$) undefined-command)
;;(define-key *default-keymap* (alt #\%) undefined-command)
;;(define-key *default-keymap* (alt #\&) undefined-command)
;;(define-key *default-keymap* (alt #\') undefined-command)
(define-key *default-keymap* (alt #\() insert-parentheses)
;;(define-key *default-keymap* (alt #\)) undefined-command)
;;(define-key *default-keymap* (alt #\*) undefined-command)
;;(define-key *default-keymap* (alt #\+) undefined-command)
;;(define-key *default-keymap* (alt #\,) undefined-command)
;;(define-key *default-keymap* (alt #\-) undefined-command)
;;(define-key *default-keymap* (alt #\.) undefined-command)
;;(define-key *default-keymap* (alt #\/) undefined-command)
;;(define-key *default-keymap* (alt #\0) undefined-command)
;;(define-key *default-keymap* (alt #\1) undefined-command)
;;(define-key *default-keymap* (alt #\2) undefined-command)
;;(define-key *default-keymap* (alt #\3) undefined-command)
;;(define-key *default-keymap* (alt #\4) undefined-command)
;;(define-key *default-keymap* (alt #\5) undefined-command)
;;(define-key *default-keymap* (alt #\6) undefined-command)
;;(define-key *default-keymap* (alt #\7) undefined-command)
;;(define-key *default-keymap* (alt #\8) undefined-command)
;;(define-key *default-keymap* (alt #\9) undefined-command)
;;(define-key *default-keymap* (alt #\:) undefined-command)
;;(define-key *default-keymap* (alt #\;) undefined-command)
(define-key *default-keymap* (alt #\<) move-beginning-of-buffer)
;;(define-key *default-keymap* (alt #\=) undefined-command)
(define-key *default-keymap* (alt #\>) move-end-of-buffer)
;;(define-key *default-keymap* (alt #\?) undefined-command)


;;(define-key *default-keymap* (alt #\@) undefined-command)
;;(define-key *default-keymap* (alt #\A) undefined-command)
;;(define-key *default-keymap* (alt #\B) undefined-command)
;;(define-key *default-keymap* (alt #\C) undefined-command)
;;(define-key *default-keymap* (alt #\D) undefined-command)
;;(define-key *default-keymap* (alt #\E) undefined-command)
;;(define-key *default-keymap* (alt #\F) undefined-command)
;;(define-key *default-keymap* (alt #\G) undefined-command)
;;(define-key *default-keymap* (alt #\H) undefined-command)
;;(define-key *default-keymap* (alt #\I) undefined-command)
;;(define-key *default-keymap* (alt #\J) undefined-command)
;;(define-key *default-keymap* (alt #\K) undefined-command)
;;(define-key *default-keymap* (alt #\L) undefined-command)
;;(define-key *default-keymap* (alt #\M) undefined-command)
;;(define-key *default-keymap* (alt #\N) undefined-command)
;;(define-key *default-keymap* (alt #\O) undefined-command)
;;(define-key *default-keymap* (alt #\P) undefined-command)
;;(define-key *default-keymap* (alt #\Q) undefined-command)
;;(define-key *default-keymap* (alt #\R) undefined-command)
;;(define-key *default-keymap* (alt #\S) undefined-command)
;;(define-key *default-keymap* (alt #\T) undefined-command)
;;(define-key *default-keymap* (alt #\U) undefined-command)
;;(define-key *default-keymap* (alt #\V) undefined-command)
;;(define-key *default-keymap* (alt #\W) undefined-command)
;;(define-key *default-keymap* (alt #\X) undefined-command)
(define-key *default-keymap* (alt #\Y) yank-pop)
;;(define-key *default-keymap* (alt #\Z) undefined-command)
;;(define-key *default-keymap* (alt #\[) undefined-command)
;;(define-key *default-keymap* (alt #\\) undefined-command)
;;(define-key *default-keymap* (alt #\]) undefined-command)
;;(define-key *default-keymap* (alt #\^) undefined-command)
;;(define-key *default-keymap* (alt #\_) undefined-command)

;;(define-key *default-keymap* (alt #\`) undefined-command)
;;(define-key *default-keymap* (alt #\a) undefined-command)
(define-key *default-keymap* (alt #\b) backward-word)
;;(define-key *default-keymap* (alt #\c) undefined-command)
(define-key *default-keymap* (alt #\d) kill-word)
;;(define-key *default-keymap* (alt #\e) undefined-command)
(define-key *default-keymap* (alt #\f) forward-word)
;;(define-key *default-keymap* (alt #\g) undefined-command)
(define-key *default-keymap* (alt #\h) *help-keymap*)
;;(define-key *default-keymap* (alt #\i) undefined-command)
;;(define-key *default-keymap* (alt #\j) undefined-command)
;;(define-key *default-keymap* (alt #\k) undefined-command)
;;(define-key *default-keymap* (alt #\l) undefined-command)
;;(define-key *default-keymap* (alt #\m) undefined-command)
(define-key *default-keymap* (alt #\n) next-history)
;;(define-key *default-keymap* (alt #\o) undefined-command)
(define-key *default-keymap* (alt #\p) prev-history)
;;(define-key *default-keymap* (alt #\q) undefined-command)
;;(define-key *default-keymap* (alt #\r) undefined-command)
;;(define-key *default-keymap* (alt #\s) undefined-command)
;;(define-key *default-keymap* (alt #\t) undefined-command)
;;(define-key *default-keymap* (alt #\u) undefined-command)
;;(define-key *default-keymap* (alt #\v) undefined-command)
(define-key *default-keymap* (alt #\w) kill-ring-save)
;;(define-key *default-keymap* (alt #\x) undefined-command)
(define-key *default-keymap* (alt #\y) yank-pop)
;;(define-key *default-keymap* (alt #\z) undefined-command)
;;(define-key *default-keymap* (alt #\{) undefined-command)
;;(define-key *default-keymap* (alt #\|) undefined-command)
;;(define-key *default-keymap* (alt #\}) undefined-command)
;;(define-key *default-keymap* (alt #\)) undefined-command)
;;(define-key *default-keymap* (alt #\_) undefined-command)
(define-key *default-keymap* (alt #\x7f) backward-kill-word)

(define-key *default-keymap* #\x7f delete-backward-char)

;;(define-key *default-keymap* (alt (ctrl #\space)) undefined-command)
;;(define-key *default-keymap* (alt (ctrl #\a)) undefined-command)
;;(define-key *default-keymap* (alt (ctrl #\b)) undefined-command)
;;(define-key *default-keymap* (alt (ctrl #\c)) undefined-command)
;;(define-key *default-keymap* (alt (ctrl #\d)) undefined-command)
;;(define-key *default-keymap* (alt (ctrl #\e)) undefined-command)
;;(define-key *default-keymap* (alt (ctrl #\f)) undefined-command)
;;(define-key *default-keymap* (alt (ctrl #\g)) undefined-command)
;;(define-key *default-keymap* (alt (ctrl #\h)) undefined-command)
;;(define-key *default-keymap* (alt (ctrl #\i)) undefined-command)
;;(define-key *default-keymap* (alt (ctrl #\j)) undefined-command)
;;(define-key *default-keymap* (alt (ctrl #\k)) undefined-command)
;;(define-key *default-keymap* (alt (ctrl #\l)) undefined-command)
;;(define-key *default-keymap* (alt (ctrl #\m)) undefined-command)
;;(define-key *default-keymap* (alt (ctrl #\n)) undefined-command)
;;(define-key *default-keymap* (alt (ctrl #\o)) undefined-command)
;;(define-key *default-keymap* (alt (ctrl #\p)) undefined-command)
;;(define-key *default-keymap* (alt (ctrl #\q)) undefined-command)
;;(define-key *default-keymap* (alt (ctrl #\r)) undefined-command)
;;(define-key *default-keymap* (alt (ctrl #\s)) undefined-command)
;;(define-key *default-keymap* (alt (ctrl #\t)) undefined-command)
;;(define-key *default-keymap* (alt (ctrl #\u)) undefined-command)
;;(define-key *default-keymap* (alt (ctrl #\v)) undefined-command)
;;(define-key *default-keymap* (alt (ctrl #\w)) undefined-command)
(define-key *default-keymap* (alt (ctrl #\x)) commit-input)
;;(define-key *default-keymap* (alt (ctrl #\y)) undefined-command)
;;(define-key *default-keymap* (alt (ctrl #\z)) undefined-command)
;;(define-key *default-keymap* (alt (ctrl #\[)) undefined-command)
;;(define-key *default-keymap* (alt (ctrl #\\)) undefined-command)
;;(define-key *default-keymap* (alt (ctrl #\])) undefined-command)
;;(define-key *default-keymap* (alt (ctrl #\^)) undefined-command)
;;(define-key *default-keymap* (alt (ctrl #\_)) undefined-command)

(define-key *default-keymap* 'KEY_UP    prev-line-or-history)
(define-key *default-keymap* 'KEY_DOWN  next-line-or-history)
(define-key *default-keymap* 'KEY_LEFT  backward-char)
(define-key *default-keymap* 'KEY_RIGHT forward-char)
;;(define-key *default-keymap* 'KEY_INS ,undefined-command)
(define-key *default-keymap* 'KEY_DEL   delete-char)
;;(define-key *default-keymap* 'KEY_HOME undefined-command)
;;(define-key *default-keymap* 'KEY_END ,undefined-command)
;;(define-key *default-keymap* 'KEY_PGDN undefined-command)
;;(define-key *default-keymap* 'KEY_PGUP undefined-command)
;;(define-key *default-keymap* 'KEY_F1  ,undefined-command)
;;(define-key *default-keymap* 'KEY_F2  ,undefined-command)
;;(define-key *default-keymap* 'KEY_F3  ,undefined-command)
;;(define-key *default-keymap* 'KEY_F4  ,undefined-command)
;;(define-key *default-keymap* 'KEY_F5  ,undefined-command)
;;(define-key *default-keymap* 'KEY_F6  ,undefined-command)
;;(define-key *default-keymap* 'KEY_F7  ,undefined-command)
;;(define-key *default-keymap* 'KEY_F8  ,undefined-command)
;;(define-key *default-keymap* 'KEY_F9  ,undefined-command)
;;(define-key *default-keymap* 'KEY_F10 ,undefined-command)
;;(define-key *default-keymap* 'KEY_F11 ,undefined-command)
;;(define-key *default-keymap* 'KEY_F12 ,undefined-command)

;; external api
(define (default-keymap) *default-keymap*)

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
