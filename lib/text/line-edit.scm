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

(define-class <line-edit-context> ()
  ((console :init-keyword :console :init-form (make-default-console))
   (prompt  :init-keyword :prompt :init-value "")
   (keymap  :init-form (default-keymap))
   (initpos-y)
   (initpos-x)
   (screen-height)
   (screen-width)
   (kill-ring :init-form (make-ring-buffer
                          (make-vector *kill-ring-size*)
                          :overflow-handler 'overwrite))
   (last-yank :init-value -1) ; index into the kill-ring buffer.  -1 means
                              ; last op wasn't yank.
   (last-yank-pos :init-value 0) ; index into buffer where last yank is put.
   (last-yank-size :init-value 0)
   (undo-stack :init-form (make-queue)) ; see the comment at the bottom
   (redo-queue :init-form (make-queue)) ; ditto
   (history :init-value '()) ; newer history pushed onto this
   (history-pos :init-value 0) ; the current history point; C-p/n moves this
   ))

;; Entry point API
(define (read-line/edit ctx)
  (reset-undo-info! ctx)
  (reset-last-yank! ctx)
  ($ call-with-console (~ ctx'console)
     (^[con]
       (define buffer (make-gap-buffer))
       (define (eofread)
         (if (> (gap-buffer-content-length buffer) 0)
           (commit-history ctx buffer)
           (eof-object)))
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
       ;;   #t     - the command changed something so we need to redisplay,
       ;;            but the buffer contents is intact.
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
         (when redisp (redisplay ctx buffer))
         (let* ([ch (getch con)]
                [h (hash-table-get (~ ctx'keymap) ch ch)])
           (cond
            [(eof-object? h) (eofread)]
            [(char? h)
             (reset-last-yank! ctx)
             (break-undo-sequence! ctx)
             (push-undo! ctx (self-insert-command ctx buffer h))
             (loop #t)]
            [(procedure? h)
             (match (h ctx buffer ch)
               [(? boolean? b)
                (reset-last-yank! ctx)
                (break-undo-sequence! ctx)
                (loop b)]
               [(? eof-object?)
                (if (> (gap-buffer-content-length buffer) 0)
                  (commit-history ctx buffer)
                  (eof-object))]
               ['commit (commit-history ctx buffer)]
               ['undone (reset-last-yank! ctx)
                        (loop #t)] ; don't break undo sequence
               ['#t     (reset-last-yank! ctx)
                        (break-undo-sequence! ctx)
                        (loop #t)]
               [('yanked edit-command)
                (break-undo-sequence! ctx)
                (push-undo! ctx edit-command)
                (loop #t)]
               [(? list? edit-command)
                (reset-last-yank! ctx)
                (break-undo-sequence! ctx)
                (push-undo! ctx edit-command)
                (loop #t)]
               [x (error "[internal] invalid return value from a command:" x)])]
            [else
             (error "[internal] do not know how to handle key:" h)]))))))

;; Check some parameters of screen.
(define (init-screen-params ctx)
  ;(enable-scroll (~ ctx'console))
  (receive (y x) (query-cursor-position (~ ctx'console))
    (set! (~ ctx'initpos-y) y)
    (set! (~ ctx'initpos-x) x))
  (receive (h w) (query-screen-size (~ ctx'console))
    (set! (~ ctx'screen-height) h)
    (set! (~ ctx'screen-width) w)))

;; Show prompt
(define (show-prompt ctx)
  (let* ([p (~ ctx'prompt)]
         [s (if (applicable? p) (with-output-to-string p) (x->string p))])
    (putstr (~ ctx'console) s)))

;; TODO: Mind char-width.
(define (redisplay ctx buffer)
  ;; character generator with tab expansion
  (define (gen/tab g start-column)
    (gbuffer-filter (^[ch col]
                      (if (eqv? ch #\tab)
                        (let1 nextcol (* (quotient (+ col 8) 8) 8)
                          (values (make-list (- nextcol col) #\space) nextcol))
                        (values (list ch) (+ col 1))))
                    start-column g))
  ;; calculate character position, with tab expansion
  (define (pos/tab g start-column pos)
    (let loop ([n 0]
               [col start-column])
      (if (= n pos)
        col
        (let1 ch (g)
          (loop (+ n 1)
                (if (eqv? ch #\tab)
                  (* (quotient (+ col 8) 8) 8)
                  (+ col 1)))))))

  (let ([con (~ ctx'console)]
        [y   (~ ctx'initpos-y)]
        [x   (~ ctx'initpos-x)]
        [w   (~ ctx'screen-width)]
        [h   (~ ctx'screen-height)])
    (define g (gen/tab (gap-buffer->generator buffer) x))
    (move-cursor-to con y x)
    (clear-to-eos con)
    (let loop ([y y] [x x])
      (glet1 ch (g)
        (cond
         [(= x w)
          (if (= y (- h 1))
            (begin (dec! (~ ctx'initpos-y))
                   (cursor-down/scroll-up con))
            (inc! y))
          (move-cursor-to con y 0)
          (putch con ch)
          (move-cursor-to con y 1)
          (loop y 1)]
         [else (putch con ch) (loop y (+ x 1))])))
    (let1 pos (pos/tab (gap-buffer->generator buffer) x
                       (gap-buffer-pos buffer))
      (move-cursor-to con
                      (+ (~ ctx'initpos-y) (quotient pos w))
                      (modulo pos w)))))

;;
;; Key combinations
;;

(define (ctrl k)
  (integer->char (- (logand (char->integer k) (lognot #x20)) #x40)))
(define (alt k) `(ALT ,k))

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

;;
;; History
;;

(define (commit-history ctx buffer)
  (rlet1 str (gap-buffer->string buffer)
    (push! (~ ctx'history) str)
    (set! (~ ctx'history-pos) 0)))

(define (get-history ctx)
  (list-ref (~ ctx'history) (~ ctx'history-pos) ""))

(define (get-prev-history ctx)
  (begin0 (get-history ctx)
    (set! (~ ctx'history-pos)
          (min (+ (~ ctx'history-pos) 1) (length (~ ctx'history))))))

(define (get-next-history ctx)
  (if (zero? (~ ctx'history-pos))
    ""
    (begin (set! (~ ctx'history-pos) (- (~ ctx'history-pos) 1))
           (get-history ctx))))

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
  (and (not (gap-buffer-gap-at? buf 'beginning))
       (begin (gap-buffer-move! buf -1 'current)
              #t)))

(define (forward-char ctx buf key)
  (and (not (gap-buffer-gap-at? buf 'end))
       (begin (gap-buffer-move! buf 1 'current)
              #t)))

(define (move-beginning-of-line ctx buf key)
  (and (not (gap-buffer-gap-at? buf 'beginning))
       (begin (gap-buffer-move! buf 0 'beginning)
              #t)))

(define (move-end-of-line ctx buf key)
  (and (not (gap-buffer-gap-at? buf 'end))
       (begin (gap-buffer-move! buf 0 'end)
              #t)))

(define (kill-line ctx buf key) ; todo: kill-ring
  (and (not (gap-buffer-gap-at? buf 'end))
       (let* ([len (- (gap-buffer-content-length buf) (gap-buffer-pos buf))]
              [e (gap-buffer-edit! buf `(d #f ,len))])
         ;; e contiains (i <pos> <killed-string>)
         (save-kill-ring ctx (caddr e))
         e)))

(define (refresh-display ctx buf key)
  (reset-terminal (~ ctx'console))
  (show-prompt ctx)
  (init-screen-params ctx)
  #t)

(define (prev-history ctx buf key)
  (let1 s (get-prev-history ctx)
    (gap-buffer-edit! buf `(c 0 ,(gap-buffer-content-length buf) ,s))))

(define (next-history ctx buf key)
  (let1 s (get-next-history ctx)
    (gap-buffer-edit! buf `(c 0 ,(gap-buffer-content-length buf) ,s))))

(define (transpose-chars ctx buf key)
  (cond [(gap-buffer-gap-at? buf 'beginning) #f]
        [(= (gap-buffer-content-length buf) 1) ; special case
         (gap-buffer-move! buf 0)
         #t]
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

(define (default-keymap)
  (hash-table 'equal?
              `(,(ctrl #\space) . undefined-command) ; should be set-mark
              `(,(ctrl #\a) . ,move-beginning-of-line)
              `(,(ctrl #\b) . ,backward-char)
              `(,(ctrl #\c) . ,undefined-command)
              `(,(ctrl #\d) . ,eot-or-delete-char)
              `(,(ctrl #\e) . ,move-end-of-line)
              `(,(ctrl #\f) . ,forward-char)
              `(,(ctrl #\g) . ,keyboard-quit)
              `(,(ctrl #\h) . ,delete-backward-char)
              `(,(ctrl #\i) . ,self-insert-command) ; tab
              `(,(ctrl #\j) . ,commit-input)        ; newline
              `(,(ctrl #\k) . ,kill-line)
              `(,(ctrl #\l) . ,refresh-display)
              `(,(ctrl #\m) . ,commit-input)     ; return
              `(,(ctrl #\n) . ,next-history)
              `(,(ctrl #\o) . ,undefined-command); todo
              `(,(ctrl #\p) . ,prev-history)
              `(,(ctrl #\q) . ,undefined-command); todo
              `(,(ctrl #\r) . ,undefined-command); todo
              `(,(ctrl #\s) . ,undefined-command); todo
              `(,(ctrl #\t) . ,transpose-chars)
              `(,(ctrl #\u) . ,undefined-command); todo
              `(,(ctrl #\v) . ,undefined-command); todo
              `(,(ctrl #\w) . ,undefined-command); todo
              `(,(ctrl #\x) . ,undefined-command); todo
              `(,(ctrl #\y) . ,yank)
              `(,(ctrl #\z) . ,undefined-command); todo
              `(,(ctrl #\[) . ,undefined-command)
              `(,(ctrl #\\) . ,undefined-command)
              `(,(ctrl #\]) . ,undefined-command)
              `(,(ctrl #\^) . ,undefined-command)
              `(,(ctrl #\_) . ,undo)

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
              `(,(alt #\n) . ,undefined-command)
              `(,(alt #\o) . ,undefined-command)
              `(,(alt #\p) . ,undefined-command)
              `(,(alt #\q) . ,undefined-command)
              `(,(alt #\r) . ,undefined-command)
              `(,(alt #\s) . ,undefined-command)
              `(,(alt #\t) . ,undefined-command)
              `(,(alt #\u) . ,undefined-command)
              `(,(alt #\v) . ,undefined-command)
              `(,(alt #\w) . ,undefined-command)
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


