;;
;; testing os.windows
;;

(use gauche.test)
(use gauche.uvector)

(cond-expand
 [gauche.os.windows
  (test-start "windows")
  (use os.windows)
  (test-module 'os.windows)

(define hin  (sys-get-std-handle STD_INPUT_HANDLE))
(define hout (sys-get-std-handle STD_OUTPUT_HANDLE))
(define (redirected-handle? hdl)
  (guard (exc ((<system-error> exc) #t))
    (sys-get-console-mode hdl) #f))
(define rin  (redirected-handle? hin))
(define rout (redirected-handle? hout))

(test-section "Console procedures")
(test* "sys-alloc-console" (test-error <system-error>) (sys-alloc-console))
;; This test causes a program termination.
;(test* "sys-free-console" (undefined) (sys-free-console))
;(test* "sys-generate-console-ctrl-event 1" (undefined) (sys-generate-console-ctrl-event CTRL_C_EVENT 0))
;(test* "sys-generate-console-ctrl-event 2" (undefined) (sys-generate-console-ctrl-event CTRL_BREAK_EVENT 0))

(when (not rout)
  (test-section "Console Buffers")
  (define cbuf1 (sys-create-console-screen-buffer (logior GENERIC_READ GENERIC_WRITE) 0 #f))
  (define cbuf2 (sys-get-std-handle STD_OUTPUT_HANDLE))
  (test* "sys-create-console-screen-buffer" '<win:handle> cbuf1
         (lambda (expected result) (equal? expected (class-name (class-of result)))))
  (test* "sys-set-console-active-screen-buffer 1" (undefined) (sys-set-console-active-screen-buffer cbuf1))
  (test* "sys-set-console-active-screen-buffer 2" (undefined) (sys-set-console-active-screen-buffer cbuf2))
  (test* "sys-scroll-console-screen-buffer" (undefined)
         (sys-scroll-console-screen-buffer cbuf2 (s16vector 0 0 4 2) #f 5 0 0))
  )

(test-section "Console Code Page")
(define cp1 (sys-get-console-cp))
(define cp2 (sys-get-console-output-cp))
(test* "sys-set-console-cp" (undefined) (sys-set-console-cp 65001))
(test* "sys-set-console-output-cp" (undefined) (sys-set-console-output-cp 65001))
(test* "sys-get-console-cp" 65001 (sys-get-console-cp))
(test* "sys-get-console-output-cp" 65001 (sys-get-console-output-cp))
(sys-set-console-cp cp1)
(sys-set-console-output-cp cp2)

(when (not rout)
  (test-section "Console Cursor Info")
  (define-values (csize cvisible) (sys-get-console-cursor-info hout))
  (test* "sys-set-console-cursor-info" (undefined) (sys-set-console-cursor-info hout 1 #f))
  (test* "sys-get-console-cursor-info" '(1 #f) (values->list (sys-get-console-cursor-info hout)))
  (sys-set-console-cursor-info hout csize cvisible)
  ;; This test causes a cursor position change.
  ;(test* "sys-set-console-cursor-position" (undefined) (sys-set-console-cursor-position hout 0 0))
  ;(exit)
  )

(when (not rout)
  (test-section "Console Mode")
  (define cmode1 (sys-get-console-mode hin))
  (define cmode2 (sys-get-console-mode hout))
  (test* "sys-set-console-mode" (undefined) (sys-set-console-mode hin ENABLE_LINE_INPUT))
  (test* "sys-set-console-mode" (undefined) (sys-set-console-mode hout ENABLE_PROCESSED_OUTPUT))
  (test* "sys-get-console-mode" ENABLE_LINE_INPUT (sys-get-console-mode hin))
  (test* "sys-get-console-mode" ENABLE_PROCESSED_OUTPUT (sys-get-console-mode hout))
  (sys-set-console-mode hin cmode1)
  (sys-set-console-mode hout cmode2)
  )

(when (not rout)
  (test-section "Console Screen Buffer Info")
  (define cinfo (sys-get-console-screen-buffer-info hout))
  (test* "sys-get-console-screen-buffer-info" '<win:console-screen-buffer-info> cinfo
         (lambda (expected result) (equal? expected (class-name (class-of result)))))
  (test-log "cinfo.size.x = ~a" (~ cinfo 'size.x))
  (test-log "cinfo.size.y = ~a" (~ cinfo 'size.y))
  (test-log "cinfo.cursor-position.x = ~a" (~ cinfo 'cursor-position.x))
  (test-log "cinfo.cursor-position.y = ~a" (~ cinfo 'cursor-position.y))
  (test-log "cinfo.attributes = ~a" (~ cinfo 'attributes))
  (test-log "window.left = ~a"   (~ cinfo 'window.left))
  (test-log "window.top = ~a"    (~ cinfo 'window.top))
  (test-log "window.right = ~a"  (~ cinfo 'window.right))
  (test-log "window.bottom = ~a" (~ cinfo 'window.bottom))
  (test-log "maximum-window-size.x = ~a" (~ cinfo 'maximum-window-size.x))
  (test-log "maximum-window-size.y = ~a" (~ cinfo 'maximum-window-size.y))
  (define wsize (values->list (sys-get-largest-console-window-size hout)))
  (test* "sys-get-largest-console-window-size" 2 wsize
         (lambda (expected result) (equal? expected (length result))))
  (test-log "largest-console-window-width  = ~a" (car  wsize))
  (test-log "largest-console-window-height = ~a" (cadr wsize))
  ;; This test causes a screen buffer size change.
  ;(test* "sys-set-screen-buffer-size" (undefined) (sys-set-screen-buffer-size hout 80 25))
  ;(exit)
  )

(when (not rin)
  (test-section "Console input")
  (define evnum (sys-get-number-of-console-input-events hin))
  (test* "sys-get-number-of-console-input-events" '<integer> evnum
         (lambda (expected result) (equal? expected (class-name (class-of result)))))
  (define mbnum (sys-get-number-of-console-mouse-buttons))
  (test* "sys-get-number-of-console-mouse-buttons" '<integer> mbnum
         (lambda (expected result) (equal? expected (class-name (class-of result)))))
  (test-log "number-of-console-input-events  = ~a" evnum)
  (test-log "number-of-console-mouse-buttons = ~a" mbnum)
  )

(define KEY_EVENT                #x01)
(define MOUSE_EVENT              #x02)
(define WINDOW_BUFFER_SIZE_EVENT #x04)
(define MENU_EVENT               #x08)
(define FOCUS_EVENT              #x10)
(define (event-loop-test)
  (let ((hin    (sys-get-std-handle STD_INPUT_HANDLE))
        (cmode  0)
        (done   #f)
        (ir     #f)
        (irlist '())
        (evt    #f))
    (set! cmode (sys-get-console-mode hin))
    (sys-set-console-mode hin (logior ENABLE_WINDOW_INPUT ENABLE_MOUSE_INPUT))
    (test-log "Event loop test (Hit [esc] key to exit)")
    (while (not done)
      (set! irlist (sys-peek-console-input hin))
      (when (not (null? irlist))
        (sys-read-console-input hin)
        (while (not (null? irlist))
          (set! ir     (car irlist))
          (set! irlist (cdr irlist))
          (set! evt    (~ ir 'event-type))
          (cond
           ((= evt KEY_EVENT)
            (let ((kdown (~ ir 'key.down))
                  (rept  (~ ir 'key.repeat-count))
                  (vk    (~ ir 'key.virtual-key-code))
                  (vs    (~ ir 'key.virtual-scan-code))
                  (ch    (~ ir 'key.unicode-char))
                  (asc   (~ ir 'key.ascii-char))
                  (ctls  (~ ir 'key.control-key-state)))
              (test-log "key : kdown=~a repeat=~a vk=~a vs=~a ch=~a asc=~a ctrlkeys=~a" kdown rept vk vs ch asc ctls)
              (if (and kdown (= vk 27))
                (set! done #t))))
           ((= evt MOUSE_EVENT)
            (let ((x     (~ ir 'mouse.x))
                  (y     (~ ir 'mouse.y))
                  (btn   (~ ir 'mouse.button-state))
                  (ctls  (~ ir 'mouse.control-key-state))
                  (evflg (~ ir 'mouse.event-flags)))
              (test-log "mouse : x=~a y=~a button=~a ctrlkeys=~a eventflags=~a" x y btn ctls evflg)))
           ((= evt WINDOW_BUFFER_SIZE_EVENT)
            (let ((x     (~ ir 'window-buffer-size.x))
                  (y     (~ ir 'window-buffer-size.y)))
              (test-log "window-buffer-size : x=~a y=~a" x y)))
           ((= evt MENU_EVENT)
            (let ((id    (~ ir 'menu.command-id)))
              (test-log "menu : menu-command-id=~a" id)))
           ((= evt FOCUS_EVENT)
            (let ((fcs   (~ ir 'focus.set-focus)))
              (test-log "focus : set-focus=~a" fcs)))
           )))
      (sys-nanosleep (* 100 1000000)) ; 100msec
      )
    (sys-set-console-mode hin cmode)))
;; This test causes an event loop.
;(event-loop-test)
;(exit)

;; This test causes a keyboard input waiting.
;(when (not rin)
;  (define cmode1 (sys-get-console-mode hin))
;  (sys-set-console-mode hin 0)
;  (define rnum (sys-read-console hin (make-u8vector 2 0)))
;  (test* "sys-read-console" '<integer> rnum
;         (lambda (expected result) (equal? expected (class-name (class-of result)))))
;  (sys-set-console-mode hin cmode1)
;  (test-log "number of read characters=~a" rnum)
;  (exit)
;  )

(when (not rout)
  (define rbuf (sys-read-console-output hout (make-u32vector 6 0) 3 2 0 0 (s16vector 0 3 2 4)))
  (test* "sys-read-console-output" '<u32vector> rbuf
         (lambda (expected result) (equal? expected (class-name (class-of result)))))
  (test-log (string-append "read-buffer=" (x->string (map (cut format "~8,'0Xh" <>) (u32vector->list rbuf)))))

  (define rbuf (make-u16vector 6 0))
  (define rnum (sys-read-console-output-attribute hout rbuf 0 3))
  (test* "sys-read-console-output-attribute" '<integer> rnum
         (lambda (expected result) (equal? expected (class-name (class-of result)))))
  (test-log (string-append "read-attribute-buffer=" (x->string (map (cut format "~4,'0Xh" <>) (u16vector->list rbuf)))))
  (test-log "number of read attributes=~a" rnum)

  (define rstr (sys-read-console-output-character hout 6 0 3))
  (test* "sys-read-console-output-character 1" '<string> rstr
         (lambda (expected result) (equal? expected (class-name (class-of result)))))
  (test-log "read-string=\"~a\"" rstr)
  (define rstr (sys-read-console-output-character hout 65535 0 3))
  (test* "sys-read-console-output-character 2" '<string> rstr
         (lambda (expected result) (equal? expected (class-name (class-of result)))))
  (test* "sys-read-console-output-character 3" (test-error <error>)
         (sys-read-console-output-character hout 65536 0 3))

  (test* "sys-set-console-text-attribute" (undefined) (sys-set-console-text-attribute hout 10))
  (test-log "color=10")
  (test* "sys-set-console-text-attribute" (undefined) (sys-set-console-text-attribute hout 7))
  (test-log "color=7")

  ;; This test causes a window size change.
  ;(test* "sys-set-console-window-info" (undefined) (sys-set-console-window-info hout #t (s16vector 0 0 10 10)))
  ;(exit)

  (define wnum (sys-write-console hout "abcde fghij klmno\n"))
  (test* "sys-write-console 1" '<integer> wnum
         (lambda (expected result) (equal? expected (class-name (class-of result)))))
  (test-log "number of write characters=~a" wnum)
  (define wnum (sys-write-console hout (string-copy "aaaaa" 0 1)))
  (test* "sys-write-console 2" '<integer> wnum
         (lambda (expected result) (equal? expected (class-name (class-of result)))))
  (test-log "number of write characters=~a" wnum)

  (define wnum (sys-write-console-output-character hout "ABC" 0 0))
  (test* "sys-write-console-output-character 1" '<integer> wnum
         (lambda (expected result) (equal? expected (class-name (class-of result)))))
  (test-log "number of write characters=~a" wnum)
  (define wnum (sys-write-console-output-character hout (string-copy "aaaaa" 0 1) 0 1))
  (test* "sys-write-console-output-character 2" '<integer> wnum
         (lambda (expected result) (equal? expected (class-name (class-of result)))))
  (test-log "number of write characters=~a" wnum)

  (define wnum (sys-fill-console-output-character hout #\Z 5 0 2))
  (test* "sys-fill-console-output-character" '<integer> wnum
         (lambda (expected result) (equal? expected (class-name (class-of result)))))
  (test-log "number of write characters=~a" wnum)

  (define wnum (sys-fill-console-output-attribute hout 10 5 0 2))
  (test* "sys-fill-console-output-attribute" '<integer> wnum
         (lambda (expected result) (equal? expected (class-name (class-of result)))))
  (test-log "number of write characters=~a" wnum)
  )

(when (not rin)
  (test* "sys-flush-console-input-buffer" (undefined) (sys-flush-console-input-buffer hin))
  )


(test-section "Console Title")
(define tstr (sys-get-console-title))
(test* "sys-set-console-title" (test-error <error>) (sys-set-console-title (make-string 1024 #\a)))
(test* "sys-set-console-title 1" (undefined) (sys-set-console-title "abcde"))
(test* "sys-get-console-title 1" "abcde" (sys-get-console-title))
(test* "sys-set-console-title 2" (undefined) (sys-set-console-title (string-copy "aaaaa" 0 1)))
(test* "sys-get-console-title 2" "a" (sys-get-console-title))
(sys-set-console-title tstr)


(test-section "Std Handles")
(test* "sys-get-std-handle 1" '<win:handle> (sys-get-std-handle STD_INPUT_HANDLE)
       (lambda (expected result) (equal? expected (class-name (class-of result)))))
(test* "sys-get-std-handle 2" '<win:handle> (sys-get-std-handle STD_OUTPUT_HANDLE)
       (lambda (expected result) (equal? expected (class-name (class-of result)))))
(test* "sys-get-std-handle 3" '<win:handle> (sys-get-std-handle STD_ERROR_HANDLE)
       (lambda (expected result) (equal? expected (class-name (class-of result)))))
(test* "sys-set-std-handle" (undefined) (sys-set-std-handle STD_OUTPUT_HANDLE hout))


;; This test causes a message box.
;(test-section "MessageBox")
;(define msgret (sys-message-box #f "Hello" "test" (logior MB_OK MB_ICONINFORMATION)))
;(test* "sys-message-box" '<integer> msgret
;       (lambda (expected result) (equal? expected (class-name (class-of result)))))
;(test-log "message-box-return-value=~a" msgret)
;(exit)


  (test-end)]
 [else])

