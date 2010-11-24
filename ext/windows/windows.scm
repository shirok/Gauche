
(define-module os.windows
  (export
   ;; MessageBox
   MB_ABORTRETRYIGNORE MB_CANCELTRYCONTINUE MB_HELP MB_OK MB_OKCANCEL
   MB_RETRYCANCEL MB_YESNO MB_YESNOCANCEL
   MB_ICONEXCLAMATION MB_ICONWARNING MB_ICONINFORMATION MB_ICONASTERISK
   MB_ICONQUESTION MB_ICONSTOP MB_ICONERROR MB_ICONHAND
   MB_DEFBUTTON1 MB_DEFBUTTON2 MB_DEFBUTTON3 MB_DEFBUTTON4
   MB_APPLMODAL MB_SYSTEMMODAL MB_TASKMODAL
   MB_DEFAULT_DESKTOP_ONLY MB_RIGHT MB_RTLREADING MB_SETFOREGROUND
   MB_TOPMOST MB_SERVICE_NOTIFICATION
   IDABORT IDCANCEL IDCONTINUE IDIGNORE IDNO IDOK IDRETRY IDTRYAGAIN IDYES
   sys-message-box

   ;; Console-related (see the comment in console.stub)
   sys-alloc-console sys-free-console
   CTRL_C_EVENT CTRL_BREAK_EVENT sys-generate-console-ctrl-event
   GENERIC_READ GENERIC_WRITE FILE_SHARE_READ FILE_SHARE_WRITE
   sys-create-console-screen-buffer
   sys-set-console-active-screen-buffer
   sys-scroll-console-screen-buffer
   sys-get-console-cp sys-get-console-output-cp
   sys-set-console-cp sys-set-console-output-cp
   sys-get-console-cursor-info
   sys-set-console-cursor-info
   sys-set-console-cursor-position
   ENABLE_LINE_INPUT ENABLE_ECHO_INPUT ENABLE_PROCESSED_INPUT
   ENABLE_WINDOW_INPUT ENABLE_MOUSE_INPUT ENABLE_PROCESSED_OUTPUT
   ENABLE_WRAP_AT_EOL_OUTPUT
   sys-get-console-mode sys-set-console-mode
   <win:console-screen-buffer-info>
   FOREGROUND_BLUE
   FOREGROUND_GREEN
   FOREGROUND_RED
   FOREGROUND_INTENSITY
   BACKGROUND_BLUE
   BACKGROUND_GREEN
   BACKGROUND_RED
   BACKGROUND_INTENSITY
   sys-get-console-screen-buffer-info
   sys-get-largest-console-window-size
   sys-set-screen-buffer-size
   <win:input-record>
   sys-get-number-of-console-input-events
   sys-get-number-of-console-mouse-buttons
   sys-peek-console-input
   sys-read-console-input
   sys-read-console
   sys-read-console-output
   sys-read-console-output-attribute
   sys-read-console-output-character
   sys-set-console-text-attribute
   sys-set-console-window-info
   sys-write-console
   sys-write-console-output-character
   sys-get-console-title
   STD_INPUT_HANDLE STD_OUTPUT_HANDLE STD_ERROR_HANDLE
   sys-get-std-handle sys-set-std-handle
   ))
(select-module os.windows)

(dynamic-load "os--windows")
