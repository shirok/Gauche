;;;
;;; os.windows.console.codepage - windows console code page support
;;;
;;;   Copyright (c) 2017-2019  Hamayama  https://github.com/Hamayama
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

(define-module os.windows.console.codepage
  (use os.windows)
  (use gauche.vport)
  (use gauche.uvector)
  (use gauche.charconv)
  (use gauche.portutil)
  (export wrap-windows-console-standard-ports
          auto-wrap-windows-console-standard-ports))
(select-module os.windows.console.codepage)

;; a character indicating conversion error
(define *conv-err-char* #\?)

;; check a standard handle redirection
(define (redirected-handle? hdl)
  (guard (e [(<system-error> e) #t])
    (sys-get-console-mode hdl) #f))

;; make a getc procedure
(define (make-conv-getc port hdl-type ces use-api vport)
  (^[]
    ;; get conversion parameters only once
    (receive (conv ces ces2 use-api)
        (begin
          (guard (e [(<system-error> e) #f]) (sys-alloc-console))
          (get-conv-param hdl-type ces use-api))
      ;; set io type
      (port-attribute-set! vport 'windows-console-io-type
                           (if conv (if use-api 'win-api 'normal) 'through))
      ;; replace to a real getc procedure
      (let1 proc
          (if conv
            (if use-api
              (make-conv-getc-sub1 hdl-type 'UTF-16LE ces2 4 2 2)
              (make-conv-getc-sub2 port ces ces2 6 1))
            (^[] (read-char port)))
        (set! (~ vport'getc) proc)
        (proc)))))
;; getc using windows api ReadConsole
(define (make-conv-getc-sub1 hdl-type ces ces2 maxbytes extrabytes readbytes)
  (define line-start-flag #t)
  (^[]
    ;; we have to allocate extra bytes because ReadConsole might write
    ;; extra 1 byte more than a specified buffer size.
    (let ([buf (make-u8vector (+ maxbytes extrabytes) 0)]
          [hdl (sys-get-std-handle hdl-type)])
      (let loop ([i1 0] [i2 readbytes])
        (if (zero? ($ sys-read-console hdl
                      (uvector-alias <u8vector> buf i1 i2)))
          (eof-object)
          (let1 str (ces-convert (u8vector->string buf 0 i2) ces ces2)
            (guard (e [(<error> e)
                       ;; a character is incomplete
                       (if (< i2 maxbytes)
                         (loop i2 (+ i2 readbytes))
                         *conv-err-char*)])
              ;; a character is complete
              (let1 chr (string-ref str 0)
                (cond
                 ;; ctrl-z
                 [(and line-start-flag (eqv? chr #\x1a))
                  (eof-object)]
                 [else
                  (set! line-start-flag (eqv? chr #\newline))
                  chr])))))))))
;; getc using read-uvector!
(define (make-conv-getc-sub2 port ces ces2 maxbytes readbytes)
  (^[]
    (let1 buf (make-u8vector maxbytes 0)
      (let loop ([i1 0] [i2 readbytes])
        (if (eof-object? (read-uvector! buf port i1 i2))
          (eof-object)
          (let1 str (ces-convert (u8vector->string buf 0 i2) ces ces2)
            (guard (e [(<error> e)
                       ;; a character is incomplete
                       (if (< i2 maxbytes)
                         (loop i2 (+ i2 readbytes))
                         *conv-err-char*)])
              ;; a character is complete
              (string-ref str 0))))))))

;; make a puts procedure
(define (make-conv-puts port hdl-type ces use-api vport)
  (^[str/char]
    ;; get conversion parameters only once
    (receive (conv ces ces2 use-api)
        (begin
          (guard (e [(<system-error> e) #f]) (sys-alloc-console))
          (get-conv-param hdl-type ces use-api))
      ;; set io type
      (port-attribute-set! vport 'windows-console-io-type
                           (if conv (if use-api 'win-api 'normal) 'through))
      ;; replace to a real puts procedure
      (let1 proc
          (if conv
            (if use-api
              (make-conv-puts-sub1 hdl-type ces ces2 4096)
              (make-conv-puts-sub2 port ces ces2))
            (^[str/char]
              (display str/char port)
              (flush port)))
        (set! (~ vport'putc) proc)
        (set! (~ vport'puts) proc)
        (proc str/char)))))
;; puts using windows api WriteConsole
(define (make-conv-puts-sub1 hdl-type ces ces2 maxchars)
  ;; WriteConsole adjustment
  (define (sys-write-console-sub hdl str)
    (cond-expand
     [gauche.ces.utf8
      ;; unicode version of WriteConsole needs a workaround for
      ;; the line wrapping of surrogate pair characters.
      (let1 buf (string->u32vector str)
        (if (u32vector-range-check buf 0 #xffff)
          (let* ([cinfo (sys-get-console-screen-buffer-info hdl)]
                 [w     (+ 1 (- (slot-ref cinfo'window.right)
                                (slot-ref cinfo'window.left)))]
                 [cw    (if (or (sys-windows-terminal?)
                                (= (sys-get-console-output-cp) 65001))
                          2 4)]
                 [len   (string-length str)])
            (let loop ([i1 0] [i2 0])
              (when (>= (u32vector-ref buf i2) #x10000)
                (sys-write-console hdl (string-copy str i1 i2))
                (set! i1 i2)
                (let* ([cinfo (sys-get-console-screen-buffer-info hdl)]
                       [x     (slot-ref cinfo'cursor-position.x)]
                       [y     (slot-ref cinfo'cursor-position.y)])
                  (when (> x (- w cw))
                    (sys-set-console-cursor-position hdl (- w 1) y)
                    (sys-write-console hdl " "))))
              (if (< (+ i2 1) len)
                (loop i1 (+ i2 1))
                (sys-write-console hdl (string-copy str i1)))))
          (sys-write-console hdl str)))]
     [else
      ;; ansi version of WriteConsole needs a ces conversion
      (sys-write-console hdl (ces-convert str ces2 ces))]))
  (^[str/char]
    (let* ([str (x->string str/char)]
           [hdl (sys-get-std-handle hdl-type)]
           [len (string-length str)])
      (let loop ([i 0])
        (cond
         [(<= len (+ i maxchars))
          (sys-write-console-sub hdl (string-copy str i))]
         [else
          (sys-write-console-sub hdl (string-copy str i (+ i maxchars)))
          (loop (+ i maxchars))])))))
;; puts using write-uvector
(define (make-conv-puts-sub2 port ces ces2)
  (^[str/char]
    (write-uvector (string->u8vector
                    (ces-convert (x->string str/char) ces2 ces))
                   port)
    (flush port)))

;; get conversion parameters
(define (get-conv-param hdl-type ces use-api)
  (let ([stdin-flag (eqv? hdl-type STD_INPUT_HANDLE)]
        [conv       #t]
        [ces2       (gauche-character-encoding)])
    ;; for windows terminal (windows 10)
    (when (sys-windows-terminal?)
      (set! use-api #t))
    ;; automatic detection of ces
    (unless ces
      (let1 cp (if stdin-flag
                 (sys-get-console-cp)
                 (sys-get-console-output-cp))
        (case cp
          [(65001)
           (set! ces 'UTF-8)
           (set! use-api #t)]
          [else
           (set! ces (string->symbol (format "CP~d" cp)))])))
    ;; workaround for the yen mark conversion error
    (cond-expand
     [gauche.ces.sjis
      (set! ces2 'CP932)
      (if (#/^(SJIS|SHIFT[\-_]?JIS)$/i (x->string ces))
        (set! ces 'CP932))]
     [else])
    ;; check a ces conversion
    (unless (if stdin-flag
              (ces-conversion-supported? ces ces2)
              (ces-conversion-supported? ces2 ces))
      (set! conv #f))
    ;; check gauche's encoding
    (cond-expand
     [gauche.ces.utf8]
     [else
      (set! use-api #f)])
    ;; check a ces equivalent
    (if (and (ces-equivalent? ces ces2) (not use-api))
      (set! conv #f))
    ;; return parameters
    (values conv ces ces2 use-api)))

;; make a standard input conversion port
(define (make-stdin-conv-port :optional (ces #f) (use-api #f))
  (and (or (= (sys-get-console-cp) 0) ; for gosh-noconsole
           (not (redirected-handle? (sys-get-std-handle STD_INPUT_HANDLE))))
       (rlet1 vport (make <virtual-input-port>
                      :name "(windows console standard input)")
         (port-attribute-set! vport 'windows-console-conversion #t)
         (port-attribute-set! vport 'windows-console-io-type 'unknown)
         (let1 proc (make-conv-getc (standard-input-port)
                                    STD_INPUT_HANDLE ces use-api vport)
           (set! (~ vport'getc) proc)))))
;; make a standard output conversion port
(define (make-stdout-conv-port :optional (ces #f) (use-api #f))
  (and (or (= (sys-get-console-cp) 0) ; for gosh-noconsole
           (not (redirected-handle? (sys-get-std-handle STD_OUTPUT_HANDLE))))
       (rlet1 vport (make <virtual-output-port>
                      :name "(windows console standard output)")
         (port-attribute-set! vport 'windows-console-conversion #t)
         (port-attribute-set! vport 'windows-console-io-type 'unknown)
         (let1 proc (make-conv-puts (standard-output-port)
                                    STD_OUTPUT_HANDLE ces use-api vport)
           (set! (~ vport'putc) proc)
           (set! (~ vport'puts) proc)))))
;; make a standard error conversion port
(define (make-stderr-conv-port :optional (ces #f) (use-api #f))
  (and (or (= (sys-get-console-cp) 0) ; for gosh-noconsole
           (not (redirected-handle? (sys-get-std-handle STD_ERROR_HANDLE))))
       (rlet1 vport (make <virtual-output-port>
                      :name "(windows console standard error output)")
         (port-attribute-set! vport 'windows-console-conversion #t)
         (port-attribute-set! vport 'windows-console-io-type 'unknown)
         (let1 proc (make-conv-puts (standard-error-port)
                                    STD_ERROR_HANDLE ces use-api vport)
           (set! (~ vport'putc) proc)
           (set! (~ vport'puts) proc)))))

;; wrap windows console standard ports
(define (wrap-windows-console-standard-ports :optional (ces #f) (use-api #f))
  (if-let1 port (make-stdin-conv-port  ces use-api) (current-input-port  port))
  (if-let1 port (make-stdout-conv-port ces use-api) (current-output-port port))
  (if-let1 port (make-stderr-conv-port ces use-api) (current-error-port  port))
  (values))

;; auto wrap windows console standard ports
(define (auto-wrap-windows-console-standard-ports)
  (let ([ces (sys-getenv "GAUCHE_WINDOWS_CONSOLE_CES")]
        [api (sys-getenv "GAUCHE_WINDOWS_CONSOLE_API")])
    (wrap-windows-console-standard-ports ces (boolean api))))
