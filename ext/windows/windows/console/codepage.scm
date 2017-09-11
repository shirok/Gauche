;;;
;;; os.windows.console.codepage - windows console code page support
;;;
;;;   Copyright (c) 2017  Hamayama  https://github.com/Hamayama
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
  (use gauche.uvector)
  (use os.windows)
  (autoload gauche.charconv ces-convert ces-conversion-supported?
            ces-equivalent?)
  (autoload gauche.vport <virtual-input-port> <virtual-output-port>)
  (export wrap-windows-console-standard-ports
          auto-wrap-windows-console-standard-ports))
(select-module os.windows.console.codepage)

;; windows console conversion port class
(define-class <windows-console-conversion-input-port> (<virtual-input-port>)
  ((name :init-keyword :name :init-value "")))
(define-class <windows-console-conversion-output-port> (<virtual-output-port>)
  ((name :init-keyword :name :init-value "")))

;; check a standard handle redirection
(define (redirected-handle? hdl)
  (guard (e [(<system-error> e) #t])
    (sys-get-console-mode hdl) #f))

;; make a getc procedure
(define (make-conv-getc port hdl-type ces use-api)
  (^[]
    (receive (conv ces ces2 use-api)
        ;; get conversion parameters only once
        ($ apply values $ force $ delay
           (begin
             (guard (e [(<system-error> e) #f]) (sys-alloc-console))
             (get-conv-param hdl-type ces use-api)))
      (if conv
        (if use-api
          (conv-getc-sub port hdl-type 'UTF-16LE ces2 #t 4 2 2)
          (conv-getc-sub port hdl-type ces       ces2 #f 6 0 1))
        (read-char port)))))
(define (conv-getc-sub port hdl-type ces ces2 use-api
                       maxbytes extrabytes readbytes)
  (rlet1 chr #\null
    ;; we have to allocate extra bytes because ReadConsole might write
    ;; extra 1 byte more than a specified buffer size.
    (let ([buf (make-u8vector (+ maxbytes extrabytes) 0)]
          [hdl (if use-api (sys-get-std-handle hdl-type) #f)])
      (let loop ([i 0])
        (if (if use-api
              (zero? ($ sys-read-console hdl
                        (uvector-alias <u8vector> buf i (+ i readbytes))))
              (eof-object? (read-uvector! buf port i (+ i readbytes))))
          (set! chr (eof-object))
          (let1 str (ces-convert (u8vector->string buf 0 (+ i readbytes))
                                 ces ces2)
            (guard (e [(<error> e)
                       ;; a character is incomplete
                       (if (< (+ i readbytes) maxbytes)
                         (loop (+ i readbytes)))])
              ;; a character is complete
              (set! chr (string-ref str 0)))))))))

;; make a puts procedure
(define (make-conv-puts port hdl-type ces use-api)
  (^[str/char]
    (receive (conv ces ces2 use-api)
        ;; get conversion parameters only once
        ($ apply values $ force $ delay
           (begin
             (guard (e [(<system-error> e) #f]) (sys-alloc-console))
             (get-conv-param hdl-type ces use-api)))
      (if conv
        (if use-api
          (conv-puts-sub1 str/char hdl-type ces ces2 4096)
          (conv-puts-sub2 str/char port ces ces2))
        (display str/char port)))))
(define (conv-puts-sub1 str/char hdl-type ces ces2 maxchars)
  ;; windows api WriteConsole adjustment
  (define (sys-write-console-sub hdl str)
    (cond-expand
     [gauche.ces.utf8
      ;; unicode version api needs a workaround for the line wrapping of
      ;; surrogate pair characters.
      (let* ([cinfo (sys-get-console-screen-buffer-info hdl)]
             [w     (+ 1 (- (~ cinfo'window.right)
                            (~ cinfo'window.left)))]
             [len   (string-length str)]
             [i1    0]
             [i2    0])
        (let loop ([c (string-ref str 0)])
          (when (>= (char->integer c) #x10000)
            (sys-write-console hdl (string-copy str i1 i2))
            (set! i1 i2)
            (let* ([cinfo (sys-get-console-screen-buffer-info hdl)]
                   [x     (~ cinfo'cursor-position.x)]
                   [y     (~ cinfo'cursor-position.y)])
              (when (> x (- w 4))
                (sys-set-console-cursor-position hdl (- w 1) y)
                (sys-write-console hdl " "))))
          (inc! i2)
          (if (< i2 len)
            (loop (string-ref str i2))))
        (sys-write-console hdl (string-copy str i1)))]
     [else
      ;; ansi version api needs a ces conversion
      (sys-write-console hdl (ces-convert str ces2 ces))]))
  (let ([str (x->string str/char)]
        [hdl (sys-get-std-handle hdl-type)])
    (let loop ([i 0])
      (cond
       [(<= (string-length str) (+ i maxchars))
        (sys-write-console-sub hdl (string-copy str i))]
       [else
        (sys-write-console-sub hdl (string-copy str i (+ i maxchars)))
        (loop (+ i maxchars))]))))
(define (conv-puts-sub2 str/char port ces ces2)
  (write-uvector (string->u8vector
                  (ces-convert (x->string str/char) ces2 ces))
                 port)
  (flush port))

;; get conversion parameters
(define (get-conv-param hdl-type ces use-api)
  (let ([stdin-flag (eqv? hdl-type STD_INPUT_HANDLE)]
        [conv       #t]
        [ces2       (gauche-character-encoding)])
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
    (list conv ces ces2 use-api)))

;; make a standard input conversion port
(define (make-stdin-conv-port :optional (ces '#f) (use-api #f))
  (if (or (= (sys-get-console-cp) 0) ; for gosh-noconsole
          (not (redirected-handle? (sys-get-std-handle STD_INPUT_HANDLE))))
    (make <windows-console-conversion-input-port>
      :name "windows console conversion input"
      :getc (make-conv-getc (standard-input-port)
                            STD_INPUT_HANDLE ces use-api))
    #f))
;; make a standard output conversion port
(define (make-stdout-conv-port :optional (ces '#f) (use-api #f))
  (if (or (= (sys-get-console-cp) 0) ; for gosh-noconsole
          (not (redirected-handle? (sys-get-std-handle STD_OUTPUT_HANDLE))))
    (make <windows-console-conversion-output-port>
      :name "windows console conversion output"
      :putc (make-conv-puts (standard-output-port)
                            STD_OUTPUT_HANDLE ces use-api)
      :puts (make-conv-puts (standard-output-port)
                            STD_OUTPUT_HANDLE ces use-api))
    #f))
;; make a standard error conversion port
(define (make-stderr-conv-port :optional (ces '#f) (use-api #f))
  (if (or (= (sys-get-console-cp) 0) ; for gosh-noconsole
          (not (redirected-handle? (sys-get-std-handle STD_ERROR_HANDLE))))
    (make <windows-console-conversion-output-port>
      :name "windows console conversion output error"
      :putc (make-conv-puts (standard-error-port)
                            STD_ERROR_HANDLE ces use-api)
      :puts (make-conv-puts (standard-error-port)
                            STD_ERROR_HANDLE ces use-api))
    #f))

;; wrap windows console standard ports
(define (wrap-windows-console-standard-ports :optional (ces '#f) (use-api #f))
  (if-let1 port (make-stdin-conv-port  ces use-api) (current-input-port  port))
  (if-let1 port (make-stdout-conv-port ces use-api) (current-output-port port))
  (if-let1 port (make-stderr-conv-port ces use-api) (current-error-port  port))
  (values))

;; auto wrap windows console standard ports
(define (auto-wrap-windows-console-standard-ports)
  (let ([ces (sys-getenv "GAUCHE_WINDOWS_CONSOLE_CES")]
        [api (sys-getenv "GAUCHE_WINDOWS_CONSOLE_API")])
    (unless (and (string? ces) (string-ci=? ces "none"))
      (wrap-windows-console-standard-ports ces (boolean api)))
    (values)))

