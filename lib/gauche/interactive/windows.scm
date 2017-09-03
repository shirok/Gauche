;;;
;;; gauche.interactive.windows - windows console code page support
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

(define-module gauche.interactive.windows
  (use gauche.charconv)
  (use gauche.vport)
  (use gauche.uvector)
  (use gauche.sequence)
  (use os.windows)
  (export wrap-windows-console-standard-ports))
(select-module gauche.interactive.windows)

;; check a standard handle redirection
(define (redirected-handle? hdl)
  (guard (e [(<system-error> e) #t])
    (sys-get-console-mode hdl) #f))

;; make a getc procedure
(define (make-conv-getc port hdl ces ces2 use-api)
  (if use-api
    (make-conv-getc-sub port hdl 'UTF-16LE ces2 #t 4 2 2)
    (make-conv-getc-sub port hdl ces       ces2 #f 6 0 1)))
(define (make-conv-getc-sub port hdl ces ces2 use-api
                            maxbytes extrabytes readbytes)
  (^[]
    (rlet1 chr #\null
      ;; we have to allocate extra bytes because ReadConsole might writes
      ;; extra 1 byte more than a specified buffer size.
      (let1 buf (make-u8vector (+ maxbytes extrabytes) 0)
        (let loop ([i 0])
          (if (if use-api
                (zero? (sys-read-console hdl (uvector-alias <u8vector> buf i (+ i readbytes))))
                (eof-object? (read-uvector! buf port i (+ i readbytes))))
            (set! chr (eof-object))
            (let1 str (ces-convert (u8vector->string buf 0 (+ i readbytes)) ces ces2)
              (guard (e [(<error> e)
                         ;; a character is incomplete
                         (if (< (+ i readbytes) maxbytes)
                           (loop (+ i readbytes)))])
                ;; a character is complete
                (set! chr (string-ref str 0))))))))))

;; make a puts procedure
(define (make-conv-puts port conv crlf hdl ces ces2 use-api)
  (if use-api
    (make-conv-puts-sub1 hdl ces ces2 4096)
    (make-conv-puts-sub2 port conv crlf ces ces2)))
(define (make-conv-puts-sub1 hdl ces ces2 maxchars)
  ;; windows api WriteConsole adjustment
  (define (sys-write-console-sub str)
    (cond-expand
     [gauche.ces.utf8
      ;; unicode version api needs a workaroud for the line wrapping of
      ;; surrogate pair characters.
      (let* ([cinfo (sys-get-console-screen-buffer-info hdl)]
             [w     (+ 1 (- (~ cinfo'window.right)
                            (~ cinfo'window.left)))]
             [i1    0])
        (for-each-with-index
         (^[i2 c]
           (when (>= (char->integer c) #x10000)
             (sys-write-console hdl (string-copy str i1 i2))
             (set! i1 i2)
             (let* ([cinfo (sys-get-console-screen-buffer-info hdl)]
                    [x     (~ cinfo'cursor-position.x)]
                    [y     (~ cinfo'cursor-position.y)])
               (when (> x (- w 4))
                 (sys-set-console-cursor-position hdl (- w 1) y)
                 (sys-write-console hdl " ")))))
         str)
        (sys-write-console hdl (string-copy str i1)))]
     [else
      ;; ansi version api needs a ces conversion
      (set! str (ces-convert str ces2 ces))
      (sys-write-console hdl str)]))
  (^[str/char]
    (let1 str (x->string str/char)
      (let loop ([i 0])
        (cond
         [(<= (string-length str) (+ i maxchars))
          (sys-write-console-sub (string-copy str i))]
         [else
          (sys-write-console-sub (string-copy str i (+ i maxchars)))
          (loop (+ i maxchars))])))))
(define (make-conv-puts-sub2 port conv crlf ces ces2)
  (^[str/char]
    (let1 str (x->string str/char)
      (if crlf (set! str (regexp-replace-all #/\n/ str "\r\n")))
      (if conv (set! str (ces-convert str ces2 ces)))
      (let1 buf (string->u8vector str)
        (write-uvector buf port)
        (flush port)))))

;; get conversion parameters
(define (get-conv-param rmode hdl ces use-api stdin-flag)
  (define (check-ces ces1 ces2 ces-err)
    (unless (ces-conversion-supported? ces1 ces2)
      (errorf "ces \"~a\" is not supported" ces-err)))
  (let* ([rdir (redirected-handle? hdl)]
         [conv (if rdir (if (or (= rmode 2) (= rmode 3)) #t #f) #t)]
         [crlf (if rdir (if (or (= rmode 1) (= rmode 3)) #t #f) #f)]
         [ces2 (gauche-character-encoding)])
    ;; automatic detection of ces
    (unless ces
      (let1 cp (if stdin-flag (sys-get-console-cp) (sys-get-console-output-cp))
        (case cp
          [(65001) (set! ces 'UTF-8)
                   (set! use-api #t)]
          [else    (set! ces (string->symbol (format "CP~d" cp)))])))
    ;; a workaroud for the yen mark conversion error
    (cond-expand
     [gauche.ces.sjis
      (set! ces2 'CP932)
      (if (#/^(SJIS|SHIFT[\-_]?JIS)$/i (x->string ces))
        (set! ces 'CP932))]
     [else])
    ;; check a ces conversion
    (if stdin-flag
      (check-ces ces ces2 ces)
      (check-ces ces2 ces ces))
    ;; check a redirection
    (if rdir (set! use-api #f))
    ;; return parameters
    (values conv crlf hdl ces ces2 use-api)))

;; make a standard input conversion port
(define (make-stdin-conv-port :optional (rmode 0) (ces '#f) (use-api #f))
  (receive (conv crlf hdl ces ces2 use-api)
      (get-conv-param rmode (sys-get-std-handle STD_INPUT_HANDLE) ces use-api #t)
    (if conv
      (make <virtual-input-port>
        :getc (make-conv-getc (standard-input-port) hdl ces ces2 use-api))
      #f)))
;; make a standard output conversion port
(define (make-stdout-conv-port :optional (rmode 0) (ces '#f) (use-api #f))
  (receive (conv crlf hdl ces ces2 use-api)
      (get-conv-param rmode (sys-get-std-handle STD_OUTPUT_HANDLE) ces use-api #f)
    (if (or conv crlf)
      (make <virtual-output-port>
        :putc (make-conv-puts (standard-output-port) conv crlf hdl ces ces2 use-api)
        :puts (make-conv-puts (standard-output-port) conv crlf hdl ces ces2 use-api))
      #f)))
;; make a standard error conversion port
(define (make-stderr-conv-port :optional (rmode 0) (ces '#f) (use-api #f))
  (receive (conv crlf hdl ces ces2 use-api)
      (get-conv-param rmode (sys-get-std-handle STD_ERROR_HANDLE) ces use-api #f)
    (if (or conv crlf)
      (make <virtual-output-port>
        :putc (make-conv-puts (standard-error-port) conv crlf hdl ces ces2 use-api)
        :puts (make-conv-puts (standard-error-port) conv crlf hdl ces ces2 use-api))
      #f)))

;; wrap windows console standard ports
(define (wrap-windows-console-standard-ports :optional (rmode 0) (ces '#f) (use-api #f))
  (if-let1 port (make-stdin-conv-port  rmode ces use-api) (current-input-port  port))
  (if-let1 port (make-stdout-conv-port rmode ces use-api) (current-output-port port))
  (if-let1 port (make-stderr-conv-port rmode ces use-api) (current-error-port  port))
  (values))

