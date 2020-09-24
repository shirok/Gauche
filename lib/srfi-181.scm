;;
;; Taken from srfi-181 reference implementation by Shiro Kawai
;; MIT License.
;;

(define-module srfi-181
  (use gauche.record)
  (use gauche.vport)
  (use gauche.uvector)
  (use srfi-42)
  (export make-custom-binary-input-port
          make-custom-textual-input-port
          make-custom-binary-output-port
          make-custom-textual-output-port
          make-custom-binary-input/output-port
          make-file-error

          make-codec latin-1-codec utf-8-codec utf-16-codec
          native-eol-style
          unknown-encoding-error? unknown-encoding-error-name
          i/o-decoding-error? i/o-encoding-error?
          i/o-encoding-error-char
          make-transcoder native-transcoder
          transcoded-port
          bytevector->string string->bytevector))
(select-module srfi-181)

;;;
;;; Adapters for gauche.vport
;;;

(define-constant *buffer-size* 1024)

(define (make-seeker get-position set-position!)
  (^[offset whence]
    (if (and (= offset 0) (eq? whence SEEK_CUR))
      (and get-position (get-position))
      (and set-position!
           (cond
            [(eqv? whence SEEK_SET) (set-position! offset)]
            [(eqv? whence SEEK_CUR)
             (and get-position
                  (set-position! (+ offset (get-position))))]
            [(eqv? whence SEEK_END) #f]            ; unsupportable
            )))))

(define (make-custom-binary-input-port id read!
                                       get-position set-position!
                                       close)
  (define (filler buf)
    (read! buf 0 (u8vector-length buf)))
  (make <buffered-input-port>
    :name id
    :fill filler
    :seek (make-seeker get-position set-position!)
    :close close))

(define (make-custom-textual-input-port id read!
                                        get-position set-position!
                                        close)
  (if (or get-position set-position!)
    ;; If positioning is required, we can't buffer characters.
    (let ([buf (make-vector 1)])
      (make <virtual-input-port>
        :getc (^[] (let1 n (read! buf 0 1)
                     (if (zero? n)
                       (eof-object)
                       (vector-ref buf 0))))
        :getpos get-position
        :setpos set-position!
        :close close))
    ;; <buffered-input-port> uses u8vector for the buffer, so we have
    ;; to convert it.
    (let ([cbuf #f])                     ;vector, allocated on demand
      (define (filler buf)
        (unless cbuf
          (set! cbuf (make-vector (quotient (u8vector-length buf) 4))))
        (let1 n (read! cbuf 0 (vector-length cbuf))
          (if (zero? n)
            n
            (let* ([s (vector->string cbuf 0 n)]
                   [size (string-size s)])
              (assume (<= size (u8vector-length buf)))
              (string->u8vector! buf 0 s)
              size))))
      (make <buffered-input-port>
        :name id
        :fill filler
        :close close))))

(define (make-custom-binary-output-port id write!
                                        get-position set-position!
                                        close :optional (flush #f))
  (define (flusher buf complete?)
    (if (not complete?)
      (write! buf 0 (u8vector-length buf))
      ;; this is a buffer-flush operation
      (let1 len (u8vector-length buf)
        (let loop ([pos 0])
          (if (= pos len)
            (begin
              (when flush (flush))
              len)
            (let1 n (write! buf pos (- len pos))
              (loop (+ pos n))))))))
  (make <buffered-output-port>
    :name id
    :flush flusher
    :seek (make-seeker get-position set-position!)
    :close close))

;; For textual output, <buffered-output-port> is inconvenient,
;; for the passed u8vector may end in the middle of multibyte character.
(define (make-custom-textual-output-port id write!
                                         get-position set-position!
                                         close :optional (flush #f))
  (define cbuf (make-vector 1))
  (make <virtual-output-port>
    :name id
    :putc (^c (vector-set! cbuf 0 c) (write! cbuf 0 1))
    :puts (^s (let1 siz (string-length s)
                (when (< (vector-length cbuf) siz)
                  (set! cbuf (make-vector siz)))
                (do-ec (: c (index i) s)
                       (vector-set! cbuf i c))
                (write! cbuf 0 siz)))
    :getpos get-position
    :setpos set-position!
    :flush (^[] (and flush (flush)))
    :close close))

(define (make-custom-binary-input/output-port id read! write!
                                              get-position set-position!
                                              close :optional (flush #f))
  ;; We don't have buffered bidirectional port.  Using virtual i/o port
  ;; is less efficient, for I/O is done one byte at a time.
  (define buf (make-u8vector 1))

  (make <virtual-io-port>
    :name id
    :getb (^[] (let1 r (read! buf 0 1)
                 (if (zero? r)
                   (eof-object)
                   (u8vector-ref buf 0))))
    :putb (^b (u8vector-set! buf 0 b)
              (write! buf 0 1))
    :getpos get-position
    :setpos set-position!
    :close close
    :flush flush))

(define (make-file-error . objs)
  ;; As of 0.9.9, Gauche uses ad-hoc way to determine file-error--
  ;; that is, a <system-error> with certain errnos is a file error. 
  ;; It is impossible to translate arbitrary objs into meaningful 
  ;; <system-error>.  This is just a crude emulation.
  (condition
   (<system-error> (errno ENXIO) (message (write-to-string objs)))))

;;;
;;; Transcoders
;;;

;; NB: Right now, it's just from the reference implementation and
;; only supports ascii, latin-1, utf-8 and utf-16.  We'll adapt
;; this to gauche.charconv later.

(define-record-type unknown-encoding-error
  (make-unknown-encoding-error name)
  unknown-encoding-error?
  (name unknown-encoding-error-name))

(define-record-type i/o-decoding-error
  (make-i/o-decoding-error message)
  i/o-decoding-error?
  (message i/o-decoding-error-message))

(define-record-type i/o-encoding-error
  (make-i/o-encoding-error char message)
  i/o-encoding-error?
  (message i/o-encoding-error-message)
  (char i/o-encoding-error-char))

(define-record-type codec
  (%make-codec name)
  codec?
  (name codec-name)) ; canonical symbol

(define *supported-codecs*
  ;; ((canonical-symbol name ...) ...)
  '((ascii    "ascii" "us-ascii" "iso-ir-6"
              "ansi_x3.4-1968" "ansi_x3.4-1986" "iso-646.irv:1991"
              "iso-646-us" "us" "ibm367" "cp367" "csascii")
    (latin-1  "latin-1" "iso-8859-1:1987" "iso-8859-1" "iso-ir-100"
              "iso_8859-1" "latin1" "l1" "ibm819" "cp819"
              "csisolatin1")
    (utf-8    "utf-8" "csutf8")
    (utf-16   "utf-16" "csutf16"))
  )

(define (make-codec name)
  ;; We support latin-1, utf-8 and utf-16
  (let ((codec-entry (find (lambda (entry)
                             (member name (cdr entry) string-ci=?))
                           *supported-codecs*)))
    (if codec-entry
      (%make-codec (car codec-entry))
      (raise (make-unknown-encoding-error name)))))

(define *ascii-codec* (make-codec "ascii"))
(define *latin-1-codec* (make-codec "latin-1"))
(define *utf-8-codec* (make-codec "utf-8"))
(define *utf-16-codec* (make-codec "utf-16"))

(define (latin-1-codec) *latin-1-codec*)
(define (utf-8-codec) *utf-8-codec*)
(define (utf-16-codec) *utf-16-codec*)

(define-record-type transcoder
  (%make-transcoder codec eol-style handling-mode)
  transcoder?
  (codec transcoder-codec)
  (eol-style transcoder-eol-style)
  (handling-mode transcoder-handling-mode))

(define (make-transcoder codec eol-style handling-mode)
  (unless (codec? codec)
    (error "codec required, but got" codec))
  (unless (memq eol-style '(none lf crlf))
    (error "unsupported eol-style, must be one of (none lf crlf), but got"
           eol-style))
  (unless (memq handling-mode '(replace raise))
    (error "unsupported handling-mode, must be either replace or raise, but got"
           handling-mode))
  (%make-transcoder codec eol-style handling-mode))

(define (native-eol-style) 'lf)

(define (native-transcoder)
  (make-transcoder *ascii-codec* (native-eol-style) 'replace))

;; actual transcoding

(define (make-filler read-1)
  (lambda (buf start count)
    (let loop ((i start)
               (k 0))
      (if (= k count)
        k
        (let ((c (read-1)))
          (if (eof-object? c)
            k
            (begin
              ((if (string? buf) string-set! vector-set!) buf i c)
              (loop (+ i 1) (+ k 1)))))))))

;; read latin-1 or utf-8 source into ascii.
(define (make-u8-ascii-read! source eol-style handling)
  (define (read-1)
    (let ((b (read-u8 source)))
      (cond ((eof-object? b) b)
            ((>= b #x80) (if (eq? handling 'replace)
                           #\?
                           (raise (make-i/o-decoding-error
                                   "input byte out of range"))))
            ((= b #x0d) (if (eq? eol-style 'none)
                          #\return
                          (let ((b2 (peek-u8 source)))
                            (when (= b2 #x0a)
                              (read-u8 source))
                            #\newline)))
            (else (integer->char b)))))
  (make-filler read-1))

;; read utf-16 source into ascii
(define (make-u16-ascii-read! source eol-style handling)
  (define endianness #f)
  (define (read-1)
    (let ((b0 (read-u8 source)))
      (if (eof-object? b0)
        b0
        (let ((b1 (read-u8 source)))
          (if (eof-object? b1)
            (raise (make-i/o-decoding-error "lone utf-16 octet"))
            (case endianness
              ((big)    (decode-1 b0 b1))
              ((little) (decode-1 b1 b0))
              (else
               (cond ((and (= b0 #xfe) (= b1 #xff))
                      (set! endianness 'big)
                      (read-1))
                     ((and (= b0 #xff) (= b1 #xfe))
                      (set! endianness 'little)
                      (read-1))
                     (else (decode-1 b0 b1)) ;; BE by default
                     ))))))))
  (define (decode-1 hi lo)
    (cond ((and (zero? hi) (< lo #x80))
           (if (and (= lo #x0d) (not (eq? eol-style 'none)))
             (let ((b2 (peek-u8 source)))
               (when (= b2 #x0a)
                 (read-u8 source))
               #\newline)
             (integer->char lo)))
          ((eq? handling 'replace) #\?)
          (else (make-i/o-decoding-error "input bytes out of range"))))
  (make-filler read-1))

(define (make-flusher write-1)
  (lambda (buf start count)
    (let loop ((i start)
               (k 0))
      (if (= k count)
        k
        (let ((c ((if (string? buf) string-ref vector-ref) buf i)))
          (write-1 c)
          (loop (+ i 1) (+ k 1)))))))

;; Write a character C to SINK as a single byte.  Handlng EOL style.
;; Returns a new pending-return value.
(define (output-1b c sink eol-style pending-return)
  (define (nl)
    (case eol-style
      ((none lf) (write-u8 #x0a sink))
      ((cr)      (write-u8 #x0d sink))
      ((crlf)    (write-u8 #x0d sink) (write-u8 #x0a sink))))
  (cond ((eq? eol-style 'none)          ; we don't need to worry newlines
         (write-u8 (char->integer c) sink) #f)
        ((eqv? c #\return) (when pending-return (nl)) #t)
        ((eqv? c #\newline) (nl) #f)
        (pending-return (nl) (write-u8 (char->integer c) sink) #f)
        (else (write-u8 (char->integer c) sink) #f)))

(define (make-u8-ascii-write! sink eol-style handling)
  (define pending-return #f)
  (define (write-1 c)
    (set! pending-return (output-1b c sink eol-style pending-return)))
  (make-flusher write-1))

(define (make-u16-ascii-write! sink eol-style handling)
  ;; we assume BE
  (define bom-written? #f)
  (define pending-return #f)
  (define (write-1 c)
    (unless bom-written?
      (write-u8 #xfe sink)
      (write-u8 #xff sink)
      (set! bom-written? #t))
    (write-u8 0 sink)
    (set! pending-return (output-1b c sink eol-style pending-return)))
  (make-flusher write-1))

;; API
(define (transcoded-port inner transcoder)
  (define (closer) (close-port inner))
  (define (port-id)
    (string-append "transcoding "
                   (if (input-port? inner) "from " "to ")
                   (symbol->string (codec-name (transcoder-codec transcoder)))))
  (unless (transcoder? transcoder)
    (error "transcoder required, but got" transcoder))
  (let ((eol (transcoder-eol-style transcoder))
        (handling (transcoder-handling-mode transcoder)))
    (cond
     ((input-port? inner)
      (case (codec-name (transcoder-codec transcoder))
        ((ascii latin-1 utf-8)
         (make-custom-textual-input-port (port-id)
                                         (make-u8-ascii-read! inner eol handling)
                                         #f #f closer))
        ((utf-16)
         (make-custom-textual-input-port (port-id)
                                         (make-u16-ascii-read! inner eol handling)
                                         #f #f closer))))
     ((output-port? inner)
      (case (codec-name (transcoder-codec transcoder))
        ((ascii latin-1 utf-8)
         (make-custom-textual-output-port (port-id)
                                          (make-u8-ascii-write! inner eol handling)
                                          #f #f closer))
        ((utf-16)
         (make-custom-textual-output-port (port-id)
                                          (make-u16-ascii-write! inner eol handling)
                                          #f #f closer))))
     (else
      (error "port required, but got" inner)))))

(define (bytevector->string bytevector transcoder)
  (let ((src (transcoded-port (open-input-bytevector bytevector) transcoder))
        (sink (open-output-string)))
    (let loop ((c (read-char src)))
      (if (eof-object? c)
        (get-output-string sink)
        (begin
          (write-char c sink)
          (loop (read-char src)))))))

(define (string->bytevector string transcoder)
  (let* ((src (open-input-string string))
         (dest (open-output-bytevector))
         (sink (transcoded-port dest transcoder)))
    (let loop ((c (read-char src)))
      (if (eof-object? c)
        (get-output-bytevector dest)
        (begin
          (write-char c sink)
          (loop (read-char src)))))))
