;;
;; Taken from srfi-181 reference implementation by Shiro Kawai
;; MIT License.
;;

(define-module srfi-181
  (use gauche.record)
  (use gauche.vport)
  (use gauche.uvector)
  (use gauche.charconv)
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

;; If set-position! method raises a condition <io-invalid-position-error>
;; using srfi-192's make-i/o-invalid-position-error, we intercept it
;; to add port info.
(define-syntax wrap-setpos
  (syntax-rules ()
    [(_ setpos portvar)
     (and setpos
          (^[pos] (guard (e [(and (<io-invalid-position-error> e)
                                  (not (~ e'port)))
                             (errorf <io-invalid-position-error>
                                     :port portvar
                                     :position (~ e'position)
                                     "Invalid position object: ~s"
                                     (~ e'position))]
                            [else (raise e)])
                    (setpos pos))))]))

(define (make-custom-binary-input-port id read!
                                       get-position set-position!
                                       close)
  (define (filler buf)
    (read! buf 0 (u8vector-length buf)))
  (letrec ([p (make <buffered-input-port>
                :name id
                :fill filler
                :getpos get-position
                :setpos (wrap-setpos set-position! p)
                :close close)])
    p))

(define (make-custom-textual-input-port id read!
                                        get-position set-position!
                                        close)
  (if (or get-position set-position!)
    ;; If positioning is required, we can't buffer characters.
    (let ([buf (make-vector 1)])
      (letrec ([p (make <virtual-input-port>
                    :getc (^[] (let1 n (read! buf 0 1)
                                 (if (zero? n)
                                   (eof-object)
                                   (vector-ref buf 0))))
                    :getpos get-position
                    :setpos (wrap-setpos set-position! p)
                    :close close)])
        p))
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
  (letrec ([p (make <buffered-output-port>
                :name id
                :flush flusher
                :getpos get-position
                :setpos (wrap-setpos set-position! p)
                :close close)])
    p))

;; For textual output, <buffered-output-port> is inconvenient,
;; for the passed u8vector may end in the middle of multibyte character.
(define (make-custom-textual-output-port id write!
                                         get-position set-position!
                                         close :optional (flush #f))
  (define cbuf (make-vector 1))
  (letrec ([p (make <virtual-output-port>
                :name id
                :putc (^c (vector-set! cbuf 0 c) (write! cbuf 0 1))
                :puts (^s (let1 siz (string-length s)
                            (when (< (vector-length cbuf) siz)
                              (set! cbuf (make-vector siz)))
                            (do-ec (: c (index i) s)
                                   (vector-set! cbuf i c))
                            (write! cbuf 0 siz)))
                :getpos get-position
                :setpos (wrap-setpos set-position! p)
                :flush (^[] (and flush (flush)))
                :close close)])
    p))

(define (make-custom-binary-input/output-port id read! write!
                                              get-position set-position!
                                              close :optional (flush #f))
  ;; We don't have buffered bidirectional port.  Using virtual i/o port
  ;; is less efficient, for I/O is done one byte at a time.
  (define buf (make-u8vector 1))

  (letrec ([p (make <virtual-io-port>
                :name id
                :getb (^[] (let1 r (read! buf 0 1)
                             (if (zero? r)
                               (eof-object)
                               (u8vector-ref buf 0))))
                :putb (^b (u8vector-set! buf 0 b)
                          (write! buf 0 1))
                :getpos get-position
                :setpos (wrap-setpos set-position! p)
                :close close
                :flush flush)])
    p))

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

;; Since we already have gauche.charconv, we don't use srfi-181 layer
;; to implement transcoded ports.

;; TODO: newline style and decoding/encoding error is temporarily
;; unsupported.  We'll support them in gauche.charconv and propagate it
;; to this layer.

(define-condition-type <unknown-encoding-error> <error>
  unknown-encoding-error?
  (name unknown-encoding-error-name))

(define (i/o-decoding-error? obj)
  (condition-has-type? obj <io-decoding-error>))

(define (i/o-encoding-error? obj)
  (condition-has-type? obj <io-encoding-error>))

(define (i/o-encoding-error-char obj) 'writeme)

(define-record-type codec
  (%make-codec name)
  codec?
  (name codec-name))
(define-method write-object ((c codec) port)
  (format port "#<codec ~s>" (codec-name c)))

;; CES 'none' is kind of special---you can treat octet stream as any
;; single-byte encoding.  However, srfi-181 transcoder needs to assume
;; specific internal encoding, so we treat 'none' as Latin1.
(define *native-codec-name*
  (cond-expand
   [gauche.ces.none 'latin1]
   [else (gauche-character-encoding)]))

(define (make-codec name)
  (if (and (ces-conversion-supported? *native-codec-name* name)
           (ces-conversion-supported? name *native-codec-name*))
    (%make-codec name)
    (error <unknown-encoding-error>
           :name name
           "Unknown encoding:" name)))

(define *native-codec* (make-codec *native-codec-name*))
(define *ascii-codec* (make-codec "ascii"))
(define *latin-1-codec* (make-codec "latin1"))
(define (latin-1-codec) *latin-1-codec*)

(cond-expand
 [gauche.ces.none]
 [else
  (define *utf-8-codec* (make-codec "utf-8"))
  (define *utf-16-codec* (make-codec "utf-16"))
  (define (utf-8-codec) *utf-8-codec*)
  (define (utf-16-codec) *utf-16-codec*)])

(define-record-type <transcoder>
  (%make-transcoder codec eol-style handling-mode)
  transcoder?
  (codec transcoder-codec)
  (eol-style transcoder-eol-style)
  (handling-mode transcoder-handling-mode))
(define-method write-object ((obj <transcoder>) port)
  (format port "#<transcoder ~a ~a ~a>"
          (~ obj'codec) (~ obj'eol-style) (~ obj'handling-mode)))

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

(define (native-eol-style) 'none)

(define (native-transcoder)
  (make-transcoder *native-codec* (native-eol-style) 'replace))

;; wrapper for eol translation
;; Ideally eol-translation should be handled directly in the port layer
;; to be efficient.  For the time being, though, we use a vport wrapper
;; to fulfill the requirement.
(define (eol-iport iport)
  (define (getb)
    (let1 b (read-byte iport)
      (cond [(eof-object? b) b]
            [(eqv? b #x0d)
             (let1 b2 (peek-byte iport)
               (if (eqv? b2 #x0a)
                 (read-byte iport) ;just return #\newline
                 #x0a))]
            [else b])))
  (define (getc)
    (let1 c (read-char iport)
      (cond [(eof-object? c) c]
            [(eqv? c #\return)
             (let1 c2 (peek-char iport)
               (if (eqv? c2 #\newline)
                 (read-char iport) ;just return #\newline
                 #\newline))]
            [else c])))
  (make <virtual-input-port> :getb getb :getc getc))

(define (eol-lf-oport oport)
  (define got-return #f)
  (define (putx ch-or-byte)
    (case ch-or-byte
      [(#\return #x0d)
       (when got-return (write-char #\newline oport))
       (set! got-return #t)]
      [(#\newline #x0a) (write-char #\newline oport)
       (set! got-return #f)]
      [else (when got-return (write-char #\newline oport))
            (if (integer? ch-or-byte)
              (write-byte ch-or-byte oport)
              (write-char ch-or-byte oport))
            (set! got-return #f)]))
  (make <virtual-output-port> :putb putx :putc putx))

(define (eol-crlf-oport oport)
  (define got-return #f)
  (define (putx ch-or-byte)
    (case ch-or-byte
      [(#\return #x0d)
       (when got-return (display "\r\n" oport))
       (set! got-return #t)]
      [(#\newline #x0a) (display "\r\n" oport)
       (set! got-return #f)]
      [else
       (when got-return (display "\r\n" oport))
       (if (integer? ch-or-byte)
         (write-byte ch-or-byte oport)
         (write-char ch-or-byte oport))
       (set! got-return #f)]))
  (make <virtual-output-port> :putb putx :putc putx))

;; API
(define (transcoded-port inner transcoder)
  (assume-type transcoder <transcoder>)
  (cond
   [(input-port? inner)
    (let1 p (case (~ transcoder'eol-style)
              [(lf crlf)   (eol-iport inner)]
              [else inner])
      (parameterize ((external-conversion-library #f))
        (open-input-conversion-port p
                                    (~ transcoder'codec'name)
                                    :owner? #t
                                    :illegal-output (~ transcoder'handling-mode))))]
   [(output-port? inner)
    (let1 p (case (~ transcoder'eol-style)
              [(lf)   (eol-lf-oport inner)]
              [(crlf) (eol-crlf-oport inner)]
              [else inner])
      (parameterize ((external-conversion-library #f))
        (open-output-conversion-port p
                                     (~ transcoder'codec'name)
                                     :owner? #t
                                     :illegal-output (~ transcoder'handling-mode))))]
   [else
    (error "port required, but got:" inner)]))

(define (bytevector->string bytevector transcoder)
  (assume-type bytevector <u8vector>)
  (assume-type transcoder <transcoder>)
  (if (eq? (~ transcoder'eol-style) 'none)
    (parameterize ((external-conversion-library #f))
      (ces-convert-to <string> bytevector
                      (codec-name (transcoder-codec transcoder))
                      *native-codec-name*
                      :illegal-output (transcoder-handling-mode transcoder)))
    (port->string
     (transcoded-port (open-input-bytevector bytevector) transcoder))))

(define (string->bytevector string transcoder)
  (assume-type string <string>)
  (assume-type transcoder <transcoder>)
  (if (eq? (~ transcoder'eol-style) 'none)
    ;; ces-convert-to doesn't distinguish 'decoding' and 'encoding', so
    ;; it always raises <io-decoding-error> if input is invalid.
    ;; In srfi-181, we should treat it as encoding error.
    (guard (e [(<io-decoding-error> e)
               (error <io-encoding-error>
                      :port (~ e'port)
                      :message (~ e'message))]
              [else (raise e)])
      (parameterize ((external-conversion-library #f))
        (ces-convert-to <u8vector> string
                        *native-codec-name*
                        (codec-name (transcoder-codec transcoder))
                        :illegal-output (transcoder-handling-mode transcoder))))
    (let* ([sink (open-output-bytevector)]
           [p (transcoded-port sink transcoder)])
      (display string p)
      (flush p)
      (begin0 (get-output-bytevector sink)
        (close-output-port p)))))
