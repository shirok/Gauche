;;;; binary.io -- serializing binary data

;;; Created:    <2003-01-20 23:25:06 foof>
;;; Time-stamp: <2003-01-29 20:53:45 foof>
;;; Author:     Alex Shinn <foof@synthcode.com>

;; It is computed that eleven thousand persons have at several times
;; suffered death, rather than submit to break their eggs at the smaller
;; end.  Many hundred large volumes have been published upon this
;; controversy: but the books of the Big-endians have been long
;; forbidden, and the whole party rendered incapable by law of holding
;; employments.
;;          -- from "Gulliver's Travels" by Jonathan Swift

;; NB: The APIs here will be replaced by the native-coded version in
;; near future.  Do not count on it -- [SK]

(define-module binary.io
  (use gauche.uvector)
  (use srfi-1)  ;; list library
  (use srfi-2)  ;; and-let*
  (use srfi-13) ;; string library
  (export-all))
(select-module binary.io)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; config

(define *bit-size* 2)   ;; hey, you never know :)
(define *byte-size* 8)
(define *byte-magnitude* (expt *bit-size* *byte-size*))
(define *byte-mask* (- *byte-magnitude* 1))
(define *byte-right-shift* (* -1 *byte-size*))

;; auto-detect endianess (middle-endians not supported)
(define *default-endian*
  (let1 vec (make-u32vector 1)
    (with-input-from-string "\x01\x02\x03\x04"
      (lambda ()
        (read-block! vec)
        (if (eq? (u32vector-ref vec 0) #x04030201)
          'little-endian
          'big-endian)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic reading

;; mind-numblingly slow, consider a uvector approach but it doesn't
;; handle endianess
(define (read-binary-uint size . args)
  (let ((port (current-input-port))
        (endian *default-endian*))
    ;; [port [endian]]
    (if (pair? args)
      (let ((args2 (cdr args)))
        (if (car args) (set! port (car args)))
        (if (pair? args2) (set! endian (car args2)))))
    (let ((ls '()))
      ;; build a list of bytes
      (dotimes (i size)
        (push! ls (read-byte port)))
      ;; reverse if big-endian
      (when (eq? endian 'big-endian)
        (set! ls (reverse ls)))
      ;; accumulate
      (fold (lambda (a b) (+ a (* b *byte-magnitude*))) 0 ls))))

(define (lognot-small int bytes)
  (logand (lognot int) (- (expt *bit-size* (* *byte-size* bytes)) 1)))

(define (uint->sint int bytes)
  (let ((highbit (- (* *byte-size* bytes) 1)))
    (if (logbit? highbit int)
      (* -1 (+ 1 (lognot-small int bytes)))
      int)))

(define (sint->uint int bytes)
  (if (< int 0)
    (+ 1 (lognot-small (abs int) bytes))
    int))

(define (read-binary-sint size . args)
  (uint->sint (apply read-binary-uint (cons size args)) size))

(define read-binary-uint8  read-byte)
(define read-binary-uint16 (cut read-binary-uint 2 <...>))
(define read-binary-uint32 (cut read-binary-uint 4 <...>))
(define read-binary-uint64 (cut read-binary-uint 8 <...>))

(define read-binary-sint8  (cut read-binary-sint 1 <...>))
(define read-binary-sint16 (cut read-binary-sint 2 <...>))
(define read-binary-sint32 (cut read-binary-sint 4 <...>))
(define read-binary-sint64 (cut read-binary-sint 8 <...>))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic writing

(define (write-binary-uint size int . args)
  (let ((port (current-output-port))
        (endian *default-endian*))
    ;; [port [endian]]
    (if (pair? args)
      (let ((args2 (cdr args)))
        (if (car args) (set! port (car args)))
        (if (pair? args2) (set! endian (car args2)))))
    (let ((ls '()))
      ;; build a list of bytes
      (dotimes (i size)
        (push! ls (logand int *byte-mask*))
        (set! int (ash int *byte-right-shift*)))
      ;; reverse if big-endian
      (unless (eq? endian 'big-endian)
        (set! ls (reverse ls)))
      ;; write the list
      (for-each (cut write-byte <> port) ls))))

(define (write-binary-sint size int . args)
  (apply write-binary-uint (cons size (cons (sint->uint int size) args))))

(define write-binary-uint8  write-byte)
(define write-binary-uint16 (cut write-binary-uint 2 <...>))
(define write-binary-uint32 (cut write-binary-uint 4 <...>))
(define write-binary-uint64 (cut write-binary-uint 8 <...>))

(define write-binary-sint8  (cut write-binary-sint 1 <...>))
(define write-binary-sint16 (cut write-binary-sint 2 <...>))
(define write-binary-sint32 (cut write-binary-sint 4 <...>))
(define write-binary-sint64 (cut write-binary-sint 8 <...>))

;; these should be defined to the native "C" meanings

(define write-binary-short  write-binary-sint16)
(define write-binary-ushort write-binary-uint16)
(define write-binary-long   write-binary-sint32)
(define write-binary-ulong  write-binary-uint32)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bignum encodings -- Basic Encoding Rules (BER) from X.209

;; A BER compressed integer is an unsigned integer in base 128, most
;; significant digit first, where the high bit is set on all but the
;; final (least significant) byte.  Thus any size integer can be
;; encoded, but the encoding is efficient and small integers don't take
;; up any more space than they would in normal char/short/int encodings.

(define (read-ber-integer . opt-port)
  (let ((port (if (pair? opt-port) (car opt-port) (current-input-port))))
    (let ((first (read-byte port)))
      (if (eof-object? first)
        first ;; stop on eof
        (if (< first 128)
          first
          (let loop ((res (ash (logand first #b01111111) 7))
                     (byte (read-binary-uint8 port)))
            (if (< byte 128)
              (+ res byte) ;; final byte
              (loop (ash (+ res (logand byte #b01111111)) 7)
                    (read-binary-uint8 port)))))))))

(define (write-ber-integer number . opt-port)
  (let ((port (if (pair? opt-port) (car opt-port) (current-output-port))))
    (let ((final (logand number #b01111111))
          (start (ash number -7)))
      (unless (zero? start)
        (let loop ((n start))
          (cond ((< n 128)
                 (write-binary-uint8 (logior n #b10000000)))
                (else
                 (loop (ash n -7)) ;; write high bytes first
                 (write-binary-uint8 (logior (logand n #b01111111) #b10000000))))))
      (write-binary-uint8 final))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; floating point numbers

;; There are no network portable floating point representations, so we
;; use the internal native representation.  For this, gauche.uvector
;; works fine.

;; Gauche uses double's internally for all floating point numbers, so
;; when using the float type you can lose some precision.  For example,
;; writing and then reading back out 123.456 produces
;; 123.45600128173828 using read/write-binary-float, but 123.456 with
;; read/write-binary-double.

(define (read-binary-float . opt-port)
  (let ((p (if (pair? opt-port) (car opt-port) (current-input-port)))
        (vec (make-f32vector 1)))
    (read-block! vec)
    (f32vector-ref vec 0)))

(define (read-binary-double . opt-port)
  (let ((p (if (pair? opt-port) (car opt-port) (current-input-port)))
        (vec (make-f64vector 1)))
    (read-block! vec)
    (f64vector-ref vec 0)))

(define (write-binary-float x . opt-port)
  (let ((p (if (pair? opt-port) (car opt-port) (current-output-port)))
        (vec (make-f32vector 1 x)))
    (write-block vec)))

(define (write-binary-double x . opt-port)
  (let ((p (if (pair? opt-port) (car opt-port) (current-output-port)))
        (vec (make-f64vector 1 x)))
    (write-block vec)))

(provide "binary/io")

