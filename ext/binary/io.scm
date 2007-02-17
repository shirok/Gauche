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

;; NB: the API will likely to be renamed according to the
;; oncoming binary i/o srfi.  Use the current API at your own risk.
;; $Id: io.scm,v 1.3 2007-02-17 13:03:40 shirok Exp $

(define-module binary.io
  (use gauche.uvector)
  (use srfi-1)  ;; list library
  (use srfi-13) ;; string library
  (export default-endian
          read-binary-uint
          read-binary-uint8 read-binary-uint16
          read-binary-uint32 read-binary-uint64
          read-binary-sint
          read-binary-sint8 read-binary-sint16
          read-binary-sint32 read-binary-sint64
          read-binary-short  read-binary-ushort
          read-binary-long   read-binary-ulong
          read-binary-half-float read-binary-float read-binary-double
          read-ber-integer
          write-binary-uint
          write-binary-uint8 write-binary-uint16
          write-binary-uint32 write-binary-uint64
          write-binary-sint
          write-binary-sint8 write-binary-sint16
          write-binary-sint32 write-binary-sint64
          write-binary-short  write-binary-ushort
          write-binary-long   write-binary-ulong
          write-binary-half-float write-binary-float  write-binary-double
          write-ber-integer)
  )
(select-module binary.io)

(dynamic-load "binary")

;; native routine defines the following:
;; default-endian
;; {read,write}-binary-[su]int{8,16,32,64}
;; {read,write}-binary-{float,double}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; config

(define-constant *bit-size* 2)   ;; hey, you never know :)
(define-constant *byte-size* 8)
(define-constant *byte-magnitude* (expt *bit-size* *byte-size*))
(define-constant *byte-mask* (- *byte-magnitude* 1))
(define-constant *byte-right-shift* (* -1 *byte-size*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic reading

;; mind-numblingly slow, consider a uvector approach but it doesn't
;; handle endianess
(define (read-binary-uint size . args)
  (let-optionals* args ((port (current-input-port))
                        (endian (default-endian)))
    (case size
      ((1) (read-binary-uint8 port endian))
      ((2) (read-binary-uint16 port endian))
      ((4) (read-binary-uint32 port endian))
      ((8) (read-binary-uint64 port endian))
      (else
       (let loop ((ls '())
                  (cnt 0))
         (if (= cnt size)
           (fold (lambda (a b) (+ a (* b *byte-magnitude*)))
                 0
                 (if (eq? endian 'big-endian) (reverse ls) ls))
           (let1 byte (read-byte port)
             (if (eof-object? byte)
               byte
               (loop (cons byte ls) (+ cnt 1)))))))
      )))

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
  (let-optionals* args ((port (current-input-port))
                        (endian (default-endian)))
    (case size
      ((1) (read-binary-sint8 port endian))
      ((2) (read-binary-sint16 port endian))
      ((4) (read-binary-sint32 port endian))
      ((8) (read-binary-sint64 port endian))
      (else
       (uint->sint (read-binary-uint size port endian) size)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic writing

(define (write-binary-uint size int . args)
  (let-optionals* args ((port (current-output-port))
                        (endian (default-endian)))
    (case size
      ((1) (write-binary-uint8 int port endian))
      ((2) (write-binary-uint16 int port endian))
      ((4) (write-binary-uint32 int port endian))
      ((8) (write-binary-uint64 int port endian))
      (else
       (let ((ls '()))
         ;; build a list of bytes
         (dotimes (i size)
           (push! ls (logand int *byte-mask*))
           (set! int (ash int *byte-right-shift*)))
         ;; reverse if big-endian
         (unless (eq? endian 'big-endian)
           (set! ls (reverse ls)))
         ;; write the list
         (for-each (cut write-byte <> port) ls))))))

(define (write-binary-sint size int . args)
  (let-optionals* args ((port (current-output-port))
                        (endian (default-endian)))
    (case size
      ((1) (write-binary-sint8 int port endian))
      ((2) (write-binary-sint16 int port endian))
      ((4) (write-binary-sint32 int port endian))
      ((8) (write-binary-sint64 int port endian))
      (else
       (write-binary-uint size (sint->uint int size) port endian)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compatibility
;;

;; These are used in binary.pack for the (unofficial) features to
;; read/write integers in "native" width of C system on the platform.
(define read-binary-short  read-binary-sint16)
(define read-binary-ushort read-binary-uint16)
(define read-binary-long   read-binary-sint32)
(define read-binary-ulong  read-binary-uint32)

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
  (let ((port (get-optional opt-port (current-input-port))))
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
  (let ((port (get-optional opt-port (current-output-port))))
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

(provide "binary/io")

