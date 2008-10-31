;;;
;;; Gauche-zlib - zlib module
;;;
;;;   Copyright (c) 2006 Rui Ueyama, All rights reserved.
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
;;;  $Id: zlib.scm,v 1.2 2006-12-02 16:00:40 rui Exp $
;;;

#!no-fold-case

(define-module rfc.zlib
  (use gauche.uvector)
  (export zlib-version adler32 crc32
          open-deflate-port open-inflate-port
          deflate-string inflate-string
          <zlib-error> <zlib-need-dict-error>
          <zlib-stream-error> <zlib-data-error>
          <zlib-memory-error> <zlib-version-error>
          <deflate-port> <inflate-port>
          deflate-port-full-flush
          zstream-total-in zstream-total-out
          zstream-params-set!
          zstream-adler32
          zstream-data-type
          zstream-dictionary-adler32
          gzip-encode-string gzip-decode-string
          inflate-sync
          Z_NO_COMPRESSION Z_BEST_SPEED
          Z_BEST_COMPRESSION Z_DEFAULT_COMPRESSION
          Z_FILTERED Z_HUFFMAN_ONLY
          Z_RLE Z_FIXED Z_DEFAULT_STRATEGY
          Z_BINARY Z_TEXT Z_ASCII Z_UNKNOWN
          ))
(select-module rfc.zlib)

(dynamic-load "zlib")

;; body
(define (open-deflate-port source . args)
  (let-keywords* args ((compression-level Z_DEFAULT_COMPRESSION)
                       (window-bits 15)
                       (memory-level 8)
                       (strategy Z_DEFAULT_STRATEGY)
                       (dictionary #f)
                       (buffer-size 0)
                       (owner? #f))
    (%open-deflate-port source compression-level
                        window-bits memory-level
                        strategy dictionary
                        buffer-size owner?)))

;; utility procedures
(define (deflate-string str . args)
  (call-with-output-string
    (lambda (p)
      (let1 p2 (apply open-deflate-port p args)
        (display str p2)
        (close-output-port p2)))))

(define (inflate-string str . args)
  (port->string
   (apply open-inflate-port
          (open-input-string str)
          args)))

(define (gzip-encode-string str . args)
  (call-with-output-string
    (lambda (p)
      (let1 p2 (apply open-deflate-port p
                      :window-bits (+ 15 16)
                      args)
        (display str p2)
        (close-output-port p2)))))

(define (gzip-decode-string str . args)
  (port->string
   (apply open-inflate-port
          (open-input-string str)
          :window-bits (+ 15 16)
          args)))

(provide "rfc/zlib")
