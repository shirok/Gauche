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

#!no-fold-case

(define-module rfc.zlib
  (use gauche.uvector)
  (export zlib-version adler32 crc32
          open-deflating-port open-inflating-port
          deflate-string inflate-string
          <zlib-error> <zlib-need-dict-error>
          <zlib-stream-error> <zlib-data-error>
          <zlib-memory-error> <zlib-version-error>
          <deflating-port> <inflating-port>
          deflating-port-full-flush
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
          Z_RLE Z_DEFAULT_STRATEGY
          Z_BINARY Z_ASCII Z_UNKNOWN
          ))
(select-module rfc.zlib)

(inline-stub
 (declcode "#include \"gauche-zlib.h\"")
 (initcode (Scm_Init_zlib))

 (define-type <deflating-port> "ScmPort*" "deflating port"
   "SCM_DEFLATING_PORT_P" "SCM_PORT")
 (define-type <inflating-port> "ScmPort*" "inflating port"
   "SCM_INFLATING_PORT_P" "SCM_PORT")

 "#define SCM_XFLATING_PORT_P(x) (SCM_INFLATING_PORT_P(x)||SCM_DEFLATING_PORT_P(x))"
 ;; proxy type for shorter code.  <xflating-port> isn't really a Scheme class.
 (define-type <xflating-port> "ScmPort*" "inflating or deflating port"
   "SCM_XFLATING_PORT_P" "SCM_PORT")

 (define-cfn data_element (data::ScmObj
                           start::(const unsigned char**)
                           siz::int*)
   ::void :static
   (cond [(SCM_U8VECTORP data)
          (set! (* start) (SCM_UVECTOR_ELEMENTS (SCM_U8VECTOR data))
                (* siz)   (SCM_U8VECTOR_SIZE (SCM_U8VECTOR data)))]
         [(SCM_STRINGP data)
          (let* ([b::(const ScmStringBody*) (SCM_STRING_BODY data)])
            (set! (* start) (cast (unsigned char*) (SCM_STRING_BODY_START b))
                  (* siz)   (SCM_STRING_BODY_SIZE b)))]
         [else
          (Scm_Error "u8vector or string required, but got: %S" data)]))

 (define-cproc zlib-version ()
   (expr <top> (SCM_MAKE_STR (zlibVersion))))

 (define-enum Z_NO_COMPRESSION)
 (define-enum Z_BEST_SPEED)
 (define-enum Z_BEST_COMPRESSION)
 (define-enum Z_DEFAULT_COMPRESSION)
 (define-enum Z_FILTERED)
 (define-enum Z_HUFFMAN_ONLY)
 (define-enum Z_RLE)
 (define-enum-conditionally Z_FIXED)
 (define-enum Z_DEFAULT_STRATEGY)
 (define-enum Z_BINARY)
 (define-enum-conditionally Z_TEXT)
 (define-enum Z_ASCII)
 (define-enum Z_UNKNOWN)

 (define-cproc adler32 (data :optional (adler::<ulong> 1)) ::<ulong>
   (let* ([start::(const unsigned char*)]
          [siz::int])
     (data_element data (& start) (& siz))
     (return (adler32 adler start siz))))

 (define-cproc crc32 (data :optional (crc::<ulong> 0)) ::<ulong>
   (let* ([start::(const unsigned char*)]
          [siz::int])
     (data_element data (& start) (& siz))
     (return (crc32 crc start siz))))

 (define-cproc %open-deflating-port (source::<output-port>
                                     compression-level::<fixnum>
                                     window-bits::<fixnum>
                                     memory-level::<fixnum>
                                     strategy::<fixnum>
                                     dictionary
                                     buffer-size::<fixnum>
                                     owner?)
   (return (Scm_MakeDeflatingPort source compression-level window-bits
                                  memory-level strategy dictionary
                                  buffer-size (not (SCM_FALSEP owner?)))))

 (define-cproc open-inflating-port (sink::<input-port>
                                    :key (buffer-size::<fixnum> 0)
                                    (window-bits::<fixnum> 15)
                                    (dictionary #f)
                                    (owner? #f))
   (return (Scm_MakeInflatingPort sink buffer-size window-bits dictionary
                                  (not (SCM_FALSEP owner?)))))

 (define-cproc zstream-total-in (port::<xflating-port>) ::<ulong>
   (return (-> (SCM_PORT_ZSTREAM port) total-in)))

 (define-cproc zstream-total-out (port::<xflating-port>) ::<ulong>
   (return (-> (SCM_PORT_ZSTREAM port) total-out)))

 (define-cproc zstream-params-set! (port::<deflating-port>
                                    :key (compression-level #f) (strategy #f))
   ::<void>
   (let* ([info::ScmZlibInfo* (SCM_PORT_ZLIB_INFO port)]
          [strm::z_streamp (SCM_PORT_ZSTREAM port)]
          [lv::int]
          [st::int])
     (cond
      [(SCM_FALSEP compression-level) (set! lv (-> info level))]
      [(SCM_INTP compression-level) (set! lv (SCM_INT_VALUE compression-level))]
      [else (SCM_TYPE_ERROR compression-level "fixnum or #f")])
     (cond
      [(SCM_FALSEP strategy) (set! st (-> info strategy))]
      [(SCM_INTP strategy) (set! st (SCM_INT_VALUE strategy))]
      [else (SCM_TYPE_ERROR strategy "fixnum or #f")])
     (let* ([r::int (deflateParams strm lv st)])
       (unless (== r Z_OK)
         (Scm_ZlibError r "deflateParams failed: %s" (-> strm msg))))))

 (define-cproc deflating-port-full-flush (port::<deflating-port>) ::<void>
   (set! (-> (SCM_PORT_ZLIB_INFO port) flush) Z_FULL_FLUSH)
   (Scm_Flush port))

 (define-cproc zstream-adler32 (port::<deflating-port>) ::<ulong>
   (return (-> (SCM_PORT_ZSTREAM port) adler)))

 (define-cproc zstream-data-type (port::<deflating-port>) ::<int>
   (return (-> (SCM_PORT_ZSTREAM port) data_type)))

 (define-cproc zstream-dictionary-adler32 (port::<xflating-port>)
   (return (-> (SCM_PORT_ZLIB_INFO port) dict_adler)))

 (define-cproc inflate-sync (port::<inflating-port>) Scm_InflateSync)
 )
 

(export-if-defined Z_TEXT Z_FIXED)

;; body
(define (open-deflating-port source
                             :key (compression-level Z_DEFAULT_COMPRESSION)
                                  (window-bits 15)
                                  (memory-level 8)
                                  (strategy Z_DEFAULT_STRATEGY)
                                  (dictionary #f)
                                  (buffer-size 0)
                                  (owner? #f))
  (%open-deflating-port source compression-level
                        window-bits memory-level
                        strategy dictionary
                        buffer-size owner?))

;; utility procedures
(define (deflate-string str . args)
  (call-with-output-string
    (^p (let1 p2 (apply open-deflating-port p args)
          (display str p2)
          (close-output-port p2)))))

(define (inflate-string str . args)
  (port->string (apply open-inflating-port (open-input-string str) args)))

(define (gzip-encode-string str . args)
  (call-with-output-string
    (^p (let1 p2 (apply open-deflating-port p :window-bits (+ 15 16) args)
          (display str p2)
          (close-output-port p2)))))

(define (gzip-decode-string str . args)
  (port->string (apply open-inflating-port
                       (open-input-string str)
                       :window-bits (+ 15 16)
                       args)))
