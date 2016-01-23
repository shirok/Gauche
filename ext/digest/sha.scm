;;;
;;; sha - SHA-1/SHA-224/SHA-256/SHA-384/SHA-512 message-digest
;;;
;;;   Copyright (c) 2002-2003 Kimura Fuyuki, All rights reserved.
;;;   Copyright (c) 2008-2015  Shiro Kawai  <shiro@acm.org>
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


;;; Cf. RFC 3174 US Secure Hash Algorithm 1 (SHA1)

(define-module rfc.sha
  (use gauche.uvector)
  (extend util.digest)
  (export <sha1> sha1-digest sha1-digest-string
          <sha224> sha224-digest sha224-digest-string
          <sha256> sha256-digest sha256-digest-string
          <sha384> sha384-digest sha384-digest-string
          <sha512> sha512-digest sha512-digest-string))
(select-module rfc.sha)

;;;
;;;  High-level API
;;;

(define-constant *sha-unit-len* 4096)

(define (gen-digest init update end)
  (^[] (let ([ctx (make <sha-context>)]
             [buf (make-u8vector *sha-unit-len*)])
         (init ctx)
         (generator-for-each
          (^x (update ctx x))
          (^[] (let1 count (read-block! buf)
                 (cond [(eof-object? count) count]
                       [(< count *sha-unit-len*)
                        (uvector-alias <u8vector> buf 0 count)]
                       [else buf]))))
         (end ctx))))

(define sha1-digest   (gen-digest %sha1-init   %sha1-update   %sha1-final))
(define sha224-digest (gen-digest %sha224-init %sha224-update %sha224-final))
(define sha256-digest (gen-digest %sha256-init %sha256-update %sha256-final))
(define sha384-digest (gen-digest %sha384-init %sha384-update %sha384-final))
(define sha512-digest (gen-digest %sha512-init %sha512-update %sha512-final))

(define (sha1-digest-string s)   (with-input-from-string s sha1-digest))
(define (sha224-digest-string s) (with-input-from-string s sha224-digest))
(define (sha256-digest-string s) (with-input-from-string s sha256-digest))
(define (sha384-digest-string s) (with-input-from-string s sha384-digest))
(define (sha512-digest-string s) (with-input-from-string s sha512-digest))

;;;
;;; Digest framework
;;;

(define-macro (define-framework n block-size)
  (let ([meta   (string->symbol #"<sha~|n|-meta>")]
        [cls    (string->symbol #"<sha~|n|>")]
        [init   (string->symbol #"%sha~|n|-init")]
        [update (string->symbol #"%sha~|n|-update")]
        [final  (string->symbol #"%sha~|n|-final")]
        [digest (string->symbol #"sha~|n|-digest")])
    `(begin
       (define-class ,meta (<message-digest-algorithm-meta>) ())
       (define-class ,cls (<message-digest-algorithm>)
         (context)
         :metaclass ,meta
         :hmac-block-size ,block-size)
       (define-method initialize ((self ,cls) initargs)
         (next-method)
         (let1 ctx (make <sha-context>)
           (,init ctx)
           (slot-set! self 'context ctx)))
       (define-method digest-update! ((self ,cls) data)
         (,update (slot-ref self'context) data))
       (define-method digest-final! ((self ,cls))
         (,final (slot-ref self'context)))
       (define-method digest ((class ,meta))
         (,digest)))))

(define-framework 1    64)
(define-framework 224  64)
(define-framework 256  64)
(define-framework 384  128)
(define-framework 512  128)

;;;
;;; Low-level bindings
;;;

(inline-stub
 "#include <gauche/class.h>"
 ;; customization for sha2.h
 "#define SHA2_USE_INTTYPES_H" ; use uintXX_t
 "#include \"sha2.h\""

 "#define LIBGAUCHE_EXT_BODY"
 "#include <gauche/extern.h>  /* fix SCM_EXTERN in SCM_CLASS_DECL */"

 "typedef struct ScmShaContextRec {"
 " SCM_HEADER;"
 " SHA_CTX ctx;"
 "} ScmShaContext;"

 (define-cclass <sha-context> :private
   ScmShaContext* "Scm_ShaContextClass" ()
   ()
   [allocator
    (let* ([ctx :: ScmShaContext* (SCM_NEW_INSTANCE ScmShaContext klass)])
      (return (SCM_OBJ ctx)))])

 (define-cproc %sha1-init (ctx::<sha-context>) ::<void>
   (SHA1_Init (& (-> ctx ctx))))
 (define-cproc %sha224-init (ctx::<sha-context>) ::<void>
   (SHA224_Init (& (-> ctx ctx))))
 (define-cproc %sha256-init (ctx::<sha-context>) ::<void>
   (SHA256_Init (& (-> ctx ctx))))
 (define-cproc %sha384-init (ctx::<sha-context>) ::<void>
   (SHA384_Init (& (-> ctx ctx))))
 (define-cproc %sha512-init (ctx::<sha-context>) ::<void>
   (SHA512_Init (& (-> ctx ctx))))

 (define-cise-stmt common-update
   [(_ update ctx data)
    `(cond
      [(SCM_U8VECTORP ,data)
       (,update (& (-> ,ctx ctx))
                (cast (const unsigned char*)
                      (SCM_UVECTOR_ELEMENTS (SCM_U8VECTOR ,data)))
                (SCM_U8VECTOR_SIZE (SCM_U8VECTOR ,data)))]
      [(SCM_STRINGP ,data)
       (let* ([b::(const ScmStringBody*) (SCM_STRING_BODY ,data)])
         (,update (& (-> ,ctx ctx))
                  (cast (const unsigned char*) (SCM_STRING_BODY_START b))
                  (SCM_STRING_BODY_SIZE b)))]
      [else (SCM_TYPE_ERROR ,data "u8vector or string")])])

 (define-cproc %sha1-update (ctx::<sha-context> data) ::<void>
   (common-update SHA1_Update ctx data))
 (define-cproc %sha224-update (ctx::<sha-context> data) ::<void>
   (common-update SHA224_Update ctx data))
 (define-cproc %sha256-update (ctx::<sha-context> data) ::<void>
   (common-update SHA256_Update ctx data))
 (define-cproc %sha384-update (ctx::<sha-context> data) ::<void>
   (common-update SHA384_Update ctx data))
 (define-cproc %sha512-update (ctx::<sha-context> data) ::<void>
   (common-update SHA512_Update ctx data))

 (define-cise-stmt common-final
   [(_ final ctx size)
    `(let* ([digest::(.array (unsigned char) (,size))])
       (,final digest (& (-> ,ctx ctx)))
       (return (Scm_MakeString (cast (const char*) digest)
                               ,size ,size
                               (logior SCM_STRING_INCOMPLETE
                                       SCM_STRING_COPYING))))])

 (define-cproc %sha1-final (ctx::<sha-context>)
   (common-final SHA1_Final ctx SHA1_DIGEST_LENGTH))
 (define-cproc %sha224-final (ctx::<sha-context>)
   (common-final SHA224_Final ctx SHA224_DIGEST_LENGTH))
 (define-cproc %sha256-final (ctx::<sha-context>)
   (common-final SHA256_Final ctx SHA256_DIGEST_LENGTH))
 (define-cproc %sha384-final (ctx::<sha-context>)
   (common-final SHA384_Final ctx SHA384_DIGEST_LENGTH))
 (define-cproc %sha512-final (ctx::<sha-context>)
   (common-final SHA512_Final ctx SHA512_DIGEST_LENGTH))
 )


