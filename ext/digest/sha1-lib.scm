;;;
;;; sha1 - SHA-1 message-digest
;;;
;;;   Copyright (c) 2002-2003 Kimura Fuyuki, All rights reserved.
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
;;;  $Id: sha1.scm,v 1.6 2004-12-15 12:46:06 shirok Exp $
;;;

;;; RFC 3174 US Secure Hash Algorithm 1 (SHA1)

(define-module rfc.sha1
  (use gauche.uvector)
  (extend util.digest)
  (export <sha1> sha1-digest sha1-digest-string)
  )
(select-module rfc.sha1)

;;;
;;;  High-level API
;;;

(define-class <sha1-meta> (<message-digest-algorithm-meta>)
  ())

(define-class <sha1> (<message-digest-algorithm>)
  ((context :getter context-of))
  :metaclass <sha1-meta>)

(define-method initialize ((self <sha1>) initargs)
  (next-method)
  (slot-set! self 'context (make <sha1-context>)))

;(define (sha1-digest)
;  (let ((sha1 (make <sha1-context>)))
;    (port-for-each
;     (lambda (b) (%sha1-update sha1 b))
;     (lambda () (read-block 4096)))
;    (%sha1-final sha1)))

(define-constant *sha1-unit-len* 4096)

(define (sha1-digest)
  (let ((sha1 (make <sha1-context>))
        (buf (make-u8vector *sha1-unit-len*)))
    (port-for-each
     (lambda (x) (%sha1-update sha1 x))
     (lambda ()
       (let1 count (read-block! buf)
         (if (eof-object? count)
           count
           (if (< count *sha1-unit-len*)
             (uvector-alias <u8vector> buf 0 count)
             buf)))))
    (%sha1-final sha1)))

(define (sha1-digest-string string)
  (with-input-from-string string sha1-digest))

;;;
;;; Digest framework
;;;
(define-method digest-update! ((self <sha1>) data)
  (%sha1-update (context-of self) data))
(define-method digest-final! ((self <sha1>))
  (%sha1-final (context-of self)))
(define-method digest ((class <sha1-meta>))
  (sha1-digest))

;;;
;;; Low-level bindings
;;;

(inline-stub
 "#include <gauche/uvector.h>"
 "#include <gauche/class.h>"
 "#include \"sha.h\""

 "#define LIBGAUCHE_EXT_BODY"
 "#include <gauche/extern.h>  /* fix SCM_EXTERN in SCM_CLASS_DECL */"
 
 "typedef struct ScmSha1Rec {"
 " SCM_HEADER;"
 " SHA_CTX ctx;"
 "} ScmSha1;"

 "SCM_CLASS_DECL(Scm_Sha1Class);"
 "static ScmObj sha1_allocate(ScmClass *, ScmObj);"
 "SCM_DEFINE_BUILTIN_CLASS(Scm_Sha1Class,"
 "                         NULL, NULL, NULL, sha1_allocate, NULL);"

 "#define SCM_CLASS_SHA1      (&Scm_Sha1Class)"
 "#define SCM_SHA1(obj)       ((ScmSha1*)obj)"
 "#define SCM_SHA1P(obj)      SCM_XTYPEP(obj, SCM_CLASS_SHA1)"

 (define-cfn sha1_allocate ((klass :: ScmClass*) initargs) :static
   (let* ((sha1 :: ScmSha1* (SCM_ALLOCATE ScmSha1 klass)))
     (SCM_SET_CLASS sha1 klass)
     (SHAInit (& (-> sha1 ctx)))
     (return (SCM_OBJ sha1))))

 (initcode (Scm_InitStaticClass (& Scm_Sha1Class) "<sha1-context>" mod NULL 0))
 (define-type <sha1> "ScmSha1*")

 (define-cproc %sha1-update (sha1::<sha1> data)
   (body <void>
         (cond
          [(SCM_U8VECTORP data)
           (SHAUpdate (& (-> sha1 ctx))
                      (cast |const unsigned char*|
                            (SCM_UVECTOR_ELEMENTS (SCM_U8VECTOR data)))
                      (SCM_U8VECTOR_SIZE (SCM_U8VECTOR data)))]
          [(SCM_STRINGP data)
           (let* ((b :: |const ScmStringBody*| (SCM_STRING_BODY data)))
             (SHAUpdate (& (-> sha1 ctx))
                        (cast |const unsigned char*| (SCM_STRING_BODY_START b))
                        (SCM_STRING_BODY_SIZE b)))]
          [else
           (Scm_Error "u8vector or string required, but got: %S" data)])))

 (define-cproc %sha1-final (sha1::<sha1>)
   (body <top>
         (let* ((|digest[20]| :: |unsigned char|))
           (SHAFinal digest (& (-> sha1 ctx)))
           (result (Scm_MakeString (cast |const char*| digest)
                                   20 20
                                   (logior SCM_STRING_INCOMPLETE
                                           SCM_STRING_COPYING))))))
 )

(provide "rfc/sha1")
