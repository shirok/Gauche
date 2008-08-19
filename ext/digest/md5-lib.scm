;;;
;;; md5 - MD5 message-digest
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
;;;  $Id: md5.scm,v 1.7 2004-12-15 12:46:06 shirok Exp $
;;;

;;; RFC 1321 The MD5 Message-Digest Algorithm

(define-module rfc.md5
  (extend util.digest)
  (use gauche.uvector)
  (export <md5> md5-digest md5-digest-string)
  )
(select-module rfc.md5)

;;;
;;; High-level stuff
;;;

(define-class <md5-meta> (<message-digest-algorithm-meta>)
  ())

(define-class <md5> (<message-digest-algorithm>)
  ((context :getter context-of))
  :metaclass <md5-meta>)

(define-method initialize ((self <md5>) initargs)
  (next-method)
  (slot-set! self 'context (make <md5-context>)))

;(define (md5-digest)
;  (let ((md5 (make <md5-context>)))
;    (port-for-each
;     (lambda (b) (%md5-update md5 b))
;     (lambda () (read-block 4096)))
;    (%md5-final md5)))

(define-constant *md5-unit-len* 4096)

(define (md5-digest)
  (let ((md5 (make <md5-context>))
        (buf (make-u8vector *md5-unit-len*)))
    (port-for-each
     (lambda (x) (%md5-update md5 x))
     (lambda ()
       (let1 count (read-block! buf)
         (if (eof-object? count)
           count
           (if (< count *md5-unit-len*)
             (uvector-alias <u8vector> buf 0 count)
             buf)))))
    (%md5-final md5)))

(define (md5-digest-string string)
  (with-input-from-string string md5-digest))

;;;
;;; Digest framework
;;;
(define-method digest-update! ((self <md5>) data)
  (%md5-update (context-of self) data))
(define-method digest-final! ((self <md5>))
  (%md5-final (context-of self)))
(define-method digest ((class <md5-meta>))
  (md5-digest))

;;;
;;; Low-level bindings
;;;

(inline-stub
 "#include <gauche/class.h>"
 "#include <gauche/uvector.h>"
 "#include \"md5.h\""

 "#define LIBGAUCHE_EXT_BODY"
 "#include <gauche/extern.h>  /* fix SCM_EXTERN in SCM_CLASS_DECL */"

 "typedef struct ScmMd5Rec {"
 "  SCM_HEADER;"
 "  MD5_CTX ctx;"
 "} ScmMd5;"

 "SCM_CLASS_DECL(Scm_Md5Class);"
 "static ScmObj md5_allocate(ScmClass *, ScmObj);"
 "SCM_DEFINE_BUILTIN_CLASS(Scm_Md5Class,"
 "                         NULL, NULL, NULL, md5_allocate, NULL);"
 "#define SCM_CLASS_MD5      (&Scm_Md5Class)"
 "#define SCM_MD5(obj)       ((ScmMd5*)obj)"
 "#define SCM_MD5P(obj)      SCM_XTYPEP(obj, SCM_CLASS_MD5)"

 (define-cfn md5_allocate ((klass :: ScmClass*) initargs) :static
   (let* ((md5 :: ScmMd5* (SCM_ALLOCATE ScmMd5 klass)))
     (SCM_SET_CLASS md5 klass)
     (MD5Init (& (-> md5 ctx)))
     (return (SCM_OBJ md5))))

 (initcode (Scm_InitStaticClass (& Scm_Md5Class) "<md5-context>" mod NULL 0))
 (define-type <md5> "ScmMd5*")

 (define-cproc %md5-update (md5::<md5> data)
   (body <void>
         (cond
          [(SCM_U8VECTORP data)
           (MD5Update (& (-> md5 ctx))
                      (SCM_UVECTOR_ELEMENTS (SCM_U8VECTOR data))
                      (SCM_U8VECTOR_SIZE (SCM_U8VECTOR data)))]
          [(SCM_STRINGP data)
           (let* ((b :: |const ScmStringBody *| (SCM_STRING_BODY data)))
             (MD5Update (& (-> md5 ctx))
                        (cast |const unsigned char*| (SCM_STRING_BODY_START b))
                        (SCM_STRING_BODY_SIZE b)))]
          [else
           (Scm_Error "u8vector or string required, but got: %S" data)])))

 (define-cproc %md5-final (md5::<md5>)
   (body <top>
         (let* ((|digest[16]| :: |unsigned char|))
           (MD5Final digest (& (-> md5 ctx)))
           (result (Scm_MakeString (cast |char *| digest)
                                   16 16
                                   (logior SCM_STRING_INCOMPLETE
                                           SCM_STRING_COPYING))))))
 )

(provide "rfc/md5")
