;;;
;;; gdbm - gdbm interface
;;;
;;;   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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

(define-module dbm.gdbm
  (extend dbm)
  (export <gdbm>
          ;; low-level functions
          gdbm-open          gdbm-close          gdbm-closed?
          gdbm-store         gdbm-fetch          gdbm-delete
          gdbm-firstkey      gdbm-nextkey        gdbm-reorganize
          gdbm-sync          gdbm-exists?        gdbm-strerror
          gdbm-setopt        gdbm-version        gdbm-file-of
          gdbm-errno
          GDBM_READER        GDBM_WRITER         GDBM_WRCREAT
          GDBM_NEWDB         GDBM_FAST           GDBM_SYNC
          GDBM_NOLOCK        GDBM_INSERT         GDBM_REPLACE
          GDBM_CACHESIZE     GDBM_FASTMODE       GDBM_SYNCMODE
          GDBM_CENTFREE      GDBM_COALESCEBLKS)
  )
(select-module dbm.gdbm)

;;;
;;; High-level dbm interface
;;;

(define-class <gdbm-meta> (<dbm-meta>)
  ())

(define-class <gdbm> (<dbm>)
  ((gdbm-file :accessor gdbm-file-of :initform #f)
   (sync      :init-keyword :sync   :initform #f)
   (nolock    :init-keyword :nolock :initform #f)
   (bsize     :init-keyword :bsize  :initform 0)
   )
  :metaclass <gdbm-meta>)

(define-method dbm-open ((self <gdbm>))
  (next-method)
  (unless (slot-bound? self 'path)
    (error "path must be set to open gdbm database"))
  (when (gdbm-file-of self)
    (errorf "gdbm ~S already opened" self))
  (let* ([path   (slot-ref self 'path)]
         [rwmode (slot-ref self 'rw-mode)]
         [sync   (slot-ref self 'sync)]
         [nolock (slot-ref self 'nolock)]
         [rwopt  (case rwmode
                   [(:read) GDBM_READER]
                   [(:write) (+ GDBM_WRCREAT
                                 (if sync GDBM_SYNC 0)
                                 (if nolock GDBM_NOLOCK 0))]
                   [(:create) (+ GDBM_NEWDB
                                 (if sync GDBM_SYNC 0)
                                 (if nolock GDBM_NOLOCK 0))])]
         [fp     (gdbm-open path
                            (slot-ref self 'bsize)
                            rwopt
                            (slot-ref self 'file-mode))])
    (slot-set! self 'gdbm-file fp)
    self))

;;
;; close operation
;;

(define-method dbm-close ((self <gdbm>))
  (let1 gdbm (gdbm-file-of self)
    (and gdbm (gdbm-close gdbm))))

(define-method dbm-closed? ((self <gdbm>))
  (let1 gdbm (gdbm-file-of self)
    (or (not gdbm) (gdbm-closed? gdbm))))

;;
;; accessors
;;

(define-method dbm-put! ((self <gdbm>) key value)
  (next-method)
  (when (positive? (gdbm-store (gdbm-file-of self)
                               (%dbm-k2s self key)
                               (%dbm-v2s self value)
                               GDBM_REPLACE))
    (error "dbm-put! failed" self)))

(define-method dbm-get ((self <gdbm>) key . args)
  (next-method)
  (cond [(gdbm-fetch (gdbm-file-of self) (%dbm-k2s self key))
         => (cut %dbm-s2v self <>)]
        [(pair? args) (car args)]     ;fall-back value
        [else  (errorf "gdbm: no data for key ~s in database ~s"
                       key (gdbm-file-of self))]))

(define-method dbm-exists? ((self <gdbm>) key)
  (next-method)
  (gdbm-exists? (gdbm-file-of self) (%dbm-k2s self key)))

(define-method dbm-delete! ((self <gdbm>) key)
  (next-method)
  (when (positive? (gdbm-delete (gdbm-file-of self) (%dbm-k2s self key)))
    (errorf "dbm-delete!: deleteting key ~s from ~s failed" key self)))

;;
;; Iterations
;;

(define-method dbm-fold ((self <gdbm>) proc knil)
  (let1 gdbm (gdbm-file-of self)
    (let loop ([key (gdbm-firstkey gdbm)] [r knil])
      (if key
        (let1 val (gdbm-fetch gdbm key)
          (loop (gdbm-nextkey gdbm key)
                (proc (%dbm-s2k self key) (%dbm-s2v self val) r)))
        r))))

;;
;; Metaoperations
;;

(autoload file.util copy-file move-file)

(define (%with-gdbm-locking path thunk)
  (let1 db (gdbm-open path 0 GDBM_READER #o664) ;; put read-lock
    (unwind-protect (thunk) (gdbm-close db))))

(define-method dbm-db-exists? ((class <gdbm-meta>) name)
  (file-exists? name))

(define-method dbm-db-remove ((class <gdbm-meta>) name)
  (sys-unlink name))

(define-method dbm-db-copy ((class <gdbm-meta>) from to . keys)
  (%with-gdbm-locking from
   (^[] (apply copy-file from to :safe #t keys))))

(define-method dbm-db-move ((class <gdbm-meta>) from to . keys)
  (%with-gdbm-locking from
   (^[] (apply move-file from to :safe #t keys))))

;;;
;;; Low-level bindings
;;;

(inline-stub
 "#include <gdbm.h>"
 "#include <stdlib.h>"

 "typedef struct ScmGdbmFileRec {
    SCM_HEADER;
    ScmObj name;
    GDBM_FILE dbf;              /* NULL if closed */
  } ScmGdbmFile;"

 (define-cclass <gdbm-file> :private ScmGdbmFile* "Scm_GdbmFileClass" ()
   ()
   [printer
    (Scm_Printf port "#<gdbm-file %S>" (-> (SCM_GDBM_FILE obj) name))])

 (define-cfn gdbm_finalize (obj data::void*) ::void :static
   (let* ((g :: ScmGdbmFile* (SCM_GDBM_FILE obj)))
     (when (-> g dbf)
       (gdbm_close (-> g dbf))
       (set! (-> g dbf) NULL))))

 ;; data conversion macros
 (define-cise-stmt TO_DATUM
   [(_ datum scm)
    (let ((tmp (gensym)))
      `(let* ((,tmp :: (const ScmStringBody*) (SCM_STRING_BODY ,scm)))
         (set! (ref ,datum dptr)  (cast char* (SCM_STRING_BODY_START ,tmp)))
         (set! (ref ,datum dsize) (SCM_STRING_BODY_SIZE ,tmp))))])

 (define-cise-stmt FROM_DATUM
   [(_ scm datum)
    `(cond [(ref ,datum dptr)
            (set! ,scm (Scm_MakeString (ref ,datum dptr) (ref ,datum dsize)
                                       -1 SCM_STRING_COPYING))
            (free (ref ,datum dptr))]
           [else
            (set! ,scm SCM_FALSE)])])

 (define-cise-stmt CHECK_GDBM
   [(_ g)
    `(unless (-> ,g dbf) (Scm_Error "gdbm file already closed: %S" ,g))])

 ;; Those symbols may not be defined in the older gdbm
 "#ifndef GDBM_SYNC"
 "#define GDBM_SYNC 0"
 "#endif"
 "#ifndef GDBM_NOLOCK"
 "#define GDBM_NOLOCK 0"
 "#endif"
 "#ifndef GDBM_SYNCMODE"
 "#define GDBM_SYNCMODE 0"
 "#endif"
 "#ifndef GDBM_CENTFREE"
 "#define GDBM_CENTFREE 0"
 "#endif"
 "#ifndef GDBM_COALESCEBLKS"
 "#define GDBM_COALESCEBLKS 0"
 "#endif"

 (define-cproc gdbm-open
   (name::<string> :optional (size::<fixnum> 0)
                   (rwmode::<fixnum> (c "SCM_MAKE_INT(GDBM_READER)"))
                   (fmode::<fixnum> (c "SCM_MAKE_INT(0666)")))
   (let* ([z::ScmGdbmFile* (SCM_NEW ScmGdbmFile)])
     (SCM_SET_CLASS z (& Scm_GdbmFileClass))
     (Scm_RegisterFinalizer (SCM_OBJ z) gdbm_finalize NULL)
     (set! (-> z name) (SCM_OBJ name))
     (set! (-> z dbf) (gdbm_open (Scm_GetString name) size rwmode fmode NULL))
     (when (== (-> z dbf) NULL)
       (Scm_Error "couldn't open gdbm file %S (gdbm_errno=%d)" name gdbm_errno))
     (return (SCM_OBJ z))))

 (define-cproc gdbm-close (gdbm::<gdbm-file>) ::<void>
   (when (-> gdbm dbf)
     (gdbm_close (-> gdbm dbf))
     (set! (-> gdbm dbf) NULL)))

 (define-cproc gdbm-closed? (gdbm::<gdbm-file>) ::<boolean>
   (return (== (-> gdbm dbf) NULL)))

 (define-cproc gdbm-store (gdbm::<gdbm-file>
                           key::<string> val::<string>
                           :optional (flags::<fixnum> 0))
   ::<int>
   (let* ([dkey::datum] [dval::datum])
     (CHECK_GDBM gdbm)
     (TO_DATUM dkey key)
     (TO_DATUM dval val)
     (return (gdbm_store (-> gdbm dbf) dkey dval flags))))

 (define-cproc gdbm-fetch (gdbm::<gdbm-file> key::<string>)
   (let* ([dkey::datum] [dval::datum])
     (CHECK_GDBM gdbm)
     (TO_DATUM dkey key)
     (set! dval (gdbm_fetch (-> gdbm dbf) dkey))
     (FROM_DATUM SCM_RESULT dval)))

 (define-cproc gdbm-delete (gdbm::<gdbm-file> key::<string>) ::<int>
   (let* ([dkey::datum])
     (CHECK_GDBM gdbm)
     (TO_DATUM dkey key)
     (return (gdbm_delete (-> gdbm dbf) dkey))))

 (define-cproc gdbm-firstkey (gdbm::<gdbm-file>)
   (let* ([dkey::datum (gdbm_firstkey (-> gdbm dbf))])
     (FROM_DATUM SCM_RESULT dkey)))

 (define-cproc gdbm-nextkey (gdbm::<gdbm-file> key::<string>)
   (let* ([dkey::datum] [dnkey::datum])
     (CHECK_GDBM gdbm)
     (TO_DATUM dkey key)
     (set! dnkey (gdbm_nextkey (-> gdbm dbf) dkey))
     (FROM_DATUM SCM_RESULT dnkey)))

 (define-cproc gdbm-reorganize (gdbm::<gdbm-file>) ::<int>
   (CHECK_GDBM gdbm)
   (return (gdbm_reorganize (-> gdbm dbf))))

 (define-cproc gdbm-sync (gdbm::<gdbm-file>) ::<void>
   (CHECK_GDBM gdbm)
   (gdbm_sync (-> gdbm dbf)))

 (define-cproc gdbm-exists? (gdbm::<gdbm-file> key::<string>) ::<boolean>
   (let* ([dkey::datum])
     (CHECK_GDBM gdbm)
     (TO_DATUM dkey key)
     (return (gdbm_exists (-> gdbm dbf) dkey))))

 (define-cproc gdbm-strerror (err_num::<fixnum>)
   (return (SCM_MAKE_STR_IMMUTABLE (gdbm_strerror err_num))))

 (define-cproc gdbm-setopt (gdbm::<gdbm-file> option::<fixnum> val) ::<int>
   (let* ([ival::int])
     (CHECK_GDBM gdbm)
     (if (SCM_EXACTP val)
       (set! ival (Scm_GetUInteger val))
       (set! ival (not (SCM_FALSEP val))))
     (return (gdbm_setopt (-> gdbm dbf) option (& ival) (sizeof (int))))))

 (define-cproc gdbm-version ()
   (return (SCM_MAKE_STR_IMMUTABLE gdbm_version)))

 (define-cproc gdbm-errno () ::<int>
   (let* ([r::int gdbm_errno])
     (set! gdbm_errno 0)
     (return r)))

 (define-enum GDBM_READER)
 (define-enum GDBM_WRITER)
 (define-enum GDBM_WRCREAT)
 (define-enum GDBM_NEWDB)
 (define-enum GDBM_FAST)
 (define-enum GDBM_SYNC)
 (define-enum GDBM_NOLOCK)
 (define-enum GDBM_INSERT)
 (define-enum GDBM_REPLACE)
 (define-enum GDBM_CACHESIZE)
 (define-enum GDBM_FASTMODE)
 (define-enum GDBM_SYNCMODE)
 (define-enum GDBM_CENTFREE)
 (define-enum GDBM_COALESCEBLKS)
 )


