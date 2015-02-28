;;;
;;; ndbm - ndbm interface
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

(define-module dbm.ndbm
  (extend dbm)
  (use srfi-1)
  (use util.match)
  (export <ndbm>
          ;; low level funcions
          ndbm-open           ndbm-close            ndbm-closed?
          ndbm-store          ndbm-fetch            ndbm-exists?
          ndbm-delete
          ndbm-firstkey       ndbm-nextkey          ndbm-error
          ndbm-clearerror
          DBM_INSERT          DBM_REPLACE)
  )
(select-module dbm.ndbm)

;;;
;;; High-level dbm interface
;;;

(define-class <ndbm-meta> (<dbm-meta>)
  ())

(define-class <ndbm> (<dbm>)
  ((ndbm-file :accessor ndbm-file-of :initform #f)
   )
  :metaclass <ndbm-meta>)

(define-method dbm-open ((self <ndbm>))
  (next-method)
  (unless (slot-bound? self 'path)
    (error "path must be set to open ndbm database"))
  (when (ndbm-file-of self)
    (errorf "ndbm ~S already opened" self))
  (let* ([path   (slot-ref self 'path)]
         [rwmode (slot-ref self 'rw-mode)]
         [rwopt  (case rwmode
                   [(:read)   O_RDONLY]
                   [(:write)  (+ O_RDWR O_CREAT)]
                   [(:create) (+ O_RDWR O_CREAT O_TRUNC)])]
         [fp     (ndbm-open path
                            rwopt
                            (slot-ref self 'file-mode))])
    (slot-set! self 'ndbm-file fp)
    self))

;;
;; close operation
;;

(define-method dbm-close ((self <ndbm>))
  (let1 ndbm (ndbm-file-of self)
    (and ndbm (ndbm-close ndbm))))

(define-method dbm-closed? ((self <ndbm>))
  (let1 ndbm (ndbm-file-of self)
    (or (not ndbm) (ndbm-closed? ndbm))))

;;
;; common operations
;;

(define-method dbm-put! ((self <ndbm>) key value)
  (next-method)
  (when (positive? (ndbm-store (ndbm-file-of self)
                               (%dbm-k2s self key)
                               (%dbm-v2s self value)
                               DBM_REPLACE))
    (error "dbm-put! failed" self)))

(define-method dbm-get ((self <ndbm>) key . args)
  (next-method)
  (cond [(ndbm-fetch (ndbm-file-of self) (%dbm-k2s self key))
         => (cut %dbm-s2v self <>)]
        [(pair? args) (car args)]     ;fall-back value
        [else  (errorf "ndbm: no data for key ~s in database ~s"
                       key (ndbm-file-of self))]))

(define-method dbm-exists? ((self <ndbm>) key)
  (next-method)
  (ndbm-exists? (ndbm-file-of self) (%dbm-k2s self key)))

(define-method dbm-delete! ((self <ndbm>) key)
  (next-method)
  (when (positive? (ndbm-delete (ndbm-file-of self) (%dbm-k2s self key)))
    (errorf "dbm-delete!: deleteting key ~s from ~s failed" key self)))

(define-method dbm-fold ((self <ndbm>) proc knil)
  (let1 ndbm (ndbm-file-of self)
    (let loop ([key (ndbm-firstkey ndbm)] [r knil])
      (if key
        (let1 val (ndbm-fetch ndbm key)
          (loop (ndbm-nextkey ndbm)
                (proc (%dbm-s2k self key) (%dbm-s2v self val) r)))
        r))))

;; *ndbm-suffixes* is defined in the stub file.
(define (ndbm-files name)
  (map (cut string-append name <>) *ndbm-suffixes*))

(define-method dbm-db-exists? ((class <ndbm-meta>) name)
  (every file-exists? (ndbm-files name)))

(define-method dbm-db-remove ((class <ndbm-meta>) name)
  (for-each sys-unlink (ndbm-files name)))

(define-method dbm-db-copy ((class <ndbm-meta>) from to . keys)
  (match (ndbm-files from)
    [(f1 f2)
     (match-let1 (t1 t2) (ndbm-files to)
       (apply %dbm-copy2 f1 t1 f2 t2 keys))]
    [(f)
     (match-let1 (t) (ndbm-files to)
       (apply copy-file f t :safe #t keys))]
    [()
     (apply copy-file from to :safe #t keys)]
    [else
     (error "dbm-db-copy: ndbm using more than three files isn't supported")]))

(define-method dbm-db-move ((class <ndbm-meta>) from to . keys)
  (match (ndbm-files from)
    [(f1 f2)
     (match-let1 (t1 t2) (ndbm-files to)
       (apply %dbm-rename2 f1 t1 f2 t2 keys))]
    [(f)
     (match-let1 (t) (ndbm-files to)
       (apply move-file f t :safe #t keys))]
    [()
     (apply move-file from to :safe #t keys)]
    [else
     (error "dbm-db-move: ndbm using more than three files isn't supported")]))

;;;
;;; Low-level bindings
;;;

(inline-stub
 "#include <fcntl.h>"
 "#include \"dbmconf.h\""

 "#if HAVE_NDBM_H"
 "#include <ndbm.h>"
 "#elif HAVE_GDBM_SLASH_NDBM_H"
 "#include <gdbm/ndbm.h>"
 "#elif HAVE_GDBM_MINUS_NDBM_H"
 "#include <gdbm-ndbm.h>"
 "#endif"

 "#include \"ndbm-suffixes.h\""

 "typedef struct ScmNdbmFileRec {"
 "  SCM_HEADER;"
 "  ScmObj name;"
 "  DBM *dbf;                   /* NULL if closed */"
 "} ScmNdbmFile;"

 (define-cclass <ndbm-file> :private ScmNdbmFile* "Scm_NdbmFileClass" ()
   ()
   [printer
    (Scm_Printf port "#<ndbm-file %S>" (-> (SCM_NDBM_FILE obj) name))])

 (define-cfn ndbm_finalize (obj data::void*) ::void :static
   (let* ((n::ScmNdbmFile* (SCM_NDBM_FILE obj)))
     (when (-> n dbf)
       (dbm_close (-> n dbf))
       (set! (-> n dbf) NULL))))

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
                                       -1 SCM_STRING_COPYING))]
           [else
            (set! ,scm SCM_FALSE)])])

 (define-cise-stmt CHECK_NDBM
   [(_ g)
    `(unless (-> ,g dbf) (Scm_Error "ndbm file already closed: %S" ,g))])

 ;; suffixes
 (define-variable *ndbm-suffixes*
   (c (Scm_CStringArrayToList ndbm_suffixes -1 SCM_STRING_IMMUTABLE)))

 ;; bindings
 (define-cproc ndbm-open (name::<string> flags::<fixnum> mode::<fixnum>)
   (let* ([z::ScmNdbmFile* (SCM_NEW ScmNdbmFile)])
     (SCM_SET_CLASS z (& Scm_NdbmFileClass))
     (Scm_RegisterFinalizer (SCM_OBJ z) ndbm_finalize NULL)
     (set! (-> z name) (SCM_OBJ name))
     (set! (-> z dbf) (dbm_open (Scm_GetString name) flags mode))
     (when (== (-> z dbf) NULL)
       (Scm_SysError "couldn't open ndbm file %S" name))
     (return (SCM_OBJ z))))

 (define-cproc ndbm-close (ndbm::<ndbm-file>) ::<void>
   (when (-> ndbm dbf)
     (dbm_close (-> ndbm dbf))
     (set! (-> ndbm dbf) NULL)))

 (define-cproc ndbm-closed? (ndbm::<ndbm-file>) ::<boolean>
   (return (== (-> ndbm dbf) NULL)))

 (define-cproc ndbm-store (ndbm::<ndbm-file> key::<string> val::<string>
                                             :optional (flags::<fixnum> 0))
   ::<int>
   (let* ([dkey::datum] [dval::datum])
     (CHECK_NDBM ndbm)
     (TO_DATUM dkey key)
     (TO_DATUM dval val)
     (return (dbm_store (-> ndbm dbf) dkey dval flags))))

 (define-cproc ndbm-fetch (ndbm::<ndbm-file> key::<string>)
   (let* ([dkey::datum] [dval::datum])
     (CHECK_NDBM ndbm)
     (TO_DATUM dkey key)
     (set! dval (dbm_fetch (-> ndbm dbf) dkey))
     (FROM_DATUM SCM_RESULT dval)))

 (define-cproc ndbm-exists? (ndbm::<ndbm-file> key::<string>) ::<boolean>
   (let* ([dkey::datum] [dval::datum])
     (CHECK_NDBM ndbm)
     (TO_DATUM dkey key)
     (set! dval (dbm_fetch (-> ndbm dbf) dkey))
     (return (!= (ref dval dptr) NULL))))

 (define-cproc ndbm-delete (ndbm::<ndbm-file> key::<string>) ::<int>
   (let* ([dkey::datum])
     (CHECK_NDBM ndbm)
     (TO_DATUM dkey key)
     (return (dbm_delete (-> ndbm dbf) dkey))))

 (define-cproc ndbm-firstkey (ndbm::<ndbm-file>)
   (let* ([dkey::datum])
     (CHECK_NDBM ndbm)
     (set! dkey (dbm_firstkey (-> ndbm dbf)))
     (FROM_DATUM SCM_RESULT dkey)))

 (define-cproc ndbm-nextkey (ndbm::<ndbm-file>)
   (let* ([dkey::datum])
     (CHECK_NDBM ndbm)
     (set! dkey (dbm_nextkey (-> ndbm dbf)))
     (FROM_DATUM SCM_RESULT dkey)))

 (define-cproc ndbm-error (ndbm::<ndbm-file>) ::<int>
   (CHECK_NDBM ndbm)
   (return (dbm_error (-> ndbm dbf))))

 (define-cproc ndbm-clearerror (ndbm::<ndbm-file>) ::<void>
   (CHECK_NDBM ndbm)
   (dbm_clearerr (-> ndbm dbf)))

 (define-enum DBM_INSERT)
 (define-enum DBM_REPLACE)
 (define-enum O_RDONLY)
 (define-enum O_WRONLY)
 (define-enum O_RDWR)
 (define-enum O_CREAT)
 (define-enum O_TRUNC)
 )

