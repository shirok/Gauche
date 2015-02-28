;;;
;;; odbm - original dbm interface
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

(define-module dbm.odbm
  (extend dbm)
  (use srfi-1)
  (export <odbm>
          ;; low level funcions
          odbm-init    odbm-close    odbm-closed?
          odbm-store   odbm-fetch   odbm-delete
          odbm-firstkey  odbm-nextkey)
  )
(select-module dbm.odbm)

;; Legacy DBM has some limitations.
;;  * only one dbm file can be opened at a time.
;;     (this is checked in C code)
;;  * no flags (read-only, read-write or create)
;;  * dbm changes file mode to #o000 when opened, to prevent
;;    other processes to access to the same file (I guess).
;;    this won't work well if the process dies before changing
;;    back the permission.  We reset the permission after
;;    opening the file.

;;;
;;; High-level dbm interface
;;;

(define-class <odbm-meta> (<dbm-meta>)
  ())

(define-class <odbm> (<dbm>)
  ()
  :metaclass <odbm-meta>)

(define-method dbm-open ((self <odbm>))
  (next-method)
  (unless (slot-bound? self 'path)
    (error "path must be set to open odbm database"))
  (let* ([path   (slot-ref self 'path)]
         [rwmode (slot-ref self 'rw-mode)]
         [fmode  (slot-ref self 'file-mode)]
         [dirfile (string-append path ".dir")]
         [pagfile (string-append path ".pag")]
         [exists? (and (file-exists? dirfile) (file-exists? pagfile))]
         [create  (^[]
                    (with-output-to-file dirfile (^[] #f))
                    (with-output-to-file pagfile (^[] #f)))])
    (case rwmode
      [(:read)
       (unless exists?
         (errorf "dbm-open: no database file ~s.dir or ~s.pag" path path))]
      [(:write)
       ;; dir and pag files must exist
       (unless exists? (create))]
      [(:create)
       ;; a trick: remove dir and pag file first if :create
       (when exists?
         (sys-unlink dirfile) (sys-unlink pagfile))
       (create)])
    (odbm-init path)
    ;; adjust mode properly
    (sys-chmod dirfile fmode)
    (sys-chmod pagfile fmode)
    self))

;;
;; close operation
;;

(define-method dbm-close ((self <odbm>))
  (odbm-close))

(define-method dbm-closed? ((self <odbm>))
  (odbm-closed?))

;;
;; common operations
;;

(define-method dbm-put! ((self <odbm>) key value)
  (next-method)
  (when (positive? (odbm-store (%dbm-k2s self key) (%dbm-v2s self value)))
    (error "dbm-put! failed" self)))

(define-method dbm-get ((self <odbm>) key . args)
  (next-method)
  (cond [(odbm-fetch (%dbm-k2s self key)) => (cut %dbm-s2v self <>)]
        [(pair? args) (car args)]     ;fall-back value
        [else  (errorf "odbm: no data for key ~s in database ~s" key self)]))

(define-method dbm-exists? ((self <odbm>) key)
  (next-method)
  (if (odbm-fetch (%dbm-k2s self key)) #t #f))

(define-method dbm-delete! ((self <odbm>) key)
  (next-method)
  (when (positive? (odbm-delete (%dbm-k2s self key)))
    (errorf "dbm-delete!: deleteting key ~s from ~s failed" key self)))

(define-method dbm-fold ((self <odbm>) proc knil)
  (next-method)
  (let loop ([key (odbm-firstkey)] [r '()])
    (if key
      (let1 val (odbm-fetch key)
        (loop (odbm-nextkey key)
              (proc (%dbm-s2k self key) (%dbm-s2v self val) r)))
      r)))


(define (odbm-files name)
  (map (cut string-append name <>) '(".pag" ".dir")))

(define-method dbm-db-exists? ((class <odbm-meta>) name)
  (every file-exists? (odbm-files name)))

(define-method dbm-db-remove ((class <odbm-meta>) name)
  (for-each sys-unlink (odbm-files name)))

(define-method dbm-db-copy ((class <odbm-meta>) from to . keys)
  (apply %dbm-copy2
         (append (append-map list (odbm-files from) (odbm-files to)) keys)))

(define-method dbm-db-move ((class <odbm-meta>) from to . keys)
  (apply %dbm-rename2
         (append (append-map list (odbm-files from) (odbm-files to))
                 keys)))

;;;
;;; Low-level binginds
;;;

(inline-stub
 "#include \"dbmconf.h\""
 "#if HAVE_DBM_H"
 "#include <dbm.h>"
 "#elif HAVE_GDBM_SLASH_DBM_H"
 "#include <gdbm/dbm.h>"
 "#elif HAVE_GDBM_MINUS_DBM_H"
 "#include <gdbm-dbm.h>"
 "#endif"

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

 (define-cise-stmt CHECK_ODBM
   [(_)
    `(unless odbm_opened (Scm_Error "odbm file already closed"))])

 ;; Original dbm allows to open only one file at a time.
 ;; The static variable odbm_opened tracks the status.
 ;; TODO: MT SAFENESS
 "static int odbm_opened = FALSE;"

 ;; bindings
 (define-cproc odbm-init (name::<string>) ::<int>
   (when odbm_opened (Scm_Error "dbm file is already opened."))
   (let* ([r::int (dbminit (Scm_GetString name))])
     (when (< r 0) (Scm_SysError "couldn't open dbm database %S" name))
     (set! odbm_opened TRUE)
     (return r)))

 (define-cproc odbm-close () ::<void>
   (when odbm_opened (dbmclose) (set! odbm_opened FALSE)))

 (define-cproc odbm-closed? () ::<boolean>
   (return (not odbm_opened)))

 (define-cproc odbm-store (key::<string> val::<string>) ::<int>
   (let* ([dkey::datum] [dval::datum])
     (CHECK_ODBM) (TO_DATUM dkey key) (TO_DATUM dval val)
     (return (store dkey dval))))

 (define-cproc odbm-fetch (key::<string>)
   (let* ([dkey::datum] [dval::datum])
     (CHECK_ODBM) (TO_DATUM dkey key) (set! dval (fetch dkey))
     (FROM_DATUM SCM_RESULT dval)))

 (define-cproc odbm-delete (key::<string>) ::<int>
   (let* ([dkey::datum])
     (CHECK_ODBM) (TO_DATUM dkey key) (return (delete dkey))))

 (define-cproc odbm-firstkey ()
   (let* ([dkey::datum])
     (CHECK_ODBM) (set! dkey (firstkey)) (FROM_DATUM SCM_RESULT dkey)))

 (define-cproc odbm-nextkey (key::<string>)
   (let* ([dkey::datum] [dnkey::datum])
     (CHECK_ODBM) (TO_DATUM dkey key) (set! dnkey (nextkey dkey))
     (FROM_DATUM SCM_RESULT dnkey)))
 )

