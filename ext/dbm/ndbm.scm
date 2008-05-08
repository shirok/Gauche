;;;
;;; ndbm - ndbm interface
;;;  
;;;   Copyright (c) 2000-2007  Shiro Kawai  <shiro@acm.org>
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
;;;  $Id: ndbm.scm,v 1.10 2008-05-08 22:03:58 shirok Exp $
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
          |DBM_INSERT|        |DBM_REPLACE|)
  )
(select-module dbm.ndbm)
(dynamic-load "ndbm")

;;
;; Initialize
;;

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
    (errorf "ndbm ~S already opened" gdbm))
  (let* ((path   (slot-ref self 'path))
         (rwmode (slot-ref self 'rw-mode))
         (rwopt  (case rwmode
                   ((:read)   |O_RDONLY|)
                   ((:write)  (+ |O_RDWR| |O_CREAT|))
                   ((:create) (+ |O_RDWR| |O_CREAT| |O_TRUNC|))))
         (fp     (ndbm-open path
                            rwopt
                            (slot-ref self 'file-mode))))
    (slot-set! self 'ndbm-file fp)
    self))

;;
;; close operation
;;

(define-method dbm-close ((self <ndbm>))
  (let ((ndbm (ndbm-file-of self)))
    (and ndbm (ndbm-close ndbm))))

(define-method dbm-closed? ((self <ndbm>))
  (let ((ndbm (ndbm-file-of self)))
    (or (not ndbm) (ndbm-closed? ndbm))))

;;
;; common operations
;;

(define-method dbm-put! ((self <ndbm>) key value)
  (next-method)
  (when (positive? (ndbm-store (ndbm-file-of self)
                               (%dbm-k2s self key)
                               (%dbm-v2s self value)
                               |DBM_REPLACE|))
    (error "dbm-put! failed" self)))

(define-method dbm-get ((self <ndbm>) key . args)
  (next-method)
  (cond ((ndbm-fetch (ndbm-file-of self) (%dbm-k2s self key))
         => (lambda (v) (%dbm-s2v self v)))
        ((pair? args) (car args))     ;fall-back value
        (else  (errorf "ndbm: no data for key ~s in database ~s"
                       key (ndbm-file-of self)))))

(define-method dbm-exists? ((self <ndbm>) key)
  (next-method)
  (ndbm-exists? (ndbm-file-of self) (%dbm-k2s self key)))

(define-method dbm-delete! ((self <ndbm>) key)
  (next-method)
  (when (positive? (ndbm-delete (ndbm-file-of self) (%dbm-k2s self key)))
    (errorf "dbm-delete!: deleteting key ~s from ~s failed" key self)))

(define-method dbm-fold ((self <ndbm>) proc knil)
  (let ((ndbm (ndbm-file-of self)))
    (let loop ((key (ndbm-firstkey ndbm))
               (r   knil))
      (if key
          (let ((val (ndbm-fetch ndbm key)))
            (loop (ndbm-nextkey ndbm)
                  (proc (%dbm-s2k self key) (%dbm-s2v self val) r)))
          r))
    ))

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
  
(provide "dbm/ndbm")
