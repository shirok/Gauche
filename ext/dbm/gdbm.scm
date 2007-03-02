;;;
;;; gdbm - gdbm interface
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
;;;  $Id: gdbm.scm,v 1.12 2007-03-02 07:39:05 shirok Exp $
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
          |GDBM_READER|      |GDBM_WRITER|       |GDBM_WRCREAT|
          |GDBM_NEWDB|       |GDBM_FAST|         |GDBM_SYNC|
          |GDBM_NOLOCK|      |GDBM_INSERT|       |GDBM_REPLACE|
          |GDBM_CACHESIZE|   |GDBM_FASTMODE|     |GDBM_CENTFREE|
          |GDBM_COALESCEBLKS|)
  )
(select-module dbm.gdbm)
(dynamic-load "gdbm")

;;
;; Initialize
;;

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
    (errorf "gdbm ~S already opened" gdbm))
  (let* ((path   (slot-ref self 'path))
         (rwmode (slot-ref self 'rw-mode))
         (sync   (slot-ref self 'sync))
         (nolock (slot-ref self 'nolock))
         (rwopt  (case rwmode
                   ((:read) |GDBM_READER|)
                   ((:write) (+ |GDBM_WRCREAT|
                                 (if sync |GDBM_SYNC| 0)
                                 (if nolock |GDBM_NOLOCK| 0)))
                   ((:create) (+ |GDBM_NEWDB|
                                 (if sync |GDBM_SYNC| 0)
                                 (if nolock |GDBM_NOLOCK| 0)))))
         (fp     (gdbm-open path
                            (slot-ref self 'bsize)
                            rwopt
                            (slot-ref self 'file-mode))))
    (slot-set! self 'gdbm-file fp)
    self))

;;
;; close operation
;;

(define-method dbm-close ((self <gdbm>))
  (let ((gdbm (gdbm-file-of self)))
    (and gdbm (gdbm-close gdbm))))

(define-method dbm-closed? ((self <gdbm>))
  (let ((gdbm (gdbm-file-of self)))
    (or (not gdbm) (gdbm-closed? gdbm))))

;;
;; accessors
;;

(define-method dbm-put! ((self <gdbm>) key value)
  (next-method)
  (when (positive? (gdbm-store (gdbm-file-of self)
                               (%dbm-k2s self key)
                               (%dbm-v2s self value)
                               |GDBM_REPLACE|))
    (error "dbm-put! failed" self)))

(define-method dbm-get ((self <gdbm>) key . args)
  (next-method)
  (cond ((gdbm-fetch (gdbm-file-of self) (%dbm-k2s self key))
         => (lambda (v) (%dbm-s2v self v)))
        ((pair? args) (car args))     ;fall-back value
        (else  (errorf "gdbm: no data for key ~s in database ~s"
                       key (gdbm-file-of self)))))

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
  (let ((gdbm (gdbm-file-of self)))
    (let loop ((key (gdbm-firstkey gdbm))
               (r   knil))
      (if key
          (let ((val (gdbm-fetch gdbm key)))
            (loop (gdbm-nextkey gdbm key)
                  (proc (%dbm-s2k self key) (%dbm-s2v self val) r)))
          r))))

;;
;; Metaoperations
;;

(autoload file.util copy-file move-file)

(define (%with-gdbm-locking path thunk)
  (let1 db (gdbm-open path 0 |GDBM_READER| #o664) ;; put read-lock
    (unwind-protect (thunk) (gdbm-close db))))
        
(define-method dbm-db-exists? ((class <gdbm-meta>) name)
  (file-exists? name))

(define-method dbm-db-remove ((class <gdbm-meta>) name)
  (sys-unlink name))

(define-method dbm-db-copy ((class <gdbm-meta>) from to . keys)
  (%with-gdbm-locking from
   (lambda () (apply copy-file from to :safe #t keys))))

(define-method dbm-db-rename ((class <gdbm-meta>) from to . keys)
  (%with-gdbm-locking from
   (lambda () (apply move-file from to :safe #t keys))))

(provide "dbm/gdbm")
