;;;
;;; odbm - original dbm interface
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
;;;  $Id: odbm.scm,v 1.8 2007-03-02 07:39:05 shirok Exp $
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
(dynamic-load "odbm")

;; Legacy DBM has some limitations.
;;  * only one dbm file can be opened at a time.
;;     (this is checked in C code)
;;  * no flags (read-only, read-write or create)
;;  * dbm changes file mode to #o000 when opened, to prevent
;;    other processes to access to the same file (I guess).
;;    this won't work well if the process dies before changing
;;    back the permission.  We reset the permission after
;;    opening the file.

;;
;; Initialize
;;

(define-class <odbm-meta> (<dbm-meta>)
  ())

(define-class <odbm> (<dbm>)
  ()
  :metaclass <odbm-meta>)

(define-method dbm-open ((self <odbm>))
  (next-method)
  (unless (slot-bound? self 'path)
    (error "path must be set to open odbm database"))
  (let* ((path   (slot-ref self 'path))
         (rwmode (slot-ref self 'rw-mode))
         (fmode  (slot-ref self 'file-mode))
         (dirfile (string-append path ".dir"))
         (pagfile (string-append path ".pag"))
         (exists? (and (file-exists? dirfile) (file-exists? pagfile)))
         (create  (lambda ()
                    (with-output-to-file dirfile (lambda () #f))
                    (with-output-to-file pagfile (lambda () #f)))))
    (case rwmode
      ((:read)
       (unless exists?
         (errorf "dbm-open: no database file ~s.dir or ~s.pag" path path)))
      ((:write)
       ;; dir and pag files must exist
       (unless exists? (create)))
      ((:create)
       ;; a trick: remove dir and pag file first if :create
       (when exists?
         (posix-unlink dirfile) (posix-unlink pagfile))
       (create)))
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
  (cond ((odbm-fetch (%dbm-k2s self key))
         => (lambda (v) (%dbm-s2v self v)))
        ((pair? args) (car args))     ;fall-back value
        (else  (errorf "odbm: no data for key ~s in database ~s"
                       key self))))

(define-method dbm-exists? ((self <odbm>) key)
  (next-method)
  (if (odbm-fetch (%dbm-k2s self key)) #t #f))

(define-method dbm-delete! ((self <odbm>) key)
  (next-method)
  (when (positive? (odbm-delete (%dbm-k2s self key)))
    (errorf "dbm-delete!: deleteting key ~s from ~s failed" key self)))

(define-method dbm-fold ((self <odbm>) proc knil)
  (next-method)
  (let loop ((key (odbm-firstkey))
             (r   '()))
    (if key
        (let ((val (odbm-fetch key)))
          (loop (odbm-nextkey key)
                (proc (%dbm-s2k self key) (%dbm-s2v self val) r)))
        r)))


(define (odbm-files name)
  (map (cut string-append name <>) ".pag" ".dir"))

(define-method dbm-db-exists? ((class <odbm-meta>) name)
  (every file-exists? (odbm-files name)))

(define-method dbm-db-remove ((class <odbm-meta>) name)
  (for-each sys-unlink (odbm-files name)))

(define-method dbm-db-copy ((class <odbm-meta>) from to . keys)
  (apply %dbm-copy2
         (append (append-map list (odbm-files from) (odbm-files to))
                 keys)))

(define-method dbm-db-rename ((class <odbm-meta>) from to . keys)
  (apply %dbm-rename2
         (append (append-map list (odbm-files from) (odbm-files to))
                 keys)))
  
(provide "dbm/odbm")
