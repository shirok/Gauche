;;;
;;; fsdbm - dbm on filesystem
;;;
;;;   Copyright (c) 2003-2019  Shiro Kawai  <shiro@acm.org>
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

(define-module dbm.fsdbm
  (extend dbm)
  (use gauche.fcntl)
  (use file.util)
  (use srfi-1)
  (use srfi-13)
  (export <fsdbm>)
  )
(select-module dbm.fsdbm)

;;; fsdbm uses a filesystem to store dbm-type database.
;;; A key is filename, and its value is the content of the file.
;;; Obviously, it is not suitable for the database that has
;;; lots of entries, have entries deleted and added very frequently,
;;; or has long keys.  The advantage is when the number of entries
;;; are relatively small, and the values are relatively large while
;;; keys are small.
;;;
;;; Fsdbm pathname is used for a directory that stores the data.
;;; The top-level directory has 37 data directories (z[0-9a-z_]),
;;; 'Incoming' directory, and a version file (FSDBM).
;;;
;;; When a new entry is added to the database, it first creates
;;; the file whose name is the key under Incoming directory, and
;;; stores the value into the file.  Then it moves the file to
;;; one of the data directories, depending on the hash value of the key.
;;;
;;; If key has non-alphanumeric characters, it is encoded into _XX
;;; manner.  If key is very long (more than 200 bytes) it is
;;; chopped by 200 bytes each, and every chunk but the last one
;;; are used as a directory name.
;;;
;;; Modification of the entry is done in the same manner.  The new
;;; entry is prepared in Incoming directory, and then replaced to
;;; the current entry.
;;;
;;; With this scheme, no race condition will occur in each entry's
;;; read/write.  It is naturally taken care of by the file system.
;;; It uses fcntl advisory lock to prevent race conditions that involve
;;; more than one entries, whenever available.

(define-constant *fsdbm-version*   "1.0")
(define-constant *version-file*    "Fsdbm")
(define-constant *incoming-dir*    "Incoming")
(define-constant *file-name-limit* 200)

(define-constant *hash-chars*
  "_0123456789abcdefghijklmnopqrstuvwxyz")
(define-constant *hash-range* (string-length *hash-chars*))
(define-constant *hash-dirs*  (map string (string->list *hash-chars*)))

(define-class <fsdbm-meta> (<dbm-meta>)
  ())

(define-class <fsdbm> (<dbm>)
  ((closed? :init-value #f))
  :metaclass <fsdbm-meta>)

(define-method dbm-open ((self <fsdbm>))
  (next-method)
  (unless (slot-bound? self 'path)
    (error "path must be set to open fsdbm database"))
  (let ((path    (ref self 'path))
        (rwmode  (ref self 'rw-mode))
        (fmode   (ref self 'file-mode)))
    (case rwmode
      [(:read)
       (unless (fsdbm-directory? path)
         (errorf "dbm-open: no fsdbm database ~a" path))]
      [(:write)
       (unless (fsdbm-directory? path)
         (fsdbm-create path fmode))]
      [(:create)
       (fsdbm-create path fmode)]
      )
    self))

(define-method dbm-close ((self <fsdbm>))
  (set! (ref self 'closed?) #t)
  #t)


(define-method dbm-closed? ((self <fsdbm>))
  (ref self 'closed?))

;; dbm protocols

(define-method dbm-put! ((self <fsdbm>) key value)
  (next-method)
  (let* ((k (%dbm-k2s self key))
         (inpath (build-path (ref self 'path) *incoming-dir* (key->path k)))
         (path   (value-file-path k (ref self 'path))))
    (make-directory* (sys-dirname path) (dir-perm (ref self 'file-mode)))
    (make-directory* (sys-dirname inpath) (dir-perm (ref self 'file-mode)))
    (with-output-to-file inpath
      (lambda () (display (%dbm-v2s self value)))
      :if-exists :error) ;; should it be error?
    (sys-rename inpath path)))

(define-method dbm-get ((self <fsdbm>) key . args)
  (next-method)
  (let ((path (value-file-path (%dbm-k2s self key) (ref self 'path))))
    (cond ((call-with-input-file path
             (^p (and p (%dbm-s2v self (read-chunk p))))
             :if-does-not-exist #f))
          ((pair? args) (car args))
          (else (errorf "fsdbm: no data for key ~s in database ~s"
                        key self)))))

(define-method dbm-exists? ((self <fsdbm>) key)
  (next-method)
  (file-exists? (value-file-path (%dbm-k2s self key) (ref self 'path))))

(define-method dbm-delete! ((self <fsdbm>) key)
  (next-method)
  (sys-unlink (value-file-path (%dbm-k2s self key) (ref self 'path))))

(define-method dbm-fold ((self <fsdbm>) proc seed)
  (define prefix-len
    (string-length (build-path (ref self 'path) "a/")))
  (define (apply-kv path seed)
    (if (file-is-directory? path)
      (fold apply-kv seed
            (directory-list path :add-path? #t :children? #t))
      (let ((k (path->key (string-drop path prefix-len))))
        (if k
          (proc (%dbm-s2k self k)
                (call-with-input-file path
                  (^p (%dbm-s2v self (read-chunk p))))
                seed)
          seed))))
  (next-method)
  (fold (lambda (c seed)
          (let1 p (build-path (ref self 'path) c)
            (if (file-exists? p)
              (fold apply-kv seed
                    (directory-list p :add-path? #t :children? #t))
              seed)))
        seed *hash-dirs*))

(define-method dbm-db-exists? ((class <fsdbm-meta>) name)
  (fsdbm-directory? name))

(define-method dbm-db-remove ((class <fsdbm-meta>) name)
  (unless (fsdbm-directory? name)
    (error "given path is not an fsdbm database:" name))
  (remove-directory* name))

(define-method dbm-db-copy ((class <fsdbm-meta>) from to)
  (unless (fsdbm-directory? from)
    (error "source path is not an fsdbm database:" from))
  (copy-directory* from to))

(define-method dbm-db-move ((class <fsdbm-meta>) from to)
  (unless (fsdbm-directory? from)
    (error "source path is not an fsdbm database:" from))
  ;; NB: move-directory*
  (copy-directory* from to)
  (remove-directory* from))

;;
;; Internal utilities
;;

(define (fsdbm-directory? path)
  (and (file-is-directory? path)
       (file-exists? (build-path path *version-file*))))

(define (fsdbm-create path mode)
  (when (file-exists? path) (remove-directory* path))
  (sys-mkdir path (dir-perm mode))
  (with-output-to-file (build-path path *version-file*)
    (lambda () (display *fsdbm-version*)))
  (sys-mkdir (build-path path *incoming-dir*) (dir-perm mode)))

;; read everything from the port and returns potentially incomplete string
(define (read-chunk port)
  (let loop ((elt (read-block 1024 port))
             (chunks '()))
    (if (eof-object? elt)
      (let ((s (string-concatenate-reverse chunks)))
        (or (string-incomplete->complete s) s))
      (loop (read-block 1024 port) (cons elt chunks)))))

(define (dir-perm file-perm)
  ;; trick: we copy 'r' bits to 'x' bits to make sure the 'readable'
  ;; database is also searchable.
  (logior file-perm (ash (logand file-perm #o444) -2)))

(define (key->path key)
  (with-string-io key
    (lambda ()
      (write-char #\_) ;; always emit one char, so we can deal with null key
      (let loop ((c (read-byte))
                 (count 0))
        (cond [(eof-object? c)]
              [(>= count *file-name-limit*)
               (write-char #\/) (loop c 0)]
              [(and (< c 127)
                    (char-set-contains? #[0-9a-zA-Z] (integer->char c)))
               (write-byte c) (loop (read-byte) (+ count 1))]
              [else
               (format #t "_~2,'0x" c)
               (loop (read-byte) (+ count 1))])))))

;; reverse fn of key->path.  returns #f is path is invalid.
(define (path->key path)
  (let/cc return
    (string-incomplete->complete
     (with-string-io path
       (lambda ()
         ;; skip the first char
         (unless (eqv? (read-char) #\_) (return #f))
         (let loop ((c (read-char)))
           (cond [(eof-object? c)]
                 [(char=? c #\/) (loop (read-char))]
                 [(char=? c #\_)
                  (let* ((c1 (read-char))
                         (c2 (read-char)))
                    (when (or (eof-object? c1) (eof-object? c2)) (return #f))
                    (unless (and (char-set-contains? #[[:xdigit:]] c1)
                                 (char-set-contains? #[[:xdigit:]] c2))
                      (return #f))
                    (write-byte (+ (* (digit->integer c1 16) 16)
                                   (digit->integer c2 16)))
                    (loop (read-char)))]
                 [else
                  (write-char c) (loop (read-char))])))))))

(define (path->hash path)
  ;; NB: we use our own hash fn to keep backward compatibility.
  (define mask (- (expt 2 32) 1))
  (define (shash hval mod)
    (let1 b (read-byte)
      (if (eof-object? b)
        (modulo hval mod)
        (shash (logand (+ hval (ash hval 3) b) mask) mod))))
  (string (string-ref *hash-chars*
                      (with-input-from-string path
                        (cut shash 0 *hash-range*)))))

(define (value-file-path key :optional (dir #f))
  (let1 p (key->path key)
    (if dir
	(build-path dir (path->hash p) p)
	(build-path (path->hash p) p))))


