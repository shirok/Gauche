;;;
;;; dbm - abstract base class for dbm interface
;;;
;;;   Copyright (c) 2000-2019  Shiro Kawai  <shiro@acm.org>
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

(define-module dbm
  (use gauche.collection)
  (use gauche.dictionary)
  (use gauche.generator)
  (export <dbm> <dbm-meta>
          dbm-open    dbm-close   dbm-closed? dbm-get
          dbm-put!    dbm-delete! dbm-exists?
          dbm-fold    dbm-for-each  dbm-map
          dbm-db-exists? dbm-db-remove dbm-db-copy dbm-db-move dbm-db-rename
          dbm-type->class)
  )
(select-module dbm)

(define-class <dbm-meta> (<class>)
  ())

(define-class <dbm> (<dictionary>)
  ((path       :init-keyword :path)
   (rw-mode    :init-keyword :rw-mode    :initform :write)
   (file-mode  :init-keyword :file-mode  :initform #o664)
   (key-convert   :init-keyword :key-convert :initform #f)
   (value-convert :init-keyword :value-convert :initform #f)
   ;; internal.  set up by dbm-open
   k2s s2k v2s s2v)
  :metaclass <dbm-meta>)

;; Macros & procedures that can be used by implementation modules
(define-syntax %dbm-k2s
  (syntax-rules ()
    ((_ self key) ((slot-ref self 'k2s) key))))

(define-syntax %dbm-s2k
  (syntax-rules ()
    ((_ self key) ((slot-ref self 's2k) key))))

(define-syntax %dbm-v2s
  (syntax-rules ()
    ((_ self key) ((slot-ref self 'v2s) key))))

(define-syntax %dbm-s2v
  (syntax-rules ()
    ((_ self key) ((slot-ref self 's2v) key))))

;; Utilities to copy/rename two files (esp. *.dir and *.pag file of
;; traditional dbm).  Makes some effort to take care of rollback on failure.
;; Also check if two files are hard-linked (gdbm_compat does that).

(autoload file.util file-eq? copy-file move-file)

(define (%dbm-copy2 from1 to1 from2 to2 :key (if-exists :error))
  (if (file-eq? from1 from2)
    (begin ;; dir and pag files are identical
      (copy-file from1 to1 :safe #t :if-exists if-exists)
      (sys-link to1 to2))
    (begin
      (copy-file from1 to1 :safe #t :if-exists if-exists)
      (guard (e [else (sys-unlink to1) (sys-unlink to2) (raise e)])
        (copy-file from2 to2 :safe #t :if-exists if-exists)))))

(define (%dbm-rename2 from1 to1 from2 to2 :key (if-exists :error))
  (if (file-eq? from1 from2)
    (begin
      (move-file from1 to1 :if-exists if-exists)
      (sys-link to1 to2)
      (sys-unlink from2))
    (begin
      (move-file from1 to1 :if-exists if-exists)
      (move-file from2 to2 :if-exists if-exists))))

;;
;; DBM-OPEN
;;

(define-method dbm-open ((class <dbm-meta>) . initargs)
  (dbm-open (apply make class initargs)))

(define-method dbm-open ((self <dbm>))
  (define (pick-proc slot default custom)
    (let1 spec (slot-ref self slot)
      (cond [(eq? spec #f) identity]
            [(eq? spec #t) default]
            [(and (pair? spec)
                  (null? (cddr spec))
                  (procedure? (car spec))
                  (procedure? (cadr spec)))
             (custom spec)]
            [else (errorf "bad value for ~s: has to be boolean or a list of two procedures, but got ~s" slot spec)])))

  (slot-set! self 'k2s (pick-proc 'key-convert write-to-string car))
  (slot-set! self 's2k (pick-proc 'key-convert read-from-string cadr))
  (slot-set! self 'v2s (pick-proc 'value-convert write-to-string car))
  (slot-set! self 's2v (pick-proc 'value-convert read-from-string cadr))
  self)

;;
;; Method prototypes.  Actual method should be defined in subclasses.
;;

(define-method dbm-put! ((dbm <dbm>) key value)
  (when (dbm-closed? dbm) (errorf "dbm-put!: dbm already closed: ~s" dbm))
  (when (eqv? (slot-ref dbm 'rw-mode) :read)
    (errorf "dbm-put!: dbm is read only: ~s" dbm)))

(define-method dbm-get ((dbm <dbm>) key . args)
  (when (dbm-closed? dbm) (errorf "dbm-get: dbm already closed: ~s" dbm)))

(define-method dbm-exists? ((dbm <dbm>) key)
  (when (dbm-closed? dbm) (errorf "dbm-exists?: dbm already closed: ~s" dbm)))

(define-method dbm-delete! ((dbm <dbm>) key)
  (when (dbm-closed? dbm) (errorf "dbm-delete!: dbm already closed: ~s" dbm))
  (when (eqv? (slot-ref dbm 'rw-mode) :read)
    (errorf "dbm-delete!: dbm is read only: ~s" dbm)))

(define-method dbm-fold ((dbm <dbm>) proc knil) #f)

(define-method dbm-close ((dbm <dbm>)) #f)

(define-method dbm-closed? ((dbm <dbm>)) #f)

;;
;; These work if dbm-fold is defined, but may be more efficient
;; if specialized.
;;

(define-method dbm-for-each ((dbm <dbm>) proc)
  (when (dbm-closed? dbm) (errorf "dbm-for-each: dbm already closed: ~s" dbm))
  (dbm-fold dbm (^[key value r] (proc key value)) #f))

(define-method dbm-map ((dbm <dbm>) proc)
  (when (dbm-closed? dbm) (errorf "dbm-map: dbm already closed: ~s" dbm))
  (reverse
   (dbm-fold dbm (^[key value r] (cons (proc key value) r)) '())))

;;
;; Collection framework
;;
(define-method call-with-iterator ((dbm <dbm>) proc . options)
  (let* ([g (generate
             (^[yield] (dbm-fold dbm (^[k v r] (yield (cons k v))) #f)))]
         [buf (g)])
    (proc (^[] (eof-object? buf))
          (^[] (begin0 buf (set! buf (g)))))))

(define-method coerce-to ((target <list-meta>) (dbm <dbm>))
  (dbm-map dbm cons))

;;
;; Dictionary framework
;;
(define-dict-interface <dbm>
  :get       dbm-get
  :put!      dbm-put!
  :exists?   dbm-exists?
  :delete!   dbm-delete!
  :fold      dbm-fold
  :map       dbm-map
  :for-each  dbm-for-each)

(define-method dict-comparator ((dbm <dbm>))
  (let1 k2s (~ dbm'k2s)
    (make-comparator #t
                     (^[a b] (equal? (k2s a) (k2s b)))
                     #f #f)))

;;
;; Meta-operations
;;  Subclass has to implement at least dbm-db-exists? and dbm-db-remove.
;;

(define-method dbm-db-exists? ((class <dbm-meta>) name)
  (errorf "dbm-db-exists?: not supported in ~a" class))

(define-method dbm-db-remove ((class <dbm-meta>) name)
  (errorf "dbm-db-remove: not supported in ~a" class))

(define-method dbm-db-copy ((class <dbm-meta>) from to)
  ;; generic one - might be slow, and it may not copy meta info.
  ;; it also doesn't check if from and to is the same databases;
  ;; but it opens from-db first with read mode, so if the implementation
  ;; has sane locking, the to-db opening with create option would fail.
  ;; (That's why we're using let* here.)
  (let* ([from-db (dbm-open class :path from :rw-mode :read)]
         [to-db   (dbm-open class :path to   :rw-mode :create)])
    (dbm-for-each from-db (^[k v] (dbm-put! to-db k v)))
    (dbm-close to-db)
    (dbm-close from-db)))

(define-method dbm-db-move ((class <dbm-meta>) from to)
  ;; generic one - see above.
  (let* ([from-db (dbm-open class :path from :rw-mode :read)]
         [to-db   (dbm-open class :path to   :rw-mode :create)])
    (dbm-for-each from-db (^[k v] (dbm-put! to-db k v)))
    (dbm-close to-db)
    (dbm-close from-db)
    (dbm-db-remove class from)))

(define dbm-db-rename dbm-db-move) ; backward compatibility

;; Try to dynamically load named dbm module and returns the class.
;; DBMTYPE must be a symbol like 'gdbm'.
;; Returns #f if it couldn't retrieve the named dbm module.

(define (dbm-type->class dbmtype)
  (let ([module-name (string->symbol #"dbm.~dbmtype")]
        [class-name (string->symbol #"<~|dbmtype|>")])
    (and (library-exists? module-name :strict? #t)
         (guard (e [else #f])
           ((with-module gauche.internal %require)
            (module-name->path module-name))
           (global-variable-ref (find-module module-name) class-name)))))
