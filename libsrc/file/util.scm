;;;
;;; file/util.scm - filesystem utility functions
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

;;; This module provides convenient utility functions to handle
;;; files and directories.   Some functions are provided with
;;; different names for compatibility with existing Scheme
;;; implementations.

#!no-fold-case

(define-module file.util
  (use srfi-1)
  (use srfi-13)
  (use util.match)
  (use gauche.parameter)
  (export current-directory directory-list directory-list2 directory-fold
          home-directory temporary-directory
          make-directory* create-directory* remove-directory* delete-directory*
          copy-directory*
          call-with-temporary-file call-with-temporary-directory
          create-directory-tree check-directory-tree
          build-path resolve-path expand-path simplify-path decompose-path
          absolute-path? relative-path? find-file-in-paths
          path-separator
          path-extension path-sans-extension path-swap-extension
          file-is-readable? file-is-writable? file-is-executable?
          file-is-symlink?
          file-type file-perm file-mode file-ino file-dev file-rdev file-nlink
          file-uid file-gid file-size file-mtime file-atime file-ctime
          file-eq? file-eqv? file-equal? file-device=?
          file-mtime=? file-mtime<? file-mtime<=? file-mtime>? file-mtime>=?
          file-atime=? file-atime<? file-atime<=? file-atime>? file-atime>=?
          file-ctime=? file-ctime<? file-ctime<=? file-ctime>? file-ctime>=?
          touch-file touch-files copy-file move-file
          ;copy-files move-files
          remove-file delete-file
          remove-files delete-files
          null-device console-device
          file->string file->string-list file->list file->sexp-list
          string->file string-list->file list->file sexp-list->file
          <lock-file-failure> with-lock-file
          ))
(select-module file.util)

;; Common util.  Returns #f if PATH does not exist.

(define (safe-stat path follow-link?)
  (if (cond-expand [gauche.os.windows #t]
                   [else follow-link?])
    (and (sys-access path F_OK) (sys-stat path))
    ;; When we don't follow symlink, we can't use sys-access since it returns
    ;; #f if PATH is a dangling symlink.
    (guard (e [(and (<system-error> e) (eq? (ref e 'errno) ENOENT)) #f])
      (sys-lstat path))))

(define (%stat path follow-link?)
  ((if follow-link? sys-stat sys-lstat) path))

;;;=============================================================
;;; Directory entries

;; "current-directory" is found in ChezScheme, MzScheme, etc.
(define (current-directory :optional (newdir #f))
  (match newdir
    [#f (sys-getcwd)]
    [(? string?) (sys-chdir newdir)]
    [_ (error "directory name should be a string" newdir)]))

(define (home-directory :optional (user (sys-getuid)))
  (cond-expand
   [gauche.os.windows
    (unless (eqv? user (sys-getuid))
      (error "On windows native platforms, getting other user's home directory \
              is not supported (yet)."))
    ;; See src/system.c for the way to determine the home directory.
    (sys-normalize-pathname "~" :expand #t)]
   [else
    (and-let* ([ent (cond [(integer? user) (sys-getpwuid user)]
                          [(string? user)  (sys-getpwnam user)]
                          [else (error "bad user" user)])])
      (slot-ref ent 'dir))]))

(define temporary-directory
  (make-parameter (sys-tmpdir)))

(define (call-with-temporary-file proc
                                  :key (directory (temporary-directory))
                                       (prefix "gtemp"))
  (receive (oport name) (sys-mkstemp (build-path directory prefix))  
    (unwind-protect (proc oport name)
      (close-output-port oport)
      (sys-unlink name))))

(define (call-with-temporary-directory proc 
                                       :key (directory (temporary-directory))
                                            (prefix "gtemp"))
  (let1 dir (sys-mkdtemp (build-path directory prefix))
    (unwind-protect (proc dir)
      (remove-directory* dir))))

(define (null-device)
  (cond-expand
   [gauche.os.windows "NUL"]
   [else "/dev/null"]))

(define (console-device)
  (cond-expand
   [gauche.os.windows "CON"]
   [else "/dev/tty"]))

;; utility for directory-list and directory-list2
(define (%directory-filter dir pred filter-add-path?)
  (if filter-add-path?
    (filter (^e (pred (build-path dir e))) (sys-readdir dir))
    (filter pred (sys-readdir dir))))

(define (%directory-filter-compose opts)
  (let-keywords opts ((children? #f)
                      (filter #f))
    (apply every-pred
           (cond-list
            [children? (^e (not (member (sys-basename e) '("." ".."))))]
            [filter]))))

;; directory-list DIR &keyword ADD-PATH? FILTER-ADD-PATH? CHILDREN? FILTER
(define (directory-list dir :key (add-path? #f)
                                 (filter-add-path? #f)
                            :allow-other-keys other-keys)
  (let1 entries
      (sort (%directory-filter dir (%directory-filter-compose other-keys)
                               filter-add-path?))
    (if add-path?
      (map (cut build-path dir <>) entries)
      entries)))

;; directory-list2 DIR &optional ADD-DIR? FILTER-ADD-PATH? CHILDREN? FILTER FOLLOW-LINK?
(define (directory-list2 dir :key (add-path? #f)
                                  (filter-add-path? #f)
                                  (follow-link? #t)
                             :allow-other-keys other-keys)
  (let* ([filters (%directory-filter-compose other-keys)]
         [selector (lambda (e)
                     (and-let* ([s (safe-stat e follow-link?)])
                       (eq? (slot-ref s 'type) 'directory)))]
         [entries (sort (%directory-filter dir filters filter-add-path?))])
    (if add-path?
      (partition selector (map (cut build-path dir <>) entries))
      (partition (^e (selector (build-path dir e))) entries))))

;; directory-fold DIR PROC KNIL &keyword LISTER FOLDER FOLLOW-LINK?
(define (directory-fold dir proc knil
                        :key (lister (lambda (path knil)
                                       (values (directory-list path
                                                               :add-path? #t
                                                               :children? #t)
                                               knil)))
                             (folder fold)
                             (follow-link? #t))
  (define (selector e)
    (and (file-exists? e)
         (eq? (slot-ref (%stat e follow-link?) 'type) 'directory)))
  (define (rec path knil)
    (if (selector path)
      ;; [TODO]: For the backward compatibiliy, we allow LISTER to return
      ;; only a single value.  Should be removed, probably in 0.9.
      (receive res (lister path knil)
        (folder rec (get-optional (cdr res) knil) (car res)))
      (proc path knil)))
  (rec dir knil))

;; mkdir -p
(define (make-directory* dir :optional (mode #o755))
  (define (rec p)
    (if (file-exists? p)
      (unless (file-is-directory? p)
        (errorf "non-directory ~s is found while creating a directory ~s"
                (sys-basename p) dir))
      (let1 d (sys-dirname p)
        (rec d)
        (unless (file-is-writable? d)
          (errorf "directory ~s unwritable during creating a directory ~s"
                  d dir))
        (unless (equal? (sys-basename p) ".") ; omit the last component in "/a/b/c/."
          (sys-mkdir p mode))
        )))
  ;; some platform complains the last "/"
  (rec (string-trim-right dir #[/])))

;; synonym
(define create-directory* make-directory*)

;; rm -rf
(define (remove-directory* dir)
  (define (rec d)
    (receive (dirs files)
        (directory-list2 d :add-path? #t :children? #t :follow-link? #f)
      (for-each rec dirs)
      (for-each sys-rmdir dirs)
      (for-each sys-unlink files)))
  (rec dir)
  (sys-rmdir dir))

(define delete-directory* remove-directory*)

;; cp -r
;;   if-exists  - :error :supersede :backup :overwrite #f
;;   backup-suffix
;;   safe
;;   follow-link? - follow symlink.  the default is #f
;;   keep-timestamp
;;   keep-mode
(define (copy-directory* src dst :key (if-exists :error)
                                      ((:backup-suffix backsfx) ".orig")
                                      (safe #f)
                                      (follow-link? #f)
                                      ((:keep-timestamp keeptime) #f)
                                      ((:keep-mode keepmode) #f))

  (define (postprocess src dst)
    (let1 stat (sys-stat src)
      (when keeptime (sys-utime dst (ref stat'atime) (ref stat'mtime)))
      (when keepmode (sys-chmod dst (ref stat'perm)))))

  (define (do-copy src dst)
    (copy-file src dst :if-exists if-exists :backup-suffix backsfx
               :follow-link? follow-link?
               :keep-timestamp keeptime :keep-mode keepmode))

  (define (rec src dst)
    (cond [(file-is-directory? src)
           (let1 exists? (file-exists? dst)
             (if (not exists?)
               (sys-mkdir dst #o777)
               (case if-exists
                 [(:error) (error "destination file exists" dst)]
                 [(:supersede)
                  (remove-files dst)
                  (sys-mkdir dst #o777)]
                 [(:overwrite)
                  (unless (file-is-directory? dst)
                    (remove-files dst))
                  ;; TODO: if dst directory exists, check the permission
                  ]
                 ;; NB: :backup is handled by toplevel
                 [else (error "unknown if-exists value:" if-exists)]))
             (dolist [s (directory-list src :add-path? #t :children? #t)]
               (rec s (build-path dst (sys-basename s))))
             (unless exists? (postprocess src dst))
             )]
          [else (do-copy src dst)]))

  (cond [(file-is-symlink? src) ; we check this first, for src may be a dangling symlink.
         (do-copy src dst)]
        [(not (file-exists? src))
         (error "source file does not exist:" src)]
        [(file-exists? dst)
         (case if-exists
           [(:backup)
            (sys-rename dst (string-append dst backsfx))
            (rec src dst)]
           [(#f) #f]
           [else  ; other if-exists options are handled later
            (rec src dst)])]
        [else (rec src dst)])
  )

;; Create/check certain directory structure at once
;;
;; Directory Tree
;;   <tree> : <name>                      ; empty file
;;          | (<name> [options])          ; empty file
;;          | (<name> [options] <string>) ; file with content
;;          | (<name> [options] <proc>)   ;   using output of <proc>
;;          | (<name> [options] (<tree> ...)) ; directory
;;   <name> : string
;;
;;   options is alternating list of keywords and values.
;;
;;     :mode <integer>   ; file/dir mode bits
;;     :owner <id>       ; file/dir owner
;;     :group <id>       ; file/dir group
;;     :symlink <path>   ; file is actually a symlink to <path>

(define-values (create-directory-tree
                check-directory-tree)
  (let ()
    (define (name? x) (or (string? x) (symbol? x)))
    (define (collect-options args)
      (let loop ([args args] [r '()])
        (match args
          [() (values (reverse r) #f)]
          [([? keyword? k] val . rest) (loop rest (list* val k r))]
          [(arg) (values (reverse r) arg)]
          [_ (error "invalid option list:" args)])))
    (define (mkpath dir name) (build-path dir (x->string name)))
    (define chown (cond-expand
                   [gauche.sys.lchown sys-lchown]
                   [else sys-chown]))
    (define (walk dir node do-file do-dir)
      (match node
        [[? name?] (do-file (mkpath dir node) #f)]
        [([? name? n] . args)
         (receive (opts content) (collect-options args)
           (if (list? content)
             (apply do-dir (mkpath dir n) content opts)
             (apply do-file (mkpath dir n) content opts)))]
        [_ (error "invalid tree node:" node)]))

    (define (ensure-file path content
                         :key (mode #f) (owner -1) (group -1) (symlink #f))
      (if symlink
        (sys-symlink symlink path)
        (cond
         [(not content) (touch-file path)]
         [(string? content) (with-output-to-file path (cut display content))]
         [else (with-output-to-file path (cut content path))]))
      ;; NB: BSD systems has lchmod, so we may support :mode with :symlink
      ;; in future.
      (when (and mode (not symlink)) (sys-chmod path mode))
      (when (or (>= owner 0) (>= group 0)) (chown path owner group)))
    (define (ensure-dir path children :key (mode #o755) (owner -1) (group -1))
      (make-directory* path mode)
      (for-each (cut ensure path <>) children)
      (when (or (>= owner 0) (>= group 0)) (sys-chown path owner group)))
    (define (ensure dir node) (walk dir node ensure-file ensure-dir))
    (define (create-directory-tree start tree) (ensure start tree))

    (define (check-file path content
                        :key (mode #f) (owner -1) (group -1) (symlink #f))
      (if symlink
        (cond-expand
         [gauche.sys.symlink
          (and (file-is-symlink? path)
               (check-attrs path mode owner group)
               (equal? symlink (sys-readlink path)))]
         [else #f])
        (and (file-is-regular? path)
             (check-attrs path mode owner group)
             (cond
              [(not content) #t]
              [(string? content) (string=? content (file->string path))]
              [else (equal? (with-output-to-string (cut content path))
                            (file->string path))]))))
    (define (check-dir path children :key (mode #f) (owner -1) (group -1))
      (and (file-is-directory? path)
           (check-attrs path mode owner group)
           (every (cut check path <>) children)))
    (define (check-attrs path mode owner group)
      (let1 s (sys-lstat path)
        (and (or (not mode) (= (~ s'perm) mode))
             (or (negative? owner) (= (~ s'uid) owner))
             (or (negative? group) (= (~ s'gid) group)))))
    (define (check dir node) (walk dir node check-file check-dir))
    (define (check-directory-tree start tree) (check start tree))

    (values create-directory-tree check-directory-tree)))

;;;=============================================================
;;; Pathnames

(define (build-path base-path . components)
  (define path-separator-string (string (path-separator)))
  (define (rec base components)
    (if (null? components)
      base
      (let1 component
          (match (car components)
            ['up   ".."]
            ['same "."]
            [(? absolute-path? p)
             (error "can't append absolute path after other path" p)]
            [p (sys-normalize-pathname p)])
        (rec (cond [(string-null? base) component]
                   [(string-null? component) base]
                   [(#/[\/\\]$/ base) (string-append base component)]
                   [else
                    (string-append base path-separator-string component)])
          (cdr components)))))
  (rec (sys-normalize-pathname base-path) components))

(define (expand-path path)
  (sys-normalize-pathname path :expand #t))

(define (resolve-path path)
  (define (pathcat dir base) (simplify-path (build-path dir base)))
  (define (rec pat)
    (let ([dir  (sys-dirname pat)]
          [base (sys-basename pat)])
      (receive (ndir p) (if (or (string=? dir "/")  (string=? dir "."))
                          (values dir pat)
                          (let1 nd (rec dir) (values nd (pathcat nd base))))
        (unless (sys-access p F_OK)
          (error "path doesn't exist" path))
        (let loop ((count 0) (p p))
          (cond [(>= count 8) ;; arbitrary upper bound to detect infinite loop
                 (error "possibly looping symlink" pat)]
                [(eq? (file-type p :follow-link? #f) 'symlink)
                 (cond-expand
                  [gauche.sys.symlink
                   (loop (+ count 1)
                         (let1 np (sys-readlink p)
                           (if (absolute-path? np) np (pathcat ndir np))))]
                  [else p])]
                [else p])))))
  (rec (expand-path path)))

(define (simplify-path path)
  (sys-normalize-pathname path :canonicalize #t))

(define (decompose-path path)
  (if (#/[\/\\]$/ path)
    (values (string-trim-right path #[\\/]) #f #f)
    (let ([dir (sys-dirname path)]
          [base (sys-basename path)])
      (cond [(string-index-right base #\.)
             => (lambda (pos)
                  (if (zero? pos)
                    (values dir base #f)  ; '.' at the beginning doesn't delimit extension
                    (values dir
                            (string-take base pos)
                            (string-drop base (+ pos 1)))))]
            [else (values dir base #f)]))))

(define (relative-path? path)
  (cond-expand
   [gauche.os.windows (not (#/^[\/\\]|^[A-Za-z]:/ path))]
   [else              (not (#/^\// path))]))

(define (absolute-path? path)
  (not (relative-path? path)))

(define (path-separator)
  (cond-expand
   [gauche.os.windows #\\]
   [else #\/]))

(define (path-extension path)
  (receive (dir file ext) (decompose-path path) ext))

(define (path-sans-extension path)
  (cond [(path-extension path)
         => (lambda (ext)
              (substring path 0
                         (- (string-length path) (string-length ext) 1)))]
        [else path]))

(define (path-swap-extension path ext)
  (if ext
    (string-append (path-sans-extension path) "." ext)
    (path-sans-extension path)))

(define (find-file-in-paths name
                            :key (paths (cond [(sys-getenv "PATH")
                                               => (cut string-split <>
                                                       (cond-expand
                                                        [gauche.os.windows #\;]
                                                        [else #\:]))]
                                              [else '()]))
                                 (pred file-is-executable?)
                                 (extensions '()))
  (define names
    (if (null? extensions)
      `(,name)
      (cons name
            (map (^e (string-append name "." e)) extensions))))
  (define (try n) (and (pred n) n))
  (if (absolute-path? name)
    (any try names)
    (let loop ((paths paths))
      (and (not (null? paths))
           (or (any try (map (cute build-path (car paths) <>) names))
               (loop (cdr paths)))))))

;;;=============================================================
;;; File attributes

;; convenient accessors for file stats.  accepts string file name or
;; <sys-stat>.  If the named file doesn't exist, returns #f.
;; accepts keyword argument FOLLOW-LINK?
(define-syntax define-stat-accessor
  (syntax-rules ()
    [(_ name slot)
     (define (name path :key (follow-link? #t))
       (and-let* ([s (safe-stat path follow-link?)])
         (slot-ref s slot)))]))

(define-stat-accessor file-type 'type)
(define-stat-accessor file-perm 'perm)
(define-stat-accessor file-mode 'mode)
(define-stat-accessor file-ino  'ino)
(define-stat-accessor file-dev  'dev)
(define-stat-accessor file-rdev 'rdev)
(define-stat-accessor file-nlink 'nlink)
(define-stat-accessor file-uid   'uid)
(define-stat-accessor file-gid   'gid)
(define-stat-accessor file-size 'size)
(define-stat-accessor file-atime 'atime)
(define-stat-accessor file-mtime 'mtime)
(define-stat-accessor file-ctime 'ctime)

;; file permissions
(define (file-is-readable? path) (sys-access path R_OK))
(define (file-is-writable? path) (sys-access path W_OK))
(define (file-is-executable? path) (sys-access path X_OK))

(define (file-is-symlink? path)
  (and-let* ([s (safe-stat path #f)])
    (eq? (slot-ref s 'type) 'symlink)))

;; compares two files are identical, in the sense that:
;;  file-eq?  - two files (or directories) are the same entity. symbolic
;;              links are not followed.   The files must exist.
;;  file-eqv? - two files are the same entity, after resolving symbolic
;;              links.  The files must exist.
;;  file-equal? - the content of two files are the same.

;; NB: on Windows/MinGW, we cannot obtain inode number correctly.
;; We compare canonicalized pathnames (this doesn't work if there's
;; an alias.  We should call Windows API to check for sure in future).

(cond-expand
 [gauche.os.windows
  (define (%stat-compare s1 s2 f1 f2)
    (let ([p1 (sys-normalize-pathname f1 :absolute #t :canonicalize #t)]
          [p2 (sys-normalize-pathname f2 :absolute #t :canonicalize #t)])
      (equal? p1 p2)))
  (define (file-eq? f1 f2)  (%stat-compare #f #f f1 f2))
  (define (file-eqv? f1 f2) (%stat-compare #f #f f1 f2))
  ]
 [else
  (define (%stat-compare s1 s2 f1 f2)
    (and (eqv? (slot-ref s1 'dev) (slot-ref s2 'dev))
         (eqv? (slot-ref s1 'ino) (slot-ref s2 'ino))))
  (define (file-eq? f1 f2)  (%stat-compare (sys-lstat f1) (sys-lstat f2) f1 f2))
  (define (file-eqv? f1 f2) (%stat-compare (sys-stat f1) (sys-stat f2) f1 f2))
  ])

(define (file-equal? f1 f2)
  (let ([s1 (sys-stat f1)]
        [s2 (sys-stat f2)])
    (cond [(%stat-compare s1 s2 f1 f2)]
          [(not (eq? (slot-ref s1 'type) (slot-ref s2 'type))) #f]
          [(not (= (slot-ref s1 'size) (slot-ref s2 'size)))   #f]
          [(eq? (slot-ref s1 'type) 'directory)
           (error "directory comparison is not supported yet" s1)]
          [else
           (call-with-input-file f1
             (lambda (p1)
               (call-with-input-file f2
                 (lambda (p2)
                   (let loop ([b1 (read-block 8192 p1)]
                              [b2 (read-block 8192 p2)])
                     (cond [(eof-object? b1) #t]
                           [(string=? b1 b2)
                            (loop (read-block 8192 p1) (read-block 8192 p2))]
                           [else #f]))))))]
          )))

;; see if two files or directories exist on the same device.
(define (file-device=? f1 f2)
  (eqv? (slot-ref (sys-stat f1) 'dev) (slot-ref (sys-stat f2) 'dev)))

;; comparing file timestamp.  accepts string file name, <sys-stat>,
;; <time>, or number.
(define-syntax define-time-comparer
  (syntax-rules ()
    [(_ name slot cmp)
     (begin
       (define-method name ((a <sys-stat>) (b <sys-stat>))
         (cmp (slot-ref a slot) (slot-ref b slot)))
       (define-method name ((a <sys-stat>) (b <number>))
         (cmp (slot-ref a slot) b))
       (define-method name ((a <number>) (b <sys-stat>))
         (cmp a (slot-ref b slot)))
       (define-method name ((a <string>) (b <string>))
         (name (sys-stat a) (sys-stat b)))
       (define-method name ((a <string>) b)
         (name (sys-stat a) b))
       (define-method name (a (b <string>))
         (name a (sys-stat b)))
       (define-method name ((a <time>) b)
         (name (slot-ref a 'second) b))
       (define-method name (a  (b <time>))
         (name a (slot-ref b 'second)))
       )]))

(define-time-comparer file-mtime=?  'mtime =)
(define-time-comparer file-mtime<?  'mtime <)
(define-time-comparer file-mtime<=? 'mtime <=)
(define-time-comparer file-mtime>?  'mtime >)
(define-time-comparer file-mtime>=? 'mtime >=)

(define-time-comparer file-ctime=?  'ctime =)
(define-time-comparer file-ctime<?  'ctime <)
(define-time-comparer file-ctime<=? 'ctime <=)
(define-time-comparer file-ctime>?  'ctime >)
(define-time-comparer file-ctime>=? 'ctime >=)

(define-time-comparer file-atime=?  'atime =)
(define-time-comparer file-atime<?  'atime <)
(define-time-comparer file-atime<=? 'atime <=)
(define-time-comparer file-atime>?  'atime >)
(define-time-comparer file-atime>=? 'atime >=)

;;;=============================================================
;;; File operation
;;;

(define (touch-file pathname :key (time #f) (type #f) (create #t))
  (unless (or (not time) (and (real? time) (not (negative? time))))
    (error "bad value for time: #f or nonnegative real is expected, got:" time))
  (cond
   [(sys-access pathname F_OK)
    (case type
      [(#f) (sys-utime pathname time time)]
      [(mtime) (let1 s (sys-stat pathname)
                 (sys-utime pathname (~ s'atime) time))]
      [(atime) (let1 s (sys-stat pathname)
                 (sys-utime pathname time (~ s'mtime)))]
      [else (error "bad value for type: #f, 'atime, or 'mtime is expected, got:"
                   type)])]
   [create
    (close-output-port (open-output-file pathname))
    (when time (touch-file pathname :time time :type type))])
  (values))

(define (touch-files pathnames :key (time (undefined))
                                    (type (undefined))
                                    (create (undefined)))
  (for-each (cut touch-file <> :time time :type type :create create) pathnames))

;; copy-file
;;  if-exists     - :error :supersede :backup :append #f
;;  follow-link?  - follow symlinks (default #t)
;;  backup-suffix
;;  safe
;;  keep-timestamp
;;  keep-mode
(define (copy-file src dst :key (if-exists :error)
                                ((:backup-suffix backsfx) ".orig")
                                (follow-link? #t)
                                (safe #f)
                                ((:keep-timestamp keeptime) #f)
                                ((:keep-mode keepmode) #f))
  (let ([backfile  (string-append dst backsfx)]
        [times     '()]
        [tmpfile   #f]
        [inport    #f]
        [outport   #f]
        [dst-exists #f]
        [default-perm (logand #o666 (lognot (sys-umask)))])
    (define (rollback)
      (cond [inport  => close-input-port])
      (cond [outport => close-output-port])
      (cond [tmpfile => sys-unlink]))
    (define (commit)
      (cond [inport  => close-input-port])
      (cond [outport => close-output-port])
      (cond [tmpfile
             (safe-chmod tmpfile (if keepmode (file-perm src) default-perm))
             (when (eq? if-exists :backup) (sys-rename dst backfile))
             (sys-rename tmpfile dst)]
            [keepmode         (safe-chmod dst (file-perm src))]
            [(not dst-exists) (safe-chmod dst default-perm)])
      (unless (null? times) (apply sys-utime dst times)))
    (define (safe-chmod path mode)
      (and (not (file-is-symlink? path)) (sys-chmod path mode)))
    (define (open-destination)
      (if safe
        (cond [(and (eq? if-exists :error) (file-exists? dst))
               (error "destination file exists" dst)]
              [(and (not if-exists) (file-exists? dst)) #f]
              [(and (eq? if-exists :append) (file-exists? dst))
               (set!-values (outport tmpfile) (sys-mkstemp dst))
               (call-with-input-file dst (cut copy-port <> outport))
               #t]
              [else
               (set!-values (outport tmpfile) (sys-mkstemp dst)) #t])
        (ecase if-exists
          [(#f) (set! outport (open-output-file dst :if-exists #f)) outport]
          [(:backup)
           (when (file-exists? dst) (sys-rename dst backfile))
           (set! outport (open-output-file dst)) #t]
          [(:supersede :append :error)
           (set! outport (open-output-file dst :if-exists if-exists)) #t])))
    (define (get-times stat)
      (map (cut slot-ref stat <>) '(atime mtime)))
    (define (do-symlink)
      (define (doit)
        (cond-expand
         [gauche.sys.symlink (sys-symlink (sys-readlink src) dst)]
         [else #f]));NB: if system doesn't support symlink, we can't be here.
      (when keeptime (set! times (get-times (sys-lstat src))))
      (if (file-exists? dst)
        (case if-exists
          [(:error :append) (error <system-error> :errno EEXIST
                                   "destination file exists" dst)]
          [(#f) #f]
          [(:backup) (sys-rename dst backfile) (doit)]
          [else      (sys-unlink dst) (doit)])
        (doit))
      (commit))
    (define (do-copy)
      (guard (e (else (rollback) (raise e)))
        (set! inport (open-input-file src))
        (when keeptime (set! times (get-times (sys-fstat inport))))
        (set! dst-exists (file-exists? dst))
        (begin0
            (and (open-destination)
                 (copy-port inport outport :unit 65536)
                 #t)
          (commit))))

    ;; body of copy-file
    (unless (memq if-exists '(#f :error :supersede :backup :append))
      (error "argument for :if-exists must be either :error, :supersede, :backup, :append or #f, but got" if-exists))
    (when (and (file-exists? src) (file-exists? dst)
               ((if follow-link? file-eqv? file-eq?) src dst))
      (errorf "source ~s and destination ~s are the same file" src dst))
    (if (and (not follow-link?) (file-is-symlink? src))
      (do-symlink)
      (do-copy))
    ))

;; move-file
;;  if-exists  - :error :supersede :backup #f
;;  backup-suffix
(define (move-file src dst :key (if-exists :error)
                                ((:backup-suffix backsfx) ".orig"))

  (define (do-rename)
    (if (file-exists? dst)
      (cond [(eq? if-exists :error) (error "destination file exists" dst)]
            [(eq? if-exists :supersede) (sys-rename src dst) #t]
            [(eq? if-exists :backup)
             (sys-rename dst (string-append dst backsfx))
             (sys-rename src dst) #t]
            [else #f])
      (begin (sys-rename src dst) #t)))
  (define (do-copying)
    (and (copy-file src dst :if-exists if-exists
                    :backup-suffix backsfx
                    :safe #t :keep-timestamp #t :keep-mode #t)
         (begin (sys-unlink src) #t)))

  ;; body of move-file
  (unless (memq if-exists '(#f :error :supersede :backup))
    (error "argument for :if-exists must be either :error, :supersede, :backup or #f, but got" if-exists))
  (unless (file-exists? src)
    (error "source file does not exist" src))
  (when (and (file-exists? dst) (file-eqv? src dst))
    (errorf "source ~s and destination ~s are the same file" src dst))
  (let1 dstdir (sys-dirname dst)
    (unless (file-exists? dstdir)
      (errorf "can't move to ~s: path does not exist" dst))
    (if (file-device=? src dstdir)
      (do-rename)
      (do-copying)))
  )

;; copy-files & move files
;; NB: need to think more about argument order.  Should it be
;;     (copy-files file1 file2 ... destdir)  ;; cp(1) style
;;   or
;;     (copy-files destdir file1 file2 ...)
;; And how shall we specify keyword args to pass to copy-file?

; (define (%multifile-cpmv op)
;   (lambda (files dst . opts)
;     (unless (file-exists? dst)
;       (error "destination does not exist" dst))
;     (unless (file-is-directory? dst)
;       (error "destination is not a directory" dst))
;     (dolist (f files)
;       (apply op f (build-path dst (sys-basename f)) opts))))

; (define copy-files (%multifile-cpmv copy-file))
; (define move-files (%multifile-cpmv move-file))

;; removing.
;; Unlike sys-unlink, we throw an error if FILE doesn't exist,
;; to align with R7RS.  (Technically we should throw a file-error
;; object.  We'll fix that later.)
(define (remove-file file)
  (or (sys-unlink file)
      (error <system-error> :errno ENOENT "File does not exist:" file)))
(define delete-file remove-file)

(define (remove-files . paths)
  (dolist (p paths)
    (cond [(list? p) (apply remove-files p)] ; for the convenience
          [(file-is-directory? p) (remove-directory* p)]
          [else (sys-unlink p)])))

(define delete-files remove-files)

;; file->string, file->list, file->string-list, file->sexp-list
;; shortcuts of port->string etc.
;; NB: call-with-input-file may pass #f to the proc if :if-does-not-exist #f
;; is given.  We return #f in such case.

(define (%maybe proc)
  (lambda (maybe-port) (and maybe-port (proc maybe-port))))

(define (file->string file . opts)
  (apply call-with-input-file file (%maybe port->string) opts))

(define (file->list reader file . opts)
  (apply call-with-input-file file (%maybe (cut port->list reader <>)) opts))

(define (file->string-list file . opts)
  (apply call-with-input-file file (%maybe (cut port->list read-line <>)) opts))

(define (file->sexp-list file . opts)
  (apply call-with-input-file file (%maybe (cut port->list read <>)) opts))

;; inverse of file->*
(define (string->file file str . opts)
  (apply call-with-output-file file (cut display str <>) opts))

(define (list->file writer file lis . opts)
  (apply call-with-output-file file (^p (dolist [s lis] (writer s p))) opts))

(define string-list->file
  (cute list->file (^[s p] (display s p) (newline p)) <> <> <...>))

(define sexp-list->file
  (cute list->file (^[s p] (write s p) (newline p)) <> <> <...>))

;;;=============================================================
;;; File locking
;;;

;; fcntl lock is unreliable in MT environment (It's process-wide lock,
;; so we need mutex to avoid inter-thread races.  However, it is not
;; trivial to provide a mutex per arbitrary file to be locked.)
;; So we just provide file/directory based locks.
;;
;; The drawback is that if a process holding the lock dies, the lock file
;; may remain, blocking all other processes.  We support 'stealing' the
;; lock: If the existing lock file is too old to be possibly held by the
;; application, the attempted locker can grab the lock.
;;
;; Stealing makes things a bit complicated, since there can be a race
;; if more than two processes tries to steal the lock at the same time.
;; We use a secondary lock file to avoid the race.

;; TODO: If we find the lock is held for a certain time,
;; we want to check if the process is actually alive---using (sys-kill pid 0)
;; or something.   If there's no process, we can steal the lock right away,
;; not waiting for abandon-timeout.   This doesn't work for the lock for
;; a resource that can be shared between multiple machines, so the feature
;; should be optional.

(define-condition-type <lock-file-failure> <error> #f
  (lock-file-name #f))

(define (with-lock-file lock-name proc
                        :key (type 'file)
                             (retry-interval 1)
                             (retry-limit 10)
                             (abandon-timeout 600)
                             (secondary-lock-name #"~|lock-name|.2")
                             (retry2-interval 1)
                             (retry2-limit 10)
                             (perms (if (eq? type 'directory)
                                      #o755
                                      #o644)))
  ;; returns #t on success, #f of failure
  (define (acquire path)
    (case type
      [(file)
       (with-output-to-file path
         (^() (print (sys-getpid)) (sys-chmod path perms) #t)
         :if-exists #f)]
      [(directory)
       (guard (e [(and (<system-error> e) (eqv? (~ e'errno) EEXIST)) #f])
         (sys-mkdir path perms) #t)]))

  ;; the simplest way is (remove-files path); but it will eradicate anything
  ;; under PATH, which is a bit too radical.
  (define (release path)
    (guard (e [(<system-error> e)
               (unless (eq? (~ e'errno) ENOENT)
                 (errorf <lock-file-failure> :lock-file-name lock-name
                         "couldn't remove lock file ~s: ~a"
                         lock-name (sys-strerror (~ e'errno))))])
      (case type
        [(file) (sys-unlink path)]
        [(directory) (sys-rmdir path)])))

  ;; Common retry logic
  ;;  interval and timeout should be in nanosecs
  (define (try acquirer releaser interval timeout check success failure)
    (let loop ([elapsed 0])
      (cond [(acquirer) (unwind-protect (success) (releaser))]
            [(< elapsed timeout)
             (let wait ([w interval])
               (if-let1 w1 (sys-nanosleep w)
                 (wait w1)))
             (check)
             (loop (+ interval elapsed))]
            [else (failure)])))

  ;; scale seconds to nanoseconds
  (define (nsec secs) (* secs 1e9))

  (define (primary-lock)
    (try (cut acquire secondary-lock-name)
         (cut release secondary-lock-name)
         (nsec retry2-interval) (nsec retry2-limit)
         values
         (cut acquire lock-name)
         secondary-lock-failure))

  (define (primary-unlock)
    (try (cut acquire secondary-lock-name)
         (cut release secondary-lock-name)
         (nsec retry2-interval) (nsec retry2-limit)
         values
         (cut release lock-name)
         secondary-lock-failure))

  (define (secondary-lock-failure)
    (errorf <lock-file-failure> :lock-file-name lock-name
            "with-lock-file: secondary lock cannot be acquired (~a).  \
             If no process is locking it, remove it and try again."
            secondary-lock-name))

  (define (primary-lock-failure)
    (errorf <lock-file-failure> :lock-file-name lock-name
            "with-lock-file: couldn't acquire lock file (~a).  \
             If no process is locking it, remove the file and try again."
            lock-name))

  (define primary-lock-steal
    (if abandon-timeout
      (lambda ()
        (if (and-let* ([m (file-mtime lock-name)])
              (< (+ m abandon-timeout) (time->seconds (current-time))))
          (primary-unlock)))
      values))

  (unless (memq type '(file directory))
    (error "unsupported lockfile type:" type))
  ;; main locker
  (try primary-lock
       primary-unlock
       (nsec retry-interval) (nsec retry-limit)
       primary-lock-steal
       proc
       primary-lock-failure))
