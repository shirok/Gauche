;;;
;;; file/util.scm - filesystem utility functions
;;;
;;;  Copyright(C) 2002 by Shiro Kawai (shiro@acm.org)
;;;
;;;  Permission to use, copy, modify, distribute this software and
;;;  accompanying documentation for any purpose is hereby granted,
;;;  provided that existing copyright notices are retained in all
;;;  copies and that this notice is included verbatim in all
;;;  distributions.
;;;  This software is provided as is, without express or implied
;;;  warranty.  In no circumstances the author(s) shall be liable
;;;  for any damages arising out of the use of this software.
;;;
;;;  $Id: util.scm,v 1.2 2002-05-02 11:35:48 shirok Exp $
;;;

;;; This module provides convenient utility functions to handle
;;; files and directories.   Some functions are provided with
;;; different names for compatibility with existing Scheme
;;; implementations.

(define-module file.util
  (use srfi-1)
  (use srfi-2)
  (use srfi-11)
  (use srfi-13)
  (use gauche.let-opt)
  (use gauche.time)
  (export current-directory directory-list directory-list2
          build-path resolve-path expand-path simplify-path
          absolute-path? relative-path? decompose-path
          file-type file-perm file-mode file-ino file-dev file-rdev file-nlink
          file-uid file-gid file-size file-mtime file-atime file-ctime
          file-eq? file-eqv? file-equal? file-device=?
          file-mtime=? file-mtime<? file-mtime<=? file-mtime>? file-mtime>=?
          file-atime=? file-atime<? file-atime<=? file-atime>? file-atime>=?
          file-ctime=? file-ctime<? file-ctime<=? file-ctime>? file-ctime>=?
          ))
(select-module file.util)

;;;=============================================================
;;; Directory entries

;; "current-directory" is found in ChezScheme, MzScheme, etc.
(define (current-directory . maybe-newdir)
  (cond ((null? maybe-newdir) (sys-getcwd))
        ((null? (cdr maybe-newdir))
         (if (string? (car maybe-newdir))
             (sys-chdir (car maybe-newdir))
             (error "directory name should be a string" (car maybe-newdir))))
        (else
         (error "too many arguments for current-directory" maybe-newdir))))

;; utility for directory-list and directory-list2
(define (%directory-filter dir preds)
  (cond ((null? preds) (sys-readdir dir))
        ((null? (cdr preds)) (filter (car preds) (sys-readdir dir)))
        (else (filter (lambda (e)
                        (every (lambda (p) (p e)) preds))
                      (sys-readdir dir)))))

;; directory-list DIR &keyword ADD-PATH? CHILDREN? FILTER
(define (directory-list dir . opts)
  (let* ((add-path? (get-keyword :add-path? opts #f))
         (filters   `(,@(if (get-keyword :children? opts #f)
                            '(,(lambda (e) (not (member e '("." "..")))))
                            '())
                      ,@(cond ((get-keyword :filter opts #f) => list)
                              (else '())))))
    (let1 entries (sort (%directory-filter dir filters))
      (if add-path?
          (map (l_ (string-append dir "/" _)) entries)
          entries))))

;; directory-list2 DIR &optional ADD-DIR? CHILDREN? FILTER DONT-FOLLOW-LINK?
(define (directory-list2 dir . opts)
  (let* ((add-path? (get-keyword :add-path? opts #f))
         (filters   `(,@(if (get-keyword :children? opts #f)
                            `(,(lambda (e) (not (member e '("." "..")))))
                            '())
                      ,@(cond ((get-keyword :filter opts #f) => list)
                              (else '()))))
         (selector  (let1 stat
                        (if (get-keyword :dont-follow-link? opts #f)
                            sys-lstat sys-stat)
                      (lambda (e)
                        (eq? (slot-ref (stat e) 'type) 'directory)))))
    (let1 entries (sort (%directory-filter dir filters))
      (if add-path?
          (partition selector
                     (map (l_ (string-append dir "/" _)) entries))
          (partition (l_ (selector (string-append dir "/" _)))
                     entries)))))

;; directory-fold DIR PROC &keyword OPENER DONT-FOLLOW-LINK


;;;=============================================================
;;; Pathnames

(define (build-path base-path . components)
  (define (rec base components)
    (if (null? components)
        base
        (let1 component
            (let1 p (car components)
              (cond ((eq? p 'up) "..")
                    ((eq? p 'same) ".")
                    ((string-prefix? "/" p)
                     (error "can't append absolute path after other path" p))
                    (else p)))
          (rec (if (string-suffix? "/" base)
                   (string-append base component)
                   (string-append base "/" component))
               (cdr components)))))
  (rec base-path components))

(define (expand-path path)
  (sys-normalize-pathname path :expand #t))

(define (resolve-path path)
  (define (rec pat)
    (let* ((dir  (sys-dirname pat))
           (base (sys-basename pat))
           (ndir (if (or (string=? dir "/") (string=? dir "."))
                     dir
                     (rec dir)))
           (p    (build-path ndir base)))
      (unless (sys-access p |F_OK|)
        (error "path doesn't exist" path))
      (if (eq? (file-type p #t) 'symlink)
          (let1 np (sys-readlink p)
            (if (absolute-path? np) np (build-path ndir np)))
          p)))
  (rec (expand-path path)))

(define (simplify-path path)
  (sys-normalize-pathname path :canonicalize #t))

(define (absolute-path? path)
  (or (string-prefix? "/" path) (string-prefix? "~" path)))

(define (relative-path? path)
  (not (absolute-path? path)))

;;;=============================================================
;;; File attributes

;; convenient accessors for file stats.  accepts string file name or
;; <sys-stat>.  If the named file doesn't exist, returns #f.
;; accepts optional argument DONT-FOLLOW-LINK? 
(define-syntax define-stat-accessor
  (syntax-rules ()
    ((_ name slot)
     (define (name path . opts)
       (and-let* (((sys-access path F_OK))
                  (stat (if (and (pair? opts) (car opts)) sys-lstat sys-stat))
                  (s (stat path)))
         (slot-ref s slot))))))

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
(define (file-is-readable? path) (sys-access path |R_OK|))
(define (file-is-writable? path) (sys-access path |W_OK|))
(define (file-is-executable? path) (sys-access path |X_OK|))

;; compares two files are identical, in the sense that:
;;  file-eq?  - two files (or directories) are the same entity. symbolic
;;              links are not followed.   The files must exist.
;;  file-eqv? - two files are the same entity, after resolving symbolic
;;              links.  The files must exist.
;;  file-equal? - the content of two files are the same.

(define (file-eq? f1 f2)
  (let ((s1 (sys-lstat f1))
        (s2 (sys-lstat f2)))
    (and (eqv? (slot-ref s1 'dev) (slot-ref s2 'dev))
         (eqv? (slot-ref s1 'ino) (slot-ref s2 'ino)))))

(define (file-eqv? f1 f2)
  (let ((s1 (sys-stat f1))
        (s2 (sys-stat f2)))
    (and (eqv? (slot-ref s1 'dev) (slot-ref s2 'dev))
         (eqv? (slot-ref s1 'ino) (slot-ref s2 'ino)))))

(define (file-equal? f1 f2)
  (let ((s1 (sys-stat f1))
        (s2 (sys-stat f2)))
    (cond ((and (eqv? (slot-ref s1 'dev) (slot-ref s2 'dev))
                (eqv? (slot-ref s1 'ino) (slot-ref s2 'ino)))
           #t)
          ((not (= (slot-ref s1 'type) (slot-ref s2 'type)))
           #f)
          ((not (= (slot-ref s1 'size) (slot-ref s2 'size)))
           #f)
          ((= (slot-ref s1 'type) 'directory)
           (directory-equal? f1 f2))
          (else
           (call-with-input-file f1
             (lambda (p1)
               (call-with-input-file f2
                 (lambda (p2)
                   (let loop ((b1 (read-block 8192 p1))
                              (b2 (read-block 8192 p2)))
                     (cond ((eof-object? b1) #t)
                           ((string=? b1 b2)
                            (loop (read-block 8192 p1) (read-block 8192 p2)))
                           (else #f))))))))
          )))

;(define (directory-equal? d1 d2 . opts)
;  (check-arg file-is-directory? d1)
;  (check-arg file-is-directory? d2)
;  (let ((recursive? (get-keyword :recursive? opts #f))
;        (nameonly   (get-keyword :name-only? opts #f)))
;    (define (contents dir)
;      (partition file-is-directory?
;                 (map (l_ (string-append dir "/" _))
;;                      (sort (sys-readdir dir)))))
;    (define (cmp-nameonly dir1 dir2)
;      (let-values (((dirs1 files1) (contents dir1))
;                   ((dirs2 files2) (contents dir2)))
;        (and (equal? files1 files2)
;             (if recursive?
;                 (map (

;; see if two files or directories exist on the same device.
(define (file-device=? f1 f2)
  (eqv? (slot-ref (sys-stat f1) 'dev) (slot-ref (sys-stat f2) 'dev)))

;; comparing file timestamp.  accepts string file name, <sys-stat>,
;; <time>, or number.
(define-syntax define-time-comparer
  (syntax-rules ()
    ((_ name slot cmp)
     (begin
       (define-method name ((a <sys-stat>) (b <sys-stat>))
         (cmp (slot-ref a slot) (slot-ref b slot)))
       (define-method name ((a <sys-stat>) (b <number>))
         (cmp (slot-ref a slot) b))
       (define-method name ((a <number>) (b <sys-stat>))
         (cmp a (slot-ref a slot)))
       (define-method name ((a <string>) (b <string>))
         (cmp (sys-stat a) (sys-stat b)))
       (define-method name ((a <string>) b)
         (cmp (sys-stat a) b))
       (define-method name (a (b <string>))
         (cmp a (sys-stat b)))
       (define-method name ((a <time>) b)
         (cmp (time-second a) b))
       (define-method name (a  (b <time>))
         (cmp b (time-second b)))
       ))))

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

;(define (copy-file src dst . opts)
;  (check-arg string? src)
;  (check-arg string? dst)
;  (unless (sys-access src |R_OK|)
;    (error "can't read the source file" src))
;  (let* ((if-exists   (get-keyword :if-exists opts :error))
;         (transaction (get-keyword :transaction opts #f))
;         (


(provide "file/util")
