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
;;;  $Id: util.scm,v 1.1 2002-05-01 04:49:33 shirok Exp $
;;;

;;; This module provides convenient utility functions to handle
;;; files and directories.   Some functions are provided with
;;; different names for compatibility with existing Scheme
;;; implementations.

(define-module file.util
  (use srfi-2)
  (use gauche.let-opt)
  (use gauche.time)
  (export current-directory
          file-type file-perm file-mode file-ino file-dev file-rdev file-nlink
          file-uid file-gid file-size file-mtime file-atime file-ctime
          file-eq? file-eqv? file-device=?
          file-mtime=? file-mtime<? file-mtime<=? file-mtime>? file-mtime>=?
          file-atime=? file-atime<? file-atime<=? file-atime>? file-atime>=?
          file-ctime=? file-ctime<? file-ctime<=? file-ctime>? file-ctime>=?
          ))
(select-module file.util)

;;;=============================================================
;;; Directory entries
;;;

;; "current-directory" is found in ChezScheme, MzScheme, etc.
(define (current-directory . maybe-newdir)
  (cond ((null? maybe-newdir) (sys-getcwd))
        ((null? (cdr maybe-newdir))
         (if (string? (car maybe-newdir))
             (sys-chdir (car maybe-newdir))
             (error "directory name should be a string" (car maybe-newdir))))
        (else
         (error "too many arguments for current-directory" maybe-newdir))))

;;;=============================================================
;;; File attributes

;; convenient accessors for file stats.  accepts string file name or
;; <sys-stat>.  If the named file doesn't exist, returns #f.
(define-syntax define-stat-accessor
  (syntax-rules ()
    ((__ name slot)
     (begin
       (define-method name ((a <string>))
         (and-let* (((sys-access a F_OK))
                    (s (sys-stat a)))
           (slot-ref s slot)))
       (define-method name ((a <sys-stat>))
         (slot-ref a 'stat))))))

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

;; compares two files are identical, in the sense that:
;;  file-eq?  - two files (or directories) are the same entity. symbolic
;;              links are not followed.   The files must exist.
;;  file-eqv? - two files are the same entity, after resolving symbolic
;;              links.  The files must exist.

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



(provide "file/util")
