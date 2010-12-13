;; -*- coding: utf-8 -*-
;;  Copyright (c) 2008 ENDO Yasuyuki, All rights reserved. 
;;  Copyright (c) 2008 Kahua Project, All rights reserved. 
;;  See COPYING for terms and conditions of using this software

(use file.util)
(use srfi-1)
(use srfi-13)
(use util.list)
(use util.match)
(use gauche.parameter)
(use gauche.parseopt)
(use text.tree)

(define *instdir* "$INSTDIR")
(define *ignore-path* ".svn")
(define *target-prefix* "    SetOutPath ")
(define *install-prefix* "    File ")
(define *delete-prefix* "    Delete /REBOOTOK ")
(define *cr-lf* "\r\n")
(define *path-separator* "\\")
(define *root-path* (make-parameter #f))
(define *file-list* (make-parameter #f))
(define *current-version* (make-parameter #f))

(define (replace-inst prefix path)
  (let* ([pre1 (regexp-replace-all #/\// prefix "\\\\")]
         [pre2 (regexp-replace-all #/\\/ pre1 "/")])
    (cond [(or (string-prefix? pre1 path)
               (string-prefix? pre2 path))
           (string-replace path *instdir* 0 (string-length pre1))]
          [else (error "Path prefix ~s does not match: ~a" prefix path)])))

;; NSIS requires version number to be X.X.X.X
(define (canonical-version version)
  (string-join (take* (string-split (regexp-replace #/[-_].+$/ version "") #\.)
                      4 #t "0")
               "."))

(define (file-list path)
  (directory-fold
   path cons '()
   :lister (lambda (path seed)
	     (receive (d f) (directory-list2 path :add-path? #t :children? #t)
               (values d (acons path f seed))))))

(define (initialize path version)
  (*root-path* path)
  (*current-version* (canonical-version version))
  (*file-list*
   (reverse
    (filter
     (lambda (ls) (and (pair? (cdr ls))
                       (not (string-contains (car ls) *ignore-path*))))
     (file-list (*root-path*))))))

(define (inst-str ls)
  (cons
   #`",|*target-prefix*|,(change-path (replace-inst (*root-path*) (car ls))),|*cr-lf*|"
   (map change-path
	(map (lambda (s) #`",|*install-prefix*|,|s|,|*cr-lf*|") (cdr ls)))))

(define (del-str ls)
  (map (lambda (s) #`",|*delete-prefix*|,(change-path (replace-inst (*root-path*) s)),|*cr-lf*|")
       (cdr ls)))

(define (install-files) (map inst-str (*file-list*)))

(define (delete-files)  (map del-str (*file-list*)))

(define (include line)
  (or (and-let* ((match (#/^(.*)##\(include (.*)\)(.*\r\n)$/ line))
                 (exp (call-with-input-string (match 2) (cut read <>))))
        (list (match 1) (eval exp (current-module)) (match 3)))
      line))

(define (include-all lis) (map include lis))

(define (change-path str) (string-join (string-split str #\/) *path-separator*))

(define (read-line-crlf file)
  (let1 line (read-line file)
    (if (eof-object? line) line #`",|line|,|*cr-lf*|")))

(define (main args)
  (match (cdr args)
    [(path in version)
     (let ([out (path-sans-extension in)]
           [lines (file->list read-line-crlf in)])
       (if (file-exists? path)
         (begin
           (initialize path version)
           (call-with-output-file out
             (cut write-tree (include-all lines) <>)))
         (print "Path not exists: " path)))]
    [else
     (print "Usage: gosh " (car args) " <dist-path> <template-file> <gauche-version>")])
  0)
