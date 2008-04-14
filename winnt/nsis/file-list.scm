;; -*- coding: utf-8 -*-
;;  Copyright (c) 2008 ENDO Yasuyuki, All rights reserved. 
;;  Copyright (c) 2008 Kahua Project, All rights reserved. 
;;  See COPYING for terms and conditions of using this software 

(use file.util)
(use srfi-1)
(use srfi-13)
(use gauche.parameter)
(use gauche.parseopt)
(use text.tree)

(define *dist-path* "../../../Gauche-mingw-dist/Gauche")
(define *instdir* "$INSTDIR")
(define *ignore-path* "CVS")
(define *target-prefix* "    SetOutPath ")
(define *install-prefix* "    File ")
(define *delete-prefix* "    Delete /REBOOTOK ")
(define *cr-lf* "\r\n")
(define *path-separater* "\\")
(define *root-path* (make-parameter #f))
(define *file-list* (make-parameter #f))

(define (replace-inst prefix path)
  (regexp-replace (string->regexp prefix)
          path *instdir*))

(define (file-list path)
  (directory-fold
   path cons '()
   :lister (lambda (path seed)
	     (receive (d f)
		      (directory-list2 path :add-path? #t :children? #t)
		      (values d
			      (cons (cons path f) seed))))))

(define (initialize path)
  (begin0
   (*root-path* path)
   (*file-list*
    (reverse
     (filter
      (lambda (ls) (and (pair? (cdr ls))
			(not (string-contains (car ls) *ignore-path*))))
      (file-list (*root-path*)))))))

(define (inst-str ls)
  (cons
   #`",|*target-prefix*|,(change-path (replace-inst (*root-path*) (car ls))),|*cr-lf*|"
   (map change-path
	(map
	 (lambda (s) #`",|*install-prefix*|,|s|,|*cr-lf*|")
	(cdr ls)))))

(define (del-str ls)
  (map
   (lambda (s) #`",|*delete-prefix*|,(change-path (replace-inst (*root-path*) s)),|*cr-lf*|")
   (cdr ls)))

(define (install-files)
  (map inst-str (*file-list*)))

(define (delete-files)
  (map del-str (*file-list*)))

(define (include line)
  (or
   (and-let* ((match (#/^##\(include (.*)\)\r\n$/ line))
	      (exp (call-with-input-string (match 1) (cut read <>))))
	     (eval exp (current-module)))
   line))

(define (include-all lis)
  (map include lis))

(define (change-path str)
  (string-join (string-split str #\/) *path-separater*))

(define (read-line-crlf file)
  (let1 line (read-line file)
	(if (eof-object? line) line
	    #`",|line|,|*cr-lf*|")))

(define (main args)
  (let* ((in (cadr args))
	 (out (path-sans-extension in))
	 (lines (file->list read-line-crlf in)))
    (begin
      (initialize *dist-path*)
      (call-with-output-file out
	(cut write-tree (include-all lines) <>))
      0)))
