;; -*- coding: utf-8 -*-
;;  Copyright (c) 2008 ENDO Yasuyuki, All rights reserved. 
;;  Copyright (c) 2008 Kahua Project, All rights reserved. 
;;  See COPYING for terms and conditions of using this software 

(use file.util)
(use srfi-13)
(use gauche.parameter)
(use gauche.parseopt)

(define *instdir* "$INSTDIR")
(define *ignore-path* "CVS")
(define *target-prefix* "    SetOutPath ")
(define *install-prefix* "    File ")
(define *delete-prefix* "    Delete /REBOOTOK ")
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
   (*file-list* (reverse (file-list (*root-path*))))))

(define (print-inst ls)
  (unless (or (null? (cdr ls))
	      (string-contains (car ls) *ignore-path*))
	  (print *target-prefix*
		 (replace-inst (*root-path*) (car ls)))
	  (for-each (cut print *install-prefix* <>)
		    (cdr ls))))

(define (print-del ls)
  (unless (or (null? (cdr ls))
	      (string-contains (car ls) *ignore-path*))
	  (for-each
	   (lambda (s)
	     (print *delete-prefix*
		    (replace-inst (*root-path*) s)))
	   (cdr ls))))

(define (main args)
  (let-args (cdr args)
   [(ins "i|ins" #f)
    (del "d|del" #f)
    (path "p|path=s")]
   (when path
	 (initialize path)
	 (when ins
	       (for-each print-inst (*file-list*)))
	 (when del
	       (for-each print-del (*file-list*))))))
