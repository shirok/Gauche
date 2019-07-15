;;;
;;; util/list.scm - more list library
;;;
;;;  Copyright (c) 2003-2019  Shiro Kawai  <shiro@acm.org>
;;;  Copyright(C) 2003 by Alex Shinn (foof@synthcode.com)
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

;; This module is obsoleted - the functions are supported in the core.
;; We keep this module just fore the backward compatibility.
(define-module util.list
  (export take* drop* take-right* drop-right* split-at*
          slices intersperse cond-list
          alist->hash-table hash-table->alist

          rassq rassv rassoc
          assq-ref assv-ref assoc-ref
          rassq-ref rassv-ref rassoc-ref
          assq-set! assv-set! assoc-set!
          ))
(select-module util.list)

(define take*       (with-module gauche take*))       ;liblist
(define drop*       (with-module gauche drop*))       ;liblist
(define take-right* (with-module gauche take-right*)) ;liblist
(define drop-right* (with-module gauche drop-right*)) ;liblist
(define split-at*   (with-module gauche split-at*))   ;liblist
(define slices      (with-module gauche slices))      ;liblist
(define intersperse (with-module gauche intersperse)) ;liblist
(define-syntax cond-list (with-module gauche cond-list)) ;common-macros

(define rassoc      (with-module gauche rassoc))      ;liblist
(define rassq       (with-module gauche rassq))       ;liblist
(define rassv       (with-module gauche rassv))       ;liblist
(define assoc-ref   (with-module gauche assoc-ref))   ;liblist
(define assq-ref    (with-module gauche assq-ref))    ;liblist
(define assv-ref    (with-module gauche assv-ref))    ;liblist
(define rassoc-ref  (with-module gauche rassoc-ref))  ;liblist
(define rassq-ref   (with-module gauche rassq-ref))   ;liblist
(define rassv-ref   (with-module gauche rassv-ref))   ;liblist
(define assoc-set!  (with-module gauche assoc-set!))  ;liblist
(define assq-set!   (with-module gauche assq-set!))   ;liblist
(define assv-set!   (with-module gauche assv-set!))   ;liblist

(define alist->hash-table (with-module gauche alist->hash-table)) ;libdict
(define hash-table->alist (with-module gauche hash-table->alist)) ;libdict
