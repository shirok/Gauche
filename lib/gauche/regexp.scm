;;;
;;; regexp.scm - regular expression matcher
;;;
;;;  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: regexp.scm,v 1.1 2001-04-01 10:22:27 shiro Exp $
;;;

;; THIS FILE IS NOT WORKING YET!

;; A rudimental implementation of regexp.
;; This will be re-implemented in C in the future.  Just to make
;; things work for now.

;; External interface
;;
;;   <regexp>       : regexp object
;;   <regexp-match> : match object
;;
;;   regexp? (obj -> bool)
;;   string->regexp (string -> procedure)
;;   rxmatch (rx, string -> match)
;;   rxmatch-start (match, integer -> integer)
;;   rxmatch-end (match, integer -> integer)
;;   rxmatch-substring (match, integer -> integer)
;;   

(select-module gauche)

(define-class <regexp> ()
  (matcher))

(define-class <regexp-match> ()
  (matchlist))

(define (%regexp-compile regexp)
  (define (compile char)
    