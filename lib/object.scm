;;;
;;; object.scm - object system
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
;;;  $Id: object.scm,v 1.3 2001-03-17 09:22:37 shiro Exp $
;;;

(select-module gauche)

;;;
;;; This module is A WORK IN PROGRESS.   Don't expect this to work.
;;;

(define (class-name class) (slot-ref class 'name))
(define (class-precedence-list class) (slot-ref class 'cpl))
(define (class-direct-supers class) (slot-ref class 'direct-supers))
(define (class-direct-slots class) (slot-ref class 'direct-slots))
(define (class-slots class) (slot-ref class 'slots))


(define (%inspect obj)
  (if (is-a? obj <object>)
      (for-each (lambda (slot)
                  (format #t "~s: ~s\n" slot (slot-ref obj slot)))
                (class-slots (class-of obj)))
      obj))
