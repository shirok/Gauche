;;;
;;; thread.scm - thread related procedures.  to be autoloaded
;;;
;;;  Copyright(C) 2001-2002 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: thread.scm,v 1.1 2002-05-21 10:56:40 shirok Exp $
;;;

(define-module gauche.thread
  (export thread? thread-name thread-specific thread-specific-set!
          mutex? mutex-name mutex-specific mutex-specific-set!
          condition-variable? condition-variable-name
          condition-variable-specific condition-variable-specific-set!
          join-timeout-exception? abandoned-mutex-exception?
          terminated-thread-exception? uncaught-exception?
          uncaught-exception-reason)
  )
(select-module gauche.thread)

;;
;; Thread
;;

(define (thread? obj) (is-a? obj <thread>))

(define (thread-name thread)
  (check-arg thread? thread)
  (slot-ref thread 'name))

(define (thread-specific-set! thread value)
  (check-arg thread? thread)
  (slot-set! thread 'specific value))

(define thread-specific
  (getter-with-setter
   (lambda (thread)
     (check-arg thread? thread)
     (slot-ref thread 'specific))
   thread-specific-set!))

;;
;; Mutex
;;

(define (mutex? obj)  (is-a? obj <mutex>))

(define (mutex-name mutex)
  (check-arg mutex? mutex)
  (slot-ref mutex 'name))

(define (mutex-state mutex)
  (check-arg mutex? mutex)
  (slot-ref mutex 'state))

(define (mutex-specific-set! mutex value)
  (check-arg mutex? mutex)
  (slot-set! mutex 'specific value))

(define mutex-specific
  (getter-with-setter
   (lambda (mutex)
     (check-arg mutex? mutex)
     (slot-ref mutex 'specific))
   mutex-specific-set!))

;;
;; Condition variable
;;

(define (condition-variable? obj) (is-a? obj <condition-variable>))

(define (condition-variable-name cv)
  (check-arg condition-variable? cv)
  (slot-ref cv 'name))

(define (condition-variable-specific-set! cv value)
  (check-arg condition-variable? cv)
  (slot-set! cv 'specific value))

(define condition-variable-specific
  (getter-with-setter
   (lambda (cv)
     (check-arg condition-variable? cv)
     (slot-ref cv 'specific))
   condition-variable-specific-set!))
     
;;
;; Exceptions
;;

(define (join-timeout-exception? obj)
  (is-a? obj <join-timeout-exception>))

(define (abandoned-mutex-exception? obj)
  (is-a? obj <abandoned-mutex-exception>))

(define (terminated-thread-exception? obj)
  (is-a? obj <terminated-thread-exception>))

(define (uncaught-exception? obj)
  (is-a? obj <uncaught-exception>))

(define (uncaught-exception-reason exc)
  (check-arg uncaught-exception? exc)
  (slot-ref exc 'reason))

(provide "gauche/thread")
