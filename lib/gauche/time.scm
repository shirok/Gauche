;;;
;;; gauche.time - time structure
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
;;;  $Id: time.scm,v 1.1 2002-03-06 18:59:11 shirok Exp $
;;;

;;; EXPERIMENTAL

;;; Defines <time> class, which can be used as a time object
;;; of SRFI-18, SRFI-19 and SRFI-21.

(define-module gauche.time
  (export <time> current-time time? time-type time-second time-nanosecond)
  )
(select-module gauche.time)

(define-class <time> ()
  ((type       :init-value 'time-utc :init-keyword :type
               :accessor time-type)
   (second     :init-value 0 :init-keyword :second
               :accessor time-second)
   (nanosecond :init-value 0 :init-keyword :nanosecond
               :accessor time-nanosecond))
  )

;; CURRENT-TIME
;;  This interface is of SRFI-18, and time type is fixed to UTC.
;;  SRFI-19 redefines to allow optioal time-type argument.

(define (current-time)
  (receive (secs usecs) (sys-gettimeofday)
    (make <time> :second secs :nanosecond (* usecs 1000))))

;; PREDICATE
(define (time? obj) (is-a? obj <time>))

;; PRINTER
(define-method write-object ((time <time>) port)
  (format port "#<~a ~d:~9,'0d>"
          (time-type time) (time-second time) (time-nanosecond time)))

(provide "gauche/time")
