;;;
;;; sequence.scm - sequence operations
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
;;;  $Id: sequence.scm,v 1.4 2002-04-26 10:05:07 shirok Exp $
;;;

;; This module defines an unified way to treat sequence-like objects
;; (that is, a collection object that can be accessed by integer index).
;; See also gauche.collection, that defines various mapping functions.

(define-module gauche.sequence
  (use gauche.collection)
  (export referencer modifier ref subseq)
  )
(select-module gauche.sequence)

(define-method referencer ((obj <list>))   list-ref)
(define-method referencer ((obj <vector>)) vector-ref)
(define-method referencer ((obj <weak-vector>)) weak-vector-ref)
(define-method referencer ((obj <string>)) string-ref)

(define-method modifier   ((obj <list>))
  (lambda (o i v) (set-car! (list-tail o i) v)))
(define-method modifier   ((obj <vector>)) vector-set!)
(define-method modifier   ((obj <weak-vector>)) weak-vector-set!)
(define-method modifier   ((obj <string>)) string-set!)

;; ref and (setter ref) --------------------------------

(define-method ref ((obj <sequence>) index)
  ((referencer obj) obj index))

(define-method ref ((obj <sequence>) index default)
  ((referencer obj) obj index default))

(define-method (setter ref) ((obj <sequence>) index value)
  ((modifier obj) obj index value))

;; subseq ----------------------------------------------

(define-method subseq ((seq <sequence>))
  (subseq seq 0 (size-of seq)))

(define-method subseq ((seq <sequence>) start)
  (subseq seq start (size-of seq)))

(define-method subseq ((seq <sequence>) start end)
  (when (< end 0) (set! end (modulo end (size-of seq))))
  (when (> start end)
    (errorf "start ~a must be smaller than or equal to end ~a" start end))
  (let ((size (- end start)))
    (with-builder ((class-of seq) add! get :size size)
      (with-iterator (seq end? next :start start)
        (dotimes (i size (get)) (add! (next)))))))


(provide "gauche/sequence")
