;;;
;;; sequence.scm - sequence operations
;;;
;;;  Copyright(C) 2001 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: sequence.scm,v 1.2 2001-11-13 10:53:26 shirok Exp $
;;;

;;;; This is still experimental.  Do not rely on the interface.

;; This module defines an unified way to treat sequence-like objects
;; (that is, a collection object that can be accessed by integer index).

(define-module gauche.sequence
  (export seq-referencer seq-modifier seq-length
          seq-ref seq-set! subseq seq-append
          seq-for-each seq-map seq-fold seq-fold-right
          seq-find seq-filter seq-remove seq-partition
          seq-sort seq-sort! seq-permute)
  )
(select-module gauche.sequence)

;;================================================================
;; Sequence Referencer Protocol
;;
;;  Mandatory methods
;;     seq-referencer
;;     seq-modifier    (if the sequence is mutable)
;;     seq-length

(define-method seq-referencer ((obj <list>))   list-ref)
(define-method seq-referencer ((obj <vector>)) vector-ref)
(define-method seq-referencer ((obj <string>)) string-ref)

(define-method seq-modifier   ((obj <list>))   list-set!)
(define-method seq-modifier   ((obj <vector>)) vector-set!)
(define-method seq-modifier   ((obj <string>)) string-set!)

(define-method seq-length     ((obj <list>))   (length obj))
(define-method seq-length     ((obj <vector>)) (vector-length obj))
(define-method seq-length     ((obj <string>)) (string-length obj))

;; These two are procedure instead of methods, for efficiency.
(define (seq-ref obj k . args)
  (apply (seq-referencer obj) obj k args))

(define (seq-set! obj k val)
  ((seq-modifier obj) obj k val))

(set! (setter seq-ref) seq-set!)

;; The implementations may override the following methods for efficiency.

(define-method seq-fold (proc knil obj)
  (let ((len (seq-length obj))
        (ref (seq-referencer obj)))
    (do ((i 0 (+ i 1))
         (r knil (proc (ref obj i) r)))
        ((>= i len) r)
      #f)))

(define-method seq-for-each (proc obj)
  (let ((len (seq-length obj))
        (ref (seq-referencer obj)))
    (dotimes (i len) (proc (ref obj i)))))

(define-methdo seq-map (proc obj)
  (let ((len (seq-length obj))
        (ref (seq-referencer obj)))
    (do ((i 0   (+ i 1))
         (r '() (cons (proc (ref obj i)) r)))
        ((>= i len) (reverse r))
      #f)))

;;================================================================
;; Sequence Builder Protocol
;;
;;  Mandatory methods
;;     seq-builder
;;

;; Sequence builder is a procedure to build a sequence.  It can be used
;; to build a sequence with both known and unknown length.
;;
;;   (builder 
;;



(provide "gauche/sequence")
