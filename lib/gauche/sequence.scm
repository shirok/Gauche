;;;
;;; sequence.scm - sequence operations
;;;
;;;  Copyright(C) 2001-2003 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: sequence.scm,v 1.6 2003-02-04 10:37:42 shirok Exp $
;;;

;; This module defines an unified way to treat sequence-like objects
;; (that is, a collection object that can be accessed by integer index).
;; See also gauche.collection, that defines various mapping functions.

(define-module gauche.sequence
  (use srfi-1)
  (extend gauche.collection)
  (export referencer modifier ref subseq
          fold-with-index map-with-index map-to-with-index for-each-with-index
          find-index find-with-index)
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

(define-method (setter subseq) ((seq <sequence>) start vals)
  (with-iterator (vals end? next)
    (do ((index start (+ index 1)))
        ((end?))
      (set! (ref seq index) (next)))))

(define-method (setter subseq) ((seq <sequence>) start end vals)
  (with-iterator (vals end? next)
    (do ((index start (+ index 1)))
        ((>= index end))
      (when (end?) (error "not enough values for (setter subseq)" vals))
      (set! (ref seq index) (next)))))

;; mapping with index ------------------------------------

(define-method fold-with-index (proc knil (seq <sequence>) . more)
  (if (null? more)
      (with-iterator (seq end? next)
        (do ((i 0    (+ i 1))
             (r knil (proc i (next) r)))
            ((end?) r)
          #f))
      (call-with-iterators
       (cons seq more)
       (lambda (ends? nexts)
         (do ((i 0    (+ i 1))
              (r knil (apply proc i (fold-right (lambda (p r) (cons (p) r))
                                                (list r)
                                                nexts))))
             ((any (cut <>) ends?) r)
           #f)))))

;; shortcut
(define-method fold-with-index (proc knil (seq <list>))
  (do ((i 0     (+ i 1))
       (seq seq (cdr seq))
       (r knil  (proc i (car seq) r)))
      ((null? seq) r)
    #f))

(define-method fold-with-index (proc knil (seq <vector>))
  (do ((len (vector-length seq))
       (i 0 (+ i 1))
       (r knil (proc i (vector-ref seq i) r)))
      ((= i len) r)
    #f))

(define-method map-with-index (proc (seq <sequence>) . more)
  (if (null? more)
      (with-iterator (seq end? next)
        (do ((i 0   (+ i 1))
             (r '() (cons (proc i (next)) r)))
            ((end?) (reverse! r))
          #f))
      (call-with-iterators
       (cons seq more)
       (lambda (ends? nexts)
         (do ((i 0   (+ i 1))
              (r '() (cons (apply proc i (map (cut <>) nexts)) r)))
             ((any (cut <>) ends?) (reverse! r))
           #f)))))

;; shortcut
(define-method map-with-index (proc (seq <list>))
  (do ((i 0   (+ i 1))
       (seq seq (cdr seq))
       (r '() (cons (proc i (car seq)) r)))
      ((null? seq) (reverse! r))
    #f))

(define-method map-with-index (proc (seq <vector>))
  (do ((len (vector-length seq))
       (i 0   (+ i 1))
       (r '() (cons (proc i (vector-ref seq i)) r)))
      ((= i len) (reverse! r))
    #f))

(define-method map-to-with-index (class proc (seq <sequence>) . more)
  (if (null? more)
      (with-builder (class add! get :size (size-of seq))
        (with-iterator (seq end? next)
          (do ((i 0   (+ i 1)))
              ((end?) (get))
            (add! (proc i (next))))))
      (with-builder (class add! get :size (size-of seq))
        (call-with-iterators
         (cons seq more)
         (lambda (ends? nexts)
           (do ((i 0   (+ i 1)))
               ((any (cut <>) ends?) (get))
             (add! (apply proc i (map (cut <>) nexts)))))))))

(define-method map-to-with-index ((class <list-meta>) proc (seq <sequence>) . more)
  (apply map-with-index proc seq more))

(define-method for-each-with-index (proc (seq <sequence>) . more)
  (if (null? more)
      (with-iterator (seq end? next)
        (do ((i 0   (+ i 1)))
            ((end?))
          (proc i (next))))
      (call-with-iterators
       (cons seq more)
       (lambda (ends? nexts)
         (do ((i 0   (+ i 1)))
             ((any (cut <>) ends?))
           (apply proc i (map (cut <>) nexts)))))))

;; shortcut
(define-method for-each-with-index (proc (seq <list>))
  (do ((i 0   (+ i 1))
       (seq seq (cdr seq)))
      ((null? seq))
    (proc i (car seq))))

(define-method for-each-with-index (proc (seq <vector>))
  (do ((len (vector-length seq))
       (i 0 (+ i 1)))
      ((= i len))
    (proc i (vector-ref seq i))))

;; find with index ------------------------------------

(define-method find-with-index (pred (seq <sequence>))
  (with-iterator (seq end? next)
    (let loop ((i 0))
      (if (end?)
          (values #f #f)
          (let1 elt (next)
            (if (pred elt)
                (values i elt)
                (loop (+ i 1))))))))

;; shortcut
(define-method find-with-index (pred (seq <list>))
  (let loop ((i 0) (seq seq))
    (cond ((null? seq) (values #f #f))
          ((pred (car seq)) (values i (car seq)))
          (else (loop (+ i 1) (cdr seq))))))
(define-method find-with-index (pred (seq <vector>))
  (let loop ((i 0) (len (vector-length seq)))
    (cond ((= i len) (values #f #f))
          ((pred (vector-ref seq i)) (values i (vector-ref seq i)))
          (else (loop (+ i 1) len)))))

(define-method find-index (pred (seq <sequence>))
  (receive (i e) (find-with-index pred seq) i))


(provide "gauche/sequence")
