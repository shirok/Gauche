;;; 
;;; gauche/time.scm - time the procedure
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
;;;  $Id: time.scm,v 1.4 2002-12-06 00:37:11 shirok Exp $
;;;

(define-module gauche.time
  (use srfi-11)
  (export time
          <real-time-counter> <user-time-counter>
          <system-time-counter> <process-time-counter>
          time-counter-start! time-counter-stop! time-counter-reset!
          time-counter-value with-time-counter
          time-counter-get-current-time time-counter-get-delta)
  )
(select-module gauche.time)

;; Time, a simple measurement ------------------------------

(define (format-delta-time delta)
  (let*-values (((frac sec) (modf delta))
                ((ignore ifrac) (modf (+ (* frac 1000) 0.5))))
    (format #f "~3d.~3,'0d" (inexact->exact sec) (inexact->exact ifrac))))

(define-syntax time
  (syntax-rules ()
    ((_ expr . exprs)
     (let*-values (((stimes) (sys-times))
                   ((sreal-sec sreal-msec) (sys-gettimeofday))
                   (r (begin expr . exprs))
                   ((etimes) (sys-times))
                   ((ereal-sec ereal-msec) (sys-gettimeofday)))
       (format (current-error-port)
               ";~,,,,75:s\n; real ~a\n; user ~a\n; sys  ~a\n"
               '(time expr . exprs)
               (format-delta-time
                (- (+ ereal-sec (/ ereal-msec 1000000))
                   (+ sreal-sec (/ sreal-msec 1000000))))
               (format-delta-time
                (/ (- (list-ref etimes 0) (list-ref stimes 0))
                   (list-ref stimes 4)))
               (format-delta-time
                (/ (- (list-ref etimes 1) (list-ref stimes 1))
                   (list-ref stimes 4))))
       (apply values r)))
    ((_)
     (syntax-error "usage: (time expr expr2 ...); or you meant sys-time?"))
    ))

;; Timers ---------------------------------------------

(define-class <time-counter> ()
  ((value   :init-value 0)    ;accured time, in seconds
   (start   :init-value #f)   ;start time.  format depends on the subclass.
   (nesting :init-value 0))   ;nesting level.
  )

(define-method initialize ((self <time-counter>) initargs)
  (next-method)
  (when (eq? (class-of self) <time-counter>)
    (error "you can't instantiate <time-counter> class directly; use either <real-time-counter>, <user-time-counter> or <process-time-counter>"))
  )

(define-method time-counter-start! ((self <time-counter>))
  (let1 nesting (slot-ref self 'nesting)
    (when (zero? nesting)
      (slot-set! self 'start (time-counter-get-current-time self)))
    (slot-set! self 'nesting (+ nesting 1))))


(define-method time-counter-stop! ((self <time-counter>))
  (let1 nesting (slot-ref self 'nesting)
    (cond
     ((= nesting 1)
      (inc! (slot-ref self 'value) (time-counter-get-delta self))
      (set! (slot-ref self 'nesting) 0))
     ((> nesting 1)
      (set! (slot-ref self 'nesting) (- nesting 1)))
     (else
      ;; sprious stop.  make sure the state is sane
      (set! (slot-ref self 'nesting) 0)))))

(define-method time-counter-get-delta ((self <time-counter>))
  ;; This default method assumes time-counter-get-current-time returns
  ;; a real number of seconds.  A subclass that uses different representation
  ;; should override this method as well.
  (let1 end (time-counter-get-current-time self)
    (- end (slot-ref self 'start))))

(define-method time-counter-value ((self <time-counter>))
  (slot-ref self 'value))

(define-method time-counter-reset! ((self <time-counter>))
  (slot-set! self 'value 0)
  (slot-set! self 'nesting 0))

(define-method write-object ((self <time-counter>) port)
  (format port "#<~a ~a>"
          (class-name (class-of self))
          (format-delta-time (slot-ref self 'value))))

(define-syntax with-time-counter
  (syntax-rules ()
    ((_ counter . exprs)
     (dynamic-wind
      (lambda () (time-counter-start! counter))
      (lambda () . exprs)
      (lambda () (time-counter-stop! counter))))
    ))

;; 'real' time counter
(define-class <real-time-counter> (<time-counter>)
  ())

(define-method time-counter-get-current-time ((self <real-time-counter>))
  (receive r (sys-gettimeofday) r))

(define-method time-counter-get-delta ((self <real-time-counter>))
  (receive (sec usec) (sys-gettimeofday)
    (+ (- sec (car (slot-ref self 'start)))
       (/ (- usec (cadr (slot-ref self 'start)))
          1000000))))

;; 'user' time counter
(define-class <user-time-counter> (<time-counter>)
  ())

(define-method time-counter-get-current-time ((self <user-time-counter>))
  (let1 times (sys-times)
    (/ (list-ref times 0) (list-ref times 4))))

;; 'system' time counter
(define-class <system-time-counter> (<time-counter>)
  ())

(define-method time-counter-get-current-time ((self <system-time-counter>))
  (let1 times (sys-times)
    (/ (list-ref times 1) (list-ref times 4))))

;; 'process' time counter - 'user' + 'sys'
(define-class <process-time-counter> (<time-counter>)
  ())

(define-method time-counter-get-current-time ((self <process-time-counter>))
  (let1 times (sys-times)
    (/ (+ (list-ref times 0) (list-ref times 1))
       (list-ref times 4))))

(provide "gauche/time")
