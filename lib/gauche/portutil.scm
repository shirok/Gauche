;;;
;;; port related utility functions.  to be autoloaded.
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
;;;  $Id: portutil.scm,v 1.4 2003-01-31 02:34:30 shirok Exp $
;;;

(define-module gauche.portutil
  (export port->string port->list port->string-list port->sexp-list
          copy-port port-fold port-fold-right port-for-each port-map
          port-position-prefix port-tell)
  )
(select-module gauche.portutil)

;;-----------------------------------------------------
;; port->something
;;   TODO: allow caller to specify reading units
(define (port->string port)
  (let ((out (open-output-string)))
    (with-port-locking port
      (lambda ()
        (with-port-locking out
          (lambda ()
            (let loop ((ch (read-char port)))
              (unless (eof-object? ch)
                (write-char ch out)
                (loop (read-char port))))
            (get-output-string out)))))))

(define (port->list reader port)
  (with-port-locking port
    (lambda ()
      (let loop ((obj (reader port))
                 (result '()))
        (if (eof-object? obj)
            (reverse! result)
            (loop (reader port) (cons obj result)))))))

(define (port->string-list port)
  (port->list read-line port))

(define (port->sexp-list port)
  (port->list read port))

;;-----------------------------------------------------
;; copy-port
;;

;; only load gauche.uvector if we use chunked copy
(autoload gauche.uvector make-u8vector read-block! write-block)

(define-macro (%do-copy reader writer incr)
  `(with-port-locking src
    (lambda ()
      (with-port-locking dst
        (lambda ()
          (let loop ((data  ,reader)
                     (count 0))
            (if (eof-object? data)
                count
                (begin ,writer
                       (loop ,reader ,incr)))))))))

(define-macro (%do-copy/limit1 reader writer limit)
  `(with-port-locking src
    (lambda ()
      (with-port-locking dst
        (lambda ()
          (let loop ((count 0))
            (if (>= count ,limit)
                count
                (let ((data ,reader))
                  (if (eof-object? data)
                      count
                      (begin ,writer
                             (loop (+ count 1))))))))))))

(define (do-copy/limitN src dst buf unit limit)
  (with-port-locking src
    (lambda ()
      (with-port-locking dst
        (lambda ()
          (let loop ((count 0))
            (if (>= count limit)
                count
                (let1 nr (read-block! buf src 0
                                      (if (>= (+ count unit) limit)
                                          (- limit count)
                                          unit))
                  (if (eof-object? nr)
                      count
                      (begin (write-block buf dst 0 nr)
                             (loop (+ count nr))))))))))))

(define (copy-port src dst . args)
  (check-arg input-port? src)
  (check-arg output-port? dst)
  (let-keywords* args ((unit 4096)
                       (size 0))
    (cond ((eq? unit 'byte)
           (if (and (integer? size) (positive? size))
               (%do-copy/limit1 (read-byte src)
                                (write-byte data dst)
                                size)
               (%do-copy (read-byte src)
                         (write-byte data dst)
                         (+ count 1))))
          ((eq? unit 'char)
           (if (and (integer? size) (positive? size))
               (%do-copy/limit1 (read-char src)
                                (write-char data dst)
                                size)
               (%do-copy (read-char src)
                         (write-char data dst)
                         (+ count 1))))
          ((integer? unit)
           (let ((buf (make-u8vector (if (zero? unit) 4196 unit))))
             (if (and (integer? size) (positive? size))
                 (%do-copy/limitN src dst buf unit size)
                 (%do-copy (read-block! buf src)
                           (write-block buf dst 0 data)
                           (+ count data)))))
          (else
           (error "unit must be 'char, 'byte, or non-negative integer" unit))
          )
    )
  )

;;-----------------------------------------------------
;; Iterators on the input stream
;;   These constructs take reader function that expects to return
;;   new data for each invocation, and EOF whe exhausted.  It can
;;   be any function; need not be bound to a port.

(define (port-fold fn knil reader)
  (let loop ((item (reader))
             (r    knil))
    (if (eof-object? item)
        r
        (loop (reader) (fn item r)))))

;; This will consume large stack if input file is large.
(define (port-fold-right fn knil reader)
  (let loop ((item (reader)))
    (if (eof-object? item)
        knil
        (fn item (loop (reader))))))

(define (port-for-each fn reader)
  (let loop ((item (reader)))
    (unless (eof-object? item)
      (fn item)
      (loop (reader)))))

(define (port-map fn reader)
  (let loop ((item (reader))
             (r    '()))
    (if (eof-object? item)
        (reverse! r)
        (loop (reader) (cons (fn item) r)))))

;; useful for error messages
(define (port-position-prefix port)
  (let ((n (port-name port))
        (l (port-current-line port)))
    (if n
        (if (positive? l)
            (format #f "~s:line ~a: " n l)
            (format #f "~s: " n))
        "")))

;;-----------------------------------------------------
;; useful alias
;;

(define (port-tell p) (port-seek p 0 SEEK_CUR))

(provide "gauche/portutil")
