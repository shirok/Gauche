;;;
;;; parse.scm - utilities to parse input
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
;;;  $Id: parse.scm,v 1.3 2001-10-05 09:55:12 shirok Exp $
;;;

;; This module implements the input parsing utilities described in Oleg's site
;;  http://pobox.com/~oleg/ftp
;; (follow the link of "Scheme Code -> Input parsing")
;;
;; The functions are API compatible with Oleg's library.  I reimplemented
;; these to be efficient on Gauche; you see no string-set! and
;; string-ref here.

(define-module text.parse
  (use srfi-13)
  (use srfi-14)
  (use gauche.let-opt)
  (export find-string-from-port?
          assert-curr-char
          skip-until
          skip-while
          peek-next-char
          next-token
          next-token-of
          read-string)
  )
(select-module text.parse)

(define (find-string-from-port? str in-port . args)
  (let-optionals* args ((max-no-chars #f))

   (if (string-null? str)
       0                               ;special case
       (let ((restart (make-kmp-restart-vector str))
             (pattern (list->vector (string->list str)))
             (patlen  (string-length str)))

         (define (scan patpos count char)
           (cond ((eof-object? char) #f)
                 ((char=? char (vector-ref pattern patpos))
                  (if (= patpos (- patlen 1))
                      count
                      (scan (+ patpos 1) (+ count 1) (read-char in-port))))
                 ((and max-no-chars (>= count max-no-chars)) #f)
                 ((= patpos 0)
                  (scan 0 (+ count 1) (read-char in-port)))
                 (else
                  (scan (vector-ref restart patpos) count char))
                 ))
      
         (scan 0 1 (read-char in-port))
         ))))

;; the functions taking CHAR-LIST in oleg's utilities are extended to
;; accept a character set, or a list of mixture of characters, 
;; character sets and symbol *eof*.

(define (fold-char-list char-list)
  (cond
   ((char-set? char-list) (values char-list #f))
   ((list? char-list)
    (let ((cs (char-set-copy char-set:empty))
          (eof-allowed? #f))
      (for-each (lambda (item)
                  (cond ((char? item) (char-set-adjoin! cs item))
                        ((char-set? item) (char-set-union! cs item))
                        ((eq? item '*eof*) (set! eof-allowed? #t))
                        (else (error "CHAR-LIST must be a list of characters, character sets and/or symbol '*eof*, but found" item))))
                char-list)
      (values cs eof-allowed?)))
   (else (error "CHAR-LIST must be a char-set or a list of characters, char-sets and/or symbol '*eof*" char-list))))

(define (assert-curr-char char-list string . args)
  (let-optionals* args ((port (current-input-port)))
    (receive (cset eof-ok?) (fold-char-list char-list)

      (define (bad c)
        (errorf "~awrong character c ~a. ~s expected."
                (port-position-prefix port)
                string
                char-list))

      (let ((c (read-char port)))
        (cond ((eof-object? c) (if eof-ok? c (bad c)))
              ((char-set-contains? cset c) c)
              (else (bad c)))))))

(define (skip-until char-list/number . args)
  (let-optionals* args ((port (current-input-port)))
    (define (bad) (errorf "~aunexpected EOF" (port-position-prefix port)))
    (if (number? char-list/number)
        (if (<= 1 char-list/number)
            (let loop ((i 1) (c (read-char port)))
              (cond ((eof-object? c) (bad))
                    ((>= i char-list/number) #f)
                    (else (loop (+ i 1) (read-char port)))))
            #f)
        (receive (cset eof-ok?) (fold-char-list char-list/number)
          (let loop ((c (read-char port)))
            (cond ((eof-object? c) (if eof-ok? c (bad)))
                  ((char-set-contains? cset c) c)
                  (else (loop (read-char port)))))))))

(define (skip-while char-list . args)
  (let-optionals* args ((port (current-input-port)))
    (receive (cset eof-ok?) (fold-char-list char-list)
      (define (bad) (errorf "~aunexpected EOF" (port-position-prefix port)))
      (let loop ((c (peek-char port)))
        (cond ((eof-object? c) c)
              ((char-set-contains? cset c)
               (read-char port)
               (loop (peek-char port)))
              (else c))))))

(define (peek-next-char . args)
  (let-optionals* args ((port (current-input-port)))
    (read-char port)
    (peek-char port)))

(define (next-token prefix-char-list break-char-list . args)
  (let-optionals* args ((comment "")
                        (port (current-input-port)))
    (receive (cs eof-ok?) (fold-char-list break-char-list)
      (with-output-to-string
        (lambda ()
          (let loop ((c (skip-while prefix-char-list port)))
            (cond ((eof-object? c)
                   (unless eof-ok?
                     (errorf "~aunexpected EOF" (port-position-prefix port))))
                  ((char-set-contains? cs c))
                  (else (display (read-char port))
                        (loop (peek-char port))))))))
    ))

(define (next-token-of inc-charset/pred . args)
  (let-optionals* args ((port (current-input-port)))
    (if (procedure? inc-charset/pred)
        (with-output-to-string
          (lambda ()
            (let loop ((c (peek-char port)))
              (when (inc-charset/pred c)
                (display (read-char port))
                (unless (eof-object? c) ;prevent infinite loop
                  (loop (peek-char port)))))))
        (with-output-to-string
          (lambda ()
            (receive (cs eof-ok?) (fold-char-list inc-charset/pred)
              (let loop ((c (peek-char port)))
                (cond ((eof-object? c)) ;ok to see EOF
                      ((char-set-contains? cs c)
                       (display (read-char port))
                       (loop (peek-char port)))
                      (else #f))))))
        )))

;; read-line is built in Gauche.

(define (read-string n . args)
  (let-optionals* args ((port (current-input-port)))
    (with-output-to-string
      (lambda ()
        (let loop ((i 0))
          (unless (>= i n)
            (let ((c (read-char port)))
              (unless (eof-object? c)
                (display c)
                (loop (+ i 1))))))))))

(provide "text/parse")
