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
;;;  $Id: parse.scm,v 1.5 2001-10-06 07:21:20 shirok Exp $
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

(define (skip-until char-list/number/pred . args)
  (let-optionals* args ((port (current-input-port)))
    (define (bad) (errorf "~aunexpected EOF" (port-position-prefix port)))
    (cond
     ((number? char-list/number/pred)
      (if (<= 1 char-list/number/pred)
          (let loop ((i 1) (c (read-char port)))
            (cond ((eof-object? c) (bad))
                  ((>= i char-list/number/pred) #f)
                  (else (loop (+ i 1) (read-char port)))))
          #f))
     ((procedure? char-list/number/pred)
      (let loop ((c (read-char port)))
        (cond ((char-list/number/pred c) c)
              ((eof-object? c) (bad))
              (else (loop (read-char port)))))
      )
     (else
      (receive (cset eof-ok?) (fold-char-list char-list/number/pred)
        (let loop ((c (read-char port)))
          (cond ((eof-object? c) (if eof-ok? c (bad)))
                ((char-set-contains? cset c) c)
                (else (loop (read-char port))))))))))

(define (skip-while char-list/pred . args)
  (let-optionals* args ((port (current-input-port)))
    (define (bad) (errorf "~aunexpected EOF" (port-position-prefix port)))
    (cond
     ((procedure? char-list/pred)
      (let loop ((c (peek-char port)))
        (cond ((char-list/pred c) (read-char port) (loop (peek-char port)))
              (else c)))
      )
     (else
      (receive (cset eof-ok?) (fold-char-list char-list/pred)
        (let loop ((c (peek-char port)))
          (cond ((eof-object? c) c)
                ((char-set-contains? cset c)
                 (read-char port)
                 (loop (peek-char port)))
                (else c))))))))

(define (peek-next-char . args)
  (let-optionals* args ((port (current-input-port)))
    (read-char port)
    (peek-char port)))

(define (next-token prefix-char-list/pred break-char-list/pred . args)
  (let-optionals* args ((comment "")
                        (port (current-input-port)))
    (define (bad) (errorf "~aunexpected EOF" (port-position-prefix port)))
    (let ((c (skip-while prefix-char-list/pred port)))
      (cond
       ((procedure? break-char-list/pred)
        (with-output-to-string
          (lambda ()
            (let loop ((c c))
              (cond ((break-char-list/pred c))
                    ((eof-object? c) (bad))
                    (else                    
                     (display (read-char port))
                     (loop (peek-char port)))))))
        )
       (else
        (receive (cs eof-ok?) (fold-char-list break-char-list/pred)
          (with-output-to-string
            (lambda ()
              (let loop ((c c))
                (cond ((eof-object? c) (unless eof-ok? (bad)))
                      ((char-set-contains? cs c))
                      (else (display (read-char port))
                            (loop (peek-char port))))))))
        )))))

(define (next-token-of char-list/pred . args)
  (let-optionals* args ((port (current-input-port)))
    (cond
     ((procedure? char-list/pred)
      (with-output-to-string
        (lambda ()
          (let loop ((c (peek-char port)))
            (when (char-list/pred c)
              (display (read-char port))
              (unless (eof-object? c) ;prevent infinite loop
                (loop (peek-char port))))))))
     (else
      (with-output-to-string
        (lambda ()
          (receive (cs eof-ok?) (fold-char-list char-list/pred)
            (let loop ((c (peek-char port)))
              (cond ((eof-object? c)) ;ok to see EOF
                    ((char-set-contains? cs c)
                     (display (read-char port))
                     (loop (peek-char port)))
                    (else #f))))))
      ))))

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
