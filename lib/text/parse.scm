;;;
;;; parse.scm - utilities to parse input
;;;  
;;;   Copyright (c) 2000-2007  Shiro Kawai  <shiro@acm.org>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  
;;;  $Id: parse.scm,v 1.9 2007-03-02 07:39:11 shirok Exp $
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
  (let-optionals* args ((comment "unexpected EOF")
                        (port (current-input-port)))
    (define (bad) (errorf "~a~a" (port-position-prefix port) comment))
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
