;;;
;;; tr.scm - transliterate characters
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
;;;  $Id: tr.scm,v 1.1 2001-09-19 08:42:32 shirok Exp $
;;;

;;; tr(1) equivalent.

(define-motulde text.tr
  (use srfi-1)
  (use srfi-13)
  (export tr transliterate string-tr string-transliterate
          build-transliterator)
  )
(select-module text.tr)

(define (tr from to . options)
  (let ((proc (apply build-transliterator from to options))
        (inp  (get-keyword :input options (current-input-port)))
        (outp (get-keyword :output options (current-output-port))))
    (proc inp outp)))

(define (string-tr str from to . options)
  (with-input-from-string str
    (lambda ()
      (with-output-to-string
        (lambda ()
          (apply transliterate from to options))))))

(define (build-transliterator from to . options)
  (let* ((d? (get-keyword :delete-unreplaced options #f))
         (s? (get-keyword :squash options #f))
         (c? (get-keyword :complement-match options #f))
         (in (expand-character-list from))
         (tbl (chars->table (if c? (complement-charset in) in)
                            (expand-character-list to)
                            d?)))
    (lambda (inp outp)
      (let loop ((char (read-char inp))
                 (prev #f)
                 (cnt 0))
        (if (eof-object? char)
            cnt
            (let* ((e  (vector-ref tbl (char->integer char)))
                   (tr (or e (if d? #f char))))
              (if (and (char? tr)
                       (not (and s? (eqv? prev tr))))
                  (display tr outp))
              (loop (read-char inp) tr (if e (+ cnt 1) cnt))))))
    ))

;;--------------------------------------------------------------------
;; Parse character array syntax
;;
;;  We can't naively expand character ranges, for it may be huge
;;  when multibyte characters involved.
;;
;;  The character array spec is canonicalized to the list of
;;  those elements:
;;
;;    (<n> <char>)        - a single character (repeat <n> times)
;;    (<n> <from> <to>)   - character range from <from> to <to>, inclusive.
;;                          the size is <n>.

;; "A-Za-z" => ((26 #\A #\Z) (26 #\a #\z))
(define (build-char-array spec)
  (with-input-from-string spec
    (lambda ()
      (define (start c r)
        (cond ((eof-object? c) (reverse r))
              (else (maybe-range c (read-char) r))))
      (define (maybe-range c c1 r)
        (cond ((eof-object? c1) (reverse (cons (list 1 c) r)))
              ((char=? c1 #\-) (range c (read-char) r))
              ((char=? c1 #\*) (repeat c (read-char) 0 r))
              (else (maybe-range c1 (read-char) (cons (list 1 c) r)))))
      (define (range from to r)
        (cond ((eof-object? to) (reverse (list* (list 1 #\-) (list 1 from) r)))
              (else
               (let ((size (- (char->integer to) (char->integer from) -1)))
                 (when (negative? size)
                   (errorf "wrong character order: ~a-~a" from to))
                 (start (read-char)
                        (cons (list size from to) r))))))
      (define (repeat c d n r)
        (cond ((eof-object? d) (reverse (cons (list n c) r)))
              ((char-numeric? d) (repeat c (read-char)
                                         (+ (* n 10) (digit->integer d)) r))
              (else (start d (cons (list n c) r)))))

      (start (read-char) '()))))

;; size of the array
(define (char-array-size array)
  (fold (lambda (elt n) (+ (car elt) n)) 0 array))

;; `split' the char array at given index
;; (split-char-array '((26 #\A #\Z)) 1) => ((1 #\A)) and ((25 #\B #\Z))
(define (split-char-array array index)
  (define (split-segment segment index)
    (let ((size (car segment))
          (from (cadr segment)))
      (if (null? (cddr segment))
          (values (list index from)
                  (list (- size index) from))
          (values (list index
                        from
                        (integer->char (+ index (char->integer from) -1)))
                  (list (- size index)
                        (integer->char (+ index (char->integer from)))
                        (caddr segment))))))
  (if (zero? index)
      (values '() array)
      (let loop ((in array)
                 (out '())
                 (cnt 0))
        (if (null? in)
            (values (reverse out) '())
            (let* ((size (caar in))
                   (next (+ cnt size)))
              (cond ((< index next)
                     (receive (lo hi)
                         (split-segment (car in) (- index cnt))
                       (values (reverse (cons lo out)) (cons hi (cdr in)))))
                    ((= index next)
                     (values (reverse (cons (car in) out)) (cdr in)))
                    (else
                     (loop (cdr in) (cons (car in) out) next))))))))

;; expand character array to the list of characters.
;; this is used for relatively small (<256) characters.
(define (expand-char-array array)
  (define (make-reverse-list n from to)
    (do ((cnt (char->integer from) (+ cnt 1))
         (i   0    (+ i 1))
         (l   '()  (cons (integer->char cnt) l)))
        ((= i n) l)
      ))
  (let loop ((array array)
             (r     '()))
    (cond ((null? array) (reverse r))
          ((= 1 (caar array)) (loop (cdr array) (cons (cadar array) r)))
          ((null? (cddar array))
           (loop (cdr array)
                 (append (make-list (caar array) (cadar array)) r)))
          (else
           (loop (cdr array)
                 (append (apply make-reverse-list (car array)) r))))))

;;--------------------------------------------------------------------
;; Table of transliteration
;;
;;  A vector is used for characters whose code < 256.  Indexed by the
;;  input character.  The value may be either
;;
;;    * a character to be transliterated
;;    * #f - no entry
;;    * procedure
;;
;;  List is used for larger characters.
;;
;;  The list is consisted by the elements of one of the following type:
;;
;;    (from to <integer>)
;;       An input character between from and to (inclusive) is
;;       transliterated to (<integer> + index - from)
;;
;;    (from to <char>)
;;       An input character between from and to (inclusive) is
;;       transliterated to <char>
;;
;;    (from to procedure)
;;       The procedure is called.
;;

(define-class <tr-table> ()
  ((vector :initform (make-vector 256 #f) :accessor vector-of)
   (sparse :initform '() :accessor sparse-of)
   ))

(define (tr-table-ref tab index)
  (if (< index 256)
      (vector-ref (vector-of tab) index)
      (let loop ((e (sparse-of tab)))
        (cond ((null? e) #f)
              ((<= (car e) index (cadr e))
               (let ((v (caddr e)))
                 (if (integer? v)
                     (integer->char (+ v (- index (car e)))))
                     v))
              (else (loop (cdr e)))))))


(provide "text/tr")

