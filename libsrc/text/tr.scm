;;;
;;; tr.scm - transliterate characters
;;;
;;;   Copyright (c) 2000-2019  Shiro Kawai  <shiro@acm.org>
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

;;; tr(1) equivalent.

(define-module text.tr
  (use srfi-1)
  (use srfi-13)
  (use gauche.generator)
  (export tr transliterate string-tr string-transliterate
          build-transliterator)
  )
(select-module text.tr)

(define (tr from to . options)
  ((apply build-transliterator from to options)))

(define transliterate tr)               ;alias

(define (string-tr str from to . options)
  (with-string-io str (cut apply tr from to options)))

(define string-transliterate string-tr) ;alias

(define (build-transliterator from to :key ((:delete d?) #f) ((:squeeze s?) #f)
                              ((:complement c?) #f) ((:table-size size) 256)
                              (input #f) (output #f))
  (let1 tab (build-tr-table from to size c?)
    (^[]
      (let ([in  (or input (current-input-port))]
            [out (or output (current-output-port))])
        (let loop ([char (read-char in)] [prev #f])
          (unless (eof-object? char)
            (let1 c (tr-table-ref tab (char->integer char))
              (cond
               [(char? c)               ;transliterated
                (unless (and s? (eqv? prev c)) (display c out))
                (loop (read-char in) c)]
               [c                       ;char is not in from-set
                (display char out)
                (loop (read-char in) #f)]
               [(not d?)                ;char is in from but not to, and no :d
                (unless (and s? (eqv? prev char)) (display char out))
                (loop (read-char in) char)]
               [else
                (loop (read-char in) prev)]))))))))

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
;; The grouping never crosses the boundary of table-size, i.e. if the table
;; size is 70 ('F'), "A-Z" => ((6 #\A #\F) (20 #\G #\Z))
(define (build-char-array spec table-size)
  (with-input-from-string spec
    (^[]
      (define (start c r)
        (cond [(eof-object? c) (reverse r)]
              [(char=? c #\\) (let1 c (read-char)
                                (when (eof-object? c)
                                  (error "stray backslash in tr spec" spec))
                                (maybe-range c (read-char) r))]
              [else (maybe-range c (read-char) r)]))
      (define (maybe-range c c1 r)
        (cond [(eof-object? c1) (reverse (cons (list 1 c) r))]
              [(char=? c1 #\-) (range c (read-char) r)]
              [(char=? c1 #\*) (repeat c (read-char) 0 r)]
              [(char=? c1 #\\) (start (read-char) (cons (list 1 c) r))]
              [else (maybe-range c1 (read-char) (cons (list 1 c) r))]))
      (define (range from to r)
        (cond [(eof-object? to) (reverse (list* (list 1 #\-) (list 1 from) r))]
              [else
               (let* ([fromi (char->integer from)]
                      [toi   (char->integer to)]
                      [size  (- toi fromi -1)])
                 (when (negative? size)
                   (errorf "wrong character order: ~a-~a" from to))
                 (if (and (< fromi table-size) (<= table-size toi))
                   (start (read-char)
                          (list* (list (- toi table-size)
                                       (integer->char table-size)
                                       to)
                                 (list (- table-size fromi)
                                       from
                                       (integer->char (- table-size 1)))
                                 r))
                   (start (read-char)
                          (cons (list size from to) r))))]))
      (define (repeat c d n r)
        (cond [(eof-object? d)
               (reverse (cons (list (if (= n 0) *char-code-max* n) c) r))]
              [(char-numeric? d)
               (repeat c (read-char) (+ (* n 10) (digit->integer d)) r)]
              [else
               (start d (cons (list (if (= n 0) *char-code-max* n) c) r))]))

      (start (read-char) '()))))

;; size of the array
(define (char-array-size array)
  (fold (^[elt n] (+ (car elt) n)) 0 array))

;; ref
(define (char-array-ref array index)
  (let loop ([array array]
             [cnt   0])
    (cond [(null? array) #f]
          [(>= index (+ cnt (caar array)))
           (loop (cdr array) (+ cnt (caar array)))]
          [(null? (cddar array)) (cadar array)]
          [else (let ([from (char->integer (cadar array))]
                      [to   (char->integer (caddar array))])
                  (integer->char (+ from (- index cnt))))])))

;; complement.  array doesn't contain repeat.
(define (complement-char-array array table-size)
  (define (add-range from to rest)
    (if (and (< from table-size) (<= table-size to))
      (list* (list (- to table-size -1)
                   (integer->char table-size)
                   (integer->char to))
             (list (- table-size from)
                   (integer->char from)
                   (integer->char (- table-size 1)))
             rest)
      (cons (list (- to from -1) (integer->char from) (integer->char to))
            rest)))
  (let loop ([in (sort-by array cadr)]
             [out '()]
             [index 0])
    (if (null? in)
      (if (<= index *char-code-max*)
        (reverse! (add-range index *char-code-max* out))
        (reverse! out))
      (let* ([lo (char->integer (cadar in))]
             [hi (+ lo (caar in))])
        (if (< index lo)
          (loop (cdr in)
                (add-range index (- lo 1) out)
                hi)
          (loop (cdr in) out hi))))))

;; `split' the char array at given index
;; (split-char-array '((26 #\A #\Z)) 1) => ((1 #\A)) and ((25 #\B #\Z))
(define (split-char-array array index)
  (define (split-segment segment index)
    (let ([size (car segment)]
          [from (cadr segment)])
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
    (let loop ([in array]
               [out '()]
               [cnt 0])
      (if (null? in)
        (values (reverse out) '())
        (let* ([size (caar in)]
               [next (+ cnt size)])
          (cond [(< index next)
                 (receive (lo hi)
                     (split-segment (car in) (- index cnt))
                   (values (reverse (cons lo out)) (cons hi (cdr in))))]
                [(= index next)
                 (values (reverse (cons (car in) out)) (cdr in))]
                [else
                 (loop (cdr in) (cons (car in) out) next)]))))))

;; "from" character list shouldn't contain
(define (check-from-spec-validity spec array)
  (dolist [segment array]
    (when (and (< 1 (car segment)) (null? (cddr segment)))
      (error "from-spec can't contain repeat characters" spec))))

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
  ((vector-size :init-keyword :vector-size :initform 256)
   (vector :accessor vector-of)
   (sparse :initform '() :accessor sparse-of)
   ))

(define-method initialize ((self <tr-table>) initargs)
  (next-method)
  (let1 size (slot-ref self 'vector-size)
    (set! (vector-of self) (make-vector size #t))))

;; Returns
;;   char - mapped char
;;   #f   - appears in from-set but not to-set
;;   #t   - doesn't appear in from-set
(define (tr-table-ref tab index)
  (if (< index (slot-ref tab 'vector-size))
    (vector-ref (vector-of tab) index)
    (let loop ([e (sparse-of tab)])
      (cond [(null? e) #t]
            [(<= (caar e) index (cadar e))
             (let1 v (caddar e)
               (if (integer? v)
                 (integer->char (+ v (- index (caar e))))
                 v))]
            [else (loop (cdr e))]))))

(define (build-tr-table from-spec to-spec size compl?)
  (rlet1 tab (make <tr-table> :vector-size size)
    (let ([from-ca (build-char-array from-spec size)]
          [to-ca   (build-char-array to-spec size)])
      (check-from-spec-validity from-spec from-ca)
      (fill-tr-table tab
                     (if compl?
                       (complement-char-array from-ca size)
                       from-ca)
                     to-ca))))

(define (fill-tr-table tab from-ca to-ca)
  (let loop ([from-ca from-ca]
             [to-ca   to-ca])
    (if (null? from-ca)
      tab
      (let* ([vsiz     (slot-ref tab 'vector-size)]
             [from-seg (car from-ca)]
             [size     (car from-seg)])
        (receive (to-segs to-rest) (split-char-array to-ca size)
          (if (< (char->integer (cadr from-seg)) vsiz)
            (fill-tr-vector tab from-seg to-segs)
            (fill-tr-sparse tab from-seg to-segs))
          (loop (cdr from-ca) to-rest))))))

(define (fill-tr-vector tab from-seg to-ca)
  (do ([v       (vector-of tab)]
       [size    (car from-seg)]
       [from-ch (char->integer (cadr from-seg)) (+ from-ch 1)]
       [cnt     0   (+ cnt 1)])
      [(= cnt size)]
    (vector-set! v from-ch (char-array-ref to-ca cnt))))

(define (fill-tr-sparse tab from-seg to-ca)
  (let loop ([from-ch (char->integer (cadr from-seg))]
             [to-segs to-ca]
             [r       '()])
    (if (null? to-segs)
      (let* ([from-end (+ (car from-seg) (char->integer (cadr from-seg)))]
             [rr (if (< from-ch from-end)
                   (reverse (cons (list from-ch (- from-end 1) #f) r))
                   (reverse r))])
        (set! (sparse-of tab) (append! (sparse-of tab) rr)))
      (let* ([to-seg  (car to-segs)]
             [to-size (car to-seg)]
             [entry   (list from-ch (+ from-ch to-size -1)
                            (if (null? (cddr to-seg))
                              (cadr to-seg)
                              (char->integer (cadr to-seg))))])
        (loop (+ from-ch to-size) (cdr to-segs) (cons entry r))))))
