;;;
;;; peg.scm - Parser Expression Grammar Parser
;;;
;;;   Copyright (c) 2006 Rui Ueyama (rui314@gmail.com)
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

(define-module peg-orig
  (use srfi-1)
  (use srfi-13)
  (use srfi-14)
  (use util.match)
  (export parse-success? parse-failure?
          stream-position
          <parse-error>

          result-value
          result-next
          failure-type
          failure-message
          failure-position
          parse-string
          $return $fail $expect 
          $do $cut $seq $or $many $skip-many
          $repeat $optional

          $alternate

          $sep-by $end-by $sep-end-by
          $count $between
          $not $many-till $chain-left $chain-right
          $lazy
          $string $string-ci
          $char $one-of $none-of
          $satisfy

          anychar upper lower letter alphanum digit
          hexdigit newline tab space spaces eof

          $->rope semantic-value-finalize!
          )
  )
(select-module peg-orig)

(debug-print-width 1024)

;;;============================================================
;;; How is EBNF represented in the PEG library?
;;;
;;;   A ::= B C
;;;     => (define a ($seq b c))
;;;    If you need values of B and C, $do can be used:
;;;     => (define a ($do ((x b) (y c)) (cons x y)))
;;;
;;;   A :: B | C
;;;     => (define a ($or b c))
;;;
;;;   A :: B*
;;;     => (define a ($many b))
;;;
;;;   A :: B+
;;;     => (define a ($many b 1))
;;;
;;;   A ::= B B | B B B
;;;     => (define a ($many b 2 3))
;;;
;;;   A ::= B?
;;;     => (define a ($optional b))
;;;

;;;============================================================
;;; Parse result types
;;;

;; result ::= ('success <semantic-value> <stream>)
;; error ::= ('fail <failure-type> <message-string> <position>)

(define-condition-type <parse-error> <error> #f
  (position))

(define-method write-object ((o <parse-error>) out)
  (format out "#<<parse-error> ~a ~S>"
          (ref o 'position)
          (ref o 'message)))

(define (parse-success? obj)
  (and (vector? obj)
       (eq? 'success (vector-ref obj 0))))

(define (parse-failure? obj)
  (and (vector? obj)
       (eq? 'fail (vector-ref obj 0))))

(define (make-result value stream)
  (vector 'success value stream))

(define result-value     (cut vector-ref <> 1))
(define result-next      (cut vector-ref <> 2))
(define failure-type     (cut vector-ref <> 1))
(define failure-message  (cut vector-ref <> 2))
(define failure-position (cut vector-ref <> 3))

(define (make-message-failure m p)
  (vector 'fail 'message (list m) p))
(define (make-expect-failure m p)
  (vector 'fail 'expect (list m) p))
(define (make-unexpect-failure m p)
  (vector 'fail 'unexpect (list m) p))

;; entry point
(define (parse-string parse str)
  (define (error->string err)
    (case (failure-type err)
      ((message)  (failure-message err))
      ((expect)   (failure-message err))
      ((unexpect) (format #f "unexpected: ~a" (failure-message err)))))
  (let1 r (parse (make-string-stream str))
    (if (parse-success? r)
      (semantic-value-finalize! (result-value r))
      (raise (make-condition <parse-error>
               'position (failure-position r)
               'message (error->string r))))))

;;;============================================================
;;; Lazily-constructed string
;;;
(define-class <rope> ()
  ((tree :init-keyword :tree)))

(define (rope->string obj)
  (define (traverse obj)
    (cond ((is-a? obj <rope>)
           (traverse (slot-ref obj 'tree)))
          ((list? obj) (map traverse obj))
          ((string? obj) (display obj))
          ((char? obj) (display obj))
          (else (error "don't know how to write:" obj))))
  (with-output-to-string
   (lambda () (traverse obj))))

(define (make-rope obj)
  (make <rope> :tree obj))

;;;============================================================
;;; Input Stream
;;;

;;(define (make-string-stream str)
;;  (let loop ((str str) (pos 0))
;;    (lambda ()
;;      (if (zero? (string-length str))
;;        (let loop () (values #f pos loop))
;;        (values (string-ref str 0)
;;                pos
;;                (loop (string-drop str 1) (+ pos 1)))))))

(define (make-string-stream str)
  (let loop ((ptr (make-string-pointer str)))
    (lambda ()
      (let ((c (string-pointer-ref ptr))
            (pos (string-pointer-index ptr)))
        (if (eof-object? c)
          (let loop () (values #f pos loop))
          (let1 new-ptr (string-pointer-copy ptr)
            (string-pointer-next! new-ptr)
            (values c pos (loop new-ptr))))))))

(define (stream-position s)
  (values-ref (s) 1))

;;;============================================================
;;; Primitives
;;;
(define ($return val)
  (lambda (s) (make-result val s)))

(define ($fail msg)
  (lambda (s)
    (make-message-failure msg (stream-position s))))

(define ($expect parse msg)
  (lambda (s)
    (let1 r (parse s)
      (if (parse-success? r)
        r
        (make-expect-failure msg (stream-position s))))))

(define ($unexpect msg pos)
  (lambda (s)
    (make-unexpect-failure msg pos)))

;;;============================================================
;;; Error handler
;;;
(define (merge-failure err)
  (let loop ((r '()) (err err) (pos 0))
    (if (null? err)
      (vector 'fail
              (vector-ref (car r) 1)
              (append-map (cut vector-ref <> 2) (reverse! r))
              pos)
      (let1 npos (failure-position (car err))
        (cond ((= pos npos)
               (loop (cons (car err) r) (cdr err) pos))
              ((< pos npos)
               (loop (list (car err)) (cdr err) npos))
              (else (loop r (cdr err) pos)))))))


;;;============================================================
;;; Backtrack control
;;;
(define-syntax $cut
  (syntax-rules ()
    ((_ mark) (set! mark #t))))

;;;==================================================================
;;; Combinators
;;;
(define-syntax $do
  (syntax-rules ()
    (($do :: var clause ...)
     (begin ($cut var) ($do clause ...)))
    (($do ((parse))) parse)
    (($do parse) parse)
    (($do (var parse) clause ...)
     (lambda (s)
       (let1 tmp (parse s)
         (if (parse-success? tmp)
           (let1 var (result-value tmp)
             (($do clause ...) (result-next tmp)))
           tmp))))
    (($do ((parse)) clause ...)
     (lambda (s)
       (let1 tmp (parse s)
         (if (parse-success? tmp)
           (($do clause ...) (result-next tmp))
           tmp))))
    (($do c0 c1 c2 ...)
     ($do (c0) c1 c2 ...))
    (($do . rest)
     (syntax-error "malformed $do binding form:" rest))))

(define-syntax $or
  (syntax-rules (quote)
    (($or 'mark) ($return #t))
    (($or 'mark p0 p1 ...)
     (lambda (s)
       (let1 mark #f
         (let loop ((errors '())
                    (parsers (list p0 p1 ...)))
           (let1 r ((car parsers) s)
             (cond ((parse-success? r) r)
                   ((or mark (null? (cdr parsers)))
                    (if (null? errors)
                      r
                      (merge-failure (reverse! (cons r errors)))))
                   (else
                    (loop (cons r errors) (cdr parsers)))))))))
    (($or) ($return #t))
    (($or p0) p0)
    (($or p0 p1 ...)
     (lambda (s)
       (let loop ((errors '())
                  (parsers (list p0 p1 ...)))
         (let1 r ((car parsers) s)
           (cond ((parse-success? r) r)
                 ((null? (cdr parsers))
                  (if (null? errors)
                    r
                    (merge-failure (reverse! (cons r errors)))))
                 (else
                  (loop (cons r errors) (cdr parsers))))))))))

(define ($seq . parsers)
  (match parsers
    (() ($return #t))
    ((parse) parse)
    ((parse . rest)
     ($do ((parse)) (apply $seq rest)))
    (_ (error "can't be here"))))

(define (%check-min-max min max)
  (when (or (negative? min)
            (and max (> min max)))
    (error "invalid argument:" min max)))

(define ($many parse . args)
  (let-optionals* args ((min 0) (max #f))
    (%check-min-max min max)
    (lambda (s)
      (define (max? count)
        (and max (>= count max)))
      (let loop ((r '()) (s s) (count 0))
        (if (max? count)
          (make-result (reverse! r) s)
          (let1 v (parse s)
            (cond ((parse-success? v)
                   (loop (cons (result-value v) r)
                         (result-next v)
                         (+ count 1)))
                  ((<= min count)
                   (make-result (reverse! r) s))
                  (else v))))))))

(define ($skip-many . args)
  (apply $many args))

(define ($repeat parse n)
  ($many parse n n))

(define ($optional parse)
  ($or parse ($return #f)))

(define ($sep-by parse sep . args)
  (let-optionals* args ((min 0) (max #f))
    (%check-min-max min max)
    (if (and max (zero? max))
      ($return #t)
      (lambda (s)
        (let1 r (parse s)
          (cond ((parse-success? r)
                 (let1 r2 (($many ($do sep parse)
                                  (clamp (- min 1) 0)
                                  (and max (- max 1)))
                           (result-next r))
                   (if (parse-success? r2)
                     (make-result (cons (result-value r) (result-value r2))
                                  (result-next r2))
                     r2)))
                ((zero? min) (make-result #t s))
                (else r)))))))

(define ($alternate parse sep)
  ($do (h parse)
       (t ($many ($do (v1 sep) (v2 parse) ($return (list v1 v2)))))
       ($return (cons h (apply append! t)))))

(define ($end-by parse sep . args)
  (apply $many ($do (v parse) sep ($return v)) args))

(define ($sep-end-by parse sep . args)
  ($do (v (apply $sep-by parse sep args))
       (($optional sep))
       ($return v)))

(define ($count parse n)
  ($many parse n n))

(define ($between open parse close)
  ($do open (v parse) close ($return v)))

(define ($not parse)
  (lambda (s)
    (($or 'grp
          ($do (v parse) :: grp ($unexpect v (stream-position s)))
          ($return #f))
     s)))

(define ($many-till parse end . args)
  (apply $many ($do (($not end)) parse) args))

(define ($chain-left parse op)
  (lambda (st)
    (let1 r (parse st)
      (if (parse-success? r)
        (let loop ((r r))
          (let1 r2 (($do (proc op) (v parse)
                         ($return (proc (result-value r) v)))
                    (result-next r))
            (if (parse-success? r2)
              (loop r2)
              r)))
        r))))

(define ($chain-right parse op)
  (rec (loop st)
    (($do (h parse)
          ($or ($do (proc op)
                    (t loop)
                    ($return (proc h t)))
               ($return h)))
     st)))

(define-syntax $lazy
  (syntax-rules ()
    ((_ parse)
     (lambda args (apply parse args)))))

;;;============================================================
;;; Intermediate structure constructor
;;;
(define ($->rope parse)
  ($do (v parse) ($return (make-rope v))))

(define (semantic-value-finalize! obj)
  (cond ((is-a? obj <rope>) (rope->string obj))
        ((pair? obj)
         (cons (semantic-value-finalize! (car obj))
               (semantic-value-finalize! (cdr obj))))
        (else obj)))

;;;============================================================
;;; String parsers
;;;
(define ($satisfy pred expect)
  (lambda (s)
    (receive (c pos next) (s)
      (if c
        (let1 r (pred c)
          (if r
            (make-result c next)
            (make-expect-failure expect pos)))
        (make-expect-failure expect pos)))))

(define-values ($string $string-ci)
  (let-syntax
      ((expand
        (syntax-rules ()
          ((_ char=)
           (lambda (str)
             (let1 lis (string->list str)
               (lambda (s)
                 (let loop ((r '()) (s s) (lis lis))
                   (if (null? lis)
                     (make-result (make-rope (reverse! r)) s)
                     (receive (c pos next) (s)
                       (if (and c (char= c (car lis)))
                         (loop (cons c r) next (cdr lis))
                         (make-expect-failure str (stream-position s)))))))))))))
    (values (expand char=?)
            (expand char-ci=?))))

(define ($char c)
  ($satisfy (cut char=? c <>) c))

(define ($char-ci c)
  ($satisfy (cut char-ci=? c <>)
            (list->char-set c (char-upcase c) (char-downcase c))))

(define ($one-of charset)
  ($satisfy (cut char-set-contains? charset <>)
            charset))

(define ($none-of charset)
  ($one-of (char-set-complement charset)))

(define (anychar st)
  (receive (c pos next) (st)
    (if c
      (make-result c next)
      (make-expect-failure "character" pos))))

(define-syntax define-char-parser
  (syntax-rules ()
    ((_ proc charset expect)
     (define proc
       ($expect ($one-of charset) expect)))))

(define-char-parser upper    #[A-Z]         "upper case letter")
(define-char-parser lower    #[a-z]         "lower case letter")
(define-char-parser letter   #[A-Za-z]      "letter")
(define-char-parser alphanum #[A-Za-z0-9]   "letter or digit")
(define-char-parser digit    #[0-9]         "digit")
(define-char-parser hexdigit #[0-9A-Fa-f]   "hexadecimal digit")
(define-char-parser newline  #[\n]          "newline")
(define-char-parser tab      #[\t]          "tab")
(define-char-parser space    #[ \v\f\t\r\n] "space")

(define spaces ($->rope ($many space)))

(define eof
  (lambda (s)
    (receive (c pos next) (s)
      (if c
        (make-expect-failure "end of input" pos)
        (make-result #t next)))))

;;============================================================
;; Token Parsers
;;

