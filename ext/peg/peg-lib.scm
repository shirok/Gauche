;;;
;;; peg.scm - Parser Expression Grammar Parser
;;;
;;;   Copyright (c) 2006 Rui Ueyama (rui314@gmail.com)
;;;   Copyright (c) 2008  Shiro Kawai  <shiro@acm.org>
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

(define-module parser.peg
  (use srfi-1)
  (use srfi-13)
  (use srfi-14)
  (use util.match)
  (export make-peg-stream
          string->peg-stream
          port->peg-stream
          list->peg-stream
          peg-stream-peek!
          peg-stream-position
          <parse-error>

          parse-string
          $return $fail $expect 
          $do $do* $try $seq $or $many $many_ $skip-many
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

(select-module parser.peg)

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

(define-condition-type <parse-error> <error> #f
  (position))

(define-method write-object ((o <parse-error>) out)
  (format out "#<<parse-error> ~a ~S>"
          (ref o 'position)
          (ref o 'message)))

(define-inline (parse-success? x) (not x))

(define-macro (return-result value stream)
  `(values #f ,value ,stream))

(define-macro (return-failure/message m s)
  `(values 'fail-message ,m ,s))
(define-macro (return-failure/expect m s)
  `(values 'fail-expect ,m ,s))
(define-macro (return-failure/unexpect m s)
  `(values 'fail-unexpect ,m ,s))
(define-macro (return-failure/compound m s)
  `(values 'fail-compound ,m ,s))

;; entry point
(define (parse-string parse str)
  (define (error->string type objs)
    (case type
      [(fail-message)  objs]
      [(fail-expect)   objs]
      [(fail-unexpect) (format #f "unexpected: ~a" objs)]
      [(fail-compound) (format #f "compound: ~a" objs)]
      [else "???"]))
  (receive (r v s) (parse (string->peg-stream str))
    (if (parse-success? r)
      (semantic-value-finalize! v)
      (raise (make-condition <parse-error>
               'position (peg-stream-position s)
               'message (error->string r v))))))

;;;============================================================
;;; Lazily-constructed string
;;;

(define-inline (make-rope obj)
  (cons 'rope obj))

(define-inline (rope? obj)
  (and (pair? obj) (eq? (car obj) 'rope)))

(inline-stub
 (define-symbol rope "sym_rope")
 
 "static void rope2string_int(ScmObj obj, ScmObj p)
 {
 restart:
   if (SCM_STRINGP(obj)) SCM_PUTS(obj, p);
   else if (SCM_CHARP(obj)) SCM_PUTC(SCM_CHAR_VALUE(obj), p);
   else if (SCM_PAIRP(obj)) {
     if (SCM_EQ(SCM_CAR(obj), sym_rope)) {
       obj = SCM_CDR(obj); goto restart;
     }
     SCM_FOR_EACH(obj, obj) {
       if (SCM_STRINGP(SCM_CAR(obj))) SCM_PUTS(SCM_CAR(obj), p);
       else if (SCM_CHARP(SCM_CAR(obj))) SCM_PUTC(SCM_CHAR_VALUE(SCM_CAR(obj)), p);
       else rope2string_int(SCM_CAR(obj), p);
     }
   } else if (!SCM_NULLP(obj)) {
     Scm_Error(\"rope->string: unknown object to write: %S\", obj);
   }
 }"
 
 (define-cproc rope->string (obj)
   (body <top>
         (let* ((p (Scm_MakeOutputStringPort TRUE)))
           (rope2string_int obj p)
           (result (Scm_GetOutputString (SCM_PORT p) 0)))))
 )

;;;============================================================
;;; Input Stream
;;;

;; For our purpose, generic stream (like util.stream) is too heavy.
;; We use a simpler mechanism.  A peg-stream is a list of tokens,
;; terminated by a special terminator.
;;
;; <peg-stream> : <terminator> | (<token> . <peg-stream>)
;;
;; <terminator> itself is a pair, but its content should be treated
;; as an opaque object.  <terminator> includes a generator that produces
;; a series of tokens.
;;
;; There's no way for users to check whether the given <peg-stream> has
;; a token in its car or not.  However, calling peg-stream-peek! on a
;; peg stream *guarantees* that, after its call, the car of <peg-stream>
;; is a token.  Of course, its cdr is a <peg-stream>.
;; 
;; peg-stream-peek! returns #t if the current token is not #<eof>, and
;; #f if it is.
;;
;; To create a peg-stream you should provide at least a generator procedure.
;; It is called on demand by peg-stream-peek! to produce a token at a time.
;; In string parser a token can just be a character.  Or you can use
;; a separate tokenizer, or even use a general Scheme objects as tokens.
;; A generator must return #<eof> if it reaches the end of the stream.
;;

(inline-stub
 "static ScmObj peg_stream_fini_cc(ScmObj result, void **data)
 {
   ScmObj s = SCM_OBJ(data[0]);
   SCM_RETURN(SCM_FALSE);
 }"

 "static ScmObj peg_stream_cc(ScmObj result, void **data)
 {
   ScmObj s = SCM_OBJ(data[0]);
   ScmObj p = SCM_CAR(s);
   int tokcnt = SCM_INT_VALUE(SCM_CDR(s));
   SCM_SET_CAR(s, result);
   SCM_SET_CDR(s, Scm_Cons(p, SCM_MAKE_INT(1+tokcnt)));
   if (SCM_EOFP(result) && !SCM_FALSEP(SCM_CDR(p))) {
     /* needs to call fini proc. */
     void *data[1];
     data[0] = s;
     Scm_VMPushCC(peg_stream_fini_cc, data, 1);
     SCM_RETURN(Scm_VMApply0(SCM_CDR(p)));
   } else {
     SCM_RETURN(SCM_MAKE_BOOL(!SCM_EOFP(result)));
   }
 }"

 (define-cproc peg-stream-peek! (s)
   (body <top>
         (cond
          [(not (SCM_PAIRP s))
           (Scm_Error "peg-stream required, but got: %S" s)]
          [(not (SCM_INTP (SCM_CDR s)))
           (result (SCM_MAKE_BOOL (not (SCM_EOFP (SCM_CAR s)))))]
          [else
           (let* ((|data[1]| :: void*))
             (set! (aref data 0) s)
             (Scm_VMPushCC peg_stream_cc data 1)
             (result (Scm_VMApply0 (SCM_CAAR s))))])))
 )

;; Create a peg-stream from the given generator.
;; "Args" part and the dispatch is a performance hack to avoid
;; extra closure invocation.
(define (make-peg-stream generator :optional (fini #f))
  `((,generator . ,fini) . 0))

(define (string->peg-stream str)
  (let1 p (open-input-string str :private? #t)
    (make-peg-stream (cut read-char p) (cut close-input-port p))))

;; NB: should we have an option to leave the port open?
(define (port->peg-stream iport :key (reader read-char))
  (make-peg-stream (cut reader iport) (cut close-input-port iport)))

(define (list->peg-stream lis)
  (make-peg-stream (lambda ()
                     (if (pair? lis)
                       (rlet1 v (car lis) (set! lis (cdr lis)))
                       (eof-object)))))

(define (peg-stream-position s)
  (let loop ((s s) (n 0))
    (if (pair? (cdr s))
      (loop (cdr s) (+ n 1))
      (- (cdr s) n))))

;;;============================================================
;;; Primitives
;;;
(define-inline ($return val)
  (lambda (s) (return-result val s)))

(define ($fail msg)
  (lambda (s)
    (return-failure/message msg s)))

(define ($expect parse msg)
  (lambda (s)
    (receive (r v ss) (parse s)
      (if (parse-success? r)
        (values r v ss)
        (return-failure/expect msg s)))))

(define ($unexpect msg s)
  (lambda (s1)
    (return-failure/unexpect msg s)))

;;;============================================================
;;; Combinators
;;;

;; $do and $do*

(define-macro (%gen-do-common)
  '(begin
     ;; an ad-hoc optimization to eliminate a closure in typical cases.
     ;; TODO: instead of literal symbol, we should compare identifiers.
     (define (%gen-body body s)
       (match body
         [('$return x) `(values #f ,x ,s)]
         [_ `(,body ,s)]))))

(define-macro ($do . clauses)

  (define (finish-body s pre-binds var&parsers body)
    `(let ,pre-binds
       (lambda (,s)
         ,(let loop ((s s) (var&parsers var&parsers))
            (match var&parsers
              [() (%gen-body body s)]
              [((var parser) . rest)
               (let ((r1 (gensym)) (s1 (gensym)))
                 `(receive (,r1 ,var ,s1) (,parser ,s)
                    (if ,r1
                      (values ,r1 ,var ,s1)
                      ,(loop s1 rest))))]
              [(parser . rest)
               (let ((r1 (gensym)) (v1 (gensym)) (s1 (gensym)))
                 `(receive (,r1 ,v1 ,s1) (,parser ,s)
                    (if ,r1
                      (values ,r1 ,v1 ,s1)
                      ,(loop s1 rest))))])))))

  (define (parse-do clauses)
    (let loop ((pre-binds   '())
               (var&parsers '())
               (clauses clauses))
      (match clauses
        [(body)
         (finish-body (gensym) (reverse pre-binds) (reverse var&parsers)
                      body)]
        [(clause . rest)
         (match clause
           [(var parser)
            (if (or (symbol? parser) (identifier? parser))
              (loop pre-binds `((,var ,parser) . ,var&parsers) rest)
              (let1 tmp (gensym)
                (loop `((,tmp ,parser) . ,pre-binds)
                      `((,var ,tmp) . ,var&parsers)
                      rest)))]
           [(or (parser) parser)
            (if (or (symbol? parser) (identifier? parser))
              (loop pre-binds `(,parser . ,var&parsers) rest)
              (let1 tmp (gensym)
                (loop `((,tmp ,parser) . ,pre-binds)
                      `(,tmp . ,var&parsers) rest)))])])))  

  (%gen-do-common)

  (when (null? clauses)
    (error "Malformed $do: at least one clause is required."))
  (parse-do clauses))

(define-macro ($do* . clauses)

  (%gen-do-common)

  (when (null? clauses)
    (error "Malformed $do: at least one clause is required."))
  (let1 s (gensym)
    `(lambda (,s)
       ,(let loop ((s s) (clauses clauses))
          (match clauses
            [(body) (%gen-body body s)]
            [(clause . rest)
             (match clause
               [(var parser)
                (let ((r1 (gensym)) (s1 (gensym)))
                  `(receive (,r1 ,var ,s1) (,parser ,s)
                     (if ,r1
                       (values ,r1 ,var ,s1)
                       ,(loop s1 rest))))]
               [(or (parser) parser)
                (let ((r1 (gensym)) (v1 (gensym)) (s1 (gensym)))
                  `(receive (,r1 ,v1 ,s1) (,parser ,s)
                     (if ,r1
                       (values ,r1 ,v1 ,s1)
                       ,(loop s1 rest))))])]))))
  )

(define-macro ($or . parsers)

  (define (parse-or parsers ps binds)
    (match parsers
      [() `(let ,binds ,(finish-or (reverse ps) (reverse binds)))] 
      [((x ...) . parsers)
       (let1 p (gensym)
         (parse-or parsers `(,p ,@ps) `((,p ,x) ,@binds)))]
      [(p . parsers)
       (parse-or parsers `(,p ,@ps) binds)]))

  (define (finish-or ps binds)
    (let ((s0  (gensym))
          (rvss0 (map (lambda (_) `(,(gensym) ,(gensym) ,(gensym))) ps)))
      `(lambda (,s0)
         ,(let loop ((ps ps) (rvss rvss0))
            (match-let1 ((and rvs (r v s)) . rvss) rvss
              `(receive ,rvs (,(car ps) ,s0)
                 (if (and ,r (eq? ,s0 ,s))
                   ,(if (null? (cdr ps))
                      (compose-failure rvss0 s0)
                      (loop (cdr ps) rvss))
                   (values ,r ,v ,s))))))))

  (define (compose-failure rvss0 s0)
    `(values 'fail-compound
             (list ,@(map (match-lambda [(r v s) `(cons ,r ,v)]) rvss0))
             ,s0))

  (if (null? parsers)
    `(cut values #f #t <>)
    (parse-or parsers '() '())))

(define ($try . parsers)
  (match parsers
    [()  ($return #t)]
    [(p) (lambda (s0)
           (receive (r v s) (p s0)
             (if (not r)
               (return-result v s)
               (return-failure/expect v s0))))]
    [ps
     (lambda (s0)
       (let loop ((s s0) (ps ps))
         (receive (r1 v1 s1) ((car ps) s)
           (cond [r1     (values r1 v1 s0)] ; failure/backtrack
                 [(null? (cdr ps)) (values #f v1 s1)] ; success
                 [else (loop s1 (cdr ps))]))))]
    ))

(define ($seq . parsers)
  (match parsers
    [() ($return #t)]
    [(parse) parse]
    [ps
     ;; simple, but inefficient def: ($do ((parse)) (apply $seq rest))
     ;; we expand the loop to avoid extra closure creation.
     (lambda (s)
       (let loop ((s s) (ps ps))
         (if (null? (cdr ps))
           ((car ps) s)
           (receive (r1 v1 s1) ((car ps) s)
             (if (parse-success? r1)
               (loop s1 (cdr ps))
               (values r1 v1 s1))))))]
    ))

(define (%check-min-max min max)
  (when (or (negative? min)
            (and max (> min max)))
    (error "invalid argument:" min max)))

(define ($many parse . args)
  (match args
    [() 
     (lambda (s)
       (let loop ((vs '()) (s s))
         (receive (r1 v1 s1) (parse s)
           (if (parse-success? r1)
             (loop (cons v1 vs) s1)
             (return-result (reverse! vs) s)))))]
    [(min) ($many parse min #f)]
    [(min max)
     (%check-min-max min max)
     (lambda (s)
       (let loop ((vs '()) (s s) (count 0))
         (if (and max (>= count max))
           (return-result (reverse! vs) s)
           (receive (r1 v1 s1) (parse s)
             (cond [(parse-success? r1)
                    (loop (cons v1 vs) s1 (+ count 1))]
                   [(<= min count)
                    (return-result (reverse! vs) s)]
                   [else (values r1 v1 s1)])))))]))

(define ($many_ parse . args)
  (match args
    [()
     (lambda (s)
       (let loop ((s s))
         (receive (r1 v1 s1) (parse s)
           (if (parse-success? r1)
             (loop s1)
             (return-result #t s)))))]
    [(min) ($many parse min #f)]
    [(min max)
     (%check-min-max min max)
     (lambda (s)
       (let loop ((s s) (count 0))
         (if (and max (>= count max))
           (return-result #t s)
           (receive (r1 v1 s1) (parse s)
             (cond [(parse-success? r1)
                    (loop s1 (+ count 1))]
                   [(<= min count)
                    (return-result #t s)]
                   [else (values r1 v1 s1)])))))]))

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
        (receive (r v s) (parse s)
          (cond [(parse-success? r)
                 (receive (r2 v2 s2) (($many ($do sep parse)
                                             (clamp (- min 1) 0)
                                             (and max (- max 1)))
                                      s)
                   (if (parse-success? r2)
                     (return-result (cons v v2) s2)
                     (values r2 v2 s2)))]
                [(zero? min) (return-result #t s)]
                [else (values r v s)]))))))

(define ($alternate parse sep)
  (let ((ts ($many ($do (v1 sep) (v2 parse) ($return (list v1 v2))))))
    ($do (h parse)
         (t ts)
         ($return (cons h (apply append! t))))))

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
  (lambda (s0)
    (receive (r v s) (parse s0)
      (if r
        (return-result #f s)
        (return-failure/unexpect v s0)))))
    
  '(lambda (s)
    (($or 'grp
          ($do (v parse) :: grp ($unexpect v s))
          ($return #f))
     s))

(define ($many-till parse end . args)
  (apply $many ($do (($not end)) parse) args))

(define ($chain-left parse op)
  (lambda (st)
    (receive (r v s) (parse st)
      (if (parse-success? r)
        (let loop ((r1 r) (v1 v) (s1 s))
          (receive (r2 v2 s2) (($do (proc op) (v parse)
                                    ($return (proc v1 v)))
                               s1)
            (if (parse-success? r2)
              (loop r2 v2 s2)
              (values r1 v1 s1))))
        (values r v s)))))

(define ($chain-right parse op)
  (rec (loop s)
    (($do (h parse)
          ($or ($try ($do [proc op]
                          [t loop]
                          ($return (proc h t))))
               ($return h)))
     s)))

(define-syntax $lazy
  (syntax-rules ()
    ((_ parse)
     (let ((p (delay parse)))
       (lambda (s) ((force p) s))))))

;;;============================================================
;;; Intermediate structure constructor
;;;
(define ($->rope parse)
  ($do (v parse) ($return (make-rope v))))

(define (semantic-value-finalize! obj)
  (cond ((rope? obj) (rope->string obj))
        ((pair? obj)
         (cons (semantic-value-finalize! (car obj))
               (semantic-value-finalize! (cdr obj))))
        (else obj)))

;;;============================================================
;;; String parsers
;;;

(define-syntax $satisfy
  (syntax-rules (cut <>)
    [(_ (cut p x <>) expect)            ;TODO: hygiene!
     (lambda (s)
       (if (and (peg-stream-peek! s) (p x (car s)))
         (return-result (car s) (cdr s))
         (return-failure/expect expect s)))]
    [(_ pred expect)
     (lambda (s)
       (if (peg-stream-peek! s)
         (if (pred (car s))
           (return-result (car s) (cdr s))
           (return-failure/expect expect s))
         (return-failure/expect expect s)))]))

(define-values ($string $string-ci)
  (let-syntax
      ([expand
        (syntax-rules ()
          ((_ char=)
           (lambda (str)
             (let1 lis (string->list str)
               (lambda (s)
                 (let loop ((r '()) (s s) (lis lis))
                   (if (null? lis)
                     (return-result (make-rope (reverse! r)) s)
                     (if (and (peg-stream-peek! s)
                              (char= (car s) (car lis)))
                       (loop (cons (car s) r) (cdr s) (cdr lis))
                       (return-failure/expect str s)))))))))])
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

(define (anychar s)
  (if (peg-stream-peek! s)
    (return-result (car s) (cdr s))
    (return-failure/expect "character" s)))

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

(define (eof s)
  (if (peg-stream-peek! s)
    (return-failure/expect "end of input" s)
    (return-result #t (cdr s))))

(provide "parser/peg")
