;;;
;;; peg.scm - Parser Expression Grammar Parser
;;;
;;;   Copyright (c) 2006 Rui Ueyama (rui314@gmail.com)
;;;   Copyright (c) 2008-2010  Shiro Kawai  <shiro@acm.org>
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
  (use gauche.collection)
  (use util.list)
  (use util.match)
  (export make-peg-stream
          string->peg-stream
          port->peg-stream
          list->peg-stream
          peg-stream-peek!
          peg-stream-position
          <parse-error>
          make-peg-parse-error

          peg-run-parser peg-parse-string peg-parse-port
          $return $fail $expect 
          $do $do* $<< $try $seq $or $fold-parsers $fold-parsers-right
          $many $many1 $skip-many
          $repeat $optional
          $alternate
          $sep-by $end-by $sep-end-by
          $count $between
          $not $many-till $chain-left $chain-right
          $lazy

          $s $c $y
          $string $string-ci 
          $char $one-of $none-of $many-chars
          $satisfy

          anychar upper lower letter alphanum digit
          hexdigit newline tab space spaces eof

          $->rope rope->string rope-finalize
          )
  )

(select-module parser.peg)

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
  (position)                            ;stream position
  (objects))                            ;offending object(s) or messages

(define-method write-object ((o <parse-error>) out)
  (format out "#<<parse-error> ~S>" (ref o 'message)))

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

(define (make-peg-parse-error type objs stream)
  (define (analyze-compound-error objs pos)
    (let1 grps (map (lambda (g) (cons (caar g) (map cdr g)))
                    (group-collection objs :key car))
      (let ((msgs (assoc-ref grps 'fail-message))
            (exps (assoc-ref grps 'fail-expect))
            (unexps (assoc-ref grps 'fail-unexpect)))
        (string-concatenate
         (or-concat (cond-list
                     [exps (compound-exps exps)]
                     [unexps (compound-unexps unexps)]
                     [msgs @ msgs]))))))
  (define (or-concat lis)
    (match lis
      [() '()]
      [(x) `(,x)]
      [(x y) `(,x " or " ,y)]
      [(x . more) `(,x ", " ,@(or-concat more))]))
  (define (compound-exps exps)
    (match exps
      [(x) (format "expecting ~s" x)]
      [(xs ...) (format "expecting one of ~s" xs)]))
  (define (compound-unexps unexps)
    (match unexps
      [(x) (format "not expecting ~s" x)]
      [(xs ...) (format "not expecting any of ~s" xs)]))
  (define (message pos nexttok)
    (case type
      [(fail-message)  (format "~a at ~s" objs pos)] ;objs is a string message
      [(fail-expect)
       (if (char? objs)
         (format "expecting ~s at ~a, but got ~s" objs pos nexttok)
         (format "expecting ~a at ~a, but got ~s" objs pos nexttok))]
      [(fail-unexpect)
       (if (char? objs)
         (format "expecting but ~s at ~a, and got ~s" objs pos nexttok)
         (format "expecting but ~a at ~a, and got ~s" objs pos nexttok))]
      [(fail-compound) (analyze-compound-error objs pos)]
      [else (format "unknown parser error at ~a: ~a" pos objs)]  ;for safety
      ))
  (let ((pos (peg-stream-position stream))
        (nexttok (begin (peg-stream-peek! stream) (car stream))))
    (make-condition <parse-error>
                    'position pos 'objects objs
                    'message (message pos nexttok))))

(define (peg-run-parser parser stream)
  (receive (r v s) (parser stream)
    (if (parse-success? r)
      (rope-finalize v)
      (raise (make-peg-parse-error r v s)))))

;; entry points
(define (peg-parse-string parser str)
  (peg-run-parser parser (string->peg-stream str)))
(define (peg-parse-port parser port)
  (peg-run-parser parser (port->peg-stream port)))

;;;============================================================
;;; Lazily-constructed string
;;;

(define-inline (make-rope obj)
  (cons 'rope obj))

(define-inline (rope? obj)
  (and (pair? obj) (eq? (car obj) 'rope)))

(inline-stub
 (define-cfn rope2string_int (obj p) :: void :static
   (label restart)
   (cond [(SCM_STRINGP obj) (SCM_PUTS obj p)]
         [(SCM_CHARP obj) (SCM_PUTC (SCM_CHAR_VALUE obj) p)]
         [(SCM_PAIRP obj)
          (when (SCM_EQ (SCM_CAR obj) 'rope)
            (set! obj (SCM_CDR obj)) (goto restart))
          (for-each (lambda (elt)
                      (cond [(SCM_STRINGP elt) (SCM_PUTS elt p)]
                            [(SCM_CHARP elt) (SCM_PUTC (SCM_CHAR_VALUE elt) p)]
                            [else (rope2string_int elt p)]))
                    obj)]
         [(not (SCM_NULLP obj))
          (Scm_Error "rope->string: unknown object to write: %S" obj)]))

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
 (define-cfn peg_stream_fini_cc (result (data :: void**)) :static
   (return SCM_FALSE))

 (define-cfn peg_stream_cc (result (data :: void**)) :static
   (let* ([s (SCM_OBJ (aref data 0))]
          [p (SCM_CAR s)]
          [tokcnt :: int (SCM_INT_VALUE (SCM_CDR s))])
     (SCM_SET_CAR s result)
     (SCM_SET_CDR s (Scm_Cons p (SCM_MAKE_INT (+ 1 tokcnt))))
     (cond [(and (SCM_EOFP result) (not (SCM_FALSEP (SCM_CDR p))))
            (Scm_VMPushCC peg_stream_fini_cc NULL 0)
            (return (Scm_VMApply0 (SCM_CDR p)))]
           [else
            (return (SCM_MAKE_BOOL (not (SCM_EOFP result))))])))

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

;; $do  [: args] clause ... body
;; $do* [: args] clause ... body
;;   where
;;     clause := (var parser)
;;            |  (parser)
;;            |  parser
;;
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
    (error "Malformed $do*: at least one clause is required."))
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

;; $<< proc parser ...
;;   == ($do [tmp parser] ... ($return (proc tmp ...)))
(define-macro ($<< proc . parsers)
  (let ((temps (map (lambda (_) (gensym)) parsers)))
    `($do ,@(map list temps parsers) ($return (,proc ,@temps)))))

;; $or p1 p2 ...
;;   Ordered choice.
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

;; $fold-parsers proc seed parsers
;; $fold-parsers-right proc seed parsers
;;   Apply parsers sequentially, passing around seed value.
;;   Note: $fold-parsers can be written much simpler (only shown in
;;   recursion branch):
;;     ($do [v (car ps)] ($fold-parsers proc (proc v seed) (cdr ps)))
;;   but it needs to create closures at parsing time, rather than construction
;;   time.  Interestingly, $fold-parsers-right can be written simply
;;   without this disadvantage.

(define ($fold-parsers proc seed ps)
  (if (null? ps)
    ($return seed)
    (lambda (s)
      (let loop ((s s) (ps ps) (seed seed))
        (if (null? ps)
          (return-result seed s)
          (receive (r1 v1 s1) ((car ps) s)
            (if (parse-success? r1)
              (loop s1 (cdr ps) (proc v1 seed))
              (values r1 v1 s1))))))))

(define ($fold-parsers-right proc seed ps)
  (match ps
    [()       ($return seed)]
    [(p . ps) ($do [v    p]
                   [seed ($fold-parsers-right proc seed ps)]
                   ($return (proc v seed)))]))

;; $seq p1 p2 ...
;;   Match p1, p2 ... sequentially.  On success, returns the semantic
;;   value of the last parser.
(define ($seq . parsers)
  ($fold-parsers (lambda (v s) v) #f parsers))

;; $try parser
;;   Try to match parsers.  If it fails, backtrack to
;;   the starting position of the stream.  So,
;;    ($or ($try a)
;;         ($try b)
;;         ...)
;;   would try a, b, ... even some of them consumes the input.
(define ($try p)
  (lambda (s0)
    (receive (r v s) (p s0)
      (if (not r)
        (return-result v s)
        (return-failure/expect v s0)))))

(define-syntax $lazy
  (syntax-rules ()
    ((_ parse)
     (let ((p (delay parse)))
       (lambda (s) ((force p) s))))))

;; alternative $lazy possibility (need benchmark!)
;(define-syntax $lazy
;  (syntax-rules ()
;    ((_ parse)
;     (letrec ((p (lambda (s) (set! p parse) (p s))))
;       (lambda (s) (p s))))))

;; Utilities
(define (%check-min-max min max)
  (when (or (negative? min)
            (and max (> min max)))
    (error "invalid argument:" min max)))

;; $loop [var parser] ([v0 init0] ...)
;;       :while expr
;;       :until expr
;;       :update expr
;;       :updates [expr ...]
;;       :finish expr
;;
;;   A low-level construct to apply PARSER repeatedly on the input,
;;   updating state values V0 ....
;;   One or more of the keyword args may be omitted.  If provided:
;;     WHILE is evaluated every iteration before applying the parser.
;;        If it returns #f, $loop returns success.
;;     UNTIL is evaluated when the parser fails without consuming
;;        input.  If it returns #t, $loop returns success.
;;        Othewise $loop fails (passing the last failure situation
;;        of the parser).
;;     UPDATE is evaluated every time the parser succeeds.  It must
;;        yield as many results as the state variables, which will be
;;        bound to V0 ... in the next iteration.
;;     UPDATES are like update, but each expr is evaluated separately
;;        to yield the state values.  It's more efficient than UPDATE.
;;     FINISH is called when $loop returns successfully.  Its value
;;        will be the semantic value of $loop.
;;
(define-syntax $loop
  (syntax-rules ()
    [(_ (v parser) ((var init) ...) . xs)
     ($loop%gather xs ($loop $loop%body (v parser) ((var init) ...)))]
    [(_ . other)
     (syntax-error "Malformed $loop: " ($loop . other))]))

(define-syntax $loop%body
  (syntax-rules ()
    [(_ [(v parser) ((var init) ...)] ?update ?while ?until ?finish)
     (lambda (s0)
       (let loop ((s0 s0) (var init) ...)
         (if ?while
           (receive (r v s) (parser s0)
             (cond [(parse-success? r)
                    ($loop%update ?update loop s var ...)]
                   [($loop%until s0 s ?until)
                    (return-result ?finish s)]
                   [else (values r v s)]))
           (return-result ?finish s0))))]))

;; (define-syntax $loop/pred
;;   (syntax-rules ()
;;     [(_ ((var init) ...) pred . xs)
;;      ($loop%gather xs ($loop-pred $loop-pred%body ((var init) ...) pred))]
;;     [(_ . other)
;;      (syntax-error "Malformed $loop-pred: " ($loop-pred . other))]))

;; (define-syntax $loop/pred%body
;;   (syntax-rules ()
;;     [(_ ((var init) ...) tok pred expect ?update ?while ?until ?finish)
;;      (lambda (s0)
;;        (let loop ((s s0) (var init) ...)
;;          (if ?while
;;            (if (peg-stream-peek! s0)
;;              (let1 tok (car s0)
;;                (if pred
;;                  ($loop%update ?update loop s var ...)
;;                    [($loop%until s0 s ?until)
;;                     (return-result ?finish s)]
;;                    [else (values r v s)]))
;;            (return-result ?finish s0))))]))

;; aux macro

;; ($loop%gather restargs (name body parser vars)) 
(define-syntax $loop%gather
  (syntax-rules ()
    [(_ () (name body . fixpart) ?update ?while ?until ?finish)
     (body fixpart ?update ?while ?until ?finish)]
    [(_ (:update u . xs) fix _ w t f)
     ($loop%gather xs fix (#t . u) w t f)]
    [(_ (:updates (u ...) . xs) fix _ w t f)
     ($loop%gather xs fix (#f u ...) w t f)]
    [(_ (:while w . xs) fix u _ t f)
     ($loop%gather xs fix u w t f)]
    [(_ (:until t . xs) fix u w _ f)
     ($loop%gather xs fix u w t f)]
    [(_ (:finish f . xs) fix u w t _)
     ($loop%gather xs fix u w t f)]
    [(_ (other . _) (name . x) u w t f)
     (syntax-errorf "Invalid keyword in ~a: ~s" name other)]
    [(_ xs fix)
     ($loop%gather xs fix #t #t #t #t)]
    ))

(define-syntax $loop%update
  (syntax-rules ()
    [(_ #t loop s . vs)            (loop s . vs)]
    [(_ (#f . us) loop s . vs)     (loop s . us)]
    [(_ (#t . #f) loop s . vs)     (loop s . vs)]
    [(_ (#t . update) loop s)      (begin update (loop s))]
    [(_ (#t . update) loop s v1)   (loop s update)]
    [(_ (#t . update) loop s . vs) (receive vs update (loop s . vs))]
    ))

(define-syntax $loop%until
  (syntax-rules ()
    [(_ s0 s #t)     (eq? s0 s)]
    [(_ s0 s #f)     #f]
    [(_ s0 s expr)   (and (eq? s0 s) expr)]))

;; $count p n
;;   Exactly n times of p.  Returns the list.
(define ($count parse n)
  ($loop [v parse] ([vs '()] [cnt 0])
         :while  (< cnt n)
         :updates [(cons v vs) (+ cnt 1)]
         :until  #f
         :finish (reverse! vs)))

(define ($skip-count parse n)
  ($loop [v parse] ([cnt 0])
         :while (< cnt n)
         :update (+ cnt 1)
         :until #f))

;; $many p &optional min max
;; $many1 p &optional max
(define ($many parse :optional (min 0) (max #f))
  (%check-min-max min max)
  (if (= min 0)
    (if (not max)
      ($loop [v parse] ([vs '()]) :update (cons v vs) :finish (reverse! vs))
      ($loop [v parse] ([vs '()] [cnt 0])
             :while (< cnt max)
             :updates [(cons v vs) (+ cnt 1)]
             :finish (reverse! vs)))
    ($do [xs ($count parse min)]
         [ys ($many parse 0 (and max (- max min)))]
         ($return (append xs ys)))))

(define ($many1 parse :optional (max #f))
  (if max
    ($do [v parse] [vs ($many parse 0 (- max 1))] ($return (cons v vs)))
    ($do [v parse] [vs ($many parse)] ($return (cons v vs)))))

;; $skip-many p &optional min max
;;   Like $many, but does not keep the results.
(define ($skip-many parse :optional (min 0) (max #f))
  (%check-min-max min max)
  (if (= min 0)
    (if (not max)
      ($loop [v parse] ())
      ($loop [v parse] ([cnt 0]) :while (< cnt max) :update (+ cnt 1)))
    ($do [($skip-count parse min)]
         [($skip-many parse 0 (and max (- max min)))]
         ($return #f))))

(define ($skip-many1 parse :optional (max #f))
  (if max
    ($do parse [($skip-many1 parse)] ($return #f))
    ($do parse [($skip-many1 parse 0 (- max 1))] ($return #f))))

(define ($optional parse :optional (fallback #f))
  ($or parse ($return fallback)))

(define ($repeat parse n)
  ($many parse n n))

(define ($sep-by parse sep :optional (min 0) (max #f))
  (define rep
    ($do [x parse]
         [xs ($many ($seq sep parse)
                    (clamp (- min 1) 0)
                    (and max (- max 1)))]
         ($return (cons x xs))))
  (cond
   [(and max (zero? max)) ($return '())]
   [(> min 0) rep]
   [else ($or rep ($return '()))]))

(define ($alternate parse sep)
  ($or ($do [h parse]
            [t ($many ($try ($do [v1 sep] [v2 parse] ($return (list v1 v2)))))]
            ($return (cons h (apply append! t))))
       ($return '())))

(define ($end-by parse sep . args)
  (apply $many ($do [v parse] sep ($return v)) args))

;; $sep-end-by
;;
;;   An unbounded version can be defined pretty concisely:
;;
;;   (define ($sep-end-by parse sep)
;;     (define rec
;;       ($lazy ($or ($do [v0 parse]
;;                        [vs ($or ($seq sep rec) ($return '()))]
;;                        ($return (cons v0 vs)))
;;                   ($return '())))))
;;     rec)
;;
;;   But it can't be easily extended to the bounded version wihtout
;;   sacrificing performance.  

(define ($sep-end-by parse sep :optional (min 0) (max #f))
  (define (bound max)
    ($loop [s&v ($do [v parse]
                     [s ($optional ($do sep ($return #t)))]
                     ($return (cons s v)))]
           ([vs '()] [cont? #t] [cnt 0])
           :while  (and cont? (if max (< cnt max) #t))
           :updates [(cons (cdr s&v) vs)
                     (car s&v)
                     (+ cnt 1)]
           :finish (reverse! vs)))
  
  (%check-min-max min max)
  ;; The fact that the last 'sep' is optional makes things complicated.
  (if (= min 0)
    (bound max)
    ($do [xs ($count ($do [a parse] sep ($return a)) (- min 1))]
         [x  parse]
         [ys ($optional ($seq sep (bound (and max (- max min -1)))) '())]
         ($return (append xs (list x) ys)))))

(define ($between open parse close)
  ($do open [v parse] close ($return v)))

(define ($not parse)
  (lambda (s0)
    (receive (r v s) (parse s0)
      (if r
        (return-result #f s)
        (return-failure/unexpect v s0)))))

(define ($many-till parse end . args)
  (apply $many ($do [($not end)] parse) args))

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

;;;============================================================
;;; Intermediate structure constructor
;;;

;;;============================================================
;;; String parsers
;;;

(define ($->rope parser)   ($<< make-rope parser))
(define ($->string parser) ($<< (.$ rope->string make-rope) parser))
(define ($->symbol parser) ($<< (.$ string->symbol rope->string make-rope) parser))

(define (rope-finalize obj)
  (cond [(rope? obj) (rope->string obj)]
        [(pair? obj)
         (let ((ca (rope-finalize (car obj)))
               (cd (rope-finalize (cdr obj))))
           (if (and (eq? ca (car obj)) (eq? cd (cdr obj)))
             obj
             (cons ca cd)))]
        [else obj]))

(define-values ($string $string-ci)
  (let-syntax
      ([expand
        (syntax-rules ()
          ((_ char=)
           (lambda (str)
             (let1 lis (string->list str)
               (lambda (s0)
                 (let loop ((r '()) (s s0) (lis lis))
                   (if (null? lis)
                     (return-result (make-rope (reverse! r)) s)
                     (if (and (peg-stream-peek! s)
                              (char= (car s) (car lis)))
                       (loop (cons (car s) r) (cdr s) (cdr lis))
                       (return-failure/expect str s0)))))))))])
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

(define ($s x) ($string x))

(define ($c x) ($char x))

(define ($y x)
  ($<< (compose string->symbol rope->string) ($s x)))

;; ($many-chars charset [min [max]]) == ($many ($one-of charset) [min [max]])
;;   with possible optimization.
(define-syntax $many-chars
  (syntax-rules ()
    [(_ parser) ($many ($one-of parser))]
    [(_ parser min) ($many ($one-of parser) min)]
    [(_ parser min max) ($many ($one-of parser) min max)]))

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
(define-char-parser space    #[\s]          "space")

(define spaces ($<< make-rope ($many space)))

(define (eof s)
  (if (peg-stream-peek! s)
    (return-failure/expect "end of input" s)
    (return-result #t (cdr s))))

