;;;
;;; peg.scm - Parser Expression Grammar Parser
;;;
;;;   Copyright (c) 2006 Rui Ueyama (rui314@gmail.com)
;;;   Copyright (c) 2008-2015  Shiro Kawai  <shiro@acm.org>
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
  (use srfi-13)
  (use srfi-14)
  (use gauche.collection)
  (use gauche.generator)
  (use gauche.lazy)
  (use util.match)
  (export <parse-error>
          make-peg-parse-error

          peg-run-parser
          peg-parse-string peg-parse-port
          peg-parser->generator ;experimental

          parse-success?
          return-result return-failure/expect return-failure/unexpect
          return-failure/message return-failure/compound
          
          $return $fail $expect $lift $lift* $debug
          $fmap ;obsoleted - same as $lift
          $<<   ;obsoleted - same as $lift
          $do $try $seq $or $fold-parsers $fold-parsers-right
          $many $many1 $skip-many $skip-many1
          $repeat $optional
          $alternate
          $sep-by $end-by $sep-end-by
          $count $between $followed-by
          $not $many-till $chain-left $chain-right
          $lazy

          $s $c $y
          $string $string-ci
          $char $one-of $none-of $many-chars
          $satisfy

          anychar upper lower letter alphanum digit
          hexdigit newline tab space spaces eof

          $->rope $->string $->symbol rope->string rope-finalize
          )
  )

(select-module parser.peg)

;;;============================================================
;;; How is EBNF represented in the PEG library?
;;;
;;;   A ::= B C
;;;     => (define a ($seq b c))
;;;    If you need values of B and C, use monadic macro $do, or applicative
;;;    combinator $lift
;;;     => (define a ($do [x b] [y c] ($return (cons x y))))
;;;     => (define a ($lift cons b c)
;;;
;;;   A :: B | C
;;;     => (define a ($or b c))
;;;     To be precise, this actually mean B / C in PEG; if B fails without
;;;     consuming input, we try C.  But if B ever consumes input, we don't
;;;     backtrack and A failes.  If you want backtracking, use $try as well.
;;;     => (define a ($or ($try b) c))
;;;
;;;   A :: B*
;;;     => (define a ($many b))
;;;   A :: B+
;;;     => (define a ($many b 1))
;;;   A ::= B B | B B B
;;;     => (define a ($many b 2 3))
;;;     '$many' gathers the semantic values of B into list.  One common case
;;;     is that you want a string out of gathered values.  $->string is a
;;;     combinator that just translates the semantic value.
;;;     => (define a ($->string ($many b)))
;;;     If the result of a is futher gathered, it is wasteful to make it
;;;     a string, since it will be thrown away soon.  We have an efficient
;;;     intermediate string representation called rope.  Here you can make
;;;     the result of a as rope:
;;;     => (define a ($->rope ($many b)))
;;;     Later, you can concatenate and convert ropes to a string by
;;;     rope->string.
;;;     If you don't need the values, you can use $skip-many instead, which
;;;     is more efficient.
;;;
;;;   A ::= B?
;;;     => (define a ($optional b))
;;;
;;;


;;;  In this module, we provide various consturctors of PARSERs,
;;;  and DRIVER procedures.
;;;
;;;  A PARSER is merely a closure that takes a list of input (most
;;;  likely it's a lazy sequence of characters, but it can be a
;;;  list of anything), and returns three results:
;;;
;;;    Status: #f if parse is successful, or a symbol to indicate
;;;            a kind of error.
;;;    Value: parsed value, or error message.
;;;    Next: a list of next input.
;;;
;;;  That is,
;;;    Parser :: [a] -> (Status, Value, [a])
;;;
;;;  Error status and value
;;;
;;;    status           value
;;;    ------------------------------------------------
;;;    fail-message     string (message)
;;;    fail-expect      string (message), char, char-set
;;;    fail-unexpect    string (message)
;;;    fail-compound    ((Status . Value) ...)
;;;
;;;  A DRIVER is a wrapper to take a parser and an input.
;;;  DRIVER applies the parser on the input, and on success, it returns the
;;;  value and the rest of the input.  On failure, it translates the error
;;;  status into <parser-error> object and raises it.
;;;

;; NB: Currently we're in the transition process of reimplementing this
;; module for better performance *and* cleaner API.  You may see some
;; code that doesn't make sense below, which may be just a remaining
;; of the old code and waiting to be removed.  The whole module is not
;; 'official' yet---do not rely on the the current behavior unless you
;; are experimenting.
;; The rfc.json module is built on top of this module and is serving as
;; one of the benchmark/testbed.  The API of rfc.json is official; though
;; its implementation may be changed as parser.peg is changed.

;;;============================================================
;;; Parse result types
;;;

(define-condition-type <parse-error> <error> #f
  (position)                            ;stream position
  (objects))                            ;offending object(s) or messages

(define-method write-object ((o <parse-error>) out)
  (format out "#<<parse-error> ~S>" (~ o 'message)))

(define-inline (parse-success? x) (not x))

(define-macro (return-result value s)
  `(values #f ,value ,s))

(define-macro (return-failure/message m s)
  `(values 'fail-message ,m ,s))
(define-macro (return-failure/expect m s)
  `(values 'fail-expect ,m ,s))
(define-macro (return-failure/unexpect m s)
  `(values 'fail-unexpect ,m ,s))
(define-macro (return-failure/compound m s)
  `(values 'fail-compound ,m ,s))

(define (make-peg-parse-error type objs pos seq)
  (define (analyze-compound-error objs pos)
    (let1 grps (map (^g (cons (caar g) (map cdr g)))
                    (group-collection objs :key car))
      (let ([msgs (assoc-ref grps 'fail-message)]
            [exps (assoc-ref grps 'fail-expect)]
            [unexps (assoc-ref grps 'fail-unexpect)])
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
  (let1 nexttok (if (pair? seq) (car seq) (eof-object))
    (make-condition <parse-error>
                    'position pos 'objects objs
                    'message (message pos nexttok))))

(define (construct-peg-parser-error r v s s1)
  (make-peg-parse-error r v
                        (let loop ([c 0] [s s])
                          (cond [(eq? s1 s) c]
                                [(null? s) 0] ;!!
                                [else (loop (+ c 1) (cdr s))]))
                        s1))

;; API
;;   Default driver.  Returns parsed value and next stream
(define (peg-run-parser parser s)
  (receive (r v s1) (parser s)
    (if (parse-success? r)
      (values (rope-finalize v) s1)
      (raise (construct-peg-parser-error r v s s1)))))

;; Coerce something to lseq.  accepts generator.
;; We check applicability of x->lseq first, since an object can be both
;; passed to x->lseq and applicable as a thunk, but x->lseq should take
;; precedence.
;; NB: This is not abstract enough---the gory details should be hidden
;; in gauche.lazy module.
(define (%->lseq obj)
  (cond [(eq? (class-of obj) <pair>) obj] ;avoid forcing a lazy pair
        [(applicable? x->generator (class-of obj))
         (generator->lseq (x->generator obj))]
        [(applicable? obj) (generator->lseq obj)]
        [else (error "object cannot be used as a source of PEG parser:" obj)]))

;; API
;;   NB: We can consolidate peg-parse-string and peg-parse-port via
;;   x->generator, but should we?
(define (peg-parse-string parser str)
  (check-arg string? str)
  (values-ref (peg-run-parser parser (x->lseq str)) 0))
;; API
(define (peg-parse-port parser port)
  (check-arg input-port? port)
  (values-ref (peg-run-parser parser (x->lseq port)) 0))

;; API
;;  Returns a generator
(define (peg-parser->generator parser src)
  (let1 s (%->lseq src)
    (^[] (if (null? s)
           (eof-object)
           (receive (r v s1) (parser s)
             (cond [(not (parse-success? r))
                    (raise (construct-peg-parser-error r v s s1))]
                   [(eof-object? v) (set! s '()) v]
                   [else (set! s s1) (rope-finalize v)]))))))

;;;============================================================
;;; Lazily-constructed string
;;;

(define-inline (make-rope obj)
  (cons 'rope obj))

(define-inline (rope? obj)
  (and (pair? obj) (eq? (car obj) 'rope)))

(inline-stub
 (define-cfn rope2string_int (obj p) ::void :static
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
         [(not (or (SCM_NULLP obj) (SCM_FALSEP obj)))
          (Scm_Error "rope->string: unknown object to write: %S" obj)]))

 (define-cproc rope->string (obj)
   (let* ([p (Scm_MakeOutputStringPort TRUE)])
     (rope2string_int obj p)
     (return (Scm_GetOutputString (SCM_PORT p) 0))))
 )

;;;============================================================
;;; Primitives
;;;

(define-inline ($return val) (^s (return-result val s)))

(define ($fail msg) (^s (return-failure/message msg s)))

;; return a parser that tries PARSE.  On success, returns what it
;; returned.  On failure, returns 'fail-expect with MSG.
(define ($expect parse msg)
  (^s (receive (r v ss) (parse s)
        (if (parse-success? r)
          (values r v ss)
          (return-failure/expect msg s)))))

;; a parser that merely returns 'fail-unexpect with MSG.
(define ($unexpect msg s) (^_ (return-failure/unexpect msg s)))


;; A convenience utility to check the upper bound, allowing unlimited
;; upper bound by #f.
(define-inline (>=? count max) (and max (>= count max)))

;;;============================================================
;;; Combinators
;;;

;; API
;; p :: Parser
;; f :: Value -> Parser
;; $bind :: Parser, (Value -> Parser) -> Parser
(define-inline ($bind p f)
  (^s (receive [r v s1] (p s)
        (if (parse-success? r)
          ((f v) s1)
          (values r v s1)))))

;; API
;; $lift :: (a,...) -> b, (Parser,..) -> Parser
;; ($lift f parser) == ($do [x parser] ($return (f x)))
(define-inline ($lift f . parsers)
  ;; We don't use the straightforward definition (using $do or $bind)
  ;; to reduce closure construction.
  (^s (let accum ([s s] [parsers parsers] [vs '()])
        (if (null? parsers)
          (return-result (apply f (reverse vs)) s)
          (receive [r v s1] ((car parsers) s)
            (if (parse-success? r)
              (accum s1 (cdr parsers) (cons v vs))
              (values r v s1)))))))

;; API
;; Like $lift, but f gets single list argument
(define-inline ($lift* f . parsers)
  (^s (let accum ([s s] [parsers parsers] [vs '()])
        (if (null? parsers)
          (return-result (f (reverse vs)) s)
          (receive [r v s1] ((car parsers) s)
            (if (parse-success? r)
              (accum s1 (cdr parsers) (cons v vs))
              (values r v s1)))))))

;; for the backward compatibility - will be dropped by 0.9.5
(define $fmap $lift)
(define $<< $lift)

;; API
;; For debugging
(define ($debug name parser)
  (^s (format (current-error-port) "#?parser(~a)<~,,,,v:s\n"
              name (debug-print-width) s)
      (receive [r v s] (parser s)
        (debug-print-post (list r v s))
        (values r v s))))

;; API
;; $do clause ... body
;;   where
;;     clause := (var parser)
;;            |  (parser)
;;            |  parser
(define-syntax $do
  (syntax-rules ()
    [(_ body) body]
    [(_ [var parser] clause . rest)
     ($bind parser (^[var] ($do clause . rest)))]
    [(_ [parser] clause . rest)
     ($bind parser (^_ ($do clause . rest)))]
    [(_ parser clause . rest)
     ($bind parser (^_ ($do clause . rest)))]
    [(_  . other) (syntax-error "malformed $do" ($do . other))]))

;; API
;; $or p1 p2 ...
;;   Ordered choice.
(define ($or . parsers)
  (define (fail vs s)
    (match vs
      [((r v s)) (values r v s)] ;; no need to create compound error
      [vs        (return-failure/compound (reverse vs) s)]))
  (match parsers
    [()  (^s (return-failure/message "empty $or" s))]
    [(p) p]
    [(ps ...)
     (^s (let loop ([vs '()] [ps ps])
           (if (null? ps)
             (fail vs s)
             (receive (r v s1) ((car ps) s)
               (cond [(parse-success? r) (values r v s1)]
                     [(eq? s s1) (loop (acons r v vs) (cdr ps))]
                     [(null? vs) (values r v s1)]
                     [else (fail (acons r v vs) s1)])))))]))

;; API
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

;; API
(define ($fold-parsers-right proc seed ps)
  (match ps
    [()       ($return seed)]
    [(p . ps) ($do [v    p]
                   [seed ($fold-parsers-right proc seed ps)]
                   ($return (proc v seed)))]))

;; API
;; $seq p1 p2 ...
;;   Match p1, p2 ... sequentially.  On success, returns the semantic
;;   value of the last parser.
;;   To get all the results of p1, p2, ... in a list, use $lift* list p1 p2 ...
(define ($seq . parsers)
  ($fold-parsers (^[v s] v) #f parsers))

;; API
;; $try parser
;;   Try to match parsers.  If it fails, backtrack to
;;   the starting position of the stream.  So,
;;    ($or ($try a)
;;         ($try b)
;;         ...)
;;   would try a, b, ... even some of them consumes the input.
(define-inline ($try p)
  (^[s0] (receive (r v s) (p s0)
           (if (not r)
             (return-result v s)
             (values r v s0)))))

;; API
(define-syntax $lazy
  (syntax-rules ()
    [(_ parse)
     (let ((p (delay parse)))
       (lambda (s) ((force p) s)))]))

;; alternative $lazy possibility (need benchmark!)
;(define-syntax $lazy
;  (syntax-rules ()
;    ((_ parse)
;     (letrec ((p (lambda (s) (set! p parse) (p s))))
;       (lambda (s) (p s))))))

;; Utility
(define (%check-min-max min max)
  (when (or (negative? min)
            (and max (> min max)))
    (error "invalid argument:" min max)))

;; API
;; $count p n
;;   Exactly n times of p.  Returns the list.
(define ($count parse n)
  ($many parse n n))

;; API
;; $skip-count p n
;;   Exactly n times of p, discarding the results but the last.
(define ($skip-count parse n)
  (if (= n 1)
    parse
    ($do parse ($skip-count parse (- n 1)))))

;; API
;; $many p :optional min max
;; $many1 p :optional max
(define-inline ($many parse :optional (min 0) (max #f))
  (%check-min-max min max)
  (lambda (s)
    (let loop ([vs '()] [s s] [count 0])
      (if (>=? count max)
        (return-result (reverse! vs) s)
        (receive (r v s1) (parse s)
          (cond [(parse-success? r) (loop (cons v vs) s1 (+ count 1))]
                [(and (eq? s s1) (<= min count))
                 (return-result (reverse! vs) s1)]
                [else (values r v s1)]))))))

(define ($many1 parse :optional (max #f))
  (if max
    ($do [v parse] [vs ($many parse 0 (- max 1))] ($return (cons v vs)))
    ($do [v parse] [vs ($many parse)] ($return (cons v vs)))))

;; API
;; $skip-many p :optional min max
;; $skip-many1 p :optional max
;;   Like $many, but does not keep the results. Always returns #f.
;;   This should be optimized; we don't need to retain intermediate values
(define ($skip-many parse :optional (min 0) (max #f))
  (%check-min-max min max)
  (if (= min 0)
    ($do [($many parse min max)]
         ($return #f))
    ($do [($skip-count parse min)]
         [($skip-many parse 0 (and max (- max min)))]
         ($return #f))))

(define ($skip-many1 parse :optional (max #f))
  (if max
    ($do parse [($skip-many parse 0 (- max 1))] ($return #f))
    ($do parse [($skip-many parse)] ($return #f))))

;; API
;; $optional p :optional fallback
;;   Try P.  If not match, use FALLBACK as the value.
;;   Does not backtrack by default; if P may consume some input and
;;   you want to backtrack later, wrap it with $try.
(define ($optional parse :optional (fallback #f))
  ($or parse ($return fallback)))

;; API
;; $repeat p n
;;   Exactly n time of P.  Same as ($many p n n)
(define ($repeat parse n)
  ($many parse n n))

;; API
;; $sep-by p sep :optional min max
;;   P sparated by SEP, e.g. P SEP P SEP P.  Returns list of values of P.
;;   If SEP consumes input then fails, or the following P fails, then the
;;   entire $sep-by fails.
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

;; API
;; $alternate p sep
;;   P separated by SEP.  Returns list of values of P.
;;   Unline $sep-by, this one sets backtrack point before each SEP.  So,
;;   for example, $sep-by failes with input P SEP P SEP P SEP Q, but
;;   $alternate returns three results from SEP, leaving SEP Q in the input.
(define ($alternate parse sep)
  ($or ($do [h parse]
            [t ($many ($try ($do [v1 sep] [v2 parse] ($return (list v1 v2)))))]
            ($return (cons h (apply append! t))))
       ($return '())))

;; API
;; $end-by p sep :optional min max
;;   Matches repetition of P SEP.  Returns a list of values of P.
;;   This one doesn't set backtrack point, so for example the input is
;;   P SEP P SEP P Q, the entire match fails.
(define ($end-by parse sep . args)
  (apply $many ($try ($do [v parse] sep ($return v))) args))

;; API
;; $sep-end-by p sep min max
;;   
(define ($sep-end-by parse sep :optional (min 0) (max #f))
  (%check-min-max min max)
  (^s (let loop ([vs '()] [s s] [count 0])
        (if (>=? count max)
          (return-result (reverse vs) s)
          (receive (r v s.) (parse s)
            (cond [(parse-success? r)
                   (receive (r. v. s..) (sep s.)
                     (cond [(parse-success? r.)
                            (loop (cons v vs) s.. (+ count 1))]
                           [(and (eq? s.. s.) (<= min (+ count 1)))
                            (return-result (reverse (cons v vs)) s.)]
                           [else (values r. v. s..)]))]
                  [(and (eq? s s.) (<= min count))
                   (return-result (reverse vs) s)]
                  [else (values r v s.)]))))))

;; API
;; $between A B C
;;   Matches A B C, and returns the result of B.
(define ($between open parse close)
  ($do open [v parse] close ($return v)))

;; API
;; $followed-by P S ...
;;   Matches P S ..., and returns the result of P.
(define ($followed-by parse . followers)
  (apply $lift (^[v . _] v) parse followers))

;; API
;; $not P
;;   Succeeds when the input does not matches P.  The value is #f.
;;   If the input matches P, unexpected failure results.
;;   Unlike other parsers, input may be consumed when this parser succeeds.
(define ($not parse)
  (lambda (s0)
    (receive (r v s) (parse s0)
      (if r
        (return-result #f s)
        (return-failure/unexpect v s0)))))

;; API
;; $many-till P E :optional min max
;;
(define ($many-till parse end . args)
  (apply $many ($do [($not end)] parse) args))

;; API
;; $chain-left P OP
(define ($chain-left parse op)
  (lambda (st)
    (receive (r v s) (parse st)
      (if (parse-success? r)
        (let loop ([r1 r] [v1 v] [s1 s])
          (receive (r2 v2 s2) (($do [proc op] [v parse]
                                    ($return (proc v1 v)))
                               s1)
            (if (parse-success? r2)
              (loop r2 v2 s2)
              (values r1 v1 s1))))
        (values r v s)))))

;; API
;; $chain-right P OP
(define ($chain-right parse op)
  (rec (loop s)
    (($do (h parse)
          ($or ($try ($do [proc op]
                          [t loop]
                          ($return (proc h t))))
               ($return h)))
     s)))

;; API
;; $satisfy
(define-syntax $satisfy
  (syntax-rules (cut <>)
    [(_ (cut p x <>) expect)            ;TODO: hygiene!
     (lambda (s)
       (if (and (pair? s) (p x (car s)))
         (return-result (car s) (cdr s))
         (return-failure/expect expect s)))]
    [(_ pred expect)
     (lambda (s)
       (if (and (pair? s) (pred (car s)))
         (return-result (car s) (cdr s))
         (return-failure/expect expect s)))]))

;;;============================================================
;;; Intermediate structure constructor
;;;

;;;============================================================
;;; String parsers
;;;

(define-inline ($->rope parser . more-parsers)
  (if (null? more-parsers)
    ($lift make-rope parser)
    (apply $lift* make-rope parser more-parsers)))
(define-inline ($->string parser . more-parsers)
  (if (null? more-parsers)
    ($lift ($ rope->string $ make-rope $) parser)
    (apply $lift* ($ rope->string $ make-rope $) parser more-parsers)))
(define-inline ($->symbol parser . more-parsers)
  (if (null? more-parsers)
    ($lift ($ string->symbol $ rope->string $ make-rope $) parser)
    (apply $lift* ($ string->symbol $ rope->string $ make-rope $)
           parser more-parsers)))

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
                     (if (and (pair? s)
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

(define ($y x) ($lift ($ string->symbol $ rope->string $) ($s x)))

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
  (if (pair? s)
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

(define spaces ($lift make-rope ($many space)))

(define (eof s)
  (if (pair? s)
    (return-failure/expect "end of input" s)
    (return-result (eof-object) s)))

