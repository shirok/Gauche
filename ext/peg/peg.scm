;;;
;;; peg.scm - Parser Expression Grammar Parser
;;;
;;;   Copyright (c) 2006 Rui Ueyama (rui314@gmail.com)
;;;   Copyright (c) 2008-2019  Shiro Kawai  <shiro@acm.org>
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
  (use gauche.parameter)
  (use text.tree)
  (use util.match)
  (export <parse-error>
          make-peg-parse-error

          peg-run-parser
          peg-parse-string peg-parse-port
          peg-parser->generator ;experimental

          parse-success?
          return-result return-failure
          return-failure/expect return-failure/unexpect
          return-failure/message return-failure/compound
          
          $bind $return $fail $expect $lift $lift* $debug
          $let $let* $try $assert $not
          $or $fold-parsers $fold-parsers-right
          $seq $seq0 $between
          $many $many1 $many_ $many1_ $repeat $repeat_
          $many-till $many-till_
          $optional
          $sep-by $end-by $sep-end-by
          $chain-left $chain-right
          $lazy $parameterize

          $any $eos $.
          
          $symbol $string $string-ci
          $char $one-of $none-of
          $satisfy $match1 $match1*

          $->rope $->string $->symbol rope->string rope-finalize
          )
  )

(select-module parser.peg)

;;;============================================================
;;; How is EBNF represented in the PEG library?
;;;
;;;   A ::= B C
;;;     => (define a ($seq b c))
;;;    If you need values of B and C, use monadic macro $let, or applicative
;;;    combinator $lift
;;;     => (define a ($let ([x b] [y c]) ($return (cons x y))))
;;;     => (define a ($lift cons b c)
;;;
;;;   A :: B | C
;;;     => (define a ($or b c))
;;;     To be precise, this actually mean B / C in PEG; if B fails without
;;;     consuming input, we try C.  But if B ever consumes input, we don't
;;;     backtrack and A fails.  If you want backtracking, use $try as well.
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
;;;     If the result of a is further gathered, it is wasteful to make it
;;;     a string, since it will be thrown away soon.  We have an efficient
;;;     intermediate string representation called rope.  Here you can make
;;;     the result of a as rope:
;;;     => (define a ($->rope ($many b)))
;;;     Later, you can concatenate and convert ropes to a string by
;;;     rope->string.
;;;     If you don't need the values, you can use $many_ instead, which
;;;     is more efficient.
;;;
;;;   A ::= B?
;;;     => (define a ($optional b))
;;;
;;;


;;;  In this module, we provide various constructors of PARSERs,
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
  (type)                                ;fail type
  (objects)                             ;expecting/unexpecting items
  (token))                              ;token that caused error

(define-method write-object ((o <parse-error>) out)
  (format out "#<<parse-error> ~S>" (~ o 'message)))

(define-inline (parse-success? x) (not x))

(define-inline (return-result v s) (values #f v s))
(define-hybrid-syntax return-failure
  (^[t v s] (values t v s))
  ;; If type is given as a literal symbol, we statically check it.
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ ('quote x) v s)
        (unless (memq x '(fail-message fail-expect fail-unexpect fail-compound))
          (error "Invalid failure type; must be one of fail-message, \
                  fail-expect, fail-unexpect or fail-compund, but got: " x))
        (quasirename r
          `(values ',x ,v ,s))]
       [(_ t v s)
        (quasirename r
          `(values ,t ,v ,s))]
       [_ f]))))
(define-inline (return-failure/message v s)  (values 'fail-message v s))
(define-inline (return-failure/expect v s)   (values 'fail-expect v s))
(define-inline (return-failure/unexpect v s) (values 'fail-unexpect v s))
(define-inline (return-failure/compound v s) (values 'fail-compound v s))

(define (make-peg-parse-error type objs pos seq)
  (define (flatten-compound-error objs)
    (append-map (^e (if (eq? (car e) 'fail-compound)
                      (flatten-compound-error (cdr e))
                      (list e)))
                objs))
  (define (analyze-compound-error objs pos)
    (let1 grps (map (^g (cons (caar g) (map cdr g)))
                    (group-collection objs :key car))
      ;; If we have explicit $fail, we just use it.  Other info is
      ;; preserved in objects slot.
      (let ([msgs (assoc-ref grps 'fail-message)]
            [exps (assoc-ref grps 'fail-expect)]
            [unexps (assoc-ref grps 'fail-unexpect)])
        (tree->string
         (if msgs
           (cons (or-concat msgs) (format " at ~s" pos))
           (cons (or-concat (cond-list
                             [exps (compound-exps exps)]
                             [unexps (compound-unexps unexps)]))
                 (format " at ~s" pos)))))))
  (define (or-concat lis)
    (define (rec lis)
      (match lis
        [(x y) `("(",x") or (",y")")]
        [(x . more) `("(",x"), ",@(rec more))]))
    (match lis
      [() '()]
      [(x) `(,x)]
      [xs (rec xs)]))
  (define (compound-exps exps)
    (match (delete-duplicates exps equal?)
      [(x) (format "expecting ~s" x)]
      [(xs ...) (format "expecting one of ~s" xs)]))
  (define (compound-unexps unexps)
    (match (delete-duplicates unexps equal?)
      [(x) (format "not expecting ~s" x)]
      [(xs ...) (format "not expecting any of ~s" xs)]))
  (define (message objs pos nexttok)
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
  (let ([nexttok (if (pair? seq) (car seq) (eof-object))]
        [objs (if (eq? type 'fail-compound)
                (flatten-compound-error objs)
                objs)])
    (make-condition <parse-error>
                    'position pos 'type type 'objects objs 'token nexttok
                    'message (message objs pos nexttok))))

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
(define (peg-parse-string parser str :optional (cont #f))
  (check-arg string? str)
  (receive (r rest) (peg-run-parser parser (x->lseq str))
    (if cont 
      (cont r rest)
      r)))
;; API
(define (peg-parse-port parser port :optional (cont #f))
  (check-arg input-port? port)
  (receive (r rest) (peg-run-parser parser (x->lseq port))
    (if cont
      (cont r rest)
      r)))

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
          (return-result v ss)
          (return-failure/expect msg s)))))

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
          (return-failure r v s1)))))

;; API
;; $lift :: (a,...) -> b, (Parser,..) -> Parser
;; ($lift f parser) == ($let ([x parser]) ($return (f x)))
(define-inline ($lift f . parsers)
  ;; We don't use the straightforward definition (using $let or $bind)
  ;; to reduce closure construction.
  (^s (let accum ([s s] [parsers parsers] [vs '()])
        (if (null? parsers)
          (return-result (apply f (reverse vs)) s)
          (receive [r v s1] ((car parsers) s)
            (if (parse-success? r)
              (accum s1 (cdr parsers) (cons v vs))
              (return-failure r v s1)))))))

;; API
;; Like $lift, but f gets single list argument
(define-inline ($lift* f . parsers)
  (^s (let accum ([s s] [parsers parsers] [vs '()])
        (if (null? parsers)
          (return-result (f (reverse vs)) s)
          (receive [r v s1] ((car parsers) s)
            (if (parse-success? r)
              (accum s1 (cdr parsers) (cons v vs))
              (return-failure r v s1)))))))

;; API
;; For debugging
(define ($debug name parser)
  (^s (format (current-error-port) "#?parser(~a)<~,,,,v:s\n"
              name (debug-print-width) s)
      (receive [r v s] (parser s)
        (debug-print-post (list r v s))
        (values r v s))))

;; API
;; $let (bind ...) body ...
;;   where
;;     bind := (var parser)
;;          |  (parser)
;;          |  parser
;; var's are visible from body ... (but not from parser)
(define-syntax $let
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ (bind ...) body ...)
        (let1 vars&parsers 
            (map (^b (match b
                       [(var parser) `(,var ,(gensym "parser") ,parser)]
                       [(parser) `(,(gensym "_") ,(gensym "parser") ,parser)]
                       [parser `(,(gensym "_") ,(gensym "parser") ,parser)]))
                 bind)
          (quasirename r
            `(let (,@(map (^b `(,(cadr b) ,(caddr b))) vars&parsers))
               ,@(let loop ([vars&parsers vars&parsers])
                   (if (null? vars&parsers)
                     body
                     (match-let1 [(var pvar _) . rest] vars&parsers
                       (quasirename r
                         `(($bind ,pvar (^[,var] ,@(loop rest)))))))))))]
       [_ (error "Malformed $let:" f)]))))

;; API
;; $let* (bind ...) body ...
;;   where
;;     bind := (var parser)
;;          |  (parser)
;;          |  parser
;; var's are visible from subsequent bind and body
(define-syntax $let*
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ () body ...) (quasirename r `(begin ,@body))]
       [(_ ((var parser) bind ...) body ...)
        (quasirename r
          `($bind ,parser (^[,var] ($let* ,bind ,@body))))]
       [(_ ((parser) bind ...) body ...)
        (quasirename r
          `($bind ,parser (^[,(gensym "_")] ($let* ,bind ,@body))))]
       [(_ (parser bind ...) body ...)
        (quasirename r
          `($bind ,parser (^[,(gensym "_")] ($let* ,bind ,@body))))]
       [_ (error "Malformed $let*:" f)]))))

;; API
;; ($parameterize ((param expr) ..) parser ...)
;; Returns a parse that run parser ... while altering the parameter values
;; like parameterize.  The parser ... are run as if in $seq.
;; Suggested by Saito Atsushi
(define-syntax $parameterize
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ ((p e) ...) parser ...)
        (let1 tmp (gensym)
          (quasirename r
            `(let1 ,tmp ($seq ,@parser)
               (^[s] (parameterize (,@(map list p e))
                       (,tmp s))))))]))))

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
;;     ($let ([v (car ps)]) ($fold-parsers proc (proc v seed) (cdr ps)))
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
              (return-failure r1 v1 s1))))))))

;; API
(define ($fold-parsers-right proc seed ps)
  (match ps
    [()       ($return seed)]
    [(p . ps) ($lift proc p ($fold-parsers-right proc seed ps))]))

;; API
;; $seq P ... Pz
;;   Matches P ... Pz sequentially, and returns the result of Pz.
;;   To get all the results of p1, p2, ... in a list, use $lift list p1 p2 ...
(define ($seq . parsers)
  ($fold-parsers (^[v s] v) #f parsers))

;; API
;; $seq0 P0 P ...
;;   Matches P0 P ..., and returns the result of P.
(define ($seq0 parse . followers)
  (apply $lift (^[v . _] v) parse followers))

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
           (if (parse-success? r)
             (return-result v s)
             (return-failure r v s0)))))

;; API
;; $assert parser
;;   Match parser, but never consumes the result.
;;   On success, the value of the parser is returned.
(define-inline ($assert p)
  (^s (receive (r v s1) (p s)
        (values r v s))))

;; API
;; $not parser
;;   Succeeds when the input does not matches parser.  The value is #t.
;;   If the input matches P, unexpected failure results.
;;   Never consumes input.
(define ($not p)
  (^s (receive (r v s1) (p s)
        (if (parse-success? r)
          (return-failure/unexpect v s)
          (return-result #f s)))))

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
;; $many p :optional min max
;; $many_ p :optional min max
;; $many1 p :optional max
;; $many1_ p :optional max
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
                [else (return-failure r v s1)]))))))

(define-inline ($many_ parse :optional (min 0) (max #f))
  (%check-min-max min max)
  (lambda (s)
    (let loop ([s s] [count 0])
      (if (>=? count max)
        (return-result #t s)
        (receive (r v s1) (parse s)
          (cond [(parse-success? r) (loop s1 (+ count 1))]
                [(and (eq? s s1) (<= min count))
                 (return-result #t s1)]
                [else (return-failure r v s1)]))))))

(define-inline ($many1 parse :optional (max #f)) ($many parse 1 max))
(define-inline ($many1_ parse :optional (max #f)) ($many_ parse 1 max))

;; API
;; $repeat p n
;; $repeat_ p n
;;   Exactly n time of P.  Same as ($many p n n)
(define ($repeat parse n) ($many parse n n))
(define ($repeat_ parse n) ($many_ parse n n))

;; API
;; $many-till P E :optional min max
;; $many-till_ P E :optional min max
(define ($many-till parse end . args)
  (apply $many ($seq ($not end) parse) args))
(define ($many-till_ parse end . args)
  (apply $many_ ($seq ($not end) parse) args))

;; API
;; $optional p :optional fallback
;;   Try P.  If not match, use FALLBACK as the value.
;;   Does not backtrack by default; if P may consume some input and
;;   you want to backtrack later, wrap it with $try.
(define ($optional parse :optional (fallback #f))
  ($or parse ($return fallback)))

;; API
;; $sep-by p sep :optional min max
;;   P sparated by SEP, e.g. P SEP P SEP P.  Returns list of values of P.
;;   If SEP consumes input then fails, or the following P fails, then the
;;   entire $sep-by fails.
(define ($sep-by parse sep :optional (min 0) (max #f))
  (define rep
    ($let ([x parse]
           [xs ($many ($seq sep parse)
                      (clamp (- min 1) 0)
                      (and max (- max 1)))])
      ($return (cons x xs))))
  (cond
   [(and max (zero? max)) ($return '())]
   [(> min 0) rep]
   [else ($or rep ($return '()))]))

;; API
;; $end-by p sep :optional min max
;;   Matches repetition of P SEP.  Returns a list of values of P.
;;   This one doesn't set backtrack point, so for example the input is
;;   P SEP P SEP P Q, the entire match fails.
(define ($end-by parse sep . args)
  (apply $many ($try ($let ([v parse] sep) ($return v))) args))

;; API
;; $sep-end-by p sep min max
;;   The last SEP is optional.  The definition is a bit involved
;;   for performance.
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
                           [else (return-failure r. v. s..)]))]
                  [(and (eq? s s.) (<= min count))
                   (return-result (reverse vs) s)]
                  [else (return-failure r v s.)]))))))

;; API
;; $between A B C
;;   Matches A B C, and returns the result of B.
(define ($between open parse close)
  ($let (open [v parse] close) ($return v)))

;; API
;; $chain-left P OP
(define ($chain-left parse op)
  (lambda (st)
    (receive (r v s) (parse st)
      (if (parse-success? r)
        (let loop ([r1 r] [v1 v] [s1 s])
          (receive (r2 v2 s2) (($let ([proc op] [v parse])
                                 ($return (proc v1 v)))
                               s1)
            (if (parse-success? r2)
              (loop r2 v2 s2)
              (return-failure r1 v1 s1))))
        (return-failure r v s)))))

;; API
;; $chain-right P OP
(define ($chain-right parse op)
  (rec (loop s)
    (($let ([h parse])
       ($or ($try ($let ([proc op]
                         [t loop])
                    ($return (proc h t))))
            ($return h)))
     s)))

;; API
;; $satisfy PRED EXPECT [RESULT])
;;   - Returns a parser such that ...
;;   - If the head of input stream satisfies PRED, 
;;     call (RESULT head (PRED head)) and let its result
;;     as the value of successulf parsing.
;;   - Otherwise, returns failure with EXPECT as the expected input.
(define-inline ($satisfy pred expect :optional (result #f))
  (^s (if-let1 v (and (pair? s) (pred (car s)))
        (return-result (if result
                         (result (car s) v) 
                         (car s))
                       (cdr s))
        (return-failure/expect expect s))))

;; API
;; $match1 PATTERN [RESULT]
;; $match1* PATTERN [RESULT]
;;   - Run util.match#match against the input stream.
;;   - $match1 takes one item from stream and see if it matches
;;     with PATTERN.
;;   - $match1* applys PATTERN on the entire input stream.
;;   - If matched, RESULT is evaluated in the environment where pattern variables
;;     in PATTERN are bound, and its result becomes the result value of the
;;     parser.
;;   - If RESULT is omitted, the matched item is returned.

(define-syntax $match1*
  (syntax-rules ()
    [(_ (pat ...) result)
     (lambda (s)
       (match s
         [(pat ... . rest) (return-result result rest)]
         [_ (return-failure/expect (write-to-string '(pat ...)) s)]))]
    [(_ (pat ... . rest) result)
     (lambda (s)
       (match s
         [(pat ... . rest) (return-result result '())] ;rest consumes all
         [_ (return-failure/expect (write-to-string '(pat ... . rest)) s)]))]
    [(_ (pat ...))
     (lambda (s)
       (match s
         [(pat ... . rest) (return-result (take s (length '(pat ...))) rest)]
         [_ (return-faiulre/expect (write-to-string '(pat ...)) s)]))]
    [(_ (pat ... . rest))
     (lambda (s)
       (match s
         [(pat ... . rest) (return-result s '())]
         [_ (return-faiulre/expect (write-to-string '(pat ... . rest)) s)]))]))

(define-syntax $match1
  (syntax-rules ()
    [(_ pat result)
     (lambda (s)
       (if (pair? s)
         (match (car s)
           [pat (return-result result (cdr s))]
           [_ (return-failure/expect (write-to-string 'pat) s)])
         (return-faiulre/expect (write-to-string 'pat) s)))]
    [(_ pat)
     (lambda (s)
       (if (pair? s)
         (match (car s)
           [pat (return-result (car s) (cdr s))]
           [_ (return-failure/expect (write-to-string 'pat) s)])
         (return-faiulre/expect (write-to-string 'pat) s)))]))

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

;; NB: On success, we know the matched input is the same as STR,
;; so we don't need to bother to collect matched chars.
(define-inline ($string str)
  (if (= (string-length str) 1)
    (let1 ch (string-ref str 0)
      (^s
        (if (and (pair? s) (eqv? (car s) ch))
          (return-result str (cdr s))
          (return-failure/expect str s))))
    (let1 lis (string->list str)
      (^[s0]
        (let loop ([s s0] [lis lis])
          (if (null? lis)
            (return-result str s)
            (if (and (pair? s) (eqv? (car s) (car lis)))
              (loop (cdr s) (cdr lis))
              (return-failure/expect str s0))))))))

(define ($string-ci str)
  (let1 lis (string->list str)
    (^[s0]
      (let loop ([r '()] [s s0] [lis lis])
        (if (null? lis)
          (return-result (make-rope (reverse! r)) s)
          (if (and (pair? s)
                   (char? (car s))
                   (char-ci=? (car s) (car lis)))
            (loop (cons (car s) r) (cdr s) (cdr lis))
            (return-failure/expect str s0)))))))
   
(define-inline ($char c)
  (assume-type c <char>) 
  ($satisfy (cut eqv? c <>) c))

(define ($char-ci c)
  (assume-type c <char>) 
  ($satisfy (^x (and (char? x) (char-ci=? c x)))
            (list->char-set c (char-upcase c) (char-downcase c))))

(define ($one-of charset)
  (assume-type charset <char-set>)
  ($satisfy (^x (and (char? x) (char-set-contains? charset x)))
            charset))

(define ($symbol sym) 
  (assume-type sym <symbol>)
  ($seq ($string (symbol->string sym)) ($return sym)))

(define ($none-of charset)
  ($one-of (char-set-complement charset)))

;; Anything except end of stream.
(define-inline ($any)
  (^s (if (pair? s)
        (return-result (car s) (cdr s))
        (return-failure/unexpect "end of input" s))))

(define-inline ($eos)
  (^s (if (pair? s)
        (return-failure/expect "end of input" s)
        (return-result (eof-object) s))))

;; Parse one item---a char, a string or a charset.
(define-inline ($. item)
  (cond
   [(char-set? item) ($one-of item)]
   [(char? item) ($char item)]
   [(string? item) ($string item)]
   [(symbol? item) ($symbol item)]
   [else (error "Bad item: $. requires a char, a char-set, a string, \
                 or a symbol, but got:" item)]))
