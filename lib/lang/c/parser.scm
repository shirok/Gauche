;;;
;;; C Parser
;;;
;;;   Copyright (c) 2021  Shiro Kawai  <shiro@acm.org>
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

(define-module lang.c.parser
  (use util.match)
  (use parser.peg)
  (use gauche.process)
  (use gauche.config)
  (use gauche.lazy)
  (use lang.c.lexer)
  (export-all)                          ;for now
  )
(select-module lang.c.parser)

;;;
;;; Parser
;;;

;; Token recognition
(define %identifier ($match1 ('ident _)))
(define %constant   ($match1 ('const . _)))
(define %string-literal ($match1 ((or 'string 'wstring) . _)))

(define %LP ($. '|\(|))
(define %RP ($. '|\)|))
(define %LC ($. '|\{|))
(define %RC ($. '|\}|))
(define %LB ($. '|\[|))
(define %RB ($. '|\]|))

;; typical infix operator parser
;; recognize term sep term sep ... term, returns (op term ...)
;; if ther's no sep, just return the result of term.
(define ($infix term sep op)
  ($lift (^[xs] (match xs
                  [(x0) x0]
                  [xs `(,op ,@xs)]))
         ($sep-by term ($. sep))))

;; multiple choice infix.  assuming sep and op are the same.
(define ($infix-n term associativity seps)
  ($binding ($seq ($: x term)
                  ($: xs ($many ($lift list ($one-of seps) term))))
            (ecase associativity
              [(left)  (fold (^[e r] `(,(car e) ,(cadr e) ,r)) x xs)]
              [(right) (fold-right (^[e r] `(,(car e) ,(cadr e) ,r)) x xs)])))

;; 6.5.1 Primary expressions
(define %primary-expression
  ($lazy ($or %identifier
              %constant
              %string-literal
              ($bindng ($: expr ($between %LP %expression %RP))
                       `(paren ,expr)))))

;; 6.5.2 Postfix operators
(define %postfix-expression
  ($lazy
   ($binding ($or ($: prim %primary-expression)
                  ($seq ($: cast ($between %LP %type-name %RP))
                        ($: init ($between %LC %initializer-list %RC))))
             (if (undefined? prim)
               `(cast ,cast ,init)
               prim))))

;; 6.5.3 Unary operators
;;  sizeof(typename) will be recognized later, for at this stage we can't
;;  separate typename from identifiers.
(define %unary-expression
  ($binding ($seq ($: ops ($many ($one-of '(++ -- + - & * ~ ! sizeof))))
                  ($: main %postfix-expression))
            (fold-right (^[op r] `(,op ,r)) main ops)))

;; 6.5.4 Cast operators
(define %cast-expression
  ($lazy
   ($binding ($seq ($: types ($many ($between %LP %type-name %RP)))
                   ($: main %unary-expression))
             (fold-right (^[type r] `(cast ,type ,r)) main types))))

;; 6.5.5 Multiplicative operators
(define %multiplicative-expression
  ($infix-n %cast-expression 'left '(* / %)))

;; 6.5.6 Additive operators
(define %additive-expression
  ($infix-n %multiplicative-expression 'left '(+ -)))

;; 6.5.7 Bitwise shift operators
(define %shift-expression
  ($infix-n %additive-expression 'left '(<< >>)))

;; 6.5.8 Relational operators
(define %relational-expression
  ($infix-n %shift-expression 'left '(< > <= >=)))

;; 6.5.9 Equality expression
(define %equality-expression
  ($infix-n %relational-expression 'right '(== !=)))

;; 6.5.10 Bitwise AND operator
(define %and-expression
  ($infix %equality-expression '& 'logand))

;; 6.5.11 Bitwise exclusive OR operator
(define %exclusive-or-expression
  ($infix %and-expression '^ 'logxor))

;; 6.5.12 Bitwise inclusive OR operator
(define %inclusive-or-expression
 ($infix %exclusive-or-expression '|\|| 'logior))

;; 6.5.13 Logical AND operator
(define %logical-and-expression
  ($infix %inclusive-or-expression '&& 'and))

;; 6.5.14 Logical OR operator
(define %logical-or-expression
  ($infix %logical-and-expression '|\|\|| 'or))

;; 6.5.15 Conditional operator
(define %conditional-expression
  ($lazy ($binding ($seq ($: main %logical-expression)
                         ($optional ($seq ($. '?)
                                          ($: then %expression)
                                          ($. '|:|)
                                          ($: else %conditional-expression))))
                   (if (undefined? then)
                     main
                     `(?: ,main ,then ,else)))))

;; 6.5.16 Assignment operators
(define %assignment-expression
  ($binding ($seq ($: uexprs ($many ($lift list %unary-expression
                                           ($one-of '(= *= /= %= += -=
                                                        <<= >>= &= ^= |\|=|)))))
                  ($: main %conditional-expression))
            (fold-right (^[uexpr r] `(,(cadr uexpr) ,(car uexpr) ,r))
                        main uexprs)))

;; 6.5.17 Comma operator
(define %expression
  ($binding ($: xs ($sep-by %assignment-expression ($. '|,|)))
            (match xs
              [(x) x]
              [xs `(comma ,@xs)])))

;; 6.6 Constant expression
;;  ... is syntactically equivalent as %conditional-expression,
;;  but all values are constant. We don't check it at the parser level.
(define %constant-expression %conditional-expression)

;; 6.7.1
(define %storage-class-specifier
  ($one-of '(typedef extern static auto register)))

;; 6.7.2 Type specifiers
(define %type-specifier
  ($lazy ($or ($one-of '(void char short int long float double signed unsigned
                              _Book _Complex))
              %struct-or-union-specifier
              %enum-specifier
              %typedef-name)))

;; 6.7.3 Type qualifiers
(define %type-qualifier ($one-of '(const restrict volatile)))

;; 6.7.2.1 Structure and union specifiers
(define %struct-declarator
  ($lazy
   ($binding ($or ($seq ($: decl %declarator)
                        ($: bitfield ($seq ($. '|:|)
                                           %constant-expression)))
                  ($: bitfield ($seq ($. '|:|)
                                     %constant-expression)))
             (if (undefined? bitfield)
               decl
               `(,(if (undefined? decl) #f decl)
                 ,bitfield)))))

(define %struct-declaration
  ($lift list
         ($many1 ($or %type-qualifier %type-specifier))
         ($sep-by %struct-declarator ($. '|,|))))

(define %struct-members
  ($between %LC ($sep-by %struct-declaration ($. '|\;|)) %RC))

(define %struct-or-union-specifier
  ($binding ($seq ($: key ($one-of '(struct union)))
                  ($or ($seq ($: tag %identifier)
                             ($optional ($: mem %struct-members)))
                       ($: mem %struct-members)))
            `(,key (if (undefined? tag) #f tag)
                   ,@(if (undefined? mem) '() mem))))

;; 6.7.2.2 Enumeration specifiers
(define %enumerator
  ($binding ($seq ($: enum %identifier)
                  ($: init ($optional ($seq ($. '=) %constant-expression))))
            (if (undefined? init)
              enum
              `(,enum ,init))))

(define %enumerators
  ($between %LC ($sep-end-by %enumerator ($. '|,|)) %RC))

(define %enum-specifier
  ($binding ($seq ($. 'enum)
                  ($or ($seq ($: tag %identifier)
                             ($optional ($: lis %enumerators)))
                       ($: lis %enumerators)))
            `(enum (if (undefined? tag) #f tag)
                   ,@(if (undefined? lis) '() lis))))

;; 6.7.4 Function specifiers
(define %function-specifier ($. 'inline))

;; 6.7.5 Declarators
(define %declarator
  ($lazy
   ($binding ($seq ($: ptr ($many %pointer))
                   ($: main ($or %identifier
                                 ($between %LP %declarator %RP)))
                   ($: post ($many ($or %array-decl-suffix
                                        %function-decl-suffix))))
             `(,main :: (,@ptr @,post)))))

(define %pointer
  ($lift cons ($. '*) ($many %type-qualifier)))

(define %array-decl-suffix
  ($binding ($seq %LC
                  ($: quals ($many ($or ($. 'static)
                                        %type-qualifier)))
                  ($: assign ($optional ($or ($. '*)
                                             %assignment-expression)))
                  %RC)
            `(array ,quals ,assign)))

(define %parameter-declaration
  ($lazy
   ($binding ($seq ($: spec %declaration-specifiers)
                   ($: decl ($or %declarator
                                 ($optional %abstract-declarator))))
             (if decl
               `(,spec ,decl)
               spec))))

(define %parameter-type-list
  ($binding ($: params ($sep-by ($or %parameter-declaration
                                     ($. '...))
                                ($. '|.|)))
            (=> fail)
            (or (and-let* ([p (memq '... params)]
                           [ (not (null? (cdr p))) ])
                  (fail "'...' appear in middle of parameter list"))
                params)))

(define %function-decl-suffix
  ($binding ($seq %LP
                  ($: params ($or ($try %parameter-type-list)
                                  ($sep-by %identifier ($. '|,|))))
                  %RP)
            `(function ,@params)))

;; 6.7.7 Type definitions
(define %typedef-name %identifier)

;; 6.7 Declarations
(define %declaration
  ($lazy ($binding ($seq ($: specs %declaration-specifiers)
                         ($: decls ($sep-by %init-declarator ($. '|,|))))
                   `(decl ,specs ,decls))))

(define %declaration-specifiers
  ($many1 ($or %storage-class-specifier
               %type-specifier
               %type-qualifier
               %function-specifier)))

(define %init-declarator
  ($lazy
   ($binding ($seq ($: decl %declarator)
                   ($: init ($optional ($seq ($. '=) %initializer))))
             (if init
               `(,decl ,init)
               `(,decl)))))

;; 6.7.6 Type names
(define %abstract-declarator
  ($lazy
   ($binding ($seq ($: ptrs ($many %pointer))
                   ($: decl %direct-abstract-declarator))
             (fold-right (^[p r] `(,p ,r)) decl ptrs))))

(define %direct-abstract-declarator
  ($lazy
   ($binding ($seq ($: paren ($optional $between %LC %abstract-declarator %RC))
                   ($: suff ($many ($or ($try ($seq %LC ($. '*) %RC))
                                        %array-decl-suffix
                                        %function-decl-suffix))))
             ;; for now
             (cons paren suff))))

(define %type-name
  ($binding ($seq ($: specs ($many1 ($or %type-specifier %type-qualifier)))
                  ($: decl ($optional %abstract-declarator)))
            (list specs decl)))

;; 6.7.8 Initialization
(define %designator
  ($or ($lift (^e `(aref ,e)) ($between %LB %constant-expression %RB))
       ($lift (^e `(ref ,e))  ($seq ($. '|.|) %identifier))))

(define %designation
  ($seq0 ($many %designator) ($. '=)))

(define %initializer-list
  ($lazy
   ($sep-end-by ($lift (^[desig init] `(initializer ,init ,desig))
                       ($optional %designation) %initializer)
                ($. '|,|))))

(define %initializer
  ($or %assignment-expression
       ($between %LC %initializer-list %RC)))

;; 6,8 Statements and blocks

(define %statement
  ($lazy ($or %labeled-statement
              %compound-statement
              %expression-statement
              %selection-statement
              %iteration-statement
              %jump-statement)))

;; more to come...


;;;
;;; Preprocessor
;;;

;; NB: We may eventually implement our own preprocessor, for we can do
;; more things (such as caching), but for the time being, we call cpp.
;;
;; gcc exits with non-zero status if output isn't fully read, which happens
;; when PROC throws an error.  We don't want to lose that error, so we set
;; :on-abnormal-exit to :ignore.

(define (call-with-cpp file proc)
  (call-with-input-process `(,(gauche-config "--cc") "-E" ,file) proc
                           :on-abnormal-exit :ignore))

;;;
;;; Driver
;;;

;; For testing
(define (c-tokenize-file file)
  (call-with-cpp file
     (^p ($ lseq->list $ c-tokenize
            $ port->char-lseq/position p
            :source-name file :line-adjusters `((#\# . ,cc1-line-adjuster))))))
