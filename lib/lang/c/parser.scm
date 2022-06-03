;;;
;;; C Parser
;;;
;;;   Copyright (c) 2021-2022  Shiro Kawai  <shiro@acm.org>
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
  (use file.util)
  (use util.match)
  (use parser.peg)
  (use gauche.process)
  (use gauche.config)
  (use gauche.dictionary)
  (use gauche.lazy)
  (use control.cseq)
  (use lang.c.lexer)
  (use lang.c.type)
  (use srfi-13)
  (use lang.c.parameter)

  (export c-tokenize-file
          c-tokenize-string
          c-parse-file
          c-parse-string
          cpp-include-paths             ;re-export from lang.c.parameter
          )
  )
(select-module lang.c.parser)


;;
;; Typename catalog.
;;    symbol -> c-type
;;
;; C syntax is ambiguous that identifier can also be a type name.  We need
;; to track previously declared type names.
;;
(define *default-typedef-table*
  (make-hash-table eq-comparator))

(define (default-typedefs) (hash-table-copy *default-typedef-table*))

(define typedef-names
  (make-parameter (make-stacked-map (default-typedefs))))

(define-syntax $with-scope
  (syntax-rules ()
    [(_ parser)
     ($parameterize ((typedef-names (make-stacked-map
                                     (make-hash-table eq-comparator)
                                     (typedef-names))))
                    parser)]))

;;;
;;; Parser
;;;

;; Token recognition
(define %identifier ($match1 ('ident _)))
(define %constant   ($match1 ('const . _)))
(define %string-literal
  ($lift string-concatenate
         ($many1 ($match1 ((or 'string 'wstring) s) s))))

(define %LP ($. #\( ))
(define %RP ($. #\) ))
(define %LC ($. #\{ ))
(define %RC ($. #\} ))
(define %LB ($. #\[ ))
(define %RB ($. #\] ))

;; typical infix operator parser
;; recognize term sep term sep ... term, returns (op term ...)
;; if ther's no sep, just return the result of term.
(define ($infix term sep op)
  ($binding ($: xs ($sep-by term ($. sep)))
            (=> fail)
            (match xs
              [() (fail (format "at least one expression required for ~s" op))]
              [(x0) x0]
              [xs `(,op ,@xs)])))

;; multiple choice infix.  assuming sep and op are the same.
(define ($infix-n term associativity seps)
  ($binding ($: x term)
            ($: xs ($many ($list ($one-of seps) term)))
            (ecase associativity
              [(left)  (fold (^[e r] `(,(car e) ,(cadr e) ,r)) x xs)]
              [(right) (fold-right (^[e r] `(,(car e) ,(cadr e) ,r)) x xs)])))

;;
;; nonstandard elements
;;

;; __attribute__ extension can appear in various places.
(define %attribute-list-item
  ($lbinding ($: name %identifier)
             ($: params ($optional
                         ($between %LP
                                   ($sep-by %expression ($. '|,|))
                                   %RP)))
             (if params
               `(,name ,@params)
               name)))

(define %attribute-specifier
  ($binding ($. '__attribute__)
            %LP %LP
            ($: attrs ($sep-by %attribute-list-item ($. '|,|)))
            %RP %RP
            `(attribute ,@attrs)))

(define %attribute-specifier-list
  ($many %attribute-specifier))

;; asm can appear as a statement, or a part of declaration.
(define %asm-decl-suffix
  ($binding ($. 'asm)
            %LP ($: s %string-literal) %RP
            `(asm ,s)))

(define %asm-operand
  ($lbinding ($: symbolic-name ($optional ($between %LB %identifier %RB)))
             ($: constraint %string-literal)
             ($: expr ($optional ($between %LP %expression %RP)))
             `(,symbolic-name ,constraint ,expr)))

(define %asm-statement
  ($binding ($. 'asm)
            ($: quals ($many ($one-of '(volatile inline goto))))
            %LP
            ($: template %string-literal)
            ($optional
             ($seq
              ($. '|:|)
              ($: outs ($sep-by %asm-operand ($. '|,|)))
              ($optional
               ($seq
                ($. '|:|)
                ($: ins ($sep-by %asm-operand ($. '|,|)))
                ($optional
                 ($seq
                  ($. '|:|)
                  ($: clobbers ($sep-by %string-literal ($. '|,|)))
                  ($optional
                   ($seq
                    ($. '|:|)
                    ($: gotos ($sep-by %identifier ($. '|,|)))))))))))
            %RP
            ($. '|\;|)
            `(asm ,template
                  ,(if (undefined? outs) '() outs)
                  ,(if (undefined? ins) '() ins)
                  ,(if (undefined? clobbers) '() clobbers)
                  ,(if (undefined? gotos) '() clobbers))))

;; gcc's va_arg expands into __builtin_va_arg and handled by the compiler.
(define %va-arg
  ($lbinding ($match1 ('ident '__builtin_va_arg))
             %LP ($: e %assignment-expression) ($. '|,|) ($: t %type-name) %RP
             `(va-arg ,e ,t)))

;;
;; Standard syntax
;;

;; 6.5.1 Primary expressions
(define %primary-expression
  ($lazy
   ($or %va-arg                   ; gcc specific
        %identifier
        %constant
        %string-literal
        ($binding ($between %LP
                            ($or ($: stmt-expr %compound-statement) ; gcc
                                 ($: expr %expression))
                            %RP)
                  (if (undefined? stmt-expr)
                    `(paren ,expr)
                    `(stmt-expr ,stmt-expr))))))

;; 6.5.2 Postfix operators
(define %postfix-expression
  ($lbinding ($or ($: prim %primary-expression)
                  ($seq ($: cast ($between %LP %type-name %RP))
                        ($: init ($between %LC %initializer-list %RC))))
             ($: pfx ($many %postfix-element))
             (let1 core (if (undefined? prim)
                          `(cast ,cast ,init)
                          prim)
               (fold (^[pfx r] (match pfx
                                 [(op . params) `(,op ,r ,@params)]
                                 [op `(,op ,r)]))
                     core pfx))))

(define %postfix-element
  ($or ($lbinding %LB ($: e %expression) %RB
                  `(aref ,e))
       ($lbinding %LP ($: as %argument-expression-list) %RP
                  `(call ,@as))
       ($binding ($. '|.|) ($: field %identifier)
                 `(ref ,field))
       ($binding ($. '->) ($: field %identifier)
                 `(-> ,field))
       ($seq ($. '++) ($return 'post++))
       ($seq ($. '--) ($return 'post--))))

(define %argument-expression-list
  ($lazy ($sep-by %assignment-expression ($. '|,|))))

;; 6.5.3 Unary operators
(define %unary-expression
  ($or ($try ($lbinding ($: op ($one-of '(sizeof __alignof__)))
                        %LP
                        ($: t %type-name)
                        %RP
                        `(,op ,t)))
       ($lbinding ($: op ($one-of '(& * + - ~ !)))
                  ($: main %cast-expression)
                  `(,op ,main))
       ($binding ($: ops ($many ($one-of '(++ -- sizeof))))
                 ($: main %postfix-expression)
                 (fold-right (^[op r] `(,op ,r)) main ops))
       ($binding ($. '&&)               ;gcc label as value
                 ($: label %identifier)
                 `(&& ,label))))

;; 6.5.4 Cast operators
(define %cast-expression
  ($lbinding ($: types ($many ($try ($between %LP %type-name %RP))))
             ($: main %unary-expression)
             (fold-right (^[type r] `(cast ,type ,r)) main types)))

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
  ($lbinding ($: main %logical-or-expression)
             ($optional ($seq ($. '?)
                              ($: then %expression)
                              ($. '|:|)
                              ($: else %conditional-expression)))
             (if (undefined? then)
               main
               `(?: ,main ,then ,else))))

;; 6.5.16 Assignment operators
(define %assignment-expression
  ($binding ($: uexprs ($many
                        ($try ($list %unary-expression
                                     ($one-of '(= *= /= %= += -=
                                                  <<= >>= &= ^= |\|=|))))))
            ($: main %conditional-expression)
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
(define-constant *storage-classes* '(typedef extern static auto register))

(define %storage-class-specifier ($one-of *storage-classes*))

;; 6.7.2 Type specifiers
;; NB: Typedef-name is removed to be distinguished from identifiers.  It is
;; handled in declaration-specifier and type-specifier-qualifier-list.
(define %type-specifier
  ($lazy ($or ($one-of '(void char short int long float double signed unsigned
                         _Bool _Complex
                         ;; gcc/clang extension
                         __builtin_va_list
                         ;; gcc additional floating types
                         __float128 _Float128 __float80 _Float64x __ibm128))
              %struct-or-union-specifier
              %enum-specifier)))

;; 6.7.3 Type qualifiers
(define %type-qualifier
  ($one-of '(const volatile restrict
                   ;; clang additional qualifiers
                   _Nullable _Nonnull _Null_unspecified _Nullable_result)))

;; This is a custom rule specially handle typedef-name.  typedef-name and
;; other type-specifiers are mutually exclusive.
(define %type-specifier-qualifier-list
  ($or ($try ($lbinding ($: pre ($many %type-qualifier))
                        ($: type %typedef-name)
                        ($: post ($many %type-qualifier))
                        (append pre (list type) post)))
       ($many1 ($or %type-qualifier %type-specifier))))

;; 6.7.2.1 Structure and union specifiers
(define %struct-declarator
  ($lbinding ($or ($seq ($: decl %declarator)
                        ($optional ($: bitfield ($seq ($. '|:|)
                                                      %constant-expression))))
                  ($: bitfield ($seq ($. '|:|)
                                     %constant-expression)))
            (if (undefined? bitfield)
              decl
              `(bitfield ,(if (undefined? decl) #f decl) ,bitfield))))

;; modified to handle typedef-name.  see %type-specifier above.
(define %struct-declaration
  ($list* %type-specifier-qualifier-list
          ($sep-by %struct-declarator ($. '|,|))))

(define %struct-members
  ($between %LC ($end-by %struct-declaration ($. '|\;|)) %RC))

(define %struct-or-union-specifier
  ($binding ($: key ($one-of '(struct union)))
            ($or ($seq ($: tag %identifier)
                       ($optional ($: mem %struct-members)))
                 ($: mem %struct-members))
            (=> fail)
            ;; need to catch error from grok-c-type
            (guard (e [else (fail 'error (~ e'message))])
              ;; mem := (quals&spec decl ...)
              (let* ([tdict (typedef-names)]
                     [member-decls
                      (append-map
                       (^m (map (cut grok-member-declaration (car m) <>)
                                (cdr m)))
                       (if (undefined? mem) '() mem))])
                `(,key ,(if (undefined? tag) #f tag)
                       ,@member-decls)))))

(define (grok-member-declaration specs decl)
  (match decl
    [('bitfield #f size) `(#f (int ()) ,size)] ;padding
    [('bitfield d size)  (match-let1 (id sorage type init)
                             (grok-declaration specs `(,d))
                           `(,id ,type ,size))]
    [d (match-let1 (id sorage type init)
           (grok-declaration specs `(,d))
         `(,id ,type))]))

;; 6.7.2.2 Enumeration specifiers
(define %enumerator
  ($binding ($: enum %identifier)
            ($: init ($optional ($seq ($. '=) %constant-expression)))
            `(,(cadr enum) ,init)))

(define %enumerators
  ($between %LC ($sep-end-by %enumerator ($. '|,|)) %RC))

;; returns (enum <tag> ((<symbol> <init-expr>) ...))
(define %enum-specifier
  ($binding ($. 'enum)
            ($or ($seq ($: tag %identifier)
                       ($optional ($: lis %enumerators)))
                 ($: lis %enumerators))
            `(enum ,(if (undefined? tag) #f tag)
                   ,@(if (undefined? lis) '() lis))))

;; 6.7.4 Function specifiers
(define %function-specifier ($. 'inline))

;; 6.7.5 Declarators
;;   Actual type of the identifier can't be determined until we see the
;;   entire declaration.  A declarator simply returns a list:
;;     (<identifier> <type-spec> ...)
;;   where each type-spec can be
;;     (* qualifier ...)
;;     (array-dimension (qualifier ...) dim-expr)
;;     (function params)  ; params := (param-spec ...) | no-args | unknown-args
;;   This is a transient information.  Declaration constructs a complete
;;   type.
(define %declarator
  ($lbinding ($: ptr ($many %pointer))
             ($: main ($or %identifier
                           ($between %LP %declarator %RP)))
             ($: post ($many ($or %array-decl-suffix
                                  %function-decl-suffix
                                  %asm-decl-suffix
                                  %attribute-specifier)))
             (match main
               [('ident id) `((ident ,id) ,@post ,@ptr)]
               [(('ident id) . specs) `((ident ,id) ,@ptr ,@specs ,@post)]
               [else (errorf "something wrong with %declarator.  main=~s"
                             main)])))

(define %pointer
  ($lift cons ($. '*) ($many ($or %type-qualifier %attribute-specifier))))

(define %array-decl-suffix
  ($binding %LB
            ($: quals ($many ($or ($. 'static)
                                  %type-qualifier
                                  %attribute-specifier)))
            ($: assign ($optional ($or ($. '*)
                                       %assignment-expression)))
            %RB
            `(array-dimension ,quals ,assign)))

;; For parameter declaration, we only return (<name> <type>),
;; where <name> can be an ident or #f.
(define %parameter-declaration
  ($lbinding ($: specs %declaration-specifiers)
             ($: decl  ($or ($try %declarator)
                            ($optional %abstract-declarator)))
             (=> fail)
             ;; need to catch error from grok-c-type
             (guard (e [else (fail 'error (~ e'message))])
               (match-let1 (id storage type init)
                   (if decl
                     (grok-declaration specs `(,decl))
                     (grok-declaration specs '((#f))))
                 `(,id ,type)))))

(define %parameter-type-list
  ;; NB: We require at least one %parameter-declaration, for the empty parameter
  ;; case will be handled by the next branch in %function-decl-suffix.
  ($binding ($: params ($sep-by ($or %parameter-declaration
                                     ($. '...))
                                ($. '|,|)
                                1))
            (=> fail)
            (or (and-let* ([p (memq '... params)]
                           [ (not (null? (cdr p))) ])
                  (fail 'error "'...' appear in middle of parameter list"))
                (and (member '(#f (void ())) params)
                     (if (length=? params 1)
                       'no-args         ; (void) means no parameters
                       (fail 'error "void in parameter list must appear by itself")))
                params)))

(define %function-decl-suffix
  ($binding %LP
            ($: params ($or ($try %parameter-type-list)
                            ($sep-by %identifier ($. '|,|))))
            %RP
            (if (null? params)
              `(function unknown-args)
              `(function ,params))))

;; 6.7.7 Type definitions
;;  The semantic value of typedef name is the same as identifier.
(define %typedef-name
  ($try ($binding ($: id %identifier)
                  (=> fail)
                  (match-let1 ('ident x) id
                    (if (dict-exists? (typedef-names) x)
                      `(ident ,x)
                      (fail "typedef name"))))))

;; 6.7 Declarations
;;   Returns (decl (identifier storage-class type init) ...)
;;   The type part is constructed by grok-declaration
(define %declaration
  ($lbinding ($: specs %declaration-specifiers)
             ($: declarators ($sep-by %init-declarator ($. '|,|)))
             %attribute-specifier-list  ;ignore for now
             ($. '|\;|)
             (=> fail)
             ;; need to catch error from grok-c-type
             (guard (e [else (fail 'error (~ e'message))])
               (let ([decls (map (cut grok-declaration specs <>) declarators)])
                 (when (memq 'typedef specs)
                   (map (^d (register-typedefs! specs d)) decls))
                 `(decl ,@decls)))))

(define (register-typedefs! specs decl)
  (match decl
    [((? symbol? typename) 'typedef type _)
     (dict-put! (typedef-names) typename type)]
    [else
     (error "something wrong with typedef decl" decl)]))

(define (grok-declaration specs decl)
  (define (build-decl id type init)
    (let* ([sc (find (cut memq <> *storage-classes*) specs)]
           [ty (delete sc specs)])
      `(,id ,sc ,(grok-c-type `(,@type ,ty) (typedef-names)) ,init)))
  (match decl
    [((('ident id) . type) init) (build-decl id type init)]
    [((('ident id) . type))      (build-decl id type #f)]
    [((#f . type))               (build-decl #f type #f)]))  ; abstract-decl

;; modified to handle typedef-name.  see %type-specifier above.
(define %declaration-specifiers
  ($or ($try ($binding ($: pre ($many ($or %storage-class-specifier
                                           %type-qualifier
                                           %function-specifier
                                           %attribute-specifier)))
                       ($: type %typedef-name)
                       ($: post ($many ($or %storage-class-specifier
                                            %type-qualifier
                                            %function-specifier
                                            %attribute-specifier)))
                       (append pre (list type) post)))
       ($many1 ($or %storage-class-specifier
                    %type-specifier
                    %type-qualifier
                    %function-specifier
                    %attribute-specifier))))

(define %init-declarator
  ($lbinding ($: decl %declarator)
             ($: init ($optional ($seq ($. '=) %initializer)))
             (if init
               `(,decl ,init)
               `(,decl))))

;; 6.7.6 Type names

;; Value of %abstract-declarator is similar to %declarator, except that
;; it has #f in place of the identifier.  It's an intermediate value and
;; eventually parsed by grok-declaration.
;;   (#f <type-spec> ...)
(define %abstract-declarator
  ($lbinding ($: ptr ($many %pointer))
             ($: main ($optional ($between %LP %abstract-declarator %RP)))
             ($: suff ($many ($or ($try ($between %LB ($. '*) %RB))
                                  %array-decl-suffix
                                  %function-decl-suffix)))
             (if main
               (let ((main-specs (cdr main)))
                 `(#f ,@suff ,@ptr ,@main-specs))
               `(#f ,@suff ,@ptr))))

(define %type-name
  ($binding ($: specs %type-specifier-qualifier-list)
            ($: decl ($optional %abstract-declarator))
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
;;  we need $try for %labeled-statement, for it may consume the first
;;  identifier before failing.
(define %statement
  ($lazy ($cut ($or %compound-statement
                    %selection-statement
                    %iteration-statement
                    %jump-statement
                    %asm-statement            ; gcc specific
                    ($try %labeled-statement)
                    %expression-statement))))

;; 6.8.1 Labeled statement
(define %labeled-statement
  ($or ($binding ($. 'case) ($: expr %constant-expression) ($. ':)
                 ($: stmt %statement)
                 `(case ,expr ,stmt))
       ($binding ($. 'default) ($. ':) ($: stmt %statement)
                 `(default ,stmt))
       ($binding ($: label %identifier) ($. ':) ($: stmt %statement)
                 `(label ,label ,stmt))))

;; 6.8.2 Compound statement
(define %compound-statement
  ($binding %LC
            ($with-scope
             ($: stmts ($many ($or %declaration %statement))))
            %RC
            `(begin ,stmts)))

;; 6.8.3 Expression and null statements
(define %expression-statement
  ($binding ($: expr ($optional %expression))
            %attribute-specifier-list   ;for now, we ignore this
            ($. '|\;|)
            (or expr '(begin))))

;; 6.8.4 Selection statements
(define %selection-statement
  ($or ($binding ($. 'if) %LP ($: test %expression) %RP ($: then %statement)
                 ($optional ($seq ($. 'else) ($: else %statement)))
                 (if (undefined? else)
                   `(if ,test ,then)
                   `(if ,test ,then ,else)))
       ($binding ($. 'switch) %LP ($: test %expression) %RP ($: stmt %statement)
                 `(switch ,test ,stmt))))

;; 6.8.5 Iteration statements
(define %iteration-statement
  ($or ($binding ($. 'while) %LP ($: test %expression) %RP ($: body %statement)
                 `(while ,test ,body))
       ($binding ($. 'do) ($: body %statement)
                 ($. 'while) %LP ($: test %expression) %RP ($. '|\;|)
                 `(do-while ,test ,body))
       ($binding ($. 'for) %LP
                 ($or ($: decl %declaration)
                      ($seq ($: init ($optional %expression))
                            ($. '|\;|)))
                 ($: test ($optional %expression))
                 ($. '|\;|)
                 ($: update ($optional %expression))
                 %RP
                 ($: body %statement)
                 (if (undefined? init)
                   `(for (,decl ,test ,update) ,body)
                   `(for (,init ,test ,update) ,body)))))

;; 6.8.6 Jump statement
(define %jump-statement
  ($or ($binding ($. 'goto)
                 ($: dest ($or %identifier
                               ($list ($. '*) %expression))) ;gcc computed goto
                 ($. '|\;|)
                 `(goto ,dest))
       ($seq ($. 'continue) ($. '|\;|) ($return 'continue))
       ($seq ($. 'break) ($. '|\;|) ($return 'break))
       ($binding ($. 'return) ($: expr ($optional %expression)) ($. '|\;|)
                 `(return ,@(if expr `(,expr) '())))))

;; 6.9 External definitions
(define %external-declaration
  ($lazy ($or ($try %function-definition) %declaration)))

(define %translation-unit ($many1 %external-declaration))

;; 6.9.1 Function definitions
(define %function-definition
  ($binding ($: spec %declaration-specifiers)
            ($: decl %declarator)
            ($: lis ($many %declaration))
            ($assert ($. #\{))
            ($: body ($cut %compound-statement))
            `(,spec ,decl ,lis ,body)))

;;;
;;; Preprocessor
;;;

;; NB: We may eventually implement our own preprocessor, for we can do
;; more things (such as caching), but for the time being, we call cpp.
;;
;; gcc exits with non-zero status if output isn't fully read, which happens
;; when PROC throws an error.  We don't want to lose that error, so we set
;; :on-abnormal-exit to :ignore.
;;
;; INCLUDES is a list of include paths, or #f to use the system default
;; (gauche-config --incdirs).  DEFS is an list of (var val ...) which will
;; be used as -Dvar=val -Dvar=val ..., or just var for -Dvar.

(define (call-with-cpp file proc
                       :key (includes #f)
                            (defs '()))
  (define Is
    (cond [(list? includes)
           (map (^d (shell-escape-string #"-I~d")) includes)]
          [(not includes)
           (map (^d #"-I~d")
                (string-split (gauche-config "--incdirs") #\:))]
          [else (error "includes keyword arg must be a list of \
                        directories or #f, but got:" includes)]))
  (define Ds
    (append-map (^[def]
                  (match def
                    [(var val ...)
                     (map (^[val] (shell-escape-string #"-D~|var|=~val")) val)]
                    [var `(,#"-D~var")]))
                defs))
  (call-with-input-process `(,(gauche-config "--cc") "-E" ,@Is ,@Ds ,file)
    proc
    :on-abnormal-exit :ignore))

;;;
;;; Driver
;;;

;; For testing - won't be official APIs.

(define (file-check file)
  (unless (file-is-readable? file)
    (error "file does not exist or unreadable:" file)))

(define (c-tokenize-file file)
  (file-check file)
  (call-with-cpp file
     (^p ($ lseq->list $ c-tokenize
            $ port->char-lseq/position p
            :source-name file :line-adjusters `((#\# . ,cc1-line-adjuster))))
     :includes (cpp-include-paths)
     :defs (cpp-definitions)))

(define (c-tokenize-file-coroutine file)
  (^[yield]
    ($ call-with-cpp file
       (^p
        (let loop ([tokens ($ c-tokenize
                              $ port->char-lseq/position p
                              :source-name file
                              :line-adjusters `((#\# . ,cc1-line-adjuster)))])
          (unless (null? tokens)
            (yield (car tokens)
                   (pair-attributes tokens))
            (loop (cdr tokens)))))
       :includes (cpp-include-paths)
       :defs (cpp-definitions))))

(define (c-parse-file file :optional (parser %translation-unit))
  (file-check file)
  (parameterize ((typedef-names (default-typedefs)))
    (if (use-concurrent-lexer)
      (peg-run-parser parser (coroutine->cseq (c-tokenize-file-coroutine file)))
      (peg-run-parser parser (c-tokenize-file file)))))

(define (%operate-on-string string proc)
  (let1 f (format "x~8,'0x.c" (receive (s us) (sys-gettimeofday)
                                (modulo (* s us) 99999989)))
    (with-output-to-file f (cut display string))
    (unwind-protect
        (proc f)
      (sys-remove f))))

(define (c-tokenize-string string)
  (%operate-on-string string c-tokenize-file))

(define (c-parse-string string :optional (parser %translation-unit))
  (%operate-on-string string (^f (c-parse-file f parser))))
