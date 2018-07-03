;;;
;;; compile.scm - The compiler
;;;
;;;   Copyright (c) 2004-2018  Shiro Kawai  <shiro@acm.org>
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

(declare) ;; a dummy form to suppress generation of "sci" file

(define-module gauche.internal
  (use util.match)
  )
(select-module gauche.internal)

(inline-stub
 (declcode
  (.include <gauche/class.h>
            <gauche/code.h>
            <gauche/vminsn.h>
            <gauche/priv/macroP.h>
            <gauche/priv/moduleP.h>
            <gauche/priv/identifierP.h>
            <gauche/priv/builtin-syms.h>)))

;;; THE COMPILER
;;;
;;;   The main entry point is COMPILE, defined under "Entry point" section.
;;;
;;;     compile :: Sexpr, Module -> CompiledCode
;;;
;;;   Gauche compiles programs at runtime, so we don't want to spend too
;;;   much time in compilation, while we still want to generate as efficient
;;;   code as possible.
;;;
;;; Structure of the compiler
;;;
;;;   We have several passes, outlined here.  See the header of each
;;;   section for the details.
;;;
;;;   Note: Source of each pass is in a separate file compile-P.scm, where
;;;   P is the pass number.
;;;
;;;   Pass 1 (Parsing):
;;;     - Converts Sexpr into an intermediate form (IForm).
;;;     - Macros and global inlinable functions are expanded.
;;;     - Global constant variables are substituted to its value.
;;;     - Variable bindings are resolved.  Local variables are marked
;;;       according to its usage (# of reference count and set count).
;;;     - Constant expressons are folded.
;;;
;;;   Pass 2 (Variable and closure optimization):
;;;     - Traverses IForm and modify the tree to optimize it.
;;;     - Limited beta-substitution (local variable substitution and
;;;       inline local functions for the obvious cases).
;;;     - Analyze local function usage and optimize away closures whenever
;;;       possible.  For example, local tail recursions are converted to
;;;       loops here.
;;;     - NB: This pass can make IForm a directed graph, possibly contains
;;;       cycles.
;;;
;;;   Pass 3 (Constant folding and branch & call optimization):
;;;     - Precompute compile-time constants.  This is here because Pass2's
;;;       inlining and substitution allow further precomputation.
;;;     - Certain $IF nodes are modified for less jump instructions
;;;       (see pass3/$IF)
;;;     - Eliminates redundant $LET and $SEQs introduced by constant folding.
;;;     - Look into $CALL nodes for further optimization; the above
;;;       optimizations may allow further inlining.
;;;
;;;   Pass 4 (Lambda lifting)
;;;     - At this point, remaining $LAMBDA nodes are the ones we absolutely
;;;       needed.  For every $LAMBDA node we find free local variables;
;;;       the lvars introduced outside of the $LAMBDA.  The result is set
;;;       in $lambda-free-lvars slot.  Then we determine $LAMBDA nodes
;;;       that does not need to form a closure.  We introduce a toplevel
;;;       letrec that binds those lifted lambdas.
;;;
;;;   Pass 5 (Code generation):
;;;     - Traverses IForm and generate VM instructions.
;;;     - Perform instruction combining.
;;;     - Perform simple-minded jump optimization.
;;;     - Some final constant foldings are done here, too.
;;;     - NB: This pass modifies $LABEL node while traversing IForm:
;;;       $label-label is set to the label number.

(include "compile-0")                   ; common macros

;;=====================================================================
;; Compile-time constants
;;

;; Used by env-lookup-int.  the frame is marked as either LEXICAL or SYNTAX.
(eval-when (:compile-toplevel)
  (define-constant LEXICAL 0)
  (define-constant SYNTAX  1))

;; Max # of argument passed *literally*.  This is limited by
;; VM stack size, for we need to expand args into stack.
;; Note that if arguments are 'apply'ed, usually we don't have
;; this limit since most of those args are not expanded but passed
;; to the function as the rest parameter.
;; The number is related to SCM_VM_STACK_SIZE but we can't directly
;; link to it because of cross compilation.  In future maybe we can
;; make it customizable by command line args or something.
(define-constant MAX_LITERAL_ARG_COUNT 8192)

;; used by pass5/$DEFINE.
;; This should match the values in src/gauche/module.h.  We intentionally
;; avoid referring to the C value,
;; via (inline-stub (define-enum SCM_BINDING_CONST) ...), since doing so
;; would complicate the compilation process in case we need to change those
;; constants.  (Compile.scm is compiled by the host gauche which refers to
;; the old value.)
(define-constant SCM_BINDING_CONST 2)
(define-constant SCM_BINDING_INLINABLE 4)

;; IForm tags
(define-enum .intermediate-tags.
  $DEFINE
  $LREF
  $LSET
  $GREF
  $GSET
  $CONST
  $IF
  $LET
  $RECEIVE
  $LAMBDA
  $LABEL
  $PROMISE
  $SEQ
  $CALL
  $ASM
  $CONS
  $APPEND
  $VECTOR
  $LIST->VECTOR
  $LIST
  $LIST*
  $MEMV
  $EQ?
  $EQV?
  $IT
  )

;; Define constants for VM instructions.
;; The 'eval-when' trick is to let the host compiler (that is compiling this
;; compiler) know those constants.
(eval-when (:compile-toplevel)
  (use gauche.vm.insn)
  (define-macro (define-insn-constants)
    (let1 name&codes
        (map (^[insn] (cons (car insn) (ref (cdr insn)'code)))
             (class-slot-ref <vm-insn-info> 'all-insns))
      `(begin
         ,@(map (^[n&c] `(define-constant ,(car n&c) ,(cdr n&c)))
                name&codes)
         (define-constant .insn-alist. ',name&codes)
         )))
  (define-insn-constants)
  )

;; Maximum size of $LAMBDA node we allow to duplicate and inline.
(define-constant SMALL_LAMBDA_SIZE 12)

(define-inline (variable? arg) (or (symbol? arg) (identifier? arg)))
(define-inline (variable-or-keyword? arg)
  (or (symbol? arg) (keyword? arg) (identifier? arg)))

;;============================================================
;; Data structures
;;

;; Local variables (lvar)
;;
;;   Slots:
;;     name  - name of the variable (symbol)
;;     initval   - initialized value
;;     ref-count - in how many places this variable is referefnced?
;;     set-count - in how many places this variable is set!
;;

(define-simple-struct lvar 'lvar make-lvar
  (name
   (initval (undefined))
   (ref-count 0)
   (set-count 0)))

(define (make-lvar+ name) ;; procedure version of constructor, for mapping
  (make-lvar name))

(define-inline (lvar? obj) (and (vector? obj) (eq? (vector-ref obj 0) 'lvar)))
(define (lvar-reset lvar)
  (lvar-ref-count-set! lvar 0)
  (lvar-set-count-set! lvar 0))

(define-inline (lvar-immutable? lvar)
  (= (lvar-set-count lvar) 0))

;; Returns IForm if this lvar has initval and it never changes.  Only valid
;; after lvar reference counting is done (that is, after pass1, and after
;; each reset-lvars call.
(define (lvar-const-value lvar)
  (and (lvar-immutable? lvar)
       (vector? (lvar-initval lvar))
       (lvar-initval lvar)))

;; implemented in C for better performance.
(inline-stub
 ;; offsets must be in sync with lvar definition above
 "#define LVAR_OFFSET_TAG        0"
 "#define LVAR_OFFSET_NAME       1"
 "#define LVAR_OFFSET_INITVAL    2"
 "#define LVAR_OFFSET_REF_COUNT  3"
 "#define LVAR_OFFSET_SET_COUNT  4"
 "#define LVAR_SIZE              5"

 ;; Specialized routine for (map (lambda (name) (make-lvar name)) objs)
 (define-cproc %map-make-lvar (names)
   (let* ((h SCM_NIL) (t SCM_NIL))
     (dolist [name names]
       (let* ([v (Scm_MakeVector LVAR_SIZE '0)])
         (set! (SCM_VECTOR_ELEMENT v LVAR_OFFSET_TAG) 'lvar
               (SCM_VECTOR_ELEMENT v LVAR_OFFSET_NAME) name
               (SCM_VECTOR_ELEMENT v LVAR_OFFSET_INITVAL) SCM_UNDEFINED)
         (SCM_APPEND1 h t v)))
     (return h)))

 (define-cise-stmt update!
   [(_ offset delta)
    `(let* ([i::int (SCM_INT_VALUE (SCM_VECTOR_ELEMENT lvar ,offset))])
       (set! (SCM_VECTOR_ELEMENT lvar ,offset) (SCM_MAKE_INT (+ i ,delta))))])

 (define-cproc lvar-ref++! (lvar) ::<void> (update! LVAR_OFFSET_REF_COUNT +1))
 (define-cproc lvar-ref--! (lvar) ::<void> (update! LVAR_OFFSET_REF_COUNT -1))
 (define-cproc lvar-set++! (lvar) ::<void> (update! LVAR_OFFSET_SET_COUNT +1))
 )

;; Compile-time environment (cenv)
;;
;;   Slots:
;;     module   - The 'current-module' to resolve global binding.
;;     frames   - List of local frames.  Each local frame has a form:
;;                (<type> (<name> . <obj>) ...)
;;
;;                <type>     <obj>
;;                ----------------------------------------------
;;                LEXICAL    <lvar>     ;; lexical binding
;;                SYNTAX     <macro>    ;; syntactic binding
;;
;;     exp-name - The "name" of the current expression, that is, the
;;                name of the variable the result of the current
;;                expression is to be bound.  This slot may contain
;;                an identifier (for global binding) or a lvar (for
;;                local binding).   This slot may be #f.
;;
;;     current-proc - Holds the information of the current
;;                compilig procedure.  It accumulates information needed
;;                in later stages for the optimization.  This slot may
;;                be #f.
;;
;;     source-path - While processing included file, this slot is set to
;;                the full path of the included filename.
(define-simple-struct cenv #f %make-cenv
  (module frames exp-name current-proc (source-path (current-load-path))))

(define (make-cenv module :optional (frames '()) (exp-name #f))
  (%make-cenv module frames exp-name))

;; Some cenv-related proceduers are in C for better performance.
(inline-stub
 ;; env-lookup-int :: Name, LookupAs, Module, [Frame] -> Var
 ;;         where Var = Lvar | Identifier | Macro
 ;;
 ;;  PERFORMANCE KLUDGE:
 ;;     - We assume the frame structure is well-formed, so skip some tests.
 ;;     - We assume 'lookupAs' and the car of each frame are small non-negative
 ;;       integers, so we directly compare them without unboxing them.
 ;;
 (define-cfn env-lookup-int (name lookup-as module::ScmModule* frames) :static
   (let* ([y name])
     (while 1
       (dopairs [fp1 frames]
         (when (> (SCM_CAAR fp1) lookup-as) ; see PERFORMANCE KLUDGE above
           (continue))
         ;; inline assq here to squeeze performance.
         (dolist [vp (SCM_CDAR fp1)]
           (when (SCM_EQ y (SCM_CAR vp)) (return (SCM_CDR vp)))))
       ;; No match.  We strip identifier wrapping and retry
       (if (SCM_IDENTIFIERP y)
         (let* ([inner (-> (SCM_IDENTIFIER y) name)])
           (unless (SCM_IDENTIFIERP inner)
             (set! frames (Scm_IdentifierEnv (SCM_IDENTIFIER y))))
           (set! y inner))
         (break))))
   ;; No local bindings.  Return an identifier.
   (if (SCM_SYMBOLP name)
     (return (Scm_MakeIdentifier name module '()))
     (begin
       (SCM_ASSERT (SCM_IDENTIFIERP name))
       (return name))))

 ;; Internal API - used while macro expansion
 (define-cproc env-lookup (name module frames)
   (return (env-lookup-int name (SCM_MAKE_INT 1) ;; SYNTAX
                           (SCM_MODULE module) frames)))
 ;; Internal API - for faster CENV lookup
 (define-cproc cenv-lookup-syntax (cenv name)
   (return
    (env-lookup-int name (SCM_MAKE_INT 1)                      ; SYNTAX
                    (SCM_MODULE (SCM_VECTOR_ELEMENT cenv 0))   ; module
                    (SCM_VECTOR_ELEMENT cenv 1))))             ; frames
 ;; Internal API - for faster CENV lookup
 (define-cproc cenv-lookup-variable (cenv name)
   (return
    (env-lookup-int name (SCM_MAKE_INT 0)                      ; LEXICAL
                    (SCM_MODULE (SCM_VECTOR_ELEMENT cenv 0))   ; module
                    (SCM_VECTOR_ELEMENT cenv 1))))             ; frames

 ;; Check if Cenv is toplevel or not.
 ;; Since R7RS let{rec}-syntax delimits scope, its body can't contain toplevel
 ;; definitions.  Hence existence of any frame, LEXICAL or SYNTAX, makes
 ;; CENV non-toplevel.
 ;;
 ;; (define (cenv-toplevel? cenv)
 ;;   (null? (cenv-frames cenv)))
 ;;
 (define-cproc cenv-toplevel? (cenv) ::<boolean>
   (SCM_ASSERT (SCM_VECTORP cenv))
   (return (SCM_NULLP (SCM_VECTOR_ELEMENT cenv 1))))
 )

(define-macro (cenv-copy-except cenv . kvs)
  `(%make-cenv ,(get-keyword :module kvs `(cenv-module ,cenv))
               ,(get-keyword :frames kvs `(cenv-frames ,cenv))
               ,(get-keyword :exp-name kvs `(cenv-exp-name ,cenv))
               ,(get-keyword :current-proc kvs `(cenv-current-proc ,cenv))
               ,(get-keyword :source-path kvs `(cenv-source-path ,cenv))))

(define-macro (make-bottom-cenv . maybe-module)
  (if (null? maybe-module)
    `(%make-cenv (vm-current-module) '())
    `(%make-cenv ,(car maybe-module) '())))

(define-inline (cenv-swap-module cenv mod)
  (cenv-copy-except cenv :module mod))

(define-inline (cenv-extend cenv frame type)
  (cenv-copy-except cenv :frames (acons type frame (cenv-frames cenv))))

(define-inline (cenv-extend/proc cenv frame type proc)
  (cenv-copy-except cenv :frames (acons type frame (cenv-frames cenv))
                    :current-proc proc))

(define-inline (cenv-add-name cenv name)
  (cenv-copy-except cenv :exp-name name))

(define-inline (cenv-add-name/source cenv name source)
  (cenv-copy-except cenv :exp-name name :source-path source))

(define-inline (cenv-sans-name cenv)
  (if (cenv-exp-name cenv)
    (cenv-copy-except cenv :exp-name #f)
    cenv))

(define-inline (cenv-extend/name cenv frame type name)
  (cenv-copy-except cenv :frames (acons type frame (cenv-frames cenv))
                    :exp-name name))

(define-inline (cenv-swap-source cenv source)
  (cenv-copy-except cenv :source-path source))

;; Intermediate tree form (IForm)
;;
;;   We first convert the program into an intermediate tree form (IForm),
;;   which is in principle similar to A-normal form, but has more
;;   convenience node types specific to our VM.   IForm is represented
;;   by a nested vectors, whose first element shows the type of the node.
;;
;;   The following table is an overview of the structure.  See
;;   [IForm Definitions] section below for the detailed specification.
;;
;; <top-iform> :=
;;    <iform>
;;    #($define <src> <flags> <id> <iform>)
;;
;; <iform> :=
;;    #($lref <lvar>)        ;; local variable reference
;;    #($lset <lvar> <iform>) ;; local variable modification
;;    #($gref <id>)          ;; global variable reference
;;    #($gset <id> <iform>)   ;; global variable modification
;;    #($const <obj>)        ;; constant literal
;;    #($if <src> <iform> <iform+> <iform+>) ;; branch
;;    #($let <src> <type> (<lvar> ...) (<iform> ...) <iform>) ;; local binding
;;    #($receive <src> <reqarg> <optarg> (<lvar> ...) <iform> <iform>)
;;                           ;; local binding (mv)
;;    #($lambda <src> <name> <reqarg> <optarg> (<lvar> ...) <iform> <flag>)
;;                           ;; closure
;;    #($label <src> <label> <iform>) ;; merge point of local call.  see below.
;;    #($promise <src> <expr>) ;; promise
;;    #($seq (<iform> ...))   ;; sequencing
;;    #($call <src> <proc-expr> (<arg-expr> ...) <flag>) ;; procedure call
;;    #($asm <src> <insn> (<arg> ...)) ;; inline assembler
;;    #($cons <src> <ca> <cd>)       ;; used in quasiquote
;;    #($append <src> <ca> <cd>)     ;; ditto
;;    #($vector <src> (<elt> ...))   ;; ditto
;;    #($list->vector <src> <list>)  ;; ditto
;;    #($list <src> (<elt> ...))     ;; ditto
;;    #($list* <src> (<elt> ...))    ;; ditto
;;    #($memv <src> <obj> <list>)    ;; used in case
;;    #($eq?  <src> <x> <y>)         ;; ditto
;;    #($eqv? <src> <x> <y>)         ;; ditto
;;
;; <iform+> :=
;;    <iform>
;;    #($it)                 ;; refer to the value in the last test clause.
;;
;;  NB: <src> slot keeps the information of the original source, and
;;      will be used to generate debug info.  Normally it holds the
;;      relevant source code, or #f if there's no relevant code.
;;
;;  NB: the actual value of the first element is an integer instead of
;;      a symbol, which allows us to use vector dispatch instead
;;      of case expressions.
;;
;;  NB: The nodes are destructively modified during compilation, in order
;;      to keep allocations minimal.   Nodes shouldn't be shared, for
;;      side-effects may vary depends on the path to the node.  The only
;;      exception is $label node.

(define-macro (iform-tag iform)
  `(vector-ref ,iform 0))

;; check intermediate tag
(define-macro (has-tag? iform tag)
  `(eqv? (vector-ref ,iform 0) ,tag))

;; [IForm Definitions]

;; $define <src> <flags> <id> <expr>
;;   Global definition.  Binds the result of <expr> to the global
;;   identifier <id>.
(define-simple-struct $define $DEFINE $define
  (src       ; original source for debugging
   flags     ; a list of flags.  Currently, only the following flag
             ;  is supported:
             ;      const   : the binding is constant.
   id        ; global identifier
   expr      ; expression IForm
   ))

;; $lref <lvar>
;;   Local variable reference.
(define-simple-struct $lref $LREF #f
  (lvar      ; lvar struct.
   ))
(define-inline ($lref lvar)   (lvar-ref++! lvar) (vector $LREF lvar))
(define-inline ($lref? iform) (has-tag? iform $LREF))

;; $lset <lvar> <expr>
;;   Local variable assignment.  The result of <expr> is set to <lvar>.
(define-simple-struct $lset $LSET #f
  (lvar      ; lvar struct
   expr      ; IForm
   ))
(define-inline ($lset lvar expr)
  (lvar-set++! lvar) (vector $LSET lvar expr))

;; $gref <id>
;;   Global variable reference.
(define-simple-struct $gref $GREF $gref
  (id        ; identifier
   ))

;; $gset <id> <iform>
;;   Global variable assignment.
(define-simple-struct $gset $GSET $gset
  (id        ; identifier
   expr      ; IForm
   ))

;; $const <value>
;;   Constant.
(define-simple-struct $const $CONST $const
  (value     ; Scheme value
   ))
(define-inline ($const? x) (has-tag? x $CONST))
;; common cases
(define $const-undef (let1 x ($const (undefined)) (^[] x)))
(define $const-nil   (let1 x ($const '()) (^[] x)))
(define $const-f     (let1 x ($const #f) (^[] x)))
(define $const-t     (let1 x ($const #t) (^[] x)))

;; $if <src> <test> <then> <else>
;;   Conditional.
;;   A special IForm, $it, can appear in either <then> or <else>
;;   clause; it is no-op and indicates that the result(s) of <test>
;;   should be carried over.
(define-simple-struct $if $IF $if
  (src       ; original source for debugging
   test      ; IForm for test expression
   then      ; IForm for then expression
   else      ; IForm for else expression
   ))

;; $let <src> <type> <lvars> <inits> <body>
;;   Binding construct.  let, letrec, and inlined closure is represented
;;   by this node (let* is expanded to the nested $let in pass 1).
(define-simple-struct $let $LET $let
  (src       ; original source for debugging
   type      ; indicates scope: 'let for normal let, 'rec[*] for letrec[*], 
   lvars     ; list of lvars
   inits     ; list of IForms to initialize lvars
   body      ; IForm for the body
   ))

;; $receive <src> <reqargs> <optarg> <lvars> <expr> <body>
;;   Multiple value binding construct.
(define-simple-struct $receive $RECEIVE $receive
  (src       ; original source for debugging
   reqargs   ; # of required args
   optarg    ; 0 or 1, # of optional arg
   lvars     ; list of lvars
   expr      ; IForm for the expr to yield multiple values
   body      ; IForm for the body
   ))

;; $lambda <src> <reqargs> <optarg> <lvars> <body> [<flag>]
;;   Closure.
;;   $lambda has a couple of transient slots, which are used only
;;   during the optimization paths and not be saved by pack-iform.
(define-simple-struct $lambda $LAMBDA $lambda
  (src              ; original source for debugging
   name             ; inferred name of this closure
   reqargs          ; # of required args
   optarg           ; 0 or 1, # of optional arg
   lvars            ; list of lvars
   body             ; IForm for the body
   flag             ; Marks some special state of this node.
                    ;   'dissolved: indicates that this lambda has been
                    ;               inline expanded.
                    ;   'used: indicates that this lambda has been already dealt
                    ;          with, and need to be eliminated.  This one is
                    ;          specifically used for communication between
                    ;          pass2/$CALL and pass2/$LET.
                    ;   <packed-iform>  : inlinable lambda
   ;; The following slots are used temporarily during pass2-5, and
   ;; need not be saved when packed.
   (calls '())      ; list of call sites
   (free-lvars '()) ; list of free local variables
   (lifted-var #f)  ; if this $LAMBDA is lifted to the toplevel, this slot
                    ; contains an lvar to which the toplevel closure
                    ; is to be bound.  See pass 4.
   ))

;; $label <src> <label> <body>
;;    This kind of IForm node is introduced in Pass2 to record a shared
;;    node.  It marks the destination of LOCAL-ENV-JUMP, and also is
;;    created during $if optimization.
(define-simple-struct $label $LABEL $label
  (src       ; original source for debugging
   label     ; label.  #f in Pass 2.  Assigned in Pass 5.
   body      ; IForm for the body
   ))

;; $seq <body>
;;    Sequensing.  <body> is a list of IForms.
;;    The compile tries to avoid creating $seq node if <body> has only
;;    one expression, but optimization paths may introduce such a $seq node.
;;    There can also be an empty $seq node, ($seq '()).
(define-simple-struct $seq $SEQ #f
  (body      ; list of IForms
   ))

(define-inline ($seq exprs)
  (if (and (pair? exprs) (null? (cdr exprs)))
    (car exprs)
    (vector $SEQ exprs)))

;; $call <src> <proc> <args> [<flag>]
;;    Call a procedure.
;;    See the "Closure optimization" section in Pass 2 for the detailed
;;    description.
(define-simple-struct $call $CALL $call
  (src       ; original source for debugging
   proc      ; IForm for the procedure to call.
   args      ; list of IForms for arguments.
   flag      ; #f, 'local, 'embed, 'jump, 'rec or 'tail-rec.
   ;; Transient slots
   (renv '()) ; runtime env.  used in embed calls to record depth of env
              ;   in Pass 5.
   ))

(define-inline ($call? iform) (has-tag? iform $CALL))

;; $asm <src> <insn> <args>
;;    Inlined assembly code.
(define-simple-struct $asm $ASM $asm
  (src       ; original source for debugging
   insn      ; instruction (<code> [<param> ...])
   args      ; list of IForms
   ))

;; $promise <src> <expr>
;;    OBSOLETED.  We keep $promise during 0.9.x releases since it
;;    can appear in the packed IForm of the inlined procedures.
;;    Will go in 1.0.
(define-simple-struct $promise $PROMISE $promise
  (src       ; original source for debugging
   expr      ; IForm
   ))

;; $it
;;   A special node.  See the explanation of $if above.
(define $it (let ((c `#(,$IT))) (^[] c)))
(define-inline ($it? x) (has-tag? x $IT))

;; The followings are builtin version of standard procedures.
;;
(define-simple-struct $cons $CONS #f (src arg0 arg1))

;; quasiquote tends to generate nested $cons, which can be
;; packed to $list or $list*.
(define ($cons o x y)
  (if (has-tag? y $CONS)
    (receive (type elts) ($cons-pack y)
      (vector type o (cons x elts)))
    (vector $CONS o x y)))

(define ($cons-pack elt)
  (cond
   [(equal? elt ($const-nil)) (values $LIST '())]
   [(has-tag? elt $CONS)
    (receive (type elts) ($cons-pack (vector-ref elt 3))
      (values type (cons (vector-ref elt 2) elts)))]
   [else (values $LIST* (list elt))]))

(define-simple-struct $append $APPEND $append (src arg0 arg1))
(define-simple-struct $memv   $MEMV   $memv   (src arg0 arg1))
(define-simple-struct $eq?    $EQ?    $eq?    (src arg0 arg1))
(define-simple-struct $eqv?   $EQV?   $eqv?   (src arg0 arg1))
(define-simple-struct $vector $VECTOR $vector (src args))
(define-simple-struct $list   $LIST   $list   (src args))
(define-simple-struct $list*  $LIST*  $list*  (src args))
(define-simple-struct $list->vector $LIST->VECTOR $list->vector (src arg0))

;; common accessors
(define-macro ($*-src  iform)  `(vector-ref ,iform 1))
(define-macro ($*-args iform)  `(vector-ref ,iform 2))
(define-macro ($*-arg0 iform)  `(vector-ref ,iform 2))
(define-macro ($*-arg1 iform)  `(vector-ref ,iform 3))
(define-macro ($*-args-set! iform val)  `(vector-set! ,iform 2 ,val))
(define-macro ($*-arg0-set! iform val)  `(vector-set! ,iform 2 ,val))
(define-macro ($*-arg1-set! iform val)  `(vector-set! ,iform 3 ,val))

;; look up symbolic name of iform tag (for debugging)
(define (iform-tag-name tag)
  (let loop ((p .intermediate-tags.))
    (cond [(null? p) #f]
          [(eqv? (cdar p) tag) (caar p)]
          [else (loop (cdr p))])))

;; look up symbolic name of VM instruction (for debugging)
;; (The proper way to realize this is using gauche.vm.insn, but we can't
;;  use it from comp.scm)
(define (insn-name code)
  (let loop ((p .insn-alist.))
    (cond [(null? p) #f]
          [(eqv? (cdar p) code) (caar p)]
          [else (loop (cdr p))])))

;; prettyprinter of intermediate form
(define (pp-iform iform :optional (lines +inf.0))
  (let/cc return
    (define labels '()) ;; alist of label node and count
    (define (indent count) (dotimes [i count] (write-char #\space)))
    (define (id->string id)
      (format "~a#~a" (module-name (slot-ref id'module)) (slot-ref id'name)))
    (define (lvar->string lvar)
      (format "~a.~a~a" (variable-name (lvar-name lvar))
              (lvar-ref-count lvar)
              (make-string (lvar-set-count lvar) #\!)))
    (define (nl ind)
      (newline) (dec! lines) (when (zero? lines) (return)) (indent ind))

    (let rec ([ind 0] [iform iform])
      (case/unquote
       (iform-tag iform)
       [($DEFINE) (format #t "($define ~a ~a" ($define-flags iform)
                          (id->string ($define-id iform)))
        (nl (+ ind 2))
        (rec (+ ind 2) ($define-expr iform)) (display ")")]
       [($LREF)   (format #t "($lref ~a)" (lvar->string ($lref-lvar iform)))]
       [($LSET)   (format #t "($lset ~a"  (lvar->string ($lset-lvar iform)))
        (nl (+ ind 2))
        (rec (+ ind 2) ($lset-expr iform)) (display ")")]
       [($GREF)   (format #t "($gref ~a)" (id->string ($gref-id iform)))]
       [($GSET)   (format #t "($gset ~a" (id->string ($gset-id iform)))
        (nl (+ ind 2))
        (rec (+ ind 2) ($gset-expr iform)) (display ")")]
       [($CONST)  (format #t "($const ~s)" ($const-value iform))]
       [($IF)     (display "($if ")
        (rec (+ ind 5) ($if-test iform)) (nl (+ ind 2))
        (rec (+ ind 2) ($if-then iform)) (nl (+ ind 2))
        (rec (+ ind 2) ($if-else iform)) (display ")")]
       [($LET)
        (let* ([hdr  (format "($let~a ("
                             (case ($let-type iform)
                               [(let) ""] [(rec) "rec"] [(rec*) "rec*"]))]
               [xind (+ ind (string-length hdr))]
               [first #t])
          (display hdr)
          (for-each (^[var init]
                      (if first (set! first #f) (nl xind))
                      (let1 z (format "[~a " (lvar->string var))
                        (display z)
                        (rec (+ xind  (string-length z)) init)
                        (display "]")))
                    ($let-lvars iform) ($let-inits iform))
          (display ")") (nl (+ ind 2))
          (rec (+ ind 2) ($let-body iform)) (display ")"))]
       [($RECEIVE)
        (format #t "($receive ~a" (map lvar->string ($receive-lvars iform)))
        (nl (+ ind 4))
        (rec (+ ind 4) ($receive-expr iform)) (nl (+ ind 2))
        (rec (+ ind 2) ($receive-body iform)) (display ")")]
       [($LAMBDA) (format #t "($lambda[~a.~a~a] ~a" ($lambda-name iform)
                          (length ($lambda-calls iform))
                          (if (vector? ($lambda-flag iform)) " inlinable" "")
                          (map lvar->string ($lambda-lvars iform)))
        (nl (+ ind 2))
        (rec (+ ind 2) ($lambda-body iform)) (display ")")]
       [($LABEL) (if-let1 p (assq iform labels)
                   (format #t "label#~a" (cdr p))
                   (let1 num (length labels)
                     (push! labels (cons iform num))
                     (format #t "($label #~a" num)
                     (nl (+ ind 2))
                     (rec (+ ind 2) ($label-body iform)) (display ")")))]
       [($SEQ)   (format #t "($seq")
        (for-each (^n (nl (+ ind 2)) (rec (+ ind 2) n)) ($seq-body iform))
        (display ")")]
       [($CALL)  (let1 pre (if-let1 flag ($call-flag iform)
                             (format "($call[~a] " flag)
                             "($call ")
                   (format #t pre)
                   (rec (+ ind (string-length pre)) ($call-proc iform))
                   (for-each (^n (nl (+ ind 2)) (rec (+ ind 2) n))
                             ($call-args iform))
                   (display ")"))]
       [($ASM)
        (let* ([insn ($asm-insn iform)]
               [args ($asm-args iform)]
               [hdr  (format "($asm ~a" (cons (insn-name (car insn))
                                              (cdr insn)))])
          (display hdr)
          (case (length args)
            [(0)]
            [(1) (display " ") (rec (+ ind (string-length hdr) 1) (car args))]
            [else (for-each (^n (nl (+ ind 2)) (rec (+ ind 2) n))
                            ($asm-args iform))])
          (display ")"))]
       [($PROMISE) (display "($promise ")
        (rec (+ ind 10) ($promise-expr iform))
        (display ")")]
       [($IT)      (display "($it)")]
       [($CONS $APPEND $MEMV $EQ? $EQV?)
        (let* ([s (format "(~a " (iform-tag-name (iform-tag iform)))]
               [ind (+ ind (string-length s))])
          (display s)
          (rec ind (vector-ref iform 2)) (nl ind)
          (rec ind (vector-ref iform 3)) (display ")"))]
       [($LIST $LIST* $VECTOR)
        (display (format "(~a " (iform-tag-name (iform-tag iform))))
        (dolist [elt (vector-ref iform 2)] (nl (+ ind 2)) (rec (+ ind 2) elt))
        (display ")")]
       [($LIST->VECTOR)
        (display "($LIST->VECTOR ")
        (rec (+ ind 14) (vector-ref iform 2))
        (display ")")]
       [else (error "pp-iform: unknown tag:" (iform-tag iform))]))
    (newline)))

;; Sometimes we need to save IForm for later use (e.g. procedure inlining)
;; We pack an IForm into a vector, instead of keeping it as is, since:
;;  - For separate compilation, the saved form has to become a static
;;    literal, keeping it's topology.  The compiler unifies equal?-literals,
;;    so we can't just rely on it.  We also need to traverse the IForm to
;;    make sure everything is serializable, anyway.
;;  - IForm is destructively modified by pass 2, so we need to copy it
;;    every time it is used.
;;
;; Packed IForm is a vector, with the references are represented by indices.

(define (pack-iform iform)

  (define dict (make-hash-table 'eq?))
  (define r '())
  (define c 1)

  (define (put! iform . objs)
    (rlet1 head c
      (hash-table-put! dict iform head)
      (dolist [obj objs] (push! r obj) (inc! c))))

  (define (get-ref iform)
    (or (hash-table-get dict iform #f) (pack-iform-rec iform)))

  (define (pack-iform-rec iform)
    (case/unquote
     (iform-tag iform)
     [($DEFINE) (put! iform $DEFINE ($*-src iform)
                      ($define-flags iform) ($define-id iform)
                      (get-ref ($define-expr iform)))]
     [($LREF) (put! iform $LREF (get-ref ($lref-lvar iform)))]
     [($LSET) (put! iform $LSET
                    (get-ref ($lset-lvar iform)) (get-ref ($lset-expr iform)))]
     [($GREF) (put! iform $GREF ($gref-id iform))]
     [($GSET) (put! iform $GSET ($gset-id iform) (get-ref ($gset-expr iform)))]
     [($CONST)(put! iform $CONST ($const-value iform))]
     [($IF)   (put! iform $IF ($*-src iform)
                    (get-ref ($if-test iform))
                    (get-ref ($if-then iform))
                    (get-ref ($if-else iform)))]
     [($LET)  (put! iform (iform-tag iform) ($*-src iform) ($let-type iform)
                    (map get-ref ($let-lvars iform))
                    (map get-ref ($let-inits iform))
                    (get-ref ($let-body iform)))]
     [($RECEIVE) (put! iform $RECEIVE ($*-src iform)
                       ($receive-reqargs iform) ($receive-optarg iform)
                       (map get-ref ($receive-lvars iform))
                       (get-ref ($receive-expr iform))
                       (get-ref ($receive-body iform)))]
     [($LAMBDA) (put! iform $LAMBDA ($*-src iform)
                      ($lambda-name iform) ($lambda-reqargs iform)
                      ($lambda-optarg iform)
                      (map get-ref ($lambda-lvars iform))
                      (get-ref ($lambda-body iform))
                      ($lambda-flag iform))]
     [($LABEL)  (put! iform $LABEL ($*-src iform) #f
                      (get-ref ($label-body iform)))]
     [($SEQ)    (put! iform $SEQ (map get-ref ($seq-body iform)))]
     [($CALL)   (put! iform $CALL ($*-src iform)
                      (get-ref ($call-proc iform))
                      (map get-ref ($call-args iform))
                      ($call-flag iform))]
     [($ASM)    (put! iform $ASM ($*-src iform)
                      ($asm-insn iform)
                      (map get-ref ($asm-args iform)))]
     [($IT)     (put! iform $IT)]
     [($PROMISE)(put! iform $PROMISE ($*-src iform)
                      (get-ref ($promise-expr iform)))]
     [($CONS $APPEND $MEMV $EQ? $EQV?)
      (put! iform (iform-tag iform) ($*-src iform)
            (get-ref ($*-arg0 iform))
            (get-ref ($*-arg1 iform)))]
     [($VECTOR $LIST $LIST*)
      (put! iform (iform-tag iform) ($*-src iform)
            (map get-ref ($*-args iform)))]
     [($LIST->VECTOR)
      (put! iform (iform-tag iform) ($*-src iform)
            (get-ref ($*-arg0 iform)))]
     [('lvar)
      (put! iform 'lvar (lvar-name iform))]
     [else
      (errorf "[internal-error] unknown IForm in pack-iform: ~S" iform)]
     ))

  ;; main body of pack-iform
  (let* ([start (pack-iform-rec iform)]
         [vec (make-vector c)])
    (do ([i (- c 1) (- i 1)]
         [r r (cdr r)])
        [(null? r)]
      (vector-set! vec i (car r)))
    (vector-set! vec 0 start)
    vec))

(define (unpack-iform ivec)
  (let-syntax ([V (syntax-rules ()
                    [(V ix) (vector-ref ivec ix)]
                    [(V ix off) (vector-ref ivec (+ ix off))])])
    (define dict (make-hash-table 'eqv?))
    (define (unpack-rec ref)
      (cond [(hash-table-get dict ref #f)]
            [else (rlet1 body (unpack-body ref)
                    (hash-table-put! dict ref body))]))
    (define (unpack-body i)
      (case/unquote
       (V i)
       [($DEFINE) ($define (V i 1) (V i 2) (V i 3) (unpack-rec (V i 4)))]
       [($LREF)   ($lref (unpack-rec (V i 1)))]
       [($LSET)   ($lset (unpack-rec (V i 1)) (unpack-rec (V i 2)))]
       [($GREF)   ($gref (V i 1))]
       [($GSET)   ($gset (V i 1) (unpack-rec (V i 2)))]
       [($CONST)  ($const (V i 1))]
       [($IF)     ($if (V i 1) (unpack-rec (V i 2))
                       (unpack-rec (V i 3)) (unpack-rec (V i 4)))]
       [($LET)    (rlet1 unpacked
                      ($let (V i 1) (V i 2)
                            (map unpack-rec (V i 3)) (map unpack-rec (V i 4))
                            (unpack-rec (V i 5)))
                    (ifor-each2 (^(lv in) (lvar-initval-set! lv in))
                                ($let-lvars unpacked) ($let-inits unpacked)))]
       [($RECEIVE) ($receive (V i 1) (V i 2) (V i 3)
                             (map unpack-rec (V i 4)) (unpack-rec (V i 5))
                             (unpack-rec (V i 6)))]
       [($LAMBDA)  ($lambda (V i 1) (V i 2) (V i 3) (V i 4)
                            (map unpack-rec (V i 5))
                            (unpack-rec (V i 6)) (V i 7))]
       [($LABEL)   ($label (V i 1) (V i 2) (unpack-rec (V i 3)))]
       [($SEQ)     ($seq (map unpack-rec (V i 1)))]
       [($CALL)    ($call (V i 1) (unpack-rec (V i 2))
                          (map unpack-rec (V i 3)) (V i 4))]
       [($ASM)     ($asm (V i 1) (V i 2) (map unpack-rec (V i 3)))]
       [($PROMISE) ($promise (V i 1) (unpack-rec (V i 2)))]
       [($IT) ($it)]
       [($CONS $APPEND $MEMV $EQ? $EQV?)
        (vector (V i) (V i 1) (unpack-rec (V i 2)) (unpack-rec (V i 3)))]
       [($VECTOR $LIST $LIST*) (vector (V i) (V i 1) (map unpack-rec (V i 2)))]
       [($LIST->VECTOR) (vector (V i) (V i 1) (unpack-rec (V i 2)))]
       [('lvar)    (make-lvar (V i 1))]
       [else (errorf "[internal error] unpack-iform: ivec broken at ~a: ~S"
                     i ivec)]))

    (unpack-rec (V 0))))

;; Counts the size (approx # of nodes) of the iform.
(define (iform-count-size-upto iform limit)
  (define (rec iform cnt)
    (letrec-syntax ([sum-items
                     (syntax-rules (*)
                       [(_ cnt) cnt]
                       [(_ cnt (* item1) item2 ...)
                        (let1 s1 (rec-list item1 cnt)
                          (if (>= s1 limit) limit (sum-items s1 item2 ...)))]
                       [(_ cnt item1 item2 ...)
                        (let1 s1 (rec item1 cnt)
                          (if (>= s1 limit) limit (sum-items s1 item2 ...)))])])
      (case/unquote
       (iform-tag iform)
       [($DEFINE) (sum-items (+ cnt 1) ($define-expr iform))]
       [($LREF $GREF $CONST) (+ cnt 1)]
       [($LSET)   (sum-items (+ cnt 1) ($lset-expr iform))]
       [($GSET)   (sum-items (+ cnt 1) ($gset-expr iform))]
       [($IF)     (sum-items (+ cnt 1) ($if-test iform)
                             ($if-then iform) ($if-else iform))]
       [($LET) (sum-items (+ cnt 1) (* ($let-inits iform)) ($let-body iform))]
       [($RECEIVE)
        (sum-items (+ cnt 1) ($receive-expr iform) ($receive-body iform))]
       [($LAMBDA) (sum-items (+ cnt 1) ($lambda-body iform))]
       [($LABEL)  (sum-items cnt ($label-body iform))]
       [($SEQ)    (sum-items cnt (* ($seq-body iform)))]
       [($CALL) (sum-items (+ cnt 1) ($call-proc iform) (* ($call-args iform)))]
       [($ASM)    (sum-items (+ cnt 1) (* ($asm-args iform)))]
       [($PROMISE)(sum-items (+ cnt 1) ($promise-expr iform))]
       [($CONS $APPEND $MEMV $EQ? $EQV?)
        (sum-items (+ cnt 1) ($*-arg0 iform) ($*-arg1 iform))]
       [($VECTOR $LIST $LIST*) (sum-items (+ cnt 1) (* ($*-args iform)))]
       [($LIST->VECTOR) (sum-items (+ cnt 1) ($*-arg0 iform))]
       [($IT) cnt]
       [else
        (error "[internal error] iform-count-size-upto: unknown iform tag:"
               (iform-tag iform))]
       )))
  (define (rec-list iform-list cnt)
    (cond [(null? iform-list) cnt]
          [(>= cnt limit) limit]
          [else (rec-list (cdr iform-list) (rec (car iform-list) cnt))]))
  (rec iform 0))

;; Copy iform.
;;  Lvars that are bound within iform should be copied.  Other lvars
;;  (free in iform, bound outside iform) should be shared and their
;;  refcount should be adjusted.  lv-alist keeps assoc list of
;;  old lvar to copied lvar.
;;  $CALL nodes need special treatment; if it is a call to local proc,
;;  the target $lambda node may have a list of call sites.  We need to
;;  add copied $CALL node as a new call site to the target.

(define (iform-copy iform lv-alist)
  (case/unquote
   (iform-tag iform)
   [($DEFINE) ($define ($*-src iform) ($define-flags iform) ($define-id iform)
                       (iform-copy ($define-expr iform) lv-alist))]
   [($LREF) ($lref (iform-copy-lvar ($lref-lvar iform) lv-alist))]
   [($LSET) ($lset (iform-copy-lvar ($lset-lvar iform) lv-alist)
                   (iform-copy ($lset-expr iform) lv-alist))]
   [($GREF) ($gref ($gref-id iform))]
   [($GSET) ($gset ($gset-id iform) (iform-copy ($gset-expr iform) lv-alist))]
   [($CONST)($const ($const-value iform))]
   [($IF)   ($if ($*-src iform)
                 (iform-copy ($if-test iform) lv-alist)
                 (iform-copy ($if-then iform) lv-alist)
                 (iform-copy ($if-else iform) lv-alist))]
   [($LET) (receive (newlvs newalist)
               (iform-copy-zip-lvs ($let-lvars iform) lv-alist)
             ($let ($*-src iform) ($let-type iform)
                   newlvs
                   (imap (cute iform-copy <> (case ($let-type iform)
                                               [(let) lv-alist]
                                               [(rec rec*) newalist]))
                         ($let-inits iform))
                   (iform-copy ($let-body iform) newalist)))]
   [($RECEIVE) (receive (newlvs newalist)
                   (iform-copy-zip-lvs ($receive-lvars iform) lv-alist)
                 ($receive ($*-src iform)
                           ($receive-reqargs iform) ($receive-optarg iform)
                           newlvs (iform-copy ($receive-expr iform) lv-alist)
                           (iform-copy ($receive-body iform) newalist)))]
   [($LAMBDA) (receive (newlvs newalist)
                  (iform-copy-zip-lvs ($lambda-lvars iform) lv-alist)
                ($lambda ($*-src iform) ($lambda-name iform)
                         ($lambda-reqargs iform) ($lambda-optarg iform)
                         newlvs
                         (iform-copy ($lambda-body iform) newalist)
                         ($lambda-flag iform)))]
   [($LABEL)
    (cond [(assq iform lv-alist) => (^p (cdr p))]
          [else
           (rlet1 newnode
               ($label ($label-src iform) ($label-label iform) #f)
             ($label-body-set! newnode
                               (iform-copy ($label-body iform)
                                           (acons iform newnode lv-alist))))])]
   [($SEQ) ($seq (imap (cut iform-copy <> lv-alist) ($seq-body iform)))]
   [($CALL) (rlet1 copy ($call ($*-src iform)
                               (iform-copy ($call-proc iform) lv-alist)
                               (imap (cut iform-copy <> lv-alist)
                                     ($call-args iform))
                               ($call-flag iform))
              (and-let* ([proc ($call-proc iform)]
                         [ ($lref? proc) ]
                         [ (lvar-immutable? ($lref-lvar proc)) ]
                         [target (lvar-initval ($lref-lvar proc))]
                         [ (vector? target) ]
                         [ (has-tag? target $LAMBDA) ]
                         [old (assq iform ($lambda-calls target))])
                ($lambda-calls-set! target
                                    (acons copy (cdr old)
                                           ($lambda-calls target)))))]
   [($ASM) ($asm ($*-src iform) ($asm-insn iform)
                 (imap (cut iform-copy <> lv-alist) ($asm-args iform)))]
   [($PROMISE)($promise ($*-src iform)
                        (iform-copy ($promise-expr iform) lv-alist))]
   [($CONS)   ($cons ($*-src iform)
                     (iform-copy ($*-arg0 iform) lv-alist)
                     (iform-copy ($*-arg1 iform) lv-alist))]
   [($APPEND) ($append ($*-src iform)
                       (iform-copy ($*-arg0 iform) lv-alist)
                       (iform-copy ($*-arg1 iform) lv-alist))]
   [($VECTOR) ($vector ($*-src iform)
                       (imap (cut iform-copy <> lv-alist) ($*-args iform)))]
   [($LIST->VECTOR)
    ($list->vector ($*-src iform) (iform-copy ($*-arg0 iform) lv-alist))]
   [($LIST)   ($list ($*-src iform)
                     (imap (cut iform-copy <> lv-alist) ($*-args iform)))]
   [($LIST*)  ($list* ($*-src iform)
                      (imap (cut iform-copy <> lv-alist) ($*-args iform)))]
   [($MEMV)   ($memv ($*-src iform)
                     (iform-copy ($*-arg0 iform) lv-alist)
                     (iform-copy ($*-arg1 iform) lv-alist))]
   [($EQ?)    ($eq? ($*-src iform)
                    (iform-copy ($*-arg0 iform) lv-alist)
                    (iform-copy ($*-arg1 iform) lv-alist))]
   [($EQV?)   ($eqv? ($*-src iform)
                     (iform-copy ($*-arg0 iform) lv-alist)
                     (iform-copy ($*-arg1 iform) lv-alist))]
   [($IT) ($it)]
   [else iform]))

(define (iform-copy-zip-lvs orig-lvars lv-alist)
  (let1 new-lvars (imap (^v (make-lvar (lvar-name v))) orig-lvars)
    (values new-lvars (fold-right acons lv-alist orig-lvars new-lvars))))

(define (iform-copy-lvar lvar lv-alist)
  (cond [(assq lvar lv-alist) => cdr]
        [else lvar]))

;; Translate instruction code embedded in $ASM node.  This isn't directly
;; used within the compiler, but called from the pre-compiler (precomp).
;; This is necessary to compile gauche core (compile.scm, scmlib.scm, ...)
;; after the instruction set has been changed.
;; SRC is a packed iform, TARGET-INSN-ALIST is a list of (insn-name . insn)
;; of the _target_ VM.  Returns translated packed iform.  We don't need
;; speed here, so we keep it simple.
(define (translate-packed-iform src target-insn-alist)
  (define (rec iform)
    (case/unquote
     (iform-tag iform)
     [($DEFINE)
      ($define ($*-src iform) ($define-flags iform) ($define-id iform)
               (rec ($define-expr iform)))]
     [($LREF) iform]
     [($LSET) ($lset ($lset-lvar iform) (rec ($lset-expr iform)))]
     [($GREF) iform]
     [($GSET) ($gset ($gset-id iform) (rec ($gset-expr iform)))]
     [($CONST) iform]
     [($IF)   ($if ($*-src iform)
                   (rec ($if-test iform))
                   (rec ($if-then iform))
                   (rec ($if-else iform)))]
     [($LET)  ($let ($*-src iform) ($let-type iform) ($let-lvars iform)
                    (imap rec ($let-inits iform))
                    (rec ($let-body iform)))]
     [($RECEIVE) ($receive ($*-src iform)
                           ($receive-reqargs iform) ($receive-optarg iform)
                           ($receive-lvars iform) (rec ($receive-expr iform))
                           (rec ($receive-body iform)))]
     [($LAMBDA) ($lambda ($*-src iform) ($lambda-name iform)
                         ($lambda-reqargs iform) ($lambda-optarg iform)
                         ($lambda-lvars iform)
                         (rec ($lambda-body iform))
                         ($lambda-flag iform))]
     [($LABEL)  (error "[compiler internal] $LABEL node shouldn't appear \
                        in the packed IForm")]
     [($SEQ)    ($seq (imap rec ($seq-body iform)))]
     [($CALL)   ($call ($*-src iform)
                       (rec ($call-proc iform))
                       (imap rec ($call-args iform))
                       #f)]
     [($ASM)    (let* ((host-insn ($asm-insn iform))
                       (target-insn-info (assq (insn-name (car host-insn))
                                               target-insn-alist)))
                  (unless target-insn-info
                    (errorf "[compiler internal] insn ~s doesn't exist \
                             in the target VM" (insn-name (car host-insn))))
                  ($asm ($*-src iform)
                        (cons (ref (cdr target-insn-info)'code)
                              (cdr host-insn))
                        (imap rec ($asm-args iform))))]
     [($PROMISE) ($promise ($*-src iform) (rec ($promise-expr iform)))]
     [($CONS)    ($cons ($*-src iform)
                        (rec ($*-arg0 iform))
                        (rec ($*-arg1 iform)))]
     [($APPEND)  ($append ($*-src iform)
                          (rec ($*-arg0 iform))
                          (rec ($*-arg1 iform)))]
     [($VECTOR)  ($vector ($*-src iform) (imap rec ($*-args iform)))]
     [($LIST->VECTOR) ($list->vector ($*-src iform) (rec ($*-arg0 iform)))]
     [($LIST)   ($list ($*-src iform) (imap rec ($*-args iform)))]
     [($LIST*)  ($list* ($*-src iform) (imap rec ($*-args iform)))]
     [($MEMV)   ($memv ($*-src iform)
                       (rec ($*-arg0 iform))
                       (rec ($*-arg1 iform)))]
     [($EQ?)    ($eq? ($*-src iform)
                      (rec ($*-arg0 iform))
                      (rec ($*-arg1 iform)))]
     [($EQV?)   ($eqv? ($*-src iform)
                       (rec ($*-arg0 iform))
                       (rec ($*-arg1 iform)))]
     [($IT) ($it)]
     [else iform]))

  (pack-iform (rec (unpack-iform src))))

;; See if the given iform is referentially transparent.   That is,
;; the iform is side-effect free, and also the value of iform
;; won't change even if we move iform to a different place in the subtree.
;; NB: This may be called after pass2, so $LABEL node may have circular
;; reference.  We have to be careful not to diverge.
;; TODO: we lift transparent?/rec manually to avoid closure allocation
;; because of the unsophisticated compiler.  Fix this in future.
(define (transparent? iform) (transparent?/rec iform (make-label-dic #f)))
(define (transparent?/rec iform labels)
  (case/unquote
   (iform-tag iform)
   [($LREF)   (lvar-immutable? ($lref-lvar iform))]
   [($GREF)   (gref-inlinable-gloc iform)]
   [($CONST)  #t]
   [($IF)     (and (transparent?/rec ($if-test iform) labels)
                   (transparent?/rec ($if-then iform) labels)
                   (transparent?/rec ($if-else iform) labels))]
   [($LET)    (and (everyc transparent?/rec ($let-inits iform) labels)
                   (transparent?/rec ($let-body iform) labels))]
   [($RECEIVE)(and (transparent?/rec ($receive-expr iform) labels)
                   (transparent?/rec ($receive-body iform) labels))]
   [($LAMBDA) #t]
   [($LABEL)  (or (label-seen? labels iform)
                  (begin (label-push! labels iform)
                         (transparent?/rec ($label-body iform) labels)))]
   [($SEQ)    (everyc transparent?/rec ($seq-body iform) labels)]
   [($CALL)   (and (side-effect-free-proc? ($call-proc iform))
                   (everyc transparent?/rec ($call-args iform) labels))]
   [($ASM)    (and (side-effect-free-insn? ($asm-insn iform))
                   (everyc transparent?/rec ($asm-args iform) labels))]
   [($PROMISE) #t]
   [($CONS $APPEND $MEMV $EQ? $EQV?)
    (and (transparent?/rec ($*-arg0 iform) labels)
         (transparent?/rec ($*-arg1 iform) labels))]
   [($VECTOR $LIST $LIST*) (everyc transparent?/rec ($*-args iform) labels)]
   [($LIST->VECTOR) (transparent?/rec ($*-arg0 iform) labels)]
   [($IT) #t] ; this branch is only executed when $if-test of the parent is
              ; transparent, thus this node is also transparent.
   [else #f]))

(define (side-effect-free-proc? iform) #f) ;for now

(define (side-effect-free-insn? insn)  #f) ;for now

;; Reset lvar reference count.  This is called in pass3,
;; when a subgraph of IForm is eliminated.
(define (reset-lvars iform) (reset-lvars/rec iform (make-label-dic #f)) iform)

(define-macro (reset-lvars/rec* iforms labels)
  `(ifor-each (^[x] (reset-lvars/rec x ,labels)) ,iforms))

(define/case (reset-lvars/rec iform labels)
  (iform-tag iform)
  [($DEFINE) (reset-lvars/rec ($define-expr iform) labels)]
  [($LREF)   (lvar-ref++! ($lref-lvar iform))]
  [($LSET)   (lvar-set++! ($lset-lvar iform))
             (reset-lvars/rec ($lset-expr iform) labels)]
  [($GSET)   (reset-lvars/rec ($gset-expr iform) labels)]
  [($IF)     (reset-lvars/rec ($if-test iform) labels)
             (reset-lvars/rec ($if-then iform) labels)
             (reset-lvars/rec ($if-else iform) labels)]
  [($LET)    (for-each lvar-reset ($let-lvars iform))
             (reset-lvars/rec* ($let-inits iform) labels)
             (reset-lvars/rec ($let-body iform) labels)]
  [($RECEIVE)(for-each lvar-reset ($receive-lvars iform))
             (reset-lvars/rec ($receive-expr iform) labels)
             (reset-lvars/rec ($receive-body iform) labels)]
  [($LAMBDA) (for-each lvar-reset ($lambda-lvars iform))
   (reset-lvars/rec ($lambda-body iform) labels)]
  [($LABEL)  (unless (label-seen? labels iform)
               (label-push! labels iform)
               (reset-lvars/rec ($label-body iform) labels))]
  [($SEQ)    (reset-lvars/rec* ($seq-body iform) labels)]
  [($CALL)   (unless (eq? ($call-flag iform) 'jump)
               (reset-lvars/rec ($call-proc iform) labels))
             (reset-lvars/rec* ($call-args iform) labels)]
  [($ASM)    (reset-lvars/rec* ($asm-args iform) labels)]
  [($PROMISE)(reset-lvars/rec ($promise-expr iform) labels)]
  [($CONS $APPEND $MEMV $EQ? $EQV?)
             (reset-lvars/rec ($*-arg0 iform) labels)
             (reset-lvars/rec ($*-arg1 iform) labels)]
  [($VECTOR $LIST $LIST*) (reset-lvars/rec* ($*-args iform) labels)]
  [($LIST->VECTOR) (reset-lvars/rec ($*-arg0 iform) labels)]
  [else #f])

;; Replaces $LREF of matching lvar with given expression.
;; Used in the transformation of inlined procedure with closed environment.
;;
;; Example:  Suppose we have the following source:
;;  (define-inline (f x y) (lambda (z) (* x (+ y z))))
;;  (define-inline h (f (g) 4))
;; We want to make the closure bound to 'h' inlinable.  That is, when we
;; see (h 5), we want it to be expanded into something like
;;  (* *g-save* (+ 4 5)), where *g-save* is the result of (g) at the time
;; h is defined.   A straight transplantation of IForm wouldn't work, since
;; the $LAMBDA node for the closure bound to 'h' refers to the outer
;; environment.
;; What we do is to eliminate the reference to the outer enviornment by
;; beta-substitution at the time of definition of 'h', as follows:
;;
;;  (define-inline h (f (g) 4))
;;    v  inline f
;;  (define-inline h ((lambda (x y) (lambda (z) (* x (+ y z)))) (g) 4)
;;    v  immediate lambda expansion
;;  (define-inline h (let ((x (g)) (y 4)) (lambda (z) (* x (+ y z)))))
;;    v  we only evaluate (g) once, so bound it to a dummy variable
;;  (define-inline *h$x.1* (g))
;;  (define-inline h (let ((x *h$x.1*) (y 4)) (lambda (z) (* x (+ y z)))))
;;    |  beta-substitution.  It is always safe to substitute y for 4, since
;;    |  it's constant.  And we know nobody else touches *h$x.1* other than
;;    v  'h, so it is also safe to substitute x.
;;  (define-inline *h$x.1* (g))
;;  (define-inline h (lambda (z) (* *h$x.1* (+ 4 z))))
;;
;; subst-lvars does the last step; the MAPPING argument contains alist
;; of (lvar . expr), and we traverse down the IForm to replace $LREF and
;; $LSET to lvar to corresponding expr.
;; NB: We assume this happens in pass1; that is, IForm is a tree, not a DG.

(define (subst-lvars iform mapping)
  (define (subst iform mapping dict)
    (or (hash-table-get dict iform #f)
        (rlet1 r (subst-body iform mapping dict)
          (unless (eq? iform r) (hash-table-put! dict iform r)))))
  (define (subst-body iform mapping dict)
    (case/unquote
     (iform-tag iform)
     [($LREF)   (cond [(assq ($lref-lvar iform) mapping) => cdr]
                      [else iform])]
     [($LSET)   (cond [(assq ($lset-lvar iform) mapping) =>
                       (^p (unless (has-tag? (cdr p) $GREF)
                             (error "[internal] subst-lvars: $LSET can only \
                                     subst with $GREF but got:" (cdr p)))
                           ($gset ($gref-id (cdr p))
                                  (subst ($lset-expr iform) mapping dict)))]
                      [else iform])]
     [($GSET)   (let1 z (subst ($gset-expr iform) mapping dict)
                  (if (eq? z ($gset-expr iform))
                    iform
                    ($gset ($gset-id iform) z)))]
     [($IF)     (let ([test (subst ($if-test iform) mapping dict)]
                      [then (subst ($if-then iform) mapping dict)]
                      [else (subst ($if-else iform) mapping dict)])
                  (if (and (eq? test ($if-test iform))
                           (eq? then ($if-then iform))
                           (eq? else ($if-else iform)))
                    iform
                    ($if ($*-src iform) test then else)))]
     [($LET)    (let ([is (imap (cut subst <> mapping dict) ($let-inits iform))]
                      [vs ($let-lvars iform)]
                      [b (subst ($let-body iform) mapping dict)])
                  (ifor-each2 (^[v i] (lvar-initval-set! v i)) vs is)
                  ($let ($*-src iform) ($let-type iform) vs is b))]
     [($RECEIVE)(let ([x (subst ($receive-expr iform) mapping dict)]
                      [b (subst ($receive-body iform) mapping dict)])
                  (if (and (eq? x ($receive-expr iform))
                           (eq? b ($receive-body iform)))
                    iform
                    ($receive ($*-src iform)
                              ($receive-reqargs iform)
                              ($receive-optarg iform)
                              ($receive-lvars iform) x b)))]
     [($LAMBDA) (let1 b (subst ($lambda-body iform) mapping dict)
                  (if (eq? b ($lambda-body iform))
                    iform
                    ($lambda ($*-src iform) ($lambda-name iform)
                             ($lambda-reqargs iform) ($lambda-optarg iform)
                             ($lambda-lvars iform) b ($lambda-flag iform))))]
     [($SEQ)    ($seq (imap (cut subst <> mapping dict) ($seq-body iform)))]
     [($CALL)   ($call ($*-src iform)
                       (subst ($call-proc iform) mapping dict)
                       (imap (cut subst <> mapping dict) ($call-args iform))
                       ($call-flag iform))]
     [($ASM)    ($asm ($*-src iform) ($asm-insn iform)
                      (imap (cut subst <> mapping dict) ($asm-args iform)))]
     [($PROMISE)($promise ($*-src iform)
                          (subst ($promise-expr iform) mapping dict))]
     [($CONS $APPEND $MEMV $EQ? $EQV?) (subst/2 iform mapping dict)]
     [($VECTOR $LIST $LIST*) (subst/* iform mapping dict)]
     [($LIST->VECTOR) ($list->vector ($*-src iform)
                                     (subst ($*-arg0 iform) mapping dict))]
     [else iform]))
  (define (subst/2 iform mapping dict)
    (vector (vector-ref iform 0) ($*-src iform)
            (subst ($*-arg0 iform) mapping dict)
            (subst ($*-arg1 iform) mapping dict)))
  (define (subst/* iform mapping dict)
    (vector (vector-ref iform 0) ($*-src iform)
            (imap (cut subst <> mapping dict) ($*-args iform))))
  (subst iform mapping (make-hash-table 'eq?)))

;; EXPERIMENTAL
;; Convert IForm back to S-expr.
;; This is used by macroexpand-all.  Note that pass2 can make IForm DG, and
;; it's not generally possible to convert it back to S-expr.  You can only
;; pass the IForm immediately after pass1.
(define (iform->sexpr iform)
  (define lvar-dict (make-hash-table 'eq?)) ;; lvar -> symbol
  (define (get-lvar lvar)
    (or (hash-table-get lvar-dict lvar #f)
        (let* ([name (lvar-name lvar)]
               [name (if (identifier? name) (identifier->symbol name) name)])
          (rlet1 s ($ string->symbol $ format "~a.~d" name
                      $ hash-table-num-entries lvar-dict)
            (hash-table-put! lvar-dict lvar s)))))
  (define (rec iform)
    (case/unquote
     (iform-tag iform)
     [($DEFINE) `(define ,($define-id iform) ,(rec ($define-expr iform)))]
     [($LREF)   (get-lvar ($lref-lvar iform))]
     [($LSET)   `(set! ,(get-lvar ($lset-lvar iform))
                       ,(rec ($lset-expr iform)))]
     [($GREF)   ($gref-id iform)]
     [($GSET)   `(set! ,($gset-id iform) ,(rec ($gset-expr iform)))]
     [($CONST)  `',($const-value iform)]
     [($IF)     (cond [(has-tag? ($if-then iform) $IT)
                       `(or ,(rec ($if-test iform))
                            ,(rec ($if-else iform)))]
                      [(has-tag? ($if-else iform) $IT)
                       `(and ,(rec ($if-test iform))
                             ,(rec ($if-then iform)))]
                      [else
                       `(if ,(rec ($if-test iform))
                          ,(rec ($if-then iform))
                          ,(rec ($if-else iform)))])]
     [($LET)    (let ([is (map rec ($let-inits iform))]
                      [vs (map get-lvar ($let-lvars iform))])
                  `(letrec ,(map list vs is)
                     ,(rec ($let-body iform))))]
     [($RECEIVE) (let* ([vars (map get-lvar ($receive-lvars iform))]
                        [binds (if (zero? ($receive-optarg iform))
                                 vars
                                 (apply list* vars))])
                   `(receive ,binds ,(rec ($receive-expr iform))
                      ,(rec ($receive-body iform))))]
     [($LAMBDA)  (let* ([vars (map get-lvar ($lambda-lvars iform))]
                        [formals (if (zero? ($lambda-optarg iform))
                                   vars
                                   (apply list* vars))])
                   `(lambda ,formals ,(rec ($lambda-body iform))))]
     [($SEQ)    `(begin ,@(map rec ($seq-body iform)))]
     [($CALL)   `(,(rec ($call-proc iform)) ,@(map rec ($call-args iform)))]
     [($ASM)    `(asm ,($asm-insn iform)
                      ,@(map rec ($asm-args iform)))]
     [($CONS)   `(cons ,(rec ($*-arg0 iform)) ,(rec ($*-arg1 iform)))]
     [($APPEND) `(append ,(rec ($*-arg0 iform)) ,(rec ($*-arg1 iform)))]
     [($MEMV)   `(memv ,(rec ($*-arg0 iform)) ,(rec ($*-arg1 iform)))]
     [($EQ?)    `(eq? ,(rec ($*-arg0 iform)) ,(rec ($*-arg1 iform)))]
     [($EQV?)   `(eqv? ,(rec ($*-arg0 iform)) ,(rec ($*-arg1 iform)))]
     [($VECTOR) `(vector ,@(map rec ($*-args iform)))]
     [($LIST)   `(list ,@(map rec ($*-args iform)))]
     [($LIST*)  `(list* ,@(map rec ($*-args iform)))]
     [($LIST->VECTOR) `(list->vector ,(rec ($*-arg0 iform)))]
     [($IT) ; ($IT) node must have been handled by $IF
      (error "[internal] Stray $IT node.")]
     [else (error "Cannot convert IForm:" (iform-tag-name (iform-tag iform)))]))
  (rec iform))

;; Some inline stuff
(define-inline (pass2-4 iform module) (pass4 (pass3 (pass2 iform) #f) module))

;;============================================================
;; Entry points
;;

;; compile:: Sexpr, Env -> CompiledCode
;;   Env can be:
;;     #f, #<unbound> - compile on bottom env of the current module
;;     module - compile on bottom env of the given module
;;     cenv   - compile on the given cenv
(define (compile program env)
  (let1 cenv (cond [(module? env) (make-bottom-cenv env)]
                   [(vector? env) env] ; assumes env is cenv
                   [else (make-bottom-cenv)]) ; use default module
    (guard (e [else
               ;; TODO: check if e is an expected error (such as syntax error)
               ;; or an unexpected error (compiler bug).
               ($ raise $ make-compound-condition e
                  $ make <compile-error-mixin> :expr program)])
      (pass5 (pass2-4 (pass1 program cenv) (cenv-module cenv))
             (make-compiled-code-builder 0 0 '%toplevel #f #f)
             '() 'tail))))

;; stub for future extension
(define (compile-partial program module) #f)
(define (compile-finish cc) #f)

;; For testing
(define (compile-p1 program :optional (env (vm-current-module)))
  (pp-iform (pass1 program (make-bottom-cenv env))))

(define (compile-p2 program :optional (env (vm-current-module)))
  (pp-iform (pass2 (pass1 program (make-bottom-cenv env)))))

(define (compile-p3 program :optional (show? #f) (env (vm-current-module)))
  (pp-iform (pass3 (pass2 (pass1 program (make-bottom-cenv env))) show?)))

(define (compile-p4 program :optional (env (vm-current-module)))
  (let1 cenv (make-bottom-cenv env)
    (pp-iform (pass2-4 (pass1 program cenv) (cenv-module cenv)))))

(define (compile-p5 program :optional (env (vm-current-module)))
  (let1 cenv (make-bottom-cenv env)
    (vm-dump-code (pass5 (pass2-4 (pass1 program cenv) (cenv-module cenv))
                         (make-compiled-code-builder 0 0 '%toplevel #f #f)
                         '() 'tail))))

(include "compile-1")
(include "compile-2")
(include "compile-3")
(include "compile-4")
(include "compile-5")
(include "compile-i")

;;============================================================
;; Macro support basis
;;

;; free-identifier=? id1 id2
;; Returns #t iff id1 and id2 would resolve to the same binding
;; (or both are free).

(select-module gauche)
(inline-stub
 (define-cfn %free-identifier=? (id1 id2) ::int :static
   ;; Eliminate trivial cases first
   (unless (and (SCM_IDENTIFIERP id1) (SCM_IDENTIFIERP id2)) (return FALSE))
   (when (SCM_EQ id1 id2) (return TRUE))
   ;; Look at their binidngs
   (let* ([b1 (env-lookup-int id1 (SCM_MAKE_INT 1) 
                              (-> (SCM_IDENTIFIER id1) module)
                              (Scm_IdentifierEnv (SCM_IDENTIFIER id1)))]
          [b2 (env-lookup-int id2 (SCM_MAKE_INT 1) 
                              (-> (SCM_IDENTIFIER id2) module)
                              (Scm_IdentifierEnv (SCM_IDENTIFIER id2)))])
     (if (and (SCM_IDENTIFIERP b1) (SCM_IDENTIFIERP b2))
       (let* ([g1::ScmGloc* (Scm__IdentifierToBoundGloc (SCM_IDENTIFIER id1))]
              [g2::ScmGloc* (Scm__IdentifierToBoundGloc (SCM_IDENTIFIER id2))])
         ;; If both has bound in toplevel, they must refer to the
         ;; same binding, hence (eq? g1 g2).  The name may differ,
         ;; because of renaming on export/import.
         ;; If at least either one is unbound, we just compare their names.
         (if (and g1 g2)
           (return (SCM_EQ g1 g2))
           (return (SCM_EQ (Scm_UnwrapSyntax id1) (Scm_UnwrapSyntax id2)))))
       ;; At least one of id1 and id2 is lvar or macro.
       (return (SCM_EQ b1 b2)))))

 (define-cproc free-identifier=? (id1 id2) ::<boolean> 
   %free-identifier=?)
 )

(select-module gauche.internal)

;; er-rename :: (Form, [Id]) -> (Form, [Id])
;; Where symbol or identifiers in the Form will be replaced with
;; fresh identifiers.   If Form has more than one occurence of a
;; specific symbol-or-id, the resulting Form has the same (eq?)
;; identifier for those occurrences.
(define (er-rename form dict module env)
  (cond
   [(variable? form)
    (if-let1 id (assq-ref dict form)
      (values id dict)
      (let1 id (make-identifier form module env)
        (values id (acons form id dict))))]
   [(pair? form)
    (receive (a dict) (er-rename (car form) dict module env)
      (receive (d dict) (er-rename (cdr form) dict module env)
        (if (and (eq? a (car form)) (eq? d (cdr form)))
          (values form dict)
          (values (cons a d) dict))))]
   [(vector? form)
    (let loop ([i 0] [vec #f] [dict dict])
      (if (= i (vector-length form))
        (values (or vec form) dict)
        (receive (e dict) (er-rename (vector-ref form i) dict module env)
          (if (eq? e (vector-ref form i))
            (loop (+ i 1) vec dict)
            (let1 vec (or vec (vector-copy form))
              (vector-set! vec i e)
              (loop (+ i 1) vec dict))))))]
   [else (values form dict)]))

;; er-compare :: (Obj, Obj, Module, Env) -> Bool
;; Used for the 'compare' procedure in ER macro.  If two Objs refer
;; to the same identifiers, return #t.
;; We implement it in C for speed; it is also called from syntax-rules
;; expander.
(inline-stub
 (define-cfn Scm__ERCompare (a b module::ScmModule* frames) ::int
   (cond [(and (or (SCM_SYMBOLP a) (SCM_IDENTIFIERP a))
               (or (SCM_SYMBOLP b) (SCM_IDENTIFIERP b)))
          (let* [(a1 (env-lookup-int a (SCM_MAKE_INT 1) module frames))
                 (b1 (env-lookup-int b (SCM_MAKE_INT 1) module frames))]
            (when (SCM_EQ a1 b1) (return TRUE))
            (unless (and (SCM_IDENTIFIERP a1) (SCM_IDENTIFIERP b1)) (return FALSE))
            (return (%free-identifier=? a1 b1)))]
         [(and (SCM_KEYWORDP a) (SCM_KEYWORDP b))
          (return (SCM_EQ a b))]
         [else (return FALSE)]))

 (define-cproc er-compare (a b use-module::<module> use-frames) ::<boolean>
   Scm__ERCompare)
 )

;; xformer :: (Sexpr, (Sym -> Sym), (Sym, Sym -> Bool)) -> Sexpr
(define (%make-er-transformer xformer def-env)
  (define def-module (cenv-module def-env))
  (define def-frames (cenv-frames def-env))
  (define (expand form use-env)
    (define use-module (cenv-module use-env))
    (define use-frames (cenv-frames use-env))
    (let1 dict '()
      (xformer form
               (^[sym]
                 (receive [id dict_] (er-rename sym dict def-module def-frames)
                   (set! dict dict_)
                   id))
               (^[a b] (er-compare a b use-module use-frames)))))
  (%make-macro-transformer (cenv-exp-name def-env) expand))

;; Call to this procedure can be inserted in the output of er-macro expander
;; and emitted in the precompiled file, so do not change this API unless
;; you're sure no existing precompiled code refers to this.
(define (%make-er-transformer/toplevel xformer def-module def-name)
  (%make-er-transformer xformer (%make-cenv def-module '() def-name)))

;; TRANSIENT: We no longer use these procedures, but reference to them
;; might be inserted in the file precompiled by 0.9.5 compiler as the
;; result of expansion of er macro.
;; Remove this on 1.0 release.
(define (%make-toplevel-cenv name) ;internal
  (%make-cenv (vm-current-module) '() name))
(define (%make-primitive-transformer xformer def-env)
  (%make-macro-transformer (cenv-exp-name def-env)
                           (^[form use-env] (xformer form def-env use-env))))
(define (%expand-er-transformer form cenv)
  (match form
    [(_ xformer)
     ;; See the discussion in primitive-macro-transformer above.
     (let1 def-env-form (if (null? (cenv-frames cenv))
                          `(,%make-toplevel-cenv. ',(cenv-exp-name cenv))
                          cenv)
       `(,%make-er-transformer. ,xformer ,def-env-form))]
    [_ (error "Invalid er-macro-transformer form:" form)]))


;; EXPERIMENTAL
;; Returns an S-expr all macros in which are expanded.
;; The resulting form may not be equivalent to the input form, though,
;; since we strip off identifier information so toplevel hygiene isn't
;; kept.  (Local variables are renamed so they won't conflict with each
;; other.)
(define-in-module gauche (macroexpand-all form)
  (let1 flags-save (vm-compiler-flag)
    (unwind-protect
        (begin
          ;; TODO: We suppress global inline expansion, otherwise we'll see
          ;; $ASM nodes.  NB: This also suppress expansion of define-inline'd
          ;; procedures and compiler macros, which may not be desirable.
          ;; We'll think of it later.
          (vm-compiler-flag-set! SCM_COMPILE_NOINLINE_GLOBALS)
          ($ unwrap-syntax $ iform->sexpr
             $ pass1 form (make-bottom-cenv (vm-current-module))))
      (begin
        (vm-compiler-flag-clear! SCM_COMPILE_NOINLINE_GLOBALS)
        (vm-compiler-flag-set! flags-save)))))

;;--------------------------------------------------------
;; Customizable inliner interface
;;

;; This is invoked from the expansion of define-inline/syntax.
(define (%with-inline-transformer proc xformer)
  (unless (procedure? proc)
    (error "define-inline/syntax: expression yields non-procedure value" proc))
  (unless (macro? xformer)
    (errorf "macro required, but got" xformer))
  ;; Copy PROC, in case the given proc refers to already defined global
  ;; procedure.  We don't want to modify existing procedure instance.
  (rlet1 p (%procedure-copy proc)
    (set! (%procedure-inliner p) xformer)))


;;============================================================
;; Utilities
;;

;; Keep track of visited $LABEL node while traversing IForm.  The lookup
;; is only done when we hit $LABEL node, so the performance is not so critical.
;; The CAR of label-dic isn't used to keep label info, and may be used by
;; the caller to keep extra info.
(define (make-label-dic init) (list init))
(define (label-seen? label-dic label-node) (memq label-node (cdr label-dic)))
(define (label-push! label-dic label-node) (push! (cdr label-dic) label-node))
(define (label-dic-info label-dic) (car label-dic))
(define (label-dic-info-set! label-dic val) (set-car! label-dic val))
(define (label-dic-info-push! label-dic val) (push! (car label-dic) val))

;; see if the immediate integer value fits in the insn arg.
(define (integer-fits-insn-arg? obj)
  (and (integer? obj)
       (exact? obj)
       (<= #x-7ffff obj #x7ffff)))

(define (unsigned-integer-fits-insn-arg? obj)
  (and (integer? obj)
       (exact? obj)
       (<= 0 obj #x7ffff)))

(define (variable-name arg)
  (cond [(symbol? arg) arg]
        [(identifier? arg) (unwrap-syntax arg)]
        [(lvar? arg) (lvar-name arg)]
        [else (error "variable required, but got:" arg)]))

;; When the compiler transforms a source ORIGINAL to CONSTRUCTED,
;; we attach the original source info to the initial pair of the
;; constructed source.
(define (with-original-source constructed original)
  (if (pair? constructed)
    (rlet1 p (if (extended-pair? constructed)
               constructed
               (extended-cons (car constructed) (cdr constructed)))
      (pair-attribute-set! p 'original original))
    constructed))

;; GLOBAL-CALL-TYPE
;;
;;   This is an aux call to dispatch the function call with global variable
;;   reference in its first position (in pass1/global-call).  It checks
;;   up the binding of global identifier, and returns two values.
;;
;;   The first value is #f if this global call would be a simple call.
;;   Otherwise, the first value is a bound value of the global variable,
;;   and the second value is one of the symbols 'macro, 'syntax or 'inline.
;;
;;   We write this in C because it is in critical path of the compiler.
;;
;;   {Experimental}
;;   If RECORD_DEPENDED_MODULES is defined, this proc also records
;;   modules from which macro/syntax identifiers come.  It will be used
;;   in test-module to detect unnecessarily 'use'd modules.  For normal
;;   procedure we can scan identifier/glocs embedded in the VM code
;;   vector, but for the case of macros/syntaxes/inlined procedures
;;   the original identifier is lost during expansion, so we need to
;;   record it at compile time.
;;   Although global-call-type is in critical path, my benchmark showed
;;   little impact from this addition.
;;   I still don't feel "right" about having this hack here, though.
;;   Just keep it for now, for the record.
(inline-stub
 (if "defined(RECORD_DEPENDED_MODULES)"
   (begin
     "static ScmModule *stdmods[3];"

     (initcode
      "stdmods[0] = Scm_NullModule();"
      "stdmods[1] = Scm_SchemeModule();"
      "stdmods[2] = Scm_GaucheModule();")))

 (define-cproc global-call-type (id cenv) ::(<top> <top>)
   (let* ([mod::ScmModule* (-> (SCM_IDENTIFIER id) module)]
          [gloc::ScmGloc* (Scm_IdentifierGlobalBinding (SCM_IDENTIFIER id))])
     (set! SCM_RESULT0 '#f SCM_RESULT1 '#f)
     (when gloc
       (let* ([gval (SCM_GLOC_GET gloc)])
         (cond [(SCM_MACROP gval)
                (set! SCM_RESULT0 gval SCM_RESULT1 'macro)]
               [(SCM_SYNTAXP gval)
                (set! SCM_RESULT0 gval SCM_RESULT1 'syntax)]
               [(and (SCM_PROCEDUREP gval)
                     (SCM_PROCEDURE_INLINER gval) ; inliner may be NULL
                     (not (SCM_FALSEP (SCM_PROCEDURE_INLINER gval)))
                     (Scm_GlocInlinableP gloc)
                     (not (SCM_VM_COMPILER_FLAG_IS_SET
                           (Scm_VM) SCM_COMPILE_NOINLINE_GLOBALS)))
                (set! SCM_RESULT0 gval SCM_RESULT1 'inline)]
               [else (goto normal)])
         (.if "defined(RECORD_DEPENDED_MODULES)"
              (begin
                (dotimes [i 3]
                  (when (SCM_EQ mod (aref stdmods i)) (goto normal)))
                (let* ([curmod (SCM_VECTOR_ELEMENT cenv 0)])
                  (SCM_ASSERT (SCM_MODULEP curmod))
                  (when (SCM_FALSEP
                         (Scm_Memq (SCM_OBJ mod)
                                   (-> (SCM_MODULE curmod) depended)))
                    (set! (-> (SCM_MODULE curmod) depended)
                          (Scm_Cons (SCM_OBJ mod)
                                    (-> (SCM_MODULE curmod) depended)))))))
         ))
     (label normal)
     ))
 )

;; Returns #t iff z is a keyword, or an identifier made from a keyword.
;; TODO: We should also check iff k isn't bound to something else!
(define (keyword-like? k)
  (or (keyword? k)
      (and (identifier? k) (keyword? (identifier->symbol k)))))

(define (global-eq? var id cenv)  ; like free-identifier=?, used in pass1.
  (and (variable? var)
       (let1 v (cenv-lookup-variable cenv var)
         (and (identifier? v)
              (global-identifier=? v id)))))

;; TRANSIENT: To compare keyword hygienically.  We need to treat keywords
;; specially until we complete keyword-symbol integration.
(define (global-keyword-eq? var key cenv)
  (or (eq? var key)
      (and (symbol? key)
           (identifier? var)
           (global-eq? var (global-id key) cenv))))

(define (everyc proc lis c)             ;avoid closure allocation
  (or (null? lis)
      (let loop ((lis lis))
        (match lis
          [(x) (proc x c)]
          [(x . xs) (and (proc x c) (loop xs))]))))

;; Check if iform is a lambda node that has no side effects.
(define (constant-lambda? iform)
  (and (vector? iform)
       (has-tag? iform $LAMBDA)
       (transparent? ($lambda-body iform))))

;; To compare identifiers w/ hygiene.  Returns a predicate that takes
;; a single argument VAR, which must be a symbol or identifier that
;; appears in the toplevel environment with a module that are returned by
;; calling a thunk MODGEN.  The predicate returns #t iff VAR represents
;; the same global binding of SYM within the module SRCMOD.
;; This is used in precomp.
(define (global-eq?? sym srcmod modgen)
  (let1 id-gloc (find-binding (find-module srcmod) sym #f)
    (^[var] (eq? id-gloc
                 (if (identifier? var)
                   (find-binding (slot-ref var'module) (slot-ref var'name) #f)
                   (find-binding (modgen) var #f))))))

;; Some C routines called from the expanded macro result.
;; These procedures are originally implemented in Scheme, but moved
;; here for efficiency.
(inline-stub
 ;; %imax - max for unsigned integer only, unsafe.
 (define-cproc %imax (x y)
   (if (> (SCM_WORD x) (SCM_WORD y)) (return x) (return y)))

 ;; (%map1c proc lis c) = (map (cut proc <> c) lis)
 (define-cfn map1c_cc (result data::void**) :static
   (let* ([proc (SCM_OBJ (aref data 0))]
          [r    (SCM_OBJ (aref data 1))]
          [lis  (SCM_OBJ (aref data 2))]
          [c    (SCM_OBJ (aref data 3))])
     (cond [(SCM_NULLP lis) (return (Scm_ReverseX (Scm_Cons result r)))]
           [else
            (set! (aref data 1) (Scm_Cons result r)
                  (aref data 2) (SCM_CDR lis))
            (Scm_VMPushCC map1c_cc data 4)
            (return (Scm_VMApply2 proc (SCM_CAR lis) c))])))

 (define-cproc %map1c (proc lis c)
   (let* ([data::(.array void* [4])])
     (cond [(SCM_NULLP lis) (return SCM_NIL)]
           [else
            (set! (aref data 0) proc
                  (aref data 1) SCM_NIL
                  (aref data 2) (SCM_CDR lis)
                  (aref data 3) c)
            (Scm_VMPushCC map1c_cc data 4)
            (return (Scm_VMApply2 proc (SCM_CAR lis) c))])))

 ;; (%map1cc proc lis c1 c2) = (map (cut proc <> c1 c2) lis)
 (define-cfn map1cc-cc (result (data :: void**)) :static
   (let* ([proc (SCM_OBJ (aref data 0))]
          [r    (SCM_OBJ (aref data 1))]
          [lis  (SCM_OBJ (aref data 2))]
          [c1   (SCM_OBJ (aref data 3))]
          [c2   (SCM_OBJ (aref data 4))])
     (cond [(SCM_NULLP lis) (return (Scm_ReverseX (Scm_Cons result r)))]
           [else
            (set! (aref data 1) (Scm_Cons result r)
                  (aref data 2) (SCM_CDR lis))
            (Scm_VMPushCC map1cc-cc data 5)
            (return (Scm_VMApply3 proc (SCM_CAR lis) c1 c2))])))

 (define-cproc %map1cc (proc lis c1 c2)
   (if (SCM_NULLP lis)
     (return SCM_NIL)
     (let* ([data::(.array void* [5])])
       (set! (aref data 0) proc
             (aref data 1) SCM_NIL
             (aref data 2) (SCM_CDR lis)
             (aref data 3) c1
             (aref data 4) c2)
       (Scm_VMPushCC map1cc-cc data 5)
       (return (Scm_VMApply3 proc (SCM_CAR lis) c1 c2)))))

 ;; (%map-cons lis1 lis2) = (map cons lis1 lis2)
 (define-cproc %map-cons (lis1 lis2)
   (let* ([h SCM_NIL] [t SCM_NIL])
     (while (and (SCM_PAIRP lis1) (SCM_PAIRP lis2))
       (SCM_APPEND1 h t (Scm_Cons (SCM_CAR lis1) (SCM_CAR lis2)))
       (set! lis1 (SCM_CDR lis1) lis2 (SCM_CDR lis2)))
     (return h)))
 )

;;============================================================
;; VM introspection
;;

(inline-stub
 ;; standard
 (define-constant ENV_HEADER_SIZE  (c "SCM_MAKE_INT(ENV_SIZE(0))"))
 (define-constant CONT_FRAME_SIZE (c "SCM_MAKE_INT(CONT_FRAME_SIZE)"))

 ;; Eval situation flag (for eval-when constrcut)
 (define-cproc vm-eval-situation (:optional val) ::<int>
   (let* ([vm::ScmVM* (Scm_VM)])
     (cond [(SCM_UNBOUNDP val) (return (-> vm evalSituation))]
           [else
            (unless (SCM_INTP val) (SCM_TYPE_ERROR val "integer"))
            (let* ([prev::int (-> vm evalSituation)])
              (set! (-> vm evalSituation) (SCM_INT_VALUE val))
              (return prev))])))

 (define-enum SCM_VM_EXECUTING)
 (define-enum SCM_VM_LOADING)
 (define-enum SCM_VM_COMPILING)

 ;; Compiler flags
 (define-cproc vm-compiler-flag-is-set? (flag::<uint>) ::<boolean>
   (return (SCM_VM_COMPILER_FLAG_IS_SET (Scm_VM) flag)))
 (define-cproc vm-compiler-flag-set! (flag::<uint>) ::<void>
   (SCM_VM_COMPILER_FLAG_SET (Scm_VM) flag))
 (define-cproc vm-compiler-flag-clear! (flag::<uint>) ::<void>
   (SCM_VM_COMPILER_FLAG_CLEAR (Scm_VM) flag))
 (define-cproc vm-compiler-flag () ::<uint>
   (result (-> (Scm_VM) compilerFlags)))

 (define-cproc vm-compiler-flag-noinline-locals? () ::<boolean>
   (return (SCM_VM_COMPILER_FLAG_IS_SET (Scm_VM) SCM_COMPILE_NOINLINE_LOCALS)))
 (define-cproc vm-compiler-flag-no-post-inline? () ::<boolean>
   (return (SCM_VM_COMPILER_FLAG_IS_SET (Scm_VM) SCM_COMPILE_NO_POST_INLINE_OPT)))
 (define-cproc vm-compiler-flag-no-lifting? () ::<boolean>
   (return (SCM_VM_COMPILER_FLAG_IS_SET (Scm_VM) SCM_COMPILE_NO_LIFTING)))

 (define-enum SCM_COMPILE_NOINLINE_GLOBALS)
 (define-enum SCM_COMPILE_NOINLINE_LOCALS)
 (define-enum SCM_COMPILE_NOINLINE_CONSTS)
 (define-enum SCM_COMPILE_NOINLINE_SETTERS)
 (define-enum SCM_COMPILE_NOSOURCE)
 (define-enum SCM_COMPILE_SHOWRESULT)
 (define-enum SCM_COMPILE_NOCOMBINE)
 (define-enum SCM_COMPILE_NO_POST_INLINE_OPT)
 (define-enum SCM_COMPILE_NO_LIFTING)
 (define-enum SCM_COMPILE_INCLUDE_VERBOSE)
 (define-enum SCM_COMPILE_ENABLE_CEXPR)

 ;; Set/get VM's current module info. (temporary)
 (define-cproc vm-current-module () (return (SCM_OBJ (-> (Scm_VM) module))))
 (define-cproc vm-set-current-module (mod::<module>) ::<void>
   (set! (-> (Scm_VM) module) mod))
 )

;;============================================================
;; Initialization
;;

(define (init-compiler)
  #f
  )

;;============================================================
;; Dummy macros
;;

(define (%precomp-only name)
  (warn "The ~a form can only be used for Scheme sources \
         to be pre-compiled.  Since you're loading the file without \
         pre-compilation, this form is ignored (Current module=~s)"
        name (current-module))
  (undefined))

(select-module gauche)

(declare (keep-private-macro inline-stub define-cproc declare))

;; Those forms are recognized by the AOT compiler (precomp), and
;; translated to C.  They can't be evaluated at runtime, so
;; we have dummy macros to warn.
(define-macro (inline-stub . _)
  ((with-module gauche.internal %precomp-only) 'inline-stub))
(define-macro (define-cproc . _)
  ((with-module gauche.internal %precomp-only) 'define-cproc))
(define-macro (define-cfn . _)
  ((with-module gauche.internal %precomp-only) 'define-cfn))
(define-macro (define-enum . _)
  ((with-module gauche.internal %precomp-only) 'define-enum))
(define-macro (define-enum-conditionally . _)
  ((with-module gauche.internal %precomp-only) 'define-enum-conditionally))


;; The form (declare ...) may be used in wider purpose.  For the time
;; being we use it in limited purposes for compilers.  In interpreter
;; we just ignore it.
(define-macro (declare . _) #f)
