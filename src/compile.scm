;;;
;;; compile.scm - The compiler
;;;
;;;   Copyright (c) 2004-2015  Shiro Kawai  <shiro@acm.org>
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
;;;       that does not need to form a closure.  They are assigned to
;;;       fresh global identifier ($lambda-lifted-var is set to it).
;;;       References to this $lambda node through local variables are
;;;       substituted to the reference of this global identifier.
;;;
;;;   Pass 5 (Code generation):
;;;     - Traverses IForm and generate VM instructions.
;;;     - Perform instruction combining.
;;;     - Perform simple-minded jump optimization.
;;;     - Some final constant foldings are done here, too.
;;;     - NB: This pass modifies $LABEL node while traversing IForm:
;;;       $label-label is set to the label number.

(include "compile-0")

;;=====================================================================
;; Compile-time constants
;;

;; used by env-lookup-int
;; NB: We'll get rid of PATTERN variable lookup after we replace the
;; macro system, then the distinction of LEXICAL/SYNTAX lookup can be
;; specified by a boolean flag and we can drop these constants.
(eval-when (:compile-toplevel)
  (define-constant LEXICAL 0)
  (define-constant SYNTAX  1)
  (define-constant PATTERN 2))

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
;;                0          <lvar>     ;; lexical binding
;;                1          <macro>    ;; syntactic binding
;;                2          <pvar>     ;; pattern variable
;;
;;                Constants LEXICAL, SYNTAX and PATTERN are defined
;;                to represent <type> for the convenience.
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
             (set! frames (-> (SCM_IDENTIFIER y) env)))
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
 ;;
 ;; (define (cenv-toplevel? cenv)
 ;;   (not (any (lambda (frame) (eqv? (car frame) LEXICAL))
 ;;             (cenv-frames cenv))))
 ;;
 (define-cproc cenv-toplevel? (cenv)
   (SCM_ASSERT (SCM_VECTORP cenv))
   (dolist [fp (SCM_VECTOR_ELEMENT cenv 1)]
     (if (== (SCM_CAR fp) '0) (return '#f)))
   (return '#t))
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
     [($IF)     (if (has-tag? ($if-then iform) $IT)
                  `(or ,(rec ($if-test iform))
                       ,(rec ($if-else iform)))
                  `(if ,(rec ($if-test iform))
                     ,(rec ($if-then iform))
                     ,(rec ($if-else iform))))]
     [($LET)    (let ([is (map rec ($let-inits iform))]
                      [vs (map get-lvar ($let-lvars iform))])
                  `(letrec ,(map list vs is)
                     ,(rec ($let-body iform))))]
     [($RECEIVE) (let* ([vars (map get-lvar ($receive-lvars iform))]
                        [binds (if (zero? ($receive-optarg iform))
                                 vars
                                 (apply list* vars))])
                   `(receive ,vars ,(rec ($receive-expr iform))
                      ,(rec ($receive-body iform))))]
     [($LAMBDA)  (let* ([vars (map get-lvar ($lambda-lvars iform))]
                        [formals (if (zero? ($lambda-optarg iform))
                                   vars
                                   (apply list* vars))])
                   `(lambda ,vars ,(rec ($lambda-body iform))))]
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
     [($IT)     '(it)] ;; TODO
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
             (make-compiled-code-builder 0 0 '%toplevel #f #f #f)
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
                         (make-compiled-code-builder 0 0 '%toplevel #f #f #f)
                         '() 'tail))))

;;===============================================================
;; Pass 1
;;
;;   Converts S-expr to IForm.  Macros are expanded.  Variable references
;;   are resolved and converted to either $lref or $gref.  The constant
;;   variable references (defined by define-constant) are converted to
;;   its values at this stage.

;; Common entry to handle procedure call
;; proc is IForm.  args is [Sexpr].
(define-inline (pass1/call program proc args cenv)
  (cond
   [(has-tag? proc $LAMBDA)        ; immediate lambda
    (expand-inlined-procedure program proc (imap (cut pass1 <> cenv) args))]
   [(null? args) ($call program proc '())] ; fast path
   [(> (length args) MAX_LITERAL_ARG_COUNT)
    (errorf "Too many arguments in the call of `~,,,,40s...'" program)]
   [else (let1 cenv (cenv-sans-name cenv)
           ($call program proc (imap (cut pass1 <> cenv) args)))]))

;; Check if the head of the list is a variable, and if so, lookup it.
;; Note that we need to detect the case ((with-module foo bar) arg ...)
;; NB: This isn't a proper fix, for we cannot deal with the situation
;; like nested or aliased with-modules.  The Right Thing is to run
;; `pass1 for syntax' on (car PROGRAM) and check the result to see if
;; we need to treat PROGRAM as a special form or an ordinary procedure.
;; It would be a large change, so this is a compromise...
(define-inline (pass1/lookup-head head cenv)
  (or (and (variable? head)
           (cenv-lookup-syntax cenv head))
      (and (pair? head)
           (module-qualified-variable? head cenv)
           (let1 mod (ensure-module (cadr head) 'with-module #f)
             (cenv-lookup-syntax (cenv-swap-module cenv mod) (caddr head))))))

;;--------------------------------------------------------------
;; pass1 :: Sexpr, Cenv -> IForm
;;
;;  The Pass 1 entry point.
;;  This is one of the most frequently called routine.  It is critical to
;;  make sure all internal functions are inlined, in case you
;;  change something.
(define (pass1 program cenv)

  ;; Handle a global call.  PROGRAM's car is resolved to an identifier, ID.
  ;; We know PROGRAM is a call to global procedure, macro, or syntax.
  (define (pass1/global-call id)
    (receive (gval type) (global-call-type id cenv)
      (if gval
        (case type
          [(macro)
           (pass1 (call-macro-expander gval program cenv) cenv)]
          [(syntax)
           (call-syntax-handler gval program cenv)]
          [(inline)
           (pass1/expand-inliner id gval)]
          )
        (pass1/call program ($gref id) (cdr program) cenv))))

  ;; Expand inlinable procedure.  Inliner may be...
  ;;   - An integer.  This must be the VM instruction number.
  ;;     (It is useful to initialize the inliner statically in .stub file).
  ;;   - A vector.  This must be a packed intermediate form.  It is set if
  ;;     the procedure is defined by define-inline.
  ;;   - A procedure.   It is called like a macro expander.
  ;;     It may return #<undef> to cancel inlining.
  (define (pass1/expand-inliner name proc)
    ;; TODO: for inline asm, check validity of opcode.
    (let1 inliner (%procedure-inliner proc)
      (match inliner
       [(? integer?)                    ;VM insn
        (let ([nargs (length (cdr program))]
              [opt?  (slot-ref proc 'optional)])
          (unless (argcount-ok? (cdr program) (slot-ref proc 'required) opt?)
            (errorf "wrong number of arguments: ~a requires ~a, but got ~a"
                    (variable-name name) (slot-ref proc 'required) nargs))
          ;; We might get away with this limit by transforming inline calls
          ;; to apply or something.  Maybe in future.
          (when (> nargs MAX_LITERAL_ARG_COUNT)
            (errorf "Too many arguments in the call of `~,,,,40s...'" program))
          ($asm program (if opt? `(,inliner ,nargs) `(,inliner))
                (imap (cut pass1 <> cenv) (cdr program))))]
       [(? vector?)                     ;inlinable lambda
        (expand-inlined-procedure program
                                  (unpack-iform inliner)
                                  (imap (cut pass1 <> cenv) (cdr program)))]
       [_
        (let1 form (inliner program cenv)
          (if (undefined? form)
            (pass1/call program ($gref name) (cdr program) cenv)
            form))])))

  ;; main body of pass1
  (cond
   [(pair? program)                    ; (op . args)
    (unless (list? program)
      (error "proper list required for function application or macro use:" program))
    (cond
     [(pass1/lookup-head (car program) cenv)
      => (^h (cond
              [(identifier? h) (pass1/global-call h)]
              [(lvar? h) (pass1/call program ($lref h) (cdr program) cenv)]
              [(macro? h) ;; local macro
               (pass1 (call-macro-expander h program cenv) cenv)]
              [(syntax? h);; locally rebound syntax
               (call-syntax-handler h program cenv)]
              [else (error "[internal] unknown resolution of head:" h)]))]
     [else (pass1/call program (pass1 (car program) (cenv-sans-name cenv))
                       (cdr program) cenv)])]
   [(variable? program)                 ; variable reference
    (let1 r (cenv-lookup-variable cenv program)
      (cond [(lvar? r) ($lref r)]
            [(identifier? r)
             (or (and-let* ([const (find-const-binding r)]) ($const const))
                 ($gref r))]
            [else (error "[internal] cenv-lookup returned weird obj:" r)]))]
   [else ($const program)]))

;; Returns #t iff exp is the form (with-module module VARIABLE)
;; We need to check the global value of with-module, for it might
;; be redefined.  We assume this function is called infrequently,
;; thus we can afford the time.
(define (module-qualified-variable? expr cenv)
  (match expr
    [((? variable? wm) mod (? variable? v))
     (and-let* ([var (cenv-lookup-syntax cenv wm)]
                [ (identifier? var) ])
       (global-identifier=? var (global-id 'with-module)))]
    [_ #f]))

;;--------------------------------------------------------------
;; pass1/body - Compiling body with internal definitions.
;;
;; First we scan internal defines.  We need to expand macros at this stage,
;; since the macro may produce more internal defines.  Note that the
;; previous internal definition in the same body may shadow the macro
;; binding.  In pass1/body, it is checked by keeping newly defined
;; identifiers in intdefs.
;;
;; Actually, this part touches the hole of R5RS---we can't determine
;; the scope of the identifiers of the body until we find the boundary
;; of internal define's, but in order to find all internal defines
;; we have to expand the macro and we need to detemine the scope
;; of the macro keyword.  Search "macro internal define" in
;; comp.lang.scheme for the details.
;;
;; I use the model that appears the same as Chez, which adopts
;; let*-like semantics for the purpose of determining macro binding
;; during expansion.

;; pass1/body :: [Sexpr], Cenv -> IForm
(define (pass1/body exprs cenv)
  ;; First, we pair up each expr with dummy source info '().  Some of expr
  ;; may be an 'include' form and expanded into the content of the file,
  ;; in which case we keep the source file info in each cdr of the pair.
  (pass1/body-rec (map list exprs) '() cenv))

;; Walks exprs and gathers internal definitions into intdefs in the form
;; of ((var init) ...).  We need to expand macros and 'begin's
;; that appears in the toplevel of exprs, for it may insert more internal
;; definitions.
(define (pass1/body-rec exprs intdefs cenv)
  (match exprs
    [(((op . args) . src) . rest)
     (or (and-let* ([ (not (assq op intdefs)) ]
                    [head (pass1/lookup-head op cenv)])
           (unless (list? args)
             (error "proper list required for function application \
                     or macro use:" (caar exprs)))
           (cond
            [(lvar? head) (pass1/body-finish intdefs exprs cenv)]
            [(macro? head)  ; locally defined macro
             (pass1/body-macro-expand-rec head exprs intdefs cenv)]
            [(syntax? head) ; when (let-syntax ((xif if)) (xif ...)) etc.
             (pass1/body-finish intdefs exprs cenv)]
            [(not (identifier? head)) (error "[internal] pass1/body" head)]
            [(global-eq? head 'define cenv)
             (let1 def (match args
                         [((name . formals) . body)
                          `(,name (,lambda. ,formals ,@body) . ,src)]
                         [(var init) `(,var ,init . ,src)]
                         [_ (error "malformed internal define:" (caar exprs))])
               (pass1/body-rec rest (cons def intdefs) cenv))]
            [(global-eq? head 'define-syntax cenv) ; internal syntax definition
             (match args
               [(name trans-spec)
                (let* ([trans (pass1/eval-macro-rhs
                               'define-syntax trans-spec
                               (cenv-add-name cenv (variable-name name)))]
                       [newenv (cenv-extend cenv `((,name . ,trans)) SYNTAX)])
                  (pass1/body-rec rest intdefs newenv))]
               [_ (error "syntax-error: malformed internal define-syntax:"
                         `(,op ,@args))])]
            [(global-identifier=? head begin.) ;intersperse forms
             (pass1/body-rec (append (imap (cut cons <> src) args) rest)
                             intdefs cenv)]
            [(global-identifier=? head include.)
             (let1 sexpr&srcs (pass1/expand-include args cenv #f)
               (pass1/body-rec (append sexpr&srcs rest) intdefs cenv))]
            [(global-identifier=? head include-ci.)
             (let1 sexpr&srcs (pass1/expand-include args cenv #t)
               (pass1/body-rec (append sexpr&srcs rest) intdefs cenv))]
            [(identifier? head)
             (or (and-let* ([gloc (id->bound-gloc head)]
                            [gval (gloc-ref gloc)]
                            [ (macro? gval) ])
                   (pass1/body-macro-expand-rec gval exprs intdefs cenv))
                 (pass1/body-finish intdefs exprs cenv))]
            [else (error "[internal] pass1/body" head)]))
         (pass1/body-finish intdefs exprs cenv))]
    [_ (pass1/body-finish intdefs exprs cenv)]))

(define (pass1/body-macro-expand-rec mac exprs intdefs cenv)
  (pass1/body-rec
   (acons (call-macro-expander mac (caar exprs) cenv)
          (cdar exprs) ;src
          (cdr exprs)) ;rest
   intdefs cenv))

;; Finishing internal definitions.  If we have internal defs, we wrap
;; the rest by letrec.
(define (pass1/body-finish intdefs exprs cenv)
  (if (null? intdefs)
    (pass1/body-rest exprs cenv)
    (let* ([intdefs. (reverse intdefs)]
           [vars  (map car intdefs.)]
           [lvars (imap make-lvar+ vars)]
           [newenv (cenv-extend cenv (%map-cons vars lvars) LEXICAL)])
      ($let #f 'rec* lvars
            (imap2 (cut pass1/body-init <> <> newenv) lvars (map cdr intdefs.))
            (pass1/body-rest exprs newenv)))))

(define (pass1/body-init lvar init&src newenv)
  (let1 e (if (null? (cdr init&src))
            (cenv-add-name newenv (lvar-name lvar))
            (cenv-add-name/source newenv (lvar-name lvar) (cdr init&src)))
    (rlet1 iexpr (pass1 (car init&src) e)
      (lvar-initval-set! lvar iexpr))))

(define (pass1/body-rest exprs cenv)
  (match exprs
    [() ($seq '())]
    [(expr&src) (pass1/body-1 expr&src cenv)]
    [_ (let1 stmtenv (cenv-sans-name cenv)
         ($seq (let loop ([exprs exprs] [r '()])
                 (if (null? (cdr exprs))
                   (reverse (cons (pass1/body-1 (car exprs) cenv) r))
                   (loop (cdr exprs)
                         (cons (pass1/body-1 (car exprs) stmtenv) r))))))]))

(define (pass1/body-1 expr&src cenv)
  (let1 src (cdr expr&src)
    (if (string? src)
      (pass1 (car expr&src) (cenv-swap-source cenv src))
      (pass1 (car expr&src) cenv))))

;;--------------------------------------------------------------
;; Pass1 utilities
;;

;; get symbol or id, and returns identiier.
(define (ensure-identifier sym-or-id cenv)
  (if (identifier? sym-or-id)
    sym-or-id
    (make-identifier sym-or-id (cenv-module cenv) (cenv-frames cenv))))

;; Does the given argument list satisfy procedure's reqargs/optarg?
(define (argcount-ok? args reqargs optarg?)
  (let1 nargs (length args)
    (or (and (not optarg?) (= nargs reqargs))
        (and optarg? (>= nargs reqargs)))))

;; signal an error if the form is not on the toplevel
(define-inline (check-toplevel form cenv)
  (unless (cenv-toplevel? cenv)
    (error "syntax-error: the form can appear only in the toplevel:" form)))

;; returns a module specified by THING.
(define (ensure-module thing name create?)
  (let1 mod (cond [(symbol? thing) (find-module thing)]
                  [(identifier? thing) (find-module (unwrap-syntax thing))]
                  [(module? thing) thing]
                  [else
                   (errorf "~a requires a module name or a module, but got: ~s"
                           name thing)])
    (or mod
        (if create?
          (make-module (if (identifier? thing) (unwrap-syntax thing) thing))
          (errorf "~a: no such module: ~s" name thing)))))

;; IFORM must be a $LAMBDA node.  This expands the application of IFORM
;; on IARGS (list of IForm) into a mere $LET node.
;; The nodes within IFORM will be reused in the resulting $LET structure,
;; so be careful not to share substructures of IFORM accidentally.
(define (expand-inlined-procedure src iform iargs)
  (let ([lvars ($lambda-lvars iform)]
        [args  (adjust-arglist ($lambda-reqargs iform) ($lambda-optarg iform)
                               iargs ($lambda-name iform))])
    (for-each (^[lv a] (lvar-initval-set! lv a)) lvars args)
    ($let src 'let lvars args ($lambda-body iform))))

;; Adjust argument list according to reqargs and optarg count.
;; Used in procedure inlining and local call optimization.
(define (adjust-arglist reqargs optarg iargs name)
  (unless (argcount-ok? iargs reqargs (> optarg 0))
    (errorf "wrong number of arguments: ~a requires ~a, but got ~a"
            name reqargs (length iargs)))
  (if (zero? optarg)
    iargs
    (receive (reqs opts) (split-at iargs reqargs)
      (append! reqs (list (if (null? opts) ($const '()) ($list #f opts)))))))

;;----------------------------------------------------------------
;; Pass1 syntaxes
;;

(define-macro (define-pass1-syntax formals module . body)
  (let ([mod (ecase module
               [(:null) 'null]
               [(:gauche) 'gauche]
               [(:internal) 'gauche.internal])]
        ;; a trick to assign comprehensive name to body:
        [name (string->symbol #"syntax/~(car formals)")])
    `(let ((,name (^ ,(cdr formals) ,@body)))
       (%insert-syntax-binding (find-module ',mod) ',(car formals)
                               (make-syntax ',(car formals) ,name)))))

(define (global-id id) (make-identifier id (find-module 'gauche) '()))
(define (global-id% id) (make-identifier id (find-module 'gauche.internal) '()))

(define define.      (global-id 'define))
(define lambda.      (global-id 'lambda))
(define r5rs-define. (make-identifier 'define (find-module 'null) '()))
(define r5rs-lambda. (make-identifier 'lambda (find-module 'null) '()))
(define setter.      (global-id 'setter))
(define lazy.        (global-id 'lazy))
(define eager.       (global-id 'eager))
(define values.      (global-id 'values))
(define begin.       (global-id 'begin))
(define include.     (global-id 'include))
(define include-ci.  (global-id 'include-ci))
(define else.        (global-id 'else))
(define =>.          (global-id '=>))
(define current-module. (global-id 'current-module))

(define %make-primitive-transformer. (global-id% '%make-primitive-transformer))
(define %make-er-transformer.        (global-id% '%make-er-transformer))
(define %make-toplevel-cenv.         (global-id% '%make-toplevel-cenv))

;; Definitions ........................................

;; Note on constant binding and inlinable binding:
;;   define-constant and define-inline both create a binding that
;;   is not supposed to be altered, but they have slightly different
;;   semantics.   Define-constant binds a global variable to a value that
;;   is computable at compile time, and serializable to a precompiled
;;   file.  When the compiler sees a global variable reference with
;;   a constant binding, it replaces the reference to the value itself
;;   at pass 1.  Define-inline can bind a variable to a value that is
;;   calculated at runtime.  The compiler does not replace the variable
;;   references with values, but it freely rearranges the references within
;;   the source code.  If an inlinable binding is used at the head position,
;;   the compiler looks at its value, and if it is known to be bound to
;;   an inlinable procedure, the procedure's body is inlined.

(define-pass1-syntax (define form cenv) :null
  (pass1/define form form '() #f (cenv-module cenv) cenv))

(define-pass1-syntax (define form cenv) :gauche
  (pass1/define form form '() #t (cenv-module cenv) cenv))

(define-pass1-syntax (define-constant form cenv) :gauche
  (pass1/define form form '(const) #t (cenv-module cenv) cenv))

(define-pass1-syntax (define-in-module form cenv) :gauche
  (match form
    [(_ module . rest)
     (pass1/define `(_ . ,rest) form '() #t
                   (ensure-module module 'define-in-module #f)
                   cenv)]
    [_ (error "syntax-error: malformed define-in-module:" form)]))

(define (pass1/define form oform flags extended? module cenv)
  (check-toplevel oform cenv)
  (match form
    [(_ (name . args) body ...)
     (pass1/define `(define ,name
                      ,(with-original-source
                        `(,(if extended? lambda. r5rs-lambda.) ,args ,@body)
                        oform))
                   oform flags extended? module cenv)]
    [(_ name expr)
     (unless (variable? name) (error "syntax-error:" oform))
     (let1 cenv (cenv-add-name cenv (variable-name name))
       ;; If NAME is an identifier, it is inserted by macro expander; we
       ;; can't simply place it in $define, since it would insert a toplevel
       ;; definition into the toplevel of macro-definition environment---
       ;; we don't want a mere macro call would modify different module.
       ;; We rename it to uninterned symbol, so, even the binding itself
       ;; is into the macro-definiting module, it won't be visible from
       ;; other code except the code generated in the same macro expansion.
       ;; A trick - we directly modify the identifier, so that other forms
       ;; referring to the same (eq?) identifier can keep referring it.
       (let1 id (if (identifier? name)
                  (%rename-toplevel-identifier! name)
                  (make-identifier name module '()))
         ($define oform flags id (pass1 expr cenv))))]
    [_ (error "syntax-error:" oform)]))

(define (%rename-toplevel-identifier! identifier)
  (slot-set! identifier 'name (gensym #"~(identifier->symbol identifier)."))
  identifier)

;; Inlinable procedure.
;;   Inlinable procedure has both properties of a macro and a procedure.
;;   It is a bit tricky since the inliner information has to exist
;;   both in compile time and execution time.
;;
;;   Processing define-inline involves two actions.
;;   (1) Process the lambda node to be inlined.  A packed IForm should be
;;       attached, and if the lambda node closes enviornment, some code
;;       transformation is required.
;;   (2) Bind the resulting node to the global name, and mark the binding
;;       'inlinable'.
;;
;;   These two are functionally orthogonal.  Especially, not all expressions
;;   can yield inlinable lambda node as (1).   However, to make procedure
;;   inlining work effectively, both of these actions are required; that's
;;   why we process them together.
;;
;;   Steps:
;;   1. Canonicalize the form to (define-inline NAME EXPR).
;;   2. See if EXPR ultimately returns $LAMBDA node.  If so, does the
;;      node closes local environment?  (pass1/check-inlinable-lambda)
;;   3. If EXPR does not yield a closure, we just create 'inlinable'
;;      binding (by pass1/make-inlinable-binding)
;;      but do not do anything further.
;;   4. If EXPR directly yields a closure without an environment,
;;      process the closure (pass1/mark-closure-inlinable!) and
;;      then make inlinable binding.
;;   5. If EXPR creates a local environment, we have to transform
;;      the closed variables into global variables.   See the comment
;;      of subst-lvars above for the details of transformation.
;;

(define-pass1-syntax (define-inline form cenv) :gauche
  (check-toplevel form cenv)
  (match form
    [(_ (name . args) . body)
     (pass1/define-inline form name `(,lambda. ,args ,@body) cenv)]
    [(_ name expr)
     (unless (variable? name) (error "syntax-error:" form))
     (pass1/define-inline form name expr cenv)]
    [_ (error "syntax-error: malformed define-inline:" form)]))

(define (pass1/define-inline form name expr cenv)
  (let1 iform (pass1 expr (cenv-add-name cenv (variable-name name)))
    (receive (closure closed) (pass1/check-inlinable-lambda iform)
      (cond
       [(and (not closure) (not closed)) ; too complex to inline
        (pass1/make-inlinable-binding form name iform cenv)]
       [(null? closed)               ; no closed env
        (pass1/mark-closure-inlinable! closure name cenv)
        (pass1/make-inlinable-binding form name closure cenv)]
       [else ; inlinable lambda has closed env.
        ;; See the comment in subst-lvars above on the transformation.
        ;; closed :: [(lvar . init-iform)]
        ;; gvars :: [(identifier . iform)]
        ;; subs :: [(lvar . iform)]  ; iform being $const or $gref
        (receive (gvars subs)
            (pass1/define-inline-classify-env name closed cenv)
          (let1 defs (pass1/define-inline-gen-closed-env gvars cenv)
            ($lambda-body-set! closure
                               (subst-lvars ($lambda-body closure) subs))
            (pass1/mark-closure-inlinable! closure name cenv)
            ($seq `(,@defs
                     ,(pass1/make-inlinable-binding form name closure cenv)))))]
       ))))

;; If IFORM generats a closure with local environment, returns
;; the closure itself ($lambda node) and the environment
;; ((lvar . init) ...).
;; Typical case is ($let ... ($lambda ...)).  In such case this
;; procedure effectively strips $let nodes.
;; If IFORM has more complicated structure, we just return (values #f #f)
;; to give up inlining.
(define (pass1/check-inlinable-lambda iform)
  (cond [(has-tag? iform $LAMBDA) (values iform '())]
        [(has-tag? iform $LET)
         (receive (closure closed)
             (pass1/check-inlinable-lambda ($let-body iform))
           (if (and (not closure) (not closed))
             (values #f #f) ; giveup
             (let loop ([lvars (reverse ($let-lvars iform))]
                        [inits (reverse ($let-inits iform))]
                        [closed closed])
               (if (null? lvars)
                 (values closure closed)
                 (loop (cdr lvars) (cdr inits)
                       (acons (car lvars) (car inits) closed))))))]
        [else (values #f #f)]))

(define (pass1/define-inline-classify-env name lv&inits cenv)
  (define gvars '())
  (define subs '())
  (let loop ([lv&inits lv&inits])
    (match lv&inits
      [() (values (reverse gvars) (reverse subs))]
      [((and (lv . (? $const?)) p) . lv&inits)
       (push! subs p) (loop lv&inits)]
      [((lv . init) . lv&inits)
       (let1 gvar (make-identifier (gensym #"~|name|$~(lvar-name lv).")
                                   (cenv-module cenv) '())
         (push! subs `(,lv . ,($gref gvar)))
         (push! gvars `(,gvar . ,(subst-lvars init subs)))
         (loop lv&inits))])))

;; gvars :: [(identifier . iform)]
(define (pass1/define-inline-gen-closed-env gvars cenv)
  (imap (^[gv] ($define #f '(inlinable) (car gv) (cdr gv))) gvars))

;; set up $LAMBDA node (closure) to be inlinable.  If NAME is given,
;; this also inserts the binding to the current compiling environment
;; so that inlining is effective for the rest of the compilation.
(define (pass1/mark-closure-inlinable! closure name cenv)
  (let* ([module  (cenv-module cenv)]
         ;; Dummy-proc is only a placeholder to record the inliner info
         ;; to be used during compilation of the current compiler unit.
         ;; Its body doesn't matter, but we need to make sure every dummy-proc
         ;; is a different instance.  If we make it a constant procedure,
         ;; Gauche's compiler optimizes it to refer to the singleton instance.
         [dummy-proc (^ _ name)]
         [packed (pack-iform closure)])
    ($lambda-flag-set! closure packed)
    (when name
      ;; record inliner function for compiler.  this is used only when
      ;; the procedure needs to be inlined in the same compiler unit.
      (%insert-binding module (unwrap-syntax name) dummy-proc)
      (set! (%procedure-inliner dummy-proc) (pass1/inliner-procedure packed)))))

(define (pass1/make-inlinable-binding form name iform cenv)
  ;; See the comment in pass1/define about renaming the toplevel identifier.
  (let1 id (if (identifier? name)
             (%rename-toplevel-identifier! name)
             (make-identifier name (cenv-module cenv) '()))
    ($define form '(inlinable) id iform)))

(define (pass1/inliner-procedure inline-info)
  (unless (vector? inline-info)
    (error "[internal] pass1/inliner-procedure got invalid info" inline-info))
  (^[form cenv]
    (expand-inlined-procedure form (unpack-iform inline-info)
                              (imap (cut pass1 <> cenv) (cdr form)))))

;; Toplevel macro definitions

(define-pass1-syntax (define-macro form cenv) :gauche
  (check-toplevel form cenv)
  (pass1/define-macro form form (cenv-module cenv) cenv))

(define (pass1/define-macro form oform module cenv)
  (check-toplevel oform cenv)
  (match form
    [(_ (name . formals) body ...)
     (let1 trans
         (make-macro-transformer name
                                 (eval `(,lambda. ,formals ,@body) module))
       ;; See the "Hygiene alert" in pass1/define.
       (%insert-syntax-binding module (unwrap-syntax name) trans)
       ($const-undef))]
    [(_ name expr)
     (unless (variable? name) (error "syntax-error:" oform))
     ;; TODO: macro autoload
     (let1 trans (make-macro-transformer name (eval expr module))
       ;; See the "Hygiene alert" in pass1/define.
       (%insert-syntax-binding module (unwrap-syntax name) trans)
       ($const-undef))]
    [_ (error "syntax-error:" oform)]))

(define-pass1-syntax (define-syntax form cenv) :null
  (check-toplevel form cenv)
  ;; Temporary: we use the old compiler's syntax-rules implementation
  ;; for the time being.
  (match form
    [(_ name expr)
     (let* ([cenv (cenv-add-name cenv (variable-name name))]
            [transformer (pass1/eval-macro-rhs 'define-syntax expr cenv)])
       ;; See the "Hygiene alert" in pass1/define.
       (%insert-syntax-binding (cenv-module cenv) (unwrap-syntax name)
                               transformer)
       ($const-undef))]
    [_ (error "syntax-error: malformed define-syntax:" form)]))

(define (pass1/eval-macro-rhs who expr cenv)
  (rlet1 transformer ((make-toplevel-closure (compile expr cenv)))
    (unless (or (is-a? transformer <syntax>)
                (is-a? transformer <macro>))
      (errorf "syntax-error: rhs expression of ~a ~s \
               doesn't yield a syntactic transformer: ~s"
              who expr transformer))))

(inline-stub
 (define-cproc make-toplevel-closure (code::<compiled-code>)
  (return (Scm_MakeClosure (SCM_OBJ code) NULL)))
 )

;; Macros ...........................................

(define-pass1-syntax (primitive-macro-transformer form cenv) :gauche
  (match form
    [(_ xformer)
     ;; We need to capture the current CENV as the macro definition
     ;; environment.  There's a catch, though---if we're AOT compiling
     ;; a macro, the captured CENV must be serializable to a file,
     ;; which isn't generally the case.
     ;; So, if we're compiling toplevel, we emit code that reconstruct
     ;; cenv at runtime.  Runtime CENV construction
     ;; will incur some overhead, but it's just per macro definition so
     ;; it won't be an issue.
     ;; If cenv has local environment, we don't bother that, for the macro
     ;; will be fully expanded during AOT compilation.  HOWEVER - we can't
     ;; embed cenv as a vector literal (e.g. `',cenv) since quoting will
     ;; strip all identifier information in cenv.  The right thing would be
     ;; to make cenv as a record.  For now, we take advantage that unquoted
     ;; vector evaluates to itself, and insert cenv without quoting.  This
     ;; has to change if we prohibit unquoted vector literals.
     (let1 def-env-form (if (null? (cenv-frames cenv))
                          `(,%make-toplevel-cenv. ',(cenv-exp-name cenv))
                          cenv)
       (pass1 `(,%make-primitive-transformer. ,xformer ,def-env-form)
              cenv))]
    [_ (error "syntax-error: malformed primitive-macro-transformer:" form)]))

;; %make-toplevel-cenv is a procedure, so it refers the runtime module.
;; In general it doesn't need to be the same as the compile-time module.
;; However, as far as this form is inserted by primitive-macro-transformer
;; or er-macro-transformer for the toplevel macro definition, it won't be an
;; issue.
;; TODO: Check if it's ok to have something like this:
;;    (begin (define-syntax ...) (select-module ...) (define-syntax ...)
(define (%make-toplevel-cenv name) :internal
  (%make-cenv (vm-current-module) '() name))

(define-pass1-syntax (%macroexpand form cenv) :gauche
  (match form
    [(_ expr) ($const (%internal-macro-expand expr cenv #f))]
    [_ (error "syntax-error: malformed %macroexpand:" form)]))

(define-pass1-syntax (%macroexpand-1 form cenv) :gauche
  (match form
    [(_ expr) ($const (%internal-macro-expand expr cenv #t))]
    [_ (error "syntax-error: malformed %macroexpand-1:" form)]))

(define (%internal-macro-expand expr cenv once?)
  (define (xpand expr)
    (match expr
      [((? variable? op) . args)
       (let1 var (cenv-lookup-syntax cenv op)
         (cond [(macro? var) (call-macro-expander var expr cenv)]
               [(identifier? var)
                (if-let1 gval (and-let* ([gloc (id->bound-gloc var)]
                                         [gval (gloc-ref gloc)]
                                         [ (macro? gval) ])
                                gval)
                  (call-macro-expander gval expr cenv)
                  expr)]
               [else expr]))]
      [((? macro? op) . args) (call-macro-expander op expr cenv)]
      [_ expr]))
  (if once?
    (xpand expr)
    (let loop ([expr expr])
      (let1 e2 (xpand expr)
        (if (eq? e2 expr)
          expr
          (loop e2))))))

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

(define-pass1-syntax (... form cenv) :null
  (error "invalid syntax:" form))

(define-pass1-syntax (let-syntax form cenv) :null
  (match form
    [(_ ((name trans-spec) ...) body ...)
     (let* ([trans (map (^[n x] (pass1/eval-macro-rhs
                                 'let-syntax x
                                 (cenv-add-name cenv (variable-name n))))
                        name trans-spec)]
            [newenv (cenv-extend cenv (%map-cons name trans) SYNTAX)])
       (pass1/body body newenv))]
    [_ (error "syntax-error: malformed let-syntax:" form)]))

(define-pass1-syntax (letrec-syntax form cenv) :null
  (match form
    [(_ ((name trans-spec) ...) body ...)
     (let* ([newenv (cenv-extend cenv (%map-cons name trans-spec) SYNTAX)]
            [trans (map (^[n x] (pass1/eval-macro-rhs
                                 'letrec-syntax x
                                 (cenv-add-name newenv (variable-name n))))
                        name trans-spec)])
       (for-each set-cdr! (cdar (cenv-frames newenv)) trans)
       (pass1/body body newenv))]
    [_ (error "syntax-error: malformed letrec-syntax:" form)]))

(define-pass1-syntax (syntax-rules form cenv) :null
  (match form
    [(_ (literal ...) rule ...)
     ($const (compile-syntax-rules (cenv-exp-name cenv) '... literal rule
                                   (cenv-module cenv)
                                   (cenv-frames cenv)))]
    [(_ (? variable-or-keyword? elli) (literal ...) rule ...)
     ;; NB: Stripping identifier from elli may be broken in macro-defining-macro.
     ;; Fix it once we have proper low-level macro system.
     ;; NB: We allow keyword for ellipsis, so that something like ::: can be
     ;; used.
     ($const (compile-syntax-rules (cenv-exp-name cenv)
                                   (if (identifier? elli)
                                     (slot-ref elli 'name)
                                     elli)
                                   literal rule
                                   (cenv-module cenv)
                                   (cenv-frames cenv)))]
    [_ (error "syntax-error: malformed syntax-rules:" form)]))

;; If family ........................................

(define-pass1-syntax (if form cenv) :null
  (match form
    [(_ test then else)
     ($if form (pass1 test (cenv-sans-name cenv))
          (pass1 then cenv) (pass1 else cenv))]
    [(_ test then)
     ($if form (pass1 test (cenv-sans-name cenv))
          (pass1 then cenv) ($const-undef))]
    [_ (error "syntax-error: malformed if:" form)]))

(define-pass1-syntax (and form cenv) :null
  (define (rec exprs)
    (match exprs
      [() `#(,$CONST #t)]
      [(expr) (pass1 expr cenv)]
      [(expr . more)
       ($if #f (pass1 expr (cenv-sans-name cenv)) (rec more) ($it))]
      [_ (error "syntax-error: malformed and:" form)]))
  (rec (cdr form)))

(define-pass1-syntax (or form cenv) :null
  (define (rec exprs)
    (match exprs
      [() ($const-f)]
      [(expr) (pass1 expr cenv)]
      [(expr . more)
       ($if #f (pass1 expr (cenv-sans-name cenv)) ($it) (rec more))]
      [_ (error "syntax-error: malformed or:" form)]))
  (rec (cdr form)))

(define-pass1-syntax (when form cenv) :gauche
  (match form
    [(_ test body ...)
     (let1 cenv (cenv-sans-name cenv)
       ($if form (pass1 test cenv)
            ($seq (imap (cut pass1 <> cenv) body))
            ($const-undef)))]
    [_ (error "syntax-error: malformed when:" form)]))

(define-pass1-syntax (unless form cenv) :gauche
  (match form
    [(_ test body ...)
     (let1 cenv (cenv-sans-name cenv)
       ($if form (pass1 test cenv)
            ($const-undef)
            ($seq (imap (cut pass1 <> cenv) body))))]
    [_ (error "syntax-error: malformed unless:" form)]))

(define-pass1-syntax (else form cenv) :null
  (error "invalid syntax:" form))
(define-pass1-syntax (=> form cenv) :null
  (error "invalid syntax:" form))

(define-pass1-syntax (cond form cenv) :null
  (define (process-clauses cls)
    (match cls
      [() ($const-undef)]
      ;; (else . exprs)
      [(([? (cut global-eq? <> 'else cenv)] exprs ...) . rest)
       (unless (null? rest)
         (error "syntax-error: 'else' clause followed by more clauses:" form))
       ($seq (imap (cut pass1 <> cenv) exprs))]
      ;; (test => proc)
      [((test [? (cut global-eq? <> '=> cenv)] proc) . rest)
       (let ([test (pass1 test cenv)]
             [tmp (make-lvar 'tmp)])
         (lvar-initval-set! tmp test)
         ($let (car cls) 'let
               (list tmp)
               (list test)
               ($if (car cls)
                    ($lref tmp)
                    ($call (car cls)
                           (pass1 proc (cenv-sans-name cenv))
                           (list ($lref tmp)))
                    (process-clauses rest))))]
      ;; (generator guard => proc) -- SRFI-61 'general cond clause'
      [((generator guard [? (cut global-eq? <> '=> cenv)] receiver) . rest)
       (let1 tmp (make-lvar 'tmp)
         ($receive (car cls) 0 1 (list tmp)
                   (pass1 generator cenv)
                   ($if (car cls)
                        ($asm #f
                              `(,APPLY 2)
                              (list (pass1 guard (cenv-sans-name cenv))
                                    ($lref tmp)))
                        ($asm #f
                              `(,APPLY 2)
                              (list (pass1 receiver (cenv-sans-name cenv))
                                    ($lref tmp)))
                        (process-clauses rest))))]
      [((test) . rest)                  ; (test)
       ($if (car cls) (pass1 test (cenv-sans-name cenv))
            ($it)
            (process-clauses rest))]
      [((test exprs ...) . rest)          ; (test . exprs)
       ;; The following check will be unnecessary once we release 0.9.5
       ;; that includes the bug fix in util.match (commit 867419d44).
       (unless (list? exprs)
         (error "syntax-error: bad clause in cond:" form))
       ($if (car cls) (pass1 test (cenv-sans-name cenv))
            ($seq (imap (cut pass1 <> cenv) exprs))
            (process-clauses rest))]
      [_ (error "syntax-error: bad clause in cond:" form)]))

  (match form
    [(_) (error "syntax-error: at least one clause is required for cond:" form)]
    [(_ clause ...) (process-clauses clause)]
    [else (error "syntax-error: malformed cond:" form)]))

(define-pass1-syntax (case form cenv) :null
  (define (process-clauses tmpvar cls)
    (match cls
      [() ($const-undef)]
      [(([? (cut global-eq? <> 'else cenv)] exprs ...) . rest)
       (unless (null? rest)
         (error "syntax-error: 'else' clause followed by more clauses:" form))
       (match exprs
         ;; (else => proc) -- SRFI-87 case clause
         [((? (cut global-eq? <> '=> cenv)) proc)
          ($call (car cls)
                 (pass1 proc (cenv-sans-name cenv))
                 (list ($lref tmpvar)))]
         ;; (else . exprs)
         [_ ($seq (imap (cut pass1 <> cenv) exprs))])]
      [((elts exprs ...) . rest)
       (let ([nelts (length elts)]
             [elts  (map unwrap-syntax elts)])
         ($if (car cls)
              (case nelts
                [(0)  ($const-f)]
                [(1)  (if (symbol? (car elts))
                        ($eq? #f  ($lref tmpvar) ($const (car elts)))
                        ($eqv? #f ($lref tmpvar) ($const (car elts))))]
                [else ($memv #f ($lref tmpvar) ($const elts))])
              (match exprs
                ;; (elts => proc) -- SRFI-87 case clause
                [((? (cut global-eq? <> '=> cenv)) proc)
                 ($call (car cls)
                        (pass1 proc (cenv-sans-name cenv))
                        (list ($lref tmpvar)))]
                ;; (elts . exprs)
                [_ ($seq (imap (cut pass1 <> cenv) exprs))])
              (process-clauses tmpvar (cdr cls))))]
      [_ (error "syntax-error: bad clause in case:" form)]))

  (match form
    [(_)
     (error "syntax-error: at least one clause is required for case:" form)]
    [(_ expr clause ...)
     (let* ([etree (pass1 expr cenv)]
            [tmp (make-lvar 'tmp)])
       (lvar-initval-set! tmp etree)
       ($let form 'let
             (list tmp)
             (list etree)
             (process-clauses tmp clause)))]
    [_ (error "syntax-error: malformed case:" form)]))

(define-pass1-syntax (and-let* form cenv) :gauche
  (define (process-binds binds body cenv)
    (match binds
      [() (pass1/body body cenv)]
      [((exp) . more)
       ($if form (pass1 exp (cenv-sans-name cenv))
            (process-binds more body cenv)
            ($it))]
      [([? variable? var] . more)
       ($if form (pass1 var (cenv-sans-name cenv))
            (process-binds more body cenv)
            ($it))]
      [(([? variable? var] init) . more)
       (let* ([lvar (make-lvar var)]
              [newenv (cenv-extend cenv `((,var . ,lvar)) LEXICAL)]
              [itree (pass1 init (cenv-add-name cenv var))])
         (lvar-initval-set! lvar itree)
         ($let form 'let
               (list lvar)
               (list itree)
               ($if form ($lref lvar)
                    (process-binds more body newenv)
                    ($it))))]
      [_ (error "syntax-error: malformed and-let*:" form)]))

  (match form
    [(_ binds . body) (process-binds binds body cenv)]
    [_ (error "syntax-error: malformed and-let*:" form)]))

;; Quote and quasiquote ................................

(define (pass1/quote obj)
  ($const (unwrap-syntax obj)))

(define-pass1-syntax (quote form cenv) :null
  (match form
    [(_ obj) (pass1/quote obj)]
    [else (error "syntax-error: malformed quote:" form)]))

(define-pass1-syntax (quasiquote form cenv) :null
  ;; We want to avoid unnecessary allocation as much as possible.
  ;; Current code generates constants not only the obvious constant
  ;; case, e.g. `(a b c), but also folds constant variable references,
  ;; e.g. (define-constant x 3) then `(,x) generate a constant list '(3).
  ;; This extends as far as the pass-1 constant folding goes, so `(,(+ x 1))
  ;; also becomes '(4).
  ;; NB: The current code allocates lots of intermediate $const node.

  (define (quasiquote? v)       (global-eq? v 'quasiquote cenv))
  (define (unquote? v)          (global-eq? v 'unquote cenv))
  (define (unquote-splicing? v) (global-eq? v 'unquote-splicing cenv))

  ;; In the context where there's no outer list to which we intersperse to.
  (define (quasi obj level)
    (match obj
      [((? quasiquote?) x)
       (let1 xx (quasi x (+ level 1))
         (if ($const? xx)
           ($const (list 'quasiquote ($const-value xx)))
           ($list obj (list ($const 'quasiquote) xx))))]
      [((and (or (? unquote?) (? unquote-splicing?)) op) . xs)
       (if (zero? level)
         (if (and (unquote? op)
                  (pair? xs) (null? (cdr xs)))
           (pass1 (car xs) cenv)
           (errorf "invalid ~a form in this context: ~s" op obj))
         (let1 xx (quasi* xs (- level 1))
           (if ($const? xx)
             ($const (cons op ($const-value xx)))
             ($cons obj ($const op) xx))))]
      [(? pair?)       (quasi* obj level)]
      [(? vector?)     (quasi-vector obj level)]
      [(? identifier?) ($const (unwrap-syntax obj))]
      [() ($const-nil)]
      [_  ($const obj)]))

  ;; In the spliceable context.  objs is always a list.
  (define (quasi* objs level)
    ;; NB: we already excluded toplevel quasiquote and unquote
    (match objs
      [(((and (or (? unquote?) (? unquote-splicing?)) op) . xs) . ys)
       (let1 yy (quasi* ys level)
         (if (zero? level)
           ((if (unquote? op) build build@)
            (imap (cut pass1 <> cenv) xs) yy)
           (let1 xx (quasi* xs (- level 1))
             (if (and ($const? xx) ($const? yy))
               ($const (acons op ($const-value xx) ($const-value yy)))
               ($cons objs ($cons (car objs) ($const op) xx) yy)))))]
      [((or (? unquote?) (? unquote-splicing?)) . _) ;`(... . ,xs) `(... . ,@xs)
       (quasi objs level)]
      [((? vector? x) . ys) (quasi-cons objs quasi-vector x ys level)]
      [(x . ys)             (quasi-cons objs quasi x ys level)]
      [_                    (quasi objs level)]))

  ;; iforms :: [IForm]
  ;; rest   :: IForm
  (define (build iforms rest)
    (match iforms
      [() rest]
      [(x . xs) (let1 xx (build xs rest)
                  (if (and ($const? x) ($const? xx))
                    ($const (cons ($const-value x) ($const-value xx)))
                    ($cons #f x xx)))]))

  (define (build@ iforms rest)
    (match iforms
      [() rest]
      [(x . xs) (let1 xx (build@ xs rest)
                  (if ($const? xx)
                    (cond [(null? ($const-value xx)) x]
                          [($const? x) ($const (append ($const-value x)
                                                       ($const-value xx)))]
                          [else ($append #f x xx)])
                    ($append #f x xx)))]))

  (define (quasi-cons src quasi-car x ys level)
    (let ([xx (quasi-car x level)]
          [yy (quasi* ys level)])
      (if (and ($const? xx) ($const? yy))
        ($const (cons ($const-value xx) ($const-value yy)))
        ($cons src xx yy))))

  (define (quasi-vector obj level)
    (if (vector-has-splicing? obj)
      ($list->vector obj (quasi* (vector->list obj) level))
      (let* ([need-construct? #f]
             [elts (map (^[elt] (rlet1 ee (quasi elt level)
                                  (unless ($const? ee)
                                    (set! need-construct? #t))))
                        (vector->list obj))])
        (if need-construct?
          ($vector obj elts)
          ($const (list->vector (map (^[e] ($const-value e)) elts)))))))

  (define (vector-has-splicing? obj)
    (let loop ((i 0))
      (cond [(= i (vector-length obj)) #f]
            [(and (pair? (vector-ref obj i))
                  (unquote-splicing? (car (vector-ref obj i))))]
            [else (loop (+ i 1))])))

  (match form
    [(_ obj) (quasi obj 0)]
    [_ (error "syntax-error: malformed quasiquote:" form)]))

(define-pass1-syntax (unquote form cenv) :null
  (error "unquote appeared outside quasiquote:" form))

(define-pass1-syntax (unquote-splicing form cenv) :null
  (error "unquote-splicing appeared outside quasiquote:" form))

;; Lambda family (binding constructs) ...................

(define-pass1-syntax (lambda form cenv) :null
  (match form
    [(_ formals . body)
     (pass1/lambda (add-arg-info form formals) formals body cenv #f)]
    [_ (error "syntax-error: malformed lambda:" form)]))

(define-pass1-syntax (lambda form cenv) :gauche
  (match form
    [(_ formals . body)
     (pass1/lambda (add-arg-info form formals) formals body cenv #t)]
    [_ (error "syntax-error: malformed lambda:" form)]))

;; Add formals list as 'arg-info attributes of the source form
(define (add-arg-info form formals)
  (rlet1 xform (if (extended-pair? form)
                 form
                 (extended-cons (car form) (cdr form)))
    (pair-attribute-set! xform 'arg-info formals)))

(define (pass1/lambda form formals body cenv extended?)
  (receive (args reqargs optarg kargs) (parse-lambda-args formals)
    (cond [(null? kargs)
           (let* ([lvars (imap make-lvar+ args)]
                  [intform ($lambda form (cenv-exp-name cenv)
                                    reqargs optarg lvars #f #f)]
                  [newenv (cenv-extend/proc cenv (%map-cons args lvars)
                                            LEXICAL intform)])
             (vector-set! intform 6 (pass1/body body newenv))
             intform)]
          [(not extended?)
           (error "syntax-error: extended formals aren't allowed in R5RS \
                   lambda:" form)]
          [else
           (let1 g (gensym)
             (pass1/lambda form (append args g)
                           (pass1/extended-lambda form g kargs body)
                           cenv #f))])))

(define-pass1-syntax (receive form cenv) :gauche
  (match form
    [(_ formals expr body ...)
     (receive (args reqargs optarg kargs) (parse-lambda-args formals)
       (unless (null? kargs)
         (error "syntax-error: extended lambda list isn't allowed in receive:"
                form))
       (let* ([lvars (imap make-lvar+ args)]
              [newenv (cenv-extend cenv (%map-cons args lvars) LEXICAL)])
         ($receive form reqargs optarg lvars (pass1 expr cenv)
                   (pass1/body body newenv))))]
    [_ (error "syntax-error: malformed receive:" form)]))

;; Returns <list of args>, <# of reqargs>, <has optarg?>, <kargs>
;; <kargs> is like (:optional (x #f) (y #f) :rest k) etc.
(define (parse-lambda-args formals)
  (let loop ([formals formals] [args '()] [n 0])
    (match formals
      [()      (values (reverse args) n 0 '())]
      [((? keyword-like?) . _) (values (reverse args) n 1 formals)]
      [(x . y) (loop (cdr formals) (cons (car formals) args) (+ n 1))]
      [x       (values (reverse (cons x args)) n 1 '())])))

;; Handles extended lambda list.  garg is a gensymed var that receives
;; restarg.
(define (pass1/extended-lambda form garg kargs body)
  (define (collect-args xs r)
    (match xs
      [() (values (reverse r) '())]
      [((? keyword-like?) . _) (values (reverse r) xs)]
      [(var . rest) (collect-args rest (cons var r))]))
  (define (parse-kargs xs os ks r a)
    (match xs
      [() (expand-opt os ks r a)]
      [(':optional . xs)
       (unless (null? os) (too-many :optional))
       (receive (os xs) (collect-args xs '()) (parse-kargs xs os ks r a))]
      [(':key . xs)
       (unless (null? ks) (too-many :key))
       (receive (ks xs) (collect-args xs '()) (parse-kargs xs os ks r a))]
      [(':rest . xs)
       (when r (too-many :rest))
       (receive (rs xs) (collect-args xs '())
         (match rs
           [(r) (parse-kargs xs os ks r a)]
           [_ (error ":rest keyword in the extended lambda list must be \
                      followed by exactly one argument:" kargs)]))]
      [(':allow-other-keys . xs)
       (when a (too-many :allow-other-keys))
       (receive (a xs) (collect-args xs '())
         (match a
           [()   (parse-kargs xs os ks r #t)]
           [(av) (parse-kargs xs os ks r av)]
           [_ (error ":allow-other-keys keyword in the extended lambda list \
                      can be followed by zero or one argument:" kargs)]))]
      [_ (error "invalid extended lambda list:" kargs)]))
  (define (too-many key)
    (errorf "too many ~s keywords in the extended lambda list: ~s" key kargs))
  (define (expand-opt os ks r a)
    (if (null? os)
      (if r
        `(((with-module gauche let) ((,r ,garg)) ,@(expand-key ks garg a)))
        (expand-key ks garg a))
      (let ([binds (map (match-lambda
                          [[? symbol? o] o]
                          [[? identifier? o] o]
                          [(o init) `(,o ,init)]
                          [_ (error "illegal optional argument spec in " kargs)])
                        os)]
            [rest (or r (gensym))])
        `(((with-module gauche let-optionals*) ,garg ,(append binds rest)
           ,@(if (and (not r) (null? ks))
               ;; TODO: better error message!
               `((unless (null? ,rest)
                   (error "too many arguments for" ',form))
                 (let () ,@(expand-key ks rest a)))
               (expand-key ks rest a)))))))
  (define (expand-key ks garg a)
    (if (null? ks)
      body
      (let1 args (map (match-lambda
                        [[? symbol? o] o]
                        [[? identifier? o] o]
                        [(([? keyword? key] o) init) `(,o ,key ,init)]
                        [(o init) `(,o ,init)]
                        [_ (error "illegal keyword argument spec in " kargs)])
                      ks)
        `(((with-module gauche let-keywords*) ,garg
           ,(if a (append args a) args)
           ,@body)))))

  (parse-kargs kargs '() '() #f #f))

;; case-lambda (srfi-16)
;;   we recognize it here so that we can do aggressive inlining.
(define-pass1-syntax (case-lambda form cenv) :gauche
  (match form
    [(_ (formals . body)) ; special case
     (pass1 `(,lambda. ,formals ,@body) cenv)]
    [(_) (error "syntax-error: malformed case-lambda:" form)]
    [(_ (formals . body) ...)
     (receive (min-req max-req) (find-argcount-minmax formals)
       (pass1 `(,(make-identifier 'make-case-lambda
                                  (find-module 'gauche.internal)
                                  '())
                ,min-req ,max-req ',formals
                (list ,@(map (^(f b) `(,lambda. ,f ,@b)) formals body))
                ',(cenv-exp-name cenv))
              cenv))]
    [_ (error "syntax-error: malformed case-lambda:" form)]))

(define (find-argcount-minmax formals)
  (define (length. xs k)
    (if (pair? xs) (length. (cdr xs) (+ k 1)) k))
  (let loop ([formals formals] [min-req #f] [max-req 0])
    (if (null? formals)
      (values min-req max-req)
      (let1 k (length. (car formals) 0)
        (loop (cdr formals) (if min-req (min min-req k) k) (max max-req k))))))

(define-pass1-syntax (let form cenv) :null
  (match form
    [(_ () body ...)
     (pass1/body body cenv)]
    [(_ ((var expr) ...) body ...)
     (let* ([lvars (imap make-lvar+ var)]
            [newenv (cenv-extend cenv (%map-cons var lvars) LEXICAL)])
       ($let form 'let lvars
             (map (^[init lvar]
                    (rlet1 iexpr
                        (pass1 init (cenv-add-name cenv (lvar-name lvar)))
                      (lvar-initval-set! lvar iexpr)))
                  expr lvars)
             (pass1/body body newenv)))]
    [(_ name ((var expr) ...) body ...)
     (unless (variable? name) (error "bad name for named let:" name))
     ;; Named let.  (let name ((var exp) ...) body ...)
     ;;
     ;;  We don't use the textbook expansion here
     ;;    ((letrec ((name (lambda (var ...) body ...))) name) exp ...)
     ;;
     ;;  Instead, we use the following expansion, except that we cheat
     ;;  environment during expanding {exp ...} so that the binding of
     ;;  name doesn't interfere with exp ....
     ;;
     ;;    (letrec ((name (lambda (var ...) body ...))) (name {exp ...}))
     ;;
     ;;  The reason is that this form can be more easily spotted by
     ;;  our simple-minded closure optimizer in Pass 2.
     (let ([lvar (make-lvar name)]
           [args (imap make-lvar+ var)]
           [argenv (cenv-sans-name cenv)])
       (let* ([env1 (cenv-extend cenv `((,name . ,lvar)) LEXICAL)]
              [env2 (cenv-extend/name env1 (%map-cons var args) LEXICAL name)]
              [lmda ($lambda form name (length args) 0 args
                             (pass1/body body env2))])
         (lvar-initval-set! lvar lmda)
         ($let form 'rec
               (list lvar)
               (list lmda)
               ($call #f ($lref lvar)
                      (imap (cut pass1 <> argenv) expr)))))]
    [_ (error "syntax-error: malformed let:" form)]))

(define-pass1-syntax (let* form cenv) :null
  (match form
    [(_ ((var expr) ...) body ...)
     (let loop ([vars var] [inits expr] [cenv cenv])
       (if (null? vars)
         (pass1/body body cenv)
         (let* ([lv (make-lvar (car vars))]
                [newenv (cenv-extend cenv `((,(car vars) . ,lv)) LEXICAL)]
                [iexpr (pass1 (car inits) (cenv-add-name cenv (car vars)))])
           (lvar-initval-set! lv iexpr)
           ($let #f 'let (list lv) (list iexpr)
                 (loop (cdr vars) (cdr inits) newenv)))))]
    [_ (error "syntax-error: malformed let*:" form)]))

(define-pass1-syntax (letrec form cenv) :null
  (pass1/letrec form cenv "letrec" 'rec))

(define-pass1-syntax (letrec* form cenv) :gauche
  (pass1/letrec form cenv "letrec*" 'rec*))

(define (pass1/letrec form cenv name type)
  (match form
    [(_ () body ...)
     (pass1/body body cenv)]
    [(_ ((var expr) ...) body ...)
     (let* ([lvars (imap make-lvar+ var)]
            [newenv (cenv-extend cenv (%map-cons var lvars) LEXICAL)])
       ($let form type lvars
             (map (^[lv init]
                    (rlet1 iexpr
                        (pass1 init (cenv-add-name newenv (lvar-name lv)))
                      (lvar-initval-set! lv iexpr)))
                  lvars expr)
             (pass1/body body newenv)))]
    [else (errorf "syntax-error: malformed ~a: ~s" name form)]))

(define-pass1-syntax (do form cenv) :null
  (match form
    [(_ ((var init . update) ...) (test expr ...) body ...)
     (let* ([tmp  (make-lvar 'do-proc)]
            [args (imap make-lvar+ var)]
            [newenv (cenv-extend/proc cenv (%map-cons var args)
                                      LEXICAL 'do-proc)]
            [clo ($lambda
                  form 'do-body (length var) 0 args
                  ($if #f
                       (pass1 test newenv)
                       (if (null? expr)
                         ($it)
                         ($seq (imap (cut pass1 <> newenv) expr)))
                       ($seq
                        (list
                         (pass1/body body newenv)
                         ($call form
                                ($lref tmp)
                                (map (match-lambda*
                                       [(() arg)   ($lref arg)]
                                       [((expr) _) (pass1 expr newenv)]
                                       [_ (error "bad update expr in do:" form)])
                                     update args)))))
                  #f)])
       (lvar-initval-set! tmp clo)
       ($let form 'rec
             (list tmp)
             (list clo)
             ($call form
                    ($lref tmp)
                    (map (cute pass1 <> (cenv-sans-name cenv)) init))))]
    [else (error "syntax-error: malformed do:" form)]))

;; Set! ......................................................

(define-pass1-syntax (set! form cenv) :null
  (match form
    [(_ (op . args) expr)
     ($call form
            ($call #f
                   ($gref setter.)
                   (list (pass1 op cenv)) #f)
            (let1 cenv (cenv-sans-name cenv)
              (append (imap (cut pass1 <> cenv) args)
                      (list (pass1 expr cenv)))))]
    [(_ name expr)
     (unless (variable? name)
       (error "syntax-error: malformed set!:" form))
     (let ([var (cenv-lookup-variable cenv name)]
           [val (pass1 expr cenv)])
       (if (lvar? var)
         ($lset var val)
         ($gset (ensure-identifier var cenv) val)))]
    [_ (error "syntax-error: malformed set!:" form)]))

;; Begin .....................................................

(define-pass1-syntax (begin form cenv) :null
  ($seq (imap (cut pass1 <> cenv) (cdr form))))

;; Lazy & Delay ..............................................

(define-pass1-syntax (lazy form cenv) :gauche
  (match form
    [(_ expr) ($asm form `(,PROMISE)
                    (list (pass1 `(,lambda. () ,expr) cenv)))]
    [_ (error "syntax-error: malformed lazy:" form)]))

(define-pass1-syntax (delay form cenv) :null
  (match form
    [(_ expr) (pass1 `(,lazy. (,eager. ,expr)) cenv)]
    [_ (error "syntax-error: malformed delay:" form)]))

;; Module related ............................................

(define-pass1-syntax (define-module form cenv) :gauche
  (check-toplevel form cenv)
  (match form
    [(_ name body ...)
     (let* ([mod (ensure-module name 'define-module #t)]
            [newenv (make-bottom-cenv mod)])
       ($seq (imap (cut pass1 <> newenv) body)))]
    [_ (error "syntax-error: malformed define-module:" form)]))

(define-pass1-syntax (with-module form cenv) :gauche
  (match form
    [(_ name body ...)
     (let* ([mod (ensure-module name 'with-module #f)]
            [newenv (cenv-swap-module cenv mod)])
       ($seq (imap (cut pass1 <> newenv) body)))]
    [_ (error "syntax-error: malformed with-module:" form)]))

(define-pass1-syntax (select-module form cenv) :gauche
  (check-toplevel form cenv)
  (match form
    [(_ module)
     ;; This is the only construct that changes VM's current module.
     ;; We also modifies CENV's module, so that select-module has an
     ;; effect in the middle of sequence of expressions like
     ;;  (begin ... (select-module foo) ...)
     ;; It is yet debatable that how select-module should interact with EVAL.
     (let1 m (ensure-module module 'select-module #f)
       (vm-set-current-module m)
       (cenv-module-set! cenv m)
       ($const-undef))]
    [else (error "syntax-error: malformed select-module:" form)]))

(define-pass1-syntax (current-module form cenv) :gauche
  (unless (null? (cdr form))
    (error "syntax-error: malformed current-module:" form))
  ($const (cenv-module cenv)))

(define-pass1-syntax (export form cenv) :gauche
  (%export-symbols (cenv-module cenv) (cdr form))
  ($const-undef))

(define-pass1-syntax (export-all form cenv) :gauche
  (unless (null? (cdr form))
    (error "syntax-error: malformed export-all:" form))
  (%export-all (cenv-module cenv))
  ($const-undef))

(define-pass1-syntax (import form cenv) :gauche
  (define (ensure m) (or (find-module m) (error "unknown module" m)))
  (define (symbol-but-not-keyword? x)
    (and (symbol? x) (not (keyword? x))))
  (dolist [f (cdr form)]
    (match f
      [((? symbol-but-not-keyword? a) (? symbol-but-not-keyword? b) . r)
       ;;likely to be an r7rs-style import
       (error "This import form looks like R7RS `import', as opposed to \
               Gauche `import'.  If you're in REPL, type (use r7rs) \
               and (select-module r7rs.user) to enter the R7RS namespace.")]
      [(m . r) (process-import (cenv-module cenv) (ensure m) r)]
      [m       (process-import (cenv-module cenv) (ensure m) '())]))
  ($const-undef))

(define (process-import current imported args)
  (let loop ([imported imported]
             [args args]
             [prefix #f])
    (match args
      [() (%import-module current imported prefix)]
      [(':prefix p . rest)
       (loop imported rest (if prefix (string->symbol #"~|p|~prefix") p))]
      [(':only (ss ...) . rest)
       (let1 m (%make-wrapper-module imported prefix)
         (process-import:mapsym
          :only (unwrap-syntax ss) #f prefix
          (^[sym orig-sym] (unless (%alias-binding m orig-sym imported orig-sym)
                             (errorf "during processing :only clause: \
                                      binding of ~a isn't exported from ~a"
                                     orig-sym imported))))
         (%extend-module m '())
         (loop m rest #f))]
      [(':except (ss ...) . rest)
       (let1 m (%make-wrapper-module imported prefix)
         (process-import:mapsym
          :except (unwrap-syntax ss) #f prefix
          (^[sym orig-sym] (%hide-binding m orig-sym)))
         (loop m rest #f))]
      [(':rename ((ss ds) ...) . rest)
       (let* ([ss (unwrap-syntax ss)]
              [ds (unwrap-syntax ds)]
              [m0 (if prefix (%make-wrapper-module imported prefix) imported)]
              [m (%make-wrapper-module imported #f)])
         (process-import:mapsym
          :rename ds ss prefix
          (^[sym orig-sym] (unless (%alias-binding m sym imported orig-sym)
                             (errorf "during processing :rename clause: \
                                      binding of ~a isn't exported from ~a"
                                     orig-sym imported))))
         (dolist [s ss] (unless (find-binding m s #t) (%hide-binding m s)))
         (%extend-module m (list m0))
         (loop m rest #f))]
      [(other . rest) (error "invalid import spec:" args)])))

;; Common work to process new bindings in a trampoline module.
;; Calls PROCESS with each symbols in SYMS and OLD-SYMS, but
;; symbols in OLD-SYMS are prefix-stripped.  OLD-SYMS can be #f
;; then we assume it is the same as SYMS.
(define (process-import:mapsym who syms old-syms prefix process)
  (define (check s)
    (unless (symbol? s)
      (errorf "~a option of import must take list of symbols, but got: ~s"
              who s)))
  (for-each (^[sym osym]
              (check sym) (check osym)
              (process sym (process-import:strip-prefix who osym prefix)))
            syms (or old-syms syms)))

(define (process-import:strip-prefix who sym prefix)
  (if prefix
    (rlet1 sans (symbol-sans-prefix sym prefix)
      (unless sans (errorf "~a specifies nonexistent symbol: ~a" who sym)))
    sym))

(define-pass1-syntax (extend form cenv) :gauche
  (%extend-module (cenv-module cenv)
                  (imap (^[m] (or (find-module m)
                                  (begin
                                    (%require (module-name->path m))
                                    (find-module m))
                                  (error "undefined module" m)))
                        (cdr form)))
  ($const-undef))

(define-pass1-syntax (require form cenv) :gauche
  (match form
    [(_ feature) (%require feature) ($const-undef)]
    [_ (error "syntax-error: malformed require:" form)]))

;; Include .............................................

(define-pass1-syntax (include form cenv) :gauche
  ($seq (map (^p (pass1 (car p) (cenv-swap-source cenv (cdr p))))
             (pass1/expand-include (cdr form) cenv #f))))

(define-pass1-syntax (include-ci form cenv) :gauche
  ($seq (map (^p (pass1 (car p) (cenv-swap-source cenv (cdr p))))
             (pass1/expand-include (cdr form) cenv #t))))

;; Returns  ((Sexpr . Filename) ...)
(define (pass1/expand-include args cenv case-fold?)
  (define (do-include filename)
    (unless (string? filename)
      (error "include requires literal string, but got:" filename))
    (let1 iport (pass1/open-include-file filename (cenv-source-path cenv))
      (port-case-fold-set! iport case-fold?)
      (pass1/report-include iport #t)
      (unwind-protect
          ;; This could be written simpler using port->sexp-list, but it would
          ;; trigger autoload and reenters to the compiler.
          (let loop ([r (read iport)] [forms '()])
            (if (eof-object? r)
              `((,begin. ,@(reverse forms)) . ,(port-name iport))
              (loop (read iport) (cons r forms))))
        (pass1/report-include iport #f)
        (close-input-port iport))))
  (map do-include args))
  
;; If filename is relative, we try to resolve it with the source file.
;; whenever possible.
(define (pass1/open-include-file path includer-path)
  (let1 search-paths (if includer-path
                       (cons (sys-dirname includer-path) *load-path*)
                       *load-path*)
    ;; find-load-file returns either (<found-path> <rest-of-search-paths>)
    ;; or (<pseudo-path> <rest-of-search-paths> <thunk-to-open-content>)
    ;; see libeval.scm for the details.
    (if-let1 path&rest (find-load-file path search-paths
                                       (cons "" *load-suffixes*)
                                       :allow-archive #t)
      (if (pair? (cddr path&rest)) ; archive hook is in effect.
        ((caddr path&rest))
        (open-input-file (car path&rest) :encoding #t))
      (error "include file is not readable:" path))))

;; Report including.
(define (pass1/report-include iport open?)
  (when (vm-compiler-flag-is-set? SCM_COMPILE_INCLUDE_VERBOSE)
    (format (current-error-port) ";;~a including ~s\n"
            (if open? "Begin" "End") (port-name iport))))

;; Class stuff ........................................

;; KLUDGES.  They should be implemented as macros, but the
;; current compiler doesn't preserve macro definitions.
;; These syntax handler merely expands the given form to
;; the call to internal procedures of objlib.scm, which
;; returns the macro expanded form.

(define-pass1-syntax (define-generic form cenv) :gauche
  (match form
    [(_ name . opts)
     (check-toplevel form cenv)
     (pass1 (with-module gauche.object (%expand-define-generic name opts)) cenv)]
    [_ (error "syntax-error: malformed define-generic:" form)]))

(define-pass1-syntax (define-method form cenv) :gauche
  (match form
    [(_ name specs . body)
     ;; Should we limit define-method only at the toplevel?  Doing so
     ;; is consistent with toplevel define and define-syntax.  Allowing
     ;; define-method in non-toplevel is rather CL-ish and not like Scheme.
     ;(check-toplevel form cenv)
     (pass1 (with-module gauche.object (%expand-define-method name specs body))
            cenv)]
    [_ (error "syntax-error: malformed define-method:" form)]))

(define-pass1-syntax (define-class form cenv) :gauche
  (match form
    [(_ name supers slots . options)
     (check-toplevel form cenv)
     (pass1 (with-module gauche.object
              (%expand-define-class name supers slots options))
            cenv)]
    [_ (error "syntax-error: malformed define-class:" form)]))

;; Black magic ........................................

(define-pass1-syntax (eval-when form cenv) :gauche
  (match form
    [(_ (w ...) expr ...)
     ;; check
     (let ([wlist
            (let loop ((w w) (r '()))
              (cond [(null? w) r]
                    [(memq (car w) '(:compile-toplevel :load-toplevel :execute))
                     (if (memq (car w) r)
                       (loop (cdr w) r)
                       (loop (cdr w) (cons (car w) r)))]
                    [else
                     (error "eval-when: situation must be a list of \
                             :compile-toplevel, :load-toplevel or :execute, \
                             but got:" (car w))]))]
           [situ (vm-eval-situation)])
       (when (and (eqv? situ SCM_VM_COMPILING)
                  (memq :compile-toplevel wlist)
                  (cenv-toplevel? cenv))
         (dolist [e expr] (eval e (cenv-module cenv))))
       (if (or (and (eqv? situ SCM_VM_LOADING)
                    (memq :load-toplevel wlist)
                    (cenv-toplevel? cenv))
               (and (eqv? situ SCM_VM_EXECUTING)
                    (memq :execute wlist)))
         ($seq (imap (cut pass1 <> cenv) expr))
         ($const-undef)))]
    [_ (error "syntax-error: malformed eval-when:" form)]))

#|
(define-pass1-syntax (with-meta form cenv) :gauche
  (match form
    [(_ (meta ...) expr)
     (let1 exp (pass1 expr cenv)
       exp)]
    [_ (error "syntax-error: malformed with-meta:" form)]))
|#

;;===============================================================
;; Pass 2.  Optimization stage 1
;;

;; Walk down IForm and perform optimizations.
;; The main focus is to lift or inline closures, and eliminate
;; local frames by beta reduction.
;; This pass may modify the tree by changing IForm nodes destructively.

;; Dispatch pass2 handler.
;; Each handler is called with three arguments: the IForm, Penv, and Tail?
;; Penv is a list of $LAMBDA nodes that we're compiling.   It is used to
;; detect self-recursive local calls.  Tail? is a flag to indicate whether
;; the expression is tail position or not.
;; Each handler returns IForm.
;; *pass2-dispatch-table* is defined below, after all handlers are defined.
(define-inline (pass2/rec iform penv tail?)
  ((vector-ref *pass2-dispatch-table* (iform-tag iform))
   iform penv tail?))

;;
;; Pass 2 entry point.
;;
(define (pass2 iform) (pass2/rec iform '() #t))

;;
;; Pass 2 handlers
;;

(define (pass2/$DEFINE iform penv tail?)
  ($define-expr-set! iform (pass2/rec ($define-expr iform) penv #f))
  iform)

;; LREF optimization.
;; Check if we can replace the $lref to its initial value.
;;  - If the lvar is never set!
;;     - if its init value is $const, just replace it
;;     - if its init value is $lref, replace it iff it is not set!,
;;       then repeat.
;;
;; There's a special LREF optimization when it appears in the operator
;; position.  If it is bound to $LAMBDA, we may be able to inline the
;; lambda.  It is dealt by pass2/head-lref, which is called by pass2/$CALL.

(define (pass2/$LREF iform penv tail?) (pass2/lref-eliminate iform))

(define (pass2/lref-eliminate iform)
  (let1 lvar ($lref-lvar iform)
    (if (lvar-immutable? lvar)
      (let1 initval (lvar-initval lvar)
        (cond [(not (vector? initval)) iform]
              [($const? initval)
               (lvar-ref--! lvar)
               (vector-set! iform 0 $CONST)
               ($const-value-set! iform ($const-value initval))
               iform]
              [(and ($lref? initval)
                    (lvar-immutable? ($lref-lvar initval)))
               (when (eq? iform initval)
                 (error "circular reference appeared in letrec bindings:"
                        (lvar-name lvar)))
               (lvar-ref--! lvar)
               (lvar-ref++! ($lref-lvar initval))
               ($lref-lvar-set! iform ($lref-lvar initval))
               (pass2/lref-eliminate iform)]
              ;; Generally we can't reorder $GREF, since it may change
              ;; the semantics (the value of the variable may be altered,
              ;; or it raises an unbound error).  However, if $GREF refers to
              ;; an inlinable binding, we can assume it is bound and its
              ;; value won't be changed.  NB: Constant bindings are already
              ;; dissolved in pass1, so we don't need to consider it.
              [(and (has-tag? initval $GREF)
                    (gref-inlinable-gloc initval))
               (lvar-ref--! lvar)
               (vector-set! iform 0 $GREF)
               ($gref-id-set! iform ($gref-id initval))
               iform]
              [else iform]))
      iform)))

(define (pass2/$LSET iform penv tail?)
  ($lset-expr-set! iform (pass2/rec ($lset-expr iform) penv #f))
  iform)

(define (pass2/$GREF iform penv tail?) iform)

(define (pass2/$GSET iform penv tail?)
  ($gset-expr-set! iform (pass2/rec ($gset-expr iform) penv #f))
  iform)

(define (pass2/$CONST iform penv tail?) iform)

(define (pass2/$IT iform penv tail?) iform)

(define (pass2/$IF iform penv tail?)
  (let ([test-form (pass2/rec ($if-test iform) penv #f)]
        [then-form (pass2/rec ($if-then iform) penv tail?)]
        [else-form (pass2/rec ($if-else iform) penv tail?)])
    (or (pass2/branch-cut iform test-form then-form else-form)
        (pass2/update-if iform test-form then-form else-form))))

;; NB: pass2/branch-cut and pass2/update-if are also called in pass3/$IF.
(define (pass2/branch-cut iform test-form then-form else-form)
  (and ($const? test-form)
       (let1 val-form (if ($const-value test-form) then-form else-form)
         (if ($it? val-form) test-form val-form))))

(define (pass2/update-if iform new-test new-then new-else)
  (if (eq? new-then new-else)
    ($seq (list new-test new-then))     ;this case happens after pass3.
    (begin ($if-test-set! iform new-test)
           ($if-then-set! iform new-then)
           ($if-else-set! iform new-else)
           iform)))

;; Let optimization:
;;
;; - Unused variable elimination: if the bound lvars becomes unused by
;;   the result of $lref optimization, we eliminate it from the frame,
;;   and move its 'init' expression to the body.  if we're lucky, all
;;   the lvars introduced by this let are eliminated, and we can change
;;   this iform into a simple $seq.
;;
;;  -- Special case: If the lvars are immediately "consumed" as the
;;     arguments to a function calls in the first expression of let,
;;     and the rest of the argument of the first expression is side-effect
;;     free, we can eliminate lvars.  This case often occurs after macro
;;     expansion.
;;       (let ([p (f a)] [q (g b)]) (h p q) (foo))
;;         => (begin (h (f a) (g b)) (foo))
;;
;;     NB: In the following pattern, we can't do the conversion unless p is
;;     constant or unmodified $LREF, for (g b) might modify p.
;;     
;;       (let ([q (g b)]) (h p q) (foo))
;;
;;     NB: We don't need to consider the possibility that (g b) modifies h,
;;     since h is evaluated after all the arguments are evaluated, even
;;     in the form of (h p (g b)).
;;
;; - Closure optimization: when an lvar is bound to a $LAMBDA node, we
;;   may be able to optimize the calls to it.  It is done here since
;;   we need to run pass2 for all the call sites of the lvar to analyze
;;   its usage.
;;
;; CAVEAT: When we go through pass2 on ($let-inits iform), it may inline expand
;; lvars that appear later in the bindings, e.g.:
;;
;;   (letrec ((foo (lambda () (bar x)))
;;            (bar (lambda (a) (baz a))))
;;     ...)
;;
;; In this case, while we're at pass2[(lambda () (bar x))], pass2/$CALL
;; inlines the call of bar to make it (lambda () (let ((a x)) (baz a))).
;; The important thing is that the IForm of original (lambda (a) (baz a))
;; is directly used to inline the call, so we shouldn't rescan it.
;; pass2/$CALL marks the inlined lambda node as 'used, so that we can skip
;; it here.

(define (pass2/$LET iform penv tail?)
  ;; Run pass2 on let-inits, returns new lvars and inits
  (define (process-inits lvars inits)
    (let loop ([lvars lvars] [inits inits]
               [new-lvars '()] [new-inits '()])
      (cond [(null? lvars) (values (reverse! new-lvars) (reverse! new-inits))]
            [(let1 lv (car lvars)
               (and (= (lvar-ref-count lv) 0)
                    (lvar-immutable? lv)
                    (has-tag? (car inits) $LAMBDA)
                    (eq? ($lambda-flag (car inits)) 'used)))
             ;; This lambda node has already been inlinded, so we can skip.
             (loop (cdr lvars) (cdr inits) new-lvars new-inits)]
            [else
             (loop (cdr lvars) (cdr inits)
                   (cons (car lvars) new-lvars)
                   (cons (pass2/rec (car inits) penv #f) new-inits))])))

  (receive (lvars inits) (process-inits ($let-lvars iform) ($let-inits iform))
    (ifor-each2 (^[lv in] (lvar-initval-set! lv in)) lvars inits)
    (let1 obody (pass2/rec ($let-body iform) penv tail?)
      ;; NB: We have to run optimize-closure after pass2 of body.
      (for-each pass2/optimize-closure lvars inits)
      (pass2/shrink-let-frame iform lvars obody))))

(define (pass2/shrink-let-frame iform lvars obody)
  (pass2/intermediate-lref-removal lvars obody)
  (receive (new-lvars new-inits removed-inits)
      (pass2/remove-unused-lvars lvars ($let-type iform))
    (cond [(null? new-lvars)
           (if (null? removed-inits)
             obody
             ($seq (append! removed-inits (list obody))))]
          [else
           ($let-lvars-set! iform new-lvars)
           ($let-inits-set! iform new-inits)
           ($let-body-set! iform obody)
           (unless (null? removed-inits)
             (if (has-tag? obody $SEQ)
               ($seq-body-set! obody
                               (append! removed-inits ($seq-body obody)))
               ($let-body-set! iform
                               ($seq (append removed-inits (list obody))))))
           iform])))

;; handle the special case in the above comment
(define (pass2/intermediate-lref-removal lvars body)
  ;; returns the call node who has replacable intermediate lrefs, or #f if
  ;; we can't do this transformation.
  (define (intermediate-lrefs node lvars)
    (let loop ([args ($call-args node)] [ilrefs '()] [subcall-node #f])
      (match args
        [() (if subcall-node
              (and (null? ilrefs) (intermediate-lrefs subcall-node lvars))
              (and (not (null? ilrefs)) node))]
        [((? $const? n) . args) (loop args ilrefs subcall-node)]
        [((? $lref? n) . args)
         (let1 lv ($lref-lvar n)
           (if (memq lv lvars)
             (and (= (lvar-ref-count lv) 1)
                  (lvar-immutable? lv)
                  (loop args (cons n ilrefs) subcall-node))
             (and (lvar-immutable? lv)
                  (loop args ilrefs subcall-node))))]
        [((? $call? n) . args) (if subcall-node
                                 #f
                                 (loop args ilrefs n))]
        [_ #f])))

  (and-let* ([first-expr (if (has-tag? body $SEQ)
                           (and (not (null? ($seq-body body)))
                                (car ($seq-body body)))
                           body)]
             [ (has-tag? first-expr $CALL) ]
             [node (intermediate-lrefs first-expr lvars)])
    ($call-args-set! node
                     (imap (^[arg] (if (and ($lref? arg)
                                            (memq ($lref-lvar arg) lvars))
                                     (rlet1 v (lvar-initval ($lref-lvar arg))
                                       (lvar-ref--! ($lref-lvar arg))
                                       (lvar-initval-set! ($lref-lvar arg)
                                                          ($const-undef)))
                                     arg))
                           ($call-args node)))))

;; Scan LVARS and returns three values:
;;   - List of needed lvars
;;   - List of init expressions, corresponding to the first return value.
;;   - List of non-transparent init expressions for removed lvars---they
;;     need to be executed at the top of the body of the binding construct.
;;
;; We have to be careful optimizing letrec* - we can't reorder init
;; when it can have side effects.  However, we still have to remove lambda
;; form that is no longer used---that means the lambda form is inlined
;; elsewhere, and its body has been modified to suit the inlined environment,
;; so we can no longer compile the $lambda node safely.
(define (pass2/remove-unused-lvars lvars type)
  (let loop ([lvars lvars] [rl '()] [ri '()] [rr '()])
    (cond [(null? lvars)
           (values (reverse rl) (reverse ri) (reverse rr))]
          [(and (zero? (lvar-ref-count (car lvars)))
                (lvar-immutable? (car lvars)))
           (let1 init (lvar-initval (car lvars))
             (if (and (eq? type 'rec*)
                      (not (transparent? init)))
               (loop (cdr lvars) (cons (car lvars) rl) (cons init ri) rr)
               (loop (cdr lvars) rl ri
                     (cond [($lref? init)
                            (lvar-ref--! ($lref-lvar init))
                            rr]
                           [(transparent? init) rr]
                           [else (cons init rr)]))))]
          [else
           (loop (cdr lvars)
                 (cons (car lvars) rl)
                 (cons (lvar-initval (car lvars)) ri)
                 rr)])))

;; Closure optimization (called from pass2/$LET)
;;
;;   Determine the strategy to optimize each closure, and modify the nodes
;;   accordingly.  We can't afford time to run iterative algorithm to find
;;   optimal strategy, so we take a rather simple-minded path to optimize
;;   common cases.
;;
;;   By now, we have the information of all call sites of the statically
;;   bound closures.   Each call site is marked as either REC, TAIL-REC
;;   or LOCAL.  See explanation of pass2/$CALL below.
;;
;;   Our objective is to categorize each call site to one of the following
;;   four options:
;;
;;     LOCAL: when we can't avoid creating a closure, calls to it are marked
;;     as "local".  The call to the local closure becomes a LOCAL-ENV-CALL
;;     instruction, which is faster than generic CALL/TAIL-CALL instructions.
;;
;;     EMBED: the lambda body is inlined at the call site.  It differs from
;;     the normal inlining in a way that we don't run beta-conversion of
;;     lrefs, since the body can be entered from other 'jump' call sites.
;;
;;     JUMP: the call becomes a LOCAL-ENV-JUMP instruction, i.e. a jump
;;     to the lambda body which is generated by the 'embed' call.
;;
;;   We can inline $LAMBDA if the following conditions are met:
;;
;;     1. The reference count of LVAR is equal to the number of call
;;        sites.  This means all use of this $LAMBDA is first-order,
;;        so we know the closure won't "leak out".
;;
;;     2. It doesn't have any REC call sites, i.e. non-tail self recursive
;;        calls.  (We may be able to convert non-tail self recursive calls
;;        to jump with environment adjustment, but it would complicates
;;        stack handling a lot.)
;;
;;     3. It doesn't have any TAIL-REC calls across closure boundary.
;;
;;         (letrec ((foo (lambda (...)
;;                           ..... (foo ...)) ;; ok
;;           ...
;;
;;         (letrec ((foo (lambda (...) ....
;;                         (lambda () ... (foo ...)) ;; not ok
;;           ...
;;
;;     4. Either:
;;         - It has only one LOCAL call, or
;;         - It doesn't have TAIL-REC calls and the body of $LAMBDA is
;;           small enough to duplicate.
;;
;;   If we determine $LAMBDA to be inlined, all LOCAL calls become EMBED
;;   calls, and TAIL-RECs become JUMP.
;;
;;   Otherwise, if the first condition is met, we can lift $LAMBDA to
;;   the toplevel by...
;;
;;     a. Scan the lambda body to find free lvars.  If there's any,
;;        transform free lvars to bound lvars by adding new arguments
;;        to the $LAMBDA node, and modifies all the call sites
;;        accordingly.
;;
;;   Otherwise, all calls become LOCAL calls.
;;

(define (pass2/optimize-closure lvar lambda-node)
  (when (and (lvar-immutable? lvar)
             (> (lvar-ref-count lvar) 0)
             (has-tag? lambda-node $LAMBDA))
    (or (and (= (lvar-ref-count lvar) (length ($lambda-calls lambda-node)))
             (receive (locals recs tail-recs)
                 (pass2/classify-calls ($lambda-calls lambda-node) lambda-node)
               (and (null? recs)
                    (pair? locals)
                    (or (and (null? (cdr locals))
                             (pass2/local-call-embedder lvar lambda-node
                                                        (car locals)
                                                        tail-recs))
                        (and (null? tail-recs)
                             (< (iform-count-size-upto lambda-node
                                                       SMALL_LAMBDA_SIZE)
                                SMALL_LAMBDA_SIZE)
                             (pass2/local-call-inliner lvar lambda-node
                                                       locals))))))
        (pass2/local-call-optimizer lvar lambda-node))))

;; Classify the calls into categories.  TAIL-REC call is classified as
;; REC if the call is across the closure boundary.
(define (pass2/classify-calls call&envs lambda-node)
  (define (direct-call? env)
    (let loop ([env env])
      (cond [(null? env) #t]
            [(eq? (car env) lambda-node) #t]
            [(eq? ($lambda-flag (car env)) 'dissolved)
             (loop (cdr env))] ;; skip dissolved (inlined) lambdas
            [else #f])))
  (let loop ([call&envs call&envs]
             [local '()]
             [rec '()]
             [trec '()])
    (match call&envs
      [() (values local rec trec)]
      [((call . env) . more)
       (case ($call-flag call)
         [(tail-rec) (if (direct-call? env)
                       (loop more local rec (cons call trec))
                       (loop more local (cons call rec) trec))]
         [(rec)      (loop more local (cons call rec) trec)]
         [else       (loop more (cons call local) rec trec)])])))

;; Set up local calls to LAMBDA-NODE.  Marking $call node as 'local
;; lets pass5 to generate LOCAL-ENV-CALL instruction.
(define (pass2/local-call-optimizer lvar lambda-node)
  (let ([nreqs ($lambda-reqargs lambda-node)]
        [nopts ($lambda-optarg lambda-node)]
        [name  ($lambda-name lambda-node)]
        [calls ($lambda-calls lambda-node)])
    (dolist [call calls]
      ($call-args-set! (car call) (adjust-arglist nreqs nopts
                                                  ($call-args (car call))
                                                  name))
      ($call-flag-set! (car call) 'local))
    ;; We clear the calls list, just in case if the lambda-node is
    ;; traversed more than once.
    ($lambda-calls-set! lambda-node '())))

;; Called when the local function (lambda-node) isn't needed to be a closure
;; and can be embedded.
;; NB: this operation introduces a shared/circular structure in the IForm.
(define (pass2/local-call-embedder lvar lambda-node call rec-calls)
  (let ([nreqs ($lambda-reqargs lambda-node)]
        [nopts ($lambda-optarg lambda-node)]
        [name  ($lambda-name lambda-node)])
    ($call-args-set! call (adjust-arglist nreqs nopts ($call-args call) name))
    (lvar-ref--! lvar)
    ($call-flag-set! call 'embed)
    ($call-proc-set! call lambda-node)
    ($lambda-flag-set! lambda-node 'dissolved)
    ($lambda-body-set! lambda-node ($label ($lambda-src lambda-node) #f
                                           ($lambda-body lambda-node)))
    (unless (null? rec-calls)
      (dolist [jcall rec-calls]
        (lvar-ref--! lvar)
        ($call-args-set! jcall (adjust-arglist nreqs nopts ($call-args jcall)
                                               name))
        ($call-proc-set! jcall call)
        ($call-flag-set! jcall 'jump)))))

;; Called when the local function (lambda-node) doesn't have recursive
;; calls, can be inlined, and called from multiple places.
;; NB: Here we destructively modify $call node to change it to $seq,
;; in order to hold the $LET node.  It breaks the invariance that $seq
;; contains zero or two or more nodes---this may prevent Pass 5 from
;; doing some optimization.
(define (pass2/local-call-inliner lvar lambda-node calls)
  (define (inline-it call-node lambda-node)
    (let1 inlined (expand-inlined-procedure ($*-src lambda-node) lambda-node
                                            ($call-args call-node))
      (vector-set! call-node 0 $SEQ)
      (if (has-tag? inlined $SEQ)
        ($seq-body-set! call-node ($seq-body inlined))
        ($seq-body-set! call-node (list inlined)))))

  (lvar-ref-count-set! lvar 0)
  ($lambda-flag-set! lambda-node 'dissolved)
  (let loop ([calls calls])
    (cond [(null? (cdr calls))
           (inline-it (car calls) lambda-node)]
          [else
           (inline-it (car calls) (iform-copy lambda-node '()))
           (loop (cdr calls))])))

(define (pass2/$RECEIVE iform penv tail?)
  ($receive-expr-set! iform (pass2/rec ($receive-expr iform) penv #f))
  ($receive-body-set! iform (pass2/rec ($receive-body iform) penv tail?))
  iform)

(define (pass2/$LAMBDA iform penv tail?)
  ($lambda-body-set! iform (pass2/rec ($lambda-body iform)
                                      (cons iform penv) #t))
  iform)

(define (pass2/$LABEL iform penv tail?)
  ;; $LABEL's body should already be processed by pass2, so we don't need
  ;; to do it again.
  iform)

(define (pass2/$PROMISE iform penv tail?)
  ($promise-expr-set! iform (pass2/rec ($promise-expr iform) penv #f))
  iform)

(define (pass2/$SEQ iform penv tail?)
  (if (null? ($seq-body iform))
    iform
    (let loop ([body ($seq-body iform)]
               [r '()])
      (cond [(null? (cdr body))
             ($seq-body-set! iform
                             (reverse (cons (pass2/rec (car body) penv tail?)
                                             r)))
             iform]
            [else
             (loop (cdr body)
                   (cons (pass2/rec (car body) penv #f) r))]))))

;; Call optimization
;;   We try to inline the call whenever possible.
;;
;;   1. If proc is $LAMBDA, we turn the whole struct into $LET.
;;
;;        ($call ($lambda .. (LVar ...) Body) Arg ...)
;;         => ($let (LVar ...) (Arg ...) Body)
;;
;;   2. If proc is $LREF which is statically bound to a $LAMBDA,
;;      call pass2/head-lref to see if we can safely inline it.
;;
;;   The second case has several subcases.
;;    2a. Some $LAMBDA nodes can be directly inlined, e.g. the whole
;;        $CALL node can be turned into $LET node.  The original $LAMBDA
;;        node vanishes if all the calls to the $LAMBDA node are first-order.
;;
;;        ($call ($lref lvar0) Arg ...)
;;          where lvar0 => ($lambda .... (LVar ...) Body)
;;
;;         => ($let (LVar ...) (Arg ...) Body)
;;
;;    2b. When $LAMBDA node is recursively called, or is called multiple
;;        times, we need more information to determine how to optimize it.
;;        So at this moment we just mark the $CALL node, and pushes
;;        it and the current penv to the 'calls' slot of the $LAMBDA node.
;;        After we finish Pass 2 of the scope of lvar0, we can know how to
;;        optimize the $LAMBDA node, and those $CALL nodes are revisited
;;        and modified accordingly.
;;
;;        If the $CALL node is a non-recursive local call, the $CALL node
;;        is marked as 'local'.  If it is a recursive call, it is marked
;;        as 'rec'.

(define (pass2/$CALL iform penv tail?)
  (cond
   [($call-flag iform) iform] ;; this node has already been visited.
   [else
    ;; scan OP first to give an opportunity of variable renaming
    ($call-proc-set! iform (pass2/rec ($call-proc iform) penv #f))
    (let ([proc ($call-proc iform)]
          [args ($call-args iform)])
      (cond
       [(vm-compiler-flag-noinline-locals?)
        ($call-args-set! iform (imap (cut pass2/rec <> penv #f) args))
        iform]
       [(has-tag? proc $LAMBDA) ;; ((lambda (...) ...) arg ...)
        (pass2/rec (expand-inlined-procedure ($*-src iform) proc args)
                   penv tail?)]
       [(and ($lref? proc)
             (pass2/head-lref proc penv tail?))
        => (^[result]
             (cond
              [(vector? result)
               ;; Directly inlinable case.  NB: this only happens if the $LREF
               ;; node is the lvar's single reference, so we know the inlined
               ;; procedure is never called recursively.  Thus we can safely
               ;; traverse the inlined body without going into infinite loop.
               ;;
               ;; We directly embed the iform (result), which is a lambda expr
               ;; bound to PROC.  The lambda iform may not be scanned yet by
               ;; pass2/$LET, though.  We mark the node 'used, so that
               ;; pass2/$LET can skip processing it.
               ($lambda-flag-set! result 'used)
               (pass2/rec (expand-inlined-procedure ($*-src iform) result args)
                          penv tail?)]
              [else
               ;; We need more info to decide optimizing this node.  For now,
               ;; we mark the call node by the returned flag and push it
               ;; to the $LAMBDA node.
               (let1 lambda-node (lvar-initval ($lref-lvar proc))
                 ($call-flag-set! iform result)
                 ($lambda-calls-set! lambda-node
                                     (acons iform penv
                                            ($lambda-calls lambda-node)))
                 ($call-args-set! iform (imap (cut pass2/rec <> penv #f) args))
                 iform)]))]
       [else
        ($call-args-set! iform (imap (cut pass2/rec <> penv #f) args))
        iform]))]))

;; Check if IFORM ($LREF node) can be a target of procedure-call optimization.
;;   - If IFORM is not statically bound to $LAMBDA node,
;;     returns #f.
;;   - If the $LAMBDA node that can be directly inlined, returns
;;     the $LAMBDA node.
;;   - If the call is self-recursing, returns 'tail-rec or 'rec, depending
;;     on whether this call is tail call or not.
;;   - Otherwise, returns 'local.

(define (pass2/head-lref iform penv tail?)
  (and-let* ([lvar ($lref-lvar iform)]
             [initval (lvar-const-value lvar)]
             [ (has-tag? initval $LAMBDA) ])
    (cond
     [(pass2/self-recursing? initval penv) (if tail? 'tail-rec 'rec)]
     [(and (= (lvar-ref-count lvar) 1)
           (lvar-immutable? lvar))
      ;; we can inline this lambda directly.
      (lvar-ref--! lvar)
      initval]
     [else 'local])))

(define (pass2/self-recursing? node penv)
  (find (cut eq? node <>) penv))

(define (pass2/$ASM iform penv tail?)
  (let1 args (imap (cut pass2/rec <> penv #f) ($asm-args iform))
    (pass2/check-constant-asm iform args)))

(define (pass2/check-constant-asm iform args)
  (or (and (every $const? args)
           (case/unquote
            (car ($asm-insn iform))
            [(NOT)     (pass2/const-pred not args)]
            [(NULLP)   (pass2/const-pred null? args)]
            [(PAIRP)   (pass2/const-pred pair? args)]
            [(CHARP)   (pass2/const-pred char? args)]
            [(STRINGP) (pass2/const-pred string? args)]
            [(VECTORP) (pass2/const-pred vector? args)]
            [(NUMBERP) (pass2/const-pred number? args)]
            [(REALP)   (pass2/const-pred real? args)]
            [(CAR)     (pass2/const-cxr car args)]
            [(CDR)     (pass2/const-cxr cdr args)]
            [(CAAR)    (pass2/const-cxxr car caar args)]
            [(CADR)    (pass2/const-cxxr cdr cadr args)]
            [(CDAR)    (pass2/const-cxxr car cdar args)]
            [(CDDR)    (pass2/const-cxxr cdr cddr args)]
            [(MEMQ)    (pass2/const-memx memq args)]
            [(MEMV)    (pass2/const-memx memv args)]
            [(ASSQ)    (pass2/const-memx assq args)]
            [(ASSV)    (pass2/const-memx assv args)]
            [(VEC-REF) (pass2/const-vecref args)]
            [(VEC-LEN) (pass2/const-veclen args)]
            [(EQ)      (pass2/const-op2 eq? args)]
            [(EQV)     (pass2/const-op2 eqv? args)]
            [(NUMADD2) (pass2/const-numop2 + args)]
            [(NUMSUB2) (pass2/const-numop2 - args)]
            [(NUMMUL2) (pass2/const-numop2 * args)]
            [(NUMDIV2) (pass2/const-numop2 / args)]
            [(NEGATE)  (pass2/const-numop1 - args)]
            [else #f]))
      (and-let* ([ (pair? args) ]
                 [ (null? (cdr args)) ]
                 [ ($lref? (car args)) ]
                 [lvar ($lref-lvar (car args))]
                 [initval (lvar-const-value lvar)])
        (case/unquote
         (car ($asm-insn iform))
         [(NULLP) (and (initval-never-null? initval)
                       (begin (lvar-ref--! lvar) ($const-f)))]
         [(NOT)   (and (initval-never-false? initval)
                       (begin (lvar-ref--! lvar) ($const-f)))]
         [(PAIRP) (and (initval-always-pair? initval)
                       (begin (lvar-ref--! lvar) ($const-t)))]
         [else #f]))
      (begin ($asm-args-set! iform args) iform)))

(define (pass2/const-pred pred args)
  (if (pred ($const-value (car args))) ($const-t) ($const-f)))

(define (pass2/const-cxr proc args)
  (let1 v ($const-value (car args))
    (and (pair? v) ($const (proc v)))))

(define (pass2/const-cxxr proc0 proc args)
  (let1 v ($const-value (car args))
    (and (pair? v) (pair? (proc0 v)) ($const (proc v)))))

(define (pass2/const-memx proc args)
  (let ([item ($const-value (car args))]
        [lis  ($const-value (cadr args))])
    (and (list? lis) ($const (proc item lis)))))

(define (pass2/const-op2 proc args)
  ($const (proc ($const-value (car args)) ($const-value (cadr args)))))

(define (pass2/const-numop1 proc args)
  (let1 n ($const-value (car args))
    (and (number? n)
         (not (and (eq? proc /) (eqv? y 0))) ; don't fold zero-division case
         ($const (proc n)))))

(define (pass2/const-numop2 proc args)
  (let ([x ($const-value (car args))]
        [y ($const-value (cadr args))])
    (and (number? x) (number? y)
         (not (and (eq? proc /) (exact? x) (eqv? y 0))) ; ditto
         ($const (proc x y)))))

(define (pass2/const-vecref args)       ;args has always 2 elements
  (let ([v ($const-value (car args))]
        [i ($const-value (cadr args))])
    (and (vector? v) (exact? i) (integer? i) (< -1 i (vector-length v))
         ($const (vector-ref v i)))))

(define (pass2/const-veclen args)
  (let1 v ($const-value (car args))
    (and (vector? v) ($const (vector-length v)))))

(define (initval-never-null? val)
  (and (vector? val)
       (let1 tag (iform-tag val)
         (or (and (eqv? tag $LIST) (not (null? ($*-args val))))
             (and (eqv? tag $LIST*) (not (null? ($*-args val))))
             (memv tag `(,$LAMBDA ,$PROMISE ,$CONS ,$EQ? ,$EQV?
                                  ,$VECTOR ,$LIST->VECTOR))))))

(define (initval-never-false? val)
  (and (vector? val)
       (let1 tag (iform-tag val)
         (memv tag `(,$LAMBDA ,$PROMISE ,$CONS ,$VECTOR
                     ,$LIST->VECTOR ,$LIST)))))

(define (initval-always-pair? val)
  (and (vector? val)
       (or (has-tag? val $CONS)
           (and (has-tag? val $LIST)
                (pair? ($*-args val)))
           (and (has-tag? val $LIST*)
                (pair? ($*-args val))
                (pair? (cdr ($*-args val)))))))

(define (initval-always-procedure? val)
  (and (vector? val) (has-tag? val $LAMBDA)))

(define (pass2/onearg-inliner iform penv tail?)
  ($*-arg0-set! iform (pass2/rec ($*-arg0 iform) penv #f))
  iform)

(define pass2/$LIST->VECTOR pass2/onearg-inliner)

(define (pass2/twoarg-inliner iform penv tail?)
  ($*-arg0-set! iform (pass2/rec ($*-arg0 iform) penv #f))
  ($*-arg1-set! iform (pass2/rec ($*-arg1 iform) penv #f))
  iform)

(define pass2/$CONS   pass2/twoarg-inliner)
(define pass2/$APPEND pass2/twoarg-inliner)
(define pass2/$MEMV   pass2/twoarg-inliner)
(define pass2/$EQ?    pass2/twoarg-inliner)
(define pass2/$EQV?   pass2/twoarg-inliner)

(define (pass2/narg-inliner iform penv tail?)
  ($*-args-set! iform (imap (cut pass2/rec <> penv #f) ($*-args iform)))
  iform)

(define pass2/$LIST   pass2/narg-inliner)
(define pass2/$LIST*  pass2/narg-inliner)
(define pass2/$VECTOR pass2/narg-inliner)

;; Dispatch table.
(define *pass2-dispatch-table* (generate-dispatch-table pass2))

;;===============================================================
;; Pass 3.  Optimization stage 2
;;

;; Closure optimization can introduce superfluous $LET, which can
;; be optimized further.  (In fact, pass2 and pass3 can be repeated
;; until no further optimization can be possible.  However, compilation
;; speed is also important for Gauche, so we just run this pass once.)

;; Dispatch pass3 handler.
;; Each handler is called with IForm and a list of label nodes.
;; Returs IForm.
;; *pass3-dispatch-table* is defined below, after all handlers are defined.
(define-inline (pass3/rec iform labels)
  ((vector-ref *pass3-dispatch-table* (iform-tag iform)) iform labels))

;; Pass 3 entry point
;;  This pass may prune the subtree of iform because of constant
;; folding.  It may further allow pruning of other subtrees.  So, when
;; pruning occurs, pass3 records the fact by setting label-dic-info to #t.
;; We repeat the pass then.

(define (pass3 iform show?)
  (if (vm-compiler-flag-no-post-inline?)
    iform
    (let loop ([iform iform] [count 0])
      (when show? (pass3-dump iform count))
      (let* ([label-dic (make-label-dic #f)]
             [iform. (pass3/rec (reset-lvars iform) label-dic)])
        (if (label-dic-info label-dic)
          (loop iform. (+ count 1))
          iform.)))))

(define (pass3-dump iform count)
  (format #t "~78,,,'=a\n" #"pass3 #~count ")
  (pp-iform iform))

;;
;; Pass 3 handlers
;;

(define (pass3/$DEFINE iform labels)
  ($define-expr-set! iform (pass3/rec ($define-expr iform) labels))
  iform)

(define (pass3/$LREF iform labels) (pass2/lref-eliminate iform))

(define (pass3/$LSET iform labels)
  ($lset-expr-set! iform (pass3/rec ($lset-expr iform) labels))
  iform)

(define (pass3/$GREF iform labels) iform)

(define (pass3/$GSET iform labels)
  ($gset-expr-set! iform (pass3/rec ($gset-expr iform) labels))
  iform)

(define (pass3/$CONST iform labels) iform)
(define (pass3/$IT iform labels) iform)

;; If optimization:
;;
;;  If the 'test' clause of $IF node contains another $IF that has $IT in
;;  either then or else clause, the straightforward code generation emits
;;  redundant jump/branch instructions.  We translate the tree into
;;  an acyclic directed graph:
;;
;;    ($if ($if <t0> ($it) <e0>) <then> <else>)
;;     => ($if <t0> #0=($label L0 <then>) ($if <e0> #0# <else>))
;;
;;    ($if ($if <t0> <e0> ($it)) <then> <else>)
;;    ($if ($if <t0> <e0> ($const #f)) <then> <else>)
;;     => ($if <t0> ($if <e0> <then> #0=($label L0 <else>)) #0#)
;;
;;    ($if ($if <t0> ($const #f) <e0>) <then> <else>)
;;     => ($if <t0> #0=($label L0 <else>) ($if <e0> <then> #0#))
;;        iff <else> != ($it)
;;     => ($if <t0> ($const #f) ($if <e0> <then> ($it)))
;;        iff <else> == ($it)
;;
;;  NB: If <then> or <else> clause is simple enough, we just duplicate
;;      it instead of creating $label node.  It is not only for optimization,
;;      but crucial when the clause is ($IT), since it affects the Pass5
;;      code generation stage.
;;
;;  NB: The latter two patterns may seem contrived, but it appears
;;      naturally in the 'cond' clause, e.g. (cond ((some-condition?) #f) ...)
;;      or (cond .... (else #f)).
;;
;;    ($if <t0> #0=($label ...) #0#)
;;     => ($seq <t0> ($label ...))
;;
;;  This form may appear as the result of if optimization.

(define (pass3/$IF iform labels)
  (let ([test-form (pass3/rec ($if-test iform) labels)]
        [then-form (pass3/rec ($if-then iform) labels)]
        [else-form (pass3/rec ($if-else iform) labels)])
    (or (and-let* ([r (pass2/branch-cut iform test-form then-form else-form)])
          (label-dic-info-set! labels #t) ; mark that we cut a branch
          r)
        (and
         (has-tag? test-form $IF)
         (let ([test-then ($if-then test-form)]
               [test-else ($if-else test-form)])
           (cond [($it? test-then)
                  (receive (l0 l1) (pass3/label-or-dup then-form)
                    (pass2/update-if iform ($if-test test-form)
                                     l0
                                     (pass3/rec ($if #f test-else l1 else-form)
                                                 labels)))]
                 [(or ($it? test-else)
                      (and ($const? test-else)
                           (not ($const-value test-else))))
                  (receive (l0 l1) (pass3/label-or-dup else-form)
                    (pass2/update-if iform ($if-test test-form)
                                     (pass3/rec ($if #f test-then then-form l0)
                                                 labels)
                                     l1))]
                 [(and ($const? test-then)
                       (not ($const-value test-then)))
                  (receive (l0 l1) (pass3/label-or-dup else-form)
                    (pass2/update-if iform ($if-test test-form)
                                     (if ($it? l0) ($const-f) l0)
                                     (pass3/rec ($if #f test-else then-form l1)
                                                  labels)))]
                 [else #f])))
        ;; default case
        (pass2/update-if iform test-form then-form else-form))))

(define (pass3/label-or-dup iform)
  (if (memv (iform-tag iform) `(,$LREF ,$CONST ,$IT))
    (values iform (iform-copy iform '()))
    (let1 lab ($label #f #f iform)
      (values lab lab))))

(define (pass3/$LET iform labels)
  (let ([lvars ($let-lvars iform)]
        [inits (imap (cut pass3/rec <> labels) ($let-inits iform))])
    (ifor-each2 (^[lv in] (lvar-initval-set! lv in)) lvars inits)
    (pass2/shrink-let-frame iform lvars (pass3/rec ($let-body iform) labels))))

(define (pass3/$RECEIVE iform labels)
  ($receive-expr-set! iform (pass3/rec ($receive-expr iform) labels))
  ($receive-body-set! iform (pass3/rec ($receive-body iform) labels))
  iform)

(define (pass3/$LAMBDA iform labels)
  ($lambda-body-set! iform (pass3/rec ($lambda-body iform) labels))
  iform)

(define (pass3/$LABEL iform labels)
  (unless (label-seen? labels iform)
    (label-push! labels iform)
    ($label-body-set! iform (pass3/rec ($label-body iform) labels)))
  iform)

(define (pass3/$PROMISE iform labels)
  ($promise-expr-set! iform (pass3/rec ($promise-expr iform) labels))
  iform)

;; We may have a dead code in $SEQ as the result of pass2 main.
;; We eliminate if the value of subtree is not used, and it is
;; referentially transparent.
(define (pass3/$SEQ iform labels)
  (let1 xs ($seq-body iform)
    (if (null? xs)
      iform
      (let loop ([r '()] [xs xs])
        (match xs
          [(x) (cond [(null? r) (pass3/rec x labels)]
                     [else
                      ($seq-body-set! iform
                                      (reverse! (cons (pass3/rec x labels) r)))
                      iform])]
          [(x . xs) (let1 x. (pass3/rec x labels)
                      (loop (if (transparent? x.) r (cons x. r)) xs))])))))

;; Some extra optimization on $CALL.  We need to run this here, since
;; $CALL classifications needs to be done by the surrounding $LET.
;; That is:
;;   pass2 root -> leaf  : gather call sites
;;   pass2 leaf -> root  : classify calls and dissolve some closures
;;   pass3 root -> leaf  : call optimization; *we are here*

(define (pass3/$CALL iform labels)
  ($call-args-set! iform (imap (cut pass3/rec <> labels) ($call-args iform)))
  (case ($call-flag iform)
    [(jump) iform]
    [(embed) ($call-proc-set! iform (pass3/rec ($call-proc iform) labels))
             iform]
    [else (pass3/optimize-call iform labels)]))

(define (pass3/optimize-call iform labels)
  (let ([proc (pass3/rec ($call-proc iform) labels)]
        [args ($call-args iform)])
    (cond [;; If we get ($call ($let (...) body) args ...), we transform it
           ;; to ($let (...) ($call body args...)).  This may allow further
           ;; optimization.
           (has-tag? proc $LET)
           (let loop ([node proc]
                      [body ($let-body proc)])
             (cond [(has-tag? body $LET) (loop body ($let-body body))]
                   [else ($call-proc-set! iform body)
                         ($let-body-set! node iform)
                         (pass3/$LET proc labels)]))]
          [;; As the result of above opration, we may get a direct lambda
           ;; call ($call ($lambda ...) args ...)
           (has-tag? proc $LAMBDA)
           (pass3/inline-call iform proc args labels)]
          [;; If we get ($call ($gref proc) args ...) and proc is inlinable,
           ;; we can inline the call.
           (and-let* ([ (has-tag? proc $GREF) ]
                      [p (gref-inlinable-proc proc)])
             (or (and (%procedure-inliner p)
                      (pass3/late-inline iform proc p labels))
                 (and (slot-ref p 'constant)
                      (every $const? args)
                      (pass3/precompute-constant p args))))]
          [(and-let* ([ (has-tag? proc $GREF) ]
                      [ (pair? args) ]
                      [ (null? (cdr args)) ]
                      [val (if ($lref? (car args))
                             (lvar-const-value ($lref-lvar (car args)))
                             (car args))])
             (pass3/deduce-predicate-result proc val))]
          [;; Like above, but we follow $LREFs.
           ;; NB: We tempted to inline calles if the $lref is bound to $lambda
           ;; node and it has reference count 1---in a non-redundant program,
           ;; it means the $lambda is no recursive call.  Unfortunately it
           ;; breaks when $lambda does have recursive call to itself, and
           ;; the whole $lambda node isn't referenced from anywhere else.
           ;; Such unused $lambda node would be removed later, but it
           ;; introduces loop in the graph which confuses this pass.
           (and-let* ([ ($lref? proc) ]
                      [lvar ($lref-lvar proc)]
                      [val (lvar-const-value lvar)]
                      [ (has-tag? val $GREF) ]
                      [p (gref-inlinable-proc val)]
                      [ (%procedure-inliner p) ])
             (rlet1 iform. (pass3/late-inline iform val p labels)
               (when iform. (lvar-ref--! lvar))))]
          [else ($call-proc-set! iform proc) iform])))

;; Returns GLOC if gref refers to an inlinable binding
(define (gref-inlinable-gloc gref)
  (and-let* ([gloc (id->bound-gloc ($gref-id gref))]
             [ (gloc-inlinable? gloc) ])
    gloc))

;; Get the value of GREF if it is bound and inlinable procedure
(define (gref-inlinable-proc gref)
  (and-let* ([gloc (gref-inlinable-gloc gref)]
             [val  (gloc-ref gloc)]
             [ (procedure? val) ])
    val))

;; An ad-hoc table of builtin predicates that we can deduce its value
;; from what we know about its argument at compile-time.  Even the argument
;; is not a constant, we sometimes know its type and thus we know how
;; the predicate responds.   Ideally, this information should be attached
;; to individual procedures, instead of keeping it in the compiler.  For now,
;; however, we don't know how to show our internal information to such
;; custom handlers.

(define (pass3/pred:null? val)
  (and (initval-never-null? val) ($const-f)))
(define (pass3/pred:not val)
  (and (initval-never-false? val) ($const-f)))
(define (pass3/pred:pair? val)
  (and (initval-always-pair? val) ($const-t)))
(define (pass3/pred:procedure? val)
  (and (initval-always-procedure? val) ($const-t)))
(define (pass3/pred:fallback val) #f)

(define *pass3/pred-table*
  `((,(global-id 'null?)      . ,pass3/pred:null?)
    (,(global-id 'not)        . ,pass3/pred:not)
    (,(global-id 'pair?)      . ,pass3/pred:pair?)
    (,(global-id 'procedure?) . ,pass3/pred:procedure?)))

(define (pass3/find-deducible-predicate id)
  (let loop ([tab *pass3/pred-table*])
    (cond [(null? tab) pass3/pred:fallback]
          [(global-identifier=? id (caar tab)) (cdar tab)]
          [else (loop (cdr tab))])))

(define (pass3/deduce-predicate-result gref arg)
  ((pass3/find-deducible-predicate ($gref-id gref)) arg))

(define (pass3/inline-call call-node proc args labels)
  ;; This inlining may enable further inlining by post pass again.
  (label-dic-info-set! labels #t)
  (expand-inlined-procedure ($call-src call-node) proc args))

;; TODO: This is similar to pass1/expand-inliner.  Call for refactoring.
(define (pass3/late-inline call-node gref-node proc labels)
  (let ([inliner (%procedure-inliner proc)]
        [src ($call-src call-node)])
    (match inliner
     [(? integer?)                      ; VM instruction
      (let ([nargs (length ($call-args call-node))]
            [opt?  (slot-ref proc 'optional)])
        (unless (argcount-ok? ($call-args call-node)
                              (slot-ref proc 'required) opt?)
          (errorf "wrong number of arguments: ~a requires ~a, but got ~a"
                  (variable-name ($gref-id gref-node))
                  (slot-ref proc 'required) nargs))
        ($asm src (if opt? `(,inliner ,nargs) `(,inliner))
              ($call-args call-node)))]
     [(? vector?)
      (pass3/inline-call call-node (unpack-iform inliner)
                          ($call-args call-node) labels)]
     [_
      ;; We can't run procedural inliner here, since what we have is no
      ;; longer an S-expr.
      #f])))

;; PROC is inlinable, constant procedure, and args-node is all $const node.
;; So we can precompute the value and replace the $call node to a single
;; $const node.  One caveat: the application may yield an error, but if
;; we let the compiler fail, it will be confusing since even a call in
;; a dead code can be the cause.  So if we get an error, we give up this
;; optimization and let the runtime fail.
(define (pass3/precompute-constant proc arg-nodes)
  (guard (e [else #f])                  ; give up optimization
    (receive r (apply proc (imap (^[a] ($const-value a)) arg-nodes))
      (match r
        [()  ($const-undef)]
        [(r) ($const r)]
        [_   #f]))))             ;for now, don't support multivalue const

(define (pass3/$ASM iform labels)
  (let1 args (imap (cut pass3/rec <> labels) ($asm-args iform))
    (pass2/check-constant-asm iform args)))

(define (pass3/onearg-inliner iform labels)
  ($*-arg0-set! iform (pass3/rec ($*-arg0 iform) labels))
  iform)
(define pass3/$LIST->VECTOR pass3/onearg-inliner)

(define (pass3/twoarg-inliner iform labels)
  ($*-arg0-set! iform (pass3/rec ($*-arg0 iform) labels))
  ($*-arg1-set! iform (pass3/rec ($*-arg1 iform) labels))
  iform)
(define pass3/$CONS   pass3/twoarg-inliner)
(define pass3/$APPEND pass3/twoarg-inliner)
(define pass3/$MEMV   pass3/twoarg-inliner)
(define pass3/$EQ?    pass3/twoarg-inliner)
(define pass3/$EQV?   pass3/twoarg-inliner)

(define (pass3/narg-inliner iform labels)
  ($*-args-set! iform (imap (cut pass3/rec <> labels) ($*-args iform)))
  iform)
(define pass3/$LIST   pass3/narg-inliner)
(define pass3/$LIST*  pass3/narg-inliner)
(define pass3/$VECTOR pass3/narg-inliner)

;; Dispatch table.
(define *pass3-dispatch-table* (generate-dispatch-table pass3))

;;===============================================================
;; Pass 4.  Lambda lifting
;;

;; First we traverse down the IForm and find free local variables
;; for each lambda node.  Within this traversal, found $lambda nodes
;; are chained into the first element of label-dic.
;;
;; Once all free lvars are sorted out, we look at the list of $lambda
;; nodes and determine the ones that doesn't need to form a closure.
;; They are to be bound to a freshly created global identifier.  If other
;; $lambda nodes have a reference to the lifted lambda node through
;; local variables, they are substituted to the reference to this global
;; identifier.
;;
;; Note for the reader of this code: The term "lambda lifting" usually
;; includes a transformation that substitutes closed variables for
;; arguments.  We don't do such transformation so far.  It trades the
;; cost of closure allocation for pushing extra arguments.  It may be
;; a win if the closure is allocated lots of times.  OTOH, if the closure
;; is created only a few times, but called lots of times, the overhead of
;; extra arguments may exceed the gain by not allocating the closure.
;;
;; Pass4 is done in three steps.
;; The first step, pass4/scan, recursively descends the IForm and determine
;; a set of free variables for each $LAMBDA nodes.
;; The second step, pass4/lift, takes a set of $LAMBDA nodes in the IForm
;; and finds which $LAMBDA nodes can be lifted.
;; The third step, pass4/subst, walks the IForm again, and replaces the
;; occurence of $LAMBDA nodes to be lifted for $LREFs.

(define-inline (pass4/add-lvar lvar bound free)
  (if (or (memq lvar bound) (memq lvar free)) free (cons lvar free)))

;; Pass 4 entry point.  Returns IForm and list of lifted lvars
(define (pass4 iform module)
  (if (vm-compiler-flag-no-lifting?)
    iform
    (let1 dic (make-label-dic '())
      (pass4/scan iform '() '() #t dic) ; Mark free variables
      (let1 lambda-nodes (label-dic-info dic)
        (if (or (null? lambda-nodes)
                (and (null? (cdr lambda-nodes)) ; iform has only a toplevel lambda
                     ($lambda-lifted-var (car lambda-nodes))))
          iform                           ;shortcut
          (let1 lifted (pass4/lift lambda-nodes module)
            (if (null? lifted)
              iform                       ;shortcut
              (let1 iform. (pass4/subst iform (make-label-dic '()))
                ($seq `(,@(map pass4/lifted-define lifted) ,iform.))))))))))

(define (pass4/lifted-define lambda-node)
  ($define ($lambda-src lambda-node) '(const)
           ($lambda-lifted-var lambda-node)
           lambda-node))

;; Pass4 step1 - scan
;;   bs - List of lvars whose binding is introduced in the current scope.
;;   fs - List of free lvars found so far in the current scope.
;;   t? - #t if we're in the top level, #f otherwise.
;;   labels - label-dic.  the info field is used to hold $LAMBDA nodes.
;; Eacl call returns a list of free lvars.

(define-macro (pass4/scan* iforms bs fs t? labels)
  (let1 iforms. (gensym)
    `(let1 ,iforms. ,iforms
       (cond [(null? ,iforms.) ,fs]
             [(null? (cdr ,iforms.))
              (pass4/scan (car ,iforms.) ,bs ,fs ,t? ,labels)]
             [else
              (let loop ([,iforms. ,iforms.] [,fs ,fs])
                (if (null? ,iforms.)
                  ,fs
                  (loop (cdr ,iforms.)
                        (pass4/scan (car ,iforms.) ,bs ,fs ,t? ,labels))))]))))

(define/case (pass4/scan iform bs fs t? labels)
  (iform-tag iform)
  [($DEFINE) (unless t? (error "[internal] pass4 $DEFINE in non-toplevel"))
             (pass4/scan ($define-expr iform) bs fs #t labels)]
  [($LREF)   (pass4/add-lvar ($lref-lvar iform) bs fs)]
  [($LSET)   (let1 fs (pass4/scan ($lset-expr iform) bs fs t? labels)
               (pass4/add-lvar ($lset-lvar iform) bs fs))]
  [($GSET)   (pass4/scan ($gset-expr iform) bs fs t? labels)]
  [($IF)     (let* ([fs (pass4/scan ($if-test iform) bs fs t? labels)]
                    [fs (pass4/scan ($if-then iform) bs fs t? labels)])
               (pass4/scan ($if-else iform) bs fs t? labels))]
  [($LET)    (let* ([new-bs (append ($let-lvars iform) bs)]
                    [bs (if (memv ($let-type iform) '(rec rec*)) new-bs bs)]
                    [fs (pass4/scan* ($let-inits iform) bs fs t? labels)])
               (pass4/scan ($let-body iform) new-bs fs #f labels))]
  [($RECEIVE)(let ([fs (pass4/scan ($receive-expr iform) bs fs t? labels)]
                   [bs (append ($receive-lvars iform) bs)])
               (pass4/scan ($receive-body iform) bs fs #f labels))]
  [($LAMBDA) (let1 inner-fs (pass4/scan ($lambda-body iform)
                                        ($lambda-lvars iform) '() #f labels)
               ;; If this $LAMBDA is outermost in the original expression,
               ;; we don't need to lift it, nor need to set free-lvars.
               ;; We just mark it by setting lifted-var to #t so that
               ;; pass4/lift phase can treat it specially.
               (unless (eq? ($lambda-flag iform) 'dissolved)
                 (label-dic-info-push! labels iform) ;save the lambda node
                 (when t?                            ;mark this is toplevel
                   ($lambda-lifted-var-set! iform #t)))
               (cond [t? '()]
                     [else ($lambda-free-lvars-set! iform inner-fs)
                           (let loop ([inner-fs inner-fs] [fs fs])
                             (if (null? inner-fs)
                               fs
                               (loop (cdr inner-fs)
                                     (pass4/add-lvar (car inner-fs) bs fs))))]))]
  [($LABEL)  (cond [(label-seen? labels iform) fs]
                   [else (label-push! labels iform)
                         (pass4/scan ($label-body iform) bs fs #f labels)])]
  [($SEQ)    (pass4/scan* ($seq-body iform) bs fs t? labels)]
  [($CALL)   (let1 fs (if (eq? ($call-flag iform) 'jump)
                        fs
                        (pass4/scan ($call-proc iform) bs fs t? labels))
               (pass4/scan* ($call-args iform) bs fs t? labels))]
  [($ASM)    (pass4/scan* ($asm-args iform) bs fs t? labels)]
  [($PROMISE)(pass4/scan ($promise-expr iform) bs fs t? labels)]
  [($CONS $APPEND $MEMV $EQ? $EQV?) (pass4/scan2 iform bs fs t? labels)]
  [($VECTOR $LIST $LIST*) (pass4/scan* ($*-args iform) bs fs t? labels)]
  [($LIST->VECTOR) (pass4/scan ($*-arg0 iform) bs fs t? labels)]
  [else fs])

(define (pass4/scan2 iform bs fs t? labels)
  (let1 fs (pass4/scan ($*-arg0 iform) bs fs t? labels)
    (pass4/scan ($*-arg1 iform) bs fs t? labels)))

;; Sort out the liftable lambda nodes.
;; Returns a list of lambda nodes, in each of which $lambda-lifted-var
;; contains an identifier.
;;
;; At this moment, we only detect closures without free variables,
;; or self-recursive closures.
;;
;; Eventually we want to detect mutual recursive case like this:
;;
;;  (define (foo ...)
;;    (define (a x)  ... (b ...) ...)
;;    (define (b y)  ... (a ...) ...)
;;    ...)
;;
;; If a's only free variable is b, and b's only free variable is a,
;; then we can lift both nodes to the toplevel.
;;
;; Tentative algorithm:
;;  - Create a directed graph consists of free lvars and lambda nodes.
;;    An edge from an lvar to a lambda node if the lvar is free in
;;    the lambda node.  An edge from lambda node to an lvar if the lambda
;;    node is bound to the lvar.
;;  - Find lvars without incoming edge, and remove them and all reachable
;;    nodes from them.
;;  - The lambda nodes in the remaining graph are the ones we can lift.
;;

(define (pass4/lift lambda-nodes module)
  (let1 top-name #f
    ;; Find a toplevel $lambda node (marked by #t in lifted-var).
    ;; Its name can be used to generate names for lifted lambdas.
    (let loop ([lms lambda-nodes])
      (when (pair? lms)
        (or (and-let* ([ ($lambda-lifted-var (car lms)) ]
                       [n ($lambda-name (car lms))])
              (set! top-name (if (identifier? n)
                               (identifier-name n)
                               n)))
            (loop (cdr lms)))))
    (rlet1 results '()
      (let loop ([lms lambda-nodes])
        (cond [(null? lms)]
              [($lambda-lifted-var (car lms))
               ($lambda-lifted-var-set! (car lms) #f)
               (loop (cdr lms))]
              [else
               (let* ([lm (car lms)]
                      [fvs ($lambda-free-lvars lm)])
                 (when (or (null? fvs)
                           (and (null? (cdr fvs))
                                (lvar-immutable? (car fvs))
                                (eq? (lvar-initval (car fvs)) lm)))
                   (let1 gvar (make-identifier (gensym) module '())
                     ($lambda-name-set! lm (list top-name
                                                 (or ($lambda-name lm)
                                                     (identifier-name gvar))))
                     ($lambda-lifted-var-set! lm gvar)
                     (push! results lm)))
                 (loop (cdr lms)))])))))

;; Final touch of pass4 - replace lifted lambda nodes to $GREFs.
;; Returns (possibly modified) IForm.
(define-macro (pass4/subst! access-form labels)
  (match-let1 (accessor expr) access-form
    (let ([orig (gensym)]
          [result (gensym)]
          [setter (if (eq? accessor 'car)
                    'set-car!
                    (string->symbol #"~|accessor|-set!"))])
      `(let* ([,orig (,accessor ,expr)]
              [,result (pass4/subst ,orig ,labels)])
         (unless (eq? ,orig ,result)
           (,setter ,expr ,result))
         ,expr))))

(define-macro (pass4/subst*! iforms labels)
  (let1 iforms. (gensym)
    `(let1 ,iforms. ,iforms
       (cond [(null? ,iforms.)]
             [(null? (cdr ,iforms.)) (pass4/subst! (car ,iforms.) ,labels)]
             [else
              (let loop ([,iforms. ,iforms.])
                (unless (null? ,iforms.)
                  (pass4/subst! (car ,iforms.) ,labels)
                  (loop (cdr ,iforms.))))]))))

(define/case (pass4/subst iform labels)
  (iform-tag iform)
  [($DEFINE) (pass4/subst! ($define-expr iform) labels)]
  [($LREF)   (or (and-let* ([ (lvar-immutable? ($lref-lvar iform)) ]
                            [init (lvar-initval ($lref-lvar iform))]
                            [ (vector? init) ]
                            [ (has-tag? init $LAMBDA) ]
                            [id ($lambda-lifted-var init)])
                   (lvar-ref--! ($lref-lvar iform))
                   ;; Modifies $LREF to $GREF
                   (vector-set! iform 0 $GREF)
                   ($gref-id-set! iform id)
                   iform)
                 iform)]
  [($LSET)   (pass4/subst! ($lset-expr iform) labels)]
  [($GSET)   (pass4/subst! ($gset-expr iform) labels)]
  [($IF)     (pass4/subst! ($if-test iform) labels)
             (pass4/subst! ($if-then iform) labels)
             (pass4/subst! ($if-else iform) labels)]
  [($LET)    (pass4/subst*! ($let-inits iform) labels)
             (pass4/subst! ($let-body iform) labels)]
  [($RECEIVE)(pass4/subst! ($receive-expr iform) labels)
             (pass4/subst! ($receive-body iform) labels)]
  [($LAMBDA) (pass4/subst! ($lambda-body iform) labels)
             (or (and-let* ([id ($lambda-lifted-var iform)])
                   ($gref id))
                 iform)]
  [($LABEL)  (unless (label-seen? labels iform)
               (label-push! labels iform)
               (pass4/subst! ($label-body iform) labels))
             iform]
  [($SEQ)    (pass4/subst*! ($seq-body iform) labels) iform]
  [($CALL)   (pass4/subst*! ($call-args iform) labels)
             (pass4/subst! ($call-proc iform) labels)]
  [($ASM)    (pass4/subst*! ($asm-args iform) labels) iform]
  [($PROMISE)(pass4/subst! ($promise-expr iform) labels)]
  [($CONS $APPEND $MEMV $EQ? $EQV?) (pass4/subst! ($*-arg0 iform) labels)
             (pass4/subst! ($*-arg1 iform) labels)]
  [($VECTOR $LIST $LIST*) (pass4/subst*! ($*-args iform) labels) iform]
  [($LIST->VECTOR) (pass4/subst! ($*-arg0 iform) labels)]
  [else iform])

;;===============================================================
;; Pass 5.  Code generation
;;

;; This pass passes down a runtime environment, renv.  It is
;; a nested list of lvars, and used to generate LREF/LSET instructions.
;;
;; The context, ctx, is either one of the following symbols.
;;
;;   normal/bottom : the FORM is evaluated in the context that the
;;            stack has no pending arguments (i.e. a continuation
;;            frame is just pushed).
;;   normal/top : the FORM is evaluated, while there are pending
;;            arguments in the stack top.  Such premature argument frame
;;            should be protected if VM calls something that may
;;            capture the continuation.
;;   stmt/bottom : Like normal/bottom, but the result of FORM won't
;;            be used.
;;   stmt/top : Like normal/top, but the result of FORM won't be used.
;;   tail   : FORM is evaluated in the tail context.  It is always
;;            bottom.
;;
;; Each IForm node handler generates the code by side-effects.  Besides
;; the code generation, each handler returns the maximum stack depth.
;;
;; The return value of pass5 entry is a <compiled-code>.
;; The pass5 main entry may be called recursively from $LAMBDA node;
;; the CLOSURE instruction takes <compiled-code> of the body of the lambda
;; as an operand.

;; predicates
(define-inline (normal-context? ctx)
  (or (eq? ctx 'normal/bottom) (eq? ctx 'normal/top)))
(define-inline (stmt-context? ctx)
  (or (eq? ctx 'stmt/bottom) (eq? ctx 'stmt/top)))
(define-inline (tail-context? ctx)
  (eq? ctx 'tail))
(define-inline (bottom-context? ctx)
  (or (eq? ctx 'normal/bottom) (eq? ctx 'stmt/bottom) (eq? ctx 'tail)))
(define-inline (top-context? ctx)
  (or (eq? ctx 'normal/top) (eq? ctx 'stmt/top)))

;; context switch
(define-inline (normal-context prev-ctx)
  (if (bottom-context? prev-ctx) 'normal/bottom 'normal/top))
(define-inline (stmt-context prev-ctx)
  (if (bottom-context? prev-ctx) 'stmt/bottom 'stmt/top))
(define-inline (tail-context prev-ctx) 'tail)

;; Dispatch pass5 handler.
;; *pass5-dispatch-table* is defined below, after all handlers are defined.
(define-inline (pass5/rec iform ccb renv ctx)
  ((vector-ref *pass5-dispatch-table* (vector-ref iform 0))
   iform ccb renv ctx))

;;
;; Pass 5 main entry.  Returns <compiled-code>
;;
(define (pass5 iform ccb initial-renv ctx)
  (let1 maxstack (pass5/rec iform ccb initial-renv ctx)
    (compiled-code-emit-RET! ccb)
    (compiled-code-finish-builder ccb maxstack)
    ccb))

;;
;; Pass 5 intermediate tree handlers
;;

(define (pass5/$DEFINE iform ccb renv ctx)
  (let ([d (pass5/rec ($define-expr iform) ccb renv 'normal/bottom)]
        [f (cond [(memq 'const ($define-flags iform)) SCM_BINDING_CONST]
                 [(memq 'inlinable ($define-flags iform)) SCM_BINDING_INLINABLE]
                 [else 0])])
    (compiled-code-emit1oi! ccb DEFINE f ($define-id iform) ($*-src iform))
    d))

(define (pass5/$LREF iform ccb renv ctx)
  (receive (depth offset) (renv-lookup renv ($lref-lvar iform))
    (compiled-code-emit2i! ccb LREF depth offset
                           (lvar-name ($lref-lvar iform)))
    (unless (lvar-immutable? ($lref-lvar iform))
      (compiled-code-emit0! ccb UNBOX))
    0))

(define (pass5/$LSET iform ccb renv ctx)
  (receive (depth offset) (renv-lookup renv ($lset-lvar iform))
    (rlet1 d (pass5/rec ($lset-expr iform) ccb renv (normal-context ctx))
      (compiled-code-emit2i! ccb LSET depth offset
                             (lvar-name ($lset-lvar iform))))))

(define (pass5/$GREF iform ccb renv ctx)
  (let1 id ($gref-id iform)
    (compiled-code-emit0oi! ccb GREF id id)
    0))

(define (pass5/$GSET iform ccb renv ctx)
  (let ([d (pass5/rec ($gset-expr iform) ccb renv (normal-context ctx))]
        [id ($gset-id iform)])
    (compiled-code-emit0oi! ccb GSET id id)
    d))

(define (pass5/$CONST iform ccb renv ctx)
  ;; if the context is stmt-context, value won't be used so we drop it.
  (unless (stmt-context? ctx)
    (compiled-code-emit0o! ccb CONST ($const-value iform)))
  0)

;; Branch peephole optimization
;;   We have variations of conditional branch instructions for typical
;;   cases.  In this handler we select an appropriate instructions.
;;
;;   Sometimes we want to inverse the test, swapping then and else,
;;   if we can strip extra NOT operation.  Note that it is only possible
;;   if the result of test isn't used directly (that is, neither then nor
;;   else clause is ($IT)), thus we treat such a case specially.
(define (pass5/$IF iform ccb renv ctx)
  (cond
   [(and (not ($it? ($if-then iform)))
         (not ($it? ($if-else iform)))
         (has-tag? ($if-test iform) $ASM)
         (eqv? (car ($asm-insn ($if-test iform))) NOT))
    (pass5/$IF ($if ($*-src iform)
                    (car ($asm-args ($if-test iform)))
                    ($if-else iform)
                    ($if-then iform))
               ccb renv ctx)]
   [else
    (pass5/branch-core iform ccb renv ctx)]))

(define (pass5/branch-core iform ccb renv ctx)
  (let1 test ($if-test iform)
    ;; Select an appropriate branch instruction
    (cond
     [(has-tag? test $ASM)
      (let ([code (car ($asm-insn test))]; ASM code
            [args ($asm-args test)])
        (cond
         [(eqv? code NULLP)
          (pass5/if-final iform (car args) BNNULL 0 0
                          ($*-src test) ccb renv ctx)]
         [(eqv? code EQ)
          (pass5/if-eq iform (car args) (cadr args)
                       ($*-src test) ccb renv ctx)]
         [(eqv? code EQV)
          (pass5/if-eqv iform (car args) (cadr args)
                        ($*-src test) ccb renv ctx)]
         [(eqv? code NUMEQ2)
          (pass5/if-numeq iform (car args) (cadr args)
                          ($*-src test) ccb renv ctx)]
         [(eqv? code NUMLE2)
          (pass5/if-numcmp iform (car args) (cadr args)
                           BNLE ($*-src test) ccb renv ctx)]
         [(eqv? code NUMLT2)
          (pass5/if-numcmp iform (car args) (cadr args)
                           BNLT ($*-src test) ccb renv ctx)]
         [(eqv? code NUMGE2)
          (pass5/if-numcmp iform (car args) (cadr args)
                           BNGE ($*-src test) ccb renv ctx)]
         [(eqv? code NUMGT2)
          (pass5/if-numcmp iform (car args) (cadr args)
                           BNGT ($*-src test) ccb renv ctx)]
         [else
          (pass5/if-final iform test BF 0 0 ($*-src iform) ccb renv ctx)]
         ))]
     [(has-tag? test $EQ?)
      (pass5/if-eq iform ($*-arg0 test) ($*-arg1 test)
                   ($*-src iform) ccb renv ctx)]
     [(has-tag? test $EQV?)
      (pass5/if-eqv iform ($*-arg0 test) ($*-arg1 test)
                    ($*-src iform) ccb renv ctx)]
     [($const? test)   ; this may occur as a result of macro expansion
      (pass5/rec (if ($const-value test)
                   (if ($it? ($if-then iform)) test ($if-then iform))
                   (if ($it? ($if-else iform)) test ($if-else iform)))
                 ccb renv ctx)]
     [else
      (pass5/if-final iform test BF 0 0 ($*-src iform) ccb renv ctx)]
     )))

;;
(define (pass5/if-eq iform x y info ccb renv ctx)
  (cond
   [($const? x) (pass5/if-final iform y BNEQC ($const-value x)
                                0 info ccb renv ctx)]
   [($const? y) (pass5/if-final iform x BNEQC ($const-value y)
                                0 info ccb renv ctx)]
   [else
    (let1 depth (imax (pass5/rec x ccb renv (normal-context ctx)) 1)
      (compiled-code-emit-PUSH! ccb)
      (pass5/if-final iform #f BNEQ 0
                      (imax (pass5/rec y ccb renv 'normal/top) depth)
                      info ccb renv ctx))]))

(define (pass5/if-eqv iform x y info ccb renv ctx)
  (cond
   [($const? x) (pass5/if-final iform y BNEQVC ($const-value x)
                                0 info ccb renv ctx)]
   [($const? y) (pass5/if-final iform x BNEQVC ($const-value y)
                                0 info ccb renv ctx)]
   [else
    (let1 depth (imax (pass5/rec x ccb renv (normal-context ctx)) 1)
      (compiled-code-emit-PUSH! ccb)
      (pass5/if-final iform #f BNEQV 0
                      (imax (pass5/rec y ccb renv 'normal/top) depth)
                      info ccb renv ctx))]))

(define (pass5/if-numeq iform x y info ccb renv ctx)
  (or (and ($const? x)
           (integer-fits-insn-arg? ($const-value x))
           (pass5/if-final iform y BNUMNEI ($const-value x)
                           0
                           info ccb renv ctx))
      (and ($const? y)
           (integer-fits-insn-arg? ($const-value y))
           (pass5/if-final iform x BNUMNEI ($const-value y)
                           0
                           info ccb renv ctx))
      (and ($lref? x)
           (lvar-immutable? ($lref-lvar x))
           (pass5/if-final iform #f LREF-VAL0-BNUMNE
                           (pass5/if-numcmp-lrefarg x renv)
                           (pass5/rec y ccb renv (normal-context ctx))
                           info ccb renv ctx))
      (and ($lref? y)
           (lvar-immutable? ($lref-lvar y))
           (pass5/if-final iform #f LREF-VAL0-BNUMNE
                           (pass5/if-numcmp-lrefarg y renv)
                           (pass5/rec x ccb renv (normal-context ctx))
                           info ccb renv ctx))
      (let1 depth (imax (pass5/rec x ccb renv (normal-context ctx)) 1)
        (compiled-code-emit-PUSH! ccb)
        (pass5/if-final iform #f BNUMNE 0
                        (imax (pass5/rec y ccb renv 'normal/top) depth)
                        info ccb renv ctx))))

(define (pass5/if-numcmp iform x y insn info ccb renv ctx)
  (define .fwd. `((,BNLT . ,LREF-VAL0-BNLT) (,BNLE . ,LREF-VAL0-BNLE)
                  (,BNGT . ,LREF-VAL0-BNGT) (,BNGE . ,LREF-VAL0-BNGE)))
  (define .rev. `((,BNLT . ,LREF-VAL0-BNGT) (,BNLE . ,LREF-VAL0-BNGE)
                  (,BNGT . ,LREF-VAL0-BNLT) (,BNGE . ,LREF-VAL0-BNLE)))
  (or (and ($lref? x)
           (lvar-immutable? ($lref-lvar x))
           (pass5/if-final iform #f (cdr (assv insn .fwd.))
                           (pass5/if-numcmp-lrefarg x renv)
                           (pass5/rec y ccb renv (normal-context ctx))
                           info ccb renv ctx))
      (and ($lref? y)
           (lvar-immutable? ($lref-lvar y))
           (pass5/if-final iform #f (cdr (assv insn .rev.))
                           (pass5/if-numcmp-lrefarg y renv)
                           (pass5/rec x ccb renv (normal-context ctx))
                           info ccb renv ctx))
      (let1 depth (imax (pass5/rec x ccb renv (normal-context ctx)) 1)
        (compiled-code-emit-PUSH! ccb)
        (pass5/if-final iform #f insn 0
                        (imax (pass5/rec y ccb renv 'normal/top) depth)
                        info ccb renv ctx))))

;; helper fn
(define (pass5/if-numcmp-lrefarg lref renv)
  (receive (dep off) (renv-lookup renv ($lref-lvar lref))
    (+ (ash off 10) dep)))


;; pass5/if-final: Final stage of emitting branch instruction.
;;
;; Optimization
;;   - tail context
;;      - if insn is (BF)
;;        - then part is ($IT)  => use RT
;;        - else part is ($IT)  => use RF
;;        - then part is jump back to labeled closure
;;          => use BT, and use the destination of the label. (*)
;;      - otherwise, place RET after then clause
;;   - otherwise
;;      - else part is ($IT)  => we can omit a jump after then clause
;;      - otherwise, merge the control after this node.
;;
;;  (*) This converts the left asm output to the right one.  This type of
;;      code appears as the result of the first pattern of $if optimization
;;      in pass3.
;;
;;        L0:                   L0:
;;            ...                   ...
;;            BF L1                 BT L0
;;            JUMP L0               xxx
;;        L1: xxx                   ...
;;            ...
;;
;; We have many variations of branch instrucitons, and the combination
;; of arguments reflects them.
;;
;;   iform - original IForm of this if expression.
;;   test - the iform for the test expression.  the result of this expression
;;          would trigger the conditional branch.  This can be #f when we use
;;          operate-and-branch instructions such as BNLT.
;;   code - an instruciton code.
;;   arg0/opr - If the instruction is one of those that take an extra operand
;;          (like BNEQC), this is the operand.  Otherwise, this is the ARG0
;;          of the instruction.
;;   depth - calculated maximum stack depth at this point.
;;   info  - source info
;;   ccb   - code buffer
;;   renv  - runtime env
;;   ctx   - compile context

(define-constant .branch-insn-extra-operand.
  `(,BNEQC ,BNEQVC))

(define (pass5/if-final iform test code arg0/opr depth info ccb renv ctx)
  (let ([depth (if test
                 (imax (pass5/rec test ccb renv (normal-context ctx)) depth)
                 depth)]
        [then-form ($if-then iform)]
        [else-form ($if-else iform)])
    (cond
     [(tail-context? ctx)
      (or (and (eqv? code BF)
               (cond
                [($it? then-form)
                 (compiled-code-emit0i! ccb RT info)
                 (imax (pass5/rec else-form ccb renv ctx) depth)]
                [($it? else-form)
                 (compiled-code-emit0i! ccb RF info)
                 (imax (pass5/rec then-form ccb renv ctx) depth)]
                [(and (has-tag? then-form $LABEL)
                      ($label-label then-form))
                 => (^[label]
                      (compiled-code-emit0o! ccb BT label)
                      (imax (pass5/rec else-form ccb renv ctx) depth))]
                [else #f]))
          (let1 elselabel (compiled-code-new-label ccb)
            (if (memv code .branch-insn-extra-operand.)
              (compiled-code-emit0oi! ccb code (list arg0/opr elselabel) info)
              (compiled-code-emit1oi! ccb code arg0/opr elselabel info))
            (set! depth (imax (pass5/rec then-form ccb renv ctx) depth))
            (compiled-code-emit-RET! ccb)
            (compiled-code-set-label! ccb elselabel)
            (imax (pass5/rec else-form ccb renv ctx) depth)))]
     [else
      (let ([elselabel  (compiled-code-new-label ccb)]
            [mergelabel (compiled-code-new-label ccb)])
        (if (memv code .branch-insn-extra-operand.)
          (compiled-code-emit0oi! ccb code (list arg0/opr elselabel) info)
          (compiled-code-emit1oi! ccb code arg0/opr elselabel info))
        (set! depth (imax (pass5/rec then-form ccb renv ctx) depth))
        (unless ($it? else-form)
          (compiled-code-emit0o! ccb JUMP mergelabel))
        (compiled-code-set-label! ccb elselabel)
        (unless ($it? else-form)
          (set! depth (imax (pass5/rec else-form ccb renv ctx) depth)))
        (compiled-code-set-label! ccb mergelabel)
        depth)])))

(define (pass5/$IT iform ccb renv ctx) 0)

;; $LET stack estimate
;;   normal let: Each init clause is evaluated while preceding results
;;     of inits are on the stack.  Pass5/prepare-args returns the maximum
;;     stack depth from the initial position of the stack (i.e. it considers
;;     accumulating values).  After all inits are evaluated, we complete
;;     the env frame and run the body.
;;
;;   letrec: We create the env frame before evaluating inits, so the max
;;     stack is: total env frame size + max of stack depth consumed by
;;     one of inits or the body.
;;
(define (pass5/$LET iform ccb renv ctx)
  (when (and (has-tag? ($let-body iform) $LET)
             (eq? ($let-type iform) 'let))
    (pass5/flatten-let*! iform))
  (let ([info ($*-src iform)]
        [lvars ($let-lvars iform)]
        [inits ($let-inits iform)]
        [body  ($let-body iform)]
        [merge-label (if (bottom-context? ctx)
                       #f
                       (compiled-code-new-label ccb))])
    (let1 nlocals (length lvars)
      (case ($let-type iform)
        [(let)
         (cond
          [(bottom-context? ctx)
           (let1 dinit (pass5/prepare-args inits ccb renv ctx)
             (compiled-code-emit1i! ccb LOCAL-ENV nlocals info)
             (pass5/box-mutable-lvars lvars ccb)
             (let1 dbody (pass5/rec body ccb (cons lvars renv) ctx)
               (unless (tail-context? ctx)
                 (compiled-code-emit0! ccb POP-LOCAL-ENV))
               (imax dinit (+ dbody ENV_HEADER_SIZE nlocals))))]
          [else
           (compiled-code-emit1oi! ccb PRE-CALL nlocals merge-label info)
           (let1 dinit (pass5/prepare-args inits ccb renv ctx)
             (compiled-code-emit1i! ccb LOCAL-ENV nlocals info)
             (pass5/box-mutable-lvars lvars ccb)
             (let1 dbody (pass5/rec body ccb (cons lvars renv) 'tail)
               (compiled-code-emit-RET! ccb)
               (compiled-code-set-label! ccb merge-label)
               (imax dinit
                     (+ dbody CONT_FRAME_SIZE ENV_HEADER_SIZE nlocals))))])]
        [(rec rec*)
         (receive (closures others)
             (partition-letrec-inits inits ccb (cons lvars renv) 0 '() '())
           (cond
            [(bottom-context? ctx)
             (compiled-code-emit1oi! ccb LOCAL-ENV-CLOSURES nlocals
                                     closures info)
             (emit-letrec-boxers ccb lvars nlocals)
             (let* ([dinit (emit-letrec-inits others lvars nlocals ccb
                                              (cons lvars renv) 0)]
                    [dbody (pass5/rec body ccb (cons lvars renv) ctx)])
               (unless (tail-context? ctx)
                 (compiled-code-emit0! ccb POP-LOCAL-ENV))
               (+ ENV_HEADER_SIZE nlocals (imax dinit dbody)))]
            [else
             (compiled-code-emit1oi! ccb PRE-CALL nlocals merge-label info)
             (compiled-code-emit1oi! ccb LOCAL-ENV-CLOSURES nlocals
                                     closures info)
             (emit-letrec-boxers ccb lvars nlocals)
             (let* ([dinit (emit-letrec-inits others lvars nlocals ccb
                                              (cons lvars renv) 0)]
                    [dbody (pass5/rec body ccb (cons lvars renv) 'tail)])
               (compiled-code-emit-RET! ccb)
               (compiled-code-set-label! ccb merge-label)
               (+ CONT_FRAME_SIZE ENV_HEADER_SIZE nlocals
                  (imax dinit dbody)))]))]
        [else
         (error "[internal error]: pass5/$LET got unknown let type:"
                ($let-type iform))]
        ))))

;; Nested let can be flattened if the initexpr of inner local vars
;; does not depend on the values of the local vars of the current frame,
;; and they don't possibly capture environments/continuations.
;; $const and $lref to outer frames are certainly safe.
;; $gref also won't capture frames, but we have to be careful if we ever
;; provide a mechanism to restart from undefined variable error.
(define (pass5/flatten-let*! iform)
  (let loop ([lvars (reverse ($let-lvars iform))]
             [inits (reverse ($let-inits iform))]
             [node ($let-body iform)])
    (let1 ins ($let-inits node)
      (cond [(everyc safe-lvar-initval-for-flatten? ins lvars)
             (let ([lvars (reverse ($let-lvars node) lvars)]
                   [inits (reverse ins inits)])
               (if (and (has-tag? ($let-body node) $LET)
                        (eq? ($let-type node) 'let))
                 (loop lvars inits ($let-body node))
                 (begin ($let-lvars-set! iform (reverse! lvars))
                        ($let-inits-set! iform (reverse! inits))
                        ($let-body-set! iform ($let-body node)))))]
            [(eq? node ($let-body iform))] ; we didn't do anything
            [else ($let-lvars-set! iform (reverse! lvars))
                  ($let-inits-set! iform (reverse! inits))
                  ($let-body-set! iform node)]))))

(define (safe-lvar-initval-for-flatten? init existing-lvars)
  (and (or ($const? init)
           (and ($lref? init)
                (not (memq ($lref-lvar init) existing-lvars))))))

;; classify init values into closures/constants and non-closure expressions.
;; returns two lists: a list of init values (closures or #under values)
;; corresponding to lvar list, and non-closure init iforms, each
;; paired with an lvar count.
(define (partition-letrec-inits inits ccb renv cnt closures others)
  (if (null? inits)
    (values (reverse closures) (reverse others))
    (let1 init (car inits)
      (cond
       [(has-tag? init $LAMBDA)
        (partition-letrec-inits (cdr inits) ccb renv (+ cnt 1)
                                (cons (pass5/lambda init ccb renv) closures)
                                others)]
       [($const? init)
        (partition-letrec-inits (cdr inits) ccb renv (+ cnt 1)
                                (cons ($const-value init) closures)
                                others)]
       [else
        (partition-letrec-inits (cdr inits) ccb renv (+ cnt 1)
                                (cons (undefined) closures)
                                (acons cnt init others))]))))

;; box set!-able slots in the ENV at stack top.  used in letrec and
;; receive frame setup.
(define (emit-letrec-boxers ccb lvars nlocals)
  (let loop ([lvars lvars] [cnt nlocals])
    (unless (null? lvars)
      (unless (lvar-immutable? (car lvars))
        (compiled-code-emit1! ccb BOX cnt))
      (loop (cdr lvars) (- cnt 1)))))
       
;; emit LSET or ENV-SET insn to initialize lvars that aren't closures or
;; constants.  init-alist is the second value partition-letrec-inits
;; returned, which is a list of (<lvar-count> <init-iform>).
(define (emit-letrec-inits init-alist lvars nlocals ccb renv depth)
  (if (null? init-alist)
    depth
    (let* ([off&expr (car init-alist)]
           [d (pass5/rec (cdr off&expr) ccb renv 'normal/bottom)]
           [lvar (list-ref lvars (car off&expr))])
      (if (lvar-immutable? lvar)
        (compiled-code-emit1! ccb ENV-SET (- nlocals 1 (car off&expr)))
        (compiled-code-emit2! ccb LSET 0 (- nlocals 1 (car off&expr))))
      (emit-letrec-inits (cdr init-alist) lvars nlocals ccb renv
                         (imax depth d)))))

(define (pass5/$RECEIVE iform ccb renv ctx)
  (let ([nargs  ($receive-reqargs iform)]
        [optarg ($receive-optarg iform)]
        [lvars  ($receive-lvars iform)]
        [expr   ($receive-expr iform)]
        [body   ($receive-body iform)])
    (cond
     [(bottom-context? ctx)
      (let1 dinit (pass5/rec expr ccb renv (normal-context ctx))
        (compiled-code-emit2i! ccb TAIL-RECEIVE nargs optarg ($*-src iform))
        (emit-letrec-boxers ccb lvars (length lvars))
        (let1 dbody (pass5/rec body ccb (cons lvars renv) ctx)
          (unless (tail-context? ctx)
            (compiled-code-emit0! ccb POP-LOCAL-ENV))
          (imax dinit (+ nargs optarg ENV_HEADER_SIZE dbody))))]
     [else
      (let ([merge-label (compiled-code-new-label ccb)]
            [dinit (pass5/rec expr ccb renv (normal-context ctx))])
        (compiled-code-emit2oi! ccb RECEIVE nargs optarg
                                merge-label ($*-src iform))
        (emit-letrec-boxers ccb lvars (length lvars))
        (let1 dbody (pass5/rec body ccb (cons lvars renv) 'tail)
          (compiled-code-emit-RET! ccb)
          (compiled-code-set-label! ccb merge-label)
          (imax dinit (+ nargs optarg CONT_FRAME_SIZE ENV_HEADER_SIZE dbody))))]
     )))

(define (pass5/$LAMBDA iform ccb renv ctx)
  (let ([code (pass5/lambda iform ccb renv)]
        [info ($*-src iform)])
    (compiled-code-emit0oi! ccb CLOSURE code info))
  0)

(define (pass5/lambda iform ccb renv)
  (let* ([inliner (let1 v ($lambda-flag iform)
                   (and (vector? v) v))]
         [ccb (make-compiled-code-builder ($lambda-reqargs iform)
                                          ($lambda-optarg iform)
                                          ($lambda-name iform)
                                          ;; TODO: For the time being, if we
                                          ;; don't have arg-info, we show
                                          ;; (proc . _).  We can do better, tho.
                                          (or (and-let* ([p ($lambda-src iform)])
                                                (pair-attribute-get p 'arg-info #f))
                                              '_)
                                          ccb  ; parent
                                          inliner)])
    (and-let* ([src ($lambda-src iform)])
      (compiled-code-push-info! ccb `(definition (source-info . ,src))))
    ;; If any of procedure parameters are set!, we should box it
    ;; upon entering the procedure.
    (let loop ([lvs ($lambda-lvars iform)]
               [k (length ($lambda-lvars iform))])
      (unless (null? lvs)
        (unless (lvar-immutable? (car lvs))
          (compiled-code-emit1i! ccb BOX k (lvar-name (car lvs))))
        (loop (cdr lvs) (- k 1))))
    (pass5 ($lambda-body iform)
           ccb
           (if (null? ($lambda-lvars iform))
             renv
             (cons ($lambda-lvars iform) renv))
           'tail)))

(define (pass5/$LABEL iform ccb renv ctx)
  (let1 label ($label-label iform)
    ;; NB: $LABEL node in the PROC position of $CALL node is handled by $CALL.
    (cond
     [label (compiled-code-emit0oi! ccb JUMP label ($*-src iform))
            0]
     [else  (compiled-code-set-label! ccb (pass5/ensure-label ccb iform))
            (pass5/rec ($label-body iform) ccb renv ctx)])))

(define (pass5/$SEQ iform ccb renv ctx)
  (let1 exprs ($seq-body iform)
    (cond
     [(null? exprs) 0]
     [(null? (cdr exprs)) (pass5/rec (car exprs) ccb renv ctx)]
     [else
      (let loop ([exprs exprs] [depth 0])
        (if (null? (cdr exprs))
          (imax (pass5/rec (car exprs) ccb renv ctx) depth)
          (loop (cdr exprs)
                (imax (pass5/rec (car exprs) ccb renv (stmt-context ctx))
                      depth))))])))

;; $CALL.
;;  There are several variations in $CALL node.  Each variation may also
;;  have tail-call version and non-tail-call version.
;;
;;  1. Local call: a $CALL node that has 'local' flag is a call to known
;;     local procedure.  Its arguments are already adjusted to match the
;;     signature of the procedure.   PROC slot contains an LREF node that
;;     points to the local procedure.
;;
;;  2. Embedded call: a $CALL node that has 'embed' flag is a control
;;     transfer to an inlined local procedure, whose entry point may be
;;     called from more than one place (Cf. an inlined procedure that is
;;     called only once becomes $LET node, so we don't need to consider it).
;;     Its arguments are already adjusted to match the signature of the
;;     procedure.  Its PROC slot contains the embedded $LAMBDA node, whose
;;     body is $LABEL node.
;;     The generated code is almost the same as $LET node, except that a
;;     label is placed just after LOCAL-ENV.
;;
;;     We also record the RENV in this node, which is later used by
;;     jump call node to determine the number of environment frames the
;;     LOCAL-ENV-JUMP should discard.  (Here we assume an embed node always
;;     goes through pass5 before related jump nodes.)
;;
;;  3. Jump call: a $CALL node that has 'jump' flag is a control transfer
;;     to an inlined local procedure, and whose body is embedded in somewhere
;;     else (by an 'embedded call' node).   The PROC slot contains the embed
;;     $CALL node.  We emit LOCAL-ENV-JUMP instruction for this type of node.
;;
;;  4. Head-heavy call: a $CALL node without any flag, and all the
;;     arguments are simple expressions (e.g. const or lref), but the
;;     operator expression has $LET.  The normal calling sequence evaluates
;;     the operator expression after pushing arguments.  That causes the
;;     $LET be evaluated in 'top' context, which requires pushing
;;     extra continuation.  If all the arguments are simple, we can evaluate
;;     the operator expression first, and keeping it in VAL0 while pushing
;;     the arguments.
;;     Notably, a named let expression tends to become a head-heavy call,
;;     so it is worth to treat it specially.
;;     Note that this head-heavy call optimization relies on the arguments
;;     to use combined instructions such as CONST-PUSH or LREF-PUSH.  If
;;     the instruction combination is turned off, we can't use this since
;;     VAL0 is overwritten by arguments.
;;
;;  5. Other call node generates the standard calling sequence.
;;

;; stack depth of $CALL nodes:
;;  - if nargs >= 1, we need (# of args) + (env header) slots
;;  - if generic call, +2 for possible object-apply hack and next-method.
;;  - if non-tail call, + CONT_FRAME_SIZE.

(define (pass5/$CALL iform ccb renv ctx)
  (case ($call-flag iform)
    [(local) (pass5/local-call iform ccb renv ctx)]
    [(embed) (pass5/embed-call iform ccb renv ctx)]
    [(jump)  (pass5/jump-call  iform ccb renv ctx)]
    [else
     (if (and (bottom-context? ctx)
              (has-tag? ($call-proc iform) $LET)
              (all-args-simple? ($call-args iform))
              (not (vm-compiler-flag-is-set? SCM_COMPILE_NOCOMBINE)))
       (pass5/head-heavy-call iform ccb renv ctx)
       (pass5/normal-call iform ccb renv ctx))]))

;; Local call
;;   PROC is always $LREF.
(define (pass5/local-call iform ccb renv ctx)
  (let* ([args ($call-args iform)]
         [nargs (length args)])
    (if (tail-context? ctx)
      (let1 dinit (pass5/prepare-args args ccb renv ctx)
        (pass5/rec ($call-proc iform) ccb renv 'normal/top)
        (compiled-code-emit1i! ccb LOCAL-ENV-TAIL-CALL nargs ($*-src iform))
        (if (= nargs 0) 0 (imax dinit (+ nargs ENV_HEADER_SIZE))))
      (let1 merge-label (compiled-code-new-label ccb)
        (compiled-code-emit1oi! ccb PRE-CALL nargs merge-label ($*-src iform))
        (let1 dinit (pass5/prepare-args args ccb renv ctx)
          (pass5/rec ($call-proc iform) ccb renv 'normal/top)
          (compiled-code-emit1i! ccb LOCAL-ENV-CALL nargs ($*-src iform))
          (compiled-code-set-label! ccb merge-label)
          (if (= nargs 0)
            CONT_FRAME_SIZE
            (imax dinit (+ nargs ENV_HEADER_SIZE CONT_FRAME_SIZE))))))))

;; Embedded call
;;   $call-proc has $lambda node.  We inline its body.
;;   We also record the RENV to the current node, so that the jump calls
;;   to the inlined body can adjust env frame properly.
(define (pass5/embed-call iform ccb renv ctx)
  (let* ([proc ($call-proc iform)]
         [args ($call-args iform)]
         [nargs (length args)]
         [label ($lambda-body proc)]
         [lvars ($lambda-lvars proc)]
         [newenv (if (= nargs 0) renv (cons lvars renv))]
         [merge-label (compiled-code-new-label ccb)])
    ($call-renv-set! iform (reverse renv))
    (unless (tail-context? ctx)
      (compiled-code-emit1oi! ccb PRE-CALL nargs merge-label ($*-src iform)))
    (let1 dinit (if (> nargs 0)
                  (rlet1 d (pass5/prepare-args args ccb renv ctx)
                    (compiled-code-emit1i! ccb LOCAL-ENV nargs ($*-src iform))
                    (pass5/box-mutable-lvars lvars ccb))
                  0)
      (compiled-code-set-label! ccb (pass5/ensure-label ccb label))
      (let1 dbody (pass5/rec ($label-body label) ccb newenv 'tail)
        (compiled-code-emit-RET! ccb)
        (compiled-code-set-label! ccb merge-label)
        (if (= nargs 0)
          (+ CONT_FRAME_SIZE dbody)
          (imax dinit (+ nargs ENV_HEADER_SIZE CONT_FRAME_SIZE dbody)))))
    ))

;; Jump call
;;   $call-proc has a $call[embed] node, whose proc slot has $lambda
;;   node, whose proc slot has $label node.
;; NB: we're not sure whether we'll have non-tail jump call yet.
(define (pass5/jump-call iform ccb renv ctx)
  (let ([args ($call-args iform)]
        [embed-node ($call-proc iform)])
    (let ([nargs (length args)]
          [label ($lambda-body ($call-proc embed-node))]
          [lvars ($lambda-lvars ($call-proc embed-node))]
          [renv-diff (list-remove-prefix ($call-renv embed-node)
                                         (reverse renv))])
      (unless renv-diff
        (errorf "[internal error] $call[jump] appeared out of context of related $call[embed] (~s vs ~s)"
                ($call-renv embed-node) renv))
      (if (tail-context? ctx)
        (let1 dinit (pass5/prepare-args args ccb renv ctx)
          (pass5/emit-local-env-jump ccb lvars (length renv-diff)
                                     (pass5/ensure-label ccb label)
                                     ($*-src iform))
          (if (= nargs 0) 0 (imax dinit (+ nargs ENV_HEADER_SIZE))))
        (let1 merge-label (compiled-code-new-label ccb)
          (compiled-code-emit1oi! ccb PRE-CALL nargs merge-label ($*-src iform))
          (let1 dinit (pass5/prepare-args args ccb renv ctx)
            (pass5/emit-local-env-jump ccb lvars (length renv-diff)
                                       (pass5/ensure-label ccb label)
                                       ($*-src iform))
            (compiled-code-set-label! ccb merge-label)
            (if (= nargs 0)
              CONT_FRAME_SIZE
              (imax dinit (+ nargs ENV_HEADER_SIZE CONT_FRAME_SIZE)))))
        ))))

(define (pass5/emit-local-env-jump ccb lvars env-depth label src)
  (let loop ([lvs lvars])
    (cond [(null? lvs)  ; no need of boxing.
           (compiled-code-emit1oi! ccb LOCAL-ENV-JUMP env-depth label src)]
          [(not (lvar-immutable? (car lvs))) ; need boxing
           (compiled-code-emit1i! ccb LOCAL-ENV-SHIFT env-depth src)
           (pass5/box-mutable-lvars lvars ccb)
           (compiled-code-emit0oi! ccb JUMP label src)]
          [else
           (loop (cdr lvs))])))

;; Head-heavy call
(define (pass5/head-heavy-call iform ccb renv ctx)
  (let* ([args ($call-args iform)]
         [nargs (length args)])
    (if (tail-context? ctx)
      (let* ([dproc (pass5/rec ($call-proc iform)
                               ccb renv (normal-context ctx))]
             [dinit (pass5/prepare-args args ccb renv 'normal/top)])
        (compiled-code-emit1i! ccb TAIL-CALL nargs ($*-src iform))
        (imax dinit (+ nargs dproc ENV_HEADER_SIZE)))
      (let1 merge-label (compiled-code-new-label ccb)
        (compiled-code-emit1oi! ccb PRE-CALL nargs merge-label ($*-src iform))
        (let* ([dproc (pass5/rec ($call-proc iform)
                                 ccb renv (normal-context ctx))]
               [dinit (pass5/prepare-args args ccb renv 'normal/top)])
          (compiled-code-emit1i! ccb CALL nargs ($*-src iform))
          (compiled-code-set-label! ccb merge-label)
          (+ CONT_FRAME_SIZE (imax dinit (+ nargs dproc ENV_HEADER_SIZE)))))
      )))

;; Normal call
(define (pass5/normal-call iform ccb renv ctx)
  (let* ([args ($call-args iform)]
         [nargs (length args)])
    (if (tail-context? ctx)
      (let* ([dinit (pass5/prepare-args args ccb renv ctx)]
             [dproc (pass5/rec ($call-proc iform) ccb renv 'normal/top)])
        (compiled-code-emit1i! ccb TAIL-CALL nargs ($*-src iform))
        (imax dinit (+ nargs dproc ENV_HEADER_SIZE)))
      (let1 merge-label (compiled-code-new-label ccb)
        (compiled-code-emit1oi! ccb PRE-CALL nargs merge-label ($*-src iform))
        (let* ([dinit (pass5/prepare-args args ccb renv ctx)]
               [dproc (pass5/rec ($call-proc iform) ccb renv 'normal/top)])
          (compiled-code-emit1i! ccb CALL nargs ($*-src iform))
          (compiled-code-set-label! ccb merge-label)
          (+ CONT_FRAME_SIZE (imax dinit (+ nargs dproc ENV_HEADER_SIZE)))))
      )))

(define (all-args-simple? args)
  (cond [(null? args) #t]
        [(memv (iform-tag (car args)) `(,$LREF ,$CONST))
         (all-args-simple? (cdr args))]
        [else #f]))

;; Returns a part of lis whose head is removed.  If HEAD is not a prefix
;; of LIS, returns #f.
(define (list-remove-prefix head lis)
  (let loop ((head head) (lis lis))
    (cond [(null? head) lis]
          [(null? lis) #f]
          [(eq? (car head) (car lis)) (loop (cdr head) (cdr lis))]
          [else #f])))

(define (pass5/ensure-label ccb label-node)
  (or ($label-label label-node)
      (rlet1 lab (compiled-code-new-label ccb)
        ($label-label-set! label-node lab))))

(define (pass5/$PROMISE iform ccb renv ctx)
  (rlet1 d (pass5/rec ($promise-expr iform) ccb renv (normal-context ctx))
    (compiled-code-emit0i! ccb PROMISE ($*-src iform))))

;; $ASMs.  For some instructions, we may pick more specialized one
;; depending on its arguments.

(define (pass5/$ASM iform ccb renv ctx)
  (let ([info ($*-src iform)]
        [insn ($asm-insn iform)]
        [args ($asm-args iform)])
    (case/unquote
     (car insn)
     [(EQ)
      (pass5/asm-eq  info (car args) (cadr args) ccb renv ctx)]
     [(EQV)
      (pass5/asm-eqv info (car args) (cadr args) ccb renv ctx)]
     [(NUMEQ2)
      (pass5/asm-numeq2 info (car args) (cadr args) ccb renv ctx)]
     [(NUMLT2 NUMLE2 NUMGT2 NUMGE2)
      (pass5/asm-numcmp info (car insn) (car args) (cadr args) ccb renv ctx)]
     [(NUMADD2)
      (pass5/asm-numadd2 info (car args) (cadr args) ccb renv ctx)]
     [(NUMSUB2)
      (pass5/asm-numsub2 info (car args) (cadr args) ccb renv ctx)]
     [(NUMMUL2)
      (pass5/asm-nummul2 info (car args) (cadr args) ccb renv ctx)]
     [(NUMDIV2)
      (pass5/asm-numdiv2 info (car args) (cadr args) ccb renv ctx)]
     [(LOGAND LOGIOR LOGXOR)
      (pass5/asm-bitwise info (car insn) (car args) (cadr args) ccb renv ctx)]
     [(VEC-REF)
      (pass5/asm-vec-ref info (car args) (cadr args) ccb renv ctx)]
     [(VEC-SET)
      (pass5/asm-vec-set info (car args) (cadr args) (caddr args) ccb renv ctx)]
     [(SLOT-REF)
      (pass5/asm-slot-ref info (car args) (cadr args) ccb renv ctx)]
     [(SLOT-SET)
      (pass5/asm-slot-set info (car args) (cadr args) (caddr args) ccb renv ctx)]
     [(TAIL-APPLY)
      (if (tail-context? ctx)
        (pass5/asm-generic ccb insn args info renv)
        (let1 merge-label (compiled-code-new-label ccb)
          (compiled-code-emit0oi! ccb PRE-CALL merge-label info)
          (let1 d (pass5/asm-generic ccb insn args info renv)
            (compiled-code-set-label! ccb merge-label)
            (+ CONT_FRAME_SIZE d))))]
     [else
      (pass5/asm-generic ccb insn args info renv)])))

(define (pass5/asm-generic ccb insn args info renv)
  ;; general case
  (case (length args)
    [(0) (pass5/emit-asm! ccb insn info) 0]
    [(1) (rlet1 d (pass5/rec (car args) ccb renv 'normal/top)
           (pass5/emit-asm! ccb insn info))]
    [(2) (let1 d0 (pass5/rec (car args) ccb renv 'normal/top)
           (compiled-code-emit-PUSH! ccb)
           (let1 d1 (pass5/rec (cadr args) ccb renv 'normal/top)
             (pass5/emit-asm! ccb insn info)
             (imax d0 (+ d1 1))))]
    [else
     (let loop ([args args] [depth 0] [cnt 0])
       (cond [(null? (cdr args))
              (let1 d (pass5/rec (car args) ccb renv 'normal/top)
                (pass5/emit-asm! ccb insn info)
                (imax depth (+ cnt d)))]
             [else
              (let1 d (pass5/rec (car args) ccb renv 'normal/top)
                (compiled-code-emit-PUSH! ccb)
                (loop (cdr args) (imax depth (+ d cnt)) (+ cnt 1)))]))]
    ))

(define (pass5/emit-asm! ccb insn info)
  (match insn
    [(code)           (compiled-code-emit0i! ccb code info)]
    [(code arg0)      (compiled-code-emit1i! ccb code arg0 info)]
    [(code arg0 arg1) (compiled-code-emit2i! ccb code arg0 arg1 info)]))

;; Utility macros.  Assumes ccb, renv and ctx are visible.

(define-macro (pass5/builtin-twoargs info code param arg0 arg1)
  (let ([d0 (gensym)]
        [d1 (gensym)])
    `(let1 ,d0 (pass5/rec ,arg0 ccb renv (normal-context ctx))
       (compiled-code-emit-PUSH! ccb)
       (let1 ,d1 (pass5/rec ,arg1 ccb renv 'normal/top)
         ,(if (equal? param 0)
            `(compiled-code-emit0i! ccb ,code ,info)
            `(compiled-code-emit1i! ccb ,code ,param ,info))
         (imax ,d0 (+ ,d1 1))))))

(define-macro (pass5/builtin-onearg info code param arg0)
  (let1 d (gensym)
    `(rlet1 ,d (pass5/rec ,arg0 ccb renv (normal-context ctx))
       ,(if (equal? param 0)
          `(compiled-code-emit0i! ccb ,code ,info)
          `(compiled-code-emit1i! ccb ,code ,param ,info)))))

(define-macro (pass5/builtin-onearg+operand info code param operand arg0)
  (let1 d (gensym)
    `(rlet1 ,d (pass5/rec ,arg0 ccb renv (normal-context ctx))
       ,(if (equal? param 0)
          `(compiled-code-emit0oi! ccb ,code ,operand ,info)
          `(compiled-code-emit1oi! ccb ,code ,param ,operand ,info)))))

(define-macro (pass5/builtin-nargs info code args)
  `(%pass5/builtin-nargs ccb ,info ,code ,args ccb renv))

(define (%pass5/builtin-nargs ccb info code args ccb renv)
  (if (null? args)
    (begin (compiled-code-emit0i! ccb code info) 0)
    (let loop ([as args] [depth 0] [cnt 0])
      (cond [(null? (cdr as))
             (let1 d (pass5/rec (car as) ccb renv 'normal/top)
               (compiled-code-emit1i! ccb code (length args) info)
               (imax (+ d cnt) depth))]
            [else
             (let1 d (pass5/rec (car as) ccb renv 'normal/top)
               (compiled-code-emit-PUSH! ccb)
               (loop (cdr as) (imax (+ d cnt) depth) (+ cnt 1)))]))))

(define (pass5/$CONS iform ccb renv ctx)
  (pass5/builtin-twoargs ($*-src iform)
                         CONS 0 ($*-arg0 iform) ($*-arg1 iform)))

(define (pass5/$APPEND iform ccb renv ctx)
  (pass5/builtin-twoargs ($*-src iform) APPEND 2 ($*-arg0 iform) ($*-arg1 iform)))

(define (pass5/$LIST iform ccb renv ctx)
  (pass5/builtin-nargs ($*-src iform) LIST ($*-args iform)))

(define (pass5/$LIST* iform ccb renv ctx)
  (pass5/builtin-nargs ($*-src iform) LIST-STAR ($*-args iform)))

(define (pass5/$VECTOR iform ccb renv ctx)
  (pass5/builtin-nargs ($*-src iform) VEC ($*-args iform)))

(define (pass5/$LIST->VECTOR iform ccb renv ctx)
  (pass5/builtin-onearg ($*-src iform) LIST2VEC 0 ($*-arg0 iform)))

(define (pass5/$MEMV iform ccb renv ctx)
  (pass5/builtin-twoargs ($*-src iform) MEMV 0 ($*-arg0 iform) ($*-arg1 iform)))

(define (pass5/$EQ? iform ccb renv ctx)
  (pass5/asm-eq ($*-src iform) ($*-arg0 iform) ($*-arg1 iform) ccb renv ctx))

(define (pass5/$EQV? iform ccb renv ctx)
  (pass5/asm-eqv ($*-src iform) ($*-arg0 iform) ($*-arg1 iform) ccb renv ctx))

;; handlers to emit specialized instruction when applicable

(define (pass5/asm-eq info x y ccb renv ctx)
  (pass5/builtin-twoargs info EQ 0 x y))

(define (pass5/asm-eqv info x y ccb renv ctx)
  (pass5/builtin-twoargs info EQV 0 x y))

(define (pass5/asm-numeq2 info x y ccb renv ctx)
  (pass5/builtin-twoargs info NUMEQ2 0 x y))

(define (pass5/asm-numcmp info code x y ccb renv ctx)
  (pass5/builtin-twoargs info code 0 x y))

(define (pass5/asm-numadd2 info x y ccb renv ctx)
  (or (and ($const? x)
           (integer-fits-insn-arg? ($const-value x))
           (pass5/builtin-onearg info NUMADDI ($const-value x) y))
      (and ($const? y)
           (integer-fits-insn-arg? ($const-value y))
           (pass5/builtin-onearg info NUMADDI ($const-value y) x))
      (and ($lref? y)
           (lvar-immutable? ($lref-lvar y))
           (receive (depth offset) (renv-lookup renv ($lref-lvar y))
             (pass5/builtin-onearg info LREF-VAL0-NUMADD2
                                   (+ (ash offset 10) depth) x)))
      (and ($lref? x)
           (lvar-immutable? ($lref-lvar x))
           (receive (depth offset) (renv-lookup renv ($lref-lvar x))
             (pass5/builtin-onearg info LREF-VAL0-NUMADD2
                                   (+ (ash offset 10) depth) y)))
      (pass5/builtin-twoargs info NUMADD2 0 x y)))

(define (pass5/asm-numsub2 info x y ccb renv ctx)
  (or (and ($const? x)
           (integer-fits-insn-arg? ($const-value x))
           (pass5/builtin-onearg info NUMSUBI ($const-value x) y))
      (and ($const? y)
           (integer-fits-insn-arg? ($const-value y))
           (pass5/builtin-onearg info NUMADDI (- ($const-value y)) x))
      (pass5/builtin-twoargs info NUMSUB2 0 x y)))

(define (pass5/asm-nummul2 info x y ccb renv ctx)
  (pass5/builtin-twoargs info NUMMUL2 0 x y))

(define (pass5/asm-numdiv2 info x y ccb renv ctx)
  (pass5/builtin-twoargs info NUMDIV2 0 x y))

;; if one of arg is constant, it's always x.  see builtin-inline-bitwise below.
(define (pass5/asm-bitwise info insn x y ccb renv ctx)
  (define lookup `((,LOGAND . ,LOGANDC)
                   (,LOGIOR . ,LOGIORC)
                   (,LOGXOR . ,LOGXORC)))
  (if ($const? x)
    (pass5/builtin-onearg+operand info (assv-ref lookup insn)
                                  0 ($const-value x) y)
    (pass5/builtin-twoargs info insn 0 x y)))

(define (pass5/asm-vec-ref info vec k ccb renv ctx)
  (cond [(and ($const? k)
              (unsigned-integer-fits-insn-arg? ($const-value k)))
         (pass5/builtin-onearg info VEC-REFI ($const-value k) vec)]
        [else
         (pass5/builtin-twoargs info VEC-REF 0 vec k)]))

(define (pass5/asm-vec-set info vec k obj ccb renv ctx)
  (cond [(and ($const? k)
              (unsigned-integer-fits-insn-arg? ($const-value k)))
         (pass5/builtin-twoargs info VEC-SETI ($const-value k) vec obj)]
        [else
         (let1 d0 (pass5/rec vec ccb renv (normal-context ctx))
           (compiled-code-emit-PUSH! ccb)
           (let1 d1 (pass5/rec k   ccb renv 'normal/top)
             (compiled-code-emit-PUSH! ccb)
             (let1 d2 (pass5/rec obj ccb renv 'normal/top)
               (compiled-code-emit0i! ccb VEC-SET info)
               (imax d0 (+ d1 1) (+ d2 2)))))]))

(define (pass5/asm-slot-ref info obj slot ccb renv ctx)
  (cond [($const? slot)
         (rlet1 d (pass5/rec obj ccb renv (normal-context ctx))
           (compiled-code-emit0oi! ccb SLOT-REFC ($const-value slot) info))]
        [else
         (pass5/builtin-twoargs info SLOT-REF 0 obj slot)]))

(define (pass5/asm-slot-set info obj slot val ccb renv ctx)
  (cond [($const? slot)
         (let1 d0 (pass5/rec obj ccb renv (normal-context ctx))
           (compiled-code-emit-PUSH! ccb)
           (let1 d1 (pass5/rec val ccb renv 'normal/top)
             (compiled-code-emit0oi! ccb SLOT-SETC ($const-value slot) info)
             (imax d0 (+ d1 1))))]
        [else
         (let1 d0 (pass5/rec obj ccb renv (normal-context ctx))
           (compiled-code-emit-PUSH! ccb)
           (let1 d1 (pass5/rec slot ccb renv 'normal/top)
             (compiled-code-emit-PUSH! ccb)
             (let1 d2 (pass5/rec val ccb renv 'normal/top)
               (compiled-code-emit0i! ccb SLOT-SET info)
               (imax d0 (+ d1 1) (+ d2 2)))))]))

;; Dispatch table.
(define *pass5-dispatch-table* (generate-dispatch-table pass5))

;; Returns depth and offset of local variable reference.
;;   renv-lookup : [[Lvar]], Lvar -> Int, Int
;;
(inline-stub
 (define-cproc renv-lookup (renv lvar)
   (let* ([depth::int 0])
     (dolist [fp renv]
       (let* ([count::int 1])
         (dolist [lp fp]
           (when (SCM_EQ lp lvar)
             (return (values (SCM_MAKE_INT depth)
                             (SCM_MAKE_INT (- (Scm_Length fp) count)))))
           (pre++ count)))
       (pre++ depth)))
   (Scm_Error "[internal error] stray local variable: %S" lvar)
   (return SCM_UNDEFINED)) ; dummy
 )

;; Emit code to evaluate expressions in args and push its result
;; into the stack one by one.  Returns the maximum depth of the stack.
;; lvars is #f for regular call sequence, or a list of lvars of the same
;; length of args for $LET or local calls.
(define (pass5/prepare-args args ccb renv ctx)
  (if (null? args)
    0
    (let1 d (pass5/rec (car args) ccb renv (normal-context ctx))
      (compiled-code-emit-PUSH! ccb)
      ;; NB: We check termination condition here.  This routine is called
      ;; lots of times, and (length args) is usually small (<=2 covers almost
      ;; half of the cases, and <=3 covers over 80%).  Check termination
      ;; condition before entering loop saves extra calculation of loop
      ;; arguments, and it is not negligible in this case.
      (if (null? (cdr args))
        d
        (let loop ([args  (cdr args)]
                   [depth (+ d 1)]
                   [cnt  1])
          (let1 d (pass5/rec (car args) ccb renv 'normal/top)
            (compiled-code-emit-PUSH! ccb)
            (if (null? (cdr args))
              (imax depth d)
              (loop (cdr args) (imax depth (+ d cnt 1)) (+ cnt 1)))))))))

;; In case of $LET
(define (pass5/box-mutable-lvars lvars ccb)
  (let1 envsize (length lvars)
    (let loop ([lvars lvars]
               [k 0])
      (unless (null? lvars)
        (unless (lvar-immutable? (car lvars))
          (compiled-code-emit1i! ccb BOX (- envsize k) (lvar-name (car lvars))))
        (loop (cdr lvars) (+ k 1))))))

;;============================================================
;; Inliners of builtin procedures
;;

;; If the subr has a directly corresponding VM instruction, the
;; inlining direction is embedded within the subr definition in
;; the stub file.  The inliners below deal with more complex
;; situations.

;; Some operations (e.g. NUMADD2) has specialized instructions when
;; one of the operands has certain properties (e.g. if one of the operand
;; is a small exact integer, NUMADDI can be used).  Such choice of
;; instructions are done in Pass 5 $ASM handler, since they may have
;; more information.  The inliner can emit a generic instruction and
;; leave the choice of specialized instructions to the later stage.

;; Defines builtin inliner for the existing SUBRs.
;; The binding of NAME must be visible from gauche.internal.
(define-macro (define-builtin-inliner name proc)
  (let1 debug-name (string->symbol #"inliner/~name")
    `(let1 ,debug-name ,proc
       (set! (%procedure-inliner ,name) ,debug-name)
       (%mark-binding-inlinable! (find-module 'gauche.internal) ',name))))

;; Some useful utilities
;;
(define-inline (asm-arg1 form insn x cenv)
  ($asm form insn (list (pass1 x cenv))))

(define-inline (asm-arg2 form insn x y cenv)
  ($asm form insn (list (pass1 x cenv) (pass1 y cenv))))

;; Generate two argument call of assembler insn INSN unconditionally
(define (gen-inliner-arg2 insn)
  (^[form cenv]
    (match form
      [(_ x y) (asm-arg2 form (list insn) x y cenv)]
      [else (undefined)])))

;; If the form has two arg and the latter arg is a constant exact integer
;; that fits insn arg, generate ***I type insn.  If both args are constant,
;; replace the form with ($const (proc x y)).  Otherwise give up.
(define (gen-inliner-argi insn proc)
  (^[form cenv]
    (match form
      [(_ n cnt)
       (receive (cnt-val cnt-tree) (check-numeric-constant cnt cenv)
         (receive (n-val n-tree) (check-numeric-constant n cenv)
           (cond [(and cnt-val n-val) ($const (proc n-val cnt-val))]
                 [(and cnt-val (integer-fits-insn-arg? cnt-val))
                  ($asm form `(,insn ,cnt-val) (list n-tree))]
                 [else (undefined)])))]
      [else (undefined)])))

(inline-stub
 (define-cproc %procedure-inliner (proc::<procedure>)
   (setter (proc::<procedure> inliner) ::<void>
           (set! (-> proc inliner) inliner))
   (return (?: (-> proc inliner) (-> proc inliner) '#f)))

 (define-cproc %mark-binding-inlinable! (mod::<module> name::<symbol>) ::<void>
   (let* ([g::ScmGloc* (Scm_FindBinding mod name 0)])
     (unless g
       (Scm_Error "[internal] %%mark-binding-inlinable!: \
                   no such binding for %S in %S"
                  (SCM_OBJ name) (SCM_OBJ mod)))
     (Scm_GlocMark g SCM_BINDING_INLINABLE)))
 )

;;--------------------------------------------------------
;; Inlining numeric operators
;;

;; (1) VM insturctions are usually binary where the corresponding
;;  Scheme operators are variable arity.  We analyze the arguments
;;  and generate a (possibly nested) $asm clause.
;;
;; (2) We try to fold constant operations.  Constant numbers may appear
;;  literally, or a result of constant-variable compilation or other
;;  constant folding.   Except the literal numbers we need to call
;;  pass1 first on the argument to see if we can get a constant.

;; NB: This part needs serious refactoring after 0.8.10.

;; Utility.  Returns two values.  The first value is a number, if
;; the given form yields a constant number.  The second value is
;; an intermediate form, if the given form is not a literal.
(define (check-numeric-constant form cenv)
  (if (number? form)
    (values form #f)
    (let1 f (pass1 form cenv)
      (if (and ($const? f) (number? ($const-value f)))
        (values ($const-value f) f)
        (values #f f)))))

(define (ensure-inexact-const numconstval)
  ($const (exact->inexact numconstval)))

(define-macro (define-builtin-inliner-+ op insn const)
  `(define-builtin-inliner ,op
     (^[form cenv]
       (define (fold-+ asm rest)
         (fold (^[arg asm]
                 (receive (val tree) (check-numeric-constant arg cenv)
                   ($asm form (list ,insn) (list asm (or tree (,const val))))))
               asm rest))
       (let inline ((args (cdr form)))
         (match args
           [()  (,const 0)]
           [(x)
            (receive (num tree) (check-numeric-constant x cenv)
              (if num
                (or tree (,const num))
                (undefined)))]
           [(x y . more)
            (receive (xval xtree) (check-numeric-constant x cenv)
              (receive (yval ytree) (check-numeric-constant y cenv)
                (if xval
                  (if yval
                    (inline (cons (,op xval yval) more))
                    (fold-+ ytree (cons xval more)))
                  (if yval
                    (fold-+ xtree (cons yval more))
                    (fold-+ ($asm form (list ,insn) `(,xtree ,ytree)) more)))))]
           )))))

(define-builtin-inliner-+ +  NUMADD2 $const)
(define-builtin-inliner-+ +. NUMIADD2 ensure-inexact-const)

(define-macro (define-builtin-inliner-- op insn const)
  `(define-builtin-inliner ,op
     (^[form cenv]
       (define (fold-- asm rest)
         (fold (^[arg asm]
                 (receive [val tree] (check-numeric-constant arg cenv)
                   ($asm form (list ,insn) (list asm (or tree (,const val))))))
               asm rest))
       (let inline ((args (cdr form)))
         (match args
           [()
            (error "procedure requires at least one argument:" form)]
           [(x)
            (receive (num tree) (check-numeric-constant x cenv)
              (if num
                (,const (- num))
                ,(if (eq? op '-)
                   '($asm form `(,NEGATE) (list tree))
                   (undefined))))]
           [(x y . more)
            (receive (xval xtree) (check-numeric-constant x cenv)
              (receive (yval ytree) (check-numeric-constant y cenv)
                (if xval
                  (if yval
                    (if (null? more)
                      ($const (,op xval yval))
                      (inline (cons (,op xval yval) more)))
                    (fold-- ($asm form (list ,insn)
                                  (list (or xtree ($const xval)) ytree))
                            more))
                  (fold-- ($asm form (list ,insn)
                                (list xtree (or ytree ($const yval))))
                          more))))]
           )))))

(define-builtin-inliner-- -  NUMSUB2  $const)
(define-builtin-inliner-- -. NUMISUB2 ensure-inexact-const)

(define-macro (define-builtin-inliner-* op insn const)
  `(define-builtin-inliner ,op
     (^[form cenv]
       (let inline ((args (cdr form)))
         (match args
           [()  (,const 1)]
           [(x)
            (receive (num tree) (check-numeric-constant x cenv)
              (if (number? num)
                (or tree (,const num))
                (undefined)))]
           [(x y . more)
            (receive (xval xtree) (check-numeric-constant x cenv)
              (receive (yval ytree) (check-numeric-constant y cenv)
                (if (and xval yval)
                  (inline (cons (,op xval yval) more))
                  (fold (^[arg asm]
                          ($asm form (list ,insn) (list asm (pass1 arg cenv))))
                        ($asm form (list ,insn)
                              (list (or xtree (,const xval))
                                    (or ytree (,const yval))))
                        more))))]
           )))))

(define-builtin-inliner-* *  NUMMUL2  $const)
(define-builtin-inliner-* *. NUMIMUL2 ensure-inexact-const)

;; NB: If we detect exact division-by-zero case, we shouldn't fold
;; the constant and let it be handled at runtime.  
(define-macro (define-builtin-inliner-/ op insn const)
  `(define-builtin-inliner ,op
     (^[form cenv]
       (let inline ((args (cdr form)))
         (match args
           [()
            (error "procedure requires at least one argument:" form)]
           [(x)
            (receive (num tree) (check-numeric-constant x cenv)
              (if (and (number? num)
                       ,(if (eq? insn 'NUMDIV2)
                          `(not (eqv? num 0)) ;exact zero
                          #t))
                ($const (,op num))
                (undefined)))]
           [(x y . more)
            (receive (xval xtree) (check-numeric-constant x cenv)
              (receive (yval ytree) (check-numeric-constant y cenv)
                (if (and xval yval
                         ,(if (eq? insn 'NUMDIV2)
                            `(not (and (eqv? yval 0) ;exact zero
                                       (exact? xval)))
                            #t))
                  (if (null? more)
                    ($const (,op xval yval))
                    (inline (cons (,op xval yval) more)))
                  (fold (^[arg asm]
                          ($asm form (list ,insn) (list asm (pass1 arg cenv))))
                        ($asm form (list ,insn)
                              (list (or xtree (,const xval))
                                    (or ytree (,const yval))))
                        more))))]
           )))))

(define-builtin-inliner-/ /  NUMDIV2  $const)
(define-builtin-inliner-/ /. NUMIDIV2 ensure-inexact-const)

(define-builtin-inliner =   (gen-inliner-arg2 NUMEQ2))
(define-builtin-inliner <   (gen-inliner-arg2 NUMLT2))
(define-builtin-inliner <=  (gen-inliner-arg2 NUMLE2))
(define-builtin-inliner >   (gen-inliner-arg2 NUMGT2))
(define-builtin-inliner >=  (gen-inliner-arg2 NUMGE2))

(define-builtin-inliner modulo (gen-inliner-argi NUMMODI modulo))
(define-builtin-inliner remainder (gen-inliner-argi NUMREMI remainder))
(define-builtin-inliner ash (gen-inliner-argi ASHI ash))

;; bitwise and, ior and xor.  we treat (op expr const) case specially.
(define (builtin-inliner-bitwise opname op opcode unit)
  ;; Classify the arguments to (integer) constants and non-constants.
  ;; Integer constants are folded.  Returns cons of the folded constant
  ;; (#f if no constant argument), and the list of iforms for the rest
  ;; of arguments.
  (define (classify-args args cenv)
    (let loop ([args args] [constval #f] [iforms '()])
      (if (null? args)
        (cons constval iforms)
        (receive (val tree) (check-numeric-constant (car args) cenv)
          (if (and val (exact-integer? val))
            (loop (cdr args) (if constval (op constval val) val) iforms)
            (loop (cdr args) constval (cons (or tree ($const val)) iforms)))))))

  (^[form cenv]
    (match (classify-args (cdr form) cenv)
      [(#f)         ($const unit)]
      [(constval)   ($const constval)]
      [(constval x) ($asm form `(,opcode) (list ($const constval) x))]
      [(#f x y)     ($asm form `(,opcode) (list x y))]
      [_ (undefined)])))

(define-builtin-inliner logand
  (builtin-inliner-bitwise 'logand logand LOGAND -1))
(define-builtin-inliner logior
  (builtin-inliner-bitwise 'logior logior LOGIOR 0))
(define-builtin-inliner logxor
  (builtin-inliner-bitwise 'logxor logxor LOGXOR 0))

;;--------------------------------------------------------
;; Inlining other operators
;;

(define-builtin-inliner vector-ref
  (^[form cenv]
    (match form
      [(_ vec ind)
       (asm-arg2 form `(,VEC-REF) vec ind cenv)]
      [else (undefined)])))

(define-builtin-inliner vector-set!
  (^[form cenv]
    (match form
      [(_ vec ind val)
       ($asm form `(,VEC-SET) `(,(pass1 vec cenv)
                                ,(pass1 ind cenv)
                                ,(pass1 val cenv)))]
      [else (error "wrong number of arguments for vector-set!:" form)])))

;; NB: %uvector-ref isn't public, and should be in the gauche.internal
;; module.  We don't have a convenience mechansi
(define-builtin-inliner %uvector-ref
  (^[form cenv]
    (match form
      [(_ vec (? integer? type) ind)
;; not enough evidence yet to support this is worth (see also vminsn.scm)
;;       (if (and (integer? ind)
;;                (unsigned-integer-fits-insn-arg? (* ind 16)))
;;         (asm-arg1 form `(,UVEC-REFI ,(+ (* ind 16) type)) vec cenv)
         (asm-arg2 form `(,UVEC-REF ,type) vec ind cenv)]
      [else (undefined)])))

(define-builtin-inliner zero?
  (^[form cenv]
    (match form
      [(_ arg)
       ($asm form `(,NUMEQ2) `(,(pass1 arg cenv) ,($const 0)))]
      [else (error "wrong number of arguments for zero?:" form)])))

(define-builtin-inliner acons
  (^[form cenv]
    (match form
      [(_ a b c)
       ($asm form `(,CONS) `(,($asm #f `(,CONS) `(,(pass1 a cenv)
                                                  ,(pass1 b cenv)))
                             ,(pass1 c cenv)))]
      [else (error "wrong number of arguments for acons:" form)])))

(define-builtin-inliner reverse
  (^[form cenv]
    (match form
      [(_ a) ($asm form `(,REVERSE) `(,(pass1 a cenv)))]
      [else (undefined)])))

(define-builtin-inliner current-input-port
  (^[form cenv]
    (match form
      [(_) ($asm form `(,CURIN) '())]
      [else (undefined)])))

(define-builtin-inliner current-output-port
  (^[form cenv]
    (match form
      [(_) ($asm form `(,CUROUT) '())]
      [else (undefined)])))

(define-builtin-inliner current-error-port
  (^[form cenv]
    (match form
      [(_) ($asm form `(,CURERR) '())]
      [else (undefined)])))

(define-builtin-inliner dynamic-wind
  (^[form cenv]
    (match form
      [(_ before thunk after)
       (let ([tenv (cenv-sans-name cenv)])
         (let ([b (pass1 before tenv)]
               [t (pass1 thunk cenv)]
               [a (pass1 after tenv)]
               [at (make-lvar 'after)]
               [bt (make-lvar 'before)]
               [tt (make-lvar 'thunk)]
               [r (make-lvar 'tmp)])
           (if (constant-lambda? a)
             ;; when after thunk is dummy, we don't bother to call it.
             ($let form 'let `(,at ,bt ,tt) `(,a ,b ,t)
                   ($seq
                    `(,($call before ($lref bt) '())
                      ,($asm form `(,PUSH-HANDLERS) `(,($lref bt) ,($lref at)))
                      ,($call thunk ($lref tt) '()))))
             ;; normal path
             ($let form 'let `(,at ,bt ,tt) `(,a ,b ,t)
                   ($seq
                    `(,($call before ($lref bt) '())
                      ,($asm form `(,PUSH-HANDLERS) `(,($lref bt) ,($lref at)))
                      ,($receive #f 0 1 (list r)
                                 ($call thunk ($lref tt) '())
                                 ($seq
                                  `(,($asm form `(,POP-HANDLERS) '())
                                    ,($call after ($lref at) '())
                                    ,($asm #f `(,TAIL-APPLY 2)
                                           (list ($gref values.) ($lref r))))))
                      ))))))]
      [_ (undefined)])))

;;--------------------------------------------------------
;; Customizable inliner interface
;;

;; This is a hook for compiler macros and other experiment
;; on user-level optimization.  The "custom inliner" procedure
;; takes a form, and returns a form that may be translated.
;; We haven't decided our base mechanism of hygienic macros,
;; but for the time being, we mimic explicitly renaming macro.

(define (%bind-inline-er-transformer module name er-xformer)
  (define macro-def-cenv (%make-cenv module '()))
  ($ %attach-inline-transformer module name
     (^[form cenv]
       ;; Call the transformer with rename and compare procedure,
       ;; just like explicit renaming macro.  However, THE CURRENT
       ;; CODE DOES NOT IMPLEMENT PROPER SEMANTICS.  They're just
       ;; placeholders for experiment.
       (er-xformer form
                   (cut ensure-identifier <> macro-def-cenv)
                   (^[a b] (free-identifier=? (ensure-identifier a cenv)
                                              (ensure-identifier b cenv)))))))

(define (%attach-inline-transformer module name xformer)
  (define proc (global-variable-ref module name #f))

  (unless proc
    (errorf "define-compiler-macro: procedure `~s' not defined in ~s"
            name module))
  ;; If PROC is defined by define-inline (thus have a packed IForm in
  ;; %procedure-inliner), we keep it and applies expand-inline-procedure
  ;; after the compiler macro finishes its job.
  (let1 orig-inliner (%procedure-inliner proc)
    (when (procedure? orig-inliner)
      (warn "Attaching a compiler macro to ~a clobbers previously attached \
             inline transformers." proc))
    (set! (%procedure-inliner proc)
          (^[form cenv]
            (let1 r (xformer form cenv)
              (cond [(eq? form r) ; no inline operation is triggered.
                     (if (vector? orig-inliner)
                       (expand-inlined-procedure form
                                                 (unpack-iform orig-inliner)
                                                 (imap (cut pass1 <> cenv)
                                                       (cdr form)))
                       (undefined))]
                    [else (pass1 r cenv)])))))
  (%mark-binding-inlinable! module name)
  name)    

;;============================================================
;; Macro support basis
;;

;; Primitive transformer takes the input form, macro-definition
;; environment, and macro-use environment.
;;
;; (Sexpr, Env, Env) -> Sexpr
;;
;; The transformer should treat the envionments as opaque data.
;; Currently it's just Cenv's, but we may change it later.

;; Internal.  The syntax (primitive-macro-transformer xformer)
;; would be expanded to a call to this procedure.
(define (%make-primitive-transformer xformer def-env)
  (%make-macro-transformer (cenv-exp-name def-env)
                           (^[form use-env] (xformer form def-env use-env))))

;; er-renamer :: (Form, [Id]) -> (Form, [Id])
;; Where symbol or identifiers in the Form will be replaced with
;; fresh identifiers.   If Form has more than one occurence of a
;; specific symbol-or-id, the resulting Form has the same (eq?)
;; identifier for those occurrences.
(define (er-renamer form dict module env)
  (cond
   [(variable? form)
    (if-let1 id (assq-ref dict form)
      (values id dict)
      (let1 id (make-identifier form module env)
        (values id (acons form id dict))))]
   [(pair? form)
    (receive (a dict) (er-renamer (car form) dict module env)
      (receive (d dict) (er-renamer (cdr form) dict module env)
        (if (and (eq? a (car form)) (eq? d (cdr form)))
          (values form dict)
          (values (cons a d) dict))))]
   [(vector? form)
    (let loop ([i 0] [vec #f] [dict dict])
      (if (= i (vector-length form))
        (values (or vec form) dict)
        (receive (e dict) (er-renamer (vector-ref form i) dict module env)
          (if (eq? e (vector-ref form i))
            (loop (+ i 1) vec dict)
            (let1 vec (or vec (vector-copy form))
              (vector-set! vec i e)
              (loop (+ i 1) vec dict))))))]
   [else (values form dict)]))

;; er-comparer :: (Sym-or-id, Sym-or-id, Env, Env) -> Bool
(define (er-comparer a b uenv cenv)
  (and (variable? a)
       (variable? b)
       (let ([a1 (cenv-lookup-variable uenv a)]
             [b1 (cenv-lookup-variable uenv b)])
         (or (eq? a1 b1)
             (free-identifier=? a1 b1)))))

;; xformer :: (Sexpr, (Sym -> Sym), (Sym, Sym -> Bool)) -> Sexpr
(define (%make-er-transformer xformer def-env)
  (define def-module (cenv-module def-env))
  (define def-frames (cenv-frames def-env))
  (define (expand form uenv)
    (let1 dict '()
      (xformer form
               (^[sym]
                 (receive [id dict_] (er-renamer sym dict def-module def-frames)
                   (set! dict dict_)
                   id))
               (^[a b] (er-comparer a b uenv def-env)))))
  (%make-macro-transformer (cenv-exp-name def-env) expand))

;; this is called during macro expansion
(define (%expand-er-transformer form cenv)
  (match form
    [(_ xformer)
     ;; See the discussion in primitive-macro-transformer above.
     (let1 def-env-form (if (null? (cenv-frames cenv))
                          `(,%make-toplevel-cenv. ',(cenv-exp-name cenv))
                          cenv)
       `(,%make-er-transformer. ,xformer ,def-env-form))]
    [_ (error "Invalid er-macro-transformer form:" form)]))
     
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
      (and (identifier? k) (keyword? (identifier-name k)))))

(define (global-eq? var sym cenv)  ; like free-identifier=?, used in pass1.
  (and (variable? var)
       (let1 v (cenv-lookup-variable cenv var)
         (and (identifier? v)
              (eq? (unwrap-syntax v) sym)
              (null? (identifier-env v))))))

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
(define-macro (define-enum . _)
  ((with-module gauche.internal %precomp-only) 'define-enum))
(define-macro (define-enum-conditionally . _)
  ((with-module gauche.internal %precomp-only) 'define-enum-conditionally))


;; The form (declare ...) may be used in wider purpose.  For the time
;; being we use it in limited purposes for compilers.  In interpreter
;; we just ignore it.
(define-macro (declare . _) #f)
