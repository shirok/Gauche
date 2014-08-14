;;;
;;; Generates builtin symbols
;;;

(use srfi-1)
(use file.util)
(use gauche.cgen)
(use gauche.parameter)
(use gauche.sequence)

(define *unit*
  (make <cgen-unit>
    :name "builtin-syms"
    :preamble "/* Generated from builtin-syms.scm.  DO NOT EDIT\n\
                  This file may be changed by minor version up, and\n\
                  binaries including builtin-syms.h must be recompiled.\n\
                  That is, binary compatibility isn't guaranteed if you\n\
                  use SCM_SYM_*. */"
    :c-file "builtin-syms.c"
    :h-file "gauche/priv/builtin-syms.h"
    :init-prologue "static void init_builtin_syms(void)\n{"
    :init-epilogue "}"
    ))

(define (main args)
  (parameterize ([cgen-current-unit *unit*])
    (cgen-decl "#define LIBGAUCHE_BODY"
               "#include <gauche.h>")

    (cgen-extern "SCM_EXTERN ScmSymbol Scm_BuiltinSymbols[];")
    (cgen-body "ScmSymbol Scm_BuiltinSymbols[] = {")
    (cgen-body "#define ENTRY(s) \
                  {{ SCM_CLASS_STATIC_TAG(Scm_SymbolClass) }, \
                   SCM_STRING(s), SCM_SYMBOL_FLAG_INTERNED }")
    (cgen-init "#define INTERN(s, i) \
                  Scm_HashTableSet(obtable, s, SCM_OBJ(&Scm_BuiltinSymbols[i]), 0)")

    (for-each-with-index
     (^[index entry]
       (let* ([str (cgen-literal (symbol->string (car entry)))]
              [strref (cgen-cexpr str)]
              [macro-name (cadr entry)])
         (cgen-extern (format "#define ~a SCM_OBJ(&Scm_BuiltinSymbols[~a])"
                              macro-name index))
         (cgen-body (format "ENTRY(~a)," strref))
         (cgen-init (format "INTERN(~a, ~a);" strref index))
         ))
     (symbols))

    (cgen-body "#undef ENTRY")
    (cgen-body "};")
    (cgen-init "#undef INTERN")

    (make-directory* "gauche/priv/")
    (cgen-emit-h (cgen-current-unit))
    (cgen-emit-c (cgen-current-unit))
    0))

;; add predefined symbols below -------------------------------

(define (symbols)
  '((quote                     SCM_SYM_QUOTE)
    (quasiquote                SCM_SYM_QUASIQUOTE)
    (unquote                   SCM_SYM_UNQUOTE)
    (unquote-splicing          SCM_SYM_UNQUOTE_SPLICING)
    (define                    SCM_SYM_DEFINE)
    (define-constant           SCM_SYM_DEFINE_CONSTANT)
    (define-in-module          SCM_SYM_DEFINE_IN_MODULE)
    (lambda                    SCM_SYM_LAMBDA)
    (if                        SCM_SYM_IF)
    (set!                      SCM_SYM_SET)
    (let                       SCM_SYM_LET)
    (let*                      SCM_SYM_LET_STAR)
    (letrec                    SCM_SYM_LETREC)
    (begin                     SCM_SYM_BEGIN)
    (when                      SCM_SYM_WHEN)
    (unless                    SCM_SYM_UNLESS)
    (and                       SCM_SYM_AND)
    (or                        SCM_SYM_OR)
    (cond                      SCM_SYM_COND)
    (case                      SCM_SYM_CASE)
    (else                      SCM_SYM_ELSE)
    (=>                        SCM_SYM_YIELDS)
    (do                        SCM_SYM_DO)
    (delay                     SCM_SYM_DELAY)
    (receive                   SCM_SYM_RECEIVE)
    (define-module             SCM_SYM_DEFINE_MODULE)
    (with-module               SCM_SYM_WITH_MODULE)
    (select-module             SCM_SYM_SELECT_MODULE)
    (current-module            SCM_SYM_CURRENT_MODULE)
    (import                    SCM_SYM_IMPORT)
    (export                    SCM_SYM_EXPORT)
    (rename                    SCM_SYM_RENAME)
    (define-macro              SCM_SYM_DEFINE_MACRO)
    (define-syntax             SCM_SYM_DEFINE_SYNTAX)
    (let-syntax                SCM_SYM_LET_SYNTAX)
    (letrec-syntax             SCM_SYM_LETREC_SYNTAX)
    (%syntax-rules             SCM_SYM_SYNTAX_RULES_INT)
    (syntax-rules              SCM_SYM_SYNTAX_RULES)
    (...                       SCM_SYM_ELLIPSIS)
    (%macroexpand              SCM_SYM_MACRO_EXPAND)
    (%macroexpand-1            SCM_SYM_MACRO_EXPAND_1)
    (%asm                      SCM_SYM_ASM)

    ;; class category
    (builtin                   SCM_SYM_BUILTIN)
    (abstract                  SCM_SYM_ABSTRACT)
    (base                      SCM_SYM_BASE)

    ;; proc
    (curried                   SCM_SYM_CURRIED)

    ;; core
    (*cond-features*           SCM_SYM_COND_FEATURES)

    ;; modules
    (null                      SCM_SYM_NULL)
    (scheme                    SCM_SYM_SCHEME)
    (gauche                    SCM_SYM_GAUCHE)
    (gauche.gf                 SCM_SYM_GAUCHE_GF)
    (gauche.internal           SCM_SYM_GAUCHE_INTERNAL)
    (user                      SCM_SYM_USER)
    (|#|                       SCM_SYM_SHARP)

    ;; load
    (*load-path*               SCM_SYM_LOAD_PATH)
    (*load-next*               SCM_SYM_LOAD_NEXT)
    (*load-history*            SCM_SYM_LOAD_HISTORY)
    (*load-port*               SCM_SYM_LOAD_PORT)
    (*load-suffixes*           SCM_SYM_LOAD_SUFFIXES)
    (*load-path-hooks*         SCM_SYM_LOAD_PATH_HOOKS)
    (*dynamic-load-path*       SCM_SYM_DYNAMIC_LOAD_PATH)

    ;; reader, writer, compiler, vm
    (source-info               SCM_SYM_SOURCE_INFO)
    (bind-info                 SCM_SYM_BIND_INFO)
    (arg-info                  SCM_SYM_ARG_INFO)
    (debug-print               SCM_SYM_DEBUG_PRINT)
    (define-reader-ctor        SCM_SYM_DEFINE_READER_CTOR)
    (string-interpolate        SCM_SYM_STRING_INTERPOLATE)
    (big-endian                SCM_SYM_BIG_ENDIAN)    ;; for binary.io, uvector
    (little-endian             SCM_SYM_LITTLE_ENDIAN) ;; ditto
    (arm-little-endian         SCM_SYM_ARM_LITTLE_ENDIAN) ;; ditto
    (%internal-eval            SCM_SYM_INTERNAL_EVAL)
    (%internal-apply           SCM_SYM_INTERNAL_APPLY)
    (%eval-before              SCM_SYM_EVAL_BEFORE)
    (%eval-after               SCM_SYM_EVAL_AFTER)
    (%toplevel                 SCM_SYM_TOPLEVEL)
    (syntax                    SCM_SYM_SYNTAX)
    (macro                     SCM_SYM_MACRO)
    (inline                    SCM_SYM_INLINE)

    ;; regexp
    (seq                       SCM_SYM_SEQ)
    (seq-case                  SCM_SYM_SEQ_CASE)
    (seq-uncase                SCM_SYM_SEQ_UNCASE)
    (alt                       SCM_SYM_ALT)
    (rep                       SCM_SYM_REP)
    (rep-min                   SCM_SYM_REP_MIN)
    (rep-while                 SCM_SYM_REP_WHILE)
    (any                       SCM_SYM_ANY)
    (bol                       SCM_SYM_BOL)
    (eol                       SCM_SYM_EOL)
    (wb                        SCM_SYM_WB)
    (nwb                       SCM_SYM_NWB)
    (comp                      SCM_SYM_COMP)
    (*                         SCM_SYM_STAR)
    (*?                        SCM_SYM_STARQ)
    (*+                        SCM_SYM_STARP)
    (+                         SCM_SYM_PLUS)
    (+?                        SCM_SYM_PLUSQ)
    (++                        SCM_SYM_PLUSP)
    (?                         SCM_SYM_QUESTION)
    (??                        SCM_SYM_QUESTIONQ)
    (?+                        SCM_SYM_QUESTIONP)
    (backref                   SCM_SYM_BACKREF)
    (once                      SCM_SYM_ONCE)
    (reg                       SCM_SYM_REG)
    (assert                    SCM_SYM_ASSERT)
    (nassert                   SCM_SYM_NASSERT)
    (lookbehind                SCM_SYM_LOOKBEHIND)
    (nlookbehind               SCM_SYM_NLOOKBEHIND)
    (cpat                      SCM_SYM_CPAT)
    (open-paren                SCM_SYM_OPEN_PAREN)
    (close-paren               SCM_SYM_CLOSE_PAREN)

    ;; system
    (directory                 SCM_SYM_DIRECTORY)
    (regular                   SCM_SYM_REGULAR)
    (character                 SCM_SYM_CHARACTER)
    (block                     SCM_SYM_BLOCK)
    (fifo                      SCM_SYM_FIFO)
    (symlink                   SCM_SYM_SYMLINK)
    (socket                    SCM_SYM_SOCKET)
    (time-utc                  SCM_SYM_TIME_UTC)

    ;; modules addition (move this up on 1.0 release)
    (keyword                   SCM_SYM_KEYWORD)
    ;; reader addition (move this up on 1.0 release)
    (legacy                    SCM_SYM_LEGACY)
    (permissive                SCM_SYM_PERMISSIVE)
    (warn-legacy               SCM_SYM_WARN_LEGACY)
    (strict-r7                 SCM_SYM_STRICT_R7)
    (debug-funcall             SCM_SYM_DEBUG_FUNCALL)
    ))


