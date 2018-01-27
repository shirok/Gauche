;;;
;;; Generates default autoloads
;;;

(use util.match)
(use gauche.cgen)

(define *autoloads* '())


(cgen-current-unit
 (make <cgen-unit>
   :name "autoloads"
   :preamble "/* Generated from autoloads.scm.  DO NOT EDIT */"
   :init-prologue "void Scm__InitAutoloads(void)\n{"
   ))

(define (register-autoload target path entries)
  (push! *autoloads*
         (list target path
               (fold (^[entry r]
                       (match entry
                         [(':macro . syms) (fold (cut acons <> #t <>) r syms)]
                         [sym (acons sym #f r)]))
                     '() entries))))

;; Emit code
(define (main args)
  (cgen-decl "#define LIBGAUCHE_BODY"
             "#include <gauche.h>")
  ;; init
  (cgen-init "  ScmModule *gauche = Scm_GaucheModule();"
             "  ScmSymbol *sym, *import_from;"
             "  ScmObj al, path;")

  ;; loop
  (dolist (al (reverse *autoloads*))
    (let* ([where     (car al)]
           [path&from (if (string? (cadr al))
                        (let1 str (cgen-literal (cadr al))
                          (cons (cgen-cexpr str) "NULL"))
                        (let1 sym (cgen-literal (cadr al))
                          (cons (format "Scm_ModuleNameToPath(SCM_SYMBOL(~a))"
                                        (cgen-cexpr sym))
                                (format "SCM_SYMBOL(~a)" (cgen-cexpr sym)))))])
      (cgen-init #"  path = ~(car path&from);"
                 #"  import_from = ~(cdr path&from);")
      (dolist [ent (caddr al)]
        (let1 str (cgen-literal (symbol->string (car ent)))
          (cgen-init
           #"  sym = SCM_SYMBOL(Scm_Intern(SCM_STRING(~(cgen-cexpr str)))); /* ~(cgen-safe-comment (car ent)) */"
           #"  al = Scm_MakeAutoload(SCM_CURRENT_MODULE(), sym, SCM_STRING(path), import_from);")
          (if (cdr ent) ;; macro?
            (cgen-init
             #"  Scm_Define(~|where|, sym, Scm_MakeMacroAutoload(sym, SCM_AUTOLOAD(al)));")
            (cgen-init
             #"  Scm_Define(~|where|, sym, al);"))))
      ))
  ;; emit
  (cgen-emit-c (cgen-current-unit))
  0)

;; Override autoload macro
(define-macro (autoload file . vars)
  `(register-autoload 'gauche ',file ',vars))

(define-macro (autoload-scheme file . vars)
  `(register-autoload 'scheme ',file ',vars))

;;==========================================================
(autoload "gauche/numerical"
          exact-integer-sqrt
          continued-fraction real->rational
          expt-mod gamma lgamma
          real-valued? rational-valued? integer-valued?
          div-and-mod div mod div0-and-mod0 div0 mod0
          floor/ floor-quotient floor-remainder
          truncate/ truncate-quotient truncate-remainder
          square encode-float
          nearly=?)

(autoload "gauche/redefutil"
          redefine-class! class-redefinition
          update-direct-subclass! change-object-class)

(autoload gauche.charconv %open-input-file/conv %open-output-file/conv)

(autoload "gauche/signal" (:macro with-signal-handlers))

(autoload gauche.modutil (:macro export-if-defined use-version))

(autoload gauche.portutil
          port->string port->list port->string-list port->sexp-list
          copy-port)

(autoload "gauche/logical"
          logtest logbit? copy-bit bit-field copy-bit-field)

(autoload gauche.common-macros
          (:macro syntax-error syntax-errorf)
          (:macro push! pop! inc! dec! update!)
          (:macro check-arg get-optional get-keyword*)
          (:macro ^ ^_ ^a ^b ^c ^d ^e ^f ^g ^h ^i ^j ^k ^l ^m ^n ^o ^p ^q
                  ^r ^s ^t ^u ^v ^w ^x ^y ^z $)
          (:macro let1 if-let1 and-let1 rlet1)
          (:macro let/cc) (:macro begin0) (:macro fluid-let)
          (:macro let-values) (:macro let*-values)
          (:macro values-ref values->list)
          (:macro ecase)
          (:macro dotimes dolist doplist while until)
          (:macro guard unwind-protect)
          (:macro cond-list)
          (:macro assume)
          (:macro assume-type)
          )

(autoload gauche.macroutil
          (:macro quasirename))

(autoload gauche.regexp
          (:macro rxmatch-let rxmatch-if rxmatch-cond rxmatch-case)
          regexp-unparse rxmatch-substrings rxmatch-positions)

(autoload gauche.procedure
          compose .$ complement pa$ map$ for-each$ apply$
          count$ fold$ fold-right$ reduce$ reduce-right$
          filter$ partition$ remove$ find$ find-tail$
          any$ every$ delete$ member$ assoc$
          any-pred every-pred
          ;;(:macro curry-lambda) (:macro define-curry)
          arity procedure-arity-includes?
          <arity-at-least> arity-at-least? arity-at-least-value
          source-code source-location disasm
          generator-fold generator-fold-right generator-for-each
          generator-map generator-find
          ;; for the backward compatibility
          port-fold port-fold-right port-for-each port-map)

(autoload gauche.time (:macro time))

(autoload gauche.vm.debugger
          (:macro debug-print debug-funcall) 
          debug-print-width debug-source-info
          debug-print-pre debug-print-post debug-funcall-pre)

(autoload gauche.vm.profiler
          profiler-show profiler-show-load-stats with-profiler)

(autoload srfi-0  (:macro cond-expand))
(autoload srfi-7  (:macro program))
(autoload srfi-26 (:macro cut cute))
(autoload srfi-31 (:macro rec))
(autoload srfi-55 (:macro require-extension))

(autoload gauche.interpolate string-interpolate
                             (:macro string-interpolate*))

(autoload "gauche/sysutil"
          sys-realpath sys-fdset list->sys-fdset sys-fdset->list)

(autoload gauche.defvalues (:macro define-values set!-values))

(autoload gauche.stringutil string-split)

(autoload gauche.vecutil
          vector-tabulate vector-map vector-map! vector-for-each
          vector-map-with-index vector-map-with-index!
          vector-for-each-with-index reverse-list->vector)

(autoload gauche.computil
          boolean-comparator char-comparator char-ci-comparator
          string-ci-comparator symbol-comparator
          exact-integer-comparator integer-comparator rational-comparator
          real-comparator complex-comparator number-comparator
          pair-comparator list-comparator vector-comparator
          bytevector-comparator uvector-comparator
          make-eq-comparator make-eqv-comparator
          make-reverse-comparator make-key-comparator make-tuple-comparator
          make-pair-comparator
          make-list-comparator make-vector-comparator
          (:macro comparator-if<=>))

(autoload gauche.fileutil
          glob glob-fold sys-glob glob-component->regexp make-glob-fs-fold
          sys-stat->file-type sys-stat->mode sys-stat->ino
          sys-stat->dev sys-stat->rdev sys-stat->nlink
          sys-stat->size sys-stat->uid sys-stat->gid
          sys-stat->atime sys-stat->mtime sys-stat->ctime
          sys-stat->type sys-tm->alist)

(autoload gauche.hashutil
          hash-table-from-pairs hash-table-r7 hash-table
          hash-table-empty? hash-table-contains? hash-table-mutable?
          hash-table-empty-copy
          hash-table-size hash-table-entries hash-table-unfold
          hash-table-ref hash-table-ref/default
          hash-table-update!-r7 hash-table-update!/default hash-table-set!
          hash-table-intern!-r7 hash-table-delete!-r7 hash-table-pop!-r7
          hash-table-seek hash-table-find hash-table-find-r7
          hash-table-for-each hash-table-for-each-r7
          hash-table-map hash-table-map-r7 hash-table-map!-r7
          hash-table-map->list-r7
          hash-table-fold hash-table-fold-r7 hash-table-count-r7
          hash-table-prune!-r7
          hash-table-compare-as-sets hash-table=?
          hash-table-union! hash-table-intersection!
          hash-table-difference! hash-table-xor!
          boolean-hash char-hash char-ci-hash
          string-hash string-ci-hash
          symbol-hash number-hash hash-bound)

(autoload gauche.treeutil make-tree-map tree-map-empty?
                          tree-map-min tree-map-max
                          tree-map-pop-min! tree-map-pop-max!
                          tree-map-seek tree-map-fold tree-map-fold-right
                          tree-map-map tree-map-for-each
                          tree-map-keys tree-map-values
                          tree-map->alist alist->tree-map
                          tree-map-compare-as-sets
                          tree-map-compare-as-sequences)

(autoload gauche.libutil  library-fold library-map library-for-each
                          library-exists? library-has-module?
                          library-name->module-name)

(autoload gauche.sortutil sort sort! merge merge! sorted?
                          stable-sort stable-sort!
                          sort-by sort-by! stable-sort-by stable-sort-by!)

(autoload gauche.condutil make-condition-type condition-type?
                          make-condition condition-ref extract-condition
                          (:macro define-condition-type condition)
                          &condition &message &serious &error
                          &i/o-error &i/o-port-error
                          &i/o-read-error &i/o-write-error &i/o-closed-error
                          &read-error)

(autoload gauche.pputil   %pretty-print pprint)

;; Autoloading r7rs allows Gauche-native programs to load R7RS library
;; seamlessly.
(autoload r7rs (:macro define-library))
