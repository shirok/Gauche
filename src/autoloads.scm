;;;
;;; Generates default autoloads
;;;

(use srfi-1)
(use util.list)
(use util.match)
(use gauche.cgen)

(define *autoloads* '())

(cgen-current-unit
 (make <cgen-unit>
   :name "autoloads"
   :preamble "/* Generated from autoloads.scm $Revision: 1.53 $.  DO NOT EDIT */"
   :pre-decl '("#define LIBGAUCHE_BODY")
   :init-prologue "void Scm__InitAutoloads(void)\n{"
   ))

(define (register-autoload target path entries)
  (push! *autoloads*
         (list target path
               (fold (lambda (entry r)
                       (match entry
                         [(:macro . syms) (fold (cut acons <> #t <>) r syms)]
                         [sym (acons sym #f r)]))
                     '() entries))))

;; Emit code
(define (main args)
  ;; init
  (cgen-init "  ScmModule *scheme = Scm_SchemeModule();"
             "  ScmModule *gauche = Scm_GaucheModule();"
             "  ScmSymbol *sym, *import_from;"
             "  ScmObj al, path;")

  ;; loop
  (dolist (al (reverse *autoloads*))
    (let* ((where     (if (eq? (car al) 'scheme) 'scheme 'gauche))
           (path&from (if (string? (cadr al))
                        (let1 str (cgen-literal (cadr al))
                          (cons (cgen-cexpr str) "NULL"))
                        (let1 sym (cgen-literal (cadr al))
                          (cons (format "Scm_ModuleNameToPath(SCM_SYMBOL(~a))"
                                        (cgen-cexpr sym))
                                (format "SCM_SYMBOL(~a)" (cgen-cexpr sym))))))
           )
      (cgen-init #`"  path = ,(car path&from);"
                 #`"  import_from = ,(cdr path&from);")
      (dolist (ent (caddr al))
        (let ((str (cgen-literal (symbol->string (car ent)))))
          (cgen-init
           #`"  sym = SCM_SYMBOL(Scm_Intern(SCM_STRING(,(cgen-cexpr str))));"
           #`"  al = Scm_MakeAutoload(SCM_CURRENT_MODULE(), sym, SCM_STRING(path), import_from);")
          (if (cdr ent) ;; macro?
            (cgen-init
             #`"  Scm_Define(,|where|,, sym,, Scm_MakeMacroAutoload(sym,, SCM_AUTOLOAD(al)));")
            (cgen-init
             #`"  Scm_Define(,|where|,, sym,, al);"))))
      ))
  ;; emit
  (cgen-emit-c (cgen-current-unit))
  0)

;; Override autoload macro
(define-macro (autoload file . vars)
  `(register-autoload #f ',file ',vars))

(define-macro (autoload-scheme file . vars)
  `(register-autoload 'scheme ',file ',vars))


;;==========================================================
(autoload-scheme "gauche/numerical"
                 exp log sqrt expt cos sin tan asin acos atan
                 gcd lcm nearly=?)

(autoload "gauche/redefutil"
          redefine-class! class-redefinition
          update-direct-subclass! change-object-class)

(autoload gauche.charconv %open-input-file/conv %open-output-file/conv)

(autoload "gauche/signal" (:macro with-signal-handlers))

(autoload gauche.modutil (:macro export-if-defined use-version))

(autoload gauche.portutil
          port->string port->list port->string-list port->sexp-list
          copy-port port-fold port-fold-right port-for-each port-map 
          port-position-prefix port-tell)

(autoload "gauche/numerical" sinh cosh tanh asinh acosh atanh)

(autoload "gauche/logical"
          logtest logbit? copy-bit bit-field copy-bit-field
          integer-length)

(autoload "gauche/common-macros"
          (:macro syntax-error syntax-errorf)
          (:macro push! pop! inc! dec! update!)
          (:macro check-arg get-optional get-keyword*)
          (:macro ^ ^_ ^a ^b ^c ^d ^e ^f ^g ^h ^i ^j ^k ^l ^m ^n ^o ^p ^q
                  ^r ^s ^t ^u ^v ^w ^x ^y ^z)
          (:macro let1 if-let1 rlet1)
          (:macro let/cc) (:macro begin0) (:macro fluid-let)
          (:macro values-ref)
          (:macro dotimes dolist while until)
          (:macro guard unwind-protect))

(autoload gauche.regexp
          (:macro rxmatch-let rxmatch-if rxmatch-cond rxmatch-case)
          regexp-unparse)

(autoload gauche.procedure
          compose .$ complement pa$ map$ for-each$ apply$
          count$ fold$ fold-right$ reduce$ reduce-right$
          filter$ partition$ remove$ find$ find-tail$
          any$ every$ delete$ member$ assoc$
          any-pred every-pred
          ;;(:macro curry-lambda) (:macro define-curry)
          arity procedure-arity-includes?
          <arity-at-least> arity-at-least? arity-at-least-value disasm
          (:macro case-lambda) ~ ref*)

(autoload gauche.time (:macro time))

(autoload gauche.vm.debugger (:macro debug-print)
                             debug-print-width debug-source-info)

(autoload gauche.vm.profiler profiler-show profiler-show-load-stats)

(autoload srfi-0  (:macro cond-expand))
(autoload srfi-7  (:macro program))
(autoload srfi-26 (:macro cut cute))
(autoload srfi-31 (:macro rec))
(autoload srfi-55 (:macro require-extension))

(autoload gauche.interpolate string-interpolate)

(autoload gauche.auxsys
          fmod frexp modf ldexp
          sys-abort sys-realpath sys-mkfifo
          sys-fdset list->sys-fdset sys-fdset->list
          sys-setgid sys-setpgid sys-getpgid sys-getpgrp
          sys-setsid sys-setuid sys-times sys-uname sys-ctermid
          sys-gethostname sys-getdomainname
          sys-putenv sys-setenv sys-unsetenv sys-environ sys-environ->alist
          sys-utime
          sys-getgroups sys-getlogin sys-localeconv
          sys-getloadavg)

(autoload gauche.defvalues (:macro define-values set!-values))

(autoload gauche.stringutil string-split)

(autoload gauche.fileutil
          file-exists? file-is-regular? file-is-directory?
          glob glob-fold sys-glob glob-component->regexp make-glob-fs-fold
          sys-stat->file-type sys-stat->mode sys-stat->ino
          sys-stat->dev sys-stat->rdev sys-stat->nlink
          sys-stat->size sys-stat->uid sys-stat->gid
          sys-stat->atime sys-stat->mtime sys-stat->ctime
          sys-stat->type sys-tm->alist)

(autoload gauche.hashutil hash-table hash-table-fold
                          hash-table-for-each hash-table-map)

(autoload gauche.treeutil make-tree-map tree-map-empty?
                          tree-map-min tree-map-max
                          tree-map-pop-min! tree-map-pop-max!
                          tree-map-fold tree-map-fold-right
                          tree-map-map tree-map-for-each
                          tree-map-keys tree-map-values
                          tree-map->alist alist->tree-map)

(autoload gauche.libutil  library-fold library-map library-for-each
                          library-exists? library-has-module?)

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
                          
