;;;
;;; Generates default autoloads
;;;
;;; $Id: autoloads.scm,v 1.4 2004-01-19 23:14:32 shirok Exp $
;;;

(use srfi-1)
(use util.list)
(use gauche.cgen)

(define *autoloads* '())

(cgen-current-unit
 (make <cgen-unit>
   :name "autoloads"
   :preamble "/* Generated from autoloads.scm $Revision: 1.4 $.  DO NOT EDIT */"
   :init-prologue "void Scm__InitAutoloads(void)\n{"
   ))

(define (register-autoload target path entries)
  (push! *autoloads*
         (list target path
               (map (lambda (entry)
                      (if (pair? entry)
                        (cons (cadr entry) #t) ;; :macro
                        (cons entry #f)))
                    entries))))

;; Emit code
(define (main args)
  (cgen-decl "#define LIBGAUCHE_BODY"
             "#include \"gauche.h\"")
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
           #`"  al = Scm_MakeAutoload(sym, SCM_STRING(path), import_from);")
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
(autoload-scheme "gauche/listutil"
                  caaar caadr cadar caddr cdaar cdadr cddar cdddr
                  caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
                  cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr)
(autoload-scheme "gauche/with"
                  call-with-input-file call-with-output-file
                  with-input-from-file with-output-to-file)
(autoload-scheme "gauche/numerical"
                  exp log sqrt expt cos sin tan asin acos atan
                  gcd lcm numerator denominator
                  real-part imag-part)

#|
(autoload "gauche/object"
          (:macro define-generic) (:macro define-method)
          (:macro define-class)
          class-slot-ref class-slot-set! |setter of class-slot-ref|
          slot-push! slot-exists?
          class-name class-precedence-list class-direct-supers
          class-direct-slots class-direct-methods class-direct-subclasses
          class-slots class-slot-definition class-slot-accessor
          slot-definition-name slot-definition-options slot-definition-option
          slot-definition-allocation slot-definition-getter
          slot-definition-setter slot-definition-accessor
          x->string x->integer x->number ref |setter of ref|)
|#

(autoload "gauche/with"
          with-output-to-string call-with-output-string
          with-input-from-string call-with-input-string
          with-string-io call-with-string-io
          write-to-string read-from-string)

(autoload "gauche/signal"
          (:macro with-signal-handlers))

(autoload gauche.modutil
          (:macro extend) (:macro export-if-defined) (:macro use-version))

(autoload gauche.portutil
          port->string port->list port->string-list port->sexp-list
          copy-port port-fold port-fold-right port-for-each port-map 
          port-position-prefix port-tell)

(autoload "gauche/numerical"
          sinh cosh tanh asinh acosh atanh)

(autoload "gauche/logical"
          logtest logbit? copy-bit bit-field copy-bit-field logcount
          integer-length)

(autoload "gauche/common-macros"
          (:macro syntax-error) (:macro syntax-errorf) unwrap-syntax
          (:macro push!) (:macro pop!) (:macro inc!) (:macro dec!)
          (:macro update!)
          (:macro check-arg) (:macro get-keyword*)
          (:macro let1) (:macro begin0) (:macro fluid-let)
          (:macro dotimes) (:macro dolist) (:macro while) (:macro until))

(autoload gauche.regexp
          (:macro rxmatch-let) (:macro rxmatch-if)
          (:macro rxmatch-cond) (:macro rxmatch-case)
          regexp-replace regexp-replace-all regexp-quote)

(autoload gauche.procedure
          compose pa$ map$ for-each$ apply$ any-pred every-pred
          (:macro let-optionals*) (:macro let-keywords*)
          (:macro get-optional)
          arity procedure-arity-includes?
          <arity-at-least> arity-at-least? arity-at-least-value
          (:macro case-lambda))

(autoload gauche.vm.debugger
          enable-debug disable-debug (:macro debug-print))

(autoload srfi-0 (:macro cond-expand))
(autoload srfi-26 (:macro cut) (:macro cute))
(autoload srfi-31 (:macro rec))

(autoload gauche.interpolate string-interpolate)

(autoload gauche.auxsys
          fmod frexp modf ldexp
          sys-abort sys-mkfifo
          sys-setgid sys-setpgid sys-getpgid sys-getpgrp
          sys-setsid sys-setuid sys-times sys-uname sys-ctermid
          sys-gethostname sys-getdomainname sys-putenv
          sys-gettimeofday sys-chown sys-utime
          sys-getgroups sys-getlogin sys-localeconv)

(autoload gauche.defvalues
          (:macro define-values) (:macro set!-values))

(autoload gauche.stringutil string-split)

(autoload gauche.fileutil file-exists? file-is-regular? file-is-directory?
                          sys-stat->file-type sys-stat->mode sys-stat->ino
                          sys-stat->dev sys-stat->rdev sys-stat->nlink
                          sys-stat->size sys-stat->uid sys-stat->gid
                          sys-stat->atime sys-stat->mtime sys-stat->ctime
                          sys-stat->type sys-tm->alist)

(autoload gauche.hashutil hash-table hash-table-fold
                          hash-table-for-each hash-table-map)

(autoload gauche.libutil  library-fold library-map library-for-each
                          library-exists? library-has-module?)

(autoload gauche.sortutil sort sort! merge merge! sorted?)

