;;;
;;; Generates default autoloads
;;;
;;; $Id: autoloads.scm,v 1.1 2003-12-09 19:50:31 shirok Exp $
;;;

(use srfi-1)
(use util.list)

(define *autoloads* '())

;; Override autoload macro
(define-macro (autoload file . vars)
  (push! *autoloads* (list #f file vars))
  #f)

(define-macro (autoload-scheme file . vars)
  (push! *autoloads* (list 'scheme file vars))
  #f)

;; Collect stuff
(define (all-symbols)
  (delete-duplicates (apply append
                            (filter symbol? (map cadr *autoloads*))
                            )))

;; Emit code
(define (emit)
  (let* ((strs (filter string? (map cadr *autoloads*)))
         (str-alist (map (lambda (cnt str)
                           (cons (string->symbol #`"str,cnt") str))
                         (iota (length strs)) strs))
         (mods (filter symbol? (map cadr *autoloads*)))
         (mod-alist (map (lambda (cnt sym)
                           (cons (string->symbol #`"mod,cnt") sym))
                         (iota (length mods)) mods))
         (syms (append-map (lambda (al)
                             (map (lambda (v)
                                    (if (symbol? v) v (cadr v)))
                                  (caddr al)))
                           *autoloads*))
         (sym-alist (map (lambda (cnt sym)
                           (cons (string->symbol #`"sym,cnt") sym))
                         (iota (length syms)) syms))
         )
    (print "/* Generated automatically.  DO NOT EDIT */")
    (print "#include \"gauche.h\"")
    ;; declarations
    (dolist (sym sym-alist)
      (format #t "static SCM_DEFINE_STRING_CONST(~a_STR, ~s, ~a, ~:*~a);\n"
              (car sym) (x->string (cdr sym))
              (string-size (x->string (cdr sym)))))
    (dolist (mod mod-alist)
      (format #t "static SCM_DEFINE_STRING_CONST(~a_STR, ~s, ~a, ~:*~a);\n"
              (car mod) (x->string (cdr mod))
              (string-size (x->string (cdr mod))))
      (format #t "static ScmObj ~a = SCM_FALSE;\n" (car mod)))
    (dolist (str str-alist)
      (format #t "static SCM_DEFINE_STRING_CONST(~a, ~s, ~a, ~:*~a);\n"
              (car str) (cdr str) (string-size (cdr str))))
    ;; init
    (print "void Scm__InitAutoloads(void)")
    (print "{")
    (print "  ScmModule *scheme = Scm_SchemeModule();")
    (print "  ScmModule *gauche = Scm_GaucheModule();")
    (print "  ScmObj key_macro = SCM_MAKE_KEYWORD(\"macro\");")
    (print "  ScmSymbol *sym;")
    (print "  ScmObj al;")
    (print "  ScmString *path = NULL;")
    (print "  ScmSymbol *import_from = NULL;")
    (dolist (mod mod-alist)
      (format #t "  ~a = Scm_Intern(&~:*~a_STR);\n" (car mod)))
    (dolist (al (reverse *autoloads*))
      (let ((where (if (eq? (car al) 'scheme) 'scheme 'gauche))
            (path (if (string? (cadr al))
                    (format "&~a" (rassoc-ref str-alist (cadr al)))
                    (format "SCM_STRING(Scm_ModuleNameToPath(SCM_SYMBOL(~a)))"
                            (rassoc-ref mod-alist (cadr al)))))
            (from (if (string? (cadr al))
                    "NULL"
                    #`"SCM_SYMBOL(,(rassoc-ref mod-alist (cadr al)))"))
            )
        (format #t "  path = ~a;\n" path)
        (format #t "  import_from = ~a;\n" from)
        (dolist (ent (caddr al))
          (if (symbol? ent)
            (let ((str (rassoc-ref sym-alist ent)))
              (format #t "  sym = SCM_SYMBOL(Scm_Intern(&~a_STR));\n" str)
              (format #t "  al = Scm_MakeAutoload(sym, path, import_from);\n")
              (format #t "  Scm_Define(~a, sym, al);\n" where))
            (let ((str (rassoc-ref sym-alist (cadr ent))))
              (format #t "  sym = SCM_SYMBOL(Scm_Intern(&~a_STR));\n" str)
              (format #t "  al = Scm_MakeAutoload(sym, path, import_from);\n")
              (format #t "  Scm_Define(~a, sym, Scm_MakeMacroAutoload(sym, SCM_AUTOLOAD(al)));\n" where))))))
    (print "}")
    ))

(define (main args)
  (with-output-to-file "autoloads.c" emit)
  0)

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
          <arity-at-least> arity-at-least? arity-at-least-value)

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

