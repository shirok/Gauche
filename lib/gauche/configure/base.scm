;;;
;;; gauche.configure.base - configuring Gauche extensions
;;;
;;;   Copyright (c) 2013-2024  Shiro Kawai  <shiro@acm.org>
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

(define-module gauche.configure.base
  (use gauche.dictionary)
  (use gauche.logger)
  (use scheme.set)
  (use util.match)
  (export
   ;; Internal to gauche.configure.*
   <package> current-package ensure-package run-quietly arg-processors
   listify

   ;; External API commonly used from other gauche.configure.*
   cf-msg-checking cf-msg-result cf-msg-warn cf-msg-error cf-msg-notice
   cf-echo

   cf-define cf-defined?
   cf-subst cf-subst-prepend cf-subst-append cf-have-subst?
   cf-arg-var cf-ref cf$ with-cf-subst
   ))
(select-module gauche.configure.base)

;;;
;;; A package
;;;

(define-class <package> ()
  ((name       :init-keyword :name)
   (version    :init-keyword :version)
   (bug-report :init-keyword :bug-report :init-value #f) ; email addr
   (url        :init-keyword :url :init-value #f)
   (gpd        :init-keyword :gpd)       ; <gauche-package-description>
   (string     :init-keyword :string)    ; package_string
   (tarname    :init-keyword :tarname)
   (tool-prefix :init-form #f)           ; cross compilation tool prefix
   (config.h   :init-form '())           ; list of (config-header . source)
   (defs       :init-form (make-hash-table 'eq?)) ;cf-define'd thingy
   (substs     :init-form (make-hash-table 'eq?)) ;cf-subst'ed thingy
   (precious   :init-form (set eq-comparator))    ;vars overridable by env
   (features   :init-form (make-hash-table 'eq?)) ;enabled features by cmdarg
   (packages   :init-form (make-hash-table 'eq?)) ;optional packages
   ))

;;;
;;; Internal parameters & apis
;;;

(define current-package (make-parameter #f))

(define (ensure-package)
  (or (current-package)
      (error "No current package - cf-init hasn't been called")))

(define run-quietly (make-parameter #f)) ;-silent or -quiet option turn this on

;; Alist of arg processors and help strings given to cf-arg-with and
;; cf-arg-enable.
;; Each element is
;; (<name> <kind> <help-string> <proc-if-given> <proc-if-not-given>)
(define arg-processors (make-parameter '()))

(define (listify x) (if (list? x) x (list x)))

;;;
;;; Basic APIs
;;;

(define (tee-msg console-fmt log-fmt args)
  (apply format #t console-fmt args)
  (apply log-format log-fmt args))

;; API
;; Like AC_MSG_*
(define (cf-msg-checking fmt . args)
  (tee-msg #"checking ~|fmt|... " #"checking: ~fmt" args))
(define (cf-msg-result fmt . args)
  (tee-msg #"~|fmt|\n" #"result: ~fmt" args))
(define (cf-msg-warn fmt . args)
  (tee-msg #"Warning: ~|fmt|\n" #"Warning: ~fmt" args))
(define (cf-msg-error fmt . args)
  (tee-msg #"Error: ~|fmt|\n" #"Error: ~fmt" args)
  (exit 1))
(define (cf-msg-notice fmt . args)
  (tee-msg #"~|fmt|\n" #"~|fmt|\n" args))

;; API
;; Convenience routine for substitute of shell's echo
;; e.g.  (cf-echo "something" > "FILE")
;; or    (cf-echo "something" >> "FILE")
;; The destination, '>' or '>>' followed by a filename, must be at the end
;; of arglist if any.  If no destination is given, output goes to the current
;; output port.
(define-macro (cf-echo . args)
  (match (take-right* args 2)
    [('> name)
     `(with-output-to-file ,name
        (cut print ,@(intersperse " " (drop-right* args 2)))
        :if-exists :supersede)]
    [('>> name)
     `(with-output-to-file ,name
        (cut print ,@(intersperse " " (drop-right* args 2)))
        :if-exists :append)]
    [_ `(,tee-msg "~a\n" "Message: ~a" (list (string-join (list ,@args))))]))

;;;
;;; Variables and substitutions
;;;

;; API
(define (cf-define symbol :optional (value 1))
  (assume-type symbol <symbol>)
  (dict-put! (~ (ensure-package)'defs) symbol value))

;; API
(define (cf-defined? symbol)
  (assume-type symbol <symbol>)
  (dict-exists? (~ (ensure-package)'defs) symbol))

;; API
;; Like AC_SUBST, but we require value (instead of implicitly referencing
;; a global variable.
(define (cf-subst symbol :optional (value #f))
  (assume-type symbol <symbol>)
  (if value
    (dict-put! (~ (ensure-package)'substs) symbol value)
    (unless (cf-have-subst? symbol)
      (dict-put! (~ (ensure-package)'substs) symbol ""))))

;; API
(define (cf-subst-prepend symbol value :optional (delim " ") (default ""))
  (let1 v (cf-ref symbol default)
    (if (equal? v "")
      (cf-subst symbol value)
      (cf-subst symbol #"~|value|~|delim|~|v|"))))

;; API
(define (cf-subst-append symbol value :optional (delim " ") (default ""))
  (let1 v (cf-ref symbol default)
    (if (equal? v "")
      (cf-subst symbol value)
      (cf-subst symbol #"~|v|~|delim|~|value|"))))

;; API
(define (cf-have-subst? symbol)
  (assume-type symbol <symbol>)
  (dict-exists? (~ (ensure-package)'substs) symbol))

;; API
;; Make the named variable overridable by the environment variable.
;; The term "precious" comes from autoconf implementation.
;; At this moment, we don't save the help string.  cf-arg-var is
;; called after cf-init and we can't include help message (the limitation
;; of one-pass processing.)
(define (cf-arg-var symbol)
  (assume-type symbol <symbol>)
  (update! (~ (ensure-package)'precious) (cut set-adjoin! <> symbol))
  (if-let1 v (sys-getenv (x->string symbol))
    (cf-subst symbol v)
    (cf-subst symbol)))

(define (var-precious? symbol)
  (assume-type symbol <symbol>)
  (set-contains? (~ (ensure-package)'precious) symbol))

;; API
;; Lookup the current value of the given variable.
(define (cf-ref symbol :optional (default (undefined)))
  (assume-type symbol <symbol>)
  (rlet1 v (dict-get (~ (ensure-package)'substs) symbol default)
    (when (undefined? v)
      (errorf "Configure variable ~s is not defined." symbol))))

;; API
;; Like cf-ref, but returns empty string if undefined.
(define (cf$ symbol) (cf-ref symbol ""))

;; API
;; Temporarily replace cf subst value
(define-syntax with-cf-subst
  (syntax-rules ()
    [(_ binds body ...)
     (%with-cf-subst binds () () (body ...))]))

(define-syntax %with-cf-subst
  (syntax-rules (+)
    [(_ () vars setters bodies)
     (let ([saves (map (cut cf$ <>) 'vars)])
       (unwind-protect
           (begin (begin . setters) . bodies)
         (for-each (cut cf-subst <> <>) 'vars saves)))]
    [(_ ((var val) . binds) (vars ...) (setters ...) bodies)
     (%with-cf-subst binds
                     (vars ... var)
                     (setters ... (cf-subst 'var val))
                     bodies)]
    [(_ ((var + val) . binds) (vars ...) (setters ...) bodies)
     (%with-cf-subst binds
                     (vars ... var)
                     (setters ... (cf-subst-append 'var val))
                     bodies)]
    [(_ ((var val +) . binds) (vars ...) (setters ...) bodies)
     (%with-cf-subst binds
                     (vars ... var)
                     (setters ... (cf-subst-prepend 'var val))
                     bodies)]
    [(_ binds _ _ bodies)
     (error "Malformed with-cf-subst:"
            '(with-cf-subst binds . bodies))]))
