;;;
;;; gauche.cgen.tmodule - Transient module
;;;
;;;   Copyright (c) 2011-2019  Shiro Kawai  <shiro@acm.org>
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

;; This is a private module used internally in cgen.  Not for general use.

;; The precompiler and genstub creates a temporary module in order to
;; keep track of the module where the target code runs; they shouldn't
;; be mixed with the module in which the precompiler or stub generator
;; itself is running.  Otherwise the compiled code may interfere with
;; the compiler unexpectedly.

(define-module gauche.cgen.tmodule
  (use gauche.cgen.unit)
  (use gauche.cgen.literal)
  (use gauche.parameter)
  (export <tmodule> current-tmodule-class
          current-tmodule tmodule-cname current-tmodule-cname
          all-tmodules find-tmodule select-tmodule
          with-tmodule with-tmodule-recording)
  )
(select-module gauche.cgen.tmodule)

;; Base class
(define-class <tmodule> ()
  ([name    :init-keyword :name]
   [module  :init-form (make-module #f)]
   [cname   :init-keyword :cname
            :init-value #f]   ; C variable name holding the module in init fn
                              ;  initialized on-demand
   ))

(define-method initialize ((m <tmodule>) initargs)
  (next-method)
  (push! (%all-tmodules) m))

(define (tmodule-cname m)
  (or (~ m'cname)
      (rlet1 cname (cgen-allocate-static-datum)
        (set! (~ m'cname) cname)
        (let1 n (cgen-literal (~ m'name))
          (cgen-init (format "  ~a = SCM_OBJ(Scm_FindModule(SCM_SYMBOL(~a), \
                                                 SCM_FIND_MODULE_CREATE)); \
                                                 /* module ~a */"
                             cname (cgen-c-name n)
                             (cgen-safe-comment (~ m'name))))))))

;; we hide the parameter
(define %current-tmodule (make-parameter #f))

(define (current-tmodule) (%current-tmodule)) ;read-only api

;; record the tmodules created during a session.
(define %all-tmodules (make-parameter '()))

(define (all-tmodules) (%all-tmodules))

;; whenever current-tmodule is switched, we need to emit a piece
;; of code in init function.
(define (%emit-select-module tm)
  (when tm
    (cgen-init (format "   Scm_SelectModule(SCM_MODULE(~a));"
                       (tmodule-cname tm)))))

;; An application may wish to use derived class.  find-tmodule and
;; select-tmodule will create a new tmodule implicitly, using this
;; class.
(define current-tmodule-class (make-parameter <tmodule>))

(define (current-tmodule-cname)
  (cond [(current-tmodule) => tmodule-cname]
        [else "mod"])) ; for the backward compatibility of genstub

(define (find-tmodule name)
  (or (find (^m (eq? name (~ m'name))) (all-tmodules))
      (make (current-tmodule-class) :name name)))

(define (select-tmodule name)
  (let1 cm (find-tmodule name)
    (%emit-select-module cm)
    (%current-tmodule cm)))

(define-syntax with-tmodule
  (syntax-rules ()
    [(_ name . body)
     (let ([prev-tm (%current-tmodule)]
           [new-tm  (find-tmodule name)])
       (dynamic-wind
           (^[] (%emit-select-module new-tm))
           (^[] (parameterize ((%current-tmodule new-tm)) . body))
           (^[] (%emit-select-module prev-tm))))]))

(define-syntax with-tmodule-recording
  (syntax-rules ()
    [(_ tmodclass . body)
     (parameterize ([current-tmodule-class tmodclass]
                    [%all-tmodules '()])
       (%setup-builtin-tmodules)
       . body)]))

;; NB: We pre-allocate some built-in modules.  This saves redundant
;; initialization at the runtime for precompiled sources of Gauche core.
(define (%setup-builtin-tmodules)
  (define (builtin name c-api)
    (make (current-tmodule-class) :name name :cname #"SCM_OBJ(~c-api())"))
  (builtin 'null            "Scm_NullModule")
  (builtin 'scheme          "Scm_SchemeModule")
  (builtin 'gauche          "Scm_GaucheModule")
  (builtin 'gauche.internal "Scm_GaucheInternalModule")
  )

