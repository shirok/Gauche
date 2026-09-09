;;;
;;; gauche.ffi.aot - AOT subsystem via runtime stub generation
;;;
;;;   Copyright (c) 2026  Shiro Kawai  <shiro@acm.org>
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

;; The `:aot` FFI subsystem: the with-ffi macro directly expands
;; to the stub code, to be processed by precomp.
;;
;; The actual code generation is handled by gauche.ffi.stubgen module.

(define-module gauche.ffi.aot
  (use gauche.ffi)
  (use gauche.native-type)
  (use gauche.cgen.unit)
  (use util.match)
  (export with-aot-ffi))
(select-module gauche.ffi.aot)

;; The code generator is only needed while a source is being precompiled,
;; so we autoload it; at runtime, it won't be loaded.
(autoload gauche.ffi.stubgen generate-ffi-c-code-unit)

;; Kludge: Access to precomp's current-tmodule during compiling.
(define (%current-tmodule)
  (and-let* ([m (find-module 'gauche.cgen.tmodule)]
             [p (global-variable-ref m 'current-tmodule #f)])
    (p)))

;; Evaluate the cdef expressions at macro-expansion time, in order to
;; obtain <foreign-c-*> instances here, not at runtime, since the C code is
;; generated now.  They are evaluated in the module precomp is compiling
;; into, so a typespec may mention anything visible there---including a
;; define-type of the same file, which precomp resolves for us.
;; DLO-VAR isn't bound yet, so we bind it to #f; the only thing that reads
;; it is the :dlobj entry of the tag info, which tolerates #f.  That tag info
;; is only used for code generation---the tags the functions actually carry
;; come from the instances %ffi-aot-setup builds at load time, when the dlobj
;; does exist.
(define (%eval-cdef-specs cdef-specs dlo-var mod)
  (map (^[spec] (eval `(let ((,dlo-var #f)) ,(cdr spec)) mod)) cdef-specs))

(define-syntax with-aot-ffi
  (er-macro-transformer
   (^[f r c]
     ;; Kludge, make sure gauche.ffi.aot is loaded when the precompiled
     ;; code runs.
     (define %require. ((with-module gauche.internal make-identifier)
                        '%require (find-module 'gauche.internal) '()))
     (match f
       [(_ dlo-var dlo-expr options cdef-specs cenum-names forms)
        (let1 tm (%current-tmodule)
          (unless tm
            (error "The FFI :aot subsystem can only be used in a source that \
                    is precompiled; use the :stub subsystem instead."))
          (when (pair? (get-keyword :c-include-paths options '()))
            (warn "FFI :aot subsystem ignores :c-include-paths; put the \
                   include paths in the CFLAGS of the build instead.\n"))
          (let* ([tag       (symbol->string (gensym "ffiaot"))]
                 [setup-sym (string->symbol #"%ffi-aot-setup-~tag")]
                 [cdefs     (%eval-cdef-specs cdef-specs dlo-var
                                              (~ tm'module))]
                 ;; A variadic call with float arguments builds a sub-stub
                 ;; at call time, and the generated dispatch code reaches into
                 ;; gauche.ffi.stubgen to do it.  Nothing else refers to that
                 ;; module at runtime, so we have to require it explicitly.
                 [variadic?  (any (^d (and (is-a? d <foreign-c-function>)
                                           (~ d'variadic?)))
                                  cdefs)]
                 [unit      (generate-ffi-c-code-unit
                             cdefs
                             (get-keyword :c-headers options '())
                             :c-name-prefix #"~|tag|_"
                             :setup-scm-name (symbol->string setup-sym))]
                 ;; The sections have to keep the order cgen-emit-c uses:
                 ;; the body refers to the static data and the decls.
                 ;; declcode and initcode place their argument in the decl
                 ;; and init sections; a bare string goes to the body.
                 [stub-form
                  ;; NB: built with a plain quasiquote, so that inline-stub
                  ;; goes in as a bare symbol.  precomp installs its handler
                  ;; for it in the module being compiled; a renamed
                  ;; identifier would resolve to the gauche one instead,
                  ;; which merely warns and discards the code.
                  `(inline-stub
                    (declcode ,(cgen-emit-part->string unit 'decl))
                    (declcode ,(cgen-emit-part->string unit 'static-data))
                    ,(cgen-emit-part->string unit 'body)
                    (initcode ,(cgen-emit-part->string unit 'init)))])
            (quasirename r
              `(begin
                 (,%require. "gauche/ffi/aot")
                 ,@(if variadic?
                     (list (quasirename r `(,%require. "gauche/ffi/stubgen")))
                     '())
                 ,@forms
                 ;; The dlobj is a runtime value, so the setup call has to be
                 ;; a toplevel form.  That is fine: precomp emits the init
                 ;; sections before Scm_VMExecuteToplevels, so the setup
                 ;; procedure the initcode binds is already there.
                 (define ,dlo-var ,dlo-expr)
                 ,stub-form
                 ;; We insert dummy binding so that expansion contains
                 ;; only definitions.
                 (define _dummy
                   (%ffi-aot-setup ',setup-sym
                                   ,dlo-var
                                   (list ,@(map cdr cdef-specs))
                                   ;; raw symbol: precomp rewrites this to
                                   ;; (find-module '<the module>)
                                   ,'(current-module)))
                 ;; %ffi-aot-setup returns the <c-enum> instances it
                 ;; reified, in declaration order.
                 ,@(map (^[name i]
                          (quasirename r
                            `(define-type ,name (list-ref _dummy ,i))))
                        cenum-names
                        (iota (length cenum-names)))))))]
       [_ (error "Malformed with-aot-ffi form:" f)]))))

;; Runtime entry point, called from the expansion above.  The C code is
;; already compiled into this module; all that is left is what the :stub
;; subsystem does after loading its DSO---hand ffisetup the values that are
;; only known now, and reify the enums it returns.
;;
;; SETUP-SYM names the setup procedure the unit's init code bound in MOD.
;; CDEF-INSTANCES are built here, at runtime, so their tag info carries the
;; values the expansion-time instances used for code generation couldn't
;; know---in particular the dlobj path.
(define (%ffi-aot-setup setup-sym dlobj cdef-instances mod)
  (receive (pointer-ret-types variadic-type-infos callback-infos fn-tag-infos)
      (ffi-setup-arguments cdef-instances)
    (ffi-reify-enums cdef-instances
                     ((module-binding-ref mod setup-sym)
                      dlobj pointer-ret-types variadic-type-infos
                      callback-infos mod fn-tag-infos))))
