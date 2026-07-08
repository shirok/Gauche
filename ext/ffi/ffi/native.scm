;;;
;;; gauche.ffi.native - Native FFI interface
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

;; Experimental.  Only available on selected platforms (OS/CPU)

(define-module gauche.ffi.native
  (use gauche.ffi)
  (use gauche.native-type)
  (use util.match)
  (export with-native-ffi))
(select-module gauche.ffi.native)

(define %procedure-copy (with-module gauche.internal %procedure-copy))

;;;
;;; Main macro
;;;

;; (with-native-ffi dlo-var dlo-expr options cdef-specs ccb-info forms)
;;
;; cdef-specs is ((name . expr) ...) where expr evaluates to a
;; <foreign-c-function> or <foreign-c-callback>.  ccb-info is a literal
;; list ((ccb-name body-name-id) ...), one entry per callback in
;; declaration order; body-name-id is the identifier of the lambda
;; defined by with-ffi for that callback's body.
;;
;; The provisional (define name) bindings and the body lambdas are
;; emitted as begin-level forms.  All callbacks are batched into a
;; single ScmFFICallbackContext which is anchored in a hidden
;; module-level binding so the foreign side's invisible references
;; cannot dangle.
(define-syntax with-native-ffi
  (er-macro-transformer
   (^[f r c]
     ;; Kludge.  We need to ensure gauche.ffi.native is loaded
     ;; when precompiled ffi code is run.
     ;; https://github.com/shirok/Gauche/issues/1293
     (define %require. ((with-module gauche.internal make-identifier)
                        '%require (find-module 'gauche.internal) '()))
     (match f
       [(_ dlo-var dlo-expr options cdef-specs ccb-info forms)
        (let* ([ccb-name-set (map car ccb-info)]
               [cfn-specs (filter (^s (not (memq (car s) ccb-name-set)))
                                  cdef-specs)]
               [ccb-specs (filter (^s (memq (car s) ccb-name-set))
                                  cdef-specs)]
               [ctx-var   (gensym "%ffi-native-ctx-")]
               [instances-var (gensym "%ccbs-")])
          (define (emit-cfn-set! spec)
            (quasirename r
              `(set! ,(car spec)
                     (make-native-ffi-proc ,dlo-var ,(cdr spec)))))
          (define (emit-ccb-block)
            (if (null? ccb-specs)
              #f
              (quasirename r
                `(let* ([,instances-var (list ,@(map cdr ccb-specs))]
                        [bodies         (list ,@(map (^[name]
                                                       (cadr (assq name ccb-info)))
                                                     (map car ccb-specs)))]
                        [ctx (install-callback-context! ,instances-var bodies)])
                   ,@(map (^[i s]
                            (quasirename r
                              `(set! ,(car s)
                                     (callback-context-handle
                                      ctx ,i (list-ref ,instances-var ,i)))))
                          (iota (length ccb-specs)) ccb-specs)
                   ctx))))
          (quasirename r
            `(begin
               (,%require. "gauche/ffi/native")
               ,@(map (^s (quasirename r `(define ,(car s)))) cdef-specs)
               ,@forms
               (define ,ctx-var
                 (let ([,dlo-var ,dlo-expr])
                   ,@(map emit-cfn-set! cfn-specs)
                   ,(emit-ccb-block))))))]))))

;;;
;;; Type canonicalization
;;;

;; Convert a <native-type> instance to the call-sysvx64 canonical type:
;;   <void>      - void (return only)
;;   <double>    - double (xmm register, 64-bit float)
;;   <float>     - float  (xmm register, 32-bit float)
;;   <c-string>  - C string (Scheme string passed as const char*)
;;   <void*>     - raw pointer (native-handle or foreign-pointer)
;;   <uintptr_t> - unsigned integer (unsigned integral types)
;;   <intptr_t>  - signed integer (all other integral types)
(define (native-type->call-canon type)
  (cond
    [(or (is-a? type <c-pointer>)
         (is-a? type <c-function>)) type]
    ;; An array argument is treated the same way as a pointer.
    [(is-a? type <c-array>)
     (make-c-pointer-type (~ type 'element-type))]
    [(or (is-a? type <c-struct>)
         (is-a? type <c-union>))
     (error "Directly passing struct or union isn't supported yet")]
    [(is-a? type <native-type>)
     (cond
      [(eq? type <void>)             <void>]
      [(eq? type <double>)           <double>]
      [(eq? type <float>)            <float>]
      [(eq? type <c-string>)         <c-string>]
      [(~ type 'unsigned?)           <uintptr_t>]
      [else                          <intptr_t>])]
    [(eq? type <top>)                <top>]
    [else (error "Invalid type for native call:" type)]))

;; <c-char> object needs to be passed as integer
(define (native-type->arg-coerce type)
  (if (eq? type <c-char>) char->integer values))

(define (native-type->return-coerce type)
  (cond [(eq? type <c-char>) integer->char]
        [(and (subtype? type <integer>)
              (not (~ type 'unsigned?)))
         (%sign-extender (~ type 'size))]
        [else values]))

(define (%sign-extender bytes)
  (ecase bytes
    [(1) (^v (if (< v 128)
               v
               (logior v (lognot #xff))))]
    [(2) (^v (if (< v 32768)
               v
               (logior v (lognot #xffff))))]
    [(4) (^v (if (< v #x8000_0000)
               v
               (logior v (lognot #xffff_ffff))))]
    [(8) (^v (if (< v #x8000_0000_0000_0000)
               v
               (logior v (lognot #xffff_ffff_ffff_ffff))))]))

;;;
;;; Runtime procedure builder
;;;

;; Build a Scheme procedure that calls the C function described by cfn via
;; call-sysvx64.  Called at load time from the set! forms emitted by
;; with-native-ffi.
(define-method make-native-ffi-proc (dlo (cfn <foreign-c-function>))
  (let* ([native-call (cond-expand
                       [x86_64
                        (use lang.asm.x86_64)
                        (cond-expand
                         [gauche.os.windows
                          (with-module gauche.internal call-winx64)]
                         [else
                          (with-module gauche.internal call-sysvx64)])]
                       [else
                        (error "Native FFI is not supported on this platform")])]
         [ptr        (dlobj-get-entry-address dlo #"_~(~ cfn'c-name)")]
         [ret-type   (~ cfn'return-type)]
         [arg-types  (~ cfn'arg-types)]
         [variadic?  (~ cfn'variadic?)]
         [tag-info   (~ cfn'tag-info)]
         [nfixed     (length arg-types)]
         [ret-canon  (native-type->call-canon ret-type)]
         [arg-canons (map native-type->call-canon arg-types)]
         [arg-coerce (map native-type->arg-coerce arg-types)]
         [ret-coerce (native-type->return-coerce ret-type)])
    (unless ptr
      (error "FFI (native): cannot find function in library:"
             (~ cfn'scheme-name)))
    (let1 raw-proc
        (if variadic?
          ;; For variadic functions, map fixed args with known types, then infer
          ;; types for the remaining variadic args from their runtime values.
          ;; NB: For now, we just distinguish flonum values form the rest in
          ;; the varargs list.  Eventually we want to check if all the varargs
          ;; can be convertable to C objects.
          (^ args
             (let* ([fixed-args (take args nfixed)]
                    [var-args   (drop args nfixed)]
                    [fixed-pairs (map (^[canon coerce val] (list canon (coerce val)))
                                      arg-canons arg-coerce fixed-args)]
                    [var-pairs   (map (^[val]
                                        `(,(if (flonum? val) <double> <intptr_t>)
                                          ,val))
                                      var-args)])
               (parameterize
                   ([(with-module gauche.typeutil native-ptr-fill-enabled?) #t])
                 (ret-coerce
                  (native-call
                   ptr
                   (append fixed-pairs var-pairs)
                   ret-canon)))))
          ;; Non-variadic: map all args with their declared types.
          (^ args
            (parameterize
                ([(with-module gauche.typeutil native-ptr-fill-enabled?) #t])
              (ret-coerce
               (native-call
                ptr
                (map (^[canon coerce val] (list canon (coerce val)))
                     arg-canons arg-coerce args)
                ret-canon)))))
      (%procedure-copy raw-proc tag-info))))

;; Callbacks no longer flow through make-native-ffi-proc; with-native-ffi
;; batches them into a single ScmFFICallbackContext.  See the helpers
;; below.

;;;
;;; Callback context construction
;;;
;;; with-native-ffi emits code that calls install-callback-context! once
;;; per with-ffi block to install all of that block's callbacks in a
;;; single mmapped region, then calls callback-context-handle per
;;; callback to wrap each entry as a <native-handle>.
;;;
;;; The context returned by install-callback-context! is held by a
;;; hidden module-level binding; this is what keeps the mapped pages
;;; alive for the program lifetime, even though foreign code may have
;;; recorded the trampoline addresses where the GC cannot see them.

(define %%install-ffi-callback-context
  (with-module gauche.internal %%install-ffi-callback-context))
(define %%ffi-callback-context-entry
  (with-module gauche.internal %%ffi-callback-context-entry))

;; Platform-dispatched entry into the trampoline assembler.  Takes the
;; body procedure plus canonical arg/ret types and returns four values:
;;   (values code-bytes entry-offset win-prolog-end win-frame-size)
(define assemble-callback
  (cond-expand
   [x86_64
    (cond-expand
     [gauche.os.windows
      (with-module gauche.internal assemble-callback-winx64)]
     [else
      (with-module gauche.internal assemble-callback-sysvx64)])]
   [else
    (^ _ (error "Native FFI callbacks not supported on this platform"))]))

(define (install-callback-context! ccb-instances body-procs)
  (let1 specs
      (map (^[ccb body]
             (let* ([arg-canons (map native-type->call-canon
                                     (~ ccb'arg-types))]
                    [ret-canon  (native-type->call-canon
                                 (~ ccb'return-type))])
               (receive (code entry win-pe win-fs)
                   (assemble-callback body arg-canons ret-canon)
                 (list code entry win-pe win-fs))))
           ccb-instances body-procs)
    (%%install-ffi-callback-context specs)))

(define (callback-context-handle ctx i ccb)
  (%%ffi-callback-context-entry
   ctx i (make-c-function-type (~ ccb'return-type) (~ ccb'arg-types))))
