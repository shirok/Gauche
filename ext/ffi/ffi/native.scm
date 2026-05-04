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

;; (with-native-ffi dlo-expr options cdef-list-expr cdef-names forms)
;;
;; cdef-list-expr is a form that evaluates to a list of <foreign-c-function>
;; instances.  cdef-names is a literal list of the corresponding Scheme names
;; (known at expansion time).
;;
;; The provisional (define name #f) bindings have already been emitted by
;; with-ffi at begin level.  Body forms are also at begin level.  This macro
;; only handles evaluating the cfn list and set!ing each name.
(define-syntax with-native-ffi
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ dlo-var dlo-expr options cdef-specs forms)
        (quasirename r
          `(begin
             ,@(map (^[spec] (quasirename r
                               `(define ,(car spec))))
                    cdef-specs)
             ,@forms
             (define _dummy
               (let ([,dlo-var ,dlo-expr])
                 ,@(map (^[spec]
                          (quasirename r
                            `(set! ,(car spec)
                                   (make-native-ffi-proc ,dlo-var ,(cdr spec)))))
                        cdef-specs)))
             ))]))))

;;;
;;; Type canonicalization
;;;

;; Convert a <native-type> instance to the call-amd64 canonical type:
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
         (is-a? type <c-array>)
         (is-a? type <c-function>)) type]
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
  (if (eq? type <c-char>) integer->char values))

;;;
;;; Runtime procedure builder
;;;

;; Build a Scheme procedure that calls the C function described by cfn via
;; call-amd64.  Called at load time from the set! forms emitted by
;; with-native-ffi.
(define (make-native-ffi-proc dlo cfn)
  (let* ([native-call (cond-expand
                       [x86_64
                        (use lang.asm.x86_64)
                        (cond-expand
                         [gauche.os.windows
                          (with-module gauche.internal call-winx64)]
                         [else
                          (with-module gauche.internal call-amd64)])]
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
