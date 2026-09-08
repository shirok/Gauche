;;;
;;; gauche.ffi.stub - FFI subsystem via runtime stub generation
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

;; The `:stub` FFI subsystem: at macro-expansion time we generate
;; C code, then compile and link it.  The expanded code also contains
;; define-type of enum declarations, if any.
;;
;; The actual code generation is handled by gauche.ffi.stubgen module.

(define-module gauche.ffi.stub
  (use gauche.ffi)
  (use gauche.ffi.stubgen)
  (use gauche.cgen.dyncomp)
  (use gauche.native-type)
  (use util.match)
  (export with-stub-ffi))
(select-module gauche.ffi.stub)

(define-syntax with-stub-ffi
  (er-macro-transformer
   (^[f r c]
     ;; Kludge.  We need to ensure gauche.ffi.stub is loaded
     ;; when precompiled ffi code is run.
     ;; https://github.com/shirok/Gauche/issues/1293
     (define %require. ((with-module gauche.internal make-identifier)
                        '%require (find-module 'gauche.internal) '()))
     (match f
       [(_ dlo-var dlo-expr options cdef-specs cenum-names forms)
        (let1 cdef-list-expr
            (quasirename r
              `(list ,@(map cdr cdef-specs)))
          (quasirename r
            `(begin
               (,%require. "gauche/ffi/stub")
               ,@forms
               ;; We insert dummy binding so that expansion contanis
               ;; only definitions.
               (define ,dlo-var ,dlo-expr)
               (define _dummy
                 (compile-and-link-ffi-stub ,dlo-var
                                            ,cdef-list-expr
                                            ',(get-keyword :c-headers options '())
                                            ',(get-keyword :c-include-paths
                                                           options '())
                                            (current-module)))
               ;; compile-and-link-ffi-stub returns the <c-enum> instances
               ;; it reified, in declaration order.
               ,@(map (^[name i]
                        (quasirename r
                          `(define-type ,name (list-ref _dummy ,i))))
                      cenum-names
                      (iota (length cenum-names)))
               )))]))))

(define (compile-and-link-ffi-stub dlobj cdef-instances c-headers
                                  c-include-paths mod)
  (let1 unit (generate-ffi-c-code-unit cdef-instances c-headers)
    (receive (pointer-ret-types variadic-type-infos callback-infos)
        (ffi-setup-arguments cdef-instances)
      (cgen-dynamic-load unit :include-paths c-include-paths)
      (ffi-reify-enums cdef-instances
                       ((module-binding-ref mod 'ffisetup)
                        dlobj pointer-ret-types variadic-type-infos
                        callback-infos mod)))))
