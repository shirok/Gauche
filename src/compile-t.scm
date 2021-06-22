;;;
;;; compile-t.scm - Type handling during compilation
;;;
;;;   Copyright (c) 2021  Shiro Kawai  <shiro@acm.org>
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

;; Descriptive types should be created during compilation.

;; Called from pass1/global-call, when we detect (<type-ctor> arg ...)
;; CTOR is the gloval value of type constructor,  IFORM is the $CALL node
;; represents the ctor invocation.
;; Since arguments for the type constructor have already gone through pass1,
;; constant variable reference and type constructor calls are already
;; handled.
(define (type/construct ctor iform cenv)
  (define (get-arg-value arg)
    (cond [($const? arg) ($const-value arg)]
          [(has-tag? arg $GREF)
           (if-let1 gloc (gref-inlinable-gloc arg)
             ;; (let1 v (gloc-ref gloc)
             ;;   (if (and (is-a? v <class>)
             ;;            (not (is-a? v <descriptive-type>)))
             ;;     (%wrap-with-proxy-type ($gref-id arg) gloc)
             ;;     v))
             (gloc-ref gloc)
             (errorf "Can't use non-inlinable global varible `~s' in \
                      type constructor expression: ~s"
                     (identifier-name ($gref-id arg))
                     ($*-src iform)))]
          [else
           ;; we can run constant folding here, but for the time being...
           (error "Arguments of type constructor expression must be \
                   a compile-time constant:" ($*-src iform))]))
  (define (check-arg-value val)
    ;; For now, we restrict type ctor arguments to simple values
    (unless (or (is-a? val <type>)
                (number? val)
                (boolean? val)
                (string? val)
                (symbol? val))
      (error "Invalid value as type constructor argument:" val))
    val)

  ;; Call type constructor
  (let1 type
      (apply (~ ctor'constructor)
             (map ($ check-arg-value $ get-arg-value $) ($call-args iform)))
    (unless (is-a? type <descriptive-type>)
      (errorf "Type costructor ~s returned an object other than a \
               type instance: ~s"
              ($*-src iform) type))
    ($const type)))


;; When generative types appear in the compile-time type expression, we
;; wrap them with proxy type, for they can be redefined.  This wrapping
;; must be done in the compiler.

(define-cproc %wrap-with-proxy-type (id gloc)
  (unless (SCM_IDENTIFIERP id)
    (SCM_TYPE_ERROR id "identifier"))
  (unless (SCM_GLOCP gloc)
    (SCM_TYPE_ERROR gloc "gloc"))
  (return (Scm_MakeProxyType (SCM_IDENTIFIER id) (SCM_GLOC gloc))))
