;;;
;;; gauche/defvalues.scm - define-values and set!-values, to be autoloaded
;;;
;;;   Copyright (c) 2000-2013  Shiro Kawai  <shiro@acm.org>
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

;; ChezScheme and MzScheme's define-values and set!-values.
;; To be autoloaded

(define-module gauche.defvalues
  (export define-values set!-values))
(select-module gauche.defvalues)

;; define-values
(define-syntax define-values
  (syntax-rules ()
    ((_ (var  ...) expr)
     (define-values-sub () (var ...) (var ...) expr))
    ((_ . else)
     (syntax-error "malformed define-values" (define-values . else)))
    ))

(define-syntax define-values-sub
  (syntax-rules ()
    ((_ (tmp ...) () (var ...) expr)
     (begin (define var (undefined)) ...
            (receive (tmp ...) expr
              (set! var tmp) ...
              (undefined))))
    ((_ (tmp ...) (v v2 ...) (var ...) expr)
     (define-values-sub (tmp ... tmp1) (v2 ...) (var ...) expr))
    ))

;; set!-values
(define-syntax set!-values
  (syntax-rules ()
    ((_ (var ...) expr)
     (set!-values-sub () (var ...) (var ...) expr))
    ((_ . else)
     (syntax-error "malformed set!-values" (set!-values . else)))
    ))

(define-syntax set!-values-sub
  (syntax-rules ()
    ((_ (tmp ...) () (var ...) expr)
     (receive (tmp ...) expr
       (set! var tmp) ...
       (undefined)))
    ((_ (tmp ...) (v v2 ...) (var ...) expr)
     (set!-values-sub (tmp ... tmp1) (v2 ...) (var ...) expr))
    ))


