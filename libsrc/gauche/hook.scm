;;;
;;; hook.scm - hook procedures
;;;
;;;   Copyright (c) 2000-2019  Shiro Kawai  <shiro@acm.org>
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

;; The API is upper-comaptible of Guile's.   The differences are:
;;
;;  * Based on the object system.
;;  * Hook object itself is applicable.
;;  * delete-hook! is defined as an alias of remove-hook!.
;;    The use of delete is consistent to the SRFI-1 and others.

(define-module gauche.hook
  (use gauche.mop.validator)
  (use srfi-1)
  (export <hook> make-hook hook? hook-empty? add-hook!
          remove-hook! delete-hook! reset-hook!
          hook->list run-hook)
  )

(select-module gauche.hook)

(define-class <hook> ()
  ((procedures :init-keyword :procedures :init-value '())
   (arity      :init-keyword :arity :init-value 0
               :validator (lambda (o v)
                            (unless (and (integer? v) (>= v 0))
                              (errorf "invalid arity ~s: must a non-negative integer" v))
                            v))
   ))

;; make-hook [arity]
(define (make-hook :optional (arity 0))
  (make <hook> :arity arity))

(define (hook? obj) (is-a? obj <hook>))

(define-method hook-empty? ((hook <hook>))
  (null? (ref hook 'procedures)))

(define-method add-hook! ((hook <hook>) proc :optional (append? #f))
  (unless (procedure-arity-includes? proc (ref hook 'arity))
    (errorf "can't add hook ~s: arity is incompatible with expected ~a"
            proc (ref hook 'arity)))
  (unless (memq proc (ref hook 'procedures))
    (if append?
        (update! (ref hook 'procedures) (cut append <> (list proc)))
        (slot-push! hook 'procedures proc)))
  (values))

(define-method delete-hook! ((hook <hook>) proc)
  (update! (ref hook 'procedures) (cut delete proc <> eq?))
  (values))

(define remove-hook! delete-hook!)

(define-method reset-hook! ((hook <hook>))
  (set! (ref hook 'procedures) '())
  (values))

(define-method hook->list ((hook <hook>))
  (list-copy (ref hook 'procedures)))

(define-method run-hook ((hook <hook>) . args)
  (unless (= (length args) (ref hook 'arity))
    (errorf "run-hook expects ~a arg(s), but got: ~s"
            (ref hook 'arity) args))
  (for-each (cut apply <> args) (ref hook 'procedures))
  (values))

;; make hook applicable.
(define-method object-apply ((hook <hook>) . args)
  (apply run-hook hook args))

