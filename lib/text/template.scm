;;;
;;; text.template - simple text templating
;;;
;;;   Copyright (c) 2016-2017  Shiro Kawai  <shiro@acm.org>
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

;; This module just uses string-interpolate feature to apply
;; runtime-provided text (template).  Most of the code is for
;; easy-to-use environment manipulation.

(define-module text.template
  (use gauche.dictionary)
  (use file.util)
  (export make-template-environment
          expand-template-string
          expand-template-file))
(select-module text.template)

(define-class <template-environment> ()
  (;; all slots are private
   (module  :init-keyword :module)))

(define (make-template-environment :key
                                   (extends '(gauche))
                                   (imports '())
                                   (bindings #f))
  (make <template-environment>
    :module (setup-template-environment extends imports bindings)))

(define (setup-template-environment extends imports bindings)
  (rlet1 m (make-module #f)
    (dolist [i imports]
      (if (list? i)
        (eval `(import ,@i) m)
        (eval `(import ,i) m)))
    (dict-for-each
     bindings
     (^[k v]
       (unless (symbol? k)
         (error "Symbol required for binding table key, but got:" k))
       (eval `(define ,k ,v) m)))
    (eval `(extend ,@extends) m)))

(define (expand-template-string text env)
  (assume-type env <template-enviroment>)
  (eval (string-interpolate text) (~ env'module)))

(define (expand-template-file filename env)
  (assume-type env <template-enviroment>)
  (expand-template-string (file->string filename) env))

