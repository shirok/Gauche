;;;
;;; r7rs - R7RS compatibility
;;;
;;;   Copyright (c) 2013  Shiro Kawai  <shiro@acm.org>
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

;; This module sets up R7RS environment.
;; This is not intended to be just 'use'-d.  Instead, you say (extend r7rs)
;; and you'll enter the initial toplevel environment of R7RS, where
;; only r7rs-style 'import' is available.  With gosh, you can start -r7
;; option to enter the R7RS environment directly.

(define-module r7rs.import
  (use util.match)
  (use srfi-1)
  (export r7rs-import)

  ;; A trick - must be replaced once we have explicit-renaming macro.
  (define import.  ((with-module gauche.internal make-identifier)
                    'import (find-module 'gauche) '()))
  (define require. ((with-module gauche.internal make-identifier)
                    'require (find-module 'gauche) '()))
  (define begin.   ((with-module gauche.internal make-identifier)
                    'begin (find-module 'gauche) '()))

  (define-macro (r7rs-import . import-sets)
    `(,begin. ,@(append-map %transfer-import-spec import-sets)))

  (define (%transfer-import-spec import-set)
    (define (rec import-set opts)
      (match import-set
        [('only import-set identifier ...)
         (rec import-set `(,@opts :only ,identifier))]
        [('except import-set identifier ...)
         (rec import-set `(,@opts :except ,identifier))]
        [('prefix import-set identifier)
         (rec import-set `(,@opts :prefix ,identifier))]
        [('rename import-set mapping ...)
         (rec import-set `(,@opts :rename ,mapping))]
        [else
         `(,($ string->symbol $ string-join (map x->string import-set) ".")
           ,@opts)]))
    (let1 import-spec (rec import-set '())
      `((,require. ,(module-name->path (car import-spec)))
        (,import. ,import-spec)))))

;;
;; The 'r7rs' module removes all bindings by empty (extend), except the
;; one imported from r7rs.toplevel.
;;
(define-module r7rs
  (define-syntax import (with-module r7rs.import r7rs-import))
  (extend))

