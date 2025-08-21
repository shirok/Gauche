;;;
;;; gauche.cond-expand-rt
;;;
;;;   Copyright (c) 2025  Shiro Kawai  <shiro@acm.org>
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

;; Runtime cond-expand
;;
;; Some of Gauche bundled libraries are precompiled for distribution
;; tarball, so that it can be built on systems without preinstalled Gauche.
;; Precompilation can be done on platforms different from the target,
;; so we can't use cond-expand in it.  This is an alternative of
;; cond-expand that checks features at runtime.
;;
;; It is not for general use; typical libraries should use cond-expand.
;; If you use this from libsrc/*, put (use gauche.cond-expand-rt) outside
;; of define-module form.  It doesn't need to be loaded at runtime.
;;
;; When you use cond-expand/runtime in src/lib*.scm, keep in mind that
;; features are initialized _after_ most runtime are initialized.  So
;; make sure cond-expand/runtime is called after system is fully booted.

(define-module gauche.cond-expand-rt
  (use util.match)
  (export cond-expand/runtime))
(select-module gauche.cond-expand-rt)

;; Runtime cond-expand
;;   This is only needed when Scheme source can be precompiled on a
;;   different architecture.
(define-syntax cond-expand/runtime
  (er-macro-transformer
   (^[f r c]
     (define has-feature? ((with-module gauche.internal make-identifier)
                           'has-feature?
                           (find-module 'gauche.internal)
                           '()))
     (define (and? x) (c (r x) (r'and)))
     (define (or? x)  (c (r x) (r'or)))
     (define (not? x) (c (r x) (r'not)))
     (define (else? x) (c (r x) (r 'else)))

     (define (expand-requirement req)
       (match req
         [(? identifier? id) `(,has-feature? ',id)]
         [((? and?) . reqs)
          (quasirename r `(and ,@(map expand-requirement reqs)))]
         [((? or?) . reqs)
          (quasirename r `(or ,@(map expand-requirement reqs)))]
         [((? not?) req)
          (quasirename r `(not ,(expand-requirement req)))]
         [_ (error "Malformed cond-expand/runtime requirement:" req)]))

     (define (rec clauses)
       (match clauses
         [() (quasirename r `(error "Unfulfilled cond-expand/runtime"))]
         [(((? else?) exprs ...) . rest)
          (if (null? rest)
            (quasirename r `(begin ,@exprs))
            (error "Malformed cond-expand/runtime"))]
         [((requirement exprs ...) . rest)
          (quasirename r
            `(if ,(expand-requirement requirement)
               (begin ,@exprs)
               ,(rec rest)))]
         [_ (error "Malformed cond-expand/runtime")]))

     (rec (cdr f)))))
