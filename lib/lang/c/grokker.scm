;;;
;;; C Semantic analysis
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

;;  Takes the output of c-parser and understands it.

(define-module lang.c.grokker
  (use util.match)
  (use lang.c.parser)
  (export <c-context>
          c-grok-file
          c-grok-string)
  )
(select-module lang.c.grokker)

(define-class <c-context> ()
  ((globals :init-form (make-hash-table 'eq?))))

(define current-c-context (make-parameter (make <c-context>)))

(define (with-fresh-c-context thunk)
  (rlet1 ctx (make <c-context>)
    (parameterize ([current-c-context ctx]) (thunk))))

(define-class <c-global> ()
  ((name :init-keyword :name)               ;symbol
   (type :init-keyword :type)
   (storage-class :init-keyword :storage-class)
   (source-info :init-keyword :source-info)))

(define-method register-global! ((ctx <c-context>) (g <c-global>))
  (let* ([tab (~ ctx'globals)]
         [name (~ g'name)]
         [e (hash-table-get tab name #f)])
    (if e
      (begin
        ;; TODO: we also need to check type mistmatch
        (unless (or (eq? (~ e'storage-class) 'extern)
                    (eq? (~ g'storage-class) 'extern))
          (warn "~aC-context: Duplicate identifier: ~a (previously ~a)\n"
                (format-source-info (~ g'source-info))
                name
                (format-source-info (~ e'source-info))))
        ;; We keep non-"extern" version
        (unless (eq? (~ g'storage-class) 'extern)
          (hash-table-put! tab name g)))
      (hash-table-put! tab name g))))

(define-class <c-variable> (<c-global>)
  ((initializer :init-keyword :initializer)))

(define (add-variable name type storage-class initializer si)
  (rlet1 e (make <c-variable> :name name :type type :source-info si
                 :storage-class storage-class :initializer initializer)
    (register-global! (current-c-context) e)))

(define-class <c-function> (<c-global>)
  ((body :init-keyword :body)))

(define (add-function name type storage-class body si)
  (rlet1 e (make <c-function> :name name :type type :source-info si
                 :storage-class storage-class :body body)
    (register-global! (current-c-context) e)))

(define (format-source-info si)
  (match si
    [(file line) (format "~s:~d:" file line)]
    [_ ""]))

(define (c-grok-parser-output-1 form)
  (let1 si (c-source-info-of form)
    (match form
      [('decl identifier storage-class type init)
       (add-variable identifier type storage-class init si)]
      [('def identifier storage-class type body)
       (add-function identifier type storage-class body si)]
      [_ (error "[internal] Unknown parser output:" form)])))

;; TODO: Make parser returns lseq/cseq and run grokker concurrently

(define (c-grok-file file)
  (with-fresh-c-context
   (^[]
     (dolist [form (c-parse-file (c-toplevel-parser) file)]
       (c-grok-parser-output-1 form)))))

(define (c-grok-string string)
  (with-fresh-c-context
   (^[]
     (dolist [form (c-parse-string (c-toplevel-parser) string)]
       (c-grok-parser-output-1 form)))))
