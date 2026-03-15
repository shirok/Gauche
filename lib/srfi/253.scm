;;;
;;; SRFI-253 - Data (Type-)Checking
;;;
;;;   Copyright (c) 2024  Artyom Bologov
;;;   Copyright (c) 2026  Antero Mejr  <mail@antr.me>
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

(define-module srfi.253
  (export check-arg
          values-checked check-case
          lambda-checked define-checked
          case-lambda-checked
          define-record-type-checked))

(define-syntax check-arg
  (syntax-rules ()
    ((_ ty expr)
     (check-arg ty expr 'check-arg))
    ((_ ty expr caller)
     (cond ((type? ty)
            (assume-type expr ty))
           ((applicable? ty <top>)
            (assume (ty expr) "type mismatch" ty expr caller))
           (else
            (error
             "First argument to check-arg must be type or applicable \
predicate procedure." ty caller))))))

;; Below this line are the helpers from the generic sample implementation.

(define-syntax values-checked
  (syntax-rules ()
    ((_ (predicate) value)
     (let ((v value))
       (check-arg predicate v 'values-checked)
       v))
    ((_ (predicate ...) value ...)
     (values (values-checked (predicate) value) ...))))

(define-syntax %check-case
  (syntax-rules (else)
    ((_ val (clause ...) (else body ...))
     (cond
      clause ...
      (else body ...)))
    ((_ val ((clause-check clause-body ...) ...))
     (cond
      (clause-check clause-body ...)
      ...
      (else (assume (or clause-check ...)
                    "at least one branch of check-case should be true"
                    'clause-check ...))))
    ((_ val (clause ...) (pred body ...) rest ...)
     (%check-case
      val
      (clause ... ((pred val) body ...))
      rest ...))))

(define-syntax check-case
  (syntax-rules ()
    ((_ value clause ...)
     (let ((v value))
       (%check-case v () clause ...)))))

(define-syntax %lambda-checked
  (syntax-rules ()
    ((_ name (body ...) args (checks ...) ())
     (lambda args
       checks ...
       body ...))
    ((_ name body (args ...) (checks ...) ((arg pred) . rest))
     (%lambda-checked
      name body
      (args ... arg) (checks ... (check-arg pred arg 'name)) rest))
    ((_ name body (args ...) (checks ...) (arg . rest))
     (%lambda-checked
      name body
      (args ... arg) (checks ...) rest))
    ((_ name body (args ...) (checks ...) last)
     (%lambda-checked
      name body
      (args ... . last) (checks ...) ()))))

(define-syntax lambda-checked
  (syntax-rules ()
    ((_ () body ...)
     (lambda () body ...))
    ((_ (arg . args) body ...)
     (%lambda-checked lambda-checked (body ...) () () (arg . args)))
    ;; Case of arg->list lambda, no-op.
    ((_ arg body ...)
     (lambda arg body ...))))

(define-syntax define-checked
  (syntax-rules ()
    ;; Procedure
    ((_ (name . args) body ...)
     (define name (%lambda-checked name (body ...) () () args)))
    ;; Variable
    ((_ name pred value)
     (define name (values-checked (pred) value)))))

(define-syntax %case-lambda-checked
  (syntax-rules ()
    ((_ (clauses-so-far ...)
        ()
        args-so-far (checks-so-far ...) (body ...) ())
     (case-lambda
      clauses-so-far ...
      (args-so-far
       checks-so-far ...
       body ...)))
    ((_ (clauses-so-far ...)
        ((() body-to-process ...) clauses-to-process ...)
        args-so-far (checks-so-far ...) (body ...) ())
     (%case-lambda-checked
      (clauses-so-far ... (args-so-far checks-so-far ... body ...))
      (clauses-to-process ...)
      () () (body-to-process ...) ()))
    ((_ (clauses-so-far ...)
        (((arg . args-to-process) body-to-process ...) clauses-to-process ...)
        args-so-far (checks-so-far ...) (body ...) ())
     (%case-lambda-checked
      (clauses-so-far ... (args-so-far checks-so-far ... body ...))
      (clauses-to-process ...)
      () () (body-to-process ...) (arg . args-to-process)))
    ((_ (clauses-so-far ...)
        ((arg-to-process body-to-process ...) clauses-to-process ...)
        args-so-far (checks-so-far ...) (body ...) ())
     (%case-lambda-checked
      (clauses-so-far ... (args-so-far checks-so-far ... body ...))
      (clauses-to-process ...)
      arg-to-process () (body-to-process ...) ()))
    ((_ (clauses-so-far ...) (clauses-to-process ...)
        (args-so-far ...) (checks-so-far ...) (body ...) ((arg pred) . args))
     (%case-lambda-checked
      (clauses-so-far ...) (clauses-to-process ...)
      (args-so-far ... arg)
      (checks-so-far ... (check-arg pred arg 'case-lambda-checked))
      (body ...) args))
    ((_ (clauses-so-far ...) (clauses-to-process ...)
        (args-so-far ...) (checks-so-far ...) (body ...) (arg . args))
     (%case-lambda-checked
      (clauses-so-far ...) (clauses-to-process ...)
      (args-so-far ... arg) (checks-so-far ...) (body ...) args))
    ((_ (clauses-so-far ...) (clauses-to-process ...)
        (args-so-far ...) (checks-so-far ...) (body ...) arg)
     (%case-lambda-checked
      (clauses-so-far ...) (clauses-to-process ...)
      (args-so-far ... . arg) (checks-so-far ...) (body ...) ()))))

(define-syntax case-lambda-checked
  (syntax-rules ()
    ((_ (() first-body ...) rest-clauses ...)
     (%case-lambda-checked () (rest-clauses ...) () () (first-body ...) ()))
    ((_ ((first-arg . first-args) first-body ...) rest-clauses ...)
     (%case-lambda-checked () (rest-clauses ...) () () (first-body ...) (first-arg . first-args)))
    ((_ (args-var first-body ...) rest-clauses ...)
     (%case-lambda-checked () (rest-clauses ...) args-var () (first-body ...) ()))))

;; define-record-type-checked

(define-syntax %define-record-type-checked
  (syntax-rules ()
    ((_ type-name constructor predicate
        (fields ...) (field-wrappers ...))
     (begin
       (define-record-type
           type-name constructor predicate
           fields ...)
       field-wrappers ...))
    ((_ type-name constructor predicate
        (fields ...) (field-wrappers ...) (field pred accessor modifier)
        fields-to-process ...)
     (%define-record-type-checked
      type-name constructor predicate
      (fields ... (field internal-accessor internal-modifier))
      (field-wrappers
       ...
       (define-checked (accessor (record predicate))
         (internal-accessor record))
       (define-checked (modifier (record predicate) (val pred))
         (internal-modifier record val)))
      fields-to-process ...))
    ((_ type-name constructor predicate
        (fields ...) (field-wrappers ...) (field pred accessor)
        fields-to-process ...)
     (%define-record-type-checked
      type-name constructor predicate
      (fields ... (field internal-accessor))
      (field-wrappers
       ...
       (define-checked (accessor (record predicate))
         (internal-accessor record)))
      fields-to-process ...))))

(define-syntax %wrap-constructor
  (syntax-rules ()
    ((_ constructor internal-constructor (arg-names ...) (args ...))
     (define-checked (constructor args ...)
       (internal-constructor arg-names ...)))
    ((_ constructor internal-constructor (arg-names ...) (args ...)
        (name pred rest ...) fields-to-process ...)
     (%wrap-constructor constructor internal-constructor
                        (arg-names ... name) (args ... (name pred))
                        fields-to-process ...))))

(define-syntax define-record-type-checked
  (syntax-rules ()
    ((_ type-name (constructor constructor-args ...) predicate field ...)
     (begin
       (%define-record-type-checked
        type-name
        (internal-constructor constructor-args ...)
        predicate
        () () field ...)
       (%wrap-constructor constructor internal-constructor () () field ...)))))
