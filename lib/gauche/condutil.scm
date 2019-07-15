;;;
;;; condutil.scm - condition primitives.  autoloaded.
;;;
;;;   Copyright (c) 2004-2019  Shiro Kawai  <shiro@acm.org>
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

;; Defines some condition-related primitives.

(define-module gauche.condutil
  (use util.match)
  (export make-condition-type condition-type?
          make-condition condition-ref extract-condition
          define-condition-type condition
          &condition &message &serious &error
          &i/o-error &i/o-port-error
          &i/o-read-error &i/o-write-error &i/o-closed-error
          &read-error)
  )
(select-module gauche.condutil)

;; defined in C:
;;   condition?
;;   condition-has-type?
;;   make-compound-condition

(define (make-condition-type name parent field-names)
  (unless (condition-type? parent)
    (error "condition-type required as a parent of make-condition-type, but got:" parent))
  (make <condition-meta>
    :name name
    :supers (list parent)
    :slots  field-names))

(define (condition-type? obj)
  (is-a? obj <condition-meta>))

(define (make-condition type . initargs)
  (unless (condition-type? type)
    (error "make-condition requires a condition type, but got:" type))
  (let ((c (make type)))
    (let loop ((args initargs))
      (cond ((null? args) c)
            ((null? (cdr args))
             (error "make-condition is given non-even initargs:" initargs))
            ((slot-exists? c (car args))
             (slot-set! c (car args) (cadr args))
             (loop (cddr args)))
            (else
             (errorf "condition type ~s doesn't have a field ~s"
                     type (car args)))))))

(define (condition-ref c slot)
  (slot-ref c slot))  ;; compound condition traps slot-missing

(define (extract-condition c type)
  (unless (condition-has-type? c type)
    (errorf "cannot extract a condition of type ~s from a condition ~s"
            type c))
  (let ((cc (make type)))
    (let loop ((slots (class-slots type)))
      (if (null? slots)
        cc
        (let ((sn (slot-definition-name (car slots))))
          (when (slot-bound? c sn)
            (slot-set! cc sn (slot-ref c sn)))
          (loop (cdr slots)))))))

;; macros

;; we extend srfi-35 to allow #f as predicate and accessors, as well as
;; omitting accessors.

(define-syntax define-condition-type
  (er-macro-transformer
   (^[f r c]
     (match (cdr f)
       [(name super pred . field-specs)
        (define (badfield-error field)
          (error "bad field spec for define-condition-type:" field))
        (define (scan-specs specs slots readers)
          (match specs
            [() (emit-defs slots readers)]
            [((field #f) . rest)
             (scan-specs rest (cons field slots) readers)]
            [((field) . rest)
             (scan-specs rest (cons field slots) readers)]
            [((field reader) . rest)
             (scan-specs rest (cons field slots)
                         (cons (quasirename r
                                 `(define (,reader obj)
                                    (condition-ref obj ',field)))
                               readers))]
            [_ (badfield-error (car specs))]))
        (define (emit-defs slots readers)
          (quasirename r
            `(begin
               (define-class ,name (,super)
                 ,(map (^s (quasirename r 
                             `(,s :init-keyword ',(make-keyword s))))
                       slots)
                 :metaclass <condition-meta>)
               ,@readers
               ,@(if pred
                   (quasirename r
                     `((define (,pred obj)
                        (condition-has-type? obj ,name))))
                   '()))))
        (scan-specs field-specs '() '())]
       ))))

(define-syntax condition
  (syntax-rules ()
    ((condition (type . bindings) ...)
     (make-compound-condition
      (condition-sub type () bindings) ...))
    ((_ . other)
     (syntax-error "malformed condition:" (condition . other)))))

(define-syntax condition-sub
  (syntax-rules ()
    ((condition-sub type inits ())
     (make-condition type . inits))
    ((condition-sub type (init ...) ((field expr) . more))
     (condition-sub type (init ... 'field expr) more))
    ((condition-sub type inits (other . more))
     (syntax-error "malformed condition field initializer:" other))))

;; Aliases for srfi-35/srfi-36 compatibility
(define &condition   <condition>)
(define &message     <message-condition>)
(define &serious     <serious-condition>)
(define &error       <error>)
(define &i/o-error   <io-error>)
(define &i/o-port-error <port-error>)
(define &i/o-read-error <io-read-error>)
(define &i/o-write-error <io-write-error>)
(define &i/o-closed-error <io-closed-error>)
(define &read-error  <read-error>)

