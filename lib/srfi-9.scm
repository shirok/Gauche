;;;
;;; srfi-9.scm - defining record types
;;;  
;;;   Copyright (c) 2000-2008  Shiro Kawai  <shiro@acm.org>
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
;;;  $Id: srfi-9.scm,v 1.4 2008-05-10 13:35:48 shirok Exp $
;;;

;; Implements record types on top of Gauche objects.
;;
;;  * A record type is realized as a class inherits <record>.
;;  * Accessors, and modifiers are realized as a method.

(define-module srfi-9
  (use srfi-1)
  (export <record> define-record-type))
(select-module srfi-9)

(define-class <record> ()
  ())

(define-syntax define-record-type
  (syntax-rules ()
    ((_ type
        (constructor init-tag ...)
        predicate
        (field accessor . maybe-modifier) ...)
     (begin
       (ensure-record-consistency 'type '(init-tag ...) '(field ...))
       (define-class type (<record>)
         (field ...))
       (define (constructor init-tag ...)
         (let ((instance (make type)))
           (slot-set! instance 'init-tag init-tag) ...
           instance))
       (define (predicate self)
         (is-a? self type))
       (define-record-field type field accessor . maybe-modifier) ...)
     )))

;; auxiliary macro
(define-syntax define-record-field
  (syntax-rules ()
    ((_ type field accessor)
     (define-method accessor ((self type))
       (slot-ref self 'field)))
    ((_ type field accessor modifier)
     (begin
       (define-method accessor ((self type))
         (slot-ref self 'field))
       (define-method modifier ((self type) value)
         (slot-set! self 'field value))))
    ))

;; auxiliary proc
(define (ensure-record-consistency type init-tags fields)
  (cond ((find (lambda (x) (not (symbol? x))) fields)
         => (lambda (x)
              (errorf "invalid define-record-type for ~a: field name must be a symbol, but got ~s" type x))))
  (for-each (lambda (init-tag)
              (unless (memq init-tag fields)
                (errorf "invalid define-record-type for ~a: constructor definition contains a tag ~a which is not a field" type init-tag)))
            init-tags))

(provide "srfi-9")
