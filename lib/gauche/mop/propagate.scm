;;;
;;; propagate.scm - propagate slot option
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

(define-module gauche.mop.propagate
  (use srfi-1)
  (use util.match)
  (export <propagate-meta> <propagate-mixin>)
  )
(select-module gauche.mop.propagate)

;; 'propagate' slot option sends get/set request to other object.
;; The idea is taken from STk's "composite metaclass".
;;
;; The slot must have ':propagated' allocation, and ':propagate' option.
;; ':Propagate' option may be a symbol or a list of two elements.
;; Suppose the slot foo is a propagated slot.  If a symbol bar is given
;; to the :propagate option, reading of the slot returns
;; (slot-ref (slot-ref obj 'bar) 'foo), and writing to the slot causes
;; (slot-set! (slot-ref obj 'bar) 'foo value).  If a list (bar baz)
;; is given, baz is used as the actual slot name instead of foo.

(define-class <propagate-meta> (<class>) ())

(define-method compute-get-n-set ((class <propagate-meta>) slot)
  (let ([name  (slot-definition-name slot)]
        [alloc (slot-definition-allocation slot)])
    (if (eq? alloc :propagated)
      (match (or (slot-definition-option slot :propagate #f)
                 (slot-definition-option slot :propagate-to #f))
        [(? symbol? prop)
         `(,(^o (slot-ref (slot-ref-using-class class o prop) name))
           ,(^(o v) (slot-set! (slot-ref-using-class class o prop) name v))
           ,(^o (slot-bound? (slot-ref-using-class class o prop) name))
           #t)]
        [((? symbol? object-slot) (? symbol? real-slot))
         `(,(^o (slot-ref (slot-ref-using-class class o object-slot)
                          real-slot))
           ,(^(o v) (slot-set! (slot-ref-using-class class o object-slot)
                               real-slot v))
           ,(^o (slot-bound? (slot-ref-using-class class o object-slot)
                             real-slot))
           #t)]
        [other
         (errorf "bad :propagated slot option value ~s for slot ~s of class ~s"
                 other name class)])
      (next-method))))

(define-class <propagate-mixin> ()
  ()
  :metaclass <propagate-meta>)

