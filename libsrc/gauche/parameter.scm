;;;
;;; parameter.scm - parameter support
;;;
;;;   Copyright (c) 2000-2020  Shiro Kawai  <shiro@acm.org>
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

;; The API is upper-compatible to ChezScheme and Chicken's.

(define-module gauche.parameter
  (export <parameter>
          make-parameter 
          parameter?
          procedure-parameter
          parameterize
          parameter-pre-observers
          parameter-post-observers
          parameter-observer-add!
          parameter-observer-delete!)
  )
(select-module gauche.parameter)

(autoload gauche.hook make-hook add-hook! delete-hook! run-hook)

;; <parameter>, make-parameter, parameter?, procedure-parameter
;; parameterize - built-in

;; This module only deals with hooks and some backward compatibility stuff

(define (%ensure-hooks param)
  (unless (slot-bound? param 'pre-observers)
    (print "poing")
    (let ([get (^[] ((with-module gauche.internal %primitive-parameter-ref)
                     param))]
          [set (^v ((with-module gauche.internal %primitive-parameter-set!)
                    param v))]
          [filter (slot-ref param 'filter)])
      (define (observed-set! old new)
        (run-hook (slot-ref param 'pre-observers) old new)
        (set new)
        (run-hook (slot-ref param 'post-observers) old new))

      (slot-set! param 'setter
                 (if filter
                   (^(val) (let1 new (filter val)
                             (rlet1 old (get)
                               (observed-set! old new))))
                   (^(val) (rlet1 old (get)
                             (observed-set! old val)))))
      (slot-set! param 'restorer          ;bypass filter proc
                 (^(val) (rlet1 old (get)
                           (observed-set! old val))))
      (slot-set! param 'pre-observers (make-hook 2))
      (slot-set! param 'post-observers (make-hook 2)))))

(define-method object-apply ((self <parameter>))
  ((slot-ref self 'getter)))

(define-method object-apply ((self <parameter>) newval)
  ((slot-ref self 'setter) newval))

;; Allow (set! (parameter) value).  By KOGURO, Naoki
(define-method (setter object-apply) ((obj <parameter>) value)
  (obj value))

(define-method parameter-pre-observers ((self <parameter>))
  (%ensure-hooks self)
  (ref self 'pre-observers))
(define-method parameter-pre-observers ((self <procedure>))
  (if-let1 p (procedure-parameter self)
    (parameter-pre-observers p)
    (error "parameter procedure required, but got:" self)))

(define-method parameter-post-observers ((self <parameter>))
  (%ensure-hooks self)
  (ref self 'post-observers))
(define-method parameter-post-observers ((self <procedure>))
  (if-let1 p (procedure-parameter self)
    (parameter-post-observers p)
    (error "parameter procedure required, but got:" self)))

(define-method parameter-observer-add! ((self <parameter>) proc . args)
  (let-optionals* args ((when 'after)
                        (where 'append))
    (unless (memq when '(before after))
      (error "`when' argument of parameter-observer-add! must be either 'before or 'after" when))
    (unless (memq where '(prepend append))
      (error "`where' argument of parameter-observer-add! must be either 'prepend or 'append" when))
    (add-hook! (if (eq? when 'before)
                   (parameter-pre-observers self)
                   (parameter-post-observers self))
               proc
               (eq? where 'append))))
(define-method parameter-observer-add! ((self <procedure>) proc . args)
  (if (parameter? self)
    (apply parameter-observer-add! (procedure-parameter self) proc args)
    (error "parameter procedure required, but got:" self)))

(define-method parameter-observer-delete! ((self <parameter>) proc . args)
  (let ((where (get-optional args #f)))
    (unless (eq? where 'after)
      (delete-hook! (parameter-pre-observers self) proc))
    (unless (eq? where 'before)
      (delete-hook! (parameter-post-observers self) proc))
    ))
(define-method parameter-observer-delete! ((self <procedure>) proc . args)
  (if (parameter? self)
    (apply parameter-observer-delete! (procedure-parameter self) proc args)
    (error "parameter procedure required, but got:" self)))


