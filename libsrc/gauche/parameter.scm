;;;
;;; parameter.scm - parameter support
;;;
;;;   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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
  (export <parameter> make-parameter parameterize
          parameter-pre-observers
          parameter-post-observers
          parameter-observer-add!
          parameter-observer-delete!)
  )
(select-module gauche.parameter)

(autoload gauche.hook make-hook add-hook! delete-hook! run-hook)

(define-class <parameter> ()
  (;; all slots should be private
   (filter :init-keyword :filter :init-value #f)
   (setter)
   (getter)
   (restorer)                           ;used to restore previous value
   (pre-observers)
   (post-observers)
   ))

(define-method initialize ((self <parameter>) initargs)
  (next-method)
  (let* ([filter (slot-ref self 'filter)]
         [pre-hook #f]
         [post-hook #f]
         [init-value (let1 v (get-keyword :init-value initargs #f)
                       (if filter (filter v) v))]
         [index ((with-module gauche.internal %vm-make-parameter-slot))]
         [%ref  (with-module gauche.internal %vm-parameter-ref)]
         [%set! (with-module gauche.internal %vm-parameter-set!)])
    (slot-set! self 'getter (^() (%ref index init-value)))
    (slot-set! self 'setter
               (if filter
                 (^(val) (let1 new (filter val)
                           (rlet1 old (%ref index init-value)
                             (when pre-hook (run-hook pre-hook old new))
                             (%set! index init-value new)
                             (when post-hook (run-hook post-hook old new)))))
                 (^(val) (rlet1 old (%ref index init-value)
                           (when pre-hook (run-hook pre-hook old val))
                           (%set! index init-value val)
                           (when post-hook (run-hook post-hook old val))))))
    (slot-set! self 'restorer          ;bypass filter proc
               (^(val) (rlet1 old (%ref index init-value)
                         (when pre-hook (run-hook pre-hook old val))
                         (%set! index init-value val)
                         (when post-hook (run-hook post-hook old val)))))
    (let-syntax ([hook-ref
                  (syntax-rules ()
                    [(_ var) (^() (or var (rlet1 h (make-hook 2)
                                            (set! var h))))])])
      (slot-set! self 'pre-observers (hook-ref pre-hook))
      (slot-set! self 'post-observers (hook-ref post-hook)))
    ))

(define-method object-apply ((self <parameter>))
  ((slot-ref self 'getter)))

(define-method object-apply ((self <parameter>) newval)
  ((slot-ref self 'setter) newval))

;; Allow (set! (parameter) value).  By KOGURO, Naoki
(define-method (setter object-apply) ((obj <parameter>) value)
  (obj value))

(define (make-parameter value :optional (filter #f))
  (make <parameter> :filter filter :init-value value))

;; restore parameter value after parameterize body.  we need to bypass
;; the filter procedure (fix for the bug reported by Joo ChurlSoo.
;; NB: For historical reasons, PARAMETERIZE may be used with paremeter-like
;; procedures.
(define (%restore-parameter param prev-val)
  (if (is-a? param <parameter>)
    ((slot-ref param'restorer) prev-val)
    (param prev-val)))

(define-syntax parameterize
  (syntax-rules ()
    [(_ (binds ...) . body)
     (%parameterize () () () (binds ...) body)]))

(define-syntax %parameterize-1
  (syntax-rules ()
    [(_ () body) (begin . body)]
    [(_ ((P L S) . ts) body)
     (dynamic-wind
       (^() (set! S (P L)))
       (^() (%parameterize-1 ts body))
       (^() (set! L (%restore-parameter P S))))]))

(define-syntax %parameterize
  (syntax-rules ()
    ;; temporaries
    ;;   P - keeps the parameter object, for the variable param may be
    ;;       reassigned during execution of body.
    ;;   L - keeps "local" value during dynamic enviornment of body.
    ;;   S - keeps "saved" value outside of parameterize.
    [(_ (param ...) (val ...) ((P L S) ...) () body)
     (let ((P param) ... (L val) ... (S #f) ...)
       (%parameterize-1 ((P L S) ...) body))]
    [(_ (param ...) (val ...) (tmps ...)
        ((p v) . more) body)
     (%parameterize (param ... p) (val ... v) (tmps ... (P L S)) more body)]
    [(_ params vals tmps other body)
     (syntax-error "malformed binding list for parameterize" other)]))

;; hooks

(define-method parameter-pre-observers ((self <parameter>))
  ((ref self 'pre-observers)))

(define-method parameter-post-observers ((self <parameter>))
  ((ref self 'post-observers)))

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

(define-method parameter-observer-delete! ((self <parameter>) proc . args)
  (let ((where (get-optional args #f)))
    (unless (eq? where 'after)
      (delete-hook! (parameter-pre-observers self) proc))
    (unless (eq? where 'before)
      (delete-hook! (parameter-post-observers self) proc))
    ))




