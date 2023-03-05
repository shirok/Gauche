;;;
;;; SRFI-235 - Combinators
;;;
;;;   Copyright (c) 2023  Shiro Kawai  <shiro@acm.org>
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

(define-module srfi.235
  (use gauche.collection)               ;group-collection
  (use util.match)
  (export constantly                    ;builtin
          complement                    ;builtin
          flip                          ;builtin
          swap                          ;builtin
          on-left on-right
          conjoin disjoin
          each-of all-of any-of
          on
          left-section right-section
          apply-chain
          arguments-drop arguments-drop-right
          arguments-take arguments-take-right
          group-by
          begin-procedure
          if-procedure
          when-procedure
          unless-procedure
          value-procedure
          case-procedure
          and-procedure eager-and-procedure
          or-procedure eager-or-procedure
          funcall-procedure loop-procedure
          while-procedure until-procedure
          always never
          boolean                       ;builtin
          )
  )
(select-module srfi.235)

(define (on-left proc) (^[x y] (proc x)))
(define (on-right proc) (^[x y] (proc y)))

(define (conjoin . preds)
  (if (null? preds)
    (constantly #t)
    (let1 pred* (apply every-pred preds)
      (^ args (every pred* args)))))

(define (disjoin . preds)
  (if (null? preds)
    (constantly #f)
    (let1 pred* (apply any-pred preds)
      (^ args (any pred* args)))))

(define (each-of . procs)
  (^ args
    (for-each (^p (apply p args)) procs)))

(define (all-of pred)
  (^[xs] (every pred xs)))

(define (any-of pred)
  (^[xs] (any pred xs)))

(define (on reducer mapper)
  (^ xs (apply reducer (map mapper xs))))

(define (left-section proc . args)
  (apply pa$ proc args))

(define (right-section proc . args)
  (let1 rargs (reverse args)
    (^ more-args (apply proc (append more-args rargs)))))

;; Same as 'compose' except at least one proc is required.
(define (apply-chain proc . procs) (apply compose proc procs))

(define (arguments-drop proc n)
  (^ args (apply proc (drop args n))))
(define (arguments-drop-right proc n)
  (^ args (apply proc (drop-right args n))))
(define (arguments-take proc n)
  (^ args (apply proc (take args n))))
(define (arguments-take-right proc n)
  (^ args (apply proc (take-right args n))))

(define (group-by key-proc :optional (= equal?))
  (^[lis]
    (assume-type lis <list>)
    (group-collection lis :key key-proc :test =)))

(define begin-procedure
  (case-lambda
    [() (undefined)]
    [thunks
     (let loop ([thunks thunks])
       (if (null? (cdr thunks))
         ((car thunks))
         (begin ((car thunks)) (loop (cdr thunks)))))]))

(define (if-procedure value then-thunk else-thunk)
  (if value (then-thunk) (else-thunk)))

(define (when-procedure value . thunks)
  (when value
    (apply begin-procedure thunks) (undefined)))

(define (unless-procedure value . thunks)
  (unless value
    (apply begin-procedure thunks) (undefined)))

(define (value-procedure value then-proc :optional (else-thunk undefined))
  (if value (then-proc value) (else-thunk)))

(define (case-procedure value thunk-alist :optional (else-thunk undefined))
  (if-let1 p (assv value thunk-alist)
    ((cdr p))
    (else-thunk)))

(define (and-procedure . thunks)
  (every funcall-procedure thunks))

(define (eager-and-procedure . thunks)
  (every identity (map funcall-procedure thunks)))

(define (or-procedure . thunks)
  (any funcall-procedure thunks))

(define (eager-or-procedure . thunks)
  (any identity (map funcall-procedure thunks)))

(define (funcall-procedure thunk)
  (thunk))

(define (loop-procedure thunk)
  (let loop () (thunk) (loop)))

(define (while-procedure thunk)
  (let loop ()
    (when (thunk) (loop))))

(define (until-procedure thunk)
  (let loop ()
    (unless (thunk) (loop))))

(define (always . objs) #t)
(define (never . objs) #f)
