;;;
;;; srfi-189 - Maybe and Either
;;;
;;;   Copyright (c) 2020  Shiro Kawai  <shiro@acm.org>
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

(define-module srfi-189
  (use srfi-1)
  (use util.match)
  (export just nothing right left either-swap
          maybe? either? just? nothing? right? left?
          maybe= either=
          maybe-ref either-ref maybe-ref/default either-ref/default
          maybe-join either-join
          ;; maybe-compose either-compose
          maybe-bind either-bind
          maybe-length either-length
          maybe-filter maybe-remove either-filter either-remove
          maybe-sequence either-sequence
          either->maybe list->just list->right
          maybe->list either->list
          maybe->lisp lisp->maybe
          maybe->eof eof->maybe
          maybe->values either->values
          values->maybe values->either
          maybe->lisp-values either->lisp-values
          ;; lisp-values->maybe lisp-values->either
          maybe-map either-map maybe-for-each either-for-each
          maybe-fold either-fold maybe-unfold either-unfold
          maybe-if

          ;; try-not try=? tri-and tri-or tri-merge
          )
  )
(select-module srfi-189)

(define-class <maybe> () ())

(define-class <just> (<maybe>)
  ((objs :init-keyword :objs)))
(define-class <nothing> (<maybe>) ())

(define-class <either> () ())

(define-class <right> (<either>)
  ((objs :init-keyword :objs)))
(define-class <left> (<either>)
  ((objs :init-keyword :objs)))

(define-method write-object ((obj <just>) port)
  (format port "#<Just ~a>"
          (string-join (map write-to-string (~ obj'objs)) " ")))
(define-method write-object ((obj <nothing>) port)
  (display "#<Nothing>" port))
(define-method write-object ((obj <right>) port)
  (format port "#<Right ~a>"
          (string-join (map write-to-string (~ obj'objs)) " ")))
(define-method write-object ((obj <left>) port)
  (format port "#<Left ~a>"
          (string-join (map write-to-string (~ obj'objs)) " ")))
  

(define *nothing* (make <nothing>))

;; API
(define (just . objs) (make <just> :objs objs))
(define (nothing) *nothing*)
(define (right . objs) (make <right> :objs objs))
(define (left . objs) (make <left> :objs objs))

(define (either-swap either)
  (assume-type either <either>)
  (make (if (left? either) <right> <left>) :objs (~ either'objs)))

(define (maybe? x) (is-a? x <maybe>))
(define (just? x) (is-a? x <just>))
(define (nothing? x) (is-a? x <nothing>))
(define (either? x) (is-a? x <either>))
(define (right? x) (is-a? x <right>))
(define (left? x) (is-a? x <left>))

(define (maybe= eqproc x y)
  (or (and (nothing? x) (nothing? y))
      (and (just? x) (just? y)
           (list= eqproc (~ x'objs) (~ y'objs)))))
(define (either= eqproc x y)
  (or (and (right? x) (right? y)
           (list= eqproc (~ x'objs) (~ y'objs)))
      (and (left? x) (left? y)
           (list= eqproc (~ x'objs) (~ y'objs)))))

(define (%maybe-ref-failure)
  (error "Attempt to derefenence <nothing>"))
(define (%either-ref-failure . args)
  (error "Attempt to derefenence <left> with values" args))

(define (maybe-ref maybe :optional (failure %maybe-ref-failure) 
                                   (success values))
  (assume-type maybe <maybe>)
  (if (nothing? maybe)
    (failure)
    (apply success (~ maybe'objs))))

(define (either-ref either :optional (failure %either-ref-failure) 
                                     (success values))
  (assume-type either <either>)
  (if (left? either)
    (apply failure (~ either'objs))
    (apply success (~ either'objs))))

(define (maybe-ref/default maybe . defaults)
  (assume-type maybe <maybe>)
  (apply values (if (just? maybe) (~ maybe'objs) defaults)))

(define (either-ref/default either . defaults)
  (assume-type either <either>)
  (apply values (if (right? either) (~ either'objs) defaults)))

(define (maybe-join maybe)
  (assume-type maybe <maybe>)
  (if (nothing? maybe)
    maybe
    (match (~ maybe'objs)
      [((? maybe? val)) val]
      [x (error "invalid payload" x)])))

(define (either-join either)
  (assume-type either <either>)
  (if (left? either)
    either
    (match (~ either'objs)
      [((? either? val)) val]
      [x (error "invalid payload" x)])))
      
(define (maybe-bind maybe proc . procs)
  (assume-type maybe <maybe>)
  (if (nothing? maybe)
    maybe
    (if (null? procs)
      (apply proc (~ maybe'objs))       ;tail call
      (apply maybe-bind (apply proc (~ maybe'objs)) procs))))

(define (either-bind either proc . procs)
  (assume-type either <either>)
  (if (left? either)
    either
    (if (null? procs)
      (apply proc (~ either'objs))      ;tail call
      (apply either-bind (apply proc (~ either'objs)) procs))))

(define (maybe-length maybe)
  (assume-type maybe <maybe>)
  (if (nothing? maybe) 0 1))

(define (either-length either)
  (assume-type either <either>)
  (if (left? either) 0 1))

(define (maybe-filter pred maybe)
  (assume-type maybe <maybe>)
  (if (and (just? maybe) (apply pred (~ maybe'objs)))
    maybe
    (nothing)))

(define (maybe-remove pred maybe)
  (assume-type maybe <maybe>)
  (if (and (just? maybe) (not (apply pred (~ maybe'objs))))
    maybe
    (nothing)))

(define (either-filter pred either obj)
  (assume-type either <either>)
  (if (and (right? either) (apply pred (~ either'objs)))
    either
    (left obj)))

(define (either-remove pred either obj)
  (assume-type either <either>)
  (if (and (right? either) (not (apply pred (~ either'objs))))
    either
    (left obj)))

;; input :: Container Maybe a*
;; cmap :: Container Maybe a* -> (Maybe a* -> b) -> Container b
;; aggregator :: a* -> b
;; Returns Maybe Container b
(define (maybe-sequence input cmap :optional (aggregator list))
  (let/cc return
    (just (cmap (^[me] (maybe-ref me (^[] (return (nothing))) aggregator))
                input))))

;; input :: Container Either a*
;; cmap :: Container Either a* -> (Either a -> b) -> Container b
;; aggregator :: a* -> b
;; returns Either Container b
(define (either-sequence input cmap :optional (aggregator list))
  (let/cc return
    (right (cmap (^[ee] (either-ref ee (^ _ (return ee)) aggregator))
                 input))))

(define (maybe->either maybe obj)
  (assume-type maybe <maybe>)
  (if (just? maybe)
    (apply right (~ maybe'objs))
    (left obj)))

(define (either->maybe either)
  (assume-type either <either>)
  (if (right? either)
    (apply just (~ either'objs))
    (nothing)))

(define (list->just lis) (apply just lis))
(define (list->right lis) (apply right lis))

(define (maybe->list maybe)
  (assume-type maybe <maybe>)
  (if (nothing? maybe) '() (~ maybe'objs)))
(define (either->list either)
  (assume-type either <either>)
  (~ either'objs))

(define (maybe->lisp maybe)
  (assume-type maybe <maybe>)
  (and (just? maybe)
       (match (~ maybe'objs)
         [(val) val]
         [_ (error "just doesn't have exactly one value:" maybe)])))

(define (lisp->maybe obj)
  (if obj (just obj) (nothing)))

(define (maybe->eof maybe)
  (assume-type maybe <maybe>)
  (if (just? maybe)
    (match (~ maybe'objs)
      [(val) val]
      [_ (error "just doesn't have exactly one value:" maybe)])
    (eof-object)))

(define (eof->maybe obj)
  (if (eof-object? obj)
    (nothing)
    (just obj)))

(define (maybe->values maybe)
  (assume-type maybe <maybe>)
  (if (nothing? maybe) (values) (apply values (~ maybe'objs))))
(define (either->values either)
  (assume-type either <either>)
  (if (left? either) (values) (apply values (~ either'objs))))

(define (values->maybe producer)
  (call-with-values producer
    (^ xs (if (null? xs) (nothing) (apply just xs)))))
(define (values->either producer obj)
  (call-with-values producer
    (^ xs (if (null? xs) (left obj) (apply right xs)))))

(define (maybe->lisp-values maybe)
  (assume-type maybe <maybe>)
  (if (nothing? maybe)
    (values #f #f)
    (match (~ maybe'objs)
      [(val) (values val #t)]
      [_ (error "just doesn't have exactly one value:" maybe)])))
(define (either->lisp-values either)
  (assume-type either <either>)
  (if (left? either)
    (values #f #f)
    (match (~ either'objs)
      [(val) (values val #t)]
      [_ (error "right doesn't have exactly one value:" either)])))

(define (maybe-map proc maybe)
  (assume-type maybe <maybe>)
  (if (nothing? maybe)
    maybe
    (make <just> :objs (apply proc (~ maybe'objs)))))
(define (either-map proc either)
  (assume-type either <either>)
  (if (left? either)
    either
    (make <right> :objs (apply proc (~ either'objs)))))

(define (maybe-for-each proc maybe)
  (assume-type maybe <maybe>)
  (when (just? maybe)
    (apply proc (~ maybe'objs)))
  (undefined))
(define (either-for-each proc either)
  (assume-type either <either>)
  (when (right? either)
    (apply proc (~ either'objs)))
  (undefined))

(define (maybe-fold kons knil maybe)
  (assume-type maybe <maybe>)
  (if (nothing? maybe)
    (apply kons (append (~ maybe'objs) (list knil)))
    knil))
(define (either-fold kons knil either)
  (assume-type either <either>)
  (if (right? either)
    (apply kons (append (~ either'objs) (list knil)))
    knil))

(define (maybe-unfold stop? mapper _ . seeds)
  (if (apply stop? seeds)
    (nothing)
    (apply just (apply mapper seeds))))
(define (either-unfold stop? mapper _ . seeds)
  (if (apply stop? seeds)
    (apply left seeds)
    (apply right (apply mapper seeds))))

(define-syntax maybe-if
  (syntax-rules ()
    [(_ expr justx nothingx)
     (let1 x expr
       (assume-type x <maybe>)
       (if (just? expr) justx nothingx))]))



    
