;;;
;;; srfi-189 - Maybe and Either
;;;
;;;   Copyright (c) 2020-2022  Shiro Kawai  <shiro@acm.org>
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
  (use scheme.list)
  (use util.match)
  (export just nothing right left either-swap
          maybe? either? just? nothing? right? left? maybe-ref-error?
          maybe= either=
          maybe-ref either-ref maybe-ref/default either-ref/default
          maybe-join either-join
          maybe-compose either-compose
          maybe-bind either-bind
          maybe-length either-length
          maybe-filter maybe-remove either-filter either-remove
          maybe-sequence either-sequence
          maybe->either either->maybe list->just list->right list->left
          maybe->list either->list list->maybe list->either
          maybe->truth either->truth truth->maybe truth->either
          maybe->list-truth either->list-truth
          list-truth->maybe list-truth->either
          maybe->generation generation->maybe
          either->generation generation->either
          maybe->values either->values
          values->maybe values->either
          maybe->two-values two-values->maybe
          exception->either
          maybe-map either-map maybe-for-each either-for-each
          maybe-fold either-fold maybe-unfold either-unfold
          maybe-if
          maybe-and maybe-or maybe-let* maybe-let*-values
          either-and either-or either-let* either-let*-values
          either-guard

          tri-not tri=? tri-and tri-or tri-merge
          )
  ;; The followings are Gauche-specific.
  (export <maybe> <just> <nothing>
          <either> <left> <right>)
  )
(select-module srfi-189)

(define-class <maybe> () ())
(define-method initialize ((obj <maybe>) initargs)
  (when (eq? (class-of obj) <maybe>)
    (error "You can't instantiate <maybe> directly."))
  (next-method))

(define-class <just> (<maybe>)
  ((objs :init-keyword :objs)))
(define-class <nothing> (<maybe>) ())

(define-class <either> () ())
(define-method initialize ((obj <either>) initargs)
  (when (eq? (class-of obj) <either>)
    (error "You can't instantiate <either> directly."))
  (next-method))

(define-class <right> (<either>)
  ((objs :init-keyword :objs)))
(define-class <left> (<either>)
  ((objs :init-keyword :objs)))

(define-condition-type <maybe-ref-error> <error>
  maybe-ref-error?)

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

;;; Constructors
(define (just . objs) (make <just> :objs objs))
(define (nothing) *nothing*)
(define (right . objs) (make <right> :objs objs))
(define (left . objs) (make <left> :objs objs))

(define (list->just lis) (make <just> :objs lis))
(define (list->right lis) (make <right> :objs lis))
(define (list->left lis) (make <left> :objs lis))

(define (maybe->either maybe . objs)
  (assume-type maybe <maybe>)
  (if (just? maybe)
    (list->right (~ maybe'objs))
    (list->left objs)))

(define (either->maybe either)
  (assume-type either <either>)
  (if (right? either)
    (list->just (~ either'objs))
    (nothing)))

(define (either-swap either)
  (assume-type either <either>)
  (make (if (left? either) <right> <left>) :objs (~ either'objs)))

;;; Predicates

(define (maybe? x) (is-a? x <maybe>))
(define (just? x) (is-a? x <just>))
(define (nothing? x) (is-a? x <nothing>))
(define (either? x) (is-a? x <either>))
(define (right? x) (is-a? x <right>))
(define (left? x) (is-a? x <left>))

(define (maybe= eqproc x . xs)
  (or (null? xs)
      (let1 y (car xs)
        (and (or (and (nothing? x) (nothing? y))
                 (and (just? x) (just? y)
                      (list= eqproc (~ x'objs) (~ y'objs))))
             (or (null? (cdr xs))
                 (apply maybe= eqproc xs))))))

(define (either= eqproc x . xs)
  (or (null? xs)
      (let1 y (car xs)
        (and (or (and (right? x) (right? y)
                      (list= eqproc (~ x'objs) (~ y'objs)))
                 (and (left? x) (left? y)
                      (list= eqproc (~ x'objs) (~ y'objs))))
             (or (null? (cdr xs))
                 (apply either= eqproc xs))))))

;;; Accessors

;; returns one value in container; raises an error if container doesn't have
;; exactly one value.
(define (%ref1 container)
  (match (~ container'objs)
    [(x) x]
    [_ (error "~a with exactly one value expected, but got: ~s"
              (class-of container) container)]))

(define (maybe-ref maybe failure :optional (success values))
  (assume-type maybe <maybe>)
  (if (nothing? maybe)
    (failure)
    (apply success (~ maybe'objs))))

(define (either-ref either failure :optional (success values))
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

;;; Join and bind

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

(define (maybe-compose proc . procs)
  (if (null? procs)
    proc
    (let1 p (apply maybe-compose procs)
      (^ args
        (let1 m (apply proc args)
          (if (nothing? m)
            m
            (apply p (~ m'objs))))))))

(define (either-bind either proc . procs)
  (assume-type either <either>)
  (if (left? either)
    either
    (if (null? procs)
      (apply proc (~ either'objs))      ;tail call
      (apply either-bind (apply proc (~ either'objs)) procs))))

(define (either-compose proc . procs)
  (if (null? procs)
    proc
    (let1 p (apply either-compose procs)
      (^ args
        (let1 e (apply proc args)
          (unless (either? e)
            (error "mproc returned non-either object:" e))
          (if (left? e)
            e
            (apply p (~ e'objs))))))))

;;; Sequence operations

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

(define (either-filter pred either . objs)
  (assume-type either <either>)
  (if (and (right? either) (apply pred (~ either'objs)))
    either
    (list->left objs)))

(define (either-remove pred either . objs)
  (assume-type either <either>)
  (if (and (right? either) (not (apply pred (~ either'objs))))
    either
    (list->left objs)))

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

;;; Protocol conversion

(define (maybe->list maybe)
  (assume-type maybe <maybe>)
  (if (nothing? maybe) '() (~ maybe'objs)))
(define (list->maybe lis)
  (if (null? lis)
    (nothing)
    (apply just lis)))
(define (either->list either)
  (assume-type either <either>)
  (~ either'objs))
(define (list->either lis . objs)
  (if (null? lis)
    (apply left objs)
    (apply right lis)))

(define (maybe->truth maybe)
  (assume-type maybe <maybe>)
  (and (just? maybe) (%ref1 maybe)))
(define (truth->maybe obj)
  (if obj (just obj) (nothing)))
(define (either->truth either)
  (assume-type either <either>)
  (and (right? either) (%ref1 either)))
(define (truth->either obj . fail-objs)
  (if obj (right obj) (apply left fail-objs)))

(define (maybe->list-truth maybe)
  (assume-type maybe <maybe>)
  (and (just? maybe) (~ maybe'objs)))
(define (list-truth->maybe lis-or-false)
  (if lis-or-false (apply just lis-or-false) (nothing)))
(define (either->list-truth either)
  (assume-type either <either>)
  (and (right? either) (~ either'objs)))
(define (list-truth->either lis-or-false . fail-objs)
  (if lis-or-false (apply right lis-or-false) (apply left fail-objs)))

(define (maybe->generation maybe)
  (assume-type maybe <maybe>)
  (if (just? maybe)
    (%ref1 maybe)
    (eof-object)))
(define (generation->maybe obj)
  (if (eof-object? obj)
    (nothing)
    (just obj)))
(define (either->generation either)
  (assume-type either <either>)
  (if (right? either)
    (%ref1 either)
    (eof-object)))
(define (generation->either obj . objs)
  (if (eof-object? obj)
    (apply left objs)
    (right obj)))

(define (maybe->values maybe)
  (assume-type maybe <maybe>)
  (if (nothing? maybe) (values) (apply values (~ maybe'objs))))
(define (either->values either)
  (assume-type either <either>)
  (if (left? either) (values) (apply values (~ either'objs))))

(define (values->maybe producer)
  (call-with-values producer
    (^ xs (if (null? xs) (nothing) (apply just xs)))))
(define (values->either producer . objs)
  (call-with-values producer
    (^ xs (if (null? xs) (apply left objs) (apply right xs)))))

(define (maybe->two-values maybe)
  (assume-type maybe <maybe>)
  (if (nothing? maybe)
    (values #f #f)
    (values (%ref1 maybe) #t)))

(define (two-values->maybe producer)
  (receive (val has-val?) (producer)
    (if has-val? (just val) (nothing))))

(define (exception->either pred thunk)
  (guard (e [(pred e) (left e)])
    (call-with-values thunk right)))

;;; Map, fold and unfold

(define (maybe-map proc maybe)
  (assume-type maybe <maybe>)
  (if (nothing? maybe)
    maybe
    (list->just (values->list (apply proc (~ maybe'objs))))))
(define (either-map proc either)
  (assume-type either <either>)
  (if (left? either)
    either
    (list->right (values->list (apply proc (~ either'objs))))))

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
    knil
    (apply kons (append (~ maybe'objs) (list knil)))))
(define (either-fold kons knil either)
  (assume-type either <either>)
  (if (right? either)
    (apply kons (append (~ either'objs) (list knil)))
    knil))

(define (maybe-unfold stop? mapper successor . seeds)
  (if (apply stop? seeds)
    (nothing)
    (if (call-with-values (cut apply successor seeds) stop?)
      (list->just (values->list (apply mapper seeds)))
      (error "unstoppable unfold"))))

(define (either-unfold stop? mapper successor . seeds)
  (if (apply stop? seeds)
    (list->left seeds)
    (if (call-with-values (cut apply successor seeds) stop?)
      (list->right (values->list (apply mapper seeds)))
      (error "unstoppable unfold"))))

;;; Conditional syntax

(define-syntax maybe-if
  (syntax-rules ()
    [(_ expr justx nothingx)
     (if (just? (assume-type expr <maybe>)) justx nothingx)]))

(define-syntax maybe-and
  (syntax-rules ()
    [(_) (just "empty maybe-and")]
    [(_ x) (assume-type x <maybe>)]
    [(_ x . xs) (let1 t (assume-type x <maybe>)
                  (if (nothing? t) t (maybe-and . xs)))]))

(define-syntax either-and
  (syntax-rules ()
    [(_) (right "empty either-and")]
    [(_ x) (assume-type x <either>)]
    [(_ x . xs) (let1 t (assume-type x <either>)
                  (if (left? t) t (either-and . xs)))]))

(define-syntax maybe-or
  (syntax-rules ()
    [(_) (nothing)]
    [(_ x) (assume-type x <maybe>)]
    [(_ x . xs) (let1 t (assume-type x <maybe>)
                  (if (just? t) t (maybe-or . xs)))]))

(define-syntax either-or
  (syntax-rules ()
    [(_) (left "empty either-or")]
    [(_ x) (assume-type x <either>)]
    [(_ x . xs) (let1 t (assume-type x <either>)
                  (if (right? t) t (either-or . xs)))]))

(define-syntax maybe-let*
  (syntax-rules ()
    ;; empty body case
    [(_ ()) (just #t)]
    [(_ ((var expr))) (assume-type expr <maybe>)]
    [(_ ((expr)))     (assume-type expr <maybe>)]
    [(_ (var))        (assume-type var <maybe>)]
    ;; normal case
    [(_ () . body) (receive xs (let () . body) (list->just xs))]
    [(_ ((var expr) . claws) . body)
     (let1 t (assume-type expr <maybe>)
       (if (nothing? t)
         t
         (let ((var (%ref1 t)))
           (maybe-let* claws . body))))]
    [(_ ((expr) . claws) . body)
     (if (nothing? (assume-type expr <maybe>))
       (nothing)
       (maybe-let* claws . body))]
    [(_ (var . claws) . body)
     (if (nothing? (assume-type var <maybe>))
       (nothing)
       (maybe-let* claws . body))]))

(define-syntax maybe-let*-values
  (syntax-rules ()
    ;; empty body case
    [(_ ()) (just #t)]
    [(_ ((formals expr))) (rlet1 t expr
                            ;; Just make sure formals match the contained values
                            (maybe-ref t nothing (^ formals #f)))]
    [(_ ((expr)))     (assume-type expr <maybe>)]
    [(_ (var))        (assume-type var <maybe>)]
    ;; normal case
    [(_ () . body) (receive xs (let () . body) (list->just xs))]
    [(_ ((formals expr) . claws) . body)
     (maybe-ref expr nothing
                (^ formals (maybe-let*-values claws . body)))]
    [(_ ((expr) . claws) . body)
     (if (nothing? (assume-type expr <maybe>))
       (nothing)
       (maybe-let*-values claws . body))]
    [(_ (var . claws) . body)
     (if (nothing? (assume-type var <maybe>))
       (nothing)
       (maybe-let*-values claws . body))]))

(define-syntax either-let*
  (syntax-rules ()
    ;; empty body case
    [(_ ()) (right #t)]
    [(_ ((var expr))) (assume-type expr <either>)]
    [(_ ((expr)))     (assume-type expr <either>)]
    [(_ (var))        (assume-type var <either>)]
    ;; normal case
    [(_ () . body) (receive xs (let () . body) (list->right xs))]
    [(_ ((var expr) . claws) . body)
     (let1 t (assume-type expr <either>)
       (if (left? t) t (let ((var (%ref1 t)))
                         (either-let* claws . body))))]
    [(_ ((expr) . claws) . body)
     (let1 t (assume-type expr <either>)
       (if (left? t) t (either-let* claws . body)))]
    [(_ (var . claws) . body)
     (let1 t (assume-type var <either>)
       (if (left? t) t (either-let* claws . body)))]))

(define-syntax either-let*-values
  (syntax-rules ()
    ;; empty body case
    [(_ ()) (right #t)]
    [(_ ((formal expr))) (rlet1 t expr
                           ;; just to check the values match formals
                           (either-ref t (^ _ #f) (^ formals #f)))]
    [(_ ((expr)))     (rlet1 t expr
                        (assume-type t <either>))]
    [(_ (var))        (assume-type var <either>)]
    ;; normal case
    [(_ () . body) (receive xs (let () . body) (list->right xs))]
    [(_ ((formals expr) . claws) . body)
     (either-ref expr left
                 (^ formals (either-let*-values claws . body)))]
    [(_ ((expr) . claws) . body)
     (let1 t (assume-type expr <either>)
       (if (left? t) t (either-let*-values claws . body)))]
    [(_ (var . claws) . body)
     (if (left? (assume-type var <either>))
       var
       (either-let*-values claws . body))]))

(define-syntax either-guard
  (syntax-rules ()
    [(_ pred-expr . body)
     (guard (e [(pred-expr e) (left e)])
       (receive xs (begin . body)
         (list->right xs)))]))

;;; Trivalent logic

(define (tri-not maybe)
  (assume-type maybe <maybe>)
  (if (nothing? maybe)
    maybe
    (just (not (%ref1 maybe)))))

(define (tri=? maybe . maybes)
  (define (rec val maybe maybes)
    (if (nothing? (assume-type maybe <maybe>))
      (just #f)
      (if (boolean=? val (boolean (%ref1 maybe)))
        (if (null? maybes)
          (just #t)
          (rec val (car maybes) (cdr maybes)))
        (just #f))))

  (if (nothing? (assume-type maybe <maybe>))
    (just #f)
    (let1 v (%ref1 maybe)
      (if (null? maybes)
        (just #t)
        (rec v (car maybes) (cdr maybes))))))

(define (tri-and . maybes)
  (define (rec maybes)
    (if (null? maybes)
      (just #t)
      (let ([maybe (car maybes)]
            [maybes (cdr maybes)])
        (if (nothing? (assume-type maybe <maybe>))
          maybe
          (if-let1 v (%ref1 maybe)
            (rec maybes)
            maybe)))))                  ; this must be #<just #f>
  (rec maybes))

(define (tri-or . maybes)
  (define (rec maybes)
    (if (null? maybes)
      (just #f)
      (let ([maybe (car maybes)]
            [maybes (cdr maybes)])
        (if (nothing? (assume-type maybe <maybe>))
          maybe
          (if-let1 v (%ref1 maybe)
            maybe
            (rec maybes))))))
  (rec maybes))

(define (tri-merge . maybes)
  (define (rec maybes)
    (if (null? maybes)
      (nothing)
      (let ([maybe (car maybes)]
            [maybes (cdr maybes)])
        (if (nothing? (assume-type maybe <maybe>))
          (rec maybes)
          maybe))))
  (rec maybes))
