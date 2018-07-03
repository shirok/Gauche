;;;
;;; compile-4.scm - The compiler: Pass 4
;;;
;;;   Copyright (c) 2004-2018  Shiro Kawai  <shiro@acm.org>
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

;;===============================================================
;; Pass 4.  Lambda lifting
;;

;; First we traverse down the IForm and find free local variables
;; for each lambda node.  Within this traversal, found $lambda nodes
;; are chained into the first element of label-dic.
;;
;; Once all free lvars are sorted out, we look at the list of $lambda
;; nodes and determine the ones that doesn't need to form a closure.
;; They are to be bound to a freshly created global identifier.  If other
;; $lambda nodes have a reference to the lifted lambda node through
;; local variables, they are substituted to the reference to this global
;; identifier.
;;
;; Note for the reader of this code: The term "lambda lifting" usually
;; includes a transformation that substitutes closed variables for
;; arguments.  We don't do such transformation so far.  It trades the
;; cost of closure allocation for pushing extra arguments.  It may be
;; a win if the closure is allocated lots of times.  OTOH, if the closure
;; is created only a few times, but called lots of times, the overhead of
;; extra arguments may exceed the gain by not allocating the closure.
;;
;; Pass4 is done in three steps.
;;
;; - The first step, pass4/scan, recursively descends the IForm and determine
;;   a set of free variables for each $LAMBDA nodes.  It also collects lambda
;;   nodes inside the iform into label-dic-info.
;;   In this pass, we mark the toplevel lambda node by setting
;;   $lambda-lifted-var to #t.  All other lambda nodes have #f at this moment.
;;
;; - The second step, pass4/lift, takes a set of $LAMBDA nodes in the IForm
;;   and finds which $LAMBDA nodes can be lifted.  When lifted, we set
;;   its $lambda-lifted-var with the variable that bound to the lifted lambda.
;;
;; - The third step, pass4/subst, walks the IForm again, and replaces the
;;   occurence of $LAMBDA nodes to be lifted for $LREFs.  Then wraps the
;;   entire iform with $LET to save the lifted lambdas.
;;
;; This pass introduces CL-like toplevel environment, e.g.
;;
;;   ($let rec (<lvar> ...)
;;             (($lambda ...) ...)   ; lifted lambdas
;;     ($define ...)                 ; toplevel definition
;;     )
;;
;; This works, since we already dissolved internal defines, and the $DEFINE
;; we have is all toplevel defines, and the toplevel variable to be bound
;; is already resolved to the identifier.
;; NB: Whether we use 'rec' or not in this outermost lambda doesn't matter,
;; for we directly refer to <lvar>s.  We just use 'rec' to remind it is
;; conceptually a letrec.

(define-inline (pass4/add-lvar lvar bound free)
  (if (or (memq lvar bound) (memq lvar free)) free (cons lvar free)))

;; Pass 4 entry point.  Returns IForm and list of lifted lvars
(define (pass4 iform module)
  (if (vm-compiler-flag-no-lifting?)
    iform
    (let1 dic (make-label-dic '())
      (pass4/scan iform '() '() #t dic) ; Mark free variables
      (let1 lambda-nodes (label-dic-info dic)
        (if (or (null? lambda-nodes)
                (and (null? (cdr lambda-nodes)) ; iform has only a toplevel lambda
                     ($lambda-lifted-var (car lambda-nodes))))
          iform                           ;shortcut
          (let1 lifted (pass4/lift lambda-nodes module)
            (if (null? lifted)
              iform                       ;shortcut
              (let1 iform. (pass4/subst iform (make-label-dic '()))
                ($let #f 'rec
                      (map (^[x] ($lambda-lifted-var x)) lifted)
                      lifted
                      iform.)))))))))

(define (pass4/lifted-define lambda-node)
  ($define ($lambda-src lambda-node) '(const)
           ($lambda-lifted-var lambda-node)
           lambda-node))

;; Pass4 step1 - scan
;;   bs - List of lvars whose binding is introduced in the current scope.
;;   fs - List of free lvars found so far in the current scope.
;;   t? - #t if we're in the top level, #f otherwise.
;;   labels - label-dic.  the info field is used to hold $LAMBDA nodes.
;; Eacl call returns a list of free lvars.

(define-macro (pass4/scan* iforms bs fs t? labels)
  (let1 iforms. (gensym)
    `(let1 ,iforms. ,iforms
       (cond [(null? ,iforms.) ,fs]
             [(null? (cdr ,iforms.))
              (pass4/scan (car ,iforms.) ,bs ,fs ,t? ,labels)]
             [else
              (let loop ([,iforms. ,iforms.] [,fs ,fs])
                (if (null? ,iforms.)
                  ,fs
                  (loop (cdr ,iforms.)
                        (pass4/scan (car ,iforms.) ,bs ,fs ,t? ,labels))))]))))

(define/case (pass4/scan iform bs fs t? labels)
  (iform-tag iform)
  [($DEFINE) (unless t? (error "[internal] pass4 $DEFINE in non-toplevel"))
             (pass4/scan ($define-expr iform) bs fs #t labels)]
  [($LREF)   (pass4/add-lvar ($lref-lvar iform) bs fs)]
  [($LSET)   (let1 fs (pass4/scan ($lset-expr iform) bs fs t? labels)
               (pass4/add-lvar ($lset-lvar iform) bs fs))]
  [($GSET)   (pass4/scan ($gset-expr iform) bs fs t? labels)]
  [($IF)     (let* ([fs (pass4/scan ($if-test iform) bs fs t? labels)]
                    [fs (pass4/scan ($if-then iform) bs fs t? labels)])
               (pass4/scan ($if-else iform) bs fs t? labels))]
  [($LET)    (let* ([new-bs (append ($let-lvars iform) bs)]
                    [bs (if (memv ($let-type iform) '(rec rec*)) new-bs bs)]
                    [fs (pass4/scan* ($let-inits iform) bs fs t? labels)])
               (pass4/scan ($let-body iform) new-bs fs #f labels))]
  [($RECEIVE)(let ([fs (pass4/scan ($receive-expr iform) bs fs t? labels)]
                   [bs (append ($receive-lvars iform) bs)])
               (pass4/scan ($receive-body iform) bs fs #f labels))]
  [($LAMBDA) (let1 inner-fs (pass4/scan ($lambda-body iform)
                                        ($lambda-lvars iform) '() #f labels)
               ;; If this $LAMBDA is outermost in the original expression,
               ;; we don't need to lift it, nor need to set free-lvars.
               ;; We just mark it by setting lifted-var to #t so that
               ;; pass4/lift phase can treat it specially.
               (unless (eq? ($lambda-flag iform) 'dissolved)
                 (label-dic-info-push! labels iform) ;save the lambda node
                 (when t?                            ;mark this is toplevel
                   ($lambda-lifted-var-set! iform #t)))
               (cond [t? '()]
                     [else ($lambda-free-lvars-set! iform inner-fs)
                           (let loop ([inner-fs inner-fs] [fs fs])
                             (if (null? inner-fs)
                               fs
                               (loop (cdr inner-fs)
                                     (pass4/add-lvar (car inner-fs) bs fs))))]))]
  [($LABEL)  (cond [(label-seen? labels iform) fs]
                   [else (label-push! labels iform)
                         (pass4/scan ($label-body iform) bs fs #f labels)])]
  [($SEQ)    (pass4/scan* ($seq-body iform) bs fs t? labels)]
  [($CALL)   (let1 fs (if (eq? ($call-flag iform) 'jump)
                        fs
                        (pass4/scan ($call-proc iform) bs fs t? labels))
               (pass4/scan* ($call-args iform) bs fs t? labels))]
  [($ASM)    (pass4/scan* ($asm-args iform) bs fs t? labels)]
  [($PROMISE)(pass4/scan ($promise-expr iform) bs fs t? labels)]
  [($CONS $APPEND $MEMV $EQ? $EQV?) (pass4/scan2 iform bs fs t? labels)]
  [($VECTOR $LIST $LIST*) (pass4/scan* ($*-args iform) bs fs t? labels)]
  [($LIST->VECTOR) (pass4/scan ($*-arg0 iform) bs fs t? labels)]
  [else fs])

(define (pass4/scan2 iform bs fs t? labels)
  (let1 fs (pass4/scan ($*-arg0 iform) bs fs t? labels)
    (pass4/scan ($*-arg1 iform) bs fs t? labels)))

;; Sort out the liftable lambda nodes.
;; Returns a list of lambda nodes, in each of which $lambda-lifted-var
;; contains an identifier.
;;
;; At this moment, we only detect closures without free variables,
;; or self-recursive closures.
;;
;; Eventually we want to detect mutual recursive case like this:
;;
;;  (define (foo ...)
;;    (define (a x)  ... (b ...) ...)
;;    (define (b y)  ... (a ...) ...)
;;    ...)
;;
;; If a's only free variable is b, and b's only free variable is a,
;; then we can lift both nodes to the toplevel.
;;
;; Tentative algorithm:
;;  - Create a directed graph consists of free lvars and lambda nodes.
;;    An edge from an lvar to a lambda node if the lvar is free in
;;    the lambda node.  An edge from lambda node to an lvar if the lambda
;;    node is bound to the lvar.
;;  - Find lvars without incoming edge, and remove them and all reachable
;;    nodes from them.
;;  - The lambda nodes in the remaining graph are the ones we can lift.
;;

(define (pass4/lift lambda-nodes module)
  (let1 top-name #f
    ;; Find a toplevel $lambda node (marked by #t in lifted-var).
    ;; Its name can be used to generate names for lifted lambdas.
    (let loop ([lms lambda-nodes])
      (when (pair? lms)
        (or (and-let* ([ ($lambda-lifted-var (car lms)) ]
                       [n ($lambda-name (car lms))])
              (set! top-name (if (identifier? n)
                               (identifier-name n)
                               n)))
            (loop (cdr lms)))))
    (rlet1 results '()
      (let loop ([lms lambda-nodes])
        (cond [(null? lms)]
              [($lambda-lifted-var (car lms)) ; this is toplevel node
               ($lambda-lifted-var-set! (car lms) #f)
               (loop (cdr lms))]
              [else
               (let* ([lm (car lms)]
                      [fvs ($lambda-free-lvars lm)])
                 (when (or (null? fvs)
                           (and (null? (cdr fvs))
                                (lvar-immutable? (car fvs))
                                (eq? (lvar-initval (car fvs)) lm)))
                   (let1 lvar (make-lvar (gensym))
                     ($lambda-name-set! lm (list top-name
                                                 (or ($lambda-name lm)
                                                     (lvar-name lvar))))
                     ($lambda-lifted-var-set! lm lvar)
                     (push! results lm)))
                 (loop (cdr lms)))])))))

;; Final touch of pass4 - replace lifted lambda nodes to $GREFs.
;; Returns (possibly modified) IForm.
(define-macro (pass4/subst! access-form labels)
  (match-let1 (accessor expr) access-form
    (let ([orig (gensym)]
          [result (gensym)]
          [setter (if (eq? accessor 'car)
                    'set-car!
                    (string->symbol #"~|accessor|-set!"))])
      `(let* ([,orig (,accessor ,expr)]
              [,result (pass4/subst ,orig ,labels)])
         (unless (eq? ,orig ,result)
           (,setter ,expr ,result))
         ,expr))))

(define-macro (pass4/subst*! iforms labels)
  (let1 iforms. (gensym)
    `(let1 ,iforms. ,iforms
       (cond [(null? ,iforms.)]
             [(null? (cdr ,iforms.)) (pass4/subst! (car ,iforms.) ,labels)]
             [else
              (let loop ([,iforms. ,iforms.])
                (unless (null? ,iforms.)
                  (pass4/subst! (car ,iforms.) ,labels)
                  (loop (cdr ,iforms.))))]))))

(define/case (pass4/subst iform labels)
  (iform-tag iform)
  [($DEFINE) (pass4/subst! ($define-expr iform) labels)]
  [($LREF)   (or (and-let* ([ (lvar-immutable? ($lref-lvar iform)) ]
                            [init (lvar-initval ($lref-lvar iform))]
                            [ (vector? init) ]
                            [ (has-tag? init $LAMBDA) ]
                            [lifted ($lambda-lifted-var init)])
                   (lvar-ref--! ($lref-lvar iform))
                   ($lref-lvar-set! iform lifted)
                   (lvar-ref++! lifted)
                   iform)
                 iform)]
  [($LSET)   (pass4/subst! ($lset-expr iform) labels)]
  [($GSET)   (pass4/subst! ($gset-expr iform) labels)]
  [($IF)     (pass4/subst! ($if-test iform) labels)
             (pass4/subst! ($if-then iform) labels)
             (pass4/subst! ($if-else iform) labels)]
  [($LET)    (pass4/subst*! ($let-inits iform) labels)
             (pass4/subst! ($let-body iform) labels)]
  [($RECEIVE)(pass4/subst! ($receive-expr iform) labels)
             (pass4/subst! ($receive-body iform) labels)]
  [($LAMBDA) (pass4/subst! ($lambda-body iform) labels)
             (or (and-let* ([lifted ($lambda-lifted-var iform)])
                   ($lref lifted))
                 iform)]
  [($LABEL)  (unless (label-seen? labels iform)
               (label-push! labels iform)
               (pass4/subst! ($label-body iform) labels))
             iform]
  [($SEQ)    (pass4/subst*! ($seq-body iform) labels) iform]
  [($CALL)   (pass4/subst*! ($call-args iform) labels)
             (pass4/subst! ($call-proc iform) labels)]
  [($ASM)    (pass4/subst*! ($asm-args iform) labels) iform]
  [($PROMISE)(pass4/subst! ($promise-expr iform) labels)]
  [($CONS $APPEND $MEMV $EQ? $EQV?) (pass4/subst! ($*-arg0 iform) labels)
             (pass4/subst! ($*-arg1 iform) labels)]
  [($VECTOR $LIST $LIST*) (pass4/subst*! ($*-args iform) labels) iform]
  [($LIST->VECTOR) (pass4/subst! ($*-arg0 iform) labels)]
  [else iform])

