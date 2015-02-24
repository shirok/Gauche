;;;
;;; liblist.scm - builtin list procedures
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

(select-module gauche.internal)

(inline-stub
 (declcode (.include <gauche/vminsn.h>)))

;;
;; R5RS Standard procs
;;

(select-module scheme)
(define-cproc pair? (obj) ::<boolean> :fast-flonum :constant
  (inliner PAIRP) SCM_PAIRP)
(define-cproc cons (obj1 obj2) (inliner CONS) Scm_Cons)
(define-cproc car (obj::<pair>) :constant
  (inliner CAR) (setter set-car!) SCM_CAR)
(define-cproc cdr (obj::<pair>) :constant
  (inliner CDR) (setter set-cdr!) SCM_CDR)
(define-cproc set-car! (obj::<pair> value) ::<void> SCM_SET_CAR)
(define-cproc set-cdr! (obj::<pair> value) ::<void> SCM_SET_CDR)

(inline-stub
 "#define CXR_SETTER(PRE, pre, tail) \
  ScmObj cell = Scm_C##tail##r(obj); \
  if (!SCM_PAIRP(cell)) \
    Scm_Error(\"can't set c\" #pre #tail \"r of %S\", obj); \
  SCM_SET_C##PRE##R(cell, value);
"
 )
(define-cproc caar (obj) :fast-flonum :constant
  (inliner CAAR) (setter (obj value) ::<void> (CXR_SETTER A a a)) Scm_Caar)
(define-cproc cadr (obj) :fast-flonum :constant
  (inliner CADR) (setter (obj value) ::<void> (CXR_SETTER A a d)) Scm_Cadr)
(define-cproc cdar (obj) :fast-flonum :constant
  (inliner CDAR) (setter (obj value) ::<void> (CXR_SETTER D d a)) Scm_Cdar)
(define-cproc cddr (obj) :fast-flonum :constant
  (inliner CDDR) (setter (obj value) ::<void> (CXR_SETTER D d d)) Scm_Cddr)

;; NB: we avoid using getter-with-setter here, since
;;   - The current compiler doesn't take advantage of locked setters
;;   - Using getter-with-setter loses the inferred closure name
;; But this may change in future, of course.
(select-module gauche)
(define-syntax %define-cxr
  (syntax-rules ()
    ((_ name a b)
     (begin
       (define-inline (name x) (a (b x)))
       (define-in-module scheme name name)
       (set! (setter name) (^[x v] (set! (a (b x)) v)))))))

(%define-cxr caaar  car  caar)
(%define-cxr caadr  car  cadr)
(%define-cxr cadar  car  cdar)
(%define-cxr caddr  car  cddr)
(%define-cxr cdaar  cdr  caar)
(%define-cxr cdadr  cdr  cadr)
(%define-cxr cddar  cdr  cdar)
(%define-cxr cdddr  cdr  cddr)
(%define-cxr caaaar caar caar)
(%define-cxr caaadr caar cadr)
(%define-cxr caadar caar cdar)
(%define-cxr caaddr caar cddr)
(%define-cxr cadaar cadr caar)
(%define-cxr cadadr cadr cadr)
(%define-cxr caddar cadr cdar)
(%define-cxr cadddr cadr cddr)
(%define-cxr cdaaar cdar caar)
(%define-cxr cdaadr cdar cadr)
(%define-cxr cdadar cdar cdar)
(%define-cxr cdaddr cdar cddr)
(%define-cxr cddaar cddr caar)
(%define-cxr cddadr cddr cadr)
(%define-cxr cdddar cddr cdar)
(%define-cxr cddddr cddr cddr)

(define-cproc null? (obj) ::<boolean> :fast-flonum :constant
  (inliner NULLP) SCM_NULLP)
(define-cproc list? (obj) ::<boolean> :fast-flonum :constant
  SCM_PROPER_LIST_P)
(define-cproc list (:rest args) (inliner LIST) (return args))

(define-cproc length (list) ::<long> :constant (inliner LENGTH)
  (let* ([len::long (Scm_Length list)])
    (when (< len 0) (Scm_Error "bad list: %S" list))
    (return len)))
(define-cproc length<=? (list k::<fixnum>) ::<boolean> :constant
  (dolist [_ list] (when (<= (post-- k) 0) (return FALSE)))
  (return TRUE))

(define-cproc append (:rest lists) (inliner APPEND) Scm_Append)
(define-cproc reverse (list::<list> :optional (tail ())) Scm_Reverse2)

(define-cproc list-tail (list k::<fixnum> :optional fallback) :constant
  Scm_ListTail)
;;We need to define list-set! as cproc in order to use it in the setter clause
;;of list-ref.  This limitation of cgen.stub should be removed in future.
;;(define (list-set! lis k v) (set-car! (list-tail lis k) v))
(define-cproc list-set! (lis k::<fixnum> v) ::<void>
  (let* ([p (Scm_ListTail lis k SCM_FALSE)])
    (if (SCM_PAIRP p)
      (SCM_SET_CAR p v)
      (Scm_Error "list-set!: index out of bound: %d" k))))
(define-cproc list-ref (list k::<fixnum> :optional fallback) :constant
  (setter list-set!)
  Scm_ListRef)

(define-cproc memq (obj list::<list>) :constant (inliner MEMQ) Scm_Memq)
(define-cproc memv (obj list::<list>) :constant (inliner MEMV) Scm_Memv)

(define-cproc assq (obj alist::<list>) :constant (inliner ASSQ) Scm_Assq)
(define-cproc assv (obj alist::<list>) :constant (inliner ASSV) Scm_Assv)

(select-module gauche.internal)
;; Actual member and assoc is defined blow.
(define-cproc %member (obj list::<list>)
  (return (Scm_Member obj list SCM_CMP_EQUAL)))
(define-cproc %assoc (obj alist::<list>)
  (return (Scm_Assoc obj alist SCM_CMP_EQUAL)))

;;
;; Some extra procedures
;;

(select-module gauche)
(define-cproc length+ (list) :constant ;; srfi-1
  (let* ([i::int (Scm_Length list)])
    (if (< i 0) (return SCM_FALSE) (return (Scm_MakeInteger i)))))

(define-cproc proper-list? (obj)   ::<boolean> :constant SCM_PROPER_LIST_P)
(define-cproc dotted-list? (obj)   ::<boolean> :constant SCM_DOTTED_LIST_P)
(define-cproc circular-list? (obj) ::<boolean> :constant SCM_CIRCULAR_LIST_P)
(define-cproc make-list (len::<fixnum> :optional (fill #f)) Scm_MakeList)
(define-cproc acons (caa cda cd) Scm_Acons)
(define-cproc last-pair (list) :constant Scm_LastPair)
(define-cproc list-copy (list) Scm_CopyList)

(define-cproc list* (:rest args)
  (inliner LIST-STAR)
  (let* ([head '()] [tail '()])
    (when (SCM_PAIRP args)
      (dopairs [cp args]
        (unless (SCM_PAIRP (SCM_CDR cp))
          (if (SCM_NULLP head)
            (set! head (SCM_CAR cp))
            (SCM_SET_CDR tail (SCM_CAR cp)))
          (break))
        (SCM_APPEND1 head tail (SCM_CAR cp))))
    (return head)))

(define-cproc append! (:rest list)
  (let* ([h '()] [t '()])
    (dopairs [cp list]
      ;; allow non-list argument at the last position
      (when (and (not (SCM_PAIRP (SCM_CAR cp)))
                 (SCM_NULLP (SCM_CDR cp)))
        (if (SCM_NULLP h)
          (set! h (SCM_CAR cp))
          (SCM_SET_CDR t (SCM_CAR cp)))
        (break))
      (SCM_APPEND h t (SCM_CAR cp)))
    (return h)))

(define-cproc reverse! (list :optional (tail ())) Scm_Reverse2X)

(define-cproc monotonic-merge (sequences::<list>) Scm_MonotonicMerge1)

(select-module gauche.internal)
(define-in-module scheme (map proc lis . more)
  (if (null? more)
    (let loop ([xs lis] [r '()])
      (cond [(pair? xs) (loop (cdr xs) (cons (proc (car xs)) r))]
            [(null? xs) (reverse r)]
            [else (error "improper list not allowed:" lis)]))
    (let loop ([xss (cons lis more)] [r '()])
      (receive (cars cdrs) (%zip-nary-args xss)
        (if (not cars)
          (reverse r)
          (loop cdrs (cons (apply proc cars) r)))))))

(define-in-module scheme (for-each proc lis . more)
  (if (null? more)
    (let loop ([xs lis])
      (cond [(pair? xs) (proc (car xs)) (loop (cdr xs))]
            [(null? xs) (undefined)]
            [else (error "improper list not allowed:" lis)]))
    (let loop ([xss (cons lis more)])
      (receive (cars cdrs) (%zip-nary-args xss)
        (unless (not cars)
          (apply proc cars)
          (loop cdrs))))))

(select-module gauche)
(define-inline (null-list? l)           ;srfi-1
  (cond [(null? l)]
        [(pair? l) #f]
        [else (error "argument must be a list, but got:" l)]))

(define cons* list*)                    ;srfi-1

(define (last lis) (car (last-pair lis))) ;srfi-1

(define (iota count :optional (start 0) (step 1)) ;srfi-1
  (unless (and (integer? count) (>= count 0))
    (error "count must be nonnegative integer: " count))
  (if (and (exact? start) (exact? step))
    ;; we allow inexact integer as 'count', for the consistency of
    ;; giota and liota in which we can also accept +inf.0 as count.
    (let1 count (exact count)
      (do ([c count (- c 1)]
           [v (+ start (* (- count 1) step)) (- v step)]
           [r '() (cons v r)])
          [(<= c 0) r]))
    ;; for inexact numbers, we use multiplication to avoid error accumulation.
    (do ([c count (- c 1)]
         [r '() (cons (+ start (* (- c 1) step)) r)])
        [(<= c 0) r])))

(select-module gauche.internal)
(inline-stub
 ;; translate cmpmode argument
 (define-cfn getcmpmode (opt) ::int :static
   (cond
    [(or (SCM_UNBOUNDP opt) (SCM_EQ opt 'equal?)) (return SCM_CMP_EQUAL)]
    [(SCM_EQ opt 'eq?) (return SCM_CMP_EQ)]
    [(SCM_EQ opt 'eqv?) (return SCM_CMP_EQV)]
    [else (Scm_Error "unrecognized compare mode: %S" opt) (return 0)]))
 )

(define-cproc %delete (obj list::<list> :optional cmpmode)
  (return (Scm_Delete obj list (getcmpmode cmpmode))))
(define-cproc %delete! (obj list::<list> :optional cmpmode)
  (return (Scm_DeleteX obj list (getcmpmode cmpmode))))
(define-cproc %delete-duplicates (list::<list> :optional cmpmode)
  (return (Scm_DeleteDuplicates list (getcmpmode cmpmode))))
(define-cproc %delete-duplicates! (list::<list> :optional cmpmode)
  (return (Scm_DeleteDuplicatesX list (getcmpmode cmpmode))))
(define-cproc %alist-delete (elt list::<list> :optional cmpmode)
  (return (Scm_AssocDelete elt list (getcmpmode cmpmode))))
(define-cproc %alist-delete! (elt list::<list> :optional cmpmode)
  (return (Scm_AssocDeleteX elt list (getcmpmode cmpmode))))

(define-in-module gauche.internal (%zip-nary-args arglists . seed)
  (let loop ([as arglists]
             [cars '()]
             [cdrs '()])
    (cond [(null? as)
           (values (reverse! (if (null? seed) cars (cons (car seed) cars)))
                   (reverse! cdrs))]
          [(null? (car as)) (values #f #f)] ;;exhausted
          [(pair? (car as))
           (loop (cdr as) (cons (caar as) cars) (cons (cdar as) cdrs))]
          [else
           (error "argument lists contained an improper list ending with:"
                  (car as))])))

;; In the common case, these procs uses Gauche native, even not loading
;; the generic filter routine.
(define-syntax %case-by-cmp
  (syntax-rules ()
    [(_ args = eq-case eqv-case equal-case default-case)
     (let1 = (if (pair? args) (car args) equal?)
       (cond [(eq? = eq?)    eq-case]
             [(eq? = eqv?)   eqv-case]
             [(eq? = equal?) equal-case]
             [else default-case]))]))

(define-in-module gauche (delete x lis . args)
  (%case-by-cmp args =
                (%delete x lis 'eq?)
                (%delete x lis 'eqv?)
                (%delete x lis 'equal?)
                (filter (^y (not (= x y))) lis)))

(define-in-module gauche (delete! x lis . args)
  (%case-by-cmp args =
                (%delete! x lis 'eq?)
                (%delete! x lis 'eqv?)
                (%delete! x lis 'equal?)
                (filter! (^y (not (= x y))) lis)))

(define-in-module scheme (member x lis . args)
  (%case-by-cmp args =
                (memq x lis)
                (memv x lis)
                (%member x lis)
                (find-tail (^y (= x y)) lis)))

(define-in-module gauche (delete-duplicates lis . args)
  (%case-by-cmp args =
                (%delete-duplicates lis 'eq?)
                (%delete-duplicates lis 'eqv?)
                (%delete-duplicates lis 'equal?)
                (let recur ([lis lis])
                  (if (null-list? lis) lis
                      (let* ([x (car lis)]
                             [tail (cdr lis)]
                             [new-tail (recur (delete x tail =))])
                        (if (eq? tail new-tail) lis (cons x new-tail)))))))

(define-in-module gauche (delete-duplicates! lis . args)
  (%case-by-cmp args =
                (%delete-duplicates! lis 'eq?)
                (%delete-duplicates! lis 'eqv?)
                (%delete-duplicates! lis 'equal?)
                (let recur ((lis lis))
                  (if (null-list? lis) lis
                      (let* ((x (car lis))
                             (tail (cdr lis))
                             (new-tail (recur (delete! x tail =))))
                        (if (eq? tail new-tail) lis (cons x new-tail)))))))

;;
;; Higher-order stuff
;;

(select-module gauche)

(define (any pred lis . more)
  (if (null? more)
    (and (not (null-list? lis))
         (let loop ((head (car lis)) (tail (cdr lis)))
           (cond [(null-list? tail) (pred head)] ; tail call
                 [(pred head)]
                 [else (loop (car tail) (cdr tail))])))
    (let loop ([liss (cons lis more)])
      (receive (cars cdrs) ((with-module gauche.internal %zip-nary-args) liss)
        (cond [(not cars) #f]
              [(apply pred cars)]
              [else (loop cdrs)])))))

(define (every pred lis . more)
  (if (null? more)
    (or (null-list? lis)
        (let loop ([head (car lis)] [tail (cdr lis)])
          (cond [(null-list? tail) (pred head)] ; tail call
                [(not (pred head)) #f]
                [else (loop (car tail) (cdr tail))])))
    (receive (heads tails)
        ((with-module gauche.internal %zip-nary-args) (cons lis more))
      (or (not heads)
          (let loop ([heads heads] [tails tails])
            (receive (next-heads next-tails)
                ((with-module gauche.internal %zip-nary-args) tails)
              (if next-heads
                  (and (apply pred heads)
                       (loop next-heads next-tails))
                  (apply pred heads))))))))

(define (filter pred lis)
  (let loop ([lis lis] [r '()])
    (cond [(null-list? lis) (reverse r)]
          [(pred (car lis)) (loop (cdr lis) (cons (car lis) r))]
          [else (loop (cdr lis) r)])))

(define (filter! pred lis)
  (define (keep! prev lis)
    (when (pair? lis)
      (if (pred (car lis))
        (keep! lis (cdr lis))
        (skip! prev (cdr lis)))))
  (define (skip! prev lis)
    (let loop ([lis lis])
      (cond [(not (pair? lis)) (set-cdr! prev lis)]
            [(pred (car lis)) (set-cdr! prev lis) (keep! lis (cdr lis))]
            [else (loop (cdr lis))])))
  (let restart ([ans lis])
    (cond [(null-list? ans) ans]
          [(not (pred (car ans))) (restart (cdr ans))]
          [else (keep! ans (cdr ans)) ans])))

(define (remove  pred l) (filter  (^x (not (pred x))) l))
(define (remove! pred l) (filter! (^x (not (pred x))) l))

(define (filter-map fn lis . more)
  (if (null? more)
    (let loop ([lis lis] [r '()])
      (cond [(null-list? lis) (reverse r)]
            [(fn (car lis)) => (^x (loop (cdr lis) (cons x r)))]
            [else (loop (cdr lis) r)]))
    (let loop ([liss (cons lis more)] [r '()])
      (receive (cars cdrs)
          ((with-module gauche.internal %zip-nary-args) liss)
        (cond [(not cars) (reverse r)]
              [(apply fn cars) => (^x (loop cdrs (cons x r)))]
              [else (loop cdrs r)])))))

(define (fold kons knil lis . more)
  (if (null? more)
    (let loop ([lis lis] [knil knil])
      (if (null-list? lis) knil (loop (cdr lis) (kons (car lis) knil))))
    (let loop ([liss (cons lis more)] [knil knil])
      (receive (cars cdrs)
          ((with-module gauche.internal %zip-nary-args) liss knil)
        (if cars
          (loop cdrs (apply kons cars))
          knil)))))

(define (fold-left snok knil lis . more)
  (if (null? more)
    (let loop ([lis lis] [knil knil])
      (if (null-list? lis) knil (loop (cdr lis) (snok knil (car lis)))))
    (let loop ([liss (cons lis more)] [knil knil])
      (receive (cars- cdrs)
          ((with-module gauche.internal %zip-nary-args) liss)
        (if cars-
          (loop cdrs (apply snok knil cars-))
          knil)))))

(define (fold-right kons knil lis . more)
  (if (null? more)
    (let rec ([lis lis])
      (if (null-list? lis)
        knil
        (kons (car lis) (rec (cdr lis)))))
    (let rec ([liss (cons lis more)])
      (receive (cars cdrs) ((with-module gauche.internal %zip-nary-args) liss)
        (if cars
          (apply kons (append! cars (list (rec cdrs))))
          knil)))))

(define (count pred lis . more)
  (if (null? more)
    (let rec ([lis lis] [cnt 0])
      (if (null-list? lis)
        cnt
        (rec (cdr lis) (if (pred (car lis)) (+ cnt 1) cnt))))
    (let rec ([liss (cons lis more)] [cnt 0])
      (receive (cars cdrs) ((with-module gauche.internal %zip-nary-args) liss)
        (if cars
          (rec cdrs (if (apply pred cars) (+ cnt 1) cnt))
          cnt)))))

(define (reduce f ridentity lis)
  (if (null-list? lis)
    ridentity
    (fold f (car lis) (cdr lis))))

(define (reduce-right f ridentity lis)
  (if (null-list? lis)
    ridentity
    (let rec ([head (car lis)] [lis (cdr lis)])
      (if (pair? lis)
        (f head (rec (car lis) (cdr lis)))
        head))))

(define (append-reverse list tail)  (reverse list tail)) ;srfi-1 compat
(define (append-reverse! list tail) (reverse! list tail));srfi-1 compat

(define (concatenate  lists) (reduce-right append  '() lists))
(define (concatenate! lists) (reduce-right append! '() lists))

(define (append-map f lis . lists)  (concatenate  (apply map f lis lists)))
(define (append-map! f lis . lists) (concatenate! (apply map f lis lists)))

(define (map* fn tail-fn lis . more)
  (if (null? more)
    (let rec ([xs lis] [rs '()])
      (if (pair? xs)
        (rec (cdr xs) (cons (fn (car xs)) rs))
        (reverse rs (tail-fn xs))))
    (let rec ([xss (cons lis more)] [rs '()])
      (if (every pair? xss)
        (receive (cars cdrs) ((with-module gauche.internal %zip-nary-args) xss)
          (rec cdrs (cons (apply fn cars) rs)))
        (reverse rs (apply tail-fn xss))))))

(define (find pred lis)
  (let loop ([lis lis])
    (cond [(not (pair? lis)) #f]
          [(pred (car lis)) (car lis)]
          [else (loop (cdr lis))])))

(define (find-tail pred lis)
  (let loop ([lis lis])
    (cond [(not (pair? lis)) #f]
          [(pred (car lis)) lis]
          [else (loop (cdr lis))])))

(define (split-at lis i)
  (let loop ([i i] [rest lis] [r '()])
    (cond [(= i 0) (values (reverse! r) rest)]
          [(null? rest) (error "given list is too short:" lis)]
          [else (loop (- i 1) (cdr rest) (cons (car rest) r))])))

(define (split-at! lis i)
  (let loop ([i i] [rest lis] [prev #f])
    (cond [(= i 0) (if prev
                     (begin (set-cdr! prev '()) (values lis rest))
                     (values '() rest))]
          [(null? rest) (error "given list is too short:" lis)]
          [else (loop (- i 1) (cdr rest) rest)])))

;; partition is here, for gauche.procedure has partition$ and we don't
;; want it to depend on srfi-1.  partition! is left in srfi-1, for its
;; optimized version is rather complicated.
(define (partition pred lis)
  (let rec ([lis lis] [xs '()] [ys '()])
    (if (null-list? lis)
      (values (reverse! xs) (reverse! ys))
      (if (pred (car lis))
        (rec (cdr lis) (cons (car lis) xs) ys)
        (rec (cdr lis) xs (cons (car lis) ys))))))

(define (take list k)
  (let loop ([lis list] [r '()] [j k])
    (cond [(= j 0) (reverse! r)]
          [(pair? lis) (loop (cdr lis) (cons (car lis) r) (- j 1))]
          [else (errorf "take: input list is too short (expected at least \
                         ~a elements, but only ~a elements long): ~,,,,70s"
                        k (- k j) list)])))

(define drop list-tail)  ; srfi-1

(define (take-right lis k)
  (let loop ([p0 (list-tail lis k)] [p1 lis])
    (if (pair? p0) (loop (cdr p0) (cdr p1)) p1)))

(define (drop-right lis k)
  (let rec ([p0 (list-tail lis k)] [p1 lis])
    (if (pair? p0) (cons (car p1) (rec (cdr p0) (cdr p1))) '())))

(define (take! lis k)
  (cond [(zero? k) '()]
        [else (set-cdr! (list-tail lis (- k 1)) '()) lis]))

(define (drop-right! lis k)
  (let1 p0 (list-tail lis k)
    (if (pair? p0)
      (let loop ([p0 (cdr p0)] [p1 lis])
        (if (pair? p0)
          (loop (cdr p0) (cdr p1))
          (begin (set-cdr! p1 '()) lis)))
      '())))

;; Permissive versions
(define (split-at* lis k :optional (fill? #f) (filler #f))
  (when (or (not (integer? k)) (negative? k))
    (error "index must be non-negative integer" k))
  (let loop ((i 0)
             (lis lis)
             (r '()))
    (cond [(= i k) (values (reverse! r) lis)]
          [(null? lis)
           (values (if fill?
                     (append! (reverse! r) (make-list (- k i) filler))
                     (reverse! r))
                   lis)]
          [else (loop (+ i 1) (cdr lis) (cons (car lis) r))])))

(define (take* lis k . args)
  (receive (h t) (apply split-at* lis k args) h))

(define (drop* lis k)
  (when (or (not (integer? k)) (negative? k))
    (error "index must be non-negative integer" k))
  (let loop ((i 0)
             (lis lis))
    (cond [(= i k) lis]
          [(null? lis) '()]
          [else (loop (+ i 1) (cdr lis))])))

(with-module gauche.internal
  ;; A tolerant version of list-tail.  If LIS is shorter than K, returns
  ;; (- k (length lis)) as the second value.
  (define (%list-tail* lis k)
    (let loop ([lis lis] [k k])
      (cond [(<= k 0) (values lis 0)]
            [(null? lis) (values lis k)]
            [else (loop (cdr lis) (- k 1))])))
  )

(define (take-right* lis k :optional (fill? #f) (filler #f))
  (when (or (not (integer? k)) (negative? k) (inexact? k))
    (error "index must be non-negative exact integer" k))
  ;; NB: This procedure can be used to take the last K elements of
  ;; a huge lazy list.  (Not so much in take-right, with which you need
  ;; to know the length of list is greater than K beforehand.)
  ;; The naive implementation (drop lis (- (length lis) k)) would require
  ;; to realize entire list on memory, which we want to avoid.
  ;; We overwrite LIS and TAIL in each iteration instead of rebinding it,
  ;; in order to release reference to the head of list.
  (receive (tail j) ((with-module gauche.internal %list-tail*) lis k)
    (if (= j 0)
      (let loop ()
        (if (pair? tail)
          (begin (set! lis (cdr lis))
                 (set! tail (cdr tail))
                 (loop))
          lis))
      (if fill?
        (append! (make-list j filler) lis)
        lis))))

(define (drop-right* lis k)
  (let1 len (length lis)
    (if (<= k len) (take lis (- len k)) '())))

;; slices - split a list to a bunch of sublists of length k
(define (slices lis k . args)
  (unless (and (integer? k) (positive? k))
    (error "index must be positive integer" k))
  (let loop ([lis lis]
             [r '()])
    (if (null? lis)
      (reverse! r)
      (receive (h t) (apply split-at* lis k args)
        (loop t (cons h r))))))

;; intersperse - insert ITEM between elements in the list.
;; (the order of arguments is taken from Haskell's intersperse)
(define (intersperse item lis)
  (define (rec l r)
    (if (null? l)
        (reverse! r)
        (rec (cdr l) (list* (car l) item r))))
  (if (null? lis)
      '()
      (rec (cdr lis) (list (car lis)))))

;;
;; Assoc lists
;;

(select-module gauche.internal)
(define-in-module scheme (assoc x lis . args)
  (%case-by-cmp args =
                (assq x lis)
                (assv x lis)
                (%assoc x lis)
                (find (^[entry] (= x (car entry))) lis)))

(define-in-module gauche (alist-copy alist)
  (map (^[elt] (cons (car elt) (cdr elt))) alist))

(define-in-module gauche (alist-delete key alist . args)
  (%case-by-cmp args =
                (%alist-delete key alist 'eq?)
                (%alist-delete key alist 'eqv?)
                (%alist-delete key alist 'equal?)
                (filter (^[elt] (not (= key (car elt)))) alist)))

(define-in-module gauche (alist-delete! key alist . args)
  (%case-by-cmp args =
                (%alist-delete! key alist 'eq?)
                (%alist-delete! key alist 'eqv?)
                (%alist-delete! key alist 'equal?)
                (filter! (^[elt] (not (= key (car elt)))) alist)))

(select-module gauche)
;; `reverse' alist search fn
(define (rassoc key alist :optional (eq equal?))
  (find (^[elt] (and (pair? elt) (eq (cdr elt) key))) alist))

(define rassq (cut rassoc <> <> eq?))
(define rassv (cut rassoc <> <> eqv?))

;; 'assoc-ref', a shortcut of value retrieval w/ default value
;; Default parameter comes first, following the convention of
;; other *-ref functions.
(define (assoc-ref alist key :optional (default #f) (eq equal?))
  (cond [(assoc key alist eq) => cdr]
        [else default]))

(define (assq-ref alist key :optional (default #f))
  (assoc-ref alist key default eq?))
(define (assv-ref alist key :optional (default #f))
  (assoc-ref alist key default eqv?))

(define (rassoc-ref alist key :optional (default #f) (eq equal?))
  (cond [(rassoc key alist eq) => car]
        [else default]))

(define (rassq-ref alist key :optional (default #f))
  (rassoc-ref alist key default eq?))
(define (rassv-ref alist key :optional (default #f))
  (rassoc-ref alist key default eqv?))

;; 'assoc-set!'
(define (assoc-set! alist key val :optional (eq equal?))
  (cond [(assoc key alist eq) => (^p (set-cdr! p val) alist)]
        [else (acons key val alist)]))

(define assq-set!  (cut assoc-set! <> <> <> eq?))
(define assv-set!  (cut assoc-set! <> <> <> eqv?))

;;;
;;; Extended pairs
;;;

(select-module gauche.internal)
;; Pair attributes
;;
;;  Pair attributes are almost exclusively used to attach source-code
;;  information to s-exprs.

(define-cproc pair-attributes (pair::<pair>) Scm_PairAttr)

(define-cproc pair-attribute-get (pair::<pair> key :optional fallback)
  (return (Scm_PairAttrGet (SCM_PAIR pair) key fallback)))

(define-cproc pair-attribute-set! (pair::<pair> key value)
  (return (Scm_PairAttrSet (SCM_PAIR pair) key value)))

(define-cproc extended-pair? (obj) ::<boolean> SCM_EXTENDED_PAIR_P)
(define-cproc extended-cons (car cdr) Scm_ExtendedCons)
(define-cproc extended-list (elt :rest more) Scm_ExtendedCons)
