;;
;; srfi-42 - eager comprehension
;;
;;  This is a port of Sebastian Egner's reference implementation to Gauche.
;;  Ported by Alex Shinn.
;;

; <PLAINTEXT>
; Eager Comprehensions in [outer..inner|expr]-Convention
; ======================================================
;
; sebastian.egner@philips.com, Eindhoven, The Netherlands, 26-Dec-2007
; Scheme R5RS (incl. macros), SRFI-23 (error).
;
; Loading the implementation into Scheme48 0.57:
;   ,open srfi-23
;   ,load ec.scm
;
; Loading the implementation into PLT/DrScheme 317:
;   ; File > Open ... "ec.scm", click Execute
;
; Loading the implementation into SCM 5d7:
;   (require 'macro) (require 'record)
;   (load "ec.scm")
;
; Implementation comments:
;   * All local (not exported) identifiers are named ec-<something>.
;   * This implementation focuses on portability, performance,
;     readability, and simplicity roughly in this order. Design
;     decisions related to performance are taken for Scheme48.
;   * Alternative implementations, Comments and Warnings are
;     mentioned after the definition with a heading.

(define-module srfi-42
  (export-all))
(select-module srfi-42)

(autoload gauche.uvector uvector->list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Gauche treats :foo forms as keywords, which are not valid as
;; identifiers or macros so we can't port this directly.  However, all
;; of the (:keyword ...) forms must be within one of the enclosing *-ec
;; forms, so we can hack it by making all of those forms replace the
;; keywords with appropriately renamed identifiers.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The hygienic syntax version.  Only replaces top-level qualifiers, so
;; nested (:keyword ...) forms fail.  Recursively replacing them is
;; possible but much trickier syntax so I haven't gotten around to it
;; yet.  Using this version, the SRFI-42 examples.scm tests fail first
;; on the following form:
;
; (my-check
;  (list-ec (:parallel (:range i 1 10) (:list x '(a b c))) (list i x))
;  => '((1 a) (2 b) (3 c)) )

; (define-syntax %replace-keywords
;   (syntax-rules ()
;     ((_ syn (collect ...))
;      (syn collect ...))
;     ((_ syn (collect ...) x y ...)
;      (%replace-one-keyword x syn (collect ...) y ...))))

; (define-syntax %replace-one-keyword
;   (syntax-rules ()
;     ((_ (: x ...) syn (c ...) y ...)
;      (%replace-keywords syn (c ... (srfi-42- x ...)) y ...))
;     ((_ (:list x ...) syn (c ...) y ...)
;      (%replace-keywords syn (c ... (srfi-42-list x ...)) y ...))
;     ((_ (:string x ...) syn (c ...) y ...)
;      (%replace-keywords syn (c ... (srfi-42-string x ...)) y ...))
;     ((_ (:vector x ...) syn (c ...) y ...)
;      (%replace-keywords syn (c ... (srfi-42-vector x ...)) y ...))
;     ((_ (:integers x ...) syn (c ...) y ...)
;      (%replace-keywords syn (c ... (srfi-42-integers x ...)) y ...))
;     ((_ (:range x ...) syn (c ...) y ...)
;      (%replace-keywords syn (c ... (srfi-42-range x ...)) y ...))
;     ((_ (:real-range x ...) syn (c ...) y ...)
;      (%replace-keywords syn (c ... (srfi-42-real-range x ...)) y ...))
;     ((_ (:char-range x ...) syn (c ...) y ...)
;      (%replace-keywords syn (c ...  (srfi-42-char-range x ...)) y ...))
;     ((_ (:port x ...) syn (c ...) y ...)
;      (%replace-keywords syn (c ...  (srfi-42-port x ...)) y ...))
;     ((_ (:dispatched x ...) syn (c ...) y ...)
;      (%replace-keywords syn (c ...  (srfi-42-dispatched x ...)) y ...))
;     ((_ (:do x ...) syn (c ...) y ...)
;      (%replace-keywords syn (c ...  (srfi-42-do x ...)) y ...))
;     ((_ (:let x ...) syn (c ...) y ...)
;      (%replace-keywords syn (c ...  (srfi-42-let x ...)) y ...))
;     ((_ (:parallel x ...) syn (c ...) y ...)
;      (%replace-keywords syn (c ...  (srfi-42-parallel x ...)) y ...))
;     ((_ (:while x ...) syn (c ...) y ...)
;      (%replace-keywords syn (c ...  (srfi-42-while x ...)) y ...))
;     ((_ (:until x ...) syn (c ...) y ...)
;      (%replace-keywords syn (c ...  (srfi-42-until x ...)) y ...))
;     ((_ x syn (c ...) y ...) (%replace-keywords syn (c ... x) y ...))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The low-level macro version.  Non-hygienic, so works so long as you
;; don't try to redefine the meaning of the :qualifiers (which you can't
;; do in Gauche anyway) or try to use syntax which expands into a
;; (:qualifier ...) form inside of a *-ec (which would be ugly and poor
;; style).

;; use the same signature as the hygienic version for compatibility, we
;; ignore the middle "collector" arg
(define-macro (%replace-keywords syn _ . args)
  (define (rename symbol)               ; bandage for hygiene
    ((with-module gauche.internal make-identifier)
     symbol (current-module) '()))
  (define (rewrite x)
    (if (pair? x)
      (if (keyword? (car x))
        (cons
         (case (car x)
           ((:)            (rename 'srfi-42-))
           ((:list)        (rename 'srfi-42-list))
           ((:string)      (rename 'srfi-42-string))
           ((:vector)      (rename 'srfi-42-vector))
           ((:uvector)     (rename 'srfi-42-uvector))
           ((:integers)    (rename 'srfi-42-integers))
           ((:range)       (rename 'srfi-42-range))
           ((:real-range)  (rename 'srfi-42-real-range))
           ((:char-range)  (rename 'srfi-42-char-range))
           ((:port)        (rename 'srfi-42-port))
           ((:generator)   (rename 'srfi-42-generator))
           ((:dispatched)  (rename 'srfi-42-dispatched))
           ((:do)          (rename 'srfi-42-do))
           ((:let)         (rename 'srfi-42-let))
           ((:parallel)    (rename 'srfi-42-parallel))
           ((:while)       (rename 'srfi-42-while))
           ((:until)       (rename 'srfi-42-until))
           (else (car x)))
         (rewrite (cdr x)))
        (cons (rewrite (car x)) (rewrite (cdr x))))
      x))
  `(,syn ,@(map rewrite args)))


; ==========================================================================
; The fundamental comprehension do-ec
; ==========================================================================
;
; All eager comprehensions are reduced into do-ec and
; all generators are reduced to :do.
;
; We use the following short names for syntactic variables
;   q    - qualifier
;   cc   - current continuation, thing to call at the end;
;          the CPS is (m (cc ...) arg ...) -> (cc ... expr ...)
;   cmd  - an expression being evaluated for its side-effects
;   expr - an expression
;   gen  - a generator of an eager comprehension
;   ob   - outer binding
;   oc   - outer command
;   lb   - loop binding
;   ne1? - not-end1? (before the payload)
;   ib   - inner binding
;   ic   - inner command
;   ne2? - not-end2? (after the payload)
;   ls   - loop step
;   etc  - more arguments of mixed type


; (do-ec q ... cmd)
;   handles nested, if/not/and/or, begin, :let, and calls generator
;   macros in CPS to transform them into fully decorated :do.
;   The code generation for a :do is delegated to do-ec:do.

; (define-syntax do-ec
;   (syntax-rules ()
;     ((_ expr ...)
;      (%do-ec (%replace-keywords expr) ...))))

(define-syntax do-ec
  (syntax-rules ()
    ((do-ec expr ...)
     (%replace-keywords %do-ec () expr ...))))

(define-syntax %do-ec
  ;;(syntax-rules (nested if not and or begin :do let)
  (syntax-rules (nested if not and or begin srfi-42-do let)

    ; explicit nesting -> implicit nesting
    ((do-ec (nested q ...) etc ...)
     (do-ec q ... etc ...) )

    ; implicit nesting -> fold do-ec
    ((do-ec q1 q2 etc1 etc ...)
     (do-ec q1 (do-ec q2 etc1 etc ...)) )

    ; no qualifiers at all -> evaluate cmd once
    ((do-ec cmd)
     (begin cmd (if #f #f)) )

; now (do-ec q cmd) remains

    ; filter -> make conditional
    ((do-ec (if test) cmd)
     (if test (do-ec cmd)) )
    ((do-ec (not test) cmd)
     (if (not test) (do-ec cmd)) )
    ((do-ec (and test ...) cmd)
     (if (and test ...) (do-ec cmd)) )
    ((do-ec (or test ...) cmd)
     (if (or test ...) (do-ec cmd)) )

    ; begin -> make a sequence
    ((do-ec (begin etc ...) cmd)
     (begin etc ... (do-ec cmd)) )

    ; fully decorated :do-generator -> delegate to do-ec:do
    ((do-ec (srfi-42-do olet lbs ne1? ilet ne2? lss) cmd)
     (do-ec:do cmd (srfi-42-do olet lbs ne1? ilet ne2? lss)) )

; anything else -> call generator-macro in CPS; reentry at (*)

    ((do-ec (g arg1 arg ...) cmd)
     (g (do-ec:do cmd) arg1 arg ...) )))


; (do-ec:do cmd (:do olet lbs ne1? ilet ne2? lss)
;   generates code for a single fully decorated :do-generator
;   with cmd as payload, taking care of special cases.

(define-syntax do-ec:do
  ;;(syntax-rules (:do let)
  (syntax-rules (srfi-42-do let)

    ; reentry point (*) -> generate code
    ((do-ec:do cmd
               (srfi-42-do (let obs oc ...)
                    lbs
                    ne1?
                    (let ibs ic ...)
                    ne2?
                    (ls ...) ))
     (ec-simplify
       (let obs
         oc ...
         (let loop lbs
           (ec-simplify
             (if ne1?
                 (ec-simplify
                   (let ibs
                      ic ...
                      cmd
                      (ec-simplify
                        (if ne2?
                            (loop ls ...) )))))))))) ))


; (ec-simplify <expression>)
;   generates potentially more efficient code for <expression>.
;   The macro handles if, (begin <command>*), and (let () <command>*)
;   and takes care of special cases.

(define-syntax ec-simplify
  (syntax-rules (if not let begin)

; one- and two-sided if

    ; literal <test>
    ((ec-simplify (if #t consequent))
     consequent )
    ((ec-simplify (if #f consequent))
     (if #f #f) )
    ((ec-simplify (if #t consequent alternate))
     consequent )
    ((ec-simplify (if #f consequent alternate))
     alternate )

    ; (not (not <test>))
    ((ec-simplify (if (not (not test)) consequent))
     (ec-simplify (if test consequent)) )
    ((ec-simplify (if (not (not test)) consequent alternate))
     (ec-simplify (if test consequent alternate)) )

; (let () <command>*)

    ; empty <binding spec>*
    ((ec-simplify (let () command ...))
     (ec-simplify (begin command ...)) )

; begin

    ; flatten use helper (ec-simplify 1 done to-do)
    ((ec-simplify (begin command ...))
     (ec-simplify 1 () (command ...)) )
    ((ec-simplify 1 done ((begin to-do1 ...) to-do2 ...))
     (ec-simplify 1 done (to-do1 ... to-do2 ...)) )
    ((ec-simplify 1 (done ...) (to-do1 to-do ...))
     (ec-simplify 1 (done ... to-do1) (to-do ...)) )

    ; exit helper
    ((ec-simplify 1 () ())
     (if #f #f) )
    ((ec-simplify 1 (command) ())
     command )
    ((ec-simplify 1 (command1 command ...) ())
     (begin command1 command ...) )

; anything else

    ((ec-simplify expression)
     expression )))


; ==========================================================================
; The special generators :do, :let, :parallel, :while, and :until
; ==========================================================================

(define-syntax srfi-42-do
  (syntax-rules ()

    ; full decorated -> continue with cc, reentry at (*)
    ((_ (cc ...) olet lbs ne1? ilet ne2? lss)
     (cc ... (srfi-42-do olet lbs ne1? ilet ne2? lss)) )

    ; short form -> fill in default values
    ((_ cc lbs ne1? lss)
     (srfi-42-do cc (let ()) lbs ne1? (let ()) #t lss) )))


(define-syntax srfi-42-let
  (syntax-rules (index)
    ((_ cc var (index i) expression)
     (srfi-42-do cc (let ((var expression) (i 0))) () #t (let ()) #f ()) )
    ((_ cc var expression)
     (srfi-42-do cc (let ((var expression))) () #t (let ()) #f ()) )))


(define-syntax srfi-42-parallel
  ;;(syntax-rules (:do)
  (syntax-rules ()
    ((_ cc)
     cc )
    ((_ cc (g arg1 arg ...) gen ...)
     (g (srfi-42-parallel-1 cc (gen ...)) arg1 arg ...) )))

; (:parallel-1 cc (to-do ...) result [ next ] )
;    iterates over to-do by converting the first generator into
;    the :do-generator next and merging next into result.

(define-syntax srfi-42-parallel-1  ; used as
  ;;(syntax-rules (:do let)
  (syntax-rules (srfi-42-do let)

    ; process next element of to-do, reentry at (**)
    ((_ cc ((g arg1 arg ...) gen ...) result)
     (g (srfi-42-parallel-1 cc (gen ...) result) arg1 arg ...) )

    ; reentry point (**) -> merge next into result
    ((_
       cc
       gens
       (srfi-42-do (let (ob1 ...) oc1 ...)
            (lb1 ...)
            ne1?1
            (let (ib1 ...) ic1 ...)
            ne2?1
            (ls1 ...) )
       (srfi-42-do (let (ob2 ...) oc2 ...)
            (lb2 ...)
            ne1?2
            (let (ib2 ...) ic2 ...)
            ne2?2
            (ls2 ...) ))
     (srfi-42-parallel-1
       cc
       gens
       (srfi-42-do (let (ob1 ... ob2 ...) oc1 ... oc2 ...)
            (lb1 ... lb2 ...)
            (and ne1?1 ne1?2)
            (let (ib1 ... ib2 ...) ic1 ... ic2 ...)
            (and ne2?1 ne2?2)
            (ls1 ... ls2 ...) )))

    ; no more gens -> continue with cc, reentry at (*)
    ((_ (cc ...) () result)
     (cc ... result) )))

; (:while-1 cc test (:do ...))
;    modifies the fully decorated :do-generator such that it
;    runs while test is a true value. 
;       The original implementation just replaced ne1? by
;    (and ne1? test) as follows:
;
;      (define-syntax :while-1
;        (syntax-rules (:do)
;          ((:while-1 cc test (:do olet lbs ne1? ilet ne2? lss))
;           (:do cc olet lbs (and ne1? test) ilet ne2? lss) )))
;
; Bug #1:
;    Unfortunately, this code is wrong because ne1? may depend
;    in the inner bindings introduced in ilet, but ne1? is evaluated
;    outside of the inner bindings. (Refer to the specification of
;    :do to see the structure.) 
;       The problem manifests itself (as sunnan@handgranat.org 
;    observed, 25-Apr-2005) when the :list-generator is modified:
; 
;      (do-ec (:while (:list x '(1 2)) (= x 1)) (display x)).
;
;    In order to generate proper code, we introduce temporary
;    variables saving the values of the inner bindings. The inner
;    bindings are executed in a new ne1?, which also evaluates ne1?
;    outside the scope of the inner bindings, then the inner commands
;    are executed (possibly changing the variables), and then the
;    values of the inner bindings are saved and (and ne1? test) is
;    returned. In the new ilet, the inner variables are bound and
;    initialized and their values are restored. So we construct:
;
;     (let (ob .. (ib-tmp #f) ...)
;       oc ...
;       (let loop (lb ...)
;         (if (let (ne1?-value ne1?)
;               (let ((ib-var ib-rhs) ...)
;                 ic ...
;                 (set! ib-tmp ib-var) ...)
;               (and ne1?-value test))
;             (let ((ib-var ib-tmp) ...)
;               /payload/
;               (if ne2?
;                   (loop ls ...) )))))
; 
; Bug #2:
;    Unfortunately, the above expansion is still incorrect (as Jens-Axel 
;    Soegaard pointed out, 4-Jun-2007) because ib-rhs are evaluated even
;    if ne1?-value is #f, indicating that the loop has ended.
;       The problem manifests itself in the following example:
;
;      (do-ec (:while (:list x '(1)) #t) (display x))
;
;    Which iterates :list beyond exhausting the list '(1).
;
;    For the fix, we follow Jens-Axel's approach of guarding the evaluation
;    of ib-rhs with a check on ne1?-value.

(define-syntax srfi-42-while
  (syntax-rules ()
    ((_ cc (g arg1 arg ...) test)
     (g (srfi-42-while-1 cc test) arg1 arg ...) )))

(define-syntax srfi-42-while-1
  (syntax-rules (srfi-42-do let)
    ((srfi-42-while-1 cc test (srfi-42-do olet lbs ne1? ilet ne2? lss))
     (srfi-42-while-2 cc test () () () (srfi-42-do olet lbs ne1? ilet ne2? lss)))))

(define-syntax srfi-42-while-2
  (syntax-rules (srfi-42-do let)
    ((srfi-42-while-2 cc
                      test
                      (ib-let     ...)
                      (ib-save    ...)
                      (ib-restore ...)
                      (srfi-42-do olet
                                  lbs
                                  ne1?
                                  (let ((ib-var ib-rhs) ib ...) ic ...)
                                  ne2?
                                  lss))
     (srfi-42-while-2 cc
                      test
                      (ib-let     ... (ib-tmp #f))
                      (ib-save    ... (ib-var ib-rhs))
                      (ib-restore ... (ib-var ib-tmp))
                      (srfi-42-do olet
                                  lbs
                                  ne1?
                                  (let (ib ...) ic ... (set! ib-tmp ib-var))
                                  ne2?
                                  lss)))
    ((srfi-42-while-2 cc
                      test
                      (ib-let     ...)
                      (ib-save    ...)
                      (ib-restore ...)
                      (srfi-42-do (let (ob ...) oc ...) lbs ne1?
                                  (let () ic ...) ne2? lss))
     (srfi-42-do cc
                 (let (ob ... ib-let ...) oc ...)
                 lbs
                 (let ((ne1?-value ne1?))
                   (and ne1?-value
                        (let (ib-save ...)
                          ic ...
                          test)))
                 (let (ib-restore ...))
                 ne2?
                 lss))))

(define-syntax srfi-42-until
  (syntax-rules ()
    ((_ cc (g arg1 arg ...) test)
     (g (srfi-42-until-1 cc test) arg1 arg ...) )))

(define-syntax srfi-42-until-1
  ;;(syntax-rules (:do)
  (syntax-rules (srfi-42-do)
    ((_ cc test (srfi-42-do olet lbs ne1? ilet ne2? lss))
     (srfi-42-do cc olet lbs ne1? ilet (and ne2? (not test)) lss) )))


; ==========================================================================
; The typed generators :list :string :vector etc.
; ==========================================================================

(define-syntax srfi-42-list
  (syntax-rules (index)
    ((_ cc var (index i) arg ...)
     (srfi-42-parallel cc (srfi-42-list var arg ...) (srfi-42-integers i)) )
    ((_ cc var arg1 arg2 arg ...)
     (srfi-42-list cc var (append arg1 arg2 arg ...)) )
    ((_ cc var arg)
     (srfi-42-do cc
          (let ())
          ((t arg))
          (not (null? t))
          (let ((var (car t))))
          #t
          ((cdr t)) ))
    ((_ x ...) #t)))


(define-syntax srfi-42-string
  (syntax-rules (index)
    ((_ cc var (index i) arg)
     (srfi-42-do cc
          (let ((str arg) (len 0))
            (set! len (string-length str)))
          ((i 0))
          (< i len)
          (let ((var (string-ref str i))))
          #t
          ((+ i 1)) ))
    ((_ cc var (index i) arg1 arg2 arg ...)
     (srfi-42-string cc var (index i) (string-append arg1 arg2 arg ...)) )
    ((_ cc var arg1 arg ...)
     (srfi-42-string cc var (index i) arg1 arg ...) )))

; Alternative: An implementation in the style of :vector can also
;   be used for :string. However, it is less interesting as the
;   overhead of string-append is much less than for 'vector-append'.

(define-syntax srfi-42-vector
  (syntax-rules ()
    [(_ . args) (srfi-42-*vector vector-length vector-ref . args)]))

(define-syntax srfi-42-uvector
  (syntax-rules ()
    [(_ . args) (srfi-42-*vector uvector-length uvector-ref . args)]))

(define-syntax srfi-42-*vector
  (syntax-rules (index)
    ((_ *len *ref cc var arg)
     (srfi-42-*vector *len *ref cc var (index i) arg) )
    ((_ *len *ref cc var (index i) arg)
     (srfi-42-do cc
          (let ((vec arg) (len 0))
            (set! len (*len vec)))
          ((i 0))
          (< i len)
          (let ((var (*ref vec i))))
          #t
          ((+ i 1)) ))

    ((_ *len *ref cc var (index i) arg1 arg2 arg ...)
     (srfi-42-parallel cc 
                       (srfi-42-*vector *len *ref cc var arg1 arg2 arg ...)
                       (srfi-42-integers i)) )
    ((_ *len *ref cc var arg1 arg2 arg ...)
     (srfi-42-do cc
          (let ((vec #f)
                (len 0)
                (vecs (ec-:vector-filter *len (list arg1 arg2 arg ...))) ))
          ((k 0))
          (if (< k len)
              #t
              (if (null? vecs)
                  #f
                  (begin (set! vec (car vecs))
                         (set! vecs (cdr vecs))
                         (set! len (*len vec))
                         (set! k 0)
                         #t )))
          (let ((var (*ref vec k))))
          #t
          ((+ k 1)) ))))

(define (ec-:vector-filter *len vecs)
  (if (null? vecs)
      '()
      (if (zero? (*len (car vecs)))
          (ec-:vector-filter *len (cdr vecs))
          (cons (car vecs) (ec-:vector-filter *len (cdr vecs))) )))

; Alternative: A simpler implementation for :vector uses vector->list
;   append and :list in the multi-argument case. Please refer to the
;   'design.scm' for more details.


(define-syntax srfi-42-integers
  (syntax-rules (index)
    ((_ cc var (index i))
     (srfi-42-do cc ((var 0) (i 0)) #t ((+ var 1) (+ i 1))) )
    ((_ cc var)
     (srfi-42-do cc ((var 0)) #t ((+ var 1))) )))


(define-syntax srfi-42-range
  (syntax-rules (index)

    ; handle index variable and add optional args
    ((_ cc var (index i) arg1 arg ...)
     (srfi-42-parallel cc (srfi-42-range var arg1 arg ...) (srfi-42-integers i)) )
    ((_ cc var arg1)
     (srfi-42-range cc var 0 arg1 1) )
    ((_ cc var arg1 arg2)
     (srfi-42-range cc var arg1 arg2 1) )

; special cases (partially evaluated by hand from general case)

    ((_ cc var 0 arg2 1)
     (srfi-42-do cc
          (let ((b arg2))
            (if (not (and (integer? b) (exact? b)))
                (error
                   "arguments of :range are not exact integer "
                   "(use :real-range?)" 0 b 1 )))
          ((var 0))
          (< var b)
          (let ())
          #t
          ((+ var 1)) ))

    ((_ cc var 0 arg2 -1)
     (srfi-42-do cc
          (let ((b arg2))
            (if (not (and (integer? b) (exact? b)))
                (error
                   "arguments of :range are not exact integer "
                   "(use :real-range?)" 0 b 1 )))
          ((var 0))
          (> var b)
          (let ())
          #t
          ((- var 1)) ))

    ((_ cc var arg1 arg2 1)
     (srfi-42-do cc
          (let ((a arg1) (b arg2))
            (if (not (and (integer? a) (exact? a)
                          (integer? b) (exact? b) ))
                (error
                   "arguments of :range are not exact integer "
                   "(use :real-range?)" a b 1 )) )
          ((var a))
          (< var b)
          (let ())
          #t
          ((+ var 1)) ))

    ((_ cc var arg1 arg2 -1)
     (srfi-42-do cc
          (let ((a arg1) (b arg2) (s -1) (stop 0))
            (if (not (and (integer? a) (exact? a)
                          (integer? b) (exact? b) ))
                (error
                   "arguments of :range are not exact integer "
                   "(use :real-range?)" a b -1 )) )
          ((var a))
          (> var b)
          (let ())
          #t
          ((- var 1)) ))

; the general case

    ((_ cc var arg1 arg2 arg3)
     (srfi-42-do cc
          (let ((a arg1) (b arg2) (s arg3) (stop 0))
            (if (not (and (integer? a) (exact? a)
                          (integer? b) (exact? b)
                          (integer? s) (exact? s) ))
                (error
                   "arguments of :range are not exact integer "
                   "(use :real-range?)" a b s ))
            (if (zero? s)
                (error "step size must not be zero in :range") )
            (set! stop (+ a (* (max 0 (ceiling (/ (- b a) s))) s))))
          ((var a))
          (not (= var stop))
          (let ())
          #t
          ((+ var s)) ))))

; Comment: The macro :range inserts some code to make sure the values
;   are exact integers. This overhead has proven very helpful for
;   saving users from themselves.


(define-syntax srfi-42-real-range
  (syntax-rules (index)

    ; add optional args and index variable
    ((_ cc var arg1)
     (srfi-42-real-range cc var (index i) 0 arg1 1) )
    ((_ cc var (index i) arg1)
     (srfi-42-real-range cc var (index i) 0 arg1 1) )
    ((_ cc var arg1 arg2)
     (srfi-42-real-range cc var (index i) arg1 arg2 1) )
    ((_ cc var (index i) arg1 arg2)
     (srfi-42-real-range cc var (index i) arg1 arg2 1) )
    ((_ cc var arg1 arg2 arg3)
     (srfi-42-real-range cc var (index i) arg1 arg2 arg3) )

    ; the fully qualified case
    ((_ cc var (index i) arg1 arg2 arg3)
     (srfi-42-do cc
          (let ((a arg1) (b arg2) (s arg3) (istop 0))
            (if (not (and (real? a) (real? b) (real? s)))
                (error "arguments of :real-range are not real" a b s) )
            (if (and (exact? a) (or (not (exact? b)) (not (exact? s))))
                (set! a (exact->inexact a)) )
            (set! istop (/ (- b a) s)) )
          ((i 0))
          (< i istop)
          (let ((var (+ a (* s i)))))
          #t
          ((+ i 1)) ))))

; Comment: The macro :real-range adapts the exactness of the start
;   value in case any of the other values is inexact. This is a
;   precaution to avoid (list-ec (: x 0 3.0) x) => '(0 1.0 2.0).


(define-syntax srfi-42-char-range
  (syntax-rules (index)
    ((_ cc var (index i) arg1 arg2)
     (srfi-42-parallel cc (srfi-42-char-range var arg1 arg2) (srfi-42-integers i)) )
    ((_ cc var arg1 arg2)
     (srfi-42-do cc
          (let ((imax (char->integer arg2))))
          ((i (char->integer arg1)))
          (<= i imax)
          (let ((var (integer->char i))))
          #t
          ((+ i 1)) ))))

; Warning: There is no R5RS-way to implement the :char-range generator
;   because the integers obtained by char->integer are not necessarily
;   consecutive. We simply assume this anyhow for illustration.


(define-syntax srfi-42-port
  (syntax-rules (index)
    [(_ cc var (index i) arg1 arg ...)
     (srfi-42-parallel cc (srfi-42-port var arg1 arg ...) (srfi-42-integers i))]
    [(_ cc var arg)
     (srfi-42-port cc var arg read)]
    [(_ cc var arg1 arg2)
     (srfi-42-do cc
          (let ((port arg1) (read-proc arg2)))
          ((var (read-proc port)))
          (not (eof-object? var))
          (let ())
          #t
          ((read-proc port)) )]))

;; Gauche extension
(define-syntax srfi-42-generator
  (syntax-rules (index)
    [(_ cc var (index i) expr)
     (srfi-42-parallel cc (srfi-42-generator var expr) (srfi-42-integers i))]
    [(_ cc var expr)
     (srfi-42-do cc
          (let ([gen expr]))
          ([var (gen)])
          (not (eof-object? var))
          (let ())
          #t
          [(gen)])]))

; ==========================================================================
; The typed generator :dispatched and utilities for constructing dispatchers
; ==========================================================================

(define-syntax srfi-42-dispatched
  (syntax-rules (index)
    ((_ cc var (index i) dispatch arg1 arg ...)
     (srfi-42-parallel cc
                (srfi-42-integers i)
                (srfi-42-dispatched var dispatch arg1 arg ...) ))
    ((_ cc var dispatch arg1 arg ...)
     (srfi-42-do cc
          (let ((d dispatch)
                (args (list arg1 arg ...))
                (g #f)
                (empty (list #f)) )
            (set! g (d args))
            (if (not (procedure? g))
                (error "unrecognized arguments in dispatching"
                       args
                       (d '()) )))
          ((var (g empty)))
          (not (eq? var empty))
          (let ())
          #t
          ((g empty)) ))))

; Comment: The unique object empty is created as a newly allocated
;   non-empty list. It is compared using eq? which distinguishes
;   the object from any other object, according to R5RS 6.1.


(define-syntax srfi-42-generator-proc
  ;;(syntax-rules (:do let)
  (syntax-rules (srfi-42-do let)

    ; call g with a variable, reentry at (**)
    ((_ (g arg ...))
     (g (srfi-42-generator-proc var) var arg ...) )

    ; reentry point (**) -> make the code from a single :do
    ((_
       var
       (srfi-42-do (let obs oc ...)
            ((lv li) ...)
            ne1?
            (let ((i v) ...) ic ...)
            ne2?
            (ls ...)) )
     (ec-simplify
      (let obs
          oc ...
          (let ((lv li) ... (ne2 #t))
            (ec-simplify
             (let ((i #f) ...) ; v not yet valid
               (lambda (empty)
                 (if (and ne1? ne2)
                     (ec-simplify
                      (begin
                        (set! i v) ...
                        ic ...
                        (let ((value var))
                          (ec-simplify
                           (if ne2?
                               (ec-simplify
                                (begin (set! lv ls) ...) )
                               (set! ne2 #f) ))
                          value )))
                     empty ))))))))

    ; silence warnings of some macro expanders
    ((_ var)
     (error "illegal macro call") )))


(define (dispatch-union d1 d2)
  (lambda (args)
    (let ((g1 (d1 args)) (g2 (d2 args)))
      (if g1
          (if g2
              (if (null? args)
                  (append (if (list? g1) g1 (list g1))
                          (if (list? g2) g2 (list g2)) )
                  (error "dispatching conflict" args (d1 '()) (d2 '())) )
              g1 )
          (if g2 g2 #f) ))))


; ==========================================================================
; The dispatching generator :
; ==========================================================================

(define (make-initial-:-dispatch)
  (lambda (args)
    (case (length args)
      [(0) 'SRFI42]
      [(1) (let ([a1 (car args)])
             (cond
              [(list? a1)
               (srfi-42-generator-proc (srfi-42-list a1))]
              [(string? a1)
               (srfi-42-generator-proc (srfi-42-string a1))]
              [(vector? a1)
               (srfi-42-generator-proc (srfi-42-vector a1))]
              [(uvector? a1)
               (srfi-42-generator-proc (srfi-42-uvector a1))]
              [(and (integer? a1) (exact? a1))
               (srfi-42-generator-proc (srfi-42-range a1))]
              [(real? a1)
               (srfi-42-generator-proc (srfi-42-real-range a1))]
              [(input-port? a1)
               (srfi-42-generator-proc (srfi-42-port a1))]
              [(applicable? a1)
               (srfi-42-generator-proc (srfi-42-generator a1))]
              [else #f]))]
      [(2) (let ([a1 (car args)] [a2 (cadr args)])
             (cond
              [(and (list? a1) (list? a2))
               (srfi-42-generator-proc (srfi-42-list a1 a2)) ]
              [(and (string? a1) (string? a1))
               (srfi-42-generator-proc (srfi-42-string a1 a2)) ]
              [(and (vector? a1) (vector? a2))
               (srfi-42-generator-proc (srfi-42-vector a1 a2)) ]
              [(and (uvector? a1) (uvector? a2))
               (srfi-42-generator-proc (srfi-42-uvector a1 a2)) ]
              [(and (integer? a1) (exact? a1) (integer? a2) (exact? a2))
               (srfi-42-generator-proc (srfi-42-range a1 a2)) ]
              [(and (real? a1) (real? a2))
               (srfi-42-generator-proc (srfi-42-real-range a1 a2)) ]
              [(and (char? a1) (char? a2))
               (srfi-42-generator-proc (srfi-42-char-range a1 a2)) ]
              [(and (input-port? a1) (procedure? a2))
               (srfi-42-generator-proc (srfi-42-port a1 a2)) ]
              [else #f]))]
      [(3) (let ([a1 (car args)] [a2 (cadr args)] [a3 (caddr args)])
             (cond
              [(and (list? a1) (list? a2) (list? a3))
               (srfi-42-generator-proc (srfi-42-list a1 a2 a3)) ]
              [(and (string? a1) (string? a1) (string? a3))
               (srfi-42-generator-proc (srfi-42-string a1 a2 a3)) ]
              [(and (vector? a1) (vector? a2) (vector? a3))
               (srfi-42-generator-proc (srfi-42-vector a1 a2 a3)) ]
              [(and (uvector? a1) (uvector? a2) (uvector? a3))
               (srfi-42-generator-proc (srfi-42-uvector a1 a2 a3)) ]
              [(and (integer? a1) (exact? a1)
                    (integer? a2) (exact? a2)
                    (integer? a3) (exact? a3))
               (srfi-42-generator-proc (srfi-42-range a1 a2 a3)) ]
              [(and (real? a1) (real? a2) (real? a3))
               (srfi-42-generator-proc (srfi-42-real-range a1 a2 a3)) ]
              [else #f]))]
      [else (cond
             [(every list? args)
              (srfi-42-generator-proc (srfi-42-list (apply append args))) ]
             [(every string? args)
              (srfi-42-generator-proc (srfi-42-string (apply string-append args)))]
             [(every vector? args)
              (srfi-42-generator-proc (srfi-42-vector (apply vector-append args)))]
             [(every uvector? args)
              (srfi-42-generator-proc (srfi-42-list (apply append (map uvector->list args))))]
             [else #f])])))

(define srfi-42--dispatch
  (make-initial-:-dispatch) )

(define (srfi-42--dispatch-ref)
  srfi-42--dispatch )

(define (srfi-42--dispatch-set! dispatch)
  (if (not (procedure? dispatch))
      (error "not a procedure" dispatch) )
  (set! srfi-42--dispatch dispatch) )

(define-syntax srfi-42-
  (syntax-rules (index)
    ((_ cc var (index i) arg1 arg ...)
     (srfi-42-dispatched cc var (index i) srfi-42--dispatch arg1 arg ...) )
    ((_ cc var arg1 arg ...)
     (srfi-42-dispatched cc var srfi-42--dispatch arg1 arg ...) )))


; ==========================================================================
; The utility comprehensions fold-ec, fold3-ec
; ==========================================================================

(define-syntax fold3-ec
  (syntax-rules (nested)
    ((fold3-ec x0 (nested q1 ...) q etc1 etc2 etc3 etc ...)
     (fold3-ec x0 (nested q1 ... q) etc1 etc2 etc3 etc ...) )
    ((fold3-ec x0 q1 q2 etc1 etc2 etc3 etc ...)
     (fold3-ec x0 (nested q1 q2) etc1 etc2 etc3 etc ...) )
    ((fold3-ec x0 expression f1 f2)
     (fold3-ec x0 (nested) expression f1 f2) )

    ((fold3-ec x0 qualifier expression f1 f2)
     (let ((result #f) (empty #t))
       (do-ec qualifier
              (let ((value expression)) ; don't duplicate
                (if empty
                    (begin (set! result (f1 value))
                           (set! empty #f) )
                    (set! result (f2 value result)) )))
       (if empty x0 result) ))))


(define-syntax fold-ec
  (syntax-rules (nested)
    ((fold-ec x0 (nested q1 ...) q etc1 etc2 etc ...)
     (fold-ec x0 (nested q1 ... q) etc1 etc2 etc ...) )
    ((fold-ec x0 q1 q2 etc1 etc2 etc ...)
     (fold-ec x0 (nested q1 q2) etc1 etc2 etc ...) )
    ((fold-ec x0 expression f2)
     (fold-ec x0 (nested) expression f2) )

    ((fold-ec x0 qualifier expression f2)
     (let ((result x0))
       (do-ec qualifier (set! result (f2 expression result)))
       result ))))


; ==========================================================================
; The comprehensions list-ec string-ec vector-ec etc.
; ==========================================================================

(define-syntax list-ec
  (syntax-rules ()
    ((list-ec etc1 etc ...)
     (reverse (fold-ec '() etc1 etc ... cons)) )))

; Alternative: Reverse can safely be replaced by reverse! if you have it.
;
; Alternative: It is possible to construct the result in the correct order
;   using set-cdr! to add at the tail. This removes the overhead of copying
;   at the end, at the cost of more book-keeping.


(define-syntax append-ec
  (syntax-rules ()
    ((append-ec etc1 etc ...)
     (apply append (list-ec etc1 etc ...)) )))

(define-syntax string-ec
  (syntax-rules ()
    ((string-ec etc1 etc ...)
     (list->string (list-ec etc1 etc ...)) )))

; Alternative: For very long strings, the intermediate list may be a
;   problem. A more space-aware implementation collect the characters
;   in an intermediate list and when this list becomes too large it is
;   converted into an intermediate string. At the end, the intermediate
;   strings are concatenated with string-append.


(define-syntax string-append-ec
  (syntax-rules ()
    ((string-append-ec etc1 etc ...)
     (apply string-append (list-ec etc1 etc ...)) )))

(define-syntax vector-ec
  (syntax-rules ()
    ((vector-ec etc1 etc ...)
     (list->vector (list-ec etc1 etc ...)) )))

; Comment: A similar approach as for string-ec can be used for vector-ec.
;   However, the space overhead for the intermediate list is much lower
;   than for string-ec and as there is no vector-append, the intermediate
;   vectors must be copied explicitly.

(define-syntax vector-of-length-ec
  (syntax-rules (nested)
    ((vector-of-length-ec k (nested q1 ...) q etc1 etc ...)
     (vector-of-length-ec k (nested q1 ... q) etc1 etc ...) )
    ((vector-of-length-ec k q1 q2             etc1 etc ...)
     (vector-of-length-ec k (nested q1 q2)    etc1 etc ...) )
    ((vector-of-length-ec k expression)
     (vector-of-length-ec k (nested) expression) )

    ((vector-of-length-ec k qualifier expression)
     (let ((len k))
       (let ((vec (make-vector len))
             (i 0) )
         (do-ec qualifier
                (if (< i len)
                    (begin (vector-set! vec i expression)
                           (set! i (+ i 1)) )
                    (error "vector is too short for the comprehension") ))
         (if (= i len)
             vec
             (error "vector is too long for the comprehension") ))))))


(define-syntax sum-ec
  (syntax-rules ()
    ((sum-ec etc1 etc ...)
     (fold-ec (+) etc1 etc ... +) )))

(define-syntax product-ec
  (syntax-rules ()
    ((product-ec etc1 etc ...)
     (fold-ec (*) etc1 etc ... *) )))

(define-syntax min-ec
  (syntax-rules ()
    ((min-ec etc1 etc ...)
     (fold3-ec (min) etc1 etc ... min min) )))

(define-syntax max-ec
  (syntax-rules ()
    ((max-ec etc1 etc ...)
     (fold3-ec (max) etc1 etc ... max max) )))

(define-syntax last-ec
  (syntax-rules (nested)
    ((last-ec default (nested q1 ...) q etc1 etc ...)
     (last-ec default (nested q1 ... q) etc1 etc ...) )
    ((last-ec default q1 q2             etc1 etc ...)
     (last-ec default (nested q1 q2)    etc1 etc ...) )
    ((last-ec default expression)
     (last-ec default (nested) expression) )

    ((last-ec default qualifier expression)
     (let ((result default))
       (do-ec qualifier (set! result expression))
       result ))))


; ==========================================================================
; The fundamental early-stopping comprehension first-ec
; ==========================================================================

(define-syntax first-ec
  (syntax-rules ()
    ((first-ec expr ...)
     (%replace-keywords %first-ec () expr ...))))

(define-syntax %first-ec
  (syntax-rules (nested)
    ((%first-ec default (nested q1 ...) q etc1 etc ...)
     (%first-ec default (nested q1 ... q) etc1 etc ...) )
    ((%first-ec default q1 q2             etc1 etc ...)
     (%first-ec default (nested q1 q2)    etc1 etc ...) )
    ((%first-ec default expression)
     (%first-ec default (nested) expression) )

    ((%first-ec default qualifier expression)
     (let ((result default) (stop #f))
       (ec-guarded-do-ec
         stop
         (nested qualifier)
         (begin (set! result expression)
                (set! stop #t) ))
       result ))))

; (ec-guarded-do-ec stop (nested q ...) cmd)
;   constructs (do-ec q ... cmd) where the generators gen in q ... are
;   replaced by (:until gen stop).

(define-syntax ec-guarded-do-ec
  (syntax-rules ()
    ((ec-guarded-do-ec expr ...)
     (%replace-keywords %ec-guarded-do-ec () expr ...))))

(define-syntax %ec-guarded-do-ec
  (syntax-rules (nested if not and or begin)

    ((ec-guarded-do-ec stop (nested (nested q1 ...) q2 ...) cmd)
     (ec-guarded-do-ec stop (nested q1 ... q2 ...) cmd) )

    ((ec-guarded-do-ec stop (nested (if test) q ...) cmd)
     (if test (ec-guarded-do-ec stop (nested q ...) cmd)) )
    ((ec-guarded-do-ec stop (nested (not test) q ...) cmd)
     (if (not test) (ec-guarded-do-ec stop (nested q ...) cmd)) )
    ((ec-guarded-do-ec stop (nested (and test ...) q ...) cmd)
     (if (and test ...) (ec-guarded-do-ec stop (nested q ...) cmd)) )
    ((ec-guarded-do-ec stop (nested (or test ...) q ...) cmd)
     (if (or test ...) (ec-guarded-do-ec stop (nested q ...) cmd)) )

    ((ec-guarded-do-ec stop (nested (begin etc ...) q ...) cmd)
     (begin etc ... (ec-guarded-do-ec stop (nested q ...) cmd)) )

    ((ec-guarded-do-ec stop (nested gen q ...) cmd)
     (do-ec
       (srfi-42-until gen stop)
       (ec-guarded-do-ec stop (nested q ...) cmd) ))

    ((ec-guarded-do-ec stop (nested) cmd)
     (do-ec cmd) )))

; Alternative: Instead of modifying the generator with :until, it is
;   possible to use call-with-current-continuation:
;
;   (define-syntax first-ec
;     ...same as above...
;     ((first-ec default qualifier expression)
;      (call-with-current-continuation
;       (lambda (cc)
;        (do-ec qualifier (cc expression))
;        default ))) ))
;
;   This is much simpler but not necessarily as efficient.


; ==========================================================================
; The early-stopping comprehensions any?-ec every?-ec
; ==========================================================================

(define-syntax any?-ec
  (syntax-rules (nested)
    ((any?-ec (nested q1 ...) q etc1 etc ...)
     (any?-ec (nested q1 ... q) etc1 etc ...) )
    ((any?-ec q1 q2             etc1 etc ...)
     (any?-ec (nested q1 q2)    etc1 etc ...) )
    ((any?-ec expression)
     (any?-ec (nested) expression) )

    ((any?-ec qualifier expression)
     (first-ec #f qualifier (if expression) #t) )))

(define-syntax every?-ec
  (syntax-rules (nested)
    ((every?-ec (nested q1 ...) q etc1 etc ...)
     (every?-ec (nested q1 ... q) etc1 etc ...) )
    ((every?-ec q1 q2             etc1 etc ...)
     (every?-ec (nested q1 q2)    etc1 etc ...) )
    ((every?-ec expression)
     (every?-ec (nested) expression) )

    ((every?-ec qualifier expression)
     (first-ec #t qualifier (if (not expression)) #f) )))

