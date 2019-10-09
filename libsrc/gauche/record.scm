;;;
;;; gauche.record - record implementation
;;;
;;;   Copyright (c) 2010-2019  Shiro Kawai  <shiro@acm.org>
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

(define-module gauche.record
  (use gauche.sequence)
  (use gauche.uvector)
  (use util.match)

  (export <record-meta> <record>
          <pseudo-record-meta> <pseudo-record> pseudo-rtd
          record? record-rtd rtd-name rtd-parent
          rtd-field-names rtd-all-field-names rtd-field-mutable?
          make-rtd rtd? rtd-constructor rtd-predicate rtd-accessor rtd-mutator
          define-record-type)
  )
(select-module gauche.record)

;; <record> is srfi-9 and srif-99 compatible record types, integrated
;; into Gauche object system, and implemented for efficiency.
;;
;; rtd is a Gauche class with metaclass <record-meta>.  It has more
;; restricted semantics than the ordinary Gauche classes:
;;
;;  - It cannot be redefined - definining the class of the same name
;;    just creates a new class, but the original class and its instances
;;    remain intact.
;;  - The accessors are inlined - so they are fast, but if you redefine
;;    the record, procedures that are using accessors and mutators must
;;    be recompiled.
;;  - The record class must form a single implementation inheritance.

;;  Pseudo records can be used just like records in the code, but at
;;  runtime you cannot distinguish them from ordinary lists or vectors.
;;  They have some advantages:
;;
;;   - Fast.  Operations on pseudo records can be expanded into operations
;;     on primitive data structures (e.g. car, vector-ref, etc), which
;;     will be compiled into efficient VM instructions.
;;   - Less module interdependency.  You can ask callers to pass in
;;     the data by a vector or a list, instead of asking them to import your
;;     record definition.  It is less safe but more flexible, especially
;;     the caller is a foreign entity.

;; some convenience utility

(define-macro (sym+ . args)
  `((with-module gauche.internal identifier-append) ,@args))

(define-macro (for-each-subst subs . forms)
  (define (walk x sub)
    (cond [(symbol? x)
           (string->symbol (regexp-replace #/\?\?/ (symbol->string x) sub))]
          [(pair? x) (cons (walk (car x) sub) (walk (cdr x) sub))]
          [else x]))
  `(begin
     ,@(append-map (^[sub] (map (cute walk <> (symbol->string sub)) forms))
                   subs)))

;;;
;;; Infrastructure
;;;

(define-class <record-meta> (<class>)
  ((field-specs :init-keyword :field-specs :init-value '#())
   ;; - A vector of field specs, as specified in the fieldspecs argument
   ;;   of make-rtd.  This only includes direct slots.
   (positional-field-accessors)
   ;; - A vector of getter/setter pairs, corresponding to the layout
   ;;   of the instance; that is, base record slots comes first.
   ;;   The setter can be nil if the slot is immutable.
   ;;   This is used to access slots positionally; e.g. util.match's '$'.
   ))
(define-class <record> () () :metaclass <record-meta>)

(define-class <pseudo-record-meta> (<record-meta>)
  ((instance-class :init-keyword :instance-class)))
(define-class <pseudo-record> () () :metaclass <pseudo-record-meta>)

(for-each-subst
 (list vector u8vector s8vector u16vector s16vector u32vector s32vector
       u64vector s64vector f16vector f32vector f64vector)
 (define-class <??-pseudo-record-meta> (<pseudo-record-meta>) ())
 (define-class <??-pseudo-record> () ()
   :metaclass <??-pseudo-record-meta>)
 ;; Ugly - we need CLOS-like :default-initargs.
 (define-method initialize ((class <??-pseudo-record-meta>) initargs)
   (next-method)
   (slot-set! class 'instance-class <??>))
 (define-method %pseudo-rtd ((class <??-meta>))
   <??-pseudo-record>)
 (define-method %pseudo-rtd ((class <??-pseudo-record-meta>))
   <??-pseudo-record>)
 )

(define-method %pseudo-rtd (other) #f)

(define (pseudo-rtd class)
  (or (%pseudo-rtd class)
      (error "pseudo-rtd requires a class object <vector>, <list>, \
              <u8vector>, <s8vector>, <u16vector>, <s16vector>, \
              <u32vector>, <s32vector>, <u64vector>, <s64vector>, \
              <f16vector>, <f32vector>, <f64vector>, \
              or other pseudo-rtd, but got" class)))

(define-method initialize ((class <record-meta>) initargs)
  (next-method)
  (let1 index&p  ; [(index . (ref . set))]
      (map (^[slotdef]
             (if-let1 i (get-keyword :index (cdr slotdef) #f)
               (cons i
                     (cons (cut instance-slot-ref <> i <...>)
                           (and (not (get-keyword :immutable (cdr slotdef) #f))
                                (cut instance-slot-set! <> i <>))))
               (errorf "Record type definition has non-instance slots `~s': \
                        MOP implementation error?" (car slotdef))))
           (~ class'slots))
    (slot-set! class 'positional-field-accessors
               (map-to <vector> cdr (sort index&p < car)))))

;; We just collect ancestor's slots, ignoring duplicate slot names.
;; R6RS records require the same name slot merely shadows ancestors' one,
;; not changing the index of the fields.
;; NB: we make the direct slots come first, so that access via
;; slot-ref/slot-set! prefer direct slots to inherited slots
;; with the same name.
(define-method compute-slots ((class <record-meta>))
  (fold (^[c r] (append (slot-ref c'direct-slots) r))
        '() (reverse (slot-ref class'cpl))))

(define-method compute-get-n-set ((class <record-meta>) slot)
  ;; trick: we let (next-method) adjust the instance slot count, but
  ;; we uses :index option of the slot to determine the actual slot number.
  (next-method)
  (let1 s
      (compute-slot-accessor class slot (slot-definition-option slot :index))
    (if (slot-definition-option slot :immutable #f)
      `(,(^o (slot-ref-using-accessor o s))
        ,(^[o v] (errorf "slot ~a of ~a is immutable"
                         (slot-definition-name slot) o))
        ,(^o (slot-bound-using-accessor? o s)))
      s)))

(define (%valid-fieldspecs? specs signal-error?)
  (define (id? x) (or (symbol? x) (identifier? x)))
  (if (not (vector? specs))
    (if signal-error?
      (error "make-rtd: fieldspecs must be a vector, but got" specs)
      #f)
    (every (^[spec]
             (match spec
               [((or 'mutable 'immutable) (? id? name)) #t]
               [(? id? name) #t]
               [other (if signal-error?
                        (error "make-rtd: invalid field spec:" other)
                        #f)]))
           (vector->list specs)))) ;; efficiency?

(define (fieldspecs->slotspecs specs offset)
  (%valid-fieldspecs? specs #t)
  (map-with-index
   (^[k spec]
     (match spec
       [('mutable   name)
        `(,(unwrap-syntax name) :index ,(+ k offset))]
       [('immutable name)
        `(,(unwrap-syntax name) :immutable #t :index ,(+ k offset))]
       [name `(,(unwrap-syntax name) :index ,(+ k offset))]))
   specs))

(define (%check-rtd obj)
  (unless (rtd? obj) (error "rtd required, bot got" obj)))

;;;
;;; Inspection layer
;;;

(define-inline (record? obj) (is-a? obj <record>))

(define-inline (record-rtd obj)
  (unless (record? obj) (error "record required, but got" obj))
  (class-of obj))

(define (rtd-name rtd) (%check-rtd rtd) (class-name rtd))

(define (rtd-parent rtd)
  (%check-rtd rtd)
  (find (^c (and (is-a? c <record-meta>) (not (eq? c <record>))))
        (cdr (class-precedence-list rtd))))

(define (rtd-field-names rtd)
  (%check-rtd rtd)
  (map-to <vector> slot-definition-name (class-direct-slots rtd)))

(define (rtd-all-field-names rtd)
  (%check-rtd rtd)
  (let loop ([rtd (rtd-parent rtd)]
             [r (map slot-definition-name (class-direct-slots rtd))])
    (if rtd
      (loop (rtd-parent rtd)
            (fold-right (^[s r] (cons (slot-definition-name s) r))
                        r (class-direct-slots rtd)))
      (list->vector r))))

(define (rtd-field-mutable? rtd field)
  (%check-rtd rtd)
  (if-let1 s (assq field (class-slots rtd))
    (not (slot-definition-option s :immutable #f))
    (error "rtd-mutable?: ~a does not have a slot ~a" rtd field)))

;; We need to specialize this, for the default method uses slot names;
;; but a record type may have multiple slots with the same name.
(define *unique* (list #f))

(define-method describe-slots ((obj <record>))
  (let* ([class (class-of obj)]
         [slots (sort (class-slots class) < (^s (get-keyword :index (cdr s))))]
         [accessors (slot-ref class 'positional-field-accessors)])
    (unless (null? slots)
      (format #t "slots:\n")
      (dolist [s slots]
        (let* ([index (get-keyword :index (cdr s))]
               [val   ((car (vector-ref accessors index)) obj *unique*)])
          (format #t "  ~10s: ~a\n" (slot-definition-name s)
                  (if (eq? val *unique*)
                    "#<unbound>"
                    (with-output-to-string (^[] (write-limited val 60))))))))))

;;;
;;; Procedural layer
;;;

(define (make-rtd name fieldspecs :optional (parent #f) :rest opts)
  (make (if parent (class-of parent) <record-meta>)
    :name name :field-specs fieldspecs :metaclass <record-meta>
    :supers (list (or parent <record>))
    :slots ($ fieldspecs->slotspecs fieldspecs
              $ if parent (length (class-slots parent)) 0)))

(define (rtd? obj) (is-a? obj <record-meta>))

;; For conciseness.  We need to use macros (for now) to ensure Gauche compiler
;; optimize the gref away---a kludge not recommended in general.
(define-macro (%make)  '(with-module gauche.object %make-record))
(define-macro (%makev) '(with-module gauche.object %make-recordv))

;; We dispatch by the number of slots to initialize, for fixed-argument
;; lambdas can be optimized more easily.
(define-macro (define-ctor-generator name 01-maker body-maker rest-maker)
  `(define-macro (,name rtd len)
     (define precalc-args 10)
     (define tmps (map (^_(gensym)) (iota (+ precalc-args 1))))
     `(case ,len
        [(0) ,(let1 vars '() `(lambda ,vars ,,01-maker))]
        ,@(map (^n (let1 vars (drop tmps (- precalc-args n -1))
                     `[(,n) (lambda ,vars ,,body-maker)]))
               (iota (- precalc-args 1) 1))
        [else (lambda (,@(cdr tmps) . ,(car tmps))
                ,,rest-maker)])))

 (define-macro (define-ctor-generators ctor-name-base make1 make* makev)
  (define default-name (sym+ ctor-name-base '-default))
  (define custom-name  (sym+ ctor-name-base '-custom))
  `(begin
     (define-ctor-generator ,default-name ,make1 ,make1 ,make*)
     (define-ctor-generator ,custom-name ,make1
       (let1 argv (gensym)
         `(let1 ,argv (make-vector nfields)
            ,@(map-with-index (^(i v)
                                `(vector-set! ,argv (vector-ref mapvec ,i) ,v))
                              vars)
            ,,makev))
       (let ([argv (gensym)] [i (gensym)] [restvar (car tmps)])
         `(let1 ,argv (make-vector nfields)
            ,@(map-with-index (^(i v)
                                `(vector-set! ,argv (vector-ref mapvec ,i) ,v))
                              (cdr tmps))
            (do ([,restvar ,restvar (cdr ,restvar)]
                 [,i ,precalc-args (+ ,i 1)])
                [(null? ,restvar)]
              (vector-set! ,argv (vector-ref mapvec ,i) (car ,restvar)))
            ,,makev)))))

(define-ctor-generators %record-ctor
  `((%make) ,rtd ,@vars)
  `(apply (%make) ,rtd ,@(cdr tmps) ,(car tmps))
  `((%makev) ,rtd ,argv))

(define (vector->vector x) x)

(for-each-subst
 (list vector u8vector s8vector u16vector s16vector u32vector s32vector
       u64vector s64vector f16vector f32vector f64vector)
 (define-ctor-generators %??-ctor
   `(?? ,@vars)
   `(apply ?? ,@(cdr tmps) ,(car tmps))
   `(vector->?? ,argv)))

;; Returns a vector where V[k] = i means k-th argument of the constructor
;; initializes i-th field.
(define (%calculate-field-mapvec allnames fieldspecs)
  (define (bad f)
    (error "rtd-constructor: field-specs contains unrecognized field name:" f))
  (let1 cat (reverse (map-with-index (^[a b] (cons b a)) allnames))
    (map-to <vector> (^f (cond [(assq f cat) => cdr] [else (bad f)]))
            fieldspecs)))

(define-method %rtd-predicate ((rtd <record-meta>)) (^o (is-a? o rtd)))
(define-method %rtd-predicate ((rtd <pseudo-record-meta>))
  (let ([iclass (slot-ref rtd 'instance-class)]
        [size   (vector-length (slot-ref rtd 'field-specs))])
    ;; NB: We allow instance to be larger than expected, for it's useful
    ;; to pretend the beginning part of the longer data as a struct
    ;; (e.g. header of some binary data).
    (^o (and (is-a? o iclass) (>= (size-of o) size)))))

;; returns (index immutable?)
(define (%get-slot-index rtd field modify?)
  (%check-rtd rtd)
  (if-let1 s (assq field (class-slots rtd))
    (values (slot-definition-option s :index)
            (slot-definition-option s :immutable #f))
    (errorf "record ~s does not have a slot named ~s" rtd field)))

(define-macro (define-rtd-methods rtd-meta ctor-name-base referencer* mutator*)
  (define gen-default-ctor (sym+ ctor-name-base '-default))
  (define gen-custom-ctor  (sym+ ctor-name-base '-custom))
  `(begin
     (define-method %rtd-constructor ((rtd ,rtd-meta) . rest)
       (%check-rtd rtd)
       (if (null? rest)
         (,gen-default-ctor rtd (length (slot-ref rtd'slots)))
         (let1 all-names (rtd-all-field-names rtd)
           (let ([mapvec  (%calculate-field-mapvec all-names (car rest))]
                 [nfields (vector-length all-names)])
             (,gen-custom-ctor rtd (vector-length (car rest)))))))
     (define-method %rtd-accessor ((rtd ,rtd-meta) field)
       (receive (k immutable?) (%get-slot-index rtd field #f)
         (if immutable?
           (^o (,@referencer* o k))
           (getter-with-setter
            (^o (,@referencer* o k))
            (^(o v) (,@mutator* o k v))))))
     (define-method %rtd-mutator ((rtd ,rtd-meta) field)
       (receive (k immutable?) (%get-slot-index rtd field #t)
         (when immutable?
           (errorf "slot ~a of record ~s is immutable" field rtd))
         (^(o v) (,@mutator* o k v))))
     ))
  
(define-rtd-methods <record-meta>
  %record-ctor
  ((with-module gauche.object %record-ref) rtd)
  ((with-module gauche.object %record-set!) rtd))

(for-each-subst
 (list vector u8vector s8vector u16vector s16vector u32vector s32vector
       u64vector s64vector f16vector f32vector f64vector)
 (define-rtd-methods <??-pseudo-record-meta>
   %??-ctor
   (??-ref)
   (??-set!))
 )

(define (rtd-predicate rtd) (%rtd-predicate rtd))
(define (rtd-constructor rtd . args) (apply %rtd-constructor rtd args))
(define (rtd-accessor rtd field) (%rtd-accessor rtd field))
(define (rtd-mutator rtd field) (%rtd-mutator rtd field))

;; These methods auguments positional field match using $ in util.match.
;; Provides a way to get/set NUM-th instance field.  Note that the order
;; of class-slots doesn't corresponds to the instance layout, since the
;; class-slots have slots of derived classes come first, while the instance
;; has parent class's slots first.
(define-method match:$-ref ((class <record-meta>) num obj)
  ((car (vector-ref (slot-ref class 'positional-field-accessors) num)) obj))

(define-method (setter match:$-ref) ((class <record-meta>) num obj val)
  (let1 m (vector-ref (slot-ref class 'positional-field-accessors) num)
    (or (and (cdr m) ((cdr m) obj val))
        (errorf "attempt to set immutable field `~a' of ~s"
                ($ slot-definition-name
                   (find (^s (= num (get-keyword :index (cdr s))))
                         (class-slots class)))))))

;;;
;;; Syntactic layer
;;;

;; TODO: Rewriter this w/ er-macro after 0.9.6 release
(define-macro (define-record-type type-spec ctor-spec pred-spec . field-specs)
  (define (->id x) ((with-module gauche.internal make-identifier) x
                    (find-module 'gauche.record) '()))
  (define (id? x)  (or (symbol? x) (identifier? x)))
  (define %make (->id 'make-rtd))
  (define %ctor (->id 'rtd-constructor))
  (define %pred (->id 'rtd-predicate))
  (define %asor (->id 'rtd-accessor))
  (define %mtor (->id 'rtd-mutator))
  (define %define-inline (->id 'define-inline))
  (define tmp   (gensym))
  (define (build-field-spec)
    (map-to <vector> (match-lambda
                       [((? id? f) a s) f]
                       [((? id? f) a) `(immutable ,f)]
                       [((? id? f)) f]
                       [(? id? f) `(immutable ,f)]
                       [x (error "invalid field spec:" x)])
            field-specs))
  (define (build-def typename parent)
    `(,%define-inline ,typename
       (,%make ',typename ',(build-field-spec) ,@(if parent `(,parent) '()))))
  (define (build-ctor typename)
    (match ctor-spec
      [#f '()]
      [#t `((,%define-inline ,(sym+ 'make- typename) (,%ctor ,typename)))]
      [((? id? ctor-name) field ...)
       `((,%define-inline ,ctor-name (,%ctor ,typename ',(list->vector field))))]
      [(? id? ctor-name)
       `((,%define-inline ,ctor-name (,%ctor ,typename)))]
      [x (error "invalid constructor spec" ctor-spec)]))
  (define (build-pred typename)
    (match pred-spec
      [#f '()]
      [#t `((,%define-inline (,(sym+ typename '?) ,tmp)
              ((,%pred ,typename) ,tmp)))]
      [(? id? pred-name)
       `((,%define-inline (,pred-name ,tmp) ((,%pred ,typename) ,tmp)))]
      [x (error "invalid predicate spec" pred-spec)]))
  (define (build-accessors typename)
    (map (match-lambda
           [(f a . _) `(,%define-inline ,a (,%asor ,typename ',f))]
           [(f)       `(,%define-inline ,(sym+ typename '- f)
                         (,%asor ,typename ',f))]
           [f         `(,%define-inline ,(sym+ typename '- f)
                         (,%asor ,typename ',f))])
         field-specs))
  (define (build-mutators typename)
    (append-map (match-lambda
                  [(f a m) `((,%define-inline ,m (,%mtor ,typename ',f)))]
                  [(f)     `((,%define-inline ,(sym+ typename '- f '-set!)
                               (,%mtor ,typename ',f)))]
                  [_ '()])
                field-specs))

  (receive (typename parent) (match type-spec
                               [((? id? name) parent) (values name parent)]
                               [(? id? name) (values name #f)]
                               [x (error "invalid type-spec" type-spec)])
    `(begin ,(build-def typename parent)
            ,@(build-ctor typename)
            ,@(build-pred typename)
            ,@(build-accessors typename)
            ,@(build-mutators typename)))
    )
