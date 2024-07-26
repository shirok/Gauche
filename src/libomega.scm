;;;
;;; libomega.scm - the stuff to be run after other parts are initialized
;;;
;;;   Copyright (c) 2000-2024  Shiro Kawai  <shiro@acm.org>
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

(select-module gauche)

;; Register built-in modules as provided, so that (use <built-in-modue>) won't
;; complain.
(dolist [m (all-modules)]
  (let1 n (module-name m)
    (provide (module-name->path n))))

;;;
;;; Object system finish-up
;;;
(select-module gauche)

;; A trick to allow slot-ref to be used for compound condition.
;; This is better to be in libexc.scm, but we need to evaluate this
;; after the object system is fully bootstrapped.
(define-method slot-missing ((class <condition-meta>)
                             (cc <compound-condition>)
                             slot)
  (let loop ((members (slot-ref cc '%conditions)))
    (cond [(null? members) (next-method)]
          [(slot-exists? (car members) slot) (slot-ref (car members) slot)]
          [else (loop (cdr members))])))

;; Printing auxiliary error information.  This also is here instead
;; of libexc.scm because of the initialization order.
(define-method report-additional-condition (c port) #f)

(define-method report-additional-condition ((c <load-condition-mixin>) port)
  (and-let* ([p (~ c'port)]
             [ (port? p) ]
             [name (port-name p)]
             [line (port-current-line p)])
    (format port "    While loading ~s at line ~d\n" name line)))

(define-method report-additional-condition ((c <compile-error-mixin>) port)
  (let* ([expr (~ c'expr)]
         [src-info (find (^[si] (and (car si) (cadr si)))
                         ((with-module gauche.internal %source-info) expr))])
    (if src-info
      (format port "    While compiling ~s at line ~d: ~,,,,105:s\n"
              (car src-info) (cadr src-info) expr)
      (format port "    While compiling: ~,,,,90:s\n" expr))))

(define-method report-additional-condition ((c <unbound-variable-error>) port)
  ;; Show potentially missed modules to be imported.
  ;; NB: This is inefficient, for export lists are built for all modules.
  ;; It'd be better to have a predicate that directly queries if a binding
  ;; is exported from a module.
  ;; NB: We may also search non-exported bindings, to detect a binding
  ;; that are missed in the export list.
  (let* ([name (~ c'identifier)]
         [xs (filter (^m (memq name (module-exports m))) (all-modules))]
         [num-xs (length xs)])
    (unless (null? xs)
      (format port
              "    NOTE: `~s' is exported from the following module~p:\n"
              name num-xs)
      (dolist [m xs] (format port "     - ~s\n" (module-name m))))))

;;;
;;; Comparator finish-up
;;;

;; Built-in comparators.  These are here instead of libcmp.scm, for
;; hash functions need to be defined before this.
;; NB: These are in SRFI-114 but not in SRFI-128.  We provide them
;; so that when they are used as the argument of make-hash-table,
;; we can recognize so and use more efficient built-in hash types.
(define eq-comparator
  (make-comparator/compare #t eq? eq-compare eq-hash 'eq))
(define eqv-comparator
  (make-comparator/compare #t eqv? #f eqv-hash 'eqv))
(define equal-comparator
  (make-comparator/compare #t equal? #f default-hash 'equal))
(define string-comparator
  (make-comparator/compare string? string=? compare
                           default-hash 'string))
(define default-comparator
  (make-comparator/compare #t (with-module gauche.internal default-comparator-equal?)
                           compare default-hash 'default))

;; NB: SRFI-128's make-eq-comparator and make-eqv-comparator will NOT
;; return eq-comparator and eqv-comparator.  They're defined in computil.scm.
(define-inline (make-equal-comparator) equal-comparator); SRFI-128
(define-inline (make-default-comparator) default-comparator) ;SRFI-128

(define (comparator-register-default! cmpr) ;SRFI-128
  (unless (is-a? cmpr <comparator>)
    (error "Comparator required, but got:" cmpr))
  (with-module gauche.internal
    (push! *custom-comparators* cmpr)))

;; comparators can be compared by equal? (Gauche extension)
(define-method object-equal? ((x <comparator>) (y <comparator>))
  (and (eqv? (~ x'type-test) (~ y'type-test))
       (eq? (comparator-flavor x) (comparator-flavor y))
       (or (eqv? (~ x'equality-test) (~ y'equality-test))
           (and ((with-module gauche.internal comparator-equality-use-comparison?) x)
                ((with-module gauche.internal comparator-equality-use-comparison?) y)))
       (or (and (not (comparator-ordered? x))
                (not (comparator-ordered? y)))
           (ecase (comparator-flavor x)
             [(ordering) (eqv? (~ x'ordering) (~ y'ordering))]
             [(comparison) (eqv? (~ x'comparison) (~ y'comparison))]))
       (or (and (not (comparator-hashable? x))
                (not (comparator-hashable? y)))
           (eqv? (~ x'hash) (~ y'hash)))
       (equal? (slot-ref x 'name) (slot-ref y 'name))))

;; Extend default-comparator by SRFI-128 way
;;
;;  Our default-comparator already handles any type of objects,
;;  and delegates customized behavior for object-equal?, object-compare
;;  and object-hash.  So we set up the "catch-all" methods for
;;  these and dispatches to the comparators registered by
;;  comparator-register-default!.
(select-module gauche.internal)
(define *custom-comparators* '())

(define (%choose-comparator-1 a)
  (find (cut comparator-test-type <> a) *custom-comparators*))
(define (%choose-comparator-2 a b)
  (find (^c (and (comparator-test-type c a) (comparator-test-type c b)))
        *custom-comparators*))

(define-method object-equal? (a b)
  (if-let1 c (%choose-comparator-2 a b)
    (=? c a b)
    #f))
(define-method object-compare (a b)
  (if-let1 c (%choose-comparator-2 a b)
    (comparator-compare c a b)
    (errorf "Object ~s and ~s are not comparable" a b)))
;; NB: We 'catch-all' on one-arg object-hash, for two arg object-hash will
;; delegate to one-arg in order to maintain the backward compatibility.
;; See below for two-arg object-has base method.
(define-method object-hash (a)
  (or (and-let* ([c (%choose-comparator-1 a)]
                 [ (comparator-hashable? c) ])
        (comparator-hash c a))
      ;; default-hash needs to accept arbitrary Scheme object, so object-hash
      ;; must return something.  If we can know that the user-defined type
      ;; uses eq? or eqv? for the comparison, we can do better (using
      ;; eq-hash or eqv-hash) but with the layer of customiation it is
      ;; quite difficult to know.  So here we are.
      (let1 v (eq-hash 57)  ;Grothendieck prime, for no reason.
        ;; If we're not computing a portable hash, sprinkle a grain of salt.
        (if (eq? (%current-recursive-hash) default-hash)
          (let1 v1 (* (hash-salt) v)
            (logxor (ash v1 -32) v1))
          v))))

;;;
;;; Hash function fixup
;;;

;; Recursive hash function
;; This is here instead of in libdict.scm, for we need the rest of
;; the runtime to be initialized.
;;
;; Equal-hash can be extended by defining object-hash method.  We used
;; to have just one hash function of this type, 'hash', so the existing
;; object-hash calls 'hash' whenever it needs to recurse.
;;
;; Now that we have mutliple hash functions that works as equal hash,
;; we need a way for object-hash to recursively call the proper equal
;; hash function.  Thus we pass the current hash function as the second
;; argument of object-hash.
;;
;; However, we can't break the existing object-hash code that takes
;; just one argument, an object to hash, and recursively calls 'hash'
;; function.  So here's a trick we employed:
;;
;;   1. The 'hash' function actually dispatches to the current recursive
;;      hash function.
;;   2. The default of current recursive hash function is the
;;      legacy-hash, for the compatibility.
;;   3. During the execution of 'portable-hash' or 'default-hash',
;;      we set the current recursive hash function to it so that
;;      'hash' actually calls back to the proper hash function.
;;

(select-module gauche.internal)

;; %call-object-hash is called from equal_hash_common (hash.c) to invoke
;; object-hash method.  HASH argument is either default-hash or portable-hash,
;; and salt argument is #f (for default-hash, since it takes salt value from
;; hash-salt parameter) or an exact integer (for portable-hash).
;;
;; TODO: We may memoize hash&salt pair so that we can avoid
;; closure allocation for every recursive call; the memoization also
;; benefits bypassing dynamic-wind in object-hash.

(define (%call-object-hash obj hash salt)
  (object-hash obj (if salt (^o (hash o salt)) hash)))

;; This is the fallback in case we have legacy one-argument object-hash.
(define-method object-hash (obj hash)
  (let1 h (%current-recursive-hash)
    (if (eq? h hash)
      (object-hash obj) ; shortcut
      (parameterize ([%current-recursive-hash hash])
        (object-hash obj)))))

;; Make hashtable hashable.  For the time being, we ignore hashtable's
;; comparators.
(define-method object-hash ((h <hash-table>) hash)
  (hash-table-fold h (^[k v r] (logxor (logxor (hash k) (hash v)) r)) 0))

(define-method object-equal? ((a <hash-table>) (b <hash-table>))
  (define (subset? x y)
    (not (hash-table-find x (^[k v] (not (and (hash-table-exists? y k)
                                              (=? default-comparator
                                                  v (hash-table-get y k))))))))
  (and (equal? (hash-table-comparator a) (hash-table-comparator b))
       (subset? a b)
       (subset? b a)))

;; Make charset hashable
(define-method object-hash ((c <char-set>) _) (char-set-hash c))

;;;
;;; Read-edit mode flag
;;;

(with-module gauche.internal
  ;; This variable affects how gauche.interactive handles input; if true,
  ;; it sets up the input editor if possible.  This is also modified by
  ;; main.c with -fread-edit/-fno-read-edit flag.
  ;; Note that once gauche.interactive is loaded, this variable has no effect.
  (define *read-edit* (not (sys-getenv "GAUCHE_NO_READ_EDIT")))
  )

;;;
;;; Stdio setup
;;;

(with-module gauche.internal
  (when (or (and (sys-isatty (standard-input-port))
                 (sys-isatty (standard-output-port)))
            (and (%sys-mintty? (standard-input-port))
                 (%sys-mintty? (standard-output-port))))
    (port-link! (standard-input-port) (standard-output-port))))

;;;
;;; Miscellaneous
;;;

;; Make %transfer-bindings unavailable.  It is only for initialization.
(with-module gauche.internal
  (set! %transfer-bindings #f))

;;
;; Turn on generic dispatcher on selected gfs.
;; Eventually, we automate attaching dispacher whenever gf meets certain
;; criteria.  The dispatcher is so effective, (e.g. ref <vector>
;; gets 8x speedup) so we manually turn it on for soem gfs.
;; In case if bug is found in dispatcher mechanism, use -fno-generic-dispatcher
;; option to turn off dispatchers.
;;
(with-module gauche.object
  (generic-build-dispatcher! ref 0)
  (generic-build-dispatcher! object-apply 0)
  )
