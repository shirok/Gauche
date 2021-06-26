;;;
;;; libbox.scm - Boxes
;;;
;;;   Copyright (c) 2000-2021  Shiro Kawai  <shiro@acm.org>
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

;; box is built-in since it is used in equal? implementation.  See
;; %interleave-equal? in libbool.scm.

;;;
;;;  srfi-111 & srfi-195 box
;;;

;; We use them in equal? implementation as well, so here we go...
(define-cproc box (:optarray (argv argc 1) :rest rest)
  (cond
   [(== argc 0) (return (SCM_OBJ (Scm_MakeMVBox 0 SCM_FALSE)))]
   [(SCM_NULLP rest) (return (SCM_OBJ (Scm_MakeBox (aref argv 0))))]
   [else (return (SCM_OBJ (Scm_ListToMVBox (Scm_Cons (aref argv 0) rest))))]))

(define-cproc box? (v) ::<boolean> (return (or (SCM_BOXP v) (SCM_MVBOXP v))))

(define-cproc unbox (b)
  (cond
   [(SCM_BOXP b) (return (SCM_BOX_VALUE b))]
   [(SCM_MVBOXP b) (return (Scm_ValuesFromArray (SCM_MVBOX_VALUES b)
                                                (SCM_MVBOX_SIZE b)))]
   [else (SCM_TYPE_ERROR b "<box> or <mv-box>") (return SCM_UNDEFINED)]))

(define-cproc set-box! (b :rest vs) ::<void>
  (cond
   [(SCM_BOXP b)
    (unless (and (SCM_PAIRP vs) (SCM_NULLP (SCM_CDR vs)))
      (Scm_Error "Wrong number of values to set to a single-value box %S: %S"
                 b vs))
    (SCM_BOX_SET b (SCM_CAR vs))]
   [(SCM_MVBOXP b)
    (let* ([argc::ScmSmallInt (Scm_Length vs)]
           [i::ScmSmallInt 0])
      (unless (== argc (SCM_MVBOX_SIZE b))
        (Scm_Error "Wrong number of values to set to a multi-value box %S: %S"
                   b vs))
      (for (() (< i argc) (begin (post++ i) (set! vs (SCM_CDR vs))))
        (set! (aref (SCM_MVBOX_VALUES b) i) (SCM_CAR vs))))]
   [else (SCM_TYPE_ERROR b "<box> or <mv-box>")]))

(define-cproc box-arity (b) ::<int>
  (cond
   [(SCM_BOXP b) (return 1)]
   [(SCM_MVBOXP b) (return (SCM_MVBOX_SIZE b))]
   [else (SCM_TYPE_ERROR b "<box> or <mv-box>")]))

(define-cproc unbox-value (b i::<fixnum>)
  (cond
   [(SCM_BOXP b)
    (unless (== i 0) (Scm_Error "index out of range for %S: %d" b i))
    (return (SCM_BOX_VALUE b))]
   [(SCM_MVBOXP b)
    (unless (and (<= 0 i)
                 (< i (SCM_MVBOX_SIZE b)))
      (Scm_Error "index out of range for %S: %d" b i))
    (return (aref (SCM_MVBOX_VALUES b) i))]
   [else (SCM_TYPE_ERROR b "<box> or <mv-box>")]))

(define-cproc set-box-value! (b i::<fixnum> val) ::<void>
  (cond
   [(SCM_BOXP b)
    (unless (== i 0) (Scm_Error "index out of range for %S: %d" b i))
    (SCM_BOX_SET b val)]
   [(SCM_MVBOXP b)
    (unless (and (<= 0 i)
                 (< i (SCM_MVBOX_SIZE b)))
      (Scm_Error "index out of range for %S: %d" b i))
    (set! (aref (SCM_MVBOX_VALUES b) i) val)]
   [else (SCM_TYPE_ERROR b "<box> or <mv-box>")]))

;;; EXPERIMENTAL

;; Counted box is an immutable box with MT-safe counter.
;; The counter can keep track of, for example, the number of
;; objects that shares the particular information kept in the box.
;; Notably, if the counter is one, you can be sure that no other
;; objects refers to the box so that you can modify the object(s)
;; kept in it without being worried that you affect the mutation
;; to others.
;;
;; Although the counter operation is MT-safe, it is not an alternative
;; of mutex.  If the object can be mutated from multiple threads, you have
;; to protect it with a separate mutex.
;;
;; The counter is neither a reference counter in a strict sense; the
;; references to a counted box can be kept in an environment, for example.
;;
;; (This is not essential in core, so some other place like ext/data may
;; be suitable, but access to the atomicP.h requires a bit of fiddling on
;; compiler flags.)
;;
;; See https://srfi-email.schemers.org/srfi-discuss/msg/16912128/
;; for the discussion triggered this implementation.

(use gauche.internal)

(inline-stub
 (.include "gauche/priv/atomicP.h")

 (define-ctype ScmCountedBox
   ::(.struct ScmCountexBoxRec
              (hdr::ScmHeader
               counter::ScmAtomicVar
               numValues::ScmSmallInt
               values::(.array ScmObj (1)))))

 (define-cclass <counted-box> :base :private :no-meta
   "ScmCountedBox*" "Scm_CountedBoxClass"
   (c "SCM_CLASS_DEFAULT_CPL")
   ())
 )

;; API
(define-cproc make-counted-box (initial-count::<fixnum> :rest values)
  (let* ([numVals::ScmSmallInt (Scm_Length values)]
         [z::ScmCountedBox* (SCM_NEW2 (.type ScmCountedBox*)
                                      (+ (sizeof (.type ScmCountedBox*))
                                         (* (sizeof (.type ScmObj))
                                            (- numVals 1))))]
         [i::int 0])
    (SCM_SET_CLASS z (& Scm_CountedBoxClass))
    (AO_store (& (-> z counter)) (cast ScmAtomicWord initial-count))
    (set! (-> z numValues) numVals)
    (dolist [v values]
      (set! (aref (-> z values) (post++ i)) v))
    (return (SCM_OBJ z))))

;; API
(define-cproc counted-box-unbox (cb::<counted-box>)
  (return (Scm_ValuesFromArray (-> cb values) (-> cb numValues))))

;; API
(define-cproc counted-box-ref (cb::<counted-box> k::<fixnum>)
  (when (or (< k 0) (>= k (-> cb numValues)))
    (Scm_Error "Index out of range: %d" k))
  (return (aref (-> cb values) k)))

;; API
(define-cproc counted-box-arity (cb::<counted-box>) ::<fixnum>
  (return (-> cb numValues)))

;; API
(define-cproc counted-box-count (cb::<counted-box>)
  (let* ([v::ScmAtomicWord (AO_load (& (-> cb counter)))])
    ;; NB: We might need an api to convert ScmAtomicWord to Scheme integer.
    (return (Scm_MakeInteger (cast long v)))))

;; API
(define-cproc counted-box-inc! (cb::<counted-box> delta::<fixnum>)
  (for ()
    (let* ([v::ScmAtomicVar (AO_load (& (-> cb counter)))]
           [vv::ScmAtomicWord (+ v delta)])
      (when (AO_compare_and_swap_full (& (-> cb counter)) v vv)
        (return (Scm_MakeInteger (cast long v)))))))
