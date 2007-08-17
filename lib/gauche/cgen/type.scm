;;;
;;; gauche.cgen.type - type management
;;;  
;;;   Copyright (c) 2004-2007  Shiro Kawai  <shiro@acm.org>
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
;;;  $Id: type.scm,v 1.1 2007-08-17 04:19:48 shirok Exp $
;;;

(define-module gauche.cgen.type
  (use gauche.mop.instance-pool)
  (export <cgen-type>
          cgen-type-from-name)
  )
(select-module gauche.cgen.type)

;; NB: a small experiment to see how I feel this...
;;  [@ a b c d] => (ref (ref (ref a b) c) d)
;; In string interpolations I have to use ,(@ ...) instead of ,[@ ...], for
;; the previous versions of interpolation code doesn't like #`",[...]".
;; Ideally this should be a compiler-macro (we can't make it a macro,
;; for we want to say (set! [@ x'y] val).
(define @
  (getter-with-setter
   (case-lambda
     ((obj selector) (ref obj selector))
     ((obj selector . more) (apply @ (ref obj selector) more)))
   (case-lambda
     ((obj selector val) ((setter ref) obj selector val))
     ((obj selector selector2 . rest)
      (apply (setter ref) (ref obj selector) selector2 rest)))))
;; end experiment

;;===================================================================
;; Type handling
;;

;; Stub's type system doesn't exactly match Scheme's, since stub has
;; to handle internal guts of Scheme implementations as well as
;; C type systems.  We call the types used in the stub generator
;; "stub type", apart from "C type" and "Scheme type".
;;
;; For each existing conversion between C type and Scheme type, a stub
;; type is defined.  For types that has one-to-one mapping between
;; C and Scheme (such as most aggregate types, for example, Scheme's
;; <u32vector> and C's ScmU32Vector*), there is only one stub type,
;; which uses the same name as the Scheme's.  There are some stub types
;; that reflects C type variations: <int>, <int8>, <int16>, <int32>,
;; <uint>, <uint8>, <uint16>, <uint32> --- these are mapped to Scheme's
;; integer, but the range limit is taken into account.   <fixnum>
;; refers to the integers that can be represented in an immediate integer.
;; Note that a stub type <integer> corresponds to Scheme's exact integers,
;; but it is mapped to C's ScmObj, since C's integer isn't enough to
;; represent all of Scheme integers.   A stub type <void> is
;; used to denote a procedure return type.
;;
;; Each stub type has a "boxer" and an "unboxer".  A boxer is a C name
;; of a function or a macro that takes an object of C type of the stub
;; type and returns a Scheme object.  An unboxer is a C name of a function
;; or a macro that takes Scheme object and checks its vailidy, then
;; returns a C object of the C type or throws an error.
;;
;; Here's a summary of primitive stub types and the mapping each one
;; represents.
;;
;;   stub type    Scheme       C           Notes
;;  -----------------------------------------------------------------
;;   <fixnum>     <integer>    int         Integers within fixnum range
;;   <integer>    <integer>    ScmObj      Any exact integers
;;   <real>       <real>       double
;;   <number>     <number>     ScmObj      Any numbers
;;
;;   <int>        <integer>    int         Integers representable in C
;;   <int8>       <integer>    int
;;   <int16>      <integer>    int
;;   <int32>      <integer>    int
;;   <short>      <integer>    short
;;   <long>       <integer>    long
;;   <uint>       <integer>    uint        Integers representable in C
;;   <uint8>      <integer>    uint
;;   <uint16>     <integer>    uint
;;   <uint32>     <integer>    uint
;;   <ushort>     <integer>    ushort
;;   <ulong>      <integer>    ulong
;;   <float>      <real>       float       Unboxed value casted to float
;;   <double>     <real>       double      Alias of <real>
;;
;;   <boolean>    <boolean>    int         Boolean value
;;   <char>       <char>       ScmChar     NB: not a C char
;;
;;   <void>       -            void        (Used only as a return type.
;;                                          Scheme function returns #<undef>)
;;
;;   <const-cstring> <string>  const char* For arguments, string is unboxed
;;                                         by Scm_GetStringConst.
;;                                         For return values, C string is boxed
;;                                         by SCM_MAKE_STR_COPYING.
;;
;;   <pair>       <pair>       ScmPair*
;;   <list>       <list>       ScmObj
;;   <string>     <string>     ScmString*
;;   <symbol>     <symbol>     ScmSymbol*
;;   <vector>     <vector>     ScmVector*
;;    :
;;
;; Pointer types can be qualified as 'maybe', by adding '?' at the
;; end of type name, e.g. '<string>?'.
;; If 'maybe' type appears as an argument type, the argument accepts #f
;; as well as the specified type, and translates #f to NULL.  If 'maybe'
;; type appears as the return type, the result of C expression can be NULL
;; and the stub translates it to #f.

;; Stub type definition
(define-class <cgen-type> (<instance-pool-mixin>)
  ((name        :init-keyword :name)
   ;; ::<symbol> - name of this stub type.
   (c-type      :init-keyword :c-type)
   ;; ::<string> - C type name this stub type represents
   (description :init-keyword :description)
   ;; ::<string> - used in the type error message
   (c-predicate :init-keyword :c-predicate)
   ;; ::<string> - name of a C function (macro) to find out the given
   ;;              ScmObj has a valid type for this stub type.
   (unboxer     :init-keyword :unboxer)
   ;; ::<string> - name of a C function (macro) that takes Scheme object
   ;;              and returns a C object.
   (boxer       :init-keyword :boxer :init-value "SCM_OBJ_SAFE")
   ;; ::<string> - name of a C function (macro) that takes C object
   ;;              and returns a Scheme Object.
   (maybe       :init-keyword :maybe       :init-value #f)
   ;; ::<type>? - base type, if this is 'maybe' qualified type.
   ))

(define (cgen-type-from-name name)
  (or (find (lambda (type) (eq? [@ type'name] name))
            (instance-pool->list <cgen-type>))
      ;; when 'maybe' qualified type is used for the first time, we
      ;; create it from the base type.
      (and-let* ((m (#/\?$/ (symbol->string name)))
                 (basename (string->symbol (m 'before)))
                 (basetype (find-type-by-name basename)))
        (make <type> :name name :c-type [@ basetype'c-type]
              :description #`",(@ basetype'description) or #f"
              :c-predicate [@ basetype'c-predicate]
              :unboxer     [@ basetype'unboxer]
              :boxer       [@ basetype'boxer]
              :maybe       basetype))))

