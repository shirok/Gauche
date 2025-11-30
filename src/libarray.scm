;;;
;;; libarray.scm - array basics
;;;
;;;   Copyright (c) 2025  Shiro Kawai  <shiro@acm.org>
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

;; Array operations are implemented in gauche.array (ext/uvector/array.scm
;; and ext/uvector/matrix.scm).  We want to dispatch array printers
;; from pprint, however, and want to delay loading gauche.array until
;; we see an array.  To do so, we need array core type to be defined
;; beforehand.

;; It would be plausible to provide basic array accessors (array-ref and
;; array-set!) in the core as well, for that could provide much better
;; performance.  Consider the current implementation transitional

;; NB: Wiring metaclass in C is cumbersome, so we define <array-base>
;; as a usual class.  The concrete classes (<array>, <u8array>, etc.)
;; must specify :metaclass <array-base>.

(select-module gauche.internal)

(inline-stub
 ;;
 ;; Array-base
 ;;
 (define-ctype ScmArrayBase
   ::(.struct ScmArrayBaseRec
              (SCM_INSTANCE_HEADER::||
               start-vector
               end-vector
               mapper
               getter
               setter
               backing-storage)))

 (.define SCM_ARRAY_BASE (obj) (cast ScmArrayBase* obj))

 (define-cclass <array-base> :base
   "ScmArrayBase*" "Scm_ArrayBaseClass"
   (c "SCM_CLASS_OBJECT_CPL")
   ((start-vector)
    (end-vector)
    (mapper)
    (getter)
    (setter)
    (backing-storage))
   (allocator (let* ([z::ScmArrayBase* (SCM_NEW_INSTANCE ScmArrayBase klass)])
                (set! (-> z start-vector) SCM_UNDEFINED)
                (set! (-> z end-vector) SCM_UNDEFINED)
                (set! (-> z mapper) SCM_UNDEFINED)
                (set! (-> z getter) SCM_UNDEFINED)
                (set! (-> z setter) SCM_UNDEFINED)
                (set! (-> z backing-storage) SCM_UNDEFINED)
                (return (SCM_OBJ z)))))
 )
