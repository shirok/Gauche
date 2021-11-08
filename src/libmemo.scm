;;;
;;; libmemo.scm - interface for memoization table
;;;
;;;   Copyright (c) 2021  Shiro Kawai  <shiro@acm.org>
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
 (declcode (.include "gauche/priv/memoP.h"))

 (declare-stub-type <memo-table> "ScmMemoTable*")
 )

;;
;; The API is provisional
;;

(define-cproc make-memo-table (capacity::<ulong>
                               num-keys::<int>
                               :key (weak #f)
                                    (fixed #f))
  (let* ([flags::u_long 0])
    (unless (SCM_FALSEP weak) (logior= flags SCM_MEMO_TABLE_WEAK))
    (unless (SCM_FALSEP fixed) (logior= flags SCM_MEMO_TABLE_FIXED))
    (return (Scm_MakeMemoTable capacity num-keys flags))))

;; Instead of the standard (*-get obj key :optional default) -> <top>
;; signature, this returns two values: The value or #<undef>, and
;; a boolean that indicates a hit.
;; Since mishit is in ordinary operation in memoization table, it is a
;; waste to provide and check default value.  The '2' suffis is to
;;  distinguish from the standard protocol.

(define-cproc memo-table-get2 (tab::<memo-table> keys) ::(<top> <boolean>)
  ;; For now, we only support vector keys; later we'll support lists.
  (unless (SCM_VECTORP keys)
    (SCM_TYPE_ERROR keys "vector"))
  (let* ([v (Scm_MemoTableGetv tab (SCM_VECTOR_ELEMENTS keys)
                               (SCM_VECTOR_SIZE keys))])
    (if (SCM_UNBOUNDP v)
      (return SCM_UNDEFINED FALSE)
      (return v TRUE))))

(define-cproc memo-table-put! (tab::<memo-table> keys value)
  ;; For now, we only support vector keys; later we'll support lists.
  (unless (SCM_VECTORP keys)
    (SCM_TYPE_ERROR keys "vector"))
  (return (Scm_MemoTablePutv tab (SCM_VECTOR_ELEMENTS keys)
                             (SCM_VECTOR_SIZE keys) value)))

(define-cproc memo-table-dump (tab::<memo-table>
                               :optional (port::<port> (current-output-port)))
  ::<void>
  (Scm__MemoTableDump tab port))
