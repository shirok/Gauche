;;;
;;; SRFI-224 - Integer Mappings
;;;
;;;   Copyright (c) 2021-2022  Shiro Kawai  <shiro@acm.org>
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

(define-module srfi.224
  (use data.sparse)
  (use util.match)
  (export
   ;; Constructors
   fxmapping fxmapping-unfold fxmapping-accumulate
   alist->fxmapping alist->fxmapping/combinator

   ;; Predicates
   fxmapping? fxmapping-contains? fxmapping-empty? fxmapping-disjoint?

   ;; Accessors
   fxmapping-min fxmapping-max fxmapping-ref fxmapping-ref/default

   ;; Updaters
   fxmapping-adjoin fxmapping-adjoin/combinator fxmapping-set
   fxmapping-adjust fxmapping-delete fxmapping-delete-all
   fxmapping-alter fxmapping-update
   fxmapping-delete-min fxmapping-delete-max
   fxmapping-update-min fxmapping-update-max
   fxmapping-pop-min fxmapping-pop-max

   ;; The whole fxmapping
   fxmapping-size fxmapping-find fxmapping-count
   fxmapping-any? fxmapping-every?

   ;; Traversal
   fxmapping-fold fxmapping-fold-right fxmapping-map fxmapping-map->list
   fxmapping-for-each fxmapping-relation-map

   ;; Filter
   fxmapping-filter fxmapping-remove fxmapping-partition

   ;; Copying and conversion
   fxmapping->alist fxmapping->decreasing-alist
   fxmapping-keys fxmapping-values
   fxmapping->generator fxmapping->decreasing-generator

   ;; Comparison
   fxmapping=? fxmapping<? fxmapping>? fxmapping<=? fxmapping>=?

   ;; Set theory operations
   fxmapping-union fxmapping-intersection fxmapping-difference fxmapping-xor
   fxmapping-union/combinator fxmapping-intersection/combinator

   ;; Submappings
   fxmapping-open-interval fxmapping-closed-interval
   fxmapping-open-closed-interval fxmapping-closed-open-interval
   fxsubmapping= fxsubmapping< fxsubmapping<= fxsubmapping>= fxsubmapping>
   fxmapping-split
   ))
(select-module srfi.224)
