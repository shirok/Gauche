;;;
;;; srfi-196 - Range objects
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

(define-module srfi-196
  (use gauche.sequence)
  (export
   ;; Constructors
   range numeric-range iota-range vector-range string-range
   range-append

   ;; Predicates
   range? range=?

   ;; Accessors
   range-length range-ref range-first range-last

   ;; Iteration
   range-split-at subrange range-segment
   range-take range-take-right
   range-drop range-drop-right
   range-count range-any
   range-every range-map range-map->list range-map->vector
   range-for-each range-filter-map range-filter-map->list
   range-filter range-filter->list range-remove range-remove->list
   range-fold range-fold-right

   ;; Searching
   range-index range-index-right
   range-take-while range-take-while-right
   range-drop-while range-drop-while-right

   ;; Conversion
   range->list range->vector range->string
   vector->range range->generator))
(select-module srfi-196)

;; The classes <range> and <flat-range> are private.  Users must use
;; srfi-196 API.

;; In Gauche, a range is a sequence whose element may be computed from
;; the index.
(define-class <range> (<sequence>)
  ((length :init-keyword :length)
   (ref :init-keyword :indexer)))   ; obj, integer -> element

(define-method write-object ((r <range>) port)
  (format port "#<range (~d)>" (~ r'length)))

;; special case, where indexer is simply a indexed reference from a sequence
(define-class <flat-range> (<range>)
  ((indexer :init-value (^[o i]
                          (assume (exact-integer? i))
                          (assume (< -1 i (~ o'length)))
                          (let1 off (+ (~ o'offset) i)
                            (ref (~ o'storage) off))))
   (storage :init-keyword :storage)
   (offset :init-keyword :offset :init-value 0)))

;;;
;;; Constructors
;;;

(define (range length indexer)
  (assume (and (exact-integer? length) (>= length 0)))
  (make <range> :length length :ref (^[_ i] (indexer i))))

(define (numeric-range start end :optional (step 1))
  (make <range>
    :length (floor->exact (- end start) step)
    :ref (^[_ i] (+ start (* i step)))))

(define (iota-range length :optional (start 0) (step 1))
  (make <range>
    :length length
    :ref (^[_ i] (+ start (* i step)))))

;; optional arguments are Gauche-specific
(define (vector-range vec :optional (start 0) (end (vector-length vec)))
  (make <flat-range>
    :length (- end start)
    :storage vec
    :offset start))

;; optional arguments are Gauche-specific
(define (string-range str :optional (start 0) (end (string-length vec)))
  (make <flat-range>
    :length (- end start)
    :storage vec
    :offset start))
