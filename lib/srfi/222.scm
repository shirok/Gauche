;;;
;;; SRFI-222 - Compound objects
;;;
;;;   Copyright (c) 2022  Shiro Kawai  <shiro@acm.org>
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

(define-module srfi.222
  (use gauche.record)
  (use data.queue)
  (export make-compound compound?
          compound-subobjects compound-length compound-ref
          compound-map compound-map->list compound-filter
          compound-predicate compound-access
          ))
(select-module srfi.222)

(define-class <compound> ()
  ((subobjects :init-keyword :subobjects)  ; list of objects
   ))

(define-method write-object ((c <compound>) port)
  (format port "#<compound ~s>" (~ c'subobjects)))

(define-method object-hash ((c <compound>) rec-hash)
  (rec-hash (~ c'subobjects)))

(define-method object-compare ((a <compound>) (b <compound>))
  (compare (~ a'subobjects) (~ b'subobjects)))

(define (%list->compound objs)
  (let1 q (make-queue)
    (dolist [o objs]
      (if (compound? o)
        (apply enqueue! q (~ o'subobjects))
        (enqueue! q o)))
    (make <compound> :subobjects (dequeue-all! q))))

(define (make-compound . objs) (%list->compound objs))

(define (compound? obj) (is-a? obj <compound>))

(define (compound-subobjects obj)
  (if (compound? obj)
    (~ obj'subobjects)
    (list obj)))

(define (compound-length obj)
  (if (compound? obj)
    (length (~ obj'subobjects))
    1))

(define (compound-ref obj k)
  (assume (and (exact-integer? k) (not (negative? k)))
          "Index must be nonnegative exact integer, but got:" k)
  (if (compound? obj)
    (~ obj'subobjects k)
    (if (= k 0) obj (error "Index out of range:" k))))

(define (compound-map mapper obj)
  (%list->compound (compound-map->list mapper obj)))

(define (compound-map->list mapper obj)
  (map mapper (compound-subobjects obj)))

(define (compound-filter pred obj)
  (%list->compound (filter pred (compound-subobjects obj))))

(define (compound-predicate pred obj)
  (or (pred obj)
      (and (compound? obj)
           (boolean (any pred (~ obj'subobjects))))))

(define (compound-access pred acc default obj)
  (cond [(pred obj) (acc obj)]
        [(and (compound? obj)
              (find pred (~ obj'subobjects)))
         => acc]
        [else default]))
