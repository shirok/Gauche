;;;
;;; SRFI-126 - R6RS hashtables
;;;
;;;   Copyright (c) 2023-2024  Shiro Kawai  <shiro@acm.org>
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

;; This srfi can be used to ease porting R6RS code.
;; The hashtable instance is Gauches' built-in.

(define-module srfi.126
  (export)
  )
(select-module srfi.126)

;; We ignore capacity argument for now, and not support weakness.

(define (make-eq-hashtable :optional (capacity #f) (weakness #f))
  (assume-type capacity (<?> <integer>))
  (unless (eqv? weakness #f)
    (error "Weak hashtable is not supported yet in srfi.216."))
  (make-hash-table eq-comparator))

(define (make-eqv-hashtable :optional (capacity #f) (weakness #f))
  (assume-type capacity (<?> <integer>))
  (unless (eqv? weakness #f)
    (error "Weak hashtable is not supported yet in srfi.216."))
  (make-hash-table eqv-comparator))

(define (make-hashtable hash equiv :optional (capacity #f) (weakness #f))
  (assume-type capacity (<?> <integer>))
  (unless (eqv? weakness #f)
    (error "Weak hashtable is not supported yet in srfi.216."))
  (cond [(and (not hash) (eqv? equiv eq?))
         (make-hash-table eq-comparator)]
        [(and (not hash) (eqv? equiv eqv?))
         (make-hash-table eqv-comparator)]
        [(or (not hash)
             (not (or (procedure? hash)
                      (and (pair? hash)
                           (procedure? (car hash))
                           (procedure? (cdr hash))))))
         (error "Hash function is required for custom equivalence procedure. \
                 It must be a procedure, or a pair of procedures, but got:"
                hash)]
        [else
         (let1 h (if (pair? hash)
                   (car hash)           ;we only use one hash function
                   hash)
           (make-hash-table (make-comparator #t equiv #f h)))]))
