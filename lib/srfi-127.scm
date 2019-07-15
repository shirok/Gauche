;;;
;;; srfi-127 - lazy sequences
;;;
;;;   Copyright (c) 2016-2019  Shiro Kawai  <shiro@acm.org>
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

;; This is a compability facade of Gauche's native lseq.
;; Srfi-127's lseq is conceptually a list terminated by a generator:
;;
;;  Lseq :: () | (x . Generator) | (x . Lseq)
;;
;; However, generally there's no guarantee that how many elements are
;; realized in a given lseq, so no portable code should count on
;; (cdr lseq) => <generator>.
;;
;; One deviation of srfi-127 is the constructor generator->lseq: It
;; explicitly specifies to return (x . Generator) of the given generator
;; yields non-EOF object.  We return Gauche's built-in lseq, which is
;; operationally equivalent to srfi-127's lseq but you cannot get
;; the generator itself in the cdr.

(define-module srfi-127
  (use srfi-1)
  (use gauche.lazy)
  (use gauche.generator)
  (export generator->lseq  ; built-in
          lseq?        lseq=?
          lseq-car     lseq-cdr
          lseq-first   lseq-rest lseq-ref
          lseq-take    lseq-drop   
          lseq-realize lseq->generator
          lseq-length  lseq-append  lseq-zip
          lseq-map     lseq-for-each
          lseq-filter  lseq-remove
          lseq-find    lseq-find-tail 
          lseq-any     lseq-every
          lseq-index
          lseq-take-while lseq-drop-while
          lseq-member  lseq-memq     lseq-memv))
(select-module srfi-127)

(define (lseq? x) (or (null? x) (pair? x)))

(define (lseq=? elt=? lseq1 lseq2)
  (list= elt=? lseq1 lseq2))

(define lseq-car car)
(define lseq-first car)
(define lseq-cdr cdr)
(define lseq-rest cdr)
(define lseq-ref list-ref)

(define (lseq-take lseq k)
  (if (and (null? lseq) (= k 0))
    '()                                 ;shortcut
    (generator->lseq
     (^[] (if (<= k 0)
            (eof-object)
            (rlet1 e (car lseq)
              (dec! k)
              (set! lseq (cdr lseq))))))))
(define lseq-drop drop)
(define (lseq-realize lseq) (and (length lseq) lseq))

(define lseq->generator list->generator)

(define lseq-length length)
(define lseq-append lappend)

(define (lseq-zip lseq . lseqs) (apply lmap list lseq lseqs))
(define lseq-map lmap)
(define lseq-for-each for-each)

(define lseq-filter lfilter)
(define (lseq-remove pred lseq) (lfilter (complement pred) lseq))
(define lseq-find find)
(define lseq-find-tail find-tail)
(define lseq-take-while ltake-while)
(define lseq-drop-while drop-while)

(define lseq-any any)
(define lseq-every every)
(define lseq-index list-index)
(define lseq-member member)
(define lseq-memq memq)
(define lseq-memv memv)




