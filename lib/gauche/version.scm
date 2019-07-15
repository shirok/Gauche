;;;
;;; version.scm - deal with version numbers
;;;
;;;   Copyright (c) 2000-2019  Shiro Kawai  <shiro@acm.org>
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

;;; Defines a few procedures that deal with popular version
;;; numbers, such as 1.2.3.
;;;
;;; <version>    := <principal-release>
;;;              |  <version> <post-subrelease>
;;;              |  <version> <pre-subrelease>
;;; <principal-release> := <relnum>
;;; <post-subrelease>   := [.-] <relnum>
;;; <pre-subrelease>    := _ <relnum>
;;; <relnum>       := [0-9A-Za-z]+
;;;
;;; Typically <relnum> is composed by numeric part and extension part.
;;; For example, "23a" is composed by an integer 23 and extension "a".
;;; If relnum is entirely composed by an extension, we assume its
;;; numeric part is -1.
;;;
;;; Relnum ordering rule:
;;;   1. If relnum A and relnum B have different numeric part, we ignore the
;;;      extension and order them numerically, e.g.  "3b" < "4a"
;;;   2. If relnum A and relnum B have the same numeric part, we compare
;;;      extension by alphabetically, e.g.  "4c" < "4d" and "5" < "5a"
;;;
;;; Version ordering rule
;;;   1. Decompose each version into a list of <principal-release> and
;;;      subsequence subrelease components.   We call each element of
;;;      the list "release components".
;;;   2. If the first release component of both lists are the same,
;;;      remove it from both.  Repeat this until the head of the lists
;;;      differ.
;;;   3. Now we have the following cases.
;;;     (1) Both lists are empty: versions are the same.
;;;     (2) One list (A) is empty and the other list (B) has post-subrelease
;;;         at head: A is prior to B
;;;     (3) One list (A) is empty and the other list (B) has pre-subrelease
;;;         at head: B is prior to A
;;;     (4) List A's head is post-subrelease and list B's head is
;;;         pre-subrelease: B is prior to A
;;;     (5) Both lists have post-subrelease or pre-subrelease at head:
;;;         compare their relnums.
;;;
;;; Examples:
;;;    "1" < "1.0" < "1.1" < "1.1.1" < "1.1.2" < "1.2" < "1.11"
;;;    "1.2.3" < "1.2.3-1" < "1.2.4"
;;;    "1.2.3" < "1.2.3a" < "1.2.3b"
;;;    "1.2_rc0" < "1.2_rc1" < "1.2" < "1.2-pl1" < "1.2-pl2"
;;;    "1.1-patch112" < "1.2_alpha"

(define-module gauche.version
  (use srfi-11)                         ;let-values
  (use srfi-13)
  (use util.match)
  (export relnum-compare
          version-compare
          version=? version<? version<=? version>? version>=?
          valid-version-spec? version-satisfy?)
  )
(select-module gauche.version)

(define (relnum-decompose vn)
  (rxmatch-if (rxmatch #/^(\d*)(\w*)$/ vn) (#f num suffix)
    (values (if (string=? num "") -1 (x->integer num)) suffix)
    (errorf "unparsable release number: ~s" vn)))

(define (relnum-compare a b)
  (let-values (((an ax) (relnum-decompose a))
               ((bn bx) (relnum-decompose b)))
    (cond ((= an bn)
           (cond ((string=? ax bx) 0)
                 ((string<? ax bx) -1)
                 (else 1)))
          ((< an bn) -1)
          (else 1))))

(define (next-component ver)
  (cond ((rxmatch #/[._-]/ ver)
         => (^m (values (rxmatch-before m)
			(if (string=? (rxmatch-substring m) "_")
			    'pre 'post)
			(rxmatch-after m))))
        (else (values ver #f #f))))

(define (version-compare va vb)
  (define (sep-cmp a-sep a-ver b-sep b-ver)
    (cond
     ((not a-sep)
      (cond ((eq? b-sep 'post) -1)
            ((eq? b-sep 'pre) 1)
            (else 0)))
     ((eq? a-sep 'post)
      (cond ((eq? b-sep 'post) (ver-cmp a-ver b-ver))
            (else 1)))
     (else
      (cond ((eq? b-sep 'pre)  (ver-cmp a-ver b-ver))
            (else -1)))))
  (define (ver-cmp a-ver b-ver)
    (let*-values (((a-rel a-sep a-next) (next-component a-ver))
                  ((b-rel b-sep b-next) (next-component b-ver))
                  ((c) (relnum-compare a-rel b-rel)))
      (if (zero? c)
          (sep-cmp a-sep a-next b-sep b-next)
          c)))
  (ver-cmp va vb))

(define (version=? a b)  (zero? (version-compare a b)))
(define (version<? a b)  (<  (version-compare a b) 0))
(define (version<=? a b) (<= (version-compare a b) 0))
(define (version>? a b)  (>  (version-compare a b) 0))
(define (version>=? a b) (>= (version-compare a b) 0))

;; Version condition is an S expression that can represent compound
;; condition about the version.  We use it for gauche.package versioning,
;; but we put it here so that the same logic can be reused by other
;; applications.
;;
;;   VERSION-SPEC can be:
;;      VERSION
;;      (OP VERSION)           ; OP : = < <= > >=
;;      (and VERSION-SPEC ...)
;;      (or VERSION-SPEC ...)
;;      (not VERSION-SPEC)

;; syntax checker
(define (valid-version-spec? spec)
  (match spec
    [(? string?) #t]  ; TODO: Check version string syntax
    [((or '= '< '<= '> '>=) (? string?)) #t]
    [('not v) (valid-version-spec? v)]
    [((or 'and 'or) v ...) (every valid-version-spec? v)]
    [else #f]))

;; check if given version satisfies spec
(define (version-satisfy? spec version)
  (match spec
    [(? string?) (version=? spec version)]
    [('= v)      (version=? version v)]
    [('< v)      (version<? version v)]
    [('<= v)     (version<=? version v)]
    [('> v)      (version>? version v)]
    [('>= v)     (version>=? version v)]
    [('and req ...) (every (cut version-satisfy? <> version) req)]
    [('or req ...)  (any (cut version-satisfy? <> version) req)]
    [('not req)  (not (version-satisfy? req version))]))
