;;;
;;; version.scm - deal with version numbers
;;;
;;;  Copyright(C) 2002 by Shiro Kawai (shiro@acm.org)
;;;
;;;  Permission to use, copy, modify, distribute this software and
;;;  accompanying documentation for any purpose is hereby granted,
;;;  provided that existing copyright notices are retained in all
;;;  copies and that this notice is included verbatim in all
;;;  distributions.
;;;  This software is provided as is, without express or implied
;;;  warranty.  In no circumstances the author(s) shall be liable
;;;  for any damages arising out of the use of this software.
;;;
;;;  $Id: version.scm,v 1.1 2002-02-14 07:38:26 shirok Exp $
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
  (export relnum-compare
          version-compare
          version=? version<? version<=? version>? version>=?)
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
         => (lambda (m) (values (rxmatch-before m)
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

(provide "gauche/version")

