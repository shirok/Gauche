;;;
;;; libomega.scm - the stuff to be run after other parts are initialized
;;;
;;;   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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

;; Register built-in modules as provided, so that (use <built-in-modue>) won't
;; complain.
;; TODO: #<module util.match> isn't built-in, but the module is created during
;; initializing compile.scm, for it has reference to util.match#match:error.
;; Eventually it should be addressed by making util.match built-in; but for now,
;; we use a kludge to exclude util.match explicitly.
(dolist [m (all-modules)]
  (let1 n (module-name m)
    (unless (eq? n 'util.match)
      (provide (module-name->path n)))))

;; A trick to allow slot-ref to be used for compound condition.
;; This is better to be in libexc.scm, but we need to evaluate this
;; after the object system is fully bootstrapped.
(define-method slot-missing ((class <condition-meta>)
                             (cc <compound-condition>)
                             slot)
  (let loop ((members (slot-ref cc '%conditions)))
    (cond [(null? members) (next-method)]
          [(slot-exists? (car members) slot) (slot-ref (car members) slot)]
          [else (loop (cdr members))])))

;; Printing auxiliary error information.  This also is here instead
;; of libexc.scm because of the initialization order.
(define-method report-mixin-condition ((c <mixin-condition>) port) #f)

(define-method report-mixin-condition ((c <load-condition-mixin>) port)
  (and-let* ([p (~ c'port)]
             [ (port? p) ]
             [name (port-name p)]
             [line (port-current-line p)])
    (format port "    While loading ~s at line ~d\n" name line)))

(define-method report-mixin-condition ((c <compile-error-mixin>) port)
  (let* ([expr (~ c'expr)]
         [src-info (find (^[si] (and (car si) (cadr si)))
                         ((with-module gauche.internal %source-info) expr))])
    (if src-info
      (format port "    While compiling ~s at line ~d: ~,,,,105:s\n"
              (car src-info) (cadr src-info) expr)
      (format port "    While compiling: ~,,,,90:s\n" expr))))

;; Built-in comparators.  These are here instead of libcmp.scm, for
;; hash functions need to be defined before this.
(define eq-comparator
  (make-comparator #t eq? eq-compare eq-hash 'eq-comparator))
(define eqv-comparator
  (make-comparator #t eqv? #f eq-hash 'eqv-comparator))
(define equal-comparator
  (make-comparator #t equal? #f hash 'equal-comparator))
(define string-comparator
  (make-comparator string? string=? compare
                   (with-module gauche.internal %hash-string)
                   'string-comparator))

;; comparators can be compared by equal? (Gauche extension)
(define-method object-equal? ((x <comparator>) (y <comparator>))
  (and (eqv? (~ x'type-test) (~ y'type-test))
       (or (and ((with-module gauche.internal comparator-equality-use-comparison?) x)
                ((with-module gauche.internal comparator-equality-use-comparison?) y))
           (eqv? (~ x'equality-test) (~ y'equality-test)))
       (or (and (not (comparator-comparison-procedure? x))
                (not (comparator-comparison-procedure? y)))
           (eqv? (~ x'comparison) (~ y'comparison)))
       (or (and (not (comparator-hash-function? x))
                (not (comparator-hash-function? y)))
           (eqv? (~ x'hash) (~ y'hash)))
       (equal? (slot-ref x 'name) (slot-ref y 'name))))

;;; TEMPORARY for 0.9.x series
;;; Remove this after 1.0 release!!!
;;;
;;; Add 0.9 directories, which doesn't follow the new directory structure,
;;; to support the extension modules that are installed with 0.9.
;;; A few extension packages installs files into the "sys" directory hierarchy
;;; instead of "site" one.  It is banned after 0.9.1, but we need to add 0.9's
;;; sys directories for the backward compatibility.
;;; NB: we set! to *load-path* etc here, which is an emergency workaround.
;;; Ordinary programs should never modify *load-path*/*dynamic-load-path*
;;; directly.
(select-module gauche)
(let* ([archdir (gauche-architecture-directory)]
       [m (rxmatch #/gauche-0\.9[\/\\]0\.9[^\/\\]*[\/\\]/ archdir)]
       [oldsitedir (string-append (rxmatch-before m)
                                  "gauche/site/0.9/"
                                  (rxmatch-after m))]
       [oldarchdir (string-append (rxmatch-before m)
                                  "gauche/0.9/"
                                  (rxmatch-after m))])
  (set! *dynamic-load-path*
        (append *dynamic-load-path* (list oldsitedir oldarchdir))))
(let* ([libdir (gauche-library-directory)]
       [m (rxmatch #/gauche-0\.9[\/\\]0\.9[^\/\\]*[\/\\]/ libdir)]
       [oldsitedir (string-append (rxmatch-before m)
                                  "gauche/site/"
                                  (rxmatch-after m))]
       [oldlibdir  (string-append (rxmatch-before m)
                                  "gauche/0.9/"
                                  (rxmatch-after m))])
  (set! *load-path* (append *load-path* (list oldsitedir oldlibdir))))
