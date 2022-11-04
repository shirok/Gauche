;;
;; Shift/reset in terms of full continuation.
;;
;; Based on:
;; Martin Gasbichler and Michael Sperber, Final Shift for Call/cc:
;; Direct implementation of Shift and Reset, ICFP '02, 2002.
;; http://citeseer.ist.psu.edu/gasbichler02final.html
;;
;; This is for the reference.  Gauche has built-in support of shift/reset,
;; and its behavior and this should match.  Sometimes the internal change
;; introduces subtle bugs, and this module helps to see how it should work.

(define-module gauche.partcont-meta
  (use gauche.parameter)
  (export reset shift call/pc))
(select-module gauche.partcont-meta)

(define meta-continuation
  (make-parameter
   (^ _ (error "stale meta-continuation invoked"))))

(define-syntax reset
  (syntax-rules ()
    [(_ expr ...) (%reset (^[] expr ...))]))

(define (%abort thunk)
  (receive v (thunk)
    (apply (meta-continuation) v)))

(define (%reset thunk)
  (let1 save (meta-continuation)
    (let/cc k
      (meta-continuation (^ vals
                           (meta-continuation save)
                           (apply k vals)))
      (%abort thunk))))

(define-syntax shift
  (syntax-rules ()
    [(_ var expr ...) (call/pc (^[var] expr ...))]))

(define (call/pc proc)
  (let/cc k
    (%abort (^[] (proc (^ vals (reset (apply k vals))))))))
