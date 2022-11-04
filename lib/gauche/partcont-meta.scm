;;
;; Shift/reset in terms of full continuation.
;;
;; Based on:
;; Martin Gasbichler and Michael Sperber, Final Shift for Call/cc:
;; Direct implementation of Shift and Reset, ICFP '02, 2002.
;; http://citeseer.ist.psu.edu/gasbichler02final.html
;;
;; This is for the reference.  Gauche has built-in support of shift/reset,
;; and its behavior and this should match, except that this version may
;; execute before/after thunks redundantly.
;  Sometimes the internal change introduces subtle bugs, and this module
;; helps to see how it should work.
;;
;; Modifications from Gasbichler's paper:
;;  - Using thread-local instead of a simple global variable for
;;    *meta-continuation*
;;  - *meta-continuation* takes thunk instead of values.  It is because the body
;;    of shift must be evaluated in the dynamic environment of the corresponding
;;    reset; the original implementation evaluates it in the dynamic
;;    environment of shift, which is wrong.
;;    The thunk is later evaluated in %reset.
;;  - Strictly speaking, nestings of reset must be equal or greater than
;;    the nesting of shift.  The original implementation throws an error
;;    if shifts are nested deeper.  In Gauche we have system's implicit
;;    resets, so it is not necessarily the case.  This implementation
;;    just evaluates the passed thunk if stale meta-continuation is invoked.

(define-module gauche.partcont-meta
  (use gauche.threads)
  (export reset shift call/pc))
(select-module gauche.partcont-meta)

(define *meta-continuation*
  (make-thread-local
   (^[thunk] (thunk))
   'meta-continuation))

(define-syntax reset
  (syntax-rules ()
    [(_ expr ...) (%reset (^[] expr ...))]))

(define (%abort thunk)
  ((tlref *meta-continuation*) thunk))

(define (%reset thunk)
  (let1 save (tlref *meta-continuation*)
    ((let/cc k
       (tlset! *meta-continuation*
               (^[thunk]
                 (tlset! *meta-continuation* save)
                 (k thunk)))
       (receive r (thunk)
         (%abort (^[] (apply values r))))))))

(define-syntax shift
  (syntax-rules ()
    [(_ var expr ...) (call/pc (^[var] expr ...))]))

(define (call/pc proc)
  (let/cc k
    (%abort (^[] (proc (^ vals (reset (apply k vals))))))))
