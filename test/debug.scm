;; Test to access debug information.
;;
;; NB: The format of information these test acquires from the runing VM
;; is not fixed yet; they may change at any time as VM evolves.

(use gauche.test)
(use srfi-1)
(use util.match)

(test-start "debug features")

(test-section "stack trace")

;; Test that stack trace isn't corrupted by the change of stack
;; boundary handling in 0.9.1

;; These functions shouldn't be in let, since doing so makes
;; them optimized away.

(define (ra x) (ra (rb x)) #f)
(define (rb x) (if (rc x) (rb x) (rb x)))
(define (rc x) ((with-module gauche.internal %apply-rec) rd x) #f) ;cross C stack boundary
(define (rd x) (list 3 (re x)))
(define (re x) (* 7 ((with-module gauche.internal %apply-rec) rf x) 9))
(define (rf x) (cons 8 (rz x)))
(define (rz x) (x (vm-get-stack-trace-lite)))

(test* "trace crossing C stack boundary"
       '(rz rf re rd rc rb ra)
       (filter-map (match-lambda
                     [(('with-module 'gauche.internal '%apply-rec) fn 'x) fn]
                     [((? symbol? fn) 'x) fn]
                     [_ #f])
                   (call/cc (^x (ra x) #f))))

(test-end)
