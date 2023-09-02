;; Test to access debug information.
;;
;; NB: The format of information these test acquires from the running VM
;; is not fixed yet; they may change at any time as VM evolves.

(use gauche.test)
(use scheme.list)
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


(test-section "source location track")

;; source info of quoted literals
;; https://github.com/shirok/Gauche/issues/847
(test* "source info of quoted literals" #t
       (let1 s (debug-source-info '(a b c))
         (and (pair? s)
              (string? (car s))
              (#/debug\.scm$/ (car s))
              (integer? (cadr s)))))

(test-section "packing debug-info")

(use gauche.vm.debug-info)
(test-module 'gauche.vm.debug-info)

(use util.isomorph)
(use gauche.cgen.unit)

(define (test-debug-info data :optional (msg #f))
  (test* (or msg (write-to-string data write-shared))
         data
         (let* ([unit (make <cgen-unit>)]
                [bv (encode-debug-info unit data)])
           (decode-debug-info bv (get-debug-info-const-vector unit)))
         isomorphic?))

(test-debug-info 1)
(test-debug-info 'abc)
(test-debug-info '(a b () c))
(test-debug-info '#0=(a b . #0#))
(test-debug-info '#0=(a b #0#))
(test-debug-info '(a #1=(b) #1#))
(test-debug-info '(a #1=(b) (#1#)))
(test-debug-info '(a #1=(b) . #1#))
(test-debug-info '#0=(1 . #1=(2 #2=(3) #0# #1# . #2#)))

(let1 xs (make-list 234 0)
  (test-debug-info `(12345 123456789 123456789012345 ,@xs #0=(1234567) . #0#)
                   "big data"))

(test-end)
