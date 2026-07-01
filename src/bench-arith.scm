#!/usr/bin/env gosh
;;;
;;; bench-arith.scm - build & compare portable vs. native arith.h
;;;
;;; Builds two variants of bench-arith.c (portable and arch-native),
;;; runs each, and prints a side-by-side table.

(use gauche.process)
(use gauche.config)
(use file.util)

(define (script-dir)
  (sys-dirname (sys-realpath (car (command-line)))))

(define (say fmt . args)
  (apply format (current-error-port) fmt args)
  (flush (current-error-port)))

(define (build cc cflags out extra-defs)
  (let1 argv `(,cc "-I." ,@(string-split cflags #[\s]) ,@extra-defs
                   "bench-arith.c" "-o" ,out)
    (say ";; ~a~%" (string-join argv " "))
    (do-process! argv)))

(define (run-bench exe)
  (say ";; running ~a ...~%" exe)
  (call-with-input-process `(,exe)
    (^p (let1 sexp (read p)
          (when (eof-object? sexp)
            (exit 1 (format "no S-expression from ~a" exe)))
          sexp))))

(define (aref key alist)
  (cond [(assq key alist) => cdr]
        [else (errorf "missing key ~s in ~s" key alist)]))

(define (show-comparison r1 r2)
  ;; r1/r2 are alists; put whichever is portable on the left.
  (receive (a b)
      (if (equal? (aref 'variant r1) "portable")
        (values r1 r2)
        (values r2 r1))
    (let* ([va (aref 'variant a)]
           [vb (aref 'variant b)]
           [ops-a (aref 'ns-per-op a)]
           [ops-b (aref 'ns-per-op b)]
           [names (map car ops-a)])
      (format #t "  iter=~d  repeat=~d  (best-of-repeat, ns per op)~%~%"
              (aref 'iter a) (aref 'repeat a))
      (format #t "  ~8a  ~14@a  ~14@a  ~10@a~%" "op" va vb "speedup")
      (format #t "  --------  --------------  --------------  ----------~%")
      (for-each
       (^[op]
         (let ([na (cdr (assq op ops-a))]
               [nb (cdr (assq op ops-b))])
           (format #t "  ~8a  ~14,3f  ~14,3f  ~9,2fx~%"
                   op na nb (if (positive? nb) (/ na nb) 0))))
       names))))

(define (main args)
  (sys-chdir (script-dir))
  (let ([cc     (gauche-config "--cc")]
        [cflags (gauche-config "--default-cflags")])
    (build cc cflags "bench-arith-portable" '("-DBENCH_PORTABLE"))
    (build cc cflags "bench-arith-native"   '())
    (let* ([r-portable (run-bench "./bench-arith-portable")]
           [r-native   (run-bench "./bench-arith-native")])
      (newline)
      (show-comparison r-portable r-native)))
  0)
