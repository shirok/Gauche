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
  (let1 argv `(,cc "-I." "-DSCM_ENABLE_ALL_ARITH_ASMS" ,@(string-split cflags #[\s])
                   ,@extra-defs
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

(define (pad-hex n width)
  (let1 s (number->string n 16)
    (if (>= (string-length s) width)
      s
      (string-append (make-string (- width (string-length s)) #\0) s))))

(define (show-comparison r1 r2)
  ;; r1/r2 are alists; put whichever is portable on the left.
  (receive (a b)
      (if (equal? (aref 'variant r1) "portable")
        (values r1 r2)
        (values r2 r1))
    (let* ([va (aref 'variant a)]
           [vb (aref 'variant b)]
           [ops-a  (aref 'ns-per-op a)]
           [ops-b  (aref 'ns-per-op b)]
           [chks-a (aref 'checksum a)]
           [chks-b (aref 'checksum b)]
           [names  (map car ops-a)])
      (format #t "  iter=~d  repeat=~d  (best-of-repeat, ns per op)~%~%"
              (aref 'iter a) (aref 'repeat a))
      (format #t "  ~8a  ~14@a  ~14@a  ~10@a  ~8@a~%"
              "op" va vb "speedup" "check")
      (format #t "  --------  --------------  --------------  ----------  --------~%")
      (let1 mismatches '()
        (for-each
         (^[op]
           (let ([na (cdr (assq op ops-a))]
                 [nb (cdr (assq op ops-b))]
                 [ca (cdr (assq op chks-a))]
                 [cb (cdr (assq op chks-b))])
             (let1 ok? (equal? ca cb)
               (unless ok? (push! mismatches (list op ca cb)))
               (format #t "  ~8a  ~14,3f  ~14,3f  ~9,2fx  ~8@a~%"
                       op na nb (if (positive? nb) (/ na nb) 0)
                       (if ok? "OK" "DIFFER")))))
         names)
        (unless (null? mismatches)
          (newline)
          (format #t "  WARNING: ~d macro(s) produced different results between~%"
                  (length mismatches))
          (format #t "  ~a and ~a — the native asm is buggy:~%" va vb)
          (for-each
           (^[m]
             (format #t "    ~8a  ~a=#x~a  ~a=#x~a~%"
                     (car m)
                     va (pad-hex (cadr m) 16)
                     vb (pad-hex (caddr m) 16)))
           (reverse mismatches)))))))

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
