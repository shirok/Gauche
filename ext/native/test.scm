(use gauche.test)
(use gauche.config)
(use gauche.ffitest)
(use file.util)

(cond-expand
 [gauche.os.windows (exit 0)]
 [else
  (unless (#/^x86_64-/ (gauche-config "--arch"))
    (exit 0))])

(test-start "ffitest")

;;==========================================================================
(test-section "raw call")

(define (foreign-call dlo name args rettype)
  ((with-module gauche.internal call-amd64)
   (dlobj-get-entry-address dlo name)
   args rettype))

(define (test-foreign-call dlo name expected args rettype)
  (test* #"call ~name" expected
         (foreign-call dlo name args rettype)))

(let ((dlo (dynamic-load "gauche--ffitest" :init-function #f)))
  (test* "open dlo" #t (is-a? dlo <dlobj>))
  (let ((dle (dlobj-get-entry-address dlo "_f_v")))
    (test* "get dlptr" #t (is-a? dle <dlptr>))
    (test* "call f_o" (list (undefined) "it works")
           (let* ((r #f)
                  (s (with-output-to-string
                       (^[]
                         (set! r ((with-module gauche.internal call-amd64)
                                  dle '() 'v))))))
             (list r s))))

  (test-section "simple, register passing call (integral)")
  (test-foreign-call dlo "_f_o" 'it_works '() 'o)
  (test-foreign-call dlo "_f_i" 42 '() 'i)
  (test-foreign-call dlo "_f_s" "it works" '() 's)

  (test-foreign-call dlo "_fo_o" '(wow . huh) '((o wow)) 'o)
  (test-foreign-call dlo "_fi_o" '(7 . huh) '((i 6)) 'o)
  (test-foreign-call dlo "_fi_o" '(-9 . huh) '((i -10)) 'o)
  (test-foreign-call dlo "_fs_o" 5 '((s "hello")) 'o)
  (test-foreign-call dlo "_fo_i" 3 '((o (a b c))) 'i)
  (test-foreign-call dlo "_fi_i" 121 '((i 11)) 'i)
  (test-foreign-call dlo "_fs_i" 6 '((s "gauche")) 'i)
  (test-foreign-call dlo "_fo_s" "(a b c)" '((o (a b c))) 's)

  (test-foreign-call dlo "_foo_o" '(a . b) '((o a) (o b)) 'o)
  (test-foreign-call dlo "_foi_o" '(a . 1) '((o a) (i 0)) 'o)
  (test-foreign-call dlo "_fis_i" (char->integer #\c) '((i 2) (s "abcde")) 'i)

  (test-foreign-call dlo "_fois_o" '("foo" 100 (cent))
                     '((o (cent)) (i 100) (s "foo")) 'o)
  (test-foreign-call dlo "_foiso_o" '(3+2i "foo" 100 (cent))
                     '((o (cent)) (i 100) (s "foo") (o 3+2i)) 'o)
  (test-foreign-call dlo "_foisoi_o" '(-56789 3+2i "foo" 100 (cent))
                     '((o (cent)) (i 100) (s "foo") (o 3+2i) (i -56789)) 'o)
  (test-foreign-call dlo "_foisois_o" '("" -56789 3+2i "foo" 100 (cent))
                     '((o (cent)) (i 100) (s "foo") (o 3+2i) (i -56789) (s ""))
                     'o)

  (test-section "register passing (flonum)")
  (test-foreign-call dlo "_fd_o" 101.0  '((d 100.0)) 'o)
  (test-foreign-call dlo "_fid_o" 99.0  '((i 100) (d 1.0)) 'o)
  (test-foreign-call dlo "_fdi_o" 99.0  '((d 100.0) (i 1)) 'o)
  (test-foreign-call dlo "_fiiiiii_d" 10.5
                     '((i 1) (i 2) (i 3) (i 4) (i 5) (i 6)) 'd)

  (test-section "calling back to Scheme")
  (test-foreign-call dlo "_fo_o_cb" '(z . z) '((o z)) 'o)
  (test-foreign-call dlo "_foo_o_cb" '(d c b a)
                     `((o ,reverse) (o (a b c d))) 'o)
  (test-foreign-call dlo "_foo_o_cb"
                     (test-error <error> "list required, but got zzz")
                     `((o ,reverse) (o zzz)) 'o)

  (test-section "spill-to-stack case")
  (test-foreign-call dlo "_fooooooo_o" '((a b c d e) (f g))
                     `((o a) (o b) (o c) (o d) (o e) (o f) (o g)) 'o)
  (test-foreign-call dlo "_foooooooo_o" '((a b c d e) (f g h))
                     `((o a) (o b) (o c) (o d) (o e) (o f) (o g) (o h)) 'o)
  (test-foreign-call dlo "_fooooooooi_o" '((a b c d e) (f g h 5))
                     `((o a) (o b) (o c) (o d) (o e) (o f) (o g) (o h) (i 4))
                     'o)
  (test-foreign-call dlo "_fdddddddddd_d"
                     (* (+ 1.0 1.1 1.2 1.3 1.4) (+ 1.5 1.6 1.7 1.8 1.9))
                     `((d 1.0) (d 1.1) (d 1.2) (d 1.3) (d 1.4)
                       (d 1.5) (d 1.6) (d 1.7) (d 1.8) (d 1.9))
                     'd)
  (test-foreign-call dlo "_fiiiiiiddddddddidid_d"
                     (* (+ 1 2 3 4 5 6 7 8)
                        (+ 1.0 1.1 1.2 1.3 1.4
                           1.5 1.6 1.7 1.8 1.9))
                     `((i 1) (i 2) (i 3) (i 4) (i 5) (i 6)
                       (d 1.0) (d 1.1) (d 1.2) (d 1.3)
                       (d 1.4) (d 1.5) (d 1.6) (d 1.7)
                       (i 7) (d 1.8) (i 8) (d 1.9))
                     'd)

  (test-foreign-call dlo "_fooooooooooo_o_cb"
                     '(A B C D E F G H I . J)
                     `((o ,list*) (o A) (o B) (o C) (o D) (o E)
                       (o F) (o G) (o H) (o I) (o J))
                     'o)

  (test-foreign-call dlo "_fio_var_o"
                     '(A A B B C C)
                     '((i 3) (o A) (o B) (o C))
                     'o)
  (test-foreign-call dlo "_fio_var_o"
                     '(A A B B C C D D E E F F G G H H)
                     '((i 8) (o A) (o B) (o C) (o D) (o E) (o F) (o G) (o H))
                     'o)
  (test-foreign-call dlo "_fido_var_o"
                     '(A A B B C C D D 1.0)
                     '((i 4) (d 1.0) (o A) (o B) (o C) (o D))
                     'o)

  (test-section "ensure error frees codepad memory")
  (test* "error and codepad memory management" #t
         (let1 proc (lambda (_) (error "wow"))
           (dotimes [2000]
             (guard (e [else #t])
               (foreign-call dlo "_foo_o_cb" `((o ,proc) (o #f)) 'o)))
           #t))
  )

;;==========================================================================
(test-section "FFI API")

(use gauche.ffi)
(test-module 'gauche.ffi)

(define *ffi* #f)
(test* "load-foreign" '(#t (f_i))
       (let ((dir (sys-dirname (current-load-path))))
         (set! *ffi* (load-foreign (build-path dir "test/f.h")
                                   (build-path dir "test/f")))
         (list (is-a? *ffi* <foreign-library>)
               (hash-table-keys (~ *ffi* 'entries)))))

(test* "call-foreign" 42
       (call-foreign *ffi* 'f_i))

(test-end)
