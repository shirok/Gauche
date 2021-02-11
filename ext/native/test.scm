
(use gauche.test)
(use gauche.config)
(use gauche.ffitest)

(cond-expand
 [gauche.windows (exit 0)]
 [else
  (unless (#/^x86_64-/ (gauche-config "--arch"))
    (exit 0))])

(test-start "ffitest")

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

  (test-foreign-call dlo "_fo_o_cb" '(z . z) '((o z)) 'o)
  (test-foreign-call dlo "_foo_o_cb" '(d c b a)
                     `((o ,reverse) (o (a b c d))) 'o)
  (test-foreign-call dlo "_foo_o_cb"
                     (test-error <error> "list required, but got zzz")
                     `((o ,reverse) (o zzz)) 'o)

  (test-foreign-call dlo "_fd_o" 101.0  '((d 100.0)) 'o)
  (test-foreign-call dlo "_fid_o" 99.0  '((i 100) (d 1.0)) 'o)
  (test-foreign-call dlo "_fdi_o" 99.0  '((d 100.0) (i 1)) 'o)
  (test-foreign-call dlo "_fiiiiii_d" 10.5
                     '((i 1) (i 2) (i 3) (i 4) (i 5) (i 6)) 'd)
  )

(test-end)




