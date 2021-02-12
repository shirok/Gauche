;;;
;;; This module isn't the "real" library module.  It is only used
;;; during the test process.
;;;


(define-module gauche.ffitest)
(select-module gauche.ffitest)

;; zero arguments

(define-cfn "f_o" () (return 'it_works))
(define-cfn "f_i" () ::int  (return 42))
(define-cfn "f_s" () ::(const char*)  (return "it works"))
(define-cfn "f_v" () ::void (Scm_Printf SCM_CUROUT "it works"))

;; one argument

(define-cfn "fo_o" (x)
  (return (Scm_Cons x 'huh)))
(define-cfn "fi_o" (i::int)
  (return (Scm_Cons (SCM_MAKE_INT (+ i 1)) 'huh)))
(define-cfn "fs_o" (s::(const char*))
  (return (Scm_MakeIntegerU (strlen s))))
(define-cfn "fo_i" (x) ::ScmSmallInt
  (return (Scm_Length x)))
(define-cfn "fi_i" (i::ScmSmallInt) ::ScmSmallInt
  (return (* i i)))
(define-cfn "fs_i" (s::(const char*)) ::ScmSmallInt
  (return (strlen s)))
(define-cfn "fo_s" (x) ::(const char*)
  (let* ([o (Scm_MakeOutputStringPort TRUE)])
    (Scm_Write x o SCM_WRITE_SIMPLE)
    (return (Scm_GetStringConst (SCM_STRING (Scm_GetOutputString (SCM_PORT o) 0))))
    ))

;; two arguments

(define-cfn "foo_o" (x y)
  (return (Scm_Cons x y)))
(define-cfn "foi_o" (x y::ScmSmallInt)
  (return (Scm_Cons x (SCM_MAKE_INT (+ y 1)))))
(define-cfn "fis_i" (x::ScmSmallInt y::(const char*)) ::ScmSmallInt
  (return (aref y x)))

;; three-six arguments
(define-cfn "fois_o" (a b::ScmSmallInt c::(const char*))
  (return (SCM_LIST3 (SCM_MAKE_STR_COPYING c) (SCM_MAKE_INT b) a)))
(define-cfn "foiso_o" (a b::ScmSmallInt c::(const char*) d)
  (return (SCM_LIST4 d (SCM_MAKE_STR_COPYING c) (SCM_MAKE_INT b) a)))
(define-cfn "foisoi_o" (a b::ScmSmallInt c::(const char*) d e::int)
  (return (SCM_LIST5 (SCM_MAKE_INT e) d (SCM_MAKE_STR_COPYING c)
                     (SCM_MAKE_INT b) a)))
(define-cfn "foisois_o" (a b::ScmSmallInt c::(const char*) d e::int f::(const char*))
  (return (Scm_Cons (SCM_MAKE_STR_COPYING f)
                    (SCM_LIST5 (SCM_MAKE_INT e) d (SCM_MAKE_STR_COPYING c)
                               (SCM_MAKE_INT b) a))))

;; seven or more (spill to stack)
(define-cfn "fooooooo_o" (a b c d e f g)
  (return (SCM_LIST2 (SCM_LIST5 a b c d e)
                     (SCM_LIST2 f g))))
(define-cfn "foooooooo_o" (a b c d e f g h)
  (return (SCM_LIST2 (SCM_LIST5 a b c d e)
                     (SCM_LIST3 f g h))))
(define-cfn "fooooooooi_o" (a b c d e f g h j::int)
  (return (SCM_LIST2 (SCM_LIST5 a b c d e)
                     (SCM_LIST4 f g h (SCM_MAKE_INT (+ j 1))))))

;; calling back to Scheme
(define-cfn "fo_o_cb" (x)
  (let* ([fn (Scm_GlobalVariableRef (Scm_GaucheModule)
                                    (SCM_SYMBOL (SCM_INTERN "cons"))
                                    0)])
    (return (Scm_ApplyRec2 fn x x))))
(define-cfn "foo_o_cb" (proc x)
  (return (Scm_ApplyRec1 proc x)))


;; flonum argument
(define-cfn "fd_o" (x::double)
  (return (Scm_MakeFlonum (+ x 1.0))))
(define-cfn "fid_o" (x::ScmSmallInt y::double)
  (return (Scm_MakeFlonum (- (cast double x) y))))
(define-cfn "fdi_o" (x::double y::ScmSmallInt)
  (return (Scm_MakeFlonum (- x (cast double y)))))

;; flonum return
(define-cfn "fiiiiii_d" (a::int b::int c::int d::int e::int f::int) ::double
  (return (/ (+ a b c d e f) 2.0)))


