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
    (Scm_Write x o SCM_WRITE_WRITE)
    (return (Scm_GetStringConst (SCM_STRING (Scm_GetOutputString (SCM_PORT o) 0))))))

