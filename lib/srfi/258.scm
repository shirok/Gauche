;;;
;;; SRFI-258 - Uninterned symbols
;;;

(define-module srfi.258
  (export string->uninterned-symbol     ;built-in
          symbol-interned?              ;built-in
          generate-uninterned-symbol))
(select-module srfi.258)

(define (generate-uninterned-symbol :optional (prefix "G"))
  (assume-type prefix (</> <string> <symbol>))
  (gensym (x->string prefix)))
