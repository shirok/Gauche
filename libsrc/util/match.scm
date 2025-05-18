;;
;; util.match - Andrew Wright's pattern matching macro.
;;

(define-module util.match
  (export match
          match-lambda
          match-lambda*
          match-let
          match-let*
          match-letrec
          match-let1
          match-define
          match:$-ref
          |setter of match:$-ref|
          match:every
          match:error))
(select-module util.match)

(include "match-impl.scm")
