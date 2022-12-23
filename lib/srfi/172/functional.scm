;; srfi.172.functional - functional subset of R7RS

(define-module srfi.172.functional
  (use scheme.base)
  (use scheme.case-lambda)
  (use scheme.char)
  (use scheme.complex)
  (use scheme.cxr)
  (use scheme.inexact)
  (use scheme.lazy)
  (export
    - * / + < <= = => > >= abs acos and angle append apply asin assoc
    assq assv atan begin boolean? boolean=? bytevector bytevector?
    bytevector-append bytevector-copy bytevector-length
    bytevector-u8-ref caaaar caaadr caaar caadar caaddr caadr caar
    cadaar cadadr cadar caddar cadddr caddr cadr call/cc
    call-with-current-continuation call-with-values car case
    case-lambda cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar
    cddadr cddar cdddar cddddr cdddr cddr cdr ceiling char? char<?
    char<=? char=? char>? char>=? char->integer char-alphabetic?
    char-ci<? char-ci<=? char-ci=? char-ci>? char-ci>=?  char-downcase
    char-foldcase char-lower-case? char-numeric? char-upcase
    char-upper-case? char-whitespace?  complex? cond cond-expand cons
    cos delay delay-force denominator digit-value do dynamic-wind else
    eof-object eof-object? eq? equal? eqv?  error error-object?
    error-object-irritants error-object-message even? exact exact?
    exact-integer? exact-integer-sqrt exp expt finite? floor floor/
    floor-quotient floor-remainder force for-each gcd
    get-output-bytevector get-output-string guard if imag-part inexact
    inexact? infinite? integer? integer->char lambda lcm length let
    let* let*-values letrec letrec* let-values list
    list?  list->string list->vector list-copy list-ref list-tail log
    magnitude make-bytevector make-list make-parameter make-polar
    make-promise make-rectangular make-string make-vector map max
    member memq memv min modulo nan? negative? newline not null?
    number? number->string numerator odd? open-input-bytevector
    open-input-string open-output-bytevector open-output-string or
    pair? parameterize positive?  procedure? promise? quasiquote quote
    quotient raise raise-continuable rational? rationalize real?
    real-part remainder reverse round sin sqrt square string string?
    string<? string<=?  string=? string>? string>=? string->list
    string->number string->utf8 string->vector string-append
    string-ci<? string-ci<=? string-ci=?  string-ci>? string-ci>=?
    string-copy string-downcase string-foldcase string-for-each
    string-length string-map string-ref string-upcase substring
    symbol? symbol=? symbol->string tan truncate truncate/
    truncate-quotient truncate-remainder unless unquote
    unquote-splicing utf8->string values vector vector? vector->list
    vector->string vector-append vector-copy vector-for-each
    vector-length vector-map vector-ref when with-exception-handler
    zero?))
