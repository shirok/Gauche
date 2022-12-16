;;
;; SRFI-178 Bitvectors
;;
;;  gauche.bitvector is a superset of SRFI-178.  we just reexport
;;  srfi-defined procedures.
;;
(define-module srfi.178
  (use gauche.bitvector)
  (export
   ;; bit conversion
   bit->integer bit->boolean            ;built-in

   ;; constructors
   make-bitvector bitvector             ;built-in
   bitvector-unfold bitvector-unfold-right
   bitvector-copy                       ;built-in
   bitvector-reverse-copy
   bitvector-append bitvector-concatenate bitvector-append-subbitvectors

   ;; predicates
   bitvector?                           ;built-in
   bitvector-empty?
   bitvector=?

   ;; selectors
   bitvector-ref/int bitvector-ref/bool ;built-in
   bitvector-length                     ;built-in

   ;; iteration
   bitvector-take bitvector-take-right
   bitvector-drop bitvector-drop-right
   bitvector-segment
   bitvector-fold/int bitvector-fold/bool
   bitvector-fold-right/int bitvector-fold-right/bool
   bitvector-map/int bitvector-map/bool
   bitvector-map!/int bitvector-map!/bool
   bitvector-map->list/int bitvector-map->list/bool
   bitvector-for-each/int bitvector-for-each/bool

   ;; prefixes, suffixes, trimming, padding
   bitvector-prefix-length bitvector-suffix-length
   bitvector-prefix? bitvector-suffix?
   bitvector-pad bitvector-pad-right
   bitvector-trim bitvector-trim-right bitvector-trim-both

   ;; mutators
   bitvector-set!                       ;built-in
   bitvector-swap!
   bitvector-reverse!
   bitvector-copy!                      ;built-in
   bitvector-reverse-copy!

   ;; conversion
   bitvector->list/int bitvector->list/bool
   reverse-bitvector->list/int reverse-bitvector->list/bool
   list->bitvector                      ;built-in
   reverse-list->bitvector
   bitvector->vector/int bitvector->vector/bool
   reverse-bitvector->vector/int reverse-bitvector->vector/bool
   vector->bitvector
   reverse-vector->bitvector

   bitvector->string                    ;built-in
   string->bitvector                    ;built-in

   bitvector->integer integer->bitvector

   ;; generators
   make-bitvector/int-generator make-bitvector/bool-generator
   make-bitvector-accumulator

   ;; basic operations
   bitvector-not bitvector-not!
   bitvector-and bitvector-and!
   bitvector-ior bitvector-ior!
   bitvector-xor bitvector-xor!
   bitvector-eqv bitvector-eqv!

   bitvector-nand bitvector-nand!
   bitvector-nor bitvector-nor!
   bitvector-andc1 bitvector-andc1!
   bitvector-andc2 bitvector-andc2!
   bitvector-orc1 bitvector-orc1!
   bitvector-orc2 bitvector-orc2!

   ;; quasi-integer operations
   bitvector-logical-shift bitvector-count bitvector-count-run
   bitvector-if bitvector-first-bit

   ;; bit field operations
   bitvector-field-any? bitvector-field-every?
   bitvector-field-clear bitvector-field-clear!
   bitvector-field-set bitvector-field-set!
   bitvector-field-replace bitvector-field-replace!
   bitvector-field-replace-same bitvector-field-replace-same!
   bitvector-field-rotate
   bitvector-field-flip bitvector-field-flip!
   ))
