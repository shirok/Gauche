;;
;; Generate export declarations
;;

(use file.util)
(use srfi-13)
(use util.match)

(define (usage)
  (print "Usage: gosh ./ifgen.scm TAG")
  (print "   or: gosh ./ifgen.scm base")
  (print "   or: gosh ./ifgen.scm exports")
  (exit 1))

(define (main args)
  (match (cdr args)
    [("exports") (gen-exports)]
    [("base") (gen-base)]
    [(tag) (gen-specific tag)]
    [_ (usage)])
 0)

(define (subst tag syms)
  (let1 t (x->string tag)
    (map (^s (string->symbol (regexp-replace #/@/ (symbol->string s) t))) syms)))

(define (gen-specific tag)
  (with-output-to-file (format "uvector/~a.scm" tag)
    (^[]
      (print ";; Generated automatically.  Do not edit")
      (pprint `(define-module ,(symbol-append 'gauche.uvector. tag)
                 (use gauche.uvector)
                 (export ,@(subst tag *srfi-160-base-api*))
                 (export ,@(subst tag *srfi-160-api*)))))))

(define (gen-base)
  (with-output-to-file "uvector/base.scm"
    (^[]
      (print ";; Generated automatically.  Do not edit")
      (pprint `(define-module gauche.uvector.base
                 (use gauche.uvector)
                 ,@(map (^[tag] `(export ,@(subst tag *srfi-160-base-api*)))
                        '(u8 s8 u16 s16 u32 s32 u64 s64
                          f32 f64 c64 c128)))))))


(define (gen-exports)
  (with-output-to-file "exports.scm"
    (^[]
      (print ";; Generated automatically.  Do not edit")
      (pprint `(export
                ,@(append-map (cute subst <> (append *srfi-160-base-api*
                                                     *srfi-160-api*
                                                     *extra-api*
                                                     *extra-api-integral*
                                                     *extra-api-scalar*))
                              '(u8 s8))
                ,@(append-map (cute subst <> (append *srfi-160-base-api*
                                                     *srfi-160-api*
                                                     *extra-api*
                                                     *extra-api-integral*
                                                     *extra-api-scalar*
                                                     *extra-api-multibyte*))
                              '(u16 s16 u32 s32 u64 s64))
                ,@(append-map (cute subst <> (append *srfi-160-base-api*
                                                     *srfi-160-api*
                                                     *extra-api*
                                                     *extra-api-real*
                                                     *extra-api-scalar*
                                                     *extra-api-multibyte*))
                              '(f16 f32 f64))
                ,@(append-map (cute subst <> (append *srfi-160-base-api*
                                                     *srfi-160-api*
                                                     *extra-api*
                                                     *extra-api-real*))
                              '(c32 c64 c128)))))))

(define *srfi-160-base-api*
  '(make-@vector
    @vector
    @vector?
    @vector-length
    @vector-ref
    @vector-set!
    @vector->list
    list->@vector
    @?))

(define *srfi-160-api*
  '(@vector-unfold
    @vector-unfold-right
    @vector-copy
    @vector-reverse-copy
    @vector-append
    @vector-concatenate
    @vector-append-subvectors
    @vector-empty?
    @vector=
    @vector-take
    @vector-take-right
    @vector-drop
    @vector-drop-right
    @vector-segment
    @vector-fold
    @vector-fold-right
    @vector-map
    @vector-map!
    @vector-for-each
    @vector-count
    @vector-cumulate
    @vector-take-while
    @vector-take-while-right
    @vector-drop-while
    @vector-drop-while-right
    @vector-index
    @vector-index-right
    @vector-skip
    @vector-skip-right
    @vector-any
    @vector-every
    @vector-partition
    @vector-filter
    @vector-remove
    @vector-swap!
    @vector-fill!
    @vector-reverse!
    @vector-copy!
    @vector-reverse-copy!
    @vector-unfold!
    @vector-unfold-right!
    reverse-list->@vector
    reverse-@vector->list
    @vector->vector
    vector->@vector
    make-@vector-generator
    write-@vector
    @vector-comparator
    ))

(define *extra-api*
  '(@vector-add
    @vector-add!
    @vector-sub
    @vector-sub!
    @vector-mul
    @vector-mul!
    @vector-dot
    @vector-multi-copy!
    @vector=?
    @vector-compare))

(define *extra-api-integral*
  '(@vector-and
    @vector-and!
    @vector-ior
    @vector-ior!
    @vector-xor
    @vector-xor!))

(define *extra-api-real*
  '(@vector-div
    @vector-div!))

(define *extra-api-scalar*
  '(@vector-clamp
    @vector-clamp!
    @vector-range-check))

(define *extra-api-multibyte*
  '(@vector-swap-bytes
    @vector-swap-bytes!))
