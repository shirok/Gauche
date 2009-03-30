;; KLUDGE to prevent 0.8.14 from complaining uvector support code.
;; literal.scm must be read by 0.8.14 to build the newer version,
;; but it doesn't have uvector core support so it would bark at this code.
;; Once next version is released, the content of this file should be
;; merged into literal.scm

;; NB: Because of building dependencies, we can't rely on gauche.uvector
;; module (since this file is used to precompile gauche.uvector).
;; The following code is carefully assembled to use only the built-in features.

(define-cgen-literal <cgen-scheme-uvector> <uvector>
  ((elements :init-keyword :elements))
  [make (value)
    (let1 elts (format "uvector__~a" (cgen-unique-name))
      (make <cgen-scheme-uvector>
        :value value :elements elts
        :c-name (cgen-allocate-static-datum
                 'runtime 'ScmUVector
                 (format "  SCM_UVECTOR_INITIALIZER(~a, ~a, ~a, ~a, NULL)"
                         (uvector-class->c-name (class-of value))
                         (uvector-length value)
                         elts
                         (if (uvector-immutable? value) 1 0)))))]
  [decl (self)
    (let* ([value (~ self'value)]
           [class (class-of value)])
      (print (uvector-class->c-type-name class)" "(~ self'elements)"[] = {")
      (dotimes [i (uvector-length value)]
        ($ uvector-class-emit-elt class
           $ %uvector-ref value (uvector-class->type-enum class) i))
      (print  "};"))]
  [static (self) #f]
  )

(define (uvector-class->tag-name class)
  (rxmatch->string #/<(.\d+)vector>/ (symbol->string (class-name class)) 1))
      
(define (uvector-class->c-name class)
  #`"Scm_,(string-upcase (uvector-class->tag-name class))VectorClass")

(define (uvector-class->type-enum class)
  (global-variable-ref
   (find-module 'gauche)
   (string->symbol
    #`"SCM_UVECTOR_,(string-upcase (uvector-class->tag-name class))")))

(define (uvector-class->c-type-name class)
  (cond
    [(eq? class <s8vector>) "signed char"]
    [(eq? class <u8vector>) "unsigned char"]
    [(eq? class <s16vector>) "short"]
    [(eq? class <u16vector>) "unsigned short"]
    [(eq? class <s32vector>) "ScmInt32"]
    [(eq? class <u32vector>) "ScmUInt32"]
    [(eq? class <s64vector>) "ScmInt64"]
    [(eq? class <u64vector>) "ScmUInt64"]
    [(eq? class <f16vector>) "ScmHalfFloat"]
    [(eq? class <f32vector>) "float"]
    [(eq? class <f64vector>) "double"]
    ))

(define (uvector-class-emit-elt class v)
  (cond
   [(memq class `(,<s8vector> ,<s16vector> ,<s32vector>))
    (format #t "~d,\n" v)]
   [(memq class `(,<u8vector> ,<u16vector> ,<u32vector>))
    (format #t "~du,\n" v)]
   [(eq? class <s64vector>)
    (print "#if SIZEOF_LONG == 8")
    (format #t "~dl,\n" v)
    (print "#else if SCM_EMULATE_INT64")
    (print "#ifdef WORDS_BIGENDIAN")
    (print "#else  /*!WORDS_BIGENDIAN*/")
    (print "#endif /*!WORDS_BIGENDIAN*/")
    (print "#else  /*SIZEOF_LONG == 4 && !SCM_EMULATE_INT64*/")
    (format #t " (((int64_t)~dl << 32)|~dl),\n"
            (ash v -32) (logand v (- (expt 2 32) 1)))
    (print "#endif /*SIZEOF_LONG == 4 && !SCM_EMULATE_INT64*/")]
   [(eq? class <u64vector>)
    (print "#if SIZEOF_LONG == 8")
    (format #t "~dlu,\n" v)
    (print "#else if SCM_EMULATE_INT64")
    (print "#ifdef WORDS_BIGENDIAN")
    (print "#else  /*!WORDS_BIGENDIAN*/")
    (print "#endif /*!WORDS_BIGENDIAN*/")
    (print "#else  /*SIZEOF_LONG == 4 && !SCM_EMULATE_INT64*/")
    (format #t " (((int64_t)~dlu << 32)|~dlu),\n"
            (ash v -32) (logand v (- (expt 2 32) 1)))
    (print "#endif /*SIZEOF_LONG == 4 && !SCM_EMULATE_INT64*/")]
   [(eq? class <f16vector>)
    (error "gauche.cgen.literal: literal f16vector isn't supported yet")]
   [(eq? class <f32vector>) (format #t "~sf,\n" v)]
   [(eq? class <f64vector>) (format #t "~s,\n" v)]
   ))

