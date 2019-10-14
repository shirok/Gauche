;;
;; Generates uvector.c, uvector.h and uvlib.scm the corresponding *.tmpl file
;;

(use srfi-13)
(use text.tree)
(use file.util)
(use gauche.sequence)
(use gauche.parseopt)
(use gauche.parameter)

(define p print)

;; overwritten by template files
(define *tmpl-prologue* '())
(define *tmpl-body* '())
(define *tmpl-epilogue* '())
(define *extra-procedure* #f)

;; entry
(define (main args)
  (if (or (null? (cdr args))
          (not (string-suffix? ".tmpl" (cadr args))))
    (exit 0 "Usage: gosh uvgen.scm <file>.tmpl")
    (let ([ifile (cadr args)]
          [rules (make-rules)])
      (process ifile rules)))
  0)

;; file translation toplevel
(define (process tmpl-file rules)
  ;; we take basename, for the input file may be prefixed by $srcdir.
  (let1 c-file (sys-basename (regexp-replace #/\.tmpl$/ tmpl-file ""))
    (load-template tmpl-file)
    (with-output-to-file c-file
      (^[]
        (for-each print *tmpl-prologue*)
        (dolist [rule rules]
          (for-each (cut substitute <> rule) *tmpl-body*))
        (when *extra-procedure*
          (*extra-procedure*))
        (for-each print *tmpl-epilogue*)))))

;; load uvect.tmpl
;; this sets up variables.
(define (load-template tmpl-file)
  (define (translate)
    (let loop ([line (read-line)])
      (cond [(eof-object? line)]
            [(string-prefix? "///" line)
             (p (string-drop line 3))
             (loop (read-line))]
            [else
             (p "\"" (regexp-replace-all #/[\\\"]/ line "\\\\\\0") "\"")
             (loop (read-line))])))

  (call-with-temporary-file
   (^[out name]
     (with-output-to-port out
       (cut with-input-from-file tmpl-file
            (cut translate)))
     (close-output-port out)
     (call-with-input-file name load-from-port))
   :prefix (sys-basename tmpl-file)))

(define (getval rules key)
  (cond [(assq key rules) => cadr] [else #f]))

;; substitute ${param arg ...} in LINE.  output goes to curout.
;; rules :: ((<string> <string-or-proc>) ...)
(define (substitute line rules)
  (define (sub key . args)
    (cond [(any (cut getval rules <>) (cons key (compatible-keys key)))
           => (^v (if (string? v)
                    (display v)
                    (display (apply v args))))]
          [else (error "unknown substitution word:" key)]))
  (with-input-from-string line
    (^[] (let loop ([ch (read-char)])
           (cond [(eof-object? ch) (newline)]
                 [(char=? ch #\$)
                  (read-char) ;; we count on well-formed template
                  (let1 lis (read-list #\})
                    (apply sub lis)
                    (loop (read-char)))]
                 [else
                  (write-char ch)
                  (loop (read-char))])))))

(define (compatible-keys key)
  (case key
    [(ntype) '(etype)]
    [(NUNBOX) '(UNBOX)]
    [(VMBOX) '(BOX)]
    [(NBOX) '(BOX)]
    [(VMNBOX) '(NBOX BOX)]
    [else '()]))

;; keys ------------------------------------------------------------
;;
;;  t           : lowercase tag, e.g. "s16"
;;  T           : uppercase tag, e.g. "S16"
;;  etype       : element C type, e.g. int16_t for s16vector
;;  ntype       : natural C type to operate on each element. (optional)
;;                e.g. long for s16vector
;;  ZERO        : 'zero' value of etype (optional)
;;  (REF_NTYPE v i) 
;;              : Reference v[i] as ntype
;;  (CAST_N2E exp)
;;              : Cast etype's expression exp to ntype (optional)
;;  (UNBOX dst src clamp)
;;              : etype dst = unbox(ScmObj src, clamp)
;;  (NUNBOX dst src clamp)
;;              : ntype dst = unbox(ScmObj src, clamp)  (optional)
;;  (BOX dst src)
;;              : ScmObj dst = box(etype src)
;;  (NBOX dst src)
;;              : ScmObj dst = box(ntype src)   (optional)
;;  (VMBOX dst src)
;;              : ScmObj dst = box(etype src) // dst is VM register (optional)
;;  (NVMBOX dst src)
;;              : ScmObj dst = box(ntype src) // dst is VM register (optional)

;; substitution rules ----------------------------------------------

(define (make-integer-rules)
  (list (make-s8rules)
        (make-u8rules)
        (make-s16rules)
        (make-u16rules)
        (make-s32rules)
        (make-u32rules)
        (make-s64rules)
        (make-u64rules)))

(define (make-flonum-rules)
  (list (make-f16rules)
        (make-f32rules)
        (make-f64rules)))

(define (make-complex-rules)
  (list (make-c32rules)
        (make-c64rules)
        (make-c128rules)))

(define (make-scalar-rules)
  (append (make-integer-rules) (make-flonum-rules)))

(define (make-rules)
  (append (make-integer-rules) (make-flonum-rules) (make-complex-rules)))

;; common stuff
(define (common-rules tag)
  (let* ([stag  (symbol->string tag)]
         [STAG  (string-upcase stag)])
    `((t   ,stag)
      (T   ,STAG)
      (CAST_N2E ,identity)
      (ZERO  "0"))))

(define (make-s8rules)
  `((etype     "int8_t")
    (ntype     "long")
    (REF_NTYPE ,(^[v i] #"(long)SCM_S8VECTOR_ELEMENTS(~v)[~i]"))
    (CAST_N2E  ,(^[exp] #"(int8_t)(~exp)"))
    (UNBOX     ,(^[dst src clamp]
                  #"~dst = (int8_t)Scm_GetInteger8Clamp(~src, ~clamp, NULL)"))
    (NUNBOX    ,(^[dst src clamp]
                  #"~dst = Scm_GetInteger8Clamp(~src, ~clamp, NULL)"))
    (BOX       ,(^[dst src] #"~dst = SCM_MAKE_INT(~src)"))
    (NBOX      ,(^[dst src] #"~dst = Scm_MakeInteger(~src)"))
    ,@(common-rules 's8)))

(define (make-u8rules)
  `((etype     "uint8_t")
    (ntype     "long")
    (REF_NTYPE ,(^[v i] #"(long)SCM_U8VECTOR_ELEMENTS(~v)[~i]"))
    (CAST_N2E  ,(^[exp] #"(uint8_t)(~exp)"))
    (UNBOX     ,(^[dst src clamp]
                  #"~dst = (uint8_t)Scm_GetIntegerU8Clamp(~src, ~clamp, NULL)"))
    (NUNBOX    ,(^[dst src clamp]
                  #"~dst = Scm_GetIntegerU8Clamp(~src, ~clamp, NULL)"))
    (BOX       ,(^[dst src] #"~dst = SCM_MAKE_INT(~src)"))
    (NBOX      ,(^[dst src] #"~dst = Scm_MakeIntegerU(~src)"))
    ,@(common-rules 'u8)))

(define (make-s16rules)
  `((etype     "int16_t")
    (ntype     "long")
    (REF_NTYPE ,(^[v i] #"(long)SCM_S16VECTOR_ELEMENTS(~v)[~i]"))
    (CAST_N2E  ,(^[exp] #"(int16_t)(~exp)"))
    (UNBOX     ,(^[dst src clamp]
                  #"~dst = (int16_t)Scm_GetInteger16Clamp(~src, ~clamp, NULL)"))
    (NUNBOX    ,(^[dst src clamp]
                  #"~dst = Scm_GetInteger16Clamp(~src, ~clamp, NULL)"))
    (BOX       ,(^[dst src] #"~dst = SCM_MAKE_INT(~src)"))
    (NBOX      ,(^[dst src] #"~dst = Scm_MakeInteger(~src)"))
    ,@(common-rules 's16)))

(define (make-u16rules)
  `((etype     "uint16_t")
    (ntype     "long")
    (REF_NTYPE ,(^[v i] #"(long)SCM_U16VECTOR_ELEMENTS(~v)[~i]"))
    (CAST_N2E  ,(^[exp] #"(uint16_t)(~exp)"))
    (UNBOX     ,(^[dst src clamp]
                  #"~dst = (uint16_t)Scm_GetIntegerU16Clamp(~src, ~clamp, NULL)"))
    (NUNBOX    ,(^[dst src clamp]
                  #"~dst = Scm_GetIntegerU16Clamp(~src, ~clamp, NULL)"))
    (BOX       ,(^[dst src] #"~dst = SCM_MAKE_INT(~src)"))
    (NBOX      ,(^[dst src] #"~dst = Scm_MakeIntegerU(~src)"))
    ,@(common-rules 'u16)))

(define (make-s32rules)
  `((etype     "int32_t")
    (ntype     "long")
    (REF_NTYPE ,(^[v i] #"(long)SCM_S32VECTOR_ELEMENTS(~v)[~i]"))
    (CAST_N2E  ,(^[exp] #"(int32_t)(~exp)"))
    (UNBOX     ,(^[dst src clamp]
                  #"~dst = (int32_t)Scm_GetInteger32Clamp(~src, ~clamp, NULL)"))
    (NUNBOX    ,(^[dst src clamp]
                  #"~dst = Scm_GetInteger32Clamp(~src, ~clamp, NULL)"))
    (BOX       ,(^[dst src] #"~dst = Scm_MakeInteger(~src)"))
    ,@(common-rules 's32)))

(define (make-u32rules)
  `((etype     "uint32_t")
    (ntype     "u_long")
    (REF_NTYPE ,(^[v i] #"(u_long)SCM_U32VECTOR_ELEMENTS(~v)[~i]"))
    (CAST_N2E  ,(^[exp] #"(uint32_t)(~exp)"))
    (UNBOX     ,(^[dst src clamp]
                  #"~dst = (uint32_t)Scm_GetIntegerU32Clamp(~src, ~clamp, NULL)"))
    (NUNBOX    ,(^[dst src clamp]
                  #"~dst = Scm_GetIntegerU32Clamp(~src, ~clamp, NULL)"))
    (BOX       ,(^[dst src] #"~dst = Scm_MakeIntegerU(~src)"))
    ,@(common-rules 'u32)))

(define (make-s64rules)
  `((etype     "int64_t")
    (REF_NTYPE ,(^[v i] #"SCM_S64VECTOR_ELEMENTS(~v)[~i]"))
    (UNBOX     ,(^[dst src clamp]
                  #"~dst = Scm_GetInteger64Clamp(~src, ~clamp, NULL)"))
    (BOX       ,(^[dst src] #"~dst = Scm_MakeInteger64(~src)"))
    ,@(common-rules 's64)))

(define (make-u64rules)
  `((etype     "uint64_t")
    (REF_NTYPE ,(^[v i] #"SCM_U64VECTOR_ELEMENTS(~v)[~i]"))
    (UNBOX     ,(^[dst src clamp]
                  #"~dst = Scm_GetIntegerU64Clamp(~src, ~clamp, NULL)"))
    (BOX       ,(^[dst src] #"~dst = Scm_MakeIntegerU64(~src)"))
    ,@(common-rules 'u64)))

(define (make-f16rules)
  `((etype     "ScmHalfFloat")
    (ntype     "double")
    (REF_NTYPE ,(^[v i] #"Scm_HalfToDouble(SCM_F16VECTOR_ELEMENTS(~v)[~i])"))
    (CAST_N2E  ,(^[exp] #"Scm_DoubleToHalf(~exp)"))
    (UNBOX     ,(^[dst src clamp]
                  #"~dst = Scm_DoubleToHalf(Scm_GetDouble(~src))"))
    (NUNBOX    ,(^[dst src clamp]
                  #"~dst = Scm_GetDouble(~src)"))
    (BOX       ,(^[dst src] #"~dst = Scm_MakeFlonum(Scm_HalfToDouble(~src))"))
    (VMBOX     ,(^[dst src] #"~dst = Scm_VMReturnFlonum(Scm_HalfToDouble(~src))"))
    (NBOX      ,(^[dst src] #"~dst = Scm_MakeFlonum(~src)"))
    (VMNBOX    ,(^[dst src] #"~dst = Scm_VMReturnFlonum(~src)"))
    ,@(common-rules 'f16)))

(define (make-f32rules)
  `((etype     "float")
    (ntype     "double")
    (REF_NTYPE ,(^[v i] #"(double)SCM_F32VECTOR_ELEMENTS(~v)[~i]"))
    (CAST_N2E  ,(^[exp] #"(float)(~exp)"))
    (UNBOX     ,(^[dst src clamp] #"~dst = (float)Scm_GetDouble(~src)"))
    (NUNBOX    ,(^[dst src clamp] #"~dst = Scm_GetDouble(~src)"))
    (BOX       ,(^[dst src] #"~dst = Scm_MakeFlonum((double)~src)"))
    (VMBOX     ,(^[dst src] #"~dst = Scm_VMReturnFlonum((double)~src)"))
    (NBOX      ,(^[dst src] #"~dst = Scm_MakeFlonum(~src)"))
    (VMNBOX    ,(^[dst src] #"~dst = Scm_VMReturnFlonum(~src)"))
    ,@(common-rules 'f32)))

(define (make-f64rules)
  `((etype     "double")
    (REF_NTYPE ,(^[v i] #"SCM_F64VECTOR_ELEMENTS(~v)[~i]"))
    (UNBOX     ,(^[dst src clamp] #"~dst = Scm_GetDouble(~src)"))
    (BOX       ,(^[dst src]       #"~dst = Scm_MakeFlonum(~src)"))
    (VMBOX     ,(^[dst src]       #"~dst = Scm_VMReturnFlonum(~src)"))
    ,@(common-rules 'f64)))

(define (make-c32rules)
  `((etype     "ScmHalfComplex")
    (REF_NTYPE ,(^[v i] #"SCM_C32VECTOR_ELEMENTS(~v)[~i]"))
    (UNBOX     ,(^[dst src clamp] #"~dst = Scm_GetHalfComplex(~src)"))
    (BOX       ,(^[dst src]       #"~dst = Scm_HalfComplexToComplex(~src)"))
    (ZERO      "(ScmHalfComplex){0, 0}")
    ,@(common-rules 'c32)))

(define (make-c64rules)
  `((etype     "complex float")
    (REF_NTYPE ,(^[v i] #"SCM_C64VECTOR_ELEMENTS(~v)[~i]"))
    (UNBOX     ,(^[dst src clamp] #"~dst = Scm_GetFloatComplex(~src)"))
    (BOX       ,(^[dst src]       #"~dst = Scm_FloatComplexToComplex(~src)"))
    ,@(common-rules 'c64)))

(define (make-c128rules)
  `((etype     "complex double")
    (REF_NTYPE ,(^[v i] #"SCM_C128VECTOR_ELEMENTS(~v)[~i]"))
    (UNBOX     ,(^[dst src clamp] #"~dst = Scm_GetDoubleComplex(~src)"))
    (BOX       ,(^[dst src]       #"~dst = Scm_DoubleComplexToComplex(~src)"))
    ,@(common-rules 'c128)))

(define (dummy . _) "/* not implemented */")

;;===============================================================
;; Uvector operation generator
;;

(define (generate-numop)
  (for-each (^[opname Opname Sopname]
              (dolist [rule (make-rules)]
                (for-each (cute substitute <> `((opname  ,opname)
                                                (Opname  ,Opname)
                                                (Sopname ,Sopname)
                                                ,@rule))
                          *tmpl-numop*)))
            '("add" "sub" "mul")
            '("Add" "Sub" "Mul")
            '("Add" "Sub" "Mul"))
  (dolist [rule (append (make-flonum-rules) (make-complex-rules))]
    (for-each (cute substitute <> `((opname  "div")
                                    (Opname  "Div")
                                    (Sopname  "Div")
                                    ,@rule))
              *tmpl-numop*)))

(define (generate-bitop)
  (dolist [rule (make-integer-rules)]
    (for-each
     (^[opname Opname]
       (define (BITOP r v0 v1)
         (let ([tag  (getval rule 't)]
               [c-op (case (string->symbol opname)
                       ((and) "&") ((ior) "|") ((xor) "^"))])
           (if (member tag '("s8" "u8" "s16" "u16" "s32" "u32"))
             #"~r = ~v0 ~c-op ~v1"
             #"INT64BITOP(~|r|, ~|v0|, ~|c-op|, ~|v1|)")))
       (define (BITEXT r v)
         (let1 tag (getval rule 't)
           (if (member tag '("s8" "u8" "s16" "u16" "s32" "u32"))
             #"~r = bitext(~v)"
             #"~r = bitext64(~v)")))
       (for-each (cute substitute <> `((opname  ,opname)
                                       (Opname  ,Opname)
                                       (BITOP  ,BITOP)
                                       (BITEXT  ,BITEXT)
                                       ,@rule))
                 *tmpl-bitop*))
     '("and" "ior" "xor")
     '("And" "Ior" "Xor"))))

(define (generate-dotop)
  (dolist [rule (make-rules)]
    (for-each (cute substitute <> rule) *tmpl-dotop*)))

(define (generate-rangeop)
  (dolist [rule (make-scalar-rules)]
    (let ([tag (string->symbol (getval rule 't))]
          [TAG (getval rule 'T)]
          [cast (getval rule 'CAST_N2E)])
      (define (GETLIM r dc v)
        (let1 getter
            (case tag
              [(s8)  #"Scm_GetInteger8Clamp(~|v|, SCM_CLAMP_BOTH, NULL)"]
              [(u8)  #"Scm_GetIntegerU8Clamp(~|v|, SCM_CLAMP_BOTH, NULL)"]
              [(s16) #"Scm_GetInteger16Clamp(~|v|, SCM_CLAMP_BOTH, NULL)"]
              [(u16) #"Scm_GetIntegerU16Clamp(~|v|, SCM_CLAMP_BOTH, NULL)"]
              [(s32) #"Scm_GetInteger32Clamp(~|v|, SCM_CLAMP_BOTH, NULL)"]
              [(u32) #"Scm_GetIntegerU32Clamp(~|v|, SCM_CLAMP_BOTH, NULL)"]
              [(s64) #"Scm_GetInteger64Clamp(~|v|, SCM_CLAMP_BOTH, NULL)"]
              [(u64) #"Scm_GetIntegerU64Clamp(~|v|, SCM_CLAMP_BOTH, NULL)"]
              [(f16 f32 f64) #"Scm_GetDouble(~|v|)"])
          (tree->string
           `("if ((",dc" = SCM_FALSEP(",v")) == FALSE) {\n"
             "  ",r" = ",getter";\n"
             "}"))))
      (define (LT a b)
        (case tag
          [(s64 u64) #"INT64LT(~|a|, ~|b|)"]
          [else      #"(~a < ~b)"]))
      (dolist [ops `(("range-check" "RangeCheck"
                      ""
                      "return Scm_MakeInteger(i)"
                      "SCM_FALSE")
                     ("clamp" "Clamp"
                      "ScmObj d = Scm_UVectorCopy(SCM_UVECTOR(x), 0, -1)"
                      ,#"SCM_~|TAG|VECTOR_ELEMENTS(d)[i] = ~(cast \"val\")"
                      "d")
                     ("clamp!" "ClampX"
                      ""
                      ,#"SCM_~|TAG|VECTOR_ELEMENTS(x)[i] = ~(cast \"val\")"
                      "SCM_OBJ(x)")
                     )]
        (for-each (cute substitute <> `((GETLIM  ,GETLIM)
                                        (LT  ,LT)
                                        (opname   ,(ref ops 0))
                                        (Opname   ,(ref ops 1))
                                        (dstdecl  ,(ref ops 2))
                                        (action   ,(ref ops 3))
                                        (okval    ,(ref ops 4))
                                        ,@rule))
                  *tmpl-rangeop*)))))

(define (generate-swapb)
  (dolist [rule (make-scalar-rules)]
    (let1 tag (string->symbol (getval rule 't))
      (define (SWAPB)
        (case tag
          [(s16 u16 f16) "SWAP_2"]
          [(s32 u32 f32) "SWAP_4"]
          [(s64 u64 f64) "SWAP_8"]))
      (unless (memq tag '(s8 u8))
        (for-each (cute substitute <> `((SWAPB  ,SWAPB) ,@rule))
                  *tmpl-swapb*)))))
