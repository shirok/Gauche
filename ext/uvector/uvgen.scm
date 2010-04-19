;;
;; Generates uvect.c from uvect.c.tmpl
;;

(use srfi-1)
(use srfi-13)
(use text.tree)
(use util.list)
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
    (usage)
    (let ((ifile (cadr args))
          (rules (make-rules)))
      (process ifile rules)))
  0)

(define (usage)
  (p "Usage: gosh uvgen.scm <file>.tmpl")
  (exit 0))

;; file translation toplevel
(define (process tmpl-file rules)
  (let ((c-file (regexp-replace #/\.tmpl$/ tmpl-file "")))
    (load-template tmpl-file)
    (with-output-to-file c-file
      (lambda ()
        (for-each print *tmpl-prologue*)
        (dolist (rule rules)
          (for-each (cut substitute <> rule) *tmpl-body*))
        (when *extra-procedure*
          (*extra-procedure*))
        (for-each print *tmpl-epilogue*)))))

;; load uvect.tmpl
;; this sets up variables.
(define (load-template tmpl-file)
  (define (translate)
    (let loop ((line (read-line)))
      (cond ((eof-object? line))
            ((string-prefix? "///" line)
             (p (string-drop line 3))
             (loop (read-line)))
            (else
             (p "\"" (regexp-replace-all #/[\\\"]/ line "\\\\\\0") "\"")
             (loop (read-line))))))

  (receive (out name) (sys-mkstemp tmpl-file)
    (unwind-protect
     (begin
       (with-output-to-port out
         (cut with-input-from-file tmpl-file
              (cut translate)))
       (close-output-port out)
       (call-with-input-file name load-from-port))
     (sys-unlink name))))

;; substitute ${param arg ...} in LINE.  output goes to curout.
;; word-alist :: ((<string> . <string-or-proc>) ...)
(define (substitute line words-alist)
  (define (sub key . args)
    (cond ((assoc key words-alist)
           => (lambda (x)
                (if (string? (cdr x))
                  (display (cdr x))
                  (display (apply (cdr x) args)))))
          (else
           (error "unknown substitution word:" key))))
  (with-input-from-string line
    (lambda ()
      (let loop ((ch (read-char)))
        (cond ((eof-object? ch) (newline))
              ((char=? ch #\$)
               (read-char) ;; we count on well-formed template
               (let1 lis (read-list #\})
                 (apply sub lis)
                 (loop (read-char))))
              (else
               (write-char ch)
               (loop (read-char))))))))

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

(define (make-rules)
  (append (make-integer-rules) (make-flonum-rules)))

;; common stuff
(define (common-rules tag)
  (let* ((stag  (symbol->string tag))
         (STAG  (string-upcase stag)))
    `((t   . ,stag)
      (T   . ,STAG))))

(define (make-s8rules)
  (list*
   '(etype . "signed char")
   '(ntype . "long")
   `(REF_NTYPE . ,(lambda (v i) #`"(long)SCM_S8VECTOR_ELEMENTS(,v)[,i]"))
   `(CAST_N2E  . ,(lambda (exp) #`"(signed char)(,exp)"))
   `(UNBOX . ,(lambda (dst src clamp)
                #`",dst = (signed char)Scm_GetInteger8Clamp(,src, ,clamp, NULL)"))
   `(NUNBOX . ,(lambda (dst src clamp)
                #`",dst = Scm_GetInteger8Clamp(,src, ,clamp, NULL)"))
   `(BOX   . ,(lambda (dst src) #`",dst = SCM_MAKE_INT(,src)"))
   `(VMBOX . ,(lambda (dst src) #`",dst = SCM_MAKE_INT(,src)"))
   `(NBOX  . ,(lambda (dst src) #`",dst = Scm_MakeInteger(,src)"))
   `(VMNBOX  . ,(lambda (dst src) #`",dst = Scm_MakeInteger(,src)"))
   `(EQ    . ,(lambda (x y)     #`"(,x == ,y)"))
   `(PRINT . ,(lambda (out elt) #`"Scm_Printf(,out, \"%d\", ,elt)"))
   (common-rules 's8)))

(define (make-u8rules)
  (list*
   '(etype . "unsigned char")
   '(ntype . "long")
   `(REF_NTYPE . ,(lambda (v i) #`"(long)SCM_U8VECTOR_ELEMENTS(,v)[,i]"))
   `(CAST_N2E . ,(lambda (exp) #`"(unsigned char)(,exp)"))
   `(UNBOX . ,(lambda (dst src clamp)
                #`",dst = (u_char)Scm_GetIntegerU8Clamp(,src, ,clamp, NULL)"))
   `(NUNBOX . ,(lambda (dst src clamp)
                #`",dst = Scm_GetIntegerU8Clamp(,src, ,clamp, NULL)"))
   `(BOX   . ,(lambda (dst src) #`",dst = SCM_MAKE_INT(,src)"))
   `(VMBOX . ,(lambda (dst src) #`",dst = SCM_MAKE_INT(,src)"))
   `(NBOX  . ,(lambda (dst src) #`",dst = Scm_MakeIntegerU(,src)"))
   `(VMNBOX  . ,(lambda (dst src) #`",dst = Scm_MakeIntegerU(,src)"))
   `(EQ    . ,(lambda (x y)     #`"(,x == ,y)"))
   `(PRINT . ,(lambda (out elt) #`"Scm_Printf(,out, \"%d\", ,elt)"))
   (common-rules 'u8)))

(define (make-s16rules)
  (list*
   '(etype . "short")
   '(ntype . "long")
   `(REF_NTYPE . ,(lambda (v i) #`"(long)SCM_S16VECTOR_ELEMENTS(,v)[,i]"))
   `(CAST_N2E . ,(lambda (exp) #`"(short)(,exp)"))
   `(UNBOX . ,(lambda (dst src clamp)
                #`",dst = (short)Scm_GetInteger16Clamp(,src, ,clamp, NULL)"))
   `(NUNBOX . ,(lambda (dst src clamp)
                #`",dst = Scm_GetInteger16Clamp(,src, ,clamp, NULL)"))
   `(BOX   . ,(lambda (dst src) #`",dst = SCM_MAKE_INT(,src)"))
   `(VMBOX . ,(lambda (dst src) #`",dst = SCM_MAKE_INT(,src)"))
   `(NBOX  . ,(lambda (dst src) #`",dst = Scm_MakeInteger(,src)"))
   `(VMNBOX  . ,(lambda (dst src) #`",dst = Scm_MakeInteger(,src)"))
   `(EQ    . ,(lambda (x y)     #`"(,x == ,y)"))
   `(PRINT . ,(lambda (out elt) #`"Scm_Printf(,out, \"%d\", ,elt)"))
   (common-rules 's16)))

(define (make-u16rules)
  (list*
   '(etype . "unsigned short")
   '(ntype . "long")
   `(REF_NTYPE . ,(lambda (v i) #`"(long)SCM_U16VECTOR_ELEMENTS(,v)[,i]"))
   `(CAST_N2E . ,(lambda (exp) #`"(unsigned short)(,exp)"))
   `(UNBOX . ,(lambda (dst src clamp)
                #`",dst = (u_short)Scm_GetIntegerU16Clamp(,src, ,clamp, NULL)"))
   `(NUNBOX . ,(lambda (dst src clamp)
                #`",dst = Scm_GetIntegerU16Clamp(,src, ,clamp, NULL)"))
   `(BOX   . ,(lambda (dst src) #`",dst = SCM_MAKE_INT(,src)"))
   `(VMBOX . ,(lambda (dst src) #`",dst = SCM_MAKE_INT(,src)"))
   `(NBOX  . ,(lambda (dst src) #`",dst = Scm_MakeIntegerU(,src)"))
   `(VMNBOX  . ,(lambda (dst src) #`",dst = Scm_MakeIntegerU(,src)"))
   `(EQ    . ,(lambda (x y)     #`"(,x == ,y)"))
   `(PRINT . ,(lambda (out elt) #`"Scm_Printf(,out, \"%d\", ,elt)"))
   (common-rules 'u16)))

(define (make-s32rules)
  (list*
   '(etype . "ScmInt32")
   '(ntype . "long")
   `(REF_NTYPE . ,(lambda (v i) #`"(long)SCM_S32VECTOR_ELEMENTS(,v)[,i]"))
   `(CAST_N2E . ,(lambda (exp) #`"(ScmInt32)(,exp)"))
   `(UNBOX . ,(lambda (dst src clamp)
                #`",dst = (ScmInt32)Scm_GetInteger32Clamp(,src,, ,clamp,, NULL)"))
   `(NUNBOX . ,(lambda (dst src clamp)
                #`",dst = Scm_GetInteger32Clamp(,src,, ,clamp,, NULL)"))
   `(BOX   . ,(lambda (dst src) #`",dst = Scm_MakeInteger(,src)"))
   `(VMBOX . ,(lambda (dst src) #`",dst = Scm_MakeInteger(,src)"))
   `(NBOX  . ,(lambda (dst src) #`",dst = Scm_MakeInteger(,src)"))
   `(VMNBOX  . ,(lambda (dst src) #`",dst = Scm_MakeInteger(,src)"))
   `(EQ    . ,(lambda (x y)     #`"(,x == ,y)"))
   `(PRINT . ,(lambda (out elt) #`"Scm_Printf(,out,, \"%d\",, ,elt)"))
   (common-rules 's32)))

(define (make-u32rules)
  (list*
   '(etype . "ScmUInt32")
   '(ntype . "u_long")
   `(REF_NTYPE . ,(lambda (v i) #`"(u_long)SCM_U32VECTOR_ELEMENTS(,v)[,i]"))
   `(CAST_N2E . ,(lambda (exp) #`"(ScmUInt32)(,exp)"))
   `(UNBOX . ,(lambda (dst src clamp)
                #`",dst = (ScmUInt32)Scm_GetIntegerU32Clamp(,src,, ,clamp,, NULL)"))
   `(NUNBOX . ,(lambda (dst src clamp)
                #`",dst = Scm_GetIntegerU32Clamp(,src,, ,clamp,, NULL)"))
   `(BOX   . ,(lambda (dst src) #`",dst = Scm_MakeIntegerU(,src)"))
   `(VMBOX . ,(lambda (dst src) #`",dst = Scm_MakeIntegerU(,src)"))
   `(NBOX  . ,(lambda (dst src) #`",dst = Scm_MakeIntegerU(,src)"))
   `(VMNBOX  . ,(lambda (dst src) #`",dst = Scm_MakeIntegerU(,src)"))
   `(EQ    . ,(lambda (x y)     #`"(,x == ,y)"))
   `(PRINT . ,(lambda (out elt) #`"Scm_Printf(,out,, \"%u\",, ,elt)"))
   (common-rules 'u32)))

(define (make-s64rules)
  (list*
   '(etype . "ScmInt64")
   '(ntype . "ScmInt64")
   `(REF_NTYPE . ,(lambda (v i) #`"SCM_S64VECTOR_ELEMENTS(,v)[,i]"))
   `(CAST_N2E . ,identity)
   `(UNBOX . ,(lambda (dst src clamp)
                #`",dst = Scm_GetInteger64Clamp(,src,, ,clamp,, NULL)"))
   `(NUNBOX . ,(lambda (dst src clamp)
                #`",dst = Scm_GetInteger64Clamp(,src,, ,clamp,, NULL)"))
   `(BOX   . ,(lambda (dst src) #`",dst = Scm_MakeInteger64(,src)"))
   `(VMBOX . ,(lambda (dst src) #`",dst = Scm_MakeInteger64(,src)"))
   `(NBOX  . ,(lambda (dst src) #`",dst = Scm_MakeInteger64(,src)"))
   `(VMNBOX  . ,(lambda (dst src) #`",dst = Scm_MakeInteger64(,src)"))
   `(EQ    . ,(lambda (x y)     #`"int64eqv(,x,, ,y)"))
   `(PRINT . ,(lambda (out elt) #`"int64print(,out,, ,elt)"))
   (common-rules 's64)))

(define (make-u64rules)
  (list*
   '(etype . "ScmUInt64")
   '(ntype . "ScmUInt64")
   `(REF_NTYPE . ,(lambda (v i) #`"SCM_U64VECTOR_ELEMENTS(,v)[,i]"))
   `(CAST_N2E . ,identity)
   `(UNBOX . ,(lambda (dst src clamp)
                #`",dst = Scm_GetIntegerU64Clamp(,src,, ,clamp,, NULL)"))
   `(NUNBOX . ,(lambda (dst src clamp)
                #`",dst = Scm_GetIntegerU64Clamp(,src,, ,clamp,, NULL)"))
   `(BOX   . ,(lambda (dst src) #`",dst = Scm_MakeIntegerU64(,src)"))
   `(VMBOX . ,(lambda (dst src) #`",dst = Scm_MakeIntegerU64(,src)"))
   `(NBOX  . ,(lambda (dst src) #`",dst = Scm_MakeIntegerU64(,src)"))
   `(VMNBOX  . ,(lambda (dst src) #`",dst = Scm_MakeIntegerU64(,src)"))
   `(EQ    . ,(lambda (x y)     #`"uint64eqv(,x,, ,y)"))
   `(PRINT . ,(lambda (out elt) #`"uint64print(,out,, ,elt)"))
   (common-rules 'u64)))

(define (make-f16rules)
  (list*
   '(etype . "ScmHalfFloat")
   '(ntype . "double")
   `(REF_NTYPE . ,(lambda (v i) #`"Scm_HalfToDouble(SCM_F16VECTOR_ELEMENTS(,v)[,i])"))
   `(CAST_N2E . ,(lambda (exp) #`"Scm_DoubleToHalf(,exp)"))
   `(UNBOX . ,(lambda (dst src clamp)
                #`",dst = Scm_DoubleToHalf(Scm_GetDouble(,src))"))
   `(NUNBOX . ,(lambda (dst src clamp)
                #`",dst = Scm_GetDouble(,src)"))
   `(BOX   . ,(lambda (dst src) #`",dst = Scm_MakeFlonum(Scm_HalfToDouble(,src))"))
   `(VMBOX . ,(lambda (dst src) #`",dst = Scm_VMReturnFlonum(Scm_HalfToDouble(,src))"))
   `(NBOX  . ,(lambda (dst src) #`",dst = Scm_MakeFlonum(,src)"))
   `(VMNBOX  . ,(lambda (dst src) #`",dst = Scm_VMReturnFlonum(,src)"))
   `(EQ    . ,(lambda (x y)     #`"SCM_HALF_FLOAT_CMP(==,, ,x,, ,y)"))
   `(PRINT . ,(lambda (out elt) #`"Scm_PrintDouble(,out,, Scm_HalfToDouble(,elt),, 0)"))
   (common-rules 'f16)))

(define (make-f32rules)
  (list*
   '(etype . "float")
   '(ntype . "double")
   `(REF_NTYPE . ,(lambda (v i) #`"(double)SCM_F32VECTOR_ELEMENTS(,v)[,i]"))
   `(CAST_N2E . ,(lambda (exp) #`"(float)(,exp)"))
   `(UNBOX . ,(lambda (dst src clamp)
                #`",dst = (float)Scm_GetDouble(,src)"))
   `(NUNBOX . ,(lambda (dst src clamp)
                #`",dst = Scm_GetDouble(,src)"))
   `(BOX   . ,(lambda (dst src) #`",dst = Scm_MakeFlonum((double),src)"))
   `(VMBOX . ,(lambda (dst src) #`",dst = Scm_VMReturnFlonum((double),src)"))
   `(NBOX  . ,(lambda (dst src) #`",dst = Scm_MakeFlonum(,src)"))
   `(VMNBOX  . ,(lambda (dst src) #`",dst = Scm_VMReturnFlonum(,src)"))
   `(EQ    . ,(lambda (x y)     #`"(,x == ,y)"))
   `(PRINT . ,(lambda (out elt) #`"Scm_PrintDouble(,out,, (double),elt,, 0)"))
   (common-rules 'f32)))

(define (make-f64rules)
  (list*
   '(etype . "double")
   '(ntype . "double")
   `(REF_NTYPE . ,(lambda (v i) #`"SCM_F64VECTOR_ELEMENTS(,v)[,i]"))
   `(CAST_N2E . ,identity)
   `(UNBOX . ,(lambda (dst src clamp) #`",dst = Scm_GetDouble(,src)"))
   `(NUNBOX . ,(lambda (dst src clamp) #`",dst = Scm_GetDouble(,src)"))
   `(BOX   . ,(lambda (dst src)       #`",dst = Scm_MakeFlonum(,src)"))
   `(VMBOX . ,(lambda (dst src)       #`",dst = Scm_VMReturnFlonum(,src)"))
   `(NBOX   . ,(lambda (dst src)      #`",dst = Scm_MakeFlonum(,src)"))
   `(VMNBOX   . ,(lambda (dst src)      #`",dst = Scm_VMReturnFlonum(,src)"))
   `(EQ    . ,(lambda (x y)           #`"(,x == ,y)"))
   `(PRINT . ,(lambda (out elt)  #`"Scm_PrintDouble(,out,, (double),elt,, 0)"))
   (common-rules 'f64)))

(define (dummy . _) "/* not implemented */")

;;===============================================================
;; Uvector opertaion generator
;;

(define (generate-numop)
  (for-each
   (lambda (opname Opname Sopname)
     (dolist (rule (make-rules))
       (for-each (cute substitute <> (list* `(opname . ,opname)
                                            `(Opname . ,Opname)
                                            `(Sopname . ,Sopname)
                                            rule))
                 *tmpl-numop*)))
   '("add" "sub" "mul")
   '("Add" "Sub" "Mul")
   '("Add" "Sub" "Mul"))
  (dolist (rule (make-flonum-rules))
    (for-each (cute substitute <> (list* '(opname . "div")
                                         '(Opname . "Div")
                                         '(Sopname . "Div")
                                         rule))
              *tmpl-numop*)))

(define (generate-bitop)
  (dolist (rule (make-integer-rules))
    (for-each
     (lambda (opname Opname)
       (define (BITOP r v0 v1)
         (let ((tag (assq-ref rule 't))
               (c-op (case (string->symbol opname)
                       ((and) "&") ((ior) "|") ((xor) "^"))))
           (if (member tag '("s8" "u8" "s16" "u16" "s32" "u32"))
             #`",r = ,v0 ,c-op ,v1"
             #`"INT64BITOP(,|r|,, ,|v0|,, ,|c-op|,, ,|v1|)")))
       (define (BITEXT r v)
         (let ((tag (assq-ref rule 't)))
           (if (member tag '("s8" "u8" "s16" "u16" "s32" "u32"))
             #`",|r| = bitext(,|v|)"
             #`",|r| = bitext64(,|v|)")))
       (for-each (cute substitute <> (list* `(opname . ,opname)
                                            `(Opname . ,Opname)
                                            `(BITOP . ,BITOP)
                                            `(BITEXT . ,BITEXT)
                                            rule))
                 *tmpl-bitop*))
     '("and" "ior" "xor")
     '("And" "Ior" "Xor"))))

(define (generate-dotop)
  (dolist (rule (make-rules))
    (let1 tag (string->symbol (assq-ref rule 't))
      (define (ZERO r)
        (case tag
          ((s64 u64)
           #`"SCM_SET_INT64_ZERO(,r)")
          (else
           #`",r = 0")))
;      (define (NBOX dst src)
;        (case tag
;          ((s8 s16 s32) #`",dst = Scm_MakeInteger(,src)")
;          ((u8 u16 u32) #`",dst = Scm_MakeIntegerU(,src)")
;          ((s64) #`",dst = Scm_MakeInteger64(,src)")
;          ((u64) #`",dst = Scm_MakeIntegerU64(,src)")
;          ((f16 f32 f64) #`",dst = Scm_MakeFlonum(,src)")))
      (for-each (cute substitute <> (list* `(ZERO . ,ZERO)
;                                           `(NBOX . ,NBOX)
                                           rule))
                *tmpl-dotop*))))

(define (generate-rangeop)
  (dolist (rule (make-rules))
    (let ((tag (string->symbol (assq-ref rule 't)))
          (TAG (assq-ref rule 'T))
          (cast (assq-ref rule 'CAST_N2E)))
      (define (ZERO r)
        (case tag
          ((s64 u64)
           #`"SCM_SET_INT64_ZERO(,r)")
          (else
           #`",r = 0")))
      (define (GETLIM r dc v)
        (let1 getter
            (case tag
              ((s8)  #`"Scm_GetInteger8Clamp(,v, SCM_CLAMP_BOTH, NULL)")
              ((u8)  #`"Scm_GetIntegerU8Clamp(,v, SCM_CLAMP_BOTH, NULL)")
              ((s16) #`"Scm_GetInteger16Clamp(,v, SCM_CLAMP_BOTH, NULL)")
              ((u16) #`"Scm_GetIntegerU16Clamp(,v, SCM_CLAMP_BOTH, NULL)")
              ((s32) #`"Scm_GetInteger32Clamp(,|v|, SCM_CLAMP_BOTH, NULL)")
              ((u32) #`"Scm_GetIntegerU32Clamp(,|v|, SCM_CLAMP_BOTH, NULL)")
              ((s64) #`"Scm_GetInteger64Clamp(,|v|, SCM_CLAMP_BOTH, NULL)")
              ((u64) #`"Scm_GetIntegerU64Clamp(,|v|, SCM_CLAMP_BOTH, NULL)")
              ((f16 f32 f64) #`"Scm_GetDouble(,|v|)"))
          (tree->string
           `("if ((",dc" = SCM_FALSEP(",v")) == FALSE) {\n"
             "  ",r" = ",getter";\n"
             "}"))))
      (define (LT a b)
        (case tag
          ((s64 u64) #`"INT64LT(,|a|,, ,|b|)")
          (else      #`"(,|a| < ,|b|)")))
      (dolist (ops `(("range-check" "RangeCheck"
                      ""
                      "return Scm_MakeInteger(i)"
                      "SCM_FALSE")
                     ("clamp" "Clamp"
                      "ScmObj d = Scm_UVectorCopy(SCM_UVECTOR(x), 0, -1)"
                      ,#`"SCM_,|TAG|VECTOR_ELEMENTS(d)[i] = ,(cast \"val\")"
                      "d")
                     ("clamp!" "ClampX"
                      ""
                      ,#`"SCM_,|TAG|VECTOR_ELEMENTS(x)[i] = ,(cast \"val\")"
                      "SCM_OBJ(x)")
                     ))
        (for-each (cute substitute <> (list* `(GETLIM . ,GETLIM)
                                             `(ZERO . ,ZERO)
                                             `(LT . ,LT)
                                             `(opname .  ,(ref ops 0))
                                             `(Opname .  ,(ref ops 1))
                                             `(dstdecl . ,(ref ops 2))
                                             `(action .  ,(ref ops 3))
                                             `(okval  .  ,(ref ops 4))
                                             rule))
                  *tmpl-rangeop*)))))

(define (generate-swapb)
  (dolist (rule (make-rules))
    (let1 tag (string->symbol (assq-ref rule 't))
      (define (SWAPB)
        (case tag
          ((s16 u16 f16) "SWAP_2")
          ((s32 u32 f32) "SWAP_4")
          ((s64 u64 f64) "SWAP_8")))
      (unless (memq tag '(s8 u8))
        (for-each (cute substitute <> (list* `(SWAPB . ,SWAPB)
                                             rule))
                  *tmpl-swapb*)))))
