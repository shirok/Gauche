cat << EOF
;;;
;;; uvlib.stub - uniform vector library
;;;
;;;   Copyright (c) 2000-2003 Shiro Kawai (shiro@acm.org)
;;;
;;;   Permission to use, copy, modify, distribute this software and
;;;   accompanying documentation for any purpose is hereby granted,
;;;   provided that existing copyright notices are retained in all
;;;   copies and that this notice is included verbatim in all
;;;   distributions.
;;;   This software is provided as is, without express or implied
;;;   warranty.  In no circumstances the author(s) shall be liable
;;;   for any damages arising out of the use of this software.
;;;
;;; \$Id: uvlib.stub.sh,v 1.23 2003-02-05 01:44:34 shirok Exp $
;;;

"
#include \\"gauche/uvector.h\\"
#include \\"uvectorP.h\\"
"

(define-symbol both "sym_both")
(define-symbol high "sym_high")
(define-symbol low  "sym_low")

"static int clamp_arg(ScmObj clamp)
 {
   if (SCM_EQ(clamp, sym_both)) return SCM_UVECTOR_CLAMP_BOTH;
   else if (SCM_EQ(clamp, sym_low))  return SCM_UVECTOR_CLAMP_LO;
   else if (SCM_EQ(clamp, sym_high)) return SCM_UVECTOR_CLAMP_HI;
   if (!SCM_FALSEP(clamp) && !SCM_UNBOUNDP(clamp))
     Scm_Error(\"clamp argument must be either 'both, 'high, 'low or #f, but got %S\", clamp);
   return SCM_UVECTOR_CLAMP_NONE;
 }
"

(define-type <uvector> "ScmUVector*")

EOF

##==============================================================
## Common uvector operations
##
emit() {
    vecttag=$1
    VECTTAG=`echo $vecttag | tr '[a-z]' '[A-Z]'`
    vecttype="${VECTTAG}Vector"
    VECTTYPE="${VECTTAG}VECTOR"
    itemtype="${VECTTAG}ELTTYPE"
    integer=$2

    cat <<EOF
;;--------------------------------------------------------------------
;; $vecttype
;;
(define-type <${vecttag}vector> "Scm${vecttype}*")

(define-cproc ${vecttag}vector? (obj)
  (return <boolean> "SCM_${VECTTYPE}P"))

(define-cproc make-${vecttag}vector (length::<fixnum> &optional (fill 0))
  "  ${itemtype} filler;
  int clamp = SCM_UVECTOR_CLAMP_NONE;
  ${VECTTAG}UNBOX(filler, fill);
  SCM_RETURN(Scm_Make${vecttype}(length, filler));")

(define-cproc ${vecttag}vector (&rest args)
  "  SCM_RETURN(Scm_ListTo${vecttype}(args, 0));")

(define-cproc ${vecttag}vector-length (v::<${vecttag}vector>)
  "  SCM_RETURN(SCM_MAKE_INT(SCM_${VECTTYPE}_SIZE(v)));")

(define-cproc ${vecttag}vector-ref (v::<${vecttag}vector>
                                    i::<fixnum> &optional fallback)
  (setter ${vecttag}vector-set!)
  "  SCM_RETURN(Scm_${vecttype}Ref(v, i, fallback));")

(define-cproc ${vecttag}vector-set! (v::<${vecttag}vector>
                                     i::<fixnum> val &optional clamp)
  "  SCM_RETURN(Scm_${vecttype}Set(v, i, val, clamp_arg(clamp)));")

(define-cproc ${vecttag}vector-copy (v::<${vecttag}vector>
                                     &optional (start::<fixnum> 0)
                                               (end::<fixnum> -1))
  (return "Scm_${vecttype}Copy"))

(define-cproc ${vecttag}vector-copy! (dst::<${vecttag}vector> 
                                      src::<${vecttag}vector>)
  (return "Scm_${vecttype}CopyX"))

(define-cproc ${vecttag}vector->list (v::<${vecttag}vector>
                                      &optional (start::<fixnum> 0)
                                                (end::<fixnum> -1))
  (return "Scm_${vecttype}ToList"))

(define-cproc list->${vecttag}vector (l::<list> &optional clamp)
  "  SCM_RETURN(Scm_ListTo${vecttype}(l, clamp_arg(clamp)));")

(define-cproc ${vecttag}vector-fill! (v::<${vecttag}vector> val
                                      &optional (start::<fixnum> 0)
                                                (end::<fixnum> -1))
  "  ${itemtype} filler;
  int clamp = SCM_UVECTOR_CLAMP_NONE;
  ${VECTTAG}UNBOX(filler, val);
  SCM_RETURN(Scm_${vecttype}Fill(v, filler, start, end));")

(define-cproc ${vecttag}vector->vector (v::<${vecttag}vector>
                                        &optional (start::<fixnum> 0)
                                                  (end::<fixnum> -1))
  (return "Scm_${vecttype}ToVector"))

(define-cproc vector->${vecttag}vector (v::<vector>>
                                        &optional (start::<fixnum> 0)
                                                  (end::<fixnum> -1)
                                                  clamp)
  "  SCM_RETURN(Scm_VectorTo${vecttype}(v, start, end, clamp_arg(clamp)));")

;; Common arithmetic operations
(define-cproc ${vecttag}vector-add! (v0::<${vecttag}vector> v1 &optional clamp)
  "SCM_RETURN(Scm_${vecttype}Op(v0, v0, v1, SCM_UVECTOR_ADD, clamp_arg(clamp)));")

(define-cproc ${vecttag}vector-add (v0::<${vecttag}vector> v1 &optional clamp)
  " ${itemtype} init = ${VECTTAG}INIT;
  Scm${vecttype} *dst = SCM_${VECTTYPE}(Scm_Make${vecttype}(SCM_${VECTTYPE}_SIZE(v0), init));
  SCM_RETURN(Scm_${vecttype}Op(dst, v0, v1, SCM_UVECTOR_ADD, clamp_arg(clamp)));")

(define-cproc ${vecttag}vector-sub! (v0::<${vecttag}vector> v1 &optional clamp)
  "SCM_RETURN(Scm_${vecttype}Op(v0, v0, v1, SCM_UVECTOR_SUB, clamp_arg(clamp)));")

(define-cproc ${vecttag}vector-sub (v0::<${vecttag}vector> v1 &optional clamp)
  " ${itemtype} init = ${VECTTAG}INIT;
  Scm${vecttype} *dst = SCM_${VECTTYPE}(Scm_Make${vecttype}(SCM_${VECTTYPE}_SIZE(v0), init));
  SCM_RETURN(Scm_${vecttype}Op(dst, v0, v1, SCM_UVECTOR_SUB, clamp_arg(clamp)));")

(define-cproc ${vecttag}vector-mul! (v0::<${vecttag}vector> v1 &optional clamp)
  "SCM_RETURN(Scm_${vecttype}Op(v0, v0, v1, SCM_UVECTOR_MUL, clamp_arg(clamp)));")

(define-cproc ${vecttag}vector-mul (v0::<${vecttag}vector> v1 &optional clamp)
  " ${itemtype} init = ${VECTTAG}INIT;
  Scm${vecttype} *dst = SCM_${VECTTYPE}(Scm_Make${vecttype}(SCM_${VECTTYPE}_SIZE(v0), init));
  SCM_RETURN(Scm_${vecttype}Op(dst, v0, v1, SCM_UVECTOR_MUL, clamp_arg(clamp)));")

(define-cproc ${vecttag}vector-dot (v0::<${vecttag}vector> v1)
  "SCM_RETURN(Scm_${vecttype}DotProd(v0, v1));")

(define-cproc ${vecttag}vector-range-check (v0::<${vecttag}vector> min max)
  "SCM_RETURN(Scm_${vecttype}RangeCheck(v0, min, max));")

(define-cproc ${vecttag}vector-clamp (v0::<${vecttag}vector> min max)
  " Scm${vecttype} *dst = SCM_${VECTTYPE}(Scm_${vecttype}Copy(v0, 0, -1));
  SCM_RETURN(Scm_${vecttype}Clamp(dst, min, max));")

(define-cproc ${vecttag}vector-clamp! (v0::<${vecttag}vector> min max)
  "SCM_RETURN(Scm_${vecttype}Clamp(v0, min, max));")
EOF

##==============================================================
## Arithmetic opertaions
##
    if [ "$integer" = 1 ]; then
    cat <<EOF
;; Integer-only arithmetic operations
(define-cproc ${vecttag}vector-and! (v0::<${vecttag}vector> v1 &optional clamp)
  "SCM_RETURN(Scm_${vecttype}Op(v0, v0, v1, SCM_UVECTOR_AND, clamp_arg(clamp)));")

(define-cproc ${vecttag}vector-and (v0::<${vecttag}vector> v1 &optional clamp)
  " ${itemtype} init = ${VECTTAG}INIT;
  Scm${vecttype} *dst = SCM_${VECTTYPE}(Scm_Make${vecttype}(SCM_${VECTTYPE}_SIZE(v0), init));
  SCM_RETURN(Scm_${vecttype}Op(dst, v0, v1, SCM_UVECTOR_AND, clamp_arg(clamp)));")

(define-cproc ${vecttag}vector-ior! (v0::<${vecttag}vector> v1 &optional clamp)
  "SCM_RETURN(Scm_${vecttype}Op(v0, v0, v1, SCM_UVECTOR_IOR, clamp_arg(clamp)));")

(define-cproc ${vecttag}vector-ior (v0::<${vecttag}vector> v1 &optional clamp)
  " ${itemtype} init = ${VECTTAG}INIT;
  Scm${vecttype} *dst = SCM_${VECTTYPE}(Scm_Make${vecttype}(SCM_${VECTTYPE}_SIZE(v0), init));
  SCM_RETURN(Scm_${vecttype}Op(dst, v0, v1, SCM_UVECTOR_IOR, clamp_arg(clamp)));")

(define-cproc ${vecttag}vector-xor! (v0::<${vecttag}vector> v1 &optional clamp)
  "SCM_RETURN(Scm_${vecttype}Op(v0, v0, v1, SCM_UVECTOR_XOR, clamp_arg(clamp)));")

(define-cproc ${vecttag}vector-xor (v0::<${vecttag}vector> v1 &optional clamp)
  " ${itemtype} init = ${VECTTAG}INIT;
  Scm${vecttype} *dst = SCM_${VECTTYPE}(Scm_Make${vecttype}(SCM_${VECTTYPE}_SIZE(v0), init));
  SCM_RETURN(Scm_${vecttype}Op(dst, v0, v1, SCM_UVECTOR_XOR, clamp_arg(clamp)));")
EOF
    else
    cat <<EOF
;; flonum only operations
(define-cproc ${vecttag}vector-div! (v0::<${vecttag}vector> v1 &optional clamp)
  "SCM_RETURN(Scm_${vecttype}Op(v0, v0, v1, SCM_UVECTOR_DIV, clamp_arg(clamp)));")

(define-cproc ${vecttag}vector-div (v0::<${vecttag}vector> v1 &optional clamp)
  " Scm${vecttype} *dst = SCM_${VECTTYPE}(Scm_Make${vecttype}(SCM_${VECTTYPE}_SIZE(v0), 0));
  SCM_RETURN(Scm_${vecttype}Op(dst, v0, v1, SCM_UVECTOR_DIV, clamp_arg(clamp)));")

EOF
    fi
}

emit s8 1
emit u8 1
emit s16 1
emit u16 1
emit s32 1
emit u32 1
emit s64 1
emit u64 1
emit f32
emit f64

##==============================================================
## String conversions
##
strlib() {
    z=$1
    Z=`echo $z | tr '[a-z]' '[A-Z]'`
cat <<EOF
;;
;; Additional Operations
;;

(define-cproc string->${z}8vector (s::<string>
                                   &optional (start::<fixnum> 0)
                                             (end::<fixnum> -1))
  "int len = SCM_STRING_LENGTH(s); const char *sp, *ep;
  SCM_CHECK_START_END(start, end, len);
  if (start == 0) sp = SCM_STRING_START(s);
  else sp = Scm_StringPosition(s, start);
  if (end == len) ep = SCM_STRING_START(s)+SCM_STRING_SIZE(s);
  else ep = Scm_StringPosition(s, end);
  SCM_RETURN(Scm_Make${Z}8VectorFromArray(ep - sp, (${Z}8ELTTYPE*)sp));
  ")

(define-cproc ${z}8vector->string (v::<${z}8vector>
                                   &optional (start::<fixnum> 0)
                                             (end::<fixnum> -1))
  "int len = SCM_${Z}8VECTOR_SIZE(v);
  SCM_CHECK_START_END(start, end, len);
  SCM_RETURN(Scm_MakeString((char *)(SCM_${Z}8VECTOR_ELEMENTS(v)+start),
                            end-start, -1, SCM_MAKSTR_COPYING));")


(define-cproc string->${z}32vector (s::<string>
                                    &optional (start::<fixnum> 0)
                                              (end::<fixnum> -1))
  "int len = SCM_STRING_LENGTH(s), i; const char *sp, *ep;
  ScmObj v; ${Z}32ELTTYPE *eltp;
  SCM_CHECK_START_END(start, end, len);
  if (start == 0) sp = SCM_STRING_START(s);
  else sp = Scm_StringPosition(s, start);
  if (end == len) ep = SCM_STRING_START(s)+SCM_STRING_SIZE(s);
  else ep = Scm_StringPosition(s, end);
  v = Scm_Make${Z}32Vector(end - start, 0);
  eltp = SCM_${Z}32VECTOR_ELEMENTS(v);
  for (i=0; sp<ep; i++) {
    ScmChar ch;
    SCM_CHAR_GET(sp, ch);
    eltp[i] = ch;
    sp += SCM_CHAR_NBYTES(ch);
  }
  SCM_RETURN(v);")

(define-cproc ${z}32vector->string (v::<${z}32vector>
                                    &optional (start::<fixnum> 0)
                                              (end::<fixnum> -1))
  "int len = SCM_${Z}32VECTOR_SIZE(v);
  ScmObj s = Scm_MakeOutputStringPort(FALSE);
  ${Z}32ELTTYPE *eltp;
  SCM_CHECK_START_END(start, end, len);
  eltp = SCM_${Z}32VECTOR_ELEMENTS(v);
  for (; start < end; start++) {
    ScmChar ch = (ScmChar)eltp[start];
    Scm_PutcUnsafe(ch, SCM_PORT(s));
  }
  SCM_RETURN(Scm_GetOutputStringUnsafe(SCM_PORT(s)));")

EOF
}

strlib u
strlib s

##==============================================================
## Sharing
##

cat <<EOF
(define-cproc uvector-alias (klass v::<uvector>
                             &optional (start::<fixnum> 0)
                                       (end::<fixnum> -1))
  "int len = SCM_UVECTOR_SIZE(v), reqalign, srcalign, dstsize;
  if (!Scm_TypeP(klass, SCM_CLASS_CLASS)) {
    Scm_Error(\"class required, but got %S\", klass);
  }
  reqalign = Scm_UVectorElementSize(SCM_CLASS(klass));
  srcalign = Scm_UVectorElementSize(Scm_ClassOf(SCM_OBJ(v)));
  SCM_CHECK_START_END(start, end, len);
  if (reqalign < 0) {
    Scm_Error(\"uvector-alias requires uniform vector class, but got %S\", klass);
  }
  if ((start*srcalign)%reqalign != 0 || (end*srcalign)%reqalign != 0) {
    Scm_Error(\"aliasing %S of range (%d, %d) to %S doesn't satisfy alignemnt requirement.\", Scm_ClassOf(SCM_OBJ(v)), start, end, klass);
  }
  if (reqalign >= srcalign) dstsize = (end-start) / (reqalign/srcalign);
  else dstsize = (end-start) * (srcalign/reqalign);
  SCM_RETURN(Scm_MakeUVectorFull(SCM_CLASS(klass),
                                 dstsize,
                                 (char*)v->elements + start*srcalign,
                                 SCM_UVECTOR_IMMUTABLE_P(v),
                                 SCM_UVECTOR_OWNER(v)));
  ")
EOF

##==============================================================
## Input and output
##

cat <<EOF
(define-cproc read-block! (v::<uvector>
                           &optional (port::<input-port> (current-input-port))
                                     (start::<fixnum> 0)
                                     (end::<fixnum> -1))
  "int len = SCM_UVECTOR_SIZE(v), eltsize, r;
  SCM_CHECK_START_END(start, end, len);
  SCM_UVECTOR_CHECK_MUTABLE(v);
  eltsize = Scm_UVectorElementSize(Scm_ClassOf(SCM_OBJ(v)));
  SCM_ASSERT(eltsize >= 1);
  r = Scm_Getz((char*)v->elements + start*eltsize, (end-start)*eltsize, port);
  if (r == EOF) SCM_RETURN(SCM_EOF);
  else SCM_RETURN(Scm_MakeInteger((r+eltsize-1)/eltsize));")

(define-cproc write-block (v::<uvector>
                           &optional (port::<output-port> (current-output-port))
                                     (start::<fixnum> 0)
                                     (end::<fixnum> -1))
  "int len = SCM_UVECTOR_SIZE(v), eltsize;
  SCM_CHECK_START_END(start, end, len);
  eltsize = Scm_UVectorElementSize(Scm_ClassOf(SCM_OBJ(v)));
  SCM_ASSERT(eltsize >= 1);
  Scm_Putz((char*)v->elements + start*eltsize, (end-start)*eltsize, port);
  SCM_RETURN(SCM_UNDEFINED);")
  
EOF
