cat << EOF
;;;
;;; uvlib.stub - uniform vector library
;;;
;;;   Copyright (c) 2000-2001 Shiro Kawai (shiro@acm.org)
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
;;; \$Id: uvlib.stub.sh,v 1.6 2002-06-16 01:46:43 shirok Exp $
;;;

"
#include \\"gauche/uvector.h\\"
#include \\"uvectorP.h\\"
"

EOF

emit() {
    vecttag=$1
    vecttype=$2
    itemtype=$3
    VECTTYPE=`echo $vecttype | tr '[a-z]' '[A-Z]'`
    cat <<EOF
;;--------------------------------------------------------------------
;; $vecttype
;;
(define-type "${vecttag}vector" ${vecttag}vector?
  "Scm${vecttype}*" "SCM_${VECTTYPE}P" "SCM_${VECTTYPE}")

(define-cproc ${vecttag}vector? (obj)
  "SCM_RETURN(SCM_MAKE_BOOL(SCM_${VECTTYPE}P(obj)));")

(define-cproc make-${vecttag}vector (length &optional (fill 0))
  (assert (small-integer? length))
  "  ${itemtype} filler;
  SCM_${VECTTYPE}_UNBOX(filler, fill);
  SCM_RETURN(Scm_Make${vecttype}(length, filler));")

(define-cproc ${vecttag}vector (&rest args)
  "  SCM_RETURN(Scm_ListTo${vecttype}(args));")

(define-cproc ${vecttag}vector-length (v)
  (assert (${vecttag}vector? v))
  "  SCM_RETURN(SCM_MAKE_INT(SCM_${VECTTYPE}_SIZE(v)));")

(define-cproc ${vecttag}vector-ref (v i)
  (assert (${vecttag}vector? v))
  (assert (small-integer? i))
  (setter ${vecttag}vector-set!)
  "  SCM_RETURN(Scm_${vecttype}Ref(v, i));")

(define-cproc ${vecttag}vector-set! (v i val)
  (assert (${vecttag}vector? v))
  (assert (small-integer? i))
  "  SCM_RETURN(Scm_${vecttype}Set(v, i, val));")

(define-cproc ${vecttag}vector-copy (v)
  (assert (${vecttag}vector? v))
  "  SCM_RETURN(Scm_${vecttype}Copy(v));")

(define-cproc ${vecttag}vector-copy! (dst src)
  (assert (${vecttag}vector? dst))
  (assert (${vecttag}vector? src))
  "  SCM_RETURN(Scm_${vecttype}CopyX(dst, src));")

(define-cproc ${vecttag}vector->list (v)
  (assert (${vecttag}vector? v))
  "  SCM_RETURN(Scm_${vecttype}ToList(v));")

(define-cproc list->${vecttag}vector (l)
  (assert (list? l))
  "  SCM_RETURN(Scm_ListTo${vecttype}(l));")

(define-cproc ${vecttag}vector-fill! (v val &optional (start 0) (end -1))
  (assert (${vecttag}vector? v))
  (assert (small-integer? start))
  (assert (small-integer? end))
  "  ${itemtype} filler;
  SCM_${VECTTYPE}_UNBOX(filler, val);
  SCM_RETURN(Scm_${vecttype}Fill(v, filler, start, end));")

(define-cproc ${vecttag}vector->vector (v)
  (assert (${vecttag}vector? v))
  "  SCM_RETURN(Scm_${vecttype}ToVector(v));")

(define-cproc vector->${vecttag}vector (v)
  (assert (vector? v))
  "  SCM_RETURN(Scm_VectorTo${vecttype}(v));")

EOF
}

emit s8  S8Vector  "char"
emit u8  U8Vector  "unsigned char"
emit s16 S16Vector "short"
emit u16 U16Vector "unsigned short"
emit s32 S32Vector SCM_UVECTOR_INT32
emit u32 U32Vector SCM_UVECTOR_UINT32
emit s64 S64Vector SCM_UVECTOR_INT64
emit u64 U64Vector SCM_UVECTOR_UINT64
emit f32 F32Vector "float"
emit f64 F64Vector "double"

