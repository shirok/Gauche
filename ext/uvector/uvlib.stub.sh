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
;;; \$Id: uvlib.stub.sh,v 1.1 2001-04-15 08:16:52 shiro Exp $
;;;

"
#include \\"uvector.h\\"
"

EOF

emit() {
    vecttag=$1
    vecttype=$2
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
  (assert (small-integer? fill))
  "SCM_RETURN(Scm_Make${vecttype}(length, fill));")

(define-cproc ${vecttag}vector (&rest args)
  "  SCM_RETURN(Scm_ListTo${vecttype}(args));")

(define-cproc ${vecttag}vector-length (v)
  (assert (${vecttag}vector? v))
  "  SCM_RETURN(SCM_MAKE_INT(SCM_${VECTTYPE}_LENGTH(v)));")

(define-cproc ${vecttag}vector-ref (v i)
  (assert (${vecttag}vector? v))
  (assert (small-integer? i))
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

(define-cproc ${vecttag}vector-fill! (v val)
  (assert (${vecttag}vector? v))
  (assert (small-integer? val))
  "  SCM_RETURN(Scm_${vecttype}Fill(v, val));")
EOF
}

emit s8  S8Vector
emit u8  U8Vector
emit s16 S16Vector
emit u16 U16Vector
emit s32 S32Vector
emit u32 U32Vector
emit f32 F32Vector
emit f64 F64Vector
