#! /bin/sh
#
#  Generate uvutil.c
#

# prologue -----------------------------------------------------------
cat <<EOF
/*
 * uvutil - additional uniform vector utilities
 *
 *  Copyright(C) 2002 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, distribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  \$Id: uvutil.c.sh,v 1.1 2002-06-17 05:48:41 shirok Exp $
 */

#include <stdlib.h>
#include <math.h>
#include <limits.h>
#include <string.h>  /* for memcpy() */
#include <gauche.h>
#include <gauche/extend.h>
#include "gauche/uvector.h"
#include "uvectorP.h"

#define SIZECHK(d, a, b)                                        \\
  do {                                                          \\
    if ((a)->size != (b)->size) {                               \\
      Scm_Error("Vector size doesn't match: %S and %S", a, b);  \\
    }                                                           \\
    SCM_ASSERT((a)->size == (d)->size);                         \\
  } while (0)

EOF

# Template ------------------------------------------------------------
optempl() {
    vecttype=$1
    itemtype=$2
    opname=$3
    calctype=$4
    VECTTYPE=`echo $vecttype | tr '[a-z]' '[A-Z]'`

    cat <<EOF
ScmObj Scm_${vecttype}${opname}(Scm${vecttype} *dst,
                                Scm${vecttype} *v0,
                                ScmObj operand,
                                int clamp)
{
    int i, size = v0->size;
    ${calctype} k;
    if (SCM_${VECTTYPE}P(operand)) {
        Scm${vecttype} *v1 = SCM_${VECTTYPE}(operand);
        SIZECHK(dst, v0, v1);
        for (i=0; i<size; i++) {
            CALC(k, v0->elements[i], v1->elements[i]);
            if (OVERFLOW(k, v0->elements[i], v1->elements[i])) {
                if (!clamp) Scm_Error("uniform vector operation overflow");
                CLAMP(k);
            }
            dst->elements[i] = (${itemtype})k;
        }
    } else {
        ${itemtype} e1;
        SCM_ASSERT(dst->size == v0->size);
        SCM_${VECTTYPE}_UNBOX(e1, operand);
        for (i=0; i<size; i++) {
            CALC(k, v0->elements[i], e1);
            if (OVERFLOW(k, v0->elements[i], e1)) {
                if (!clamp) Scm_Error("uniform vector operation overflow");
                CLAMP(k);
            }
            dst->elements[i] = (${itemtype})k;
        }
    }
    return SCM_OBJ(dst);
}
EOF
}

# Common part for [US]{8,16}Vector
smalltmpl() {
    vecttype=$1
    itemtype=$2

    echo "#define CALC(k, e0, e1) ((k) = (e0) + (e1))"
    optempl ${vecttype} "${itemtype}" Add int
    echo "#undef CALC"

    echo "#define CALC(k, e0, e1) ((k) = (e0) - (e1))"
    optempl ${vecttype} "${itemtype}" Sub int
    echo "#undef CALC"

    echo "#define CALC(k, e0, e1) ((k) = (e0) * (e1))"
    optempl ${vecttype} "${itemtype}" Mul int
    echo "#undef CALC"

    echo "#define CALC(k, e0, e1) ((k) = (e0) / (e1))"
    optempl ${vecttype} "${itemtype}" Div int
    echo "#undef CALC"

    echo "#define CALC(k, e0, e1) ((k) = (e0) % (e1))"
    optempl ${vecttype} "${itemtype}" Mod int
    echo "#undef CALC"

    echo "#define CALC(k, e0, e1) ((k) = (e0) & (e1))"
    optempl ${vecttype} "${itemtype}" And int
    echo "#undef CALC"

    echo "#define CALC(k, e0, e1) ((k) = (e0) | (e1))"
    optempl ${vecttype} "${itemtype}" Ior int
    echo "#undef CALC"

    echo "#define CALC(k, e0, e1) ((k) = (e0) ^ (e1))"
    optempl ${vecttype} "${itemtype}" Xor int
    echo "#undef CALC"
}

# S8Vector -----------------------------------------------------------

cat <<EOF
#define OVERFLOW(k, e0, e1) ((k) < -128 || (k) > 127)
#define CLAMP(k) \\
     if ((k) < -128) k = -128; \\
     else            k = 127
EOF

smalltmpl S8Vector "signed char"

cat <<EOF
#undef OVERFLOW
#undef CLAMP
EOF

# U8Vector -----------------------------------------------------------

cat <<EOF
#define OVERFLOW(k, e0, e1) ((k) < 0 || (k) > 255)
#define CLAMP(k) \\
     if ((k) < 0) k = 0; \\
     else         k = 255
EOF

smalltmpl U8Vector "unsigned char"

cat <<EOF
#undef OVERFLOW
#undef CLAMP
EOF

# S16Vector ----------------------------------------------------------

cat <<EOF
#define OVERFLOW(k, e0, e1) ((k) < -32768 || (k) > 32767)
#define CLAMP(k) \\
     if ((k) < -32768) k = -32768; \\
     else              k = 32767
EOF

smalltmpl S16Vector "signed short"

cat <<EOF
#undef OVERFLOW
#undef CLAMP
EOF

# U16Vector ----------------------------------------------------------

cat <<EOF
#define OVERFLOW(k, e0, e1) ((k) < 0 || (k) > 65535)
#define CLAMP(k) \\
     if ((k) < 0) k = 0; \\
     else         k = 65535
EOF

smalltmpl U16Vector "unsigned short"

cat <<EOF
#undef OVERFLOW
#undef CLAMP
EOF



