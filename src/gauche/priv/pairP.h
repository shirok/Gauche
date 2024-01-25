/*
 * pairP.h - Pair API private header
 *
 *   Copyright (c) 2020-2024  Shiro Kawai  <shiro@acm.org>
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   3. Neither the name of the authors nor the names of its contributors
 *      may be used to endorse or promote products derived from this
 *      software without specific prior written permission.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef GAUCHE_PRIV_PAIRP_H
#define GAUCHE_PRIV_PAIRP_H

/* ScmExtendedPair actually has a hidden tagged pointer before it.  The
 * user won't see it.
 *
 * The tagged pointer is the same as usual class tag ('111' lower bits) but
 * point to ScmExtendedPairDescriptor instead of ScmClass.
 *
 * Read access to car and cdr is simply a single pointer reference, with no
 * overhead.  Mutations on an extended pair, OTOH, are intercepted by setCar
 * and setCdr procedure of ScmExtendedPairDescriptor, which can do tricks.
 *
 * We distinguish a normal pair and an extended pair by looking at the address.
 * Both normal pairs and ScmRealExtendedPairs are aligned on even word boundary.
 * That makes ScmExtendedPair to be on odd word boundary.
 *
 * Statically allocated pairs are a bit tricky; if the compiler has a way
 * to specify alignment, we use it.  Otherwise, we need to do extra check
 * that looks at the previous word when a pointer to a pair falls on
 * on an odd-word---the previous word of normal pair will never have the
 * low-bit tag 111.
 * (It is safe to do so---all heap allocated objects are on even-word boundary,
 * so only the statically allocated pairs matter.  And we generate a dummy
 * pair at the beginning of every static ScmPair array.)
 *
 * The reason we don't use the class pointer in the first word is that
 * we don't want to split classes for mutable and immutable pairs.  We'd rather
 * see mutability as 'const' qualifier of C, rather than different types.
 * One reason is that existing code expects <pair> for both (class-of '(a . b))
 * and (class-of (cons 'a 'b)) --- we could've subclass <pair> for mutable
 * and immutable pairs, but that'll break existing code which tests equality
 * of class, rather than is-a relationship.
 */
typedef struct ScmExtendedPairDescriptorRec {
    ScmClass *klass;
    u_long flags;
    void (*setCar)(ScmObj, ScmObj);
    void (*setCdr)(ScmObj, ScmObj);
} ScmExtendedPairDescriptor;

/*

   ScmRealExtendedPair --> +--------------+  masked lower 3 bits
                           |   desc + 7   | -------> ScmExtendedPairDescriptor
   ScmObj ---------------> +--------------+
                           |     car      |
                           +--------------+
                           |     cdr      |
                           +--------------+
                           |    attrs     |
                           +--------------+

 */

typedef struct ScmRealExtendedPairRec {
    ScmWord hiddenTag;
    ScmExtendedPair data;
} ScmRealExtendedPair;

/* Pair flags */
enum {
    SCM_PAIR_IMMUTABLE = (1L<<0)
};

SCM_EXTERN ScmExtendedPairDescriptor *Scm__GetExtendedPairDescriptor(ScmObj);
SCM_EXTERN ScmExtendedPairDescriptor *Scm__GetDefaultExtendedPairDesctiptor(void);
SCM_EXTERN void Scm__InitIPairClass(ScmClass *klass);

#endif /*GAUCHE_PRIV_PAIRP_H*/
