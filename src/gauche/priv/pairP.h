/*
 * pairP.h - Pair API private header
 *
 *   Copyright (c) 2020  Shiro Kawai  <shiro@acm.org>
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

/* ScmExtendedPair actually has a hidden pointer to a class before it.  The
 * user won't see it.
 *
 * The class must be <pair> or its subclass, and may have
 * ScmExtendedPairDescriptor in its data field.
 *
 * Read access to car and cdr is simply a single pointer reference, with no
 * overhead.  Mutations on an extended pair, otoh, are intercepted by setCar
 * and setCdr procedure of ScmExtendedPairDescriptor if its class has one. 
 * It can perform tricks.
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
 */
typedef struct ScmExtendedPairDescriptorRec {
    ScmClass *klass;
    void (*setCar)(ScmExtendedPair*);
    void (*setCdr)(ScmExtendedPair*);
} ScmExtendedPairDescriptor;

typedef struct ScmRealExtendedPairRec {
    ScmClass *klass;
    ScmExtendedPair data;
} ScmRealExtendedPair;

#endif /*GAUCHE_PRIV_PAIRP_H*/
