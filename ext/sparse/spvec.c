/*
 * spvec.c - Sparse vector
 *
 *   Copyright (c) 2009  Shiro Kawai  <shiro@acm.org>
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

#include "spvec.h"

/*===================================================================
 * Leaf node manipulation
 */

typedef struct SPVLeafRec {
    Leaf hdr;
    u_long ebits;               /* 1 if element exists */
    void*  elements;            /* variable length element vector */
} SPVLeaf;

static inline int leaf_has_elem(SPVLeaf *leaf, int index)
{
    return SCM_BITS_TEST_IN_WORD(leaf->ebits, index);
}

static inline u_long leaf_offset(SPVLeaf *leaf, int index)
{
    return (u_long)Scm__CountBitsBelow(leaf->ebits, index);
}

static Leaf *leaf_allocate(void *data)
{
    SPVLeaf *z = SCM_NEW(SPVLeaf); /* NB: ebits = 0 */
    z->elements = NULL;
    return (Leaf*)z;
}

static void leaf_insert(SparseVector *sv, SPVLeaf *leaf,
                        int index, ScmObj value)
{
    if (leaf_has_elem(leaf, index)) {
        sv->desc->store(leaf->elements, leaf_offset(leaf, index), value);
    } else {
        u_long origsize = Scm__CountBitsInWord(leaf->ebits);
        u_long insertion = leaf_offset(leaf, index);
        leaf->elements = sv->desc->extend(leaf->elements, origsize, insertion);
        SCM_BITS_SET_IN_WORD(leaf->ebits, index);
        sv->desc->store(leaf->elements, insertion, value);
        sv->numEntries++;
    }
}

/*===================================================================
 * Generic stuff
 */

ScmObj MakeSparseVectorGeneric(ScmClass *klass,
                               int chunkBits,
                               int trieBits,
                               SparseVectorDescriptor *desc)
{
    SparseVector *v = SCM_NEW(SparseVector);
    SCM_SET_CLASS(v, klass);
    CompactTrieInit(&v->trie);
    v->numEntries = 0;
    v->chunkBits = chunkBits;
    v->trieBits = trieBits;
    v->desc = desc;
    return SCM_OBJ(v);
}

ScmObj SparseVectorRef(SparseVector *sv, u_long index, ScmObj fallback)
{
    u_long triekey = index & ~((1UL<<sv->chunkBits)-1);
    u_long chunkkey = index & ((1UL<<sv->chunkBits)-1);
    SPVLeaf *z = (SPVLeaf*)CompactTrieGet(&sv->trie, triekey);
    if (z != NULL && leaf_has_elem(z, chunkkey)) {
        ScmObj v = sv->desc->retrieve(z->elements, leaf_offset(z, chunkkey));
        SCM_ASSERT(v != NULL);
        return v;
    }
    if (SCM_UNBOUNDP(fallback)) {
        Scm_Error("%s-ref: no value at index %lu", sv->desc->name, index);
    }
    return fallback;
}

void SparseVectorSet(SparseVector *sv, u_long index, ScmObj value)
{
    u_long triekey = index & ~((1UL<<sv->chunkBits)-1);
    u_long chunkkey = index & ((1UL<<sv->chunkBits)-1);
    SPVLeaf *z;

    if (!sv->desc->check(value)) {
        Scm_Error("%s-set!: element value out of range: %S",
                  sv->desc->name, value);
    }
    z = (SPVLeaf*)CompactTrieAdd(&sv->trie, triekey, leaf_allocate, NULL);
    leaf_insert(sv, z, chunkkey, value);
}

static void sparse_clear(Leaf *f, void *data)
{
    SPVLeaf *z = (SPVLeaf*)f;
    SparseVectorDescriptor *desc = (SparseVectorDescriptor*)data;

    int size = Scm__CountBitsInWord(z->ebits);
    if (size&1) size++;
    if (!desc->elementAtomic) {
        memset(z->elements, 0, desc->elementSize * size);
    }
}

void SparseVectorClear(SparseVector *sv)
{
    sv->numEntries = 0;
    CompactTrieClear(&sv->trie, sparse_clear, sv->desc);
}

#if SCM_DEBUG_HELPER
void SparseVectorDump(SparseVector *sv)
{
    CompactTrieDump(SCM_CUROUT, &sv->trie, sv->desc->dump, sv->desc);
}
#endif /*SCM_DEBUG_HELPER*/

/*===================================================================
 * Individual types
 */

/* general vector */
static int g_check(ScmObj value)
{
    return TRUE;
}

static ScmObj g_retrieve(void *elements, u_long offset)
{
    return ((ScmObj*)elements)[offset];
}

static void g_store(void *elements, u_long offset, ScmObj value)
{
    ((ScmObj*)elements)[offset] = value;
}

/* We realloc for every G_INCR words.  Must be a power of 2. */
#define G_INCR 2

static void *g_extend(void *elements, int origsize, int insertion)
{
    int i;
    SCM_ASSERT(insertion <= origsize);
    if (elements == NULL) {
        ScmObj *newchunk = SCM_NEW_ARRAY(ScmObj, G_INCR);
#if G_INCR == 2
        newchunk[0] = newchunk[1] = SCM_UNDEFINED;
#elif G_INCR == 4
        newchunk[0] = newchunk[1] = newchunk[2] = newchunk[3] = SCM_UNDEFINED;
#else
        for (i=0; i<G_INCR; i++) newchunk[i] = SCM_UNDEFINED;
#endif
        return newchunk;
    } else if (origsize & (G_INCR-1)) {
        /* we have room for this elements*/
        for (i=origsize; i>insertion; i--) {
            ((ScmObj*)elements)[i] = ((ScmObj*)elements)[i-1];
        }
        return elements;
    } else {
        ScmObj *newchunk = SCM_NEW_ARRAY(ScmObj, origsize+G_INCR);
        for (i=0; i<insertion; i++) newchunk[i] = ((ScmObj*)elements)[i];
        newchunk[i++] = SCM_UNDEFINED;
        for (; i<=origsize; i++) newchunk[i] = ((ScmObj*)elements)[i-1];
        newchunk[i] = SCM_UNDEFINED;

        memset(elements, 0, sizeof(ScmObj)*origsize); /* gc friendly */

        return (void*)newchunk;
    }
}

#if SCM_DEBUG_HELPER
static void g_dump(ScmPort *out, Leaf *leaf, int indent, void *data)
{
    int i;
    SPVLeaf *z = (SPVLeaf *)leaf;

    Scm_Printf(out, "nelts=%d", Scm__CountBitsInWord(z->ebits));
    for (i=0; i<(1UL<<MAX_CHUNK_BITS); i++) {
        if (leaf_has_elem(z, i)) {
            Scm_Printf(out, "\n  %*s%2d: %25.1S", indent, "", i,
                    ((ScmObj*)z->elements)[leaf_offset(z, i)]);
        }
    }
}
#else
#define g_dump NULL
#endif /*SCM_DEBUG_HELPER*/

static SparseVectorDescriptor g_desc = {
    g_check, g_retrieve, g_store, g_extend, g_dump,
    "spvector",
    FALSE,                      /* elementAtomic */
    sizeof(ScmObj),             /* elementSize */
};

ScmObj MakeSparseVector(u_long flags)
{
    return MakeSparseVectorGeneric(SCM_CLASS_SPARSE_VECTOR,
                                   MAX_CHUNK_BITS, MAX_TRIE_BITS,
                                   &g_desc);
}

SCM_DEFINE_BUILTIN_CLASS(Scm_SparseVectorClass, NULL, NULL, NULL, NULL, NULL);



/*===================================================================
 * Initialization
 */

void Scm_Init_spvec(ScmModule *mod)
{
    Scm_InitStaticClass(&Scm_SparseVectorClass, "<sparse-vector>",
                        mod, NULL, 0);
}


