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
 * Generic stuff
 */

ScmObj MakeSparseVectorGeneric(ScmClass *klass,
                               SparseVectorDescriptor *desc)
{
    SparseVector *v = SCM_NEW(SparseVector);
    SCM_SET_CLASS(v, klass);
    CompactTrieInit(&v->trie);
    v->numEntries = 0;
    v->desc = desc;
    return SCM_OBJ(v);
}

ScmObj SparseVectorRef(SparseVector *sv, u_long index, ScmObj fallback)
{
    ScmObj v = sv->desc->ref(sv, index);
    if (SCM_UNBOUNDP(v)) return fallback;
    else return v;
}

void SparseVectorSet(SparseVector *sv, u_long index, ScmObj value)
{
    /* set returns TRUE if this is new entry */
    if (sv->desc->set(sv, index, value)) sv->numEntries++;
}

/* returns value of the deleted entry, or SCM_UNBOUND if there's no entry */
ScmObj SparseVectorDelete(SparseVector *sv, u_long index)
{ 
    ScmObj r = sv->desc->delete(sv, index);
    if (!SCM_UNBOUNDP(r)) sv->numEntries--;
    return r;
}

void SparseVectorClear(SparseVector *sv)
{
    sv->numEntries = 0;
    CompactTrieClear(&sv->trie, sv->desc->clear, sv->desc);
}

#if SCM_DEBUG_HELPER
void SparseVectorDump(SparseVector *sv)
{
    CompactTrieDump(SCM_CUROUT, &sv->trie, sv->desc->dump, sv->desc);
}
#endif /*SCM_DEBUG_HELPER*/

/*===================================================================
 * Individual vector types
 */

/*-------------------------------------------------------------------
 * General vector
 */

typedef struct GLeafRec {
    Leaf hdr;
    ScmObj val[2];
} GLeaf;

static ScmObj g_ref(SparseVector *sv, u_long index)
{
    GLeaf *z = (GLeaf*)CompactTrieGet(&sv->trie, index>>1);
    if (z == NULL) return SCM_UNBOUND;
    else return z->val[index&1];
}

static Leaf *g_allocate(void *data)
{
    GLeaf *z = SCM_NEW(GLeaf);
    z->val[0] = z->val[1] = SCM_UNBOUND;
    return (Leaf*)z;
}

static int g_set(SparseVector *sv, u_long index, ScmObj value)
{
    GLeaf *z = (GLeaf*)CompactTrieAdd(&sv->trie, index>>1, g_allocate, NULL);
    ScmObj v = z->val[index&1];
    z->val[index&1] = value;
    return SCM_UNBOUNDP(v);
}

static ScmObj g_delete(SparseVector *sv, u_long index)
{
    ScmObj v;
    GLeaf *z = (GLeaf*)CompactTrieGet(&sv->trie, index>>1);
    if (z == NULL) return SCM_UNBOUND;
    v = z->val[index&1];
    z->val[index&1] = SCM_UNBOUND;
    return v;
}

static void g_clear(Leaf *leaf, void *data)
{
    GLeaf *z = (GLeaf*)leaf;
    z->val[0] = z->val[1] = NULL;
}

#if SCM_DEBUG_HELPER
static void g_dump(ScmPort *out, Leaf *leaf, int indent, void *data)
{
    int i;
    GLeaf *z = (GLeaf*)leaf;
    for (i=0; i<2; i++) {
        if (!SCM_UNBOUNDP(z->val[i])) {
            Scm_Printf(out, "\n  %*s%2d: %25.1S", indent, "", i, z->val[i]);
        }
    }
}
#else
#define g_dump NULL
#endif /*SCM_DEBUG_HELPER*/

static SparseVectorDescriptor g_desc = {
    g_ref, g_set, g_delete, g_clear, g_dump,
    "sparse-vector",
};

ScmObj MakeSparseVector(u_long flags)
{
    return MakeSparseVectorGeneric(SCM_CLASS_SPARSE_VECTOR, &g_desc);
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


