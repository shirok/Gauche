/*
 * ctrie.h - Compact Trie
 *
 *   Copyright (c) 2009-2013  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_CTRIE_H
#define GAUCHE_CTRIE_H

#include <gauche.h>
#include <gauche/extend.h>
#include <gauche/bits_inline.h>

/* CompactTrie is a structure to store data indexed by full 32bit integer.
 * It can be used as a space-efficient vector and as a back-end of sparse
 * hash table.  Compact Trie itself is *not* an ScmObj.
 *
 * CompactTrie is not intended to be used outside of ext/sparse source tree,
 * hence we don't use 'Scm' prefix for data structures, for simplicity.
 */

/* CompactTrie consists of NODEs and LEAFs.  LEAF is application-dependent
 * structure whose header contains the key value that uniquely identifies
 * the leaf.
 * NODE is a variable length structure, whose first two words are bitmaps.
 * (NB: we may change the layout here, since full-word bitmap tends to become
 * false pointer and may have negative impact to our conservative GC.)
 *
 * NODE represents 32-way branch.  The first bitmap, EMAP or entry map,
 * shows which logical index of this node is active.  The second bitmap,
 * LMAP, shows which entry of logical index of this node is LEAF.
 * LMAP is always a strict subset of EMAP.   If bit N of EMAP is 1 and
 * bit N of LMAP is 0, the N-th index points to the child node.
 *
 * Suppose EMAP is 0x00001809 and LMAP is 0x00001008.   It means this
 * node has entries in index 0, 3, 11, 12.  Entry #0 and #11 points to
 * child NODEs, and entry #3 and #12 are LEAFs.
 *
 * The actual entries follow the bitmaps in compact format.  For the above
 * example, four intptr_t follows, corresponding to entry #0, #3, #11 and #12,
 * respectively.
 */

#define MAX_NODE_SIZE 32
#define TRIE_SHIFT    5
#define TRIE_MASK     (0x1f)

typedef struct NodeRec {
    u_long   emap;              /* bitmap: 1 = has child */
    u_long   lmap;              /* bitmap: 1 = child is leaf */
    void    *entries[2];        /* variable length; 2 is the minimum entries */
} Node;

/* We split key into two words; a well distributed keys are hard to
   distinguish from pointers by our conserative GC, and sometimes lead
   to poor GC performance when we have very large table.  */
typedef struct LeafRec {
    u_long   key0;              /* lower half word of the key + subclass data */
    u_long   key1;              /* upper half word of the key */
} Leaf;

#define LEAF_KEY(leaf) (((leaf)->key0&0xffff) + (((leaf)->key1&0xffff) << 16))

#define LEAF_DATA(leaf) ((leaf)->key0 >> 16)
#define LEAF_DATA_SET(leaf, val) \
    (((leaf)->key0) = (((leaf)->key0)&0x0ffff) | ((val)<<16))
#define LEAF_DATA_BIT_TEST(leaf, bit) \
    (((leaf)->key0) & (1UL << ((bit)+16)))
#define LEAF_DATA_BIT_SET(leaf, bit) \
    (((leaf)->key0) |= (1UL << ((bit)+16)))
#define LEAF_DATA_BIT_RESET(leaf, bit) \
    (((leaf)->key0) &= ~(1UL << ((bit)+16)))

typedef struct CompactTrieRec {
    u_int    numEntries;
    Node     *root;
} CompactTrie;

typedef struct CompactTrieIterRec {
    CompactTrie *trie;
    u_long       key;
    char         begin;
    char         end;
} CompactTrieIter;

/* Create empty CompactTrie */
extern CompactTrie *MakeCompactTrie(void);
extern void CompactTrieInit(CompactTrie *);
extern void CompactTrieClear(CompactTrie *,
                             void (*clearer)(Leaf*, void*),
                             void *data);

/* Search CompactTrie with KEY. */
extern Leaf *CompactTrieGet(CompactTrie *ct, u_long key);
extern Leaf *CompactTrieAdd(CompactTrie *ct, u_long key,
                            Leaf *(*creator)(void*), void *data);
extern Leaf *CompactTrieDelete(CompactTrie *ct, u_long key);
extern void  CompactTrieCopy(CompactTrie *dst,
                             const CompactTrie *src,
                             Leaf *(*copy)(Leaf*, void*), void *data);

extern Leaf *CompactTrieFirstLeaf(CompactTrie *ct);
extern Leaf *CompactTrieLastLeaf(CompactTrie *ct);
extern Leaf *CompactTrieNextLeaf(CompactTrie *ct, u_long key);


/* Iterator */
extern void  CompactTrieIterInit(CompactTrieIter *it, CompactTrie *ct);
extern Leaf *CompactTrieIterNext(CompactTrieIter *it);

/* For debug */
extern void CompactTrieDump(ScmPort *out, const CompactTrie *ct,
                            void (*dumper)(ScmPort *, Leaf*, int, void*),
                            void *data);
extern void CompactTrieCheck(const CompactTrie *ct, ScmObj obj,
                             void (*checker)(Leaf*, ScmObj));

#endif /*GAUCHE_CTRIE_H*/
