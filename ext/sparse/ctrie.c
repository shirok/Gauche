/*
 * ctrie.c - Compact Trie
 *
 *   Copyright (c) 2009-2015  Shiro Kawai  <shiro@acm.org>
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

#include "ctrie.h"

/*
 * Constructor
 */

CompactTrie *MakeCompactTrie(void)
{
    CompactTrie *t = SCM_NEW(CompactTrie);
    CompactTrieInit(t);
    return t;
}

void CompactTrieInit(CompactTrie *t)
{
    t->numEntries = 0;
    t->root = NULL;
}

/*
 * Nodes
 */
#define KEY2INDEX(key, level) (((key)>>((level)*TRIE_SHIFT)) & TRIE_MASK)

#define NODE_HAS_ARC(node, ind)     SCM_BITS_TEST_IN_WORD(node->emap, (ind))
#define NODE_ARC_SET(node, ind)     SCM_BITS_SET_IN_WORD(node->emap, (ind))
#define NODE_ARC_RESET(node, ind)   SCM_BITS_RESET_IN_WORD(node->emap, (ind))
#define NODE_EMPTY_P(node)          (node->emap == NULL)
#define NODE_NCHILDREN(node)        Scm__CountBitsInWord(node->emap)

#define NODE_ARC_IS_LEAF(node, ind) SCM_BITS_TEST_IN_WORD(node->lmap, (ind))
#define NODE_LEAF_SET(node, ind)    SCM_BITS_SET_IN_WORD(node->lmap, (ind))
#define NODE_LEAF_RESET(node, ind)  SCM_BITS_RESET_IN_WORD(node->lmap, (ind))

#define NODE_INDEX2OFF(node, ind)    Scm__CountBitsBelow(node->emap, (ind))

#define NODE_ENTRY(node, off)        ((node)->entries[(off)])

#define KEY_MASK(key) /* empty */

/* When extending the node, we increase the number of entries by this
   number instead of increasing every word, to avoid too frequent
   reallocation.   Must be a power of two. */
#define NODE_SIZE_INCR 2

static Node *make_node(int nentry)
{
    int nalloc = (nentry+NODE_SIZE_INCR-1)&(~(NODE_SIZE_INCR-1));
    /* SCM_NEW2 returns zero cleared chunk. */
    return SCM_NEW2(Node*, sizeof(Node) + sizeof(void*)*(nalloc-2));
}

static Node *node_insert(Node *orig, u_long ind, void *entry, int leafp)
{
    int size = NODE_NCHILDREN(orig);
    int insertpoint = Scm__CountBitsBelow(orig->emap, ind);

    if (size&(NODE_SIZE_INCR-1)) {
        /* we have one more room */
        NODE_ARC_SET(orig, ind);
        if (leafp) NODE_LEAF_SET(orig, ind);
        if (insertpoint < size) {
            for (int i=size-1; i>=insertpoint; i--) {
                orig->entries[i+1] = orig->entries[i];
            }
        }
        orig->entries[insertpoint] = entry;
        return orig;
    } else {
        /* we need to extend the node */
        Node *newn = make_node(size+NODE_SIZE_INCR);
        newn->emap = orig->emap;
        newn->lmap = orig->lmap;
        NODE_ARC_SET(newn, ind);
        if (leafp) NODE_LEAF_SET(newn, ind);
        int i = 0;
        for (; i<insertpoint; i++) newn->entries[i] = orig->entries[i];
        newn->entries[insertpoint] = entry;
        for (; i<size; i++) newn->entries[i+1] = orig->entries[i];
        return newn;
    }
}

/* returns # of children left */
static int node_delete(Node *orig, u_long ind)
{
    int size = NODE_NCHILDREN(orig);
    int deletepoint = Scm__CountBitsBelow(orig->emap, ind);

    /* TODO: shrink node */
    NODE_ARC_RESET(orig, ind);
    NODE_LEAF_RESET(orig, ind);
    for (int i=deletepoint; i<size-1; i++) {
      orig->entries[i] = orig->entries[i+1];
    }
    return size-1;
}

/*
 * Leaves
 */

static Leaf *new_leaf(u_long key, Leaf *(*creator)(void*), void *data)
{
    Leaf *l = creator(data);
    leaf_key_set(l, key);
    return l;
}

/*
 * Search
 */
static Leaf *get_rec(Node *n, u_long key, int level)
{
    u_long ind = KEY2INDEX(key, level);
    if (!NODE_HAS_ARC(n, ind)) return NULL;
    if (NODE_ARC_IS_LEAF(n, ind)) {
        Leaf *l = (Leaf*)NODE_ENTRY(n, NODE_INDEX2OFF(n, ind));
        if (leaf_key(l) == key) return l;
        else return NULL;
    } else {
        return get_rec((Node*)NODE_ENTRY(n, NODE_INDEX2OFF(n, ind)),
                       key, level+1);
    }
}

Leaf *CompactTrieGet(CompactTrie *ct, u_long key)
{
    KEY_MASK(key);
    if (ct->root == NULL) return NULL;
    else return get_rec(ct->root, key, 0);
}

/*
 * Search, and if not found, create
 */
static Node *add_rec(CompactTrie *ct, Node *n, u_long key, int level,
                     Leaf **result, Leaf *(*creator)(void*), void *data)

{
    u_long ind = KEY2INDEX(key, level);

    if (!NODE_HAS_ARC(n, ind)) {
        Leaf *l = new_leaf(key, creator, data);
        *result = l;
        ct->numEntries++;
        return node_insert(n, ind, (void*)l, TRUE);
    }
    else if (!NODE_ARC_IS_LEAF(n, ind)) {
        u_long off = NODE_INDEX2OFF(n, ind);
        Node *orig = (Node*)NODE_ENTRY(n, off);
        Node *m = add_rec(ct, orig, key, level+1, result, creator, data);
        if (m != orig) NODE_ENTRY(n, off) = m;
        return n;
    }
    else {
        u_long off = NODE_INDEX2OFF(n, ind);
        Leaf *l0 = (Leaf*)NODE_ENTRY(n, off);
        u_long k0 = leaf_key(l0);

        if (key == k0) { *result = l0; return n; }
        u_long i0 = KEY2INDEX(leaf_key(l0), level+1);
        Node *m = make_node(NODE_SIZE_INCR);
        NODE_ARC_SET(m, i0);
        NODE_LEAF_SET(m, i0);
        NODE_ENTRY(m, 0) = l0;
        NODE_ENTRY(n, off) = add_rec(ct, m, key, level+1, result, creator, data);
        NODE_LEAF_RESET(n, ind);
        return n;
    }
}

Leaf *CompactTrieAdd(CompactTrie *ct, u_long key,
                     Leaf *(*creator)(void*), void *data)
{
    KEY_MASK(key);
    if (ct->root == NULL) {
        Leaf *l = new_leaf(key, creator, data);
        ct->root = make_node(NODE_SIZE_INCR);
        ct->numEntries = 1;
        NODE_ARC_SET(ct->root, key&TRIE_MASK);
        NODE_LEAF_SET(ct->root, key&TRIE_MASK);
        NODE_ENTRY(ct->root, 0) = l;
        return l;
    } else {
        Leaf *e = NULL;
        Node *p = add_rec(ct, ct->root, key, 0, &e, creator, data);
        if (p != ct->root) ct->root = p;
        return e;
    }
}

/*
 * Delete and clear
 */

/* Usually returns the node N, but if deletion of the leaf made N have
   a leaf as a single child, returns it so that the node itself is
   eliminated.  That is, if the return value != n, the returned pointer
   always points to a leaf.
*/
void *del_rec(CompactTrie *ct, Node *n, u_long key, int level,
              Leaf **deleted_leaf)
{
    u_long ind = KEY2INDEX(key, level);

    if (NODE_HAS_ARC(n, ind)) {
        u_long off = NODE_INDEX2OFF(n, ind);
        if (!NODE_ARC_IS_LEAF(n, ind)) {
            Node *orig = (Node*)NODE_ENTRY(n, off);
            void *m = del_rec(ct, orig, key, level+1, deleted_leaf);
            if (m != (void*)orig) {
                if (NODE_NCHILDREN(n) == 1 && level > 0) return m;
                NODE_ENTRY(n, off) = m;
                NODE_LEAF_SET(n, ind);
            }
        } else {
            Leaf *l0 = (Leaf*)NODE_ENTRY(n, off);
            u_long k0 = leaf_key(l0);
            if (key == k0) {
                /* We found the leaf to delete.  If deletion of the leaf
                   causes this node to have only one leaf, we tell the
                   parent to skip this node.  */
                int nc = node_delete(n, ind);
                *deleted_leaf = l0;
                ct->numEntries--;
                if (nc == 1 && n->lmap != 0 && level > 0) {
                    return NODE_ENTRY(n, 0); /* the only leaf */
                } else if (nc == 0) {
                    /* this only happens when N is root. */
                    SCM_ASSERT(level == 0);
                    return NULL;
                }
            }
        }
    }
    return n;
}

Leaf *CompactTrieDelete(CompactTrie *ct, u_long key)
{
    Leaf *e = NULL;
    KEY_MASK(key);
    if (ct->root == NULL) return NULL;
    ct->root = (Node*)del_rec(ct, ct->root, key, 0, &e);
    return e;
}

/* 'init' can smash all the contents, but if you want to be more GC-friendly,
   this one clears up all the freed chunks. */
static void clear_rec(CompactTrie *ct, Node *n,
                      void (*clearer)(Leaf*, void*),
                      void *data)
{
    int size = Scm__CountBitsInWord(n->emap);
    char is_leaf[MAX_NODE_SIZE];

    for (int i=0, off=0; i<MAX_NODE_SIZE; i++) {
        if (NODE_HAS_ARC(n, i)) {
            if (NODE_ARC_IS_LEAF(n, i)) is_leaf[off++] = TRUE;
            else is_leaf[off++] = FALSE;
        }
    }
    for (int i=0; i<size; i++) {
        if (is_leaf[i]) clearer((Leaf*)NODE_ENTRY(n, i), data);
        else clear_rec(ct, (Node*)NODE_ENTRY(n, i), clearer, data);
        NODE_ENTRY(n, i) = NULL;
    }
    n->emap = n->lmap = 0;
}

void CompactTrieClear(CompactTrie *ct,
                      void (*clearer)(Leaf*, void*),
                      void *data)
{
    Node *n = ct->root;
    ct->numEntries = 0;
    ct->root = NULL;
    if (n) clear_rec(ct, n, clearer, data);
}

/*
 * Key finding
 */
static Leaf *next_rec(Node *n, u_long key, int level, int over)
{
    u_int ind = over? 0 : KEY2INDEX(key, level);

    for (u_int i = ind; i < MAX_NODE_SIZE; i++) {
        if (!NODE_HAS_ARC(n, i)) continue;
        if (NODE_ARC_IS_LEAF(n, i)) {
            if (!over && i == ind) continue;
            return (Leaf*)NODE_ENTRY(n, NODE_INDEX2OFF(n, i));
        } else {
            Leaf *l = next_rec((Node*)NODE_ENTRY(n, NODE_INDEX2OFF(n, i)),
                               key, level+1, (over || (i > ind)));
            if (l) return l;
        }
    }
    return NULL;
}

Leaf *CompactTrieNextLeaf(CompactTrie *ct, u_long key)
{
    KEY_MASK(key);
    if (ct->root) return next_rec(ct->root, key, 0, FALSE);
    else return NULL;
}

/* Find leaves with minimum and maximum key.  Note: minkey==0 or maxkey==0
   situation only occurs when another thread is modifying the same trie.
   We don't guarantee correct operation under the race condition, but
   we don't want SEGV as well, so we just return NULL in such case.
 */
static Leaf *first_rec(Node *n)
{
    int minkey;
    if (n->emap == 0) return NULL; /* for safety in MT situation */
    minkey = Scm__LowestBitNumber(n->emap);
    if (NODE_ARC_IS_LEAF(n, minkey)) {
        return (Leaf*)NODE_ENTRY(n, NODE_INDEX2OFF(n, minkey));
    } else {
        return first_rec((Node*)NODE_ENTRY(n, NODE_INDEX2OFF(n, minkey)));
    }
}

Leaf *CompactTrieFirstLeaf(CompactTrie *ct)
{
    if (ct->root) return first_rec(ct->root);
    else return NULL;
}

static Leaf *last_rec(Node *n)
{
    if (n->emap == 0) return NULL; /* for safety in MT situation */
    int maxkey = Scm__HighestBitNumber(n->emap);
    if (NODE_ARC_IS_LEAF(n, maxkey)) {
        return (Leaf*)NODE_ENTRY(n, NODE_INDEX2OFF(n, maxkey));
    } else {
        return last_rec((Node*)NODE_ENTRY(n, NODE_INDEX2OFF(n, maxkey)));
    }
}

Leaf *CompactTrieLastLeaf(CompactTrie *ct)
{
    if (ct->root) return last_rec(ct->root);
    else return NULL;
}

/*
 * Copy.
 * It is recommended that the caller first clear the dst.  The original
 * tree in dst is detached but otherwise remains intact, and may not be
 * friendly to GC.
 */
static Node *copy_rec(const Node *s, Leaf *(*copy)(Leaf*, void*), void *data)
{
    int size = Scm__CountBitsInWord(s->emap);
    Node *d = make_node(size);
    d->emap = s->emap;
    d->lmap = s->lmap;
    for (int i=0, off=0; i<MAX_NODE_SIZE && off < size; i++) {
        if (!NODE_HAS_ARC(s, i)) continue;
        if (NODE_ARC_IS_LEAF(s, i)) {
            NODE_ENTRY(d,off) = copy((Leaf*)NODE_ENTRY(s,off), data);
        } else {
            NODE_ENTRY(d,off) = copy_rec((Node*)NODE_ENTRY(s,off), copy, data);
        }
        off++;
    }
    return d;
}

void CompactTrieCopy(CompactTrie *dst, const CompactTrie *src,
                     Leaf *(*copy)(Leaf*, void*), void *data)
{
    if (src->root) dst->root = copy_rec(src->root, copy, data);
    else           dst->root = NULL;
    dst->numEntries = src->numEntries;
}

/*
 * Iterator
 */
void CompactTrieIterInit(CompactTrieIter *it, CompactTrie *ct)
{
    it->trie = ct;
    it->key = 0;
    it->begin = TRUE;
    it->end = FALSE;
}

Leaf *CompactTrieIterNext(CompactTrieIter *it)
{
    Leaf *l;
    if (it->end) return NULL;
    if (it->begin) {
        l = CompactTrieFirstLeaf(it->trie);
        it->begin = FALSE;
    } else {
        l = CompactTrieNextLeaf(it->trie, it->key);
    }
    if (l) it->key = leaf_key(l);
    else   it->end = TRUE;
    return l;
}

/*
 * Debug dump
 */
static char digit32(u_int n)
{
    return (n < 10)? (char)(n+'0') : (char)(n-10+'a');
}

#define BUF_SIZE 14

static char *key_dump(u_long key, char *buf) /* buf must be BUF_SIZE length */
{
    buf[BUF_SIZE-1] = '\0';
    for (int i=0; i<BUF_SIZE-1; i++) {
        buf[BUF_SIZE-i-2] = digit32(key&TRIE_MASK);
        key >>= TRIE_SHIFT;
    }
    return buf;
}

static void leaf_dump(ScmPort *out, Leaf *self, int indent,
                      void (*dumper)(ScmPort*, Leaf*, int, void*), void *data)
{
    char keybuf[BUF_SIZE];
    Scm_Printf(out, "LEAF(%s,%x) ", key_dump(leaf_key(self), keybuf),
               leaf_key(self));
    if (dumper) dumper(out, self, indent, data);
    Scm_Printf(out, "\n");
}

static void node_dump(ScmPort *out, Node *n, int level,
                      void (*dumper)(ScmPort*, Leaf*, int, void*), void *data)
{
    Scm_Printf(out, "NODE(%p)\n", n);
    for (int i=0; i<MAX_NODE_SIZE; i++) {
        if (!NODE_HAS_ARC(n, i)) continue;
        Scm_Printf(out, " %*s%c:", level*2, "", digit32(i));
        if (NODE_ARC_IS_LEAF(n, i)) {
            leaf_dump(out, (Leaf*)NODE_ENTRY(n, NODE_INDEX2OFF(n, i)),
                      level*2+1, dumper, data);
        } else {
            node_dump(out, (Node*)NODE_ENTRY(n, NODE_INDEX2OFF(n, i)),
                      level+1, dumper, data);
        }
    }
}

void CompactTrieDump(ScmPort *out, const CompactTrie *ct,
                     void (*dumper)(ScmPort*, Leaf*, int, void*), void *data)
{
    Scm_Printf(out, "CompactTrie(%p, nentries=%d):\n", ct, ct->numEntries);
    if (ct->root == NULL) {
        Scm_Putz("(empty)\n", -1, out);
    } else {
        node_dump(out, ct->root, 0, dumper, data);
    }
}

/* returns # of leaves under this subtree. */
static int check_rec(Node *n, int level,
                     void (*checker)(Leaf*, ScmObj), ScmObj obj)
{
    int direct_leaves = 0, total_leaves = 0;
    int off = 0;
    for (int i=0; i<MAX_NODE_SIZE; i++) {
        if (NODE_HAS_ARC(n, i)) {
            if (NODE_ARC_IS_LEAF(n, i)) {
                direct_leaves++;
                total_leaves++;
                if (checker) checker((Leaf*)NODE_ENTRY(n, off), obj);
            } else {
                total_leaves +=
                    check_rec((Node*)NODE_ENTRY(n, off), level+1, checker, obj);
            }
            off++;
        }
    }
    if (off == 0) Scm_Error("%S: encountered an empty node", obj);
    if (off == 1 && direct_leaves == 1 && level > 0) {
        Scm_Error("%S: non-root node has only one leaf and no other subtrees",
                  obj);
    }
    return total_leaves;
}

void CompactTrieCheck(const CompactTrie *ct, ScmObj obj,
                      void (*checker)(Leaf*, ScmObj))
{
    if (ct->root == NULL) {
        if (ct->numEntries != 0) {
            Scm_Error("%S: ct->root is NULL but numEntries is %d",
                      obj, ct->numEntries);
        }
    } else {
        int num_leaves = check_rec(ct->root, 0, checker, obj);
        if (ct->numEntries != num_leaves) {
            Scm_Error("%S: # of leafs (%d) and numEntries (%d) don't agree",
                      obj, num_leaves, ct->numEntries);
        }
    }
}
