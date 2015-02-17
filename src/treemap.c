/*
 * treemap.c - tree map implementation
 *
 *   Copyright (c) 2007-2015  Shiro Kawai  <shiro@acm.org>
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

#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/class.h"

/*================================================================
 * ScmTreeCore
 */

/* The actual node structure.  The first two elements must match
   ScmDictEntry. */
typedef struct NodeRec {
    intptr_t     key;
    intptr_t     value;
    int          color;
    struct NodeRec *parent;
    struct NodeRec *left;
    struct NodeRec *right;
} Node;

/* Tree core has an extra operation than dictionary */
enum TreeOp {
    TREE_GET    = SCM_DICT_GET,
    TREE_CREATE = SCM_DICT_CREATE,
    TREE_DELETE = SCM_DICT_DELETE,
    TREE_NEAR
};

#define BLACK 0
#define RED   1

#define BLACKP(n)        (!(n) || (n->color == BLACK))
#define REDP(n)          ((n)  && (n->color == RED))

#define PAINT(n, c)      (n->color = c)

/* The following three macros assume N has a parent. */
#define LEFTP(n)         (n == n->parent->left)
#define RIGHTP(n)        (n == n->parent->right)
#define SIBLING(n)       (LEFTP(n)? n->parent->right:n->parent->left)

/* Like the above, but can be used if the child is NULL. */
#define LEFTP2(p, n)     (p->left == n)
#define RIGHTP2(p, n)    (p->right == n)
#define SIBLING2(p, n)   (LEFTP2(p, n)? p->right : p->left)

#define ROOT(tc)         ((Node*)tc->root)
#define SET_ROOT(tc, n)  (tc->root = (ScmDictEntry*)n)

static Node *core_ref(ScmTreeCore *tc, intptr_t key, enum TreeOp op,
                      Node **lo, Node **hi);
static Node *rightmost(Node *n);
static Node *leftmost(Node *n);
static Node *next_node(Node *n);
static Node *prev_node(Node *n);
static Node *delete_node(ScmTreeCore *tc, Node *n);
static Node *copy_tree(Node *parent, Node *self);

/*
 * Public API
 */

void Scm_TreeCoreInit(ScmTreeCore *tc,
                      ScmTreeCoreCompareProc *cmp,
                      void *data)
{
    tc->root = NULL;
    tc->cmp = cmp;
    tc->num_entries = 0;
    tc->data = data;
}

void Scm_TreeCoreCopy(ScmTreeCore *dst, const ScmTreeCore *src)
{
    if (ROOT(src)) {
        SET_ROOT(dst, copy_tree(NULL, ROOT(src)));
    } else {
        SET_ROOT(dst, NULL);
    }
    dst->cmp = src->cmp;
    dst->num_entries = src->num_entries;
    dst->data = src->data;
}

void Scm_TreeCoreClear(ScmTreeCore *tc)
{
    tc->root = NULL;
    tc->num_entries = 0;
}

ScmDictEntry *Scm_TreeCoreSearch(ScmTreeCore *tc,
                                 intptr_t key,
                                 ScmDictOp op)
{
    return (ScmDictEntry*)core_ref(tc, key, (enum TreeOp)op, NULL, NULL);
}

ScmDictEntry *Scm_TreeCoreClosestEntries(ScmTreeCore *tc,
                                         intptr_t key,
                                         ScmDictEntry **lo,
                                         ScmDictEntry **hi)
{
    Node *l, *h;
    Node *r = core_ref(tc, key, TREE_NEAR, &l, &h);
    *lo = (ScmDictEntry*)l;
    *hi = (ScmDictEntry*)h;
    return (ScmDictEntry*)r;
}

ScmDictEntry *Scm_TreeCoreNextEntry(ScmTreeCore *tc, intptr_t key)
{
    Node *l, *h;
    core_ref(tc, key, TREE_NEAR, &l, &h);
    return (ScmDictEntry*)h;
}

ScmDictEntry *Scm_TreeCorePrevEntry(ScmTreeCore *tc, intptr_t key)
{
    Node *l, *h;
    core_ref(tc, key, TREE_NEAR, &l, &h);
    return (ScmDictEntry*)l;
}

static Node *core_bound(ScmTreeCore *tc, ScmTreeCoreBoundOp op, int pop)
{
    Node *root = ROOT(tc);
    if (root) {
        Node *n = (op == SCM_TREE_CORE_MIN)? leftmost(root) : rightmost(root);
        if (pop) {
            n = delete_node(tc, n);
            tc->num_entries--;
        }
        return n;
    } else {
        return NULL;
    }
}

ScmDictEntry *Scm_TreeCoreGetBound(ScmTreeCore *tc, ScmTreeCoreBoundOp op)
{
    return (ScmDictEntry*)core_bound(tc, op, FALSE);
}

ScmDictEntry *Scm_TreeCorePopBound(ScmTreeCore *tc, ScmTreeCoreBoundOp op)
{
    return (ScmDictEntry*)core_bound(tc, op, TRUE);
}

int Scm_TreeCoreNumEntries(ScmTreeCore *tc)
{
    return tc->num_entries;
}

int Scm_TreeCoreEq(ScmTreeCore *a, ScmTreeCore *b)
{
    ScmTreeIter ai, bi;
    if (a->num_entries != b->num_entries) return FALSE;
    Scm_TreeIterInit(&ai, a, NULL);
    Scm_TreeIterInit(&bi, b, NULL);
    for (;;) {
        ScmDictEntry *ae = Scm_TreeIterNext(&ai);
        ScmDictEntry *be = Scm_TreeIterNext(&bi);
        if (ae == NULL) {
            if (be == NULL) return TRUE;
            else return FALSE;
        }
        if (be == NULL) return FALSE;
        if (ae->key != be->key || ae->value != be->value) return FALSE;
    }
}

/* START can be NULL; in which case, if next call is TreeIterNext,
   it iterates from the minimum node; if next call is TreeIterPrev,
   it iterates from the maximum node. */
void Scm_TreeIterInit(ScmTreeIter *iter,
                      ScmTreeCore *tc,
                      ScmDictEntry *start)
{
    if (start && Scm_TreeCoreSearch(tc, start->key, SCM_DICT_GET) != start) {
        Scm_Error("Scm_TreeIterInit: iteration start point is not a part of the tree.");
    }
    iter->t = tc;
    iter->e = start;
    iter->at_end = FALSE;
}

ScmDictEntry *Scm_TreeIterNext(ScmTreeIter *iter)
{
    if (iter->at_end) return NULL;
    if (iter->e) {
        iter->e = (ScmDictEntry*)next_node((Node*)iter->e);
    } else {
        iter->e = Scm_TreeCoreGetBound(iter->t, SCM_TREE_CORE_MIN);
    }
    if (iter->e == NULL) iter->at_end = TRUE;
    return iter->e;
}

ScmDictEntry *Scm_TreeIterPrev(ScmTreeIter *iter)
{
    if (iter->at_end) return NULL;
    if (iter->e) {
        iter->e = (ScmDictEntry*)prev_node((Node*)iter->e);
    } else {
        iter->e = Scm_TreeCoreGetBound(iter->t, SCM_TREE_CORE_MAX);
    }
    if (iter->e == NULL) iter->at_end = TRUE;
    return iter->e;
}

ScmDictEntry *Scm_TreeIterCurrent(ScmTreeIter *iter)
{
    return iter->e;
}

int Scm_TreeIterAtEnd(ScmTreeIter *iter)
{
    return iter->at_end;
}

/* consistency check */


/* depth is # of black nodes. */
static int check_traverse(Node *node, int depth, int *count)
{
    int ld, rd;

    (*count)++;                 /* entry count */
    if (BLACKP(node)) depth++;

    if (node->left) {
        if (REDP(node) && REDP(node->left)) {
            Scm_Error("[internal] tree map has adjacent red nodes");
        }
        ld = check_traverse(node->left, depth, count);
    } else {
        ld = depth;
    }
    if (node->right) {
        if (REDP(node) && REDP(node->right)) {
            Scm_Error("[internal] tree map has adjacent red nodes");
        }
        rd = check_traverse(node->right, depth, count);
    } else {
        rd = depth;
    }
    if (ld != rd) {
        Scm_Error("[internal] tree map has different black-node depth (L:%d vs R:%d)", ld, rd);
    }
    return ld;
}

void Scm_TreeCoreCheckConsistency(ScmTreeCore *tc)
{
    Node *r = ROOT(tc);
    int cnt = 0;

    if (!BLACKP(r)) Scm_Error("[internal] tree map root node is not black.");
    if (r) check_traverse(r, 1, &cnt);
    if (cnt != tc->num_entries) {
        Scm_Error("[internal] tree map node count mismatch: record %d vs actual %d", tc->num_entries, cnt);
    }
}

/*================================================================
 * ScmTreeMap
 */

static void treemap_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    ScmTreeMap *tm = SCM_TREE_MAP(obj);
    Scm_Printf(port, "#<tree-map %p (%d entries)>", tm,
               Scm_TreeCoreNumEntries(SCM_TREE_MAP_CORE(tm)));
}

SCM_DEFINE_BUILTIN_CLASS(Scm_TreeMapClass, treemap_print, NULL, NULL, NULL,
                         SCM_CLASS_ORDERED_DICTIONARY_CPL);

/*
 * Constructor
 */

ScmObj Scm_MakeTreeMap(ScmTreeCoreCompareProc *cmp, void *data)
{
    ScmTreeMap *tm = SCM_NEW(ScmTreeMap);
    SCM_SET_CLASS(tm, SCM_CLASS_TREE_MAP);
    /* TODO: default cmp should be different from TreeCore */
    Scm_TreeCoreInit(SCM_TREE_MAP_CORE(tm), cmp, data);
    return SCM_OBJ(tm);
}

ScmObj Scm_TreeMapCopy(const ScmTreeMap *src)
{
    ScmTreeMap *tm = SCM_NEW(ScmTreeMap);
    SCM_SET_CLASS(tm, SCM_CLASS_TREE_MAP);
    Scm_TreeCoreCopy(SCM_TREE_MAP_CORE(tm), SCM_TREE_MAP_CORE(src));
    return SCM_OBJ(tm);
}

/*
 * Query
 */
ScmObj Scm_TreeMapRef(ScmTreeMap *tm, ScmObj key, ScmObj fallback)
{
    ScmDictEntry *e = Scm_TreeCoreSearch(SCM_TREE_MAP_CORE(tm),
                                         (intptr_t)key, SCM_DICT_GET);
    if (!e) {
        return fallback;
    } else {
        return SCM_DICT_VALUE(e);
    }
}

ScmObj Scm_TreeMapSet(ScmTreeMap *tm, ScmObj key, ScmObj value, int flags)
{
    ScmDictEntry *e = Scm_TreeCoreSearch(SCM_TREE_MAP_CORE(tm),
                                         (intptr_t)key,
                                         (flags&SCM_DICT_NO_CREATE)? SCM_DICT_GET : SCM_DICT_CREATE);
    if (!e) return SCM_UNBOUND;
    if (e->value) {
        if (flags&SCM_DICT_NO_OVERWRITE) return SCM_DICT_VALUE(e);
        else return SCM_DICT_SET_VALUE(e, value);
    } else {
        return SCM_DICT_SET_VALUE(e, value);
    }
}

ScmObj Scm_TreeMapDelete(ScmTreeMap *tm, ScmObj key)
{
    ScmDictEntry *e = Scm_TreeCoreSearch(SCM_TREE_MAP_CORE(tm),
                                         (intptr_t)key, SCM_DICT_DELETE);
    if (e && e->value) return SCM_DICT_VALUE(e);
    else               return SCM_UNBOUND;
}

/* for debug */
static void dump_traverse(Node *node, int depth, ScmPort *out, int scmobj)
{
    if (node->left) dump_traverse(node->left, depth+1, out, scmobj);
    for (int i=0; i<depth; i++) Scm_Printf(out, "  ");
    if (scmobj) {
        Scm_Printf(out, "%c:%S => %S\n", BLACKP(node)?'B':'R',
                   SCM_OBJ(node->key), SCM_OBJ(node->value));
    } else {
        Scm_Printf(out, "%c:%08x => %08x\n", BLACKP(node)?'B':'R',
                   node->key, node->value);
    }
    if (node->right) dump_traverse(node->right, depth+1, out, scmobj);
}

void Scm_TreeMapDump(ScmTreeMap *tm, ScmPort *out)
{
    ScmTreeCore *tc = SCM_TREE_MAP_CORE(tm);
    Node *r = ROOT(tc);
    Scm_Printf(out, "Entries=%d\n", tc->num_entries);
    if (r) {
        dump_traverse(r, 0, out, TRUE);
    }
}

void Scm_TreeCoreDump(ScmTreeCore *tc, ScmPort *out)
{
    Node *r = ROOT(tc);
    Scm_Printf(out, "Entries=%d\n", tc->num_entries);
    if (r) {
        dump_traverse(r, 0, out, FALSE);
    }
}

/*=============================================================
 * Internal stuff (Red-Black Tree implementation)
 */

/* Returns the right/leftmost node under N. */
static Node *rightmost(Node *n)
{
    while (n->right) n = n->right;
    return n;
}

static Node *leftmost(Node *n)
{
    while (n->left) n = n->left;
    return n;
}

/* Returns the previous node of N.  NULL iff n is the minimum. */
static Node *prev_node(Node *n)
{
    if (n->left) return rightmost(n->left);
    while (n->parent) {
        if (RIGHTP(n)) return n->parent;
        n = n->parent;
    }
    return NULL;
}

/* Returns the next node of N.  NULL iff n is the maximum. */
static Node *next_node(Node *n)
{
    if (n->right) return leftmost(n->right);
    while (n->parent) {
        if (LEFTP(n)) return n->parent;
        n = n->parent;
    }
    return NULL;
}

/* fresh node */
static Node *new_node(Node *parent, intptr_t key)
{
    Node *n = SCM_NEW(Node);
    n->key = key;
    n->value = 0;
    n->color = RED;             /* default is red */
    n->parent = parent;
    n->left = n->right = NULL;
    return n;
}

/* clear node (for Weak-GC safeness) */
static void clear_node(Node *node)
{
    node->parent = node->left = node->right = NULL;
}

/* replace N's position by M. M could be NULL. */
static void replace_node(ScmTreeCore *tc, Node *n, Node *m)
{
    if (n->parent) {
        if (LEFTP(n)) n->parent->left = m;
        else          n->parent->right = m;
    } else {
        SET_ROOT(tc, m);
    }
    if (m) m->parent = n->parent;
}


/* rotate_right:

              N                L
           +-----+          +-----+
           L     R    ==>  GL     N
         +---+                  +---+
        GL   GR                GR   R
*/
static void rotate_right(ScmTreeCore *tc, Node *n)
{
    Node *l = n->left;
    SCM_ASSERT(l != NULL);
    Node *gr = l->right;

    replace_node(tc, n, l);
    l->right = n;  n->parent = l;
    n->left = gr;  if (gr) gr->parent = n;
}

/* rotate_left:

              N                R
           +-----+          +-----+
           L     R    ==>   N     GR
               +---+      +---+
              GL   GR     L   GL
*/
static void rotate_left(ScmTreeCore *tc, Node *n)
{
    Node *r = n->right;
    SCM_ASSERT(r != NULL);
    Node *gl = r->left;

    replace_node(tc, n, r);
    r->left = n;   n->parent = r;
    n->right = gl; if (gl) gl->parent = n;
}

#if 0 /* for debug */
#define BALANCE_CASE(n) printf("balance case %s\n", n)
#else
#define BALANCE_CASE(n) /*nothing*/
#endif

/* balance tree after insertion of N */
static void balance_tree(ScmTreeCore *tc, Node *n)
{
    Node *p = n->parent;

    if (!p) { BALANCE_CASE("1"); n->color = BLACK; return; }  /* root */
    if (BLACKP(p)) { BALANCE_CASE("2"); return; }      /* nothing to do */

    /* Here we're sure we have grandparent. */
    Node *g = p->parent;
    SCM_ASSERT(g != NULL);
    Node *u = (g->left == p)? g->right : g->left;

    if (REDP(u)) {
        p->color = u->color = BLACK;
        g->color = RED;
        BALANCE_CASE("3");
        balance_tree(tc, g);
        return;
    }
    if (n == p->right && p == g->left) {
        rotate_left(tc, p);
        n = n->left;
        BALANCE_CASE("4a");
    } else if (n == p->left && p == g->right) {
        rotate_right(tc, p);
        n = n->right;
        BALANCE_CASE("4b");
    }
    p = n->parent;
    g = p->parent;
    p->color = BLACK;
    g->color = RED;
    if (n == p->left && p == g->left) {
        rotate_right(tc, g);
        BALANCE_CASE("5a");
    } else {
        rotate_left(tc, g);
        BALANCE_CASE("5b");
    }
}

#if 0 /* for debug */
#define DELETE_CASE(n) printf("delete case %s\n", n)
#else
#define DELETE_CASE(n) /*nothing*/
#endif

/* deletes a node TODIE who has at most one child, CHILD.
   Note that CHILD can be NULL (empty BLACK node) */
static void delete_node1(ScmTreeCore *tc, Node *todie, Node *child)
{
    Node *parent = todie->parent;

    replace_node(tc, todie, child);
    if (REDP(todie)) { DELETE_CASE("1"); return; }
    if (REDP(child)) { DELETE_CASE("2"); child->color = BLACK; return; }

  recur:
    /* At this point, child is BLACK. */
    if (parent == NULL) { DELETE_CASE("3"); return; }
    Node *sibling = SIBLING2(parent, child);
    /* sibling can't be NULL, since it would break the invariance of
       consistent # of black nodes for every path. */
    SCM_ASSERT(sibling != NULL);

    if (REDP(sibling)) {
        parent->color = RED;
        sibling->color = BLACK;
        if (LEFTP2(parent, child)) {
            rotate_left(tc, parent);
            sibling = SIBLING2(parent, child);
            DELETE_CASE("4a");
        } else {
            rotate_right(tc, parent);
            sibling = SIBLING2(parent, child);
            DELETE_CASE("4b");
        }
    }

    /* At this point, sibling is BLACK */
    if (BLACKP(parent) && BLACKP(sibling->left) && BLACKP(sibling->right)) {
        sibling->color = RED;

        child = parent;
        parent = parent->parent;
        DELETE_CASE("5");
        goto recur;
    }
    if (REDP(parent) && BLACKP(sibling->left) && BLACKP(sibling->right)) {
        parent->color = BLACK;
        sibling->color = RED;
        DELETE_CASE("6");
        return;
    }
    if (LEFTP2(parent, child)) {
        if (REDP(sibling->left) && BLACKP(sibling->right)) {
            sibling->color = RED;
            sibling->left->color = BLACK;
            rotate_right(tc, sibling);
            sibling = SIBLING2(parent, child);
            DELETE_CASE("7a");
        }
    } else {              /* RIGHTP(child) */
        if (BLACKP(sibling->left) && REDP(sibling->right)) {
            sibling->color = RED;
            sibling->right->color = BLACK;
            rotate_left(tc, sibling);
            sibling = SIBLING2(parent, child);
            DELETE_CASE("7b");
        }
    }
    sibling->color = parent->color;
    parent->color = BLACK;
    if (LEFTP2(parent, child)) {
        sibling->right->color = BLACK;
        rotate_left(tc, parent);
        DELETE_CASE("8a");
    } else {
        sibling->left->color = BLACK;
        rotate_right(tc, parent);
        DELETE_CASE("8b");
    }
}

static void swap_node(ScmTreeCore *tc, Node *x, Node *y)
{
#define SWAP(x, y, tmp) do { tmp = x; x = y; y = tmp; } while (0)
    if (x->parent) {
        if (LEFTP(x)) x->parent->left = y;
        else          x->parent->right = y;
    }
    if (y->parent) {
        if (LEFTP(y)) y->parent->left = x;
        else          y->parent->right = x;
    }
    Node *t;
    SWAP(x->parent, y->parent, t);

    if (x->left) x->left->parent = y;
    if (y->left) y->left->parent = x;
    SWAP(x->left, y->left, t);

    if (x->right) x->right->parent = y;
    if (y->right) y->right->parent = x;
    SWAP(x->right, y->right, t);

    int c;
    SWAP(x->color, y->color, c);
    if (x == ROOT(tc)) SET_ROOT(tc, y);
    else if (y == ROOT(tc)) SET_ROOT(tc, x);
#undef SWAP
}

static Node *delete_node(ScmTreeCore *tc, Node *n)
{
    while (n->left && n->right) {
        /* N has both children.  We swap N and its previous node.
           It would be easier just to swap key and value, but it could
           lead to a hard-to-track bug if a pointer to N is retained
           in somewhere (e.g. iterator).  So we actually swap the node. */
        swap_node(tc, n, prev_node(n));
    }

    /* we have at most one child */
    if (n->left) {
        delete_node1(tc, n, n->left);
    } else {
        delete_node1(tc, n, n->right);  /* this covers no child case */
    }
    clear_node(n);
    return n;
}

/* accessor */
Node *core_ref(ScmTreeCore *tc, intptr_t key, enum TreeOp op,
               Node **lo, Node **hi)
{
    Node *e = ROOT(tc), *n = NULL;

    if (e == NULL) {
        /* Tree is empty */
        if (op == TREE_CREATE) {
            n = new_node(NULL, key);
            PAINT(n, BLACK);
            SET_ROOT(tc, n);
            tc->num_entries++;
        }
        if (op == TREE_NEAR) {
            *lo = *hi = NULL;
        }
        return n;
    }

    for (;;) {
        int r = 0;
        if (tc->cmp) r = tc->cmp(tc, e->key, key);

        if (tc->cmp? (r == 0) : (e->key == key)) {
            /* Exact match */
            if (op == TREE_DELETE) {
                n = delete_node(tc, e);
                tc->num_entries--;
                return n;
            }
            if (op == TREE_NEAR) {
                *lo = prev_node(e);
                *hi = next_node(e);
            }
            return e;
        }

        if (tc->cmp? (r < 0) : (e->key < key)) {
            /* Key is larger than E */
            if (e->right) {
                e = e->right;
            } else {
                if (op == TREE_CREATE) {
                    n = new_node(e, key);
                    e->right = n;
                    balance_tree(tc, n);
                    tc->num_entries++;
                    return n;
                }
                if (op == TREE_NEAR) {
                    *lo = e;
                    *hi = next_node(e);
                }
                return NULL;
            }
        } else {
            /* Key is smaller than E */
            if (e->left) {
                e = e->left;
            } else {
                if (op == TREE_CREATE) {
                    n = new_node(e, key);
                    e->left = n;
                    balance_tree(tc, n);
                    tc->num_entries++;
                    return n;
                }
                if (op == TREE_NEAR) {
                    *hi = e;
                    *lo = prev_node(e);
                }
                return NULL;
            }
        }
    }
}

/* copy */
static Node *copy_tree(Node *parent, Node *self)
{
    Node *n = new_node(parent, self->key);
    n->value = self->value;
    n->color = self->color;
    if (self->left)  n->left = copy_tree(n, self->left);
    if (self->right) n->right = copy_tree(n, self->right);
    return n;
}
