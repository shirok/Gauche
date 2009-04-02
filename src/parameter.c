/*
 * parameter.c - parameter support
 *
 *   Copyright (c) 2000-2009  Shiro Kawai  <shiro@acm.org>
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
 *
 *  $Id: parameter.c,v 1.11 2008-05-10 13:36:20 shirok Exp $
 */

#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/vm.h"

/*
 * Parameters keep thread-local states.   When a thread is created,
 * it inherits the set of parameters from its creator (except the
 * primordial thread).
 * Parameters have additional features, such as guard procedure
 * and observer callbacks.  They are implemented in Scheme level
 * (see lib/gauche/parameter.scm).  C level only provides low-level
 * accessor and modifier methods.
 *
 * It is debatable how to implement the inheritance semantics.  MzScheme
 * keeps user-defined parameters in a hash table, and uses
 * copy-on-write mechanism to delay the copy of the table.  It is nice,
 * but difficult to use in preemptive threads, for it requires lock of
 * the table every time even in reading parameters.  Guile uses the
 * vector (Guile calls them fluids, but it's semantically equivalent
 * to parameters), and eagerly copies the vector at the creation of the
 * thread.  Since thread creation in Gauche is already heavy anyway,
 * I take Guile's approach.
 */

#define PARAMETER_INIT_SIZE 64
#define PARAMETER_GROW      16

/* Every time a new parameter is created (in any thread), it is
 * given an unique ID in the process.  It prevents a thread from
 * dereferencnig a parameter created by an unrelated thread.
 */
static int next_parameter_id = 0;
ScmInternalMutex parameter_mutex;

/* Init table.  For primordial thread, base == NULL.  For non-primordial
 * thread, base is the current thread (this must be called from the
 * creator thread).
 */
void Scm__VMParameterTableInit(ScmVMParameterTable *table,
                               ScmVM *base)
{
    int i;

    if (base) {
        table->vector = SCM_NEW_ARRAY(ScmObj, base->parameters.numAllocated);
        table->ids = SCM_NEW_ATOMIC2(int*, base->parameters.numAllocated*sizeof(int));
        table->numAllocated = base->parameters.numAllocated;
        table->numParameters = base->parameters.numParameters;
        for (i=0; i<table->numParameters; i++) {
            table->vector[i] = base->parameters.vector[i];
            table->ids[i] = base->parameters.ids[i];
        }
    } else {
        table->vector = SCM_NEW_ARRAY(ScmObj, PARAMETER_INIT_SIZE);
        table->ids = SCM_NEW_ATOMIC2(int*, PARAMETER_INIT_SIZE*sizeof(int));
        table->numParameters = 0;
        table->numAllocated = PARAMETER_INIT_SIZE;
    }
}

/*
 * Allocate new parameter slot
 */
void Scm_MakeParameterSlot(ScmVM *vm, ScmParameterLoc *location)
{
    ScmVMParameterTable *p = &(vm->parameters);
    if (p->numParameters == p->numAllocated) {
        int i, newsiz = p->numAllocated + PARAMETER_GROW;
        ScmObj *newvec = SCM_NEW_ARRAY(ScmObj, newsiz);
        int *newids = SCM_NEW_ATOMIC2(int*, newsiz*sizeof(int));
        
        for (i=0; i<p->numParameters; i++) {
            newvec[i] = p->vector[i];
            p->vector[i] = SCM_FALSE; /*GC friendly*/
            newids[i] = p->ids[i];
        }
        p->vector = newvec;
        p->ids = newids;
        p->numAllocated += PARAMETER_GROW;
    }
    p->vector[p->numParameters] = SCM_UNDEFINED;
    SCM_INTERNAL_MUTEX_LOCK(parameter_mutex);
    p->ids[p->numParameters] = location->id = next_parameter_id++;
    SCM_INTERNAL_MUTEX_UNLOCK(parameter_mutex);
    location->index = p->numParameters++;
}

/*
 * Accessor & modifier
 */

ScmObj Scm_ParameterRef(ScmVM *vm, const ScmParameterLoc *loc)
{
    ScmVMParameterTable *p = &(vm->parameters);
    SCM_ASSERT(loc->index >= 0);
    if (loc->index >= p->numParameters || p->ids[loc->index] != loc->id) {
        Scm_Error("the thread %S doesn't have parameter (%d:%d)",
                  vm, loc->index, loc->id);
    }
    SCM_ASSERT(p->vector[loc->index] != NULL);
    return p->vector[loc->index];
}

ScmObj Scm_ParameterSet(ScmVM *vm, const ScmParameterLoc *loc, ScmObj value)
{
    ScmVMParameterTable *p = &(vm->parameters);
    SCM_ASSERT(loc->index >= 0);
    if (loc->index >= p->numParameters || p->ids[loc->index] != loc->id) {
        Scm_Error("the thread %S doesn't have parameter (%d:%d)",
                  vm, loc->index, loc->id);
    }
    p->vector[loc->index] = value;
    return value;
}

struct prim_data {
    const char *name;
    ScmParameterLoc loc;
};

static ScmObj parameter_handler(ScmObj *args, int argc, void *data)
{
    struct prim_data *pd = (struct prim_data*)data;
    ScmVM *vm = Scm_VM();
    SCM_ASSERT(argc == 1);
    if (SCM_NULLP(args[0])) {
        return Scm_ParameterRef(vm, &pd->loc);
    }
    SCM_ASSERT(SCM_PAIRP(args[0]));
    if (SCM_NULLP(SCM_CDR(args[0]))) {
        return Scm_ParameterSet(vm, &pd->loc, SCM_CAR(args[0]));
    }
    else {
        Scm_Error("Bad number of arguments for parameter %s", pd->name);
        return SCM_UNDEFINED;   /* dummy */
    }
}

void Scm_DefinePrimitiveParameter(ScmModule *mod,
                                  const char *name,
                                  ScmObj initval,
                                  ScmParameterLoc *location)
{
    struct prim_data *pd = SCM_NEW(struct prim_data);
    ScmVM *vm = Scm_VM();
    ScmObj sname = SCM_MAKE_STR_IMMUTABLE(name);
    ScmObj subr;
    
    pd->name = name;
    Scm_MakeParameterSlot(vm, &pd->loc);
    Scm_ParameterSet(vm, &pd->loc, initval);
    subr = Scm_MakeSubr(parameter_handler, pd, 0, 1, sname);
    Scm_Define(mod, SCM_SYMBOL(Scm_Intern(SCM_STRING(sname))), subr);
    *location = pd->loc;
}


/*
 * Initialization
 */
void Scm__InitParameter(void)
{
    SCM_INTERNAL_MUTEX_INIT(parameter_mutex);
}
