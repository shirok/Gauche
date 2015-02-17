/*
 * parameter.c - parameter support
 *
 *   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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
 *
 * TODO: We now need to allocate a parameter slot to every thread (although
 * allocation is done lazily).  We may be able to use a tree instead of
 * a flat vector so that we can avoid allocation of leaf nodes until
 * they are accessed.
 */

#define PARAMETER_INIT_SIZE 64
#define PARAMETER_GROW      16

/* Every time a new parameter is created (in any thread), it is
 * given a unique index in the process.
 */
static int next_parameter_index = 0;
ScmInternalMutex parameter_mutex = SCM_INTERNAL_MUTEX_INITIALIZER;

/* Init table.  For primordial thread, base == NULL.  For non-primordial
 * thread, base is the current thread (this must be called from the
 * creator thread).
 */
void Scm__VMParameterTableInit(ScmVMParameterTable *table,
                               ScmVM *base)
{
    if (base) {
        /* NB: In this case, the caller is the owner thread of BASE,
           so we don't need to worry about base->parameters being
           modified during copying. */
        table->vector = SCM_NEW_ARRAY(ScmObj, base->parameters.size);
        table->size = base->parameters.size;
        for (int i=0; i<table->size; i++) {
            table->vector[i] = base->parameters.vector[i];
        }
    } else {
        table->vector = SCM_NEW_ARRAY(ScmObj, PARAMETER_INIT_SIZE);
        table->size = PARAMETER_INIT_SIZE;
        for (int i=0; i<table->size; i++) {
            table->vector[i] = SCM_UNBOUND;
        }
    }
}

static void ensure_parameter_slot(ScmVMParameterTable *p, int index)
{
    if (index >= p->size) {
        int newsiz = ((index+PARAMETER_GROW)/PARAMETER_GROW)*PARAMETER_GROW;
        ScmObj *newvec = SCM_NEW_ARRAY(ScmObj, newsiz);

        int i;
        for (i=0; i < p->size; i++) {
            newvec[i] = p->vector[i];
            p->vector[i] = SCM_FALSE; /*be friendly to GC*/
        }
        for (; i < newsiz; i++) {
            newvec[i] = SCM_UNBOUND;
        }
        p->vector = newvec;
        p->size = newsiz;
    }
}

/*
 * Allocate new parameter slot and initializes LOCATION
 */
void Scm_InitParameterLoc(ScmVM *vm, ScmParameterLoc *location, ScmObj initval)
{
    SCM_INTERNAL_MUTEX_LOCK(parameter_mutex);
    int index = next_parameter_index++;
    SCM_INTERNAL_MUTEX_UNLOCK(parameter_mutex);

    ensure_parameter_slot(&(vm->parameters), index);
    location->index = index;
    location->initialValue = initval;
}

/* TRANSIENT: This is for the backward binary compatibility.  */
void Scm_MakeParameterSlot(ScmVM *vm, ScmParameterLoc *location)
{
    Scm_InitParameterLoc(vm, location, SCM_FALSE);
}


/*
 * Accessor & modifier
 */

ScmObj Scm_ParameterRef(ScmVM *vm, const ScmParameterLoc *loc)
{
    ScmVMParameterTable *p = &(vm->parameters);

    if (loc->index >= p->size) return loc->initialValue;
    ScmObj v = p->vector[loc->index];
    if (SCM_UNBOUNDP(v)) {
        v = p->vector[loc->index] = loc->initialValue;
    }
    return v;
}

ScmObj Scm_ParameterSet(ScmVM *vm, const ScmParameterLoc *loc, ScmObj value)
{
    ScmObj oldval;
    ScmVMParameterTable *p = &(vm->parameters);

    if (loc->index >= p->size) {
        ensure_parameter_slot(p, loc->index);
        oldval = loc->initialValue;
    } else {
        oldval = p->vector[loc->index];
        if (SCM_UNBOUNDP(oldval)) {
            oldval = loc->initialValue;
        }
    }
    p->vector[loc->index] = value;
    return oldval;
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

    pd->name = name;
    Scm_InitParameterLoc(vm, &pd->loc, initval);
    ScmObj subr = Scm_MakeSubr(parameter_handler, pd, 0, 1, sname);
    Scm_Define(mod, SCM_SYMBOL(Scm_Intern(SCM_STRING(sname))), subr);
    *location = pd->loc;
}

void Scm__InitParameter(void)
{
    SCM_INTERNAL_MUTEX_INIT(parameter_mutex);
}
