/*
 * parameter.c - parameter support
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
 *  $Id: parameter.c,v 1.2 2002-12-10 13:16:26 shirok Exp $
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

/* Init table.  For primordial thread, base == NULL.  For non-primordial
 * thread, base is the current thread (this must be called from the
 * creator thread).
 */
void Scm_ParameterTableInit(ScmVMParameterTable *table,
                            ScmVM *base)
{
    int i;

    if (base) {
        table->vector = SCM_NEW2(ScmObj*, base->parameters.numAllocated);
        table->numAllocated = base->parameters.numAllocated;
        table->numParameters = base->parameters.numParameters;
        for (i=0; i<table->numParameters; i++) {
            table->vector[i] = base->parameters.vector[i];
        }
    } else {
        table->vector = SCM_NEW2(ScmObj*, PARAMETER_INIT_SIZE);
        table->numParameters = 0;
        table->numAllocated = PARAMETER_INIT_SIZE;
    }
}

/*
 * Allocate new parameter slot
 */
int Scm_MakeParameterSlot(ScmVM *vm)
{
    ScmVMParameterTable *p = &(vm->parameters);
    if (p->numParameters == p->numAllocated) {
        int i;
        ScmObj *newvec = SCM_NEW2(ScmObj*, p->numAllocated + PARAMETER_GROW);
        for (i=0; i<p->numParameters; i++) {
            newvec[i] = p->vector[i];
            p->vector[i] = SCM_FALSE; /*GC friendly*/
        }
        p->vector = newvec;
        p->numAllocated += PARAMETER_GROW;
    }
    p->vector[p->numParameters] = SCM_UNDEFINED;
    return p->numParameters++;
}

/*
 * Accessor & modifier
 */

ScmObj Scm_ParameterRef(ScmVM *vm, int index)
{
    ScmVMParameterTable *p = &(vm->parameters);
    SCM_ASSERT(0 <= index && index < p->numParameters);
    SCM_ASSERT(p->vector[index] != NULL);
    return p->vector[index];
}

ScmObj Scm_ParameterSet(ScmVM *vm, int index, ScmObj value)
{
    ScmVMParameterTable *p = &(vm->parameters);
    SCM_ASSERT(0 <= index && index < p->numParameters);
    SCM_ASSERT(p->vector[index] != NULL);
    p->vector[index] = value;
    return value;
}



