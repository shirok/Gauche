/*
 * class.c - class metaobject implementation
 *
 *  Copyright(C) 2000 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, ditribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: class.c,v 1.3 2001-01-17 08:22:37 shiro Exp $
 */

#include "gauche.h"

static int printObj(ScmObj, ScmPort *, int);
static int printClass(ScmObj, ScmPort *, int);

/*
 * Built-in classes
 */

static ScmClass *class_top_cpl[] = { SCM_CLASS_TOP, NULL };

ScmClass Scm_TopClass = {
    SCM_CLASS_CLASS,
    "<top>",
    printObj,
    class_top_cpl
};

ScmClass Scm_BoolClass = {
    SCM_CLASS_CLASS,
    "<bool>",
    printObj,
    class_top_cpl
};

ScmClass Scm_CharClass = {
    SCM_CLASS_CLASS,
    "<char>",
    printObj,
    class_top_cpl
};

ScmClass Scm_UnknownClass = {
    SCM_CLASS_CLASS,
    "<unknown>",
    printObj,
    class_top_cpl
};

ScmClass Scm_ClassClass = {
    SCM_CLASS_CLASS,
    "<class>",
    printClass,
    class_top_cpl
};

/* Collection and sequence types */

ScmClass Scm_CollectionClass = {
    SCM_CLASS_CLASS,
    "<collection>",
    printObj,
    class_top_cpl
};

static ScmClass *class_collection_cpl[] = {
    SCM_CLASS_COLLECTION, SCM_CLASS_TOP, NULL
};

ScmClass Scm_SequenceClass = {
    SCM_CLASS_CLASS,
    "<sequence>",
    printObj,
    class_collection_cpl
};

/*
 * Get class
 */

ScmClass *Scm_ClassOf(ScmObj obj)
{
    if (!SCM_PTRP(obj)) {
        if (SCM_TRUEP(obj) || SCM_FALSEP(obj)) return SCM_CLASS_BOOL;
        if (SCM_NULLP(obj)) return SCM_CLASS_NULL;
        if (SCM_CHARP(obj)) return SCM_CLASS_CHAR;
        if (SCM_INTP(obj))  return SCM_CLASS_INTEGER;
        else return SCM_CLASS_UNKNOWN;
    } else {
        return obj->klass;
    }
}

/*
 * Metainformation accessors
 */

ScmObj Scm_ClassCPL(ScmClass *klass)
{
    ScmClass **p = klass->cpl;
    ScmObj start = SCM_NIL, last;
    while (*p) {
        SCM_APPEND1(start, last, SCM_OBJ(*p));
        p++;
    }
    return start;
}

ScmObj Scm_SubtypeP(ScmClass *sub, ScmClass *type)
{
    ScmClass **p;
    if (sub == type) return SCM_TRUE;

    p = sub->cpl;
    while (*p) {
        if (*p++ == type) return SCM_TRUE;
    }
    return SCM_FALSE;
}

ScmObj Scm_TypeP(ScmObj obj, ScmClass *type)
{
    return Scm_SubtypeP(Scm_ClassOf(obj), type);
}

/*
 * fallback print methods
 */

static int printClass(ScmObj obj, ScmPort *port, int mode) 
{
    ScmClass *c = (ScmClass*)obj;
    return Scm_Printf(port, "#<class %s>", c->name);
}

static int printObj(ScmObj obj, ScmPort *port, int mode)
{
    ScmClass *c = Scm_ClassOf(obj);
    return Scm_Printf(port, "#<%s %p>", c->name, obj);
}

/*
 * Class initialization
 */

void Scm__InitClass(void)
{
    int i;
    ScmModule *mod = Scm_SchemeModule();
    
    SCM_DEFINE(mod, "<top>",      SCM_OBJ(SCM_CLASS_TOP));
    SCM_DEFINE(mod, "<boolean>",  SCM_OBJ(SCM_CLASS_BOOL));
    SCM_DEFINE(mod, "<char>",     SCM_OBJ(SCM_CLASS_CHAR));
    SCM_DEFINE(mod, "<unknown>",  SCM_OBJ(SCM_CLASS_UNKNOWN));
    SCM_DEFINE(mod, "<class>",    SCM_OBJ(SCM_CLASS_CLASS));
    SCM_DEFINE(mod, "<collection>", SCM_OBJ(SCM_CLASS_COLLECTION));
    SCM_DEFINE(mod, "<sequence>", SCM_OBJ(SCM_CLASS_SEQUENCE));
    SCM_DEFINE(mod, "<string>",   SCM_OBJ(SCM_CLASS_STRING));
    SCM_DEFINE(mod, "<symbol>",   SCM_OBJ(SCM_CLASS_SYMBOL));
    SCM_DEFINE(mod, "<gloc>",     SCM_OBJ(SCM_CLASS_GLOC));
    SCM_DEFINE(mod, "<syntax>",   SCM_OBJ(SCM_CLASS_SYNTAX));
    SCM_DEFINE(mod, "<port>",     SCM_OBJ(SCM_CLASS_PORT));
    SCM_DEFINE(mod, "<list>",     SCM_OBJ(SCM_CLASS_LIST));
    SCM_DEFINE(mod, "<pair>",     SCM_OBJ(SCM_CLASS_PAIR));
    SCM_DEFINE(mod, "<null>",     SCM_OBJ(SCM_CLASS_NULL));
    SCM_DEFINE(mod, "<vector>",   SCM_OBJ(SCM_CLASS_VECTOR));
    SCM_DEFINE(mod, "<hash-table>", SCM_OBJ(SCM_CLASS_HASHTABLE));
    SCM_DEFINE(mod, "<module>",   SCM_OBJ(SCM_CLASS_MODULE));
    SCM_DEFINE(mod, "<number>",   SCM_OBJ(SCM_CLASS_NUMBER));
    SCM_DEFINE(mod, "<complex>",  SCM_OBJ(SCM_CLASS_COMPLEX));
    SCM_DEFINE(mod, "<real>",     SCM_OBJ(SCM_CLASS_REAL));
    SCM_DEFINE(mod, "<integer>",  SCM_OBJ(SCM_CLASS_INTEGER));
    SCM_DEFINE(mod, "<procedure>", SCM_OBJ(SCM_CLASS_PROCEDURE));
    SCM_DEFINE(mod, "<closure>",  SCM_OBJ(SCM_CLASS_CLOSURE));
    SCM_DEFINE(mod, "<subr>",     SCM_OBJ(SCM_CLASS_SUBR));
    SCM_DEFINE(mod, "<vm>",       SCM_OBJ(SCM_CLASS_VM));
    SCM_DEFINE(mod, "<source-info>", SCM_OBJ(SCM_CLASS_SOURCE_INFO));
}
