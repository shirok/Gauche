/*
 * error.c - error handling
 *
 *   Copyright (c) 2000-2025  Shiro Kawai  <shiro@acm.org>
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
#include "gauche/priv/configP.h"
#include "gauche/exception.h"
#include "gauche/vm.h"
#include "gauche/priv/builtin-syms.h"

#include <errno.h>
#include <string.h>
#include <ctype.h>

static ScmObj condition_allocate(ScmClass *klass, ScmObj initargs);
static ScmObj message_allocate(ScmClass *klass, ScmObj initargs);
static ScmObj syserror_allocate(ScmClass *klass, ScmObj initargs);
static ScmObj sigerror_allocate(ScmClass *klass, ScmObj initargs);
static ScmObj unberror_allocate(ScmClass *klass, ScmObj initargs);
static ScmObj readerror_allocate(ScmClass *klass, ScmObj initargs);
static ScmObj porterror_allocate(ScmClass *klass, ScmObj initargs);
static ScmObj conterror_allocate(ScmClass *klass, ScmObj initargs);
static ScmObj compound_allocate(ScmClass *klass, ScmObj initargs);

/* Setting up CPL is a bit tricky, since we have multiple
   inheritance case. */

#define CONDITION_CPL                           \
    SCM_CLASS_STATIC_PTR(Scm_ConditionClass),   \
    SCM_CLASS_STATIC_PTR(Scm_TopClass)

#define MESSAGE_SERIOUS_CPL \
    SCM_CLASS_STATIC_PTR(Scm_MessageConditionClass), \
    SCM_CLASS_STATIC_PTR(Scm_SeriousConditionClass), \
    CONDITION_CPL

#define ERROR_CPL \
    SCM_CLASS_STATIC_PTR(Scm_ErrorClass),        \
    MESSAGE_SERIOUS_CPL

/*-----------------------------------------------------------
 * Base conditions
 */
static ScmClass *condition_cpl[] = {
    CONDITION_CPL,
    NULL
};

SCM_DEFINE_BASE_CLASS(Scm_ConditionClass, ScmInstance,
                      NULL, NULL, NULL,
                      condition_allocate, SCM_CLASS_DEFAULT_CPL);
SCM_DEFINE_BASE_CLASS(Scm_MessageConditionClass, ScmMessageCondition,
                      Scm_MessageConditionPrint, NULL, NULL,
                      message_allocate, condition_cpl);
SCM_DEFINE_BASE_CLASS(Scm_SeriousConditionClass, ScmSeriousCondition,
                      NULL, NULL, NULL,
                      condition_allocate, condition_cpl);
SCM_DEFINE_BASE_CLASS(Scm_MixinConditionClass, ScmCondition,
                      NULL, NULL, NULL,
                      condition_allocate, condition_cpl);

static ScmObj condition_allocate(ScmClass *klass, ScmObj initargs SCM_UNUSED)
{
    return SCM_OBJ(SCM_NEW_INSTANCE(ScmCondition, klass));
}

/* We expose this, for other condition subclasses may share this. */
void Scm_MessageConditionPrint(ScmObj obj, ScmPort *port,
                               ScmWriteContext *ctx SCM_UNUSED)
{
    ScmClass *k = Scm_ClassOf(obj);
    Scm_Printf(port, "#<%A \"%150.1A\">",
               Scm_ShortClassName(k),
               SCM_ERROR_MESSAGE(obj));
}

static ScmObj message_allocate(ScmClass *klass, ScmObj initargs SCM_UNUSED)
{
    ScmError *e = SCM_NEW_INSTANCE(ScmError, klass);
    e->message = SCM_FALSE;     /* would be set by initialize */
    return SCM_OBJ(e);
}

/* See comment on gauche/exception.h about hack in 'message' slot.
   TRANSIENT: Remove this hack on 1.0 release. */
static ScmObj message_get(ScmMessageCondition *obj)
{
    ScmObj msglist = obj->message;
    if (SCM_PAIRP(msglist)) return SCM_CAR(msglist);
    else return msglist;
}

static void message_set(ScmMessageCondition *obj, ScmObj val)
{
    ScmObj msglist = obj->message;
    if (SCM_PAIRP(msglist)) SCM_SET_CAR_UNCHECKED(msglist, val);
    else SCM_MESSAGE_CONDITION(obj)->message = SCM_LIST2(val, val);
}

static ScmObj message_prefix_get(ScmMessageCondition *obj)
{
    ScmObj msglist = obj->message;
    if (SCM_PAIRP(msglist) && SCM_PAIRP(SCM_CDR(msglist))) {
        return SCM_CADR(msglist);
    } else {
        return msglist;
    }
}

static void message_prefix_set(ScmMessageCondition *obj, ScmObj val)
{
    ScmObj msglist = obj->message;
    if (SCM_PAIRP(msglist) && SCM_PAIRP(SCM_CDR(msglist))) {
        SCM_SET_CAR_UNCHECKED(SCM_CDR(msglist), val);
    } else {
        obj->message = SCM_LIST2(msglist, val);
    }
}

static ScmObj message_args_get(ScmMessageCondition *obj)
{
    ScmObj msglist = obj->message;
    if (SCM_PAIRP(msglist) && SCM_PAIRP(SCM_CDR(msglist))) {
        return SCM_CDDR(msglist);
    } else {
        return SCM_NIL;
    }
}

static void message_args_set(ScmMessageCondition *obj, ScmObj val)
{
    ScmObj msglist = obj->message;
    if (SCM_PAIRP(msglist) && SCM_PAIRP(SCM_CDR(msglist))) {
        SCM_SET_CDR_UNCHECKED(SCM_CDR(msglist), val);
    } else {
        obj->message = Scm_Cons(msglist, Scm_Cons(msglist, val));
    }
}

static ScmClassStaticSlotSpec message_slots[] = {
    SCM_CLASS_SLOT_SPEC("message", message_get, message_set),
    SCM_CLASS_SLOT_SPEC("message-prefix",   message_prefix_get, message_prefix_set),
    SCM_CLASS_SLOT_SPEC("message-args", message_args_get, message_args_set),
    SCM_CLASS_SLOT_SPEC_END()
};

/*------------------------------------------------------------
 * Errors
 */

static ScmClass *error_cpl[] = {
    ERROR_CPL,
    NULL
};

static ScmClass *porterror_cpl[] = {
    SCM_CLASS_STATIC_PTR(Scm_PortErrorClass),
    SCM_CLASS_STATIC_PTR(Scm_IOErrorClass),
    ERROR_CPL,
    NULL
};

static ScmClass *decoding_error_cpl[] = {
    SCM_CLASS_STATIC_PTR(Scm_IOReadErrorClass),
    SCM_CLASS_STATIC_PTR(Scm_PortErrorClass),
    SCM_CLASS_STATIC_PTR(Scm_IOErrorClass),
    ERROR_CPL,
    NULL
};

static ScmClass *encoding_error_cpl[] = {
    SCM_CLASS_STATIC_PTR(Scm_IOWriteErrorClass),
    SCM_CLASS_STATIC_PTR(Scm_PortErrorClass),
    SCM_CLASS_STATIC_PTR(Scm_IOErrorClass),
    ERROR_CPL,
    NULL
};

SCM_DEFINE_BASE_CLASS(Scm_ErrorClass, ScmError,
                      Scm_MessageConditionPrint, NULL, NULL,
                      message_allocate, error_cpl+1);
SCM_DEFINE_BASE_CLASS(Scm_SystemErrorClass, ScmSystemError,
                      Scm_MessageConditionPrint, NULL, NULL,
                      syserror_allocate, error_cpl);
SCM_DEFINE_BASE_CLASS(Scm_AssertionViolationClass, ScmAssertionViolation,
                      Scm_MessageConditionPrint, NULL, NULL,
                      message_allocate, error_cpl);
SCM_DEFINE_BASE_CLASS(Scm_UnhandledSignalErrorClass, ScmUnhandledSignalError,
                      Scm_MessageConditionPrint, NULL, NULL,
                      sigerror_allocate, error_cpl);
SCM_DEFINE_BASE_CLASS(Scm_UnboundVariableErrorClass, ScmUnboundVariableError,
                      Scm_MessageConditionPrint, NULL, NULL,
                      unberror_allocate, error_cpl);
SCM_DEFINE_BASE_CLASS(Scm_ReadErrorClass, ScmReadError,
                      Scm_MessageConditionPrint, NULL, NULL,
                      readerror_allocate, error_cpl);
SCM_DEFINE_BASE_CLASS(Scm_IOErrorClass, ScmIOError,
                      Scm_MessageConditionPrint, NULL, NULL,
                      message_allocate, error_cpl);
SCM_DEFINE_BASE_CLASS(Scm_PortErrorClass, ScmPortError,
                      Scm_MessageConditionPrint, NULL, NULL,
                      porterror_allocate, porterror_cpl+1);
SCM_DEFINE_BASE_CLASS(Scm_IOReadErrorClass, ScmIOReadError,
                      Scm_MessageConditionPrint, NULL, NULL,
                      porterror_allocate, porterror_cpl);
SCM_DEFINE_BASE_CLASS(Scm_IOWriteErrorClass, ScmIOWriteError,
                      Scm_MessageConditionPrint, NULL, NULL,
                      porterror_allocate, porterror_cpl);
SCM_DEFINE_BASE_CLASS(Scm_IOClosedErrorClass, ScmIOClosedError,
                      Scm_MessageConditionPrint, NULL, NULL,
                      porterror_allocate, porterror_cpl);
SCM_DEFINE_BASE_CLASS(Scm_IOUnitErrorClass, ScmIOUnitError,
                      Scm_MessageConditionPrint, NULL, NULL,
                      porterror_allocate, porterror_cpl);
SCM_DEFINE_BASE_CLASS(Scm_IODecodingErrorClass, ScmIODecodingError,
                      Scm_MessageConditionPrint, NULL, NULL,
                      porterror_allocate, decoding_error_cpl);
SCM_DEFINE_BASE_CLASS(Scm_IOEncodingErrorClass, ScmIOEncodingError,
                      Scm_MessageConditionPrint, NULL, NULL,
                      porterror_allocate, encoding_error_cpl);
SCM_DEFINE_BASE_CLASS(Scm_IOInvalidPositionErrorClass, ScmIOInvalidPositionError,
                      Scm_MessageConditionPrint, NULL, NULL,
                      porterror_allocate, porterror_cpl);

SCM_DEFINE_BASE_CLASS(Scm_ContinuationErrorClass, ScmContinuationError,
                      Scm_MessageConditionPrint, NULL, NULL,
                      conterror_allocate, error_cpl);

static ScmObj syserror_allocate(ScmClass *klass, ScmObj initargs SCM_UNUSED)
{
    ScmSystemError *e = SCM_NEW_INSTANCE(ScmSystemError, klass);
    e->common.message = SCM_FALSE; /* set by initialize */
    e->error_number = 0;           /* set by initialize */
    return SCM_OBJ(e);
}

static ScmObj sigerror_allocate(ScmClass *klass, ScmObj initargs SCM_UNUSED)
{
    ScmUnhandledSignalError *e = SCM_NEW_INSTANCE(ScmUnhandledSignalError,
                                                  klass);
    e->common.message = SCM_FALSE; /* set by initialize */
    e->signal = 0;                 /* set by initialize */
    return SCM_OBJ(e);
}

static ScmObj unberror_allocate(ScmClass *klass, ScmObj initargs SCM_UNUSED)
{
    ScmUnboundVariableError *e = SCM_NEW_INSTANCE(ScmUnboundVariableError,
                                                  klass);
    e->common.message = SCM_FALSE; /* set by initialize */
    e->identifier = SCM_FALSE;     /* set by initialize */
    return SCM_OBJ(e);
}

static ScmObj readerror_allocate(ScmClass *klass, ScmObj initargs SCM_UNUSED)
{
    ScmReadError *e = SCM_NEW_INSTANCE(ScmReadError, klass);
    e->common.message = SCM_FALSE; /* set by initialize */
    e->port = NULL;                /* set by initialize */
    e->line = -1;                  /* set by initialize */
    return SCM_OBJ(e);
}

static ScmObj porterror_allocate(ScmClass *klass, ScmObj initargs SCM_UNUSED)
{
    ScmPortError *e = SCM_NEW_INSTANCE(ScmPortError, klass);
    e->common.message = SCM_FALSE; /* set by initialize */
    e->port = NULL;                /* set by initialize */
    e->auxinfo = SCM_NIL;          /* set by initialize */
    return SCM_OBJ(e);
}

static ScmObj conterror_allocate(ScmClass *klass, ScmObj initargs SCM_UNUSED)
{
    ScmContinuationError *e = SCM_NEW_INSTANCE(ScmContinuationError, klass);
    e->common.message = SCM_FALSE; /* set by initialize */
    e->promptTag = SCM_FALSE;      /* set by initialize */
    return SCM_OBJ(e);
}

static ScmObj syserror_number_get(ScmSystemError *obj)
{
    return SCM_MAKE_INT(obj->error_number);
}

static void syserror_number_set(ScmSystemError *obj, ScmObj val)
{
    if (!SCM_INTP(val)) {
        Scm_Error("small integer required, but got %S", val);
    }
    obj->error_number = SCM_INT_VALUE(val);
}

static ScmObj sigerror_signal_get(ScmUnhandledSignalError *obj)
{
    return SCM_MAKE_INT(obj->signal);
}

static void sigerror_signal_set(ScmUnhandledSignalError *obj, ScmObj val)
{
    if (!SCM_INTP(val)) {
        Scm_Error("small integer required, but got %S", val);
    }
    obj->signal = SCM_INT_VALUE(val);
}

static ScmObj unberror_identifier_get(ScmUnboundVariableError *obj)
{
    return obj->identifier;
}

static void unberror_identifier_set(ScmUnboundVariableError *obj, ScmObj val)
{
    if (!(SCM_SYMBOLP(val) || !SCM_IDENTIFIERP(val))) {
        Scm_Error("symbol or identifier required, but got %S", val);
    }
    obj->identifier = val;
}

static ScmObj readerror_port_get(ScmReadError *obj)
{
    if (obj->port) return SCM_OBJ(obj->port);
    else return SCM_FALSE;
}

static void readerror_port_set(ScmReadError *obj, ScmObj val)
{
    if (SCM_IPORTP(val)) {
        obj->port = SCM_PORT(val);
    }
    else if (SCM_FALSEP(val)) {
        obj->port = NULL;
    }
    else {
        Scm_Error("input port or #f required, but got %S", val);
    }
}

static ScmObj readerror_line_get(ScmReadError *obj)
{
    return SCM_MAKE_INT(obj->line);
}

static void readerror_line_set(ScmReadError *obj, ScmObj val)
{
    if (!SCM_INTP(val)){
        Scm_Error("small integer required, but got %S", val);
    }
    obj->line = SCM_INT_VALUE(val);
}

static ScmObj readerror_dummy_get(ScmReadError *obj SCM_UNUSED)
{
    return SCM_FALSE;
}

static void readerror_dummy_set(ScmReadError *obj SCM_UNUSED,
                                ScmObj val SCM_UNUSED)
{
    /* nothing */
}

static ScmObj porterror_port_get(ScmPortError *obj)
{
    return obj->port? SCM_OBJ(obj->port) : SCM_FALSE;
}

static void porterror_port_set(ScmPortError *obj, ScmObj val)
{
    if (!SCM_PORTP(val) && !SCM_FALSEP(val)) {
        Scm_Error("port or #f required, but got %S", val);
    }
    obj->port = SCM_FALSEP(val)? NULL : SCM_PORT(val);
}

static ScmObj porterror_auxinfo_getter(ScmPortError *obj, ScmObj key)
{
    ScmObj p = Scm_Assq(key, obj->auxinfo);
    if (SCM_PAIRP(p)) return SCM_CDR(p);
    return SCM_FALSE;
}

static void porterror_auxinfo_setter(ScmPortError *obj, ScmObj key, ScmObj val)
{
    ScmObj p = Scm_Assq(key, obj->auxinfo);
    if (SCM_PAIRP(p)) SCM_SET_CDR(p, val);
    else obj->auxinfo = Scm_Acons(key, val, obj->auxinfo);
}

static ScmObj sym_offending_char; /* 'offending-char */

static ScmObj porterror_offending_char_get(ScmPortError *obj)
{
    return porterror_auxinfo_getter(obj, sym_offending_char);
}

static void porterror_offending_char_set(ScmPortError *obj, ScmObj val)
{
    porterror_auxinfo_setter(obj, sym_offending_char, val);
}

static ScmObj sym_position;     /* 'position */

static ScmObj porterror_position_get(ScmPortError *obj)
{
    return porterror_auxinfo_getter(obj, sym_position);
}

static void porterror_position_set(ScmPortError *obj, ScmObj val)
{
    porterror_auxinfo_setter(obj, sym_position, val);
}

static ScmObj conterror_prompt_tag_get(ScmContinuationError *obj)
{
    return obj->promptTag;
}

static void conterror_prompt_tag_set(ScmContinuationError *obj, ScmObj val)
{
    obj->promptTag = val;
}


static ScmClassStaticSlotSpec syserror_slots[] = {
    SCM_CLASS_SLOT_SPEC("errno", syserror_number_get, syserror_number_set),
    SCM_CLASS_SLOT_SPEC_END()
};

static ScmClassStaticSlotSpec sigerror_slots[] = {
    SCM_CLASS_SLOT_SPEC("signal", sigerror_signal_get, sigerror_signal_set),
    SCM_CLASS_SLOT_SPEC_END()
};

static ScmClassStaticSlotSpec unberror_slots[] = {
    SCM_CLASS_SLOT_SPEC("identifier", unberror_identifier_get, unberror_identifier_set),
    SCM_CLASS_SLOT_SPEC_END()
};

static ScmClassStaticSlotSpec readerror_slots[] = {
    SCM_CLASS_SLOT_SPEC("port", readerror_port_get, readerror_port_set),
    SCM_CLASS_SLOT_SPEC("line", readerror_line_get, readerror_line_set),
    SCM_CLASS_SLOT_SPEC("column", readerror_dummy_get, readerror_dummy_set),
    SCM_CLASS_SLOT_SPEC("position", readerror_dummy_get, readerror_dummy_set),
    SCM_CLASS_SLOT_SPEC("span", readerror_dummy_get, readerror_dummy_set),
    SCM_CLASS_SLOT_SPEC_END()
};

static ScmClassStaticSlotSpec porterror_slots[] = {
    SCM_CLASS_SLOT_SPEC("port", porterror_port_get, porterror_port_set),
    SCM_CLASS_SLOT_SPEC_END()
};

static ScmClassStaticSlotSpec encodingerror_slots[] = {
    SCM_CLASS_SLOT_SPEC("port", porterror_port_get, porterror_port_set),
    SCM_CLASS_SLOT_SPEC("offending-char",
                        porterror_offending_char_get,
                        porterror_offending_char_set),
    SCM_CLASS_SLOT_SPEC_END()
};

static ScmClassStaticSlotSpec invalidpositionerror_slots[] = {
    SCM_CLASS_SLOT_SPEC("port", porterror_port_get, porterror_port_set),
    SCM_CLASS_SLOT_SPEC("position",
                        porterror_position_get,
                        porterror_position_set),
    SCM_CLASS_SLOT_SPEC_END()
};

static ScmClassStaticSlotSpec conterror_slots[] = {
    SCM_CLASS_SLOT_SPEC("prompt-tag",
                        conterror_prompt_tag_get,
                        conterror_prompt_tag_set),
    SCM_CLASS_SLOT_SPEC_END()
};


/*------------------------------------------------------------
 * Compound conditions
 */

static ScmClass *compound_cpl[] = {
    SCM_CLASS_STATIC_PTR(Scm_CompoundConditionClass),
    SCM_CLASS_STATIC_PTR(Scm_SeriousConditionClass),
    CONDITION_CPL,
    NULL
};

static void compound_print(ScmObj obj, ScmPort *port,
                           ScmWriteContext *ctx SCM_UNUSED)
{
    ScmClass *k = Scm_ClassOf(obj);
    Scm_Printf(port, "#<%A", Scm_ShortClassName(k));
    ScmCompoundCondition *c = SCM_COMPOUND_CONDITION(obj);
    ScmObj cp;
    SCM_FOR_EACH(cp, c->conditions) {
        Scm_Printf(port, " %A", SCM_CAR(cp));
    }
    Scm_Printf(port, ">");
}

SCM_DEFINE_BASE_CLASS(Scm_CompoundConditionClass, ScmCompoundCondition,
                      compound_print, NULL, NULL,
                      compound_allocate, compound_cpl+2);
SCM_DEFINE_BASE_CLASS(Scm_SeriousCompoundConditionClass, ScmCompoundCondition,
                      compound_print, NULL, NULL,
                      compound_allocate, compound_cpl);

static ScmObj compound_allocate(ScmClass *klass, ScmObj initargs SCM_UNUSED)
{
    ScmCompoundCondition *e = SCM_NEW_INSTANCE(ScmCompoundCondition, klass);
    e->conditions = SCM_NIL;
    return SCM_OBJ(e);
}

ScmObj Scm_MakeCompoundCondition(ScmObj conditions)
{
    ScmObj h = SCM_NIL, t = SCM_NIL;
    int serious = FALSE;
    int nconds = Scm_Length(conditions);

    /* some boundary cases */
    if (nconds < 0) {
        Scm_Error("Scm_MakeCompoundCondition: list required, but got %S",
                  conditions);
    }
    if (nconds == 0) {
        return compound_allocate(SCM_CLASS_COMPOUND_CONDITION, SCM_NIL);
    }
    if (nconds == 1) {
        if (!SCM_CONDITIONP(SCM_CAR(conditions))) {
            Scm_Error("make-compound-condition: given non-condition object: %S", SCM_CAR(conditions));
        }
        return SCM_CAR(conditions);
    }

    /* collect conditions and creates compound one */
    ScmObj cp;
    SCM_FOR_EACH(cp, conditions) {
        ScmObj c = SCM_CAR(cp);
        if (!SCM_CONDITIONP(c)) {
            Scm_Error("make-compound-condition: given non-condition object: %S", SCM_CAR(cp));
        }
        if (SCM_SERIOUS_CONDITION_P(c)) {
            serious = TRUE;
        }

        if (SCM_COMPOUND_CONDITION_P(c)) {
            ScmCompoundCondition *cc = SCM_COMPOUND_CONDITION(c);
            SCM_APPEND(h, t, cc->conditions);
        } else {
            SCM_APPEND1(h, t, c);
        }
    }
    ScmObj cond = compound_allocate((serious?
                                     SCM_CLASS_COMPOUND_CONDITION :
                                     SCM_CLASS_SERIOUS_COMPOUND_CONDITION),
                                    SCM_NIL);
    SCM_COMPOUND_CONDITION(cond)->conditions = h;
    return cond;
}

/* Extract condition of type TYPE from a compound condition CONDITION.
   The returned condition may be of subtype of TYPE.
   CONDITION may be simple condition; in that case, CONDITION itself
   is returned iff condition is of a subtype of TYPE.
   This isn't the same as `extract-condition' of SRFI-35, which always
   returns a condition of the specified type; it also has different
   rule to refer to the slots.
 */
ScmObj Scm_ExtractSimpleCondition(ScmObj condition, ScmClass *type)
{
    ScmObj cs = (SCM_COMPOUND_CONDITION_P(condition)
                 ? SCM_COMPOUND_CONDITION(condition)->conditions
                 : SCM_LIST1(condition));
    ScmObj cp;
    SCM_FOR_EACH(cp, cs) {
        if (SCM_ISA(SCM_CAR(cp), type)) return SCM_CAR(cp);
    }
    return SCM_FALSE;
}

static ScmObj conditions_get(ScmCompoundCondition *obj)
{
    return obj->conditions;
}

static void   conditions_set(ScmCompoundCondition *obj, ScmObj conds)
{
    ScmObj cp;
    SCM_FOR_EACH(cp, conds) {
        if (!SCM_CONDITIONP(SCM_CAR(cp))) goto err;
    }
    if (!SCM_NULLP(cp)) {
      err:
        Scm_Error("conditions slot of a compound condition must be a list of conditions, but got %S", conds);
    }
    obj->conditions = conds;
}

static ScmClassStaticSlotSpec compound_slots[] = {
    SCM_CLASS_SLOT_SPEC("%conditions", conditions_get, conditions_set),
    SCM_CLASS_SLOT_SPEC_END()
};


/*
 * C-level Constructors & generic API
 */

/* actual class structure of thread exceptions are in ext/threads */
ScmObj Scm_MakeThreadException(ScmClass *klass, ScmVM *thread)
{
    ScmThreadException *e = SCM_NEW(ScmThreadException);
    SCM_SET_CLASS(e, klass);
    e->thread = thread;
    e->data = SCM_UNDEFINED;
    return SCM_OBJ(e);
}

ScmObj Scm_MakeError(ScmObj message)
{
    ScmError *e = SCM_ERROR(message_allocate(SCM_CLASS_ERROR, SCM_NIL));
    e->message = SCM_LIST2(message, message);
    return SCM_OBJ(e);
}

ScmObj Scm_MakeSystemError(ScmObj message, int en)
{
    ScmSystemError *e =
        SCM_SYSTEM_ERROR(syserror_allocate(SCM_CLASS_SYSTEM_ERROR, SCM_NIL));
    e->common.message = SCM_LIST2(message, message);
    e->error_number = en;
    return SCM_OBJ(e);
}

ScmObj Scm_MakeAssertionViolation(ScmObj message, ScmObj irritants)
{
    ScmAssertionViolation *e =
        SCM_ASSERTION_VIOLATION(message_allocate(SCM_CLASS_ASSERTION_VIOLATION,
                                                 SCM_NIL));
    e->message = Scm_Cons(message, irritants);
    return SCM_OBJ(e);
}

ScmObj Scm_MakeUnboundVariableError(ScmObj identifier)
{
    static ScmString *prefix = NULL;
    if (prefix == NULL) {
        prefix = SCM_STRING(SCM_MAKE_STR_IMMUTABLE("unbound variable"));
    }

    ScmUnboundVariableError *e =
        SCM_UNBOUND_VARIABLE_ERROR(unberror_allocate(SCM_CLASS_UNBOUND_VARIABLE_ERROR, SCM_NIL));
    ScmObj msg = Scm_Sprintf("%A: %S", SCM_OBJ(prefix), identifier);
    e->common.message = SCM_LIST3(msg, SCM_OBJ(prefix), identifier);
    e->identifier = identifier;
    return SCM_OBJ(e);
}

ScmObj Scm_MakeReadError(ScmObj message, ScmPort *port, int line)
{
    ScmReadError *e =
        SCM_READ_ERROR(readerror_allocate(SCM_CLASS_READ_ERROR, SCM_NIL));
    e->common.message = SCM_LIST2(message, message);
    e->port = port;
    e->line = line;
    return SCM_OBJ(e);
}

int Scm_ConditionHasType(ScmObj c, ScmObj k)
{
    if (!SCM_CONDITIONP(c)) return FALSE;
    if (!SCM_CLASSP(k)) return FALSE;
    if (!SCM_COMPOUND_CONDITION_P(c)) return SCM_ISA(c, SCM_CLASS(k));

    ScmObj cp;
    SCM_FOR_EACH(cp, SCM_COMPOUND_CONDITION(c)->conditions) {
        if (SCM_ISA(SCM_CAR(cp), SCM_CLASS(k))) return TRUE;
    }
    return FALSE;
}

ScmObj Scm_ConditionMessage(ScmObj c)
{
    if (SCM_MESSAGE_CONDITION_P(c)) {
        return message_get(SCM_MESSAGE_CONDITION(c));
    } else if (SCM_COMPOUND_CONDITION_P(c)) {
        ScmObj cp;
        SCM_FOR_EACH(cp, SCM_COMPOUND_CONDITION(c)->conditions) {
            if (SCM_MESSAGE_CONDITION_P(SCM_CAR(cp))) {
                return message_get(SCM_MESSAGE_CONDITION(SCM_CAR(cp)));
            }
        }
    }
    return SCM_FALSE;
}

/* Returns a ScmString representiong the 'type name' of the condition,
   suitable for the error message.  Because of personal preference
   and backward compatibility, I upcase the class name of the condition
   sans brackets.  If it is a composite condition, the component's typenames
   are joind with commas, excluding mixin conditions.
*/
ScmObj Scm_ConditionTypeName(ScmObj c)
{
    ScmObj sname;
    static SCM_DEFINE_STRING_CONST(cond_name_delim, ",", 1, 1);

    /* just a safety net */
    if (!SCM_CONDITIONP(c)) return SCM_MAKE_STR("(not a condition)");

    if (!SCM_COMPOUND_CONDITION_P(c)) {
        sname = Scm_ShortClassName(Scm_ClassOf(c));
    } else {
        ScmObj h = SCM_NIL, t = SCM_NIL, cp;
        SCM_FOR_EACH(cp, SCM_COMPOUND_CONDITION(c)->conditions) {
            ScmObj cc = SCM_CAR(cp);
            if (SCM_MIXIN_CONDITION_P(cc)) continue;
            SCM_APPEND1(h, t, Scm_ShortClassName(Scm_ClassOf(cc)));
        }
        if (SCM_NULLP(h)) {
            /* not usual, but tolerate */
            sname = Scm_ShortClassName(Scm_ClassOf(c));
        } else {
            sname = Scm_StringJoin(h, &cond_name_delim, SCM_STRING_JOIN_INFIX);
        }
    }

    ScmDString ds;
    Scm_DStringInit(&ds);
    ScmObj p = Scm_MakeInputStringPort(SCM_STRING(sname), TRUE);
    int ch;
    while ((ch = Scm_Getc(SCM_PORT(p))) != EOF) {
        Scm_DStringPutc(&ds, Scm_CharUpcase(ch));
    }
    return Scm_DStringGet(&ds, 0);
}

/*================================================================
 * Error handling
 *
 *   The interaction with dynamic environment of VM is handled by
 *   Scm_VMThrowException() in vm.c.   These routines provide
 *   application interface.
 *
 *   Note on Windows system error: Windows may return error code
 *   in two different ways; GetLastError for Windows API, and errno
 *   for posix-compatibility calls.   When Scm_SysError is called
 *   we don't know which error code to look at for sure.  As a
 *   convention, we always clear both error code after Scm_SysError
 *   and assumes whichever non-zero code indicates the actual error.
 *   To map to integer error code, we reverse the sign of Windows
 *   error code (Windows error code reserves full 32bit, but they don't
 *   seem to use over 2^31 for the time being).
 */

/* Double fault check
 * In order to aviod infinite loop when error throwing routine
 * throws an error, we use vm flag SCM_ERROR_BEING_HANDLED to
 * check that.  Ideally a common single API should handle it,
 * but for the time being, we add the check at the beginning
 * of Scm_*Error APIs.
 * The SCM_ERROR_BEING_HANDLED flag is cleared in Scm_VMThrowException().
 */
#define SCM_ERROR_DOUBLE_FAULT_CHECK(vm, msg)                           \
    do {                                                                \
        if (SCM_VM_RUNTIME_FLAG_IS_SET(vm, SCM_ERROR_BEING_HANDLED)) {  \
            ScmDString ds;                                              \
            Scm_DStringInit(&ds);                                       \
            Scm_DStringPutz(&ds, "Error occurred in error handler (", -1); \
            Scm_DStringPutz(&ds, msg, -1);                              \
            Scm_DStringPutz(&ds, ")", -1);                              \
            ScmObj e = Scm_MakeError(Scm_DStringGet(&ds, 0));           \
            Scm_VMThrowException(vm, e, SCM_RAISE_NON_CONTINUABLE);     \
        }                                                               \
        SCM_VM_RUNTIME_FLAG_SET(vm, SCM_ERROR_BEING_HANDLED);           \
    } while (0)

/* Common part to format error message passed as varargs.
 * ostr must be declared as ScmObj, and msg must be the last
 * arg of function before '...'.
 */
#define SCM_ERROR_MESSAGE_FORMAT(ostr, msg)             \
    do {                                                \
        ostr = Scm_MakeOutputStringPort(TRUE);          \
        va_list args__;                                 \
        va_start(args__, msg);                          \
        Scm_Vprintf(SCM_PORT(ostr), msg, args__, TRUE); \
        va_end(args__);                                 \
    } while (0)

/* Like SCM_ERROR_MESSAGE_FORMAT, but we also append system error message.
 * Need to pass errno, which should be fetched by get_errno() *before*
 * calling Scm_VM().
 */
#define SCM_SYSERROR_MESSAGE_FORMAT(ostr, msg, en)      \
    do {                                                \
        SCM_ERROR_MESSAGE_FORMAT(ostr, msg);            \
        ScmObj syserr = get_syserrmsg(en);              \
        SCM_PUTZ(": ", -1, ostr);                       \
        SCM_PUTS(syserr, ostr);                         \
    } while (0)

/*
 * C-like interface
 */
void Scm_Error(const char *msg, ...)
{
    ScmVM *vm = Scm_VM();
    SCM_ERROR_DOUBLE_FAULT_CHECK(vm, msg);
    ScmObj ostr;
    SCM_ERROR_MESSAGE_FORMAT(ostr, msg);
    ScmObj e = Scm_MakeError(Scm_GetOutputString(SCM_PORT(ostr), TRUE));
    Scm_VMThrowException(vm, e, SCM_RAISE_NON_CONTINUABLE);
    Scm_Panic("Scm_Error: Scm_VMThrowException returned.  something wrong.");
}

/*
 * Just for convenience to report a system error.   Add strerror() message
 * after the provided message.
 */
static ScmObj get_syserrmsg(int en)
{
    ScmObj syserr;
#if !defined(GAUCHE_WINDOWS)
    syserr = SCM_MAKE_STR_COPYING(strerror(en));
#else  /*GAUCHE_WINDOWS*/
    if (en < 0) {
      ScmObj ostr = Scm_MakeOutputStringPort(TRUE);
      LPTSTR msgbuf = NULL;
      const char *xmsgbuf;
      if (FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
                        FORMAT_MESSAGE_FROM_SYSTEM |
                        FORMAT_MESSAGE_IGNORE_INSERTS,
                        NULL,
                        -en,
                        MAKELANGID(LANG_ENGLISH, SUBLANG_DEFAULT),
                        (LPTSTR)&msgbuf,
                        0, NULL)) {
        xmsgbuf = SCM_WCS2MBS(msgbuf);
        SCM_PUTZ(xmsgbuf, -1, ostr);
      }
      LocalFree(msgbuf);
      Scm_Printf(SCM_PORT(ostr), "(error code = %d)", -en);
      syserr = Scm_GetOutputString(SCM_PORT(ostr), 0);
    } else {
      syserr = SCM_MAKE_STR_COPYING(strerror(en));
    }
#endif /*GAUCHE_WINDOWS*/
    return syserr;
}

static int get_errno(void)
{
#if !defined(GAUCHE_WINDOWS)
    return errno;
#else  /*GAUCHE_WINDOWS*/
    int en;

    if (errno == 0) {
        en = -(int)GetLastError();
    } else {
        en = errno;             /* NB: MSDN says we should use _get_errno,
                                   but MinGW doesn't seem to have it yet.*/
    }

    /* Reset the error code, so that we can find which is the actual
       error code in the next occasion. */
    errno = 0;                  /* NB: MSDN says we should use _set_errno,
                                   but MinGW doesn't seem to have it yet. */
    SetLastError(0);

    return en;
#endif /*GAUCHE_WINDOWS*/
}

void Scm_SysError(const char *msg, ...)
{
    int en = get_errno();       /* must take this before Scm_VM() */
    ScmVM *vm = Scm_VM();
    SCM_ERROR_DOUBLE_FAULT_CHECK(vm, msg);

    ScmObj ostr;
    SCM_SYSERROR_MESSAGE_FORMAT(ostr, msg, en);
    ScmObj e = Scm_MakeSystemError(Scm_GetOutputString(SCM_PORT(ostr), TRUE), en);
    Scm_VMThrowException(vm, e, SCM_RAISE_NON_CONTINUABLE);
    Scm_Panic("Scm_Error: Scm_VMThrowException returned.  something wrong.");
}

/*
 * A convenience function to raise argument-type-violation errors.
 * Right now it raises <error>, but it'll be changed once we adopt R6RS
 * (The draft R6RS defines 'contract violation' in such cases).
 */
void Scm_TypeError(const char *what, const char *expected, ScmObj got)
{
    ScmObj ostr = Scm_MakeOutputStringPort(TRUE);
    Scm_Printf(SCM_PORT(ostr),
               "%s is supposed to be of type %s, but got %S",
               what, expected, got);
    ScmObj msg = Scm_GetOutputString(SCM_PORT(ostr), TRUE);
    ScmObj e = Scm_MakeAssertionViolation(msg, SCM_LIST1(got));
    Scm_VMThrowException(Scm_VM(), SCM_OBJ(e), SCM_RAISE_NON_CONTINUABLE);
    Scm_Panic("Scm_TypeError: Scm_VMThrowException returned.  something wrong.");
}

/*
 * A convenience function to raise <assertion-violation> with printf-style
 * message formatting.
 */
void Scm_AssertionError(ScmObj irritants, const char *msg, ...)
{
    ScmVM *vm = Scm_VM();
    ScmObj ostr;
    SCM_ERROR_MESSAGE_FORMAT(ostr, msg);
    ScmObj e =
        Scm_MakeAssertionViolation(Scm_GetOutputString(SCM_PORT(ostr), TRUE),
                                   irritants);
    Scm_VMThrowException(vm, e, SCM_RAISE_NON_CONTINUABLE);
    Scm_Panic("Scm_AssertionError: Scm_VMThrowException returned.  something wrong.");
}

/*
 * A convenience function to raise port-related errors.
 * It creates either one of port error instance,
 * depending on the 'reason' argument.
 * If errno isn't zero, it also creates a <system-error> and throws
 * a compound condition of both.
 */
void raise_port_error(ScmVM *vm, ScmPort *port, int reason, ScmObj auxinfo,
                      int orig_errno, const char *msg, va_list args)
{
    Scm_SetPortErrorOccurred(port, TRUE);

    ScmObj ostr = Scm_MakeOutputStringPort(TRUE);
    Scm_Vprintf(SCM_PORT(ostr), msg, args, TRUE);
    ScmObj syserr = get_syserrmsg(orig_errno);
    Scm_Putz(": ", -1, SCM_PORT(ostr));
    Scm_Puts(SCM_STRING(syserr), SCM_PORT(ostr));
    ScmObj smsg = Scm_GetOutputString(SCM_PORT(ostr), TRUE);

    ScmClass *peclass;
    switch (reason) {
    case SCM_PORT_ERROR_INPUT:
        peclass = SCM_CLASS_IO_READ_ERROR; break;
    case SCM_PORT_ERROR_OUTPUT:
        peclass = SCM_CLASS_IO_WRITE_ERROR; break;
    case SCM_PORT_ERROR_CLOSED:
        peclass = SCM_CLASS_IO_CLOSED_ERROR; break;
    case SCM_PORT_ERROR_UNIT:
        peclass = SCM_CLASS_IO_UNIT_ERROR; break;
    case SCM_PORT_ERROR_DECODING:
        peclass = SCM_CLASS_IO_DECODING_ERROR; break;
    case SCM_PORT_ERROR_ENCODING:
        peclass = SCM_CLASS_IO_ENCODING_ERROR; break;
    case SCM_PORT_ERROR_SEEK:
        peclass = SCM_CLASS_PORT_ERROR; break;
    case SCM_PORT_ERROR_INVALID_POSITION:
        peclass = SCM_CLASS_IO_INVALID_POSITION_ERROR; break;
    default:
        peclass = SCM_CLASS_PORT_ERROR; break;
    }

    ScmObj pe = porterror_allocate(peclass, SCM_NIL);
    SCM_ERROR(pe)->message = SCM_LIST2(smsg, smsg);
    SCM_PORT_ERROR(pe)->port = port;
    SCM_PORT_ERROR(pe)->auxinfo = auxinfo;

    ScmObj e = pe;
    if (orig_errno != 0) {
        e = Scm_MakeCompoundCondition(SCM_LIST2(Scm_MakeSystemError(smsg, orig_errno),
                                                pe));
    }
    Scm_VMThrowException(vm, e, SCM_RAISE_NON_CONTINUABLE);
}

void Scm_PortErrorWithAux(ScmPort *port, int reason, ScmObj auxinfo,
                          const char *msg, ...)
{
    int en = get_errno();       /* must take this before Scm_VM() */
    ScmVM *vm = Scm_VM();
    SCM_ERROR_DOUBLE_FAULT_CHECK(vm, msg);

    va_list ap;
    va_start(ap, msg);
    raise_port_error(vm, port, reason, auxinfo, en, msg, ap);
    va_end(ap);
    Scm_Panic("Scm_Error: Scm_VMThrowException returned.  something wrong.");
}

void Scm_PortError(ScmPort *port, int reason, const char *msg, ...)
{
    int en = get_errno();       /* must take this before Scm_VM() */
    ScmVM *vm = Scm_VM();
    SCM_ERROR_DOUBLE_FAULT_CHECK(vm, msg);

    va_list ap;
    va_start(ap, msg);
    raise_port_error(vm, port, reason, SCM_NIL, en, msg, ap);
    va_end(ap);
    Scm_Panic("Scm_Error: Scm_VMThrowException returned.  something wrong.");
}

/*
 * Just print warning
 *  TODO: customize behavior
 */

void Scm_Warn(const char *msg, ...)
{
    if (Scm_GetEnv("GAUCHE_SUPPRESS_WARNING") != NULL) return;
    va_list args;
    va_start(args, msg);
    Scm_Printf(SCM_CURERR, "WARNING: %A\n", Scm_Vsprintf(msg, args, TRUE));
    Scm_Flush(SCM_CURERR);
    va_end(args);
}

/* OBSOLETED: 'warn' is now in Scheme. */
void Scm_FWarn(ScmString *fmt SCM_UNUSED, ScmObj args SCM_UNUSED)
{
    Scm_Error("Scm_FWarn is obsoleted");
}

/*
 * General exception raising
 */

/* An external API to hide Scm_VMThrowException. */
ScmObj Scm_Raise(ScmObj condition, u_long flags)
{
    return Scm_VMThrowException(Scm_VM(), condition, flags);
}


/* A convenient API---allows to call user-defined condition easily,
   even the condition type is defined in Scheme.  For example:

   Scm_RaiseCondition(SCM_SYMBOL_VALUE("mymodule", "<my-error>"),
                      "error-type", SCM_INTERN("fatal"),
                      "error-code", SCM_MAKE_INT(3),
                      SCM_RAISE_CONDITION_MESSAGE,
                      "Fatal error occurred at %S", current_proc);

   roughly corresponds to the Scheme code:

   (raise (condition
            (<my-error> (error-type 'fatal)
                        (error-code 3)
                        (message (format "Fatal error occurred at ~s"
                                         current_proc)))))

   This function isn't very efficient; but sometimes you want the convenience
   more, right?

   The argument list format:

     <condition-type> {<string-slot-name> <value>}* <terminator>

     <terminator> : SCM_RAISE_CONDITION_MESSAGE <fmtstr> <fmtarg> ...
                  | NULL
*/

ScmObj Scm_RaiseCondition(ScmObj condition_type, ...)
{
    ScmObj argh = SCM_NIL, argt = SCM_NIL;
    va_list ap;

    if (!SCM_CLASSP(condition_type)
        || !Scm_SubclassP(SCM_CLASS(condition_type), SCM_CLASS_CONDITION)) {
        /* If we don't get a condition type, fallback to a normal error. */
        condition_type = SCM_OBJ(SCM_CLASS_ERROR);
    }
    SCM_APPEND1(argh, argt, condition_type);
    va_start(ap, condition_type);
    for (;;) {
        const char *key = va_arg(ap, const char *);
        if (key == NULL) {
            break;
        } else if (key == SCM_RAISE_CONDITION_MESSAGE) {
            const char *msg = va_arg(ap, const char*);
            ScmObj ostr = Scm_MakeOutputStringPort(TRUE);
            Scm_Vprintf(SCM_PORT(ostr), msg, ap, TRUE);
            SCM_APPEND1(argh, argt, SCM_MAKE_KEYWORD("message"));
            SCM_APPEND1(argh, argt, Scm_GetOutputString(SCM_PORT(ostr), 0));
            break;
        } else {
            ScmObj arg = va_arg(ap, ScmObj);
            SCM_APPEND1(argh, argt, SCM_MAKE_KEYWORD(key));
            SCM_APPEND1(argh, argt, arg);
        }
    }
    va_end(ap);
    return Scm_ApplyRec(SCM_SYMBOL_VALUE("gauche", "error"), argh);
}

/*
 * Show stack or call trace.
 *   stacklite - return value of Scm_VMGetStackLite or Scm_VMGetCallTraceLite
 *   maxdepth - maximum # of entries to be shown.
 *              0 to use the default.  -1 for unlimited.
 *   skip     - ignore this number of frames.  Useful to call this from
 *              a Scheme error handling routine, in order to skip the
 *              frames of the handler itself.
 *   offset   - add this to the frame number.  Useful to show a middle part
 *              of frames only, by combining the skip parameter.
 *   format   - SCM_STACK_TRACE_FORMAT_* enum value.  No longer used.
 */
void Scm_ShowStackTrace(ScmPort *out, ScmObj stacklite,
                        int maxdepth, int skip, int offset,
                        int format SCM_UNUSED)
{
    static ScmObj show_stack_trace = SCM_UNDEFINED;
    SCM_BIND_PROC(show_stack_trace,
                  "%show-stack-trace",
                  Scm_GaucheInternalModule());
    Scm_ApplyRec5(show_stack_trace, stacklite, SCM_OBJ(out),
                  SCM_MAKE_INT(maxdepth), SCM_MAKE_INT(skip),
                  SCM_MAKE_INT(offset));
}

/* Dump stack trace.  Called from the default error reporter.
   Also intended to be called from the debugger, so we allow vm to be NULL
   to mean the current VM, and port to be NULL for the current error port. */
void Scm_DumpStackTrace(ScmVM *vm, ScmPort *port)
{
    if (vm == NULL) vm = Scm_VM();
    if (port == NULL) port = SCM_CURERR;
    ScmObj stack = Scm_VMGetStackLite(vm);
    ScmObj calls = Scm_VMGetCallTraceLite(vm);
    SCM_PUTZ("Stack Trace:\n", -1, port);
    SCM_PUTZ("_______________________________________\n", -1, port);
    Scm_ShowStackTrace(port, stack, 0, 0, 0, 0);

    /* display additional stack trace */
    if (vm->errorCont) {
        ScmContFrame *vmcont = vm->cont;
        ScmCompiledCode *vmbase = vm->base;
        vm->cont = vm->errorCont;
        vm->base = NULL;
        ScmObj stack2 = Scm_VMGetStackLite(vm);
        if (!Scm_EqualP(stack, stack2)) {
            SCM_PUTZ("Stack Trace on Error:\n", -1, port);
            SCM_PUTZ("_______________________________________\n", -1, port);
            Scm_ShowStackTrace(port, stack2, 0, 0, 0, 0);
        }
        vm->cont = vmcont;
        vm->base = vmbase;
        /* reset information for additional stack trace */
        vm->errorCont = NULL;
    }

    if (SCM_PAIRP(calls)) {
        SCM_PUTZ("Call Trace:\n", -1, port);
        SCM_PUTZ("_______________________________________\n", -1, port);
        Scm_ShowStackTrace(port, calls, 0, 0, 0, 0);
    }
    SCM_FLUSH(port);
}

/*
 * Default error reporter
 */

/* The default procedure to display the header of error message.
   E is a thrown condition, not necessarily an error object.

   The actual operation is written in Scheme (libexc.scm).  However,
   we can't use that before the infrastructure is fully booted; so
   this routine has a fallback which will be used only during initialization.
*/
static void Scm_PrintDefaultErrorHeading(ScmObj e, ScmPort *out)
{
    if (Scm_RuntimeState() >= SCM_RUNTIME_INITIALIZED) {
        static ScmObj print_default_error_heading = SCM_UNDEFINED;
        SCM_BIND_PROC(print_default_error_heading,
                      "print-default-error-heading",
                      Scm_GaucheModule());
        Scm_ApplyRec2(print_default_error_heading, e, SCM_OBJ(out));
        static ScmObj print_additional_error_heading = SCM_UNDEFINED;
        SCM_BIND_PROC(print_additional_error_heading,
                      "print-additional-error-heading",
                      Scm_GaucheModule());
        Scm_ApplyRec2(print_additional_error_heading, e, SCM_OBJ(out));
    } else {
        /* Error during initialization. */
        if (SCM_CONDITIONP(e)) {
            Scm_Printf(out, "*** %A: %A\n",
                       Scm_ConditionTypeName(e),
                       Scm_ConditionMessage(e));
        } else {
            Scm_Printf(out, "*** ERROR: Unhandled condition: %S\n", e);
        }
    }
}

/* We treat out == #f or #t just like 'format' - #t for the current output
   port, and #f for string port.  If it's output port, use it.  For any
   other objects, we use current error port.  This permissive behavior is
   intentional - report-error is usually called during error handling,
   and raising an error there masks the original error.  However, the
   caller should explicitly pass SCM_OBJ(SCM_CURERR) if that's what it intends,
   instead of relying on the permissive behavior.
*/
ScmObj Scm_ReportError(ScmObj e, ScmObj out)
{
    ScmVM *vm = Scm_VM();
    ScmPort *port = SCM_CURERR;
    if (SCM_FALSEP(out)) {
        port = SCM_PORT(Scm_MakeOutputStringPort(TRUE));
    } else if (SCM_TRUEP(out)) {
        port = SCM_CUROUT;
    } else if (SCM_OPORTP(out)) {
        port = SCM_PORT(out);
    }
    Scm_PrintDefaultErrorHeading(e, port);
    Scm_DumpStackTrace(vm, port);
    if (SCM_FALSEP(out)) return Scm_GetOutputString(SCM_PORT(port), 0);
    else return SCM_UNDEFINED;
}

/*
 * Initialization
 */
void Scm__InitExceptions(void)
{
    ScmModule *mod = Scm_GaucheModule();

    ScmObj mes_ser_supers
        = SCM_LIST2(SCM_OBJ(SCM_CLASS_MESSAGE_CONDITION),
                    SCM_OBJ(SCM_CLASS_SERIOUS_CONDITION));
    ScmObj com_ser_supers
        = SCM_LIST2(SCM_OBJ(SCM_CLASS_COMPOUND_CONDITION),
                    SCM_OBJ(SCM_CLASS_SERIOUS_CONDITION));

    Scm_InitStaticClassWithMeta(SCM_CLASS_CONDITION,
                                "<condition>",
                                mod, NULL, SCM_FALSE, NULL, 0);
    ScmClass *cond_meta = Scm_ClassOf(SCM_OBJ(SCM_CLASS_CONDITION));
    Scm_InitStaticClassWithMeta(SCM_CLASS_SERIOUS_CONDITION,
                                "<serious-condition>",
                                mod, cond_meta, SCM_FALSE, NULL, 0);
    Scm_InitStaticClassWithMeta(SCM_CLASS_MESSAGE_CONDITION,
                                "<message-condition>",
                                mod, cond_meta, SCM_FALSE, message_slots, 0);
    Scm_InitStaticClassWithMeta(SCM_CLASS_MIXIN_CONDITION,
                                "<mixin-condition>",
                                mod, cond_meta, SCM_FALSE, NULL, 0);

    Scm_InitStaticClassWithMeta(SCM_CLASS_ERROR,
                                "<error>",
                                mod, cond_meta, mes_ser_supers,
                                message_slots, 0);
    Scm_InitStaticClassWithMeta(SCM_CLASS_SYSTEM_ERROR,
                                "<system-error>",
                                mod, cond_meta, SCM_FALSE,
                                syserror_slots, 0);
    Scm_InitStaticClassWithMeta(SCM_CLASS_ASSERTION_VIOLATION,
                                "<assertion-violation>",
                                mod, cond_meta, SCM_FALSE,
                                message_slots, 0);
    Scm_InitStaticClassWithMeta(SCM_CLASS_UNHANDLED_SIGNAL_ERROR,
                                "<unhandled-signal-error>",
                                mod, cond_meta, SCM_FALSE,
                                sigerror_slots, 0);
    Scm_InitStaticClassWithMeta(SCM_CLASS_UNBOUND_VARIABLE_ERROR,
                                "<unbound-variable-error>",
                                mod, cond_meta, SCM_FALSE,
                                unberror_slots, 0);
    Scm_InitStaticClassWithMeta(SCM_CLASS_READ_ERROR,
                                "<read-error>",
                                mod, cond_meta, SCM_FALSE,
                                readerror_slots, 0);
    Scm_InitStaticClassWithMeta(SCM_CLASS_IO_ERROR,
                                "<io-error>",
                                mod, cond_meta, SCM_FALSE,
                                NULL, 0);
    Scm_InitStaticClassWithMeta(SCM_CLASS_PORT_ERROR,
                                "<port-error>",
                                mod, cond_meta, SCM_FALSE,
                                porterror_slots, 0);
    Scm_InitStaticClassWithMeta(SCM_CLASS_IO_READ_ERROR,
                                "<io-read-error>",
                                mod, cond_meta, SCM_FALSE,
                                porterror_slots, 0);
    Scm_InitStaticClassWithMeta(SCM_CLASS_IO_WRITE_ERROR,
                                "<io-write-error>",
                                mod, cond_meta, SCM_FALSE,
                                porterror_slots, 0);
    Scm_InitStaticClassWithMeta(SCM_CLASS_IO_CLOSED_ERROR,
                                "<io-closed-error>",
                                mod, cond_meta, SCM_FALSE,
                                porterror_slots, 0);
    Scm_InitStaticClassWithMeta(SCM_CLASS_IO_UNIT_ERROR,
                                "<io-unit-error>",
                                mod, cond_meta, SCM_FALSE,
                                porterror_slots, 0);
    Scm_InitStaticClassWithMeta(SCM_CLASS_IO_DECODING_ERROR,
                                "<io-decoding-error>",
                                mod, cond_meta, SCM_FALSE,
                                porterror_slots, 0);
    Scm_InitStaticClassWithMeta(SCM_CLASS_IO_ENCODING_ERROR,
                                "<io-encoding-error>",
                                mod, cond_meta, SCM_FALSE,
                                encodingerror_slots, 0);
    Scm_InitStaticClassWithMeta(SCM_CLASS_IO_INVALID_POSITION_ERROR,
                                "<io-invalid-position-error>",
                                mod, cond_meta, SCM_FALSE,
                                invalidpositionerror_slots, 0);

    Scm_InitStaticClassWithMeta(SCM_CLASS_CONTINUATION_ERROR,
                                "<continuation-violation>",
                                mod, cond_meta, SCM_FALSE,
                                conterror_slots, 0);

    Scm_InitStaticClassWithMeta(SCM_CLASS_COMPOUND_CONDITION,
                                "<compound-condition>",
                                mod, cond_meta, SCM_FALSE,
                                compound_slots, 0);
    Scm_InitStaticClassWithMeta(SCM_CLASS_SERIOUS_COMPOUND_CONDITION,
                                "<serious-compound-condition>",
                                mod, cond_meta, com_ser_supers,
                                compound_slots, 0);

    sym_offending_char = SCM_INTERN("offending-char");
    sym_position = SCM_INTERN("position");
}
