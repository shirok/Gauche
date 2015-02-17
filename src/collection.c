/*
 * collection.c - common stuff for collection & other aggregate types.
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

/*
 * Class stuff
 */

SCM_DEFINE_ABSTRACT_CLASS(Scm_CollectionClass, SCM_CLASS_DEFAULT_CPL);
SCM_DEFINE_ABSTRACT_CLASS(Scm_SequenceClass, SCM_CLASS_COLLECTION_CPL);
SCM_DEFINE_ABSTRACT_CLASS(Scm_DictionaryClass, SCM_CLASS_COLLECTION_CPL);
SCM_DEFINE_ABSTRACT_CLASS(Scm_OrderedDictionaryClass, Scm__OrderedDictionaryCPL+1);

ScmClass *Scm__OrderedDictionaryCPL[] = {
    SCM_CLASS_STATIC_PTR(Scm_OrderedDictionaryClass),
    SCM_CLASS_STATIC_PTR(Scm_SequenceClass),
    SCM_CLASS_STATIC_PTR(Scm_DictionaryClass),
    SCM_CLASS_STATIC_PTR(Scm_CollectionClass),
    SCM_CLASS_STATIC_PTR(Scm_TopClass),
    NULL
};

ScmClass *Scm__SequenceCPL[] = {
    SCM_CLASS_STATIC_PTR(Scm_SequenceClass),
    SCM_CLASS_STATIC_PTR(Scm_CollectionClass),
    SCM_CLASS_STATIC_PTR(Scm_TopClass),
    NULL
};

/*
 * Some internal utilities
 */

intptr_t Scm__CheckDictValue(ScmObj val, const char *file, int line)
{
    if (val == NULL || SCM_UNBOUNDP(val)) {
        Scm_Panic("[internal] attempted to set an invalid ScmObj value (%p) as a value of a dictionary, at %s:%d", val, file, line);
    }
    return (intptr_t)val;
}

/*
 * Initialization
 */
void Scm__InitCollection(void)
{
    ScmModule *mod = Scm_GaucheModule();

    Scm_InitStaticClass(&Scm_CollectionClass, "<collection>",
                        mod, NULL, 0);
    Scm_InitStaticClass(&Scm_SequenceClass, "<sequence>",
                        mod, NULL, 0);
    Scm_InitStaticClass(&Scm_DictionaryClass, "<dictionary>",
                        mod, NULL, 0);
    Scm_InitStaticClass(&Scm_OrderedDictionaryClass, "<ordered-dictionary>",
                        mod, NULL, 0);
}
