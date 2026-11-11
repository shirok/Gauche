/*
 * moduleP.h - Module private API
 *
 *   Copyright (c) 2014-2025  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_PRIV_MODULEP_H
#define GAUCHE_PRIV_MODULEP_H

/* Module */
struct ScmModuleRec {
    SCM_HEADER;
    ScmObj name;                /* symbol or #f */
    ScmObj imported;            /* list of imported modules. */
    int    exportAll;           /* TRUE if (export-all) */
    ScmObj parents;             /* direct parent modules */
    ScmObj mpl;                 /* module precedence list */
    ScmObj depended;            /* list of modules that are depended by this
                                   module for compilation */
    ScmHashTable *internal;     /* Symbol -> GLoc, looked up from this module
                                   itself and inherited ones */
    ScmHashTable *external;     /* Symbol -> GLoc, looked up from the modules
                                   that imports this module.  This table only
                                   holds exported symbols (it may have
                                   different name if export renaming is in
                                   effect) */
    ScmObj exports;             /* Cached exports list.  This does not include
                                   exported symbols from inherited modules.
                                   #f indicates cache is invalidated and needs
                                   to be rebuilt.
                                   May contain (<symbol>) to indicate <symbol>
                                   is a hidden export, hiding the ancestor's
                                   export of <symbol>.  */
    ScmObj origin;              /* if this module is an anonymous wrapper
                                   module, this holds a original module.
                                   this isn't used for resolving bindings,
                                   but used to avoid duplicating imports. */
    ScmObj prefix;              /* if symbol, all bindings in this module
                                   appear to have the prefix.  used in an
                                   anonymous wrapper modules. */
    ScmObj info;                /* alist of metainfo; e.g.
                                   (source-info . <string>) */
    int    sealed;              /* if true, no modification is allowed */
    int    placeholding;        /* if true, this module is created just for
                                   hygienic identifiers, and the module body
                                   isn't loaded. */
};

SCM_EXTERN ScmModule *Scm__KeywordModule(void);
SCM_EXTERN ScmModule *Scm__GaucheKeywordModule(void);
SCM_EXTERN ScmModule *Scm__RequireBaseModule(void);

SCM_EXTERN ScmObj Scm__MakeWrapperModule(ScmModule *origin, ScmObj prefix);

SCM_EXTERN ScmGloc   *Scm__IdentifierToBoundGloc(ScmIdentifier*);

ScmObj Scm__GetModuleExports(ScmModule *mod);

#endif /*GAUCHE_PRIV_MODULEP_H*/
