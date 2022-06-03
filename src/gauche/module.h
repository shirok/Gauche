/*
 * module.h - Modules
 *
 *   Copyright (c) 2000-2022  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_MODULE_H
#define GAUCHE_MODULE_H

/*
 * A module keeps "toplevel environment", which maps names of free
 * variables (symbols) to a location (GLOCs).
 *
 * Note on anonymous wrapper modules:  Sometimes, the system
 * implicitly creates an anonymous module to wrap an original
 * module to manipulate visibility of bindings.
 */

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

#define SCM_MODULE(obj)       ((ScmModule*)(obj))
#define SCM_MODULEP(obj)      SCM_XTYPEP(obj, SCM_CLASS_MODULE)

SCM_CLASS_DECL(Scm_ModuleClass);
#define SCM_CLASS_MODULE     (&Scm_ModuleClass)

SCM_EXTERN ScmObj Scm_MakeModule(ScmSymbol *name, int error_if_exists);

/* Flags for Scm_FindBinding (F), MakeBinding (M)
   and Scm_GlobalVariableRef (R)*/
enum {
    SCM_BINDING_STAY_IN_MODULE = (1L<<0), /*(F,R) do not search parent/imported*/
    SCM_BINDING_CONST = (1L<<1),          /*(M) constant binding */
    SCM_BINDING_INLINABLE = (1L<<2),      /*(M) inlinable binding */
    SCM_BINDING_EXTERNAL = (1L<<3),       /*(F) only search externally visible
                                            bindings, as if we're importing
                                            the module.  Currently used to
                                            create alias binding. */
    SCM_BINDING_DUMMY = (1L<<4)           /*(M) inline or constant binding,
                                            inserted only during compilation.
                                            this is a dummy binding only to be
                                            recognized by the compiler to expand
                                            the following expressions. The
                                            binding is ot be overwritten by
                                            the proper one when the compiled
                                            module is loaded, so we won't warn
                                            the overwriting. */
};

SCM_EXTERN ScmGloc *Scm_FindBinding(ScmModule *module, ScmSymbol *symbol,
                                    int flags);
SCM_EXTERN ScmGloc *Scm_MakeBinding(ScmModule *module, ScmSymbol *symbol,
                                    ScmObj value, int flags);
SCM_EXTERN ScmObj Scm_GlobalVariableRef(ScmModule *module,
                                        ScmSymbol *symbol,
                                        int flags);
SCM_EXTERN void   Scm_HideBinding(ScmModule *module, ScmSymbol *symbol);
SCM_EXTERN int    Scm_AliasBinding(ScmModule *target, ScmSymbol *targetName,
                                   ScmModule *origin, ScmSymbol *originName);

/* Convenience API.  Wrapper of Scm_MakeBinding. */
SCM_EXTERN ScmObj Scm_Define(ScmModule *module,
                             ScmSymbol *symbol,
                             ScmObj value);
SCM_EXTERN ScmObj Scm_DefineConst(ScmModule *module,
                                  ScmSymbol *symbol,
                                  ScmObj value);


SCM_EXTERN ScmObj Scm_ExtendModule(ScmModule *module, ScmObj supers);
SCM_EXTERN ScmObj Scm_ImportModule(ScmModule *module, ScmObj imported,
                                   ScmObj prefix, u_long flags);
SCM_EXTERN ScmObj Scm_ImportModules(ScmModule *module, ScmObj list);/*deprecated*/
SCM_EXTERN ScmObj Scm_ExportSymbols(ScmModule *module, ScmObj list);
SCM_EXTERN ScmObj Scm_ExportAll(ScmModule *module);
SCM_EXTERN ScmModule *Scm_FindModule(ScmSymbol *name, int flags);
SCM_EXTERN ScmObj Scm_AllModules(void);
SCM_EXTERN void   Scm_SelectModule(ScmModule *mod);
SCM_EXTERN ScmObj Scm_ModuleExports(ScmModule *mod);

SCM_EXTERN void   Scm_ModuleSeal(ScmModule *mod);

/* Flags for Scm_FindModule
   NB: Scm_FindModule's second arg has been changed since 0.8.6;
   before, it was just a boolean value to indicate whether a new
   module should be created (TRUE) or not (FALSE).  We added a
   new flag value to make Scm_FindModule raises an error if the named
   module doesn't exist.  This change should be transparent as far
   as the caller's using Gauche's definition of TRUE. */
enum {
    SCM_FIND_MODULE_CREATE = (1L<<0), /* Create if there's no named module */
    SCM_FIND_MODULE_QUIET  = (1L<<1), /* Do not signal an error if there's no
                                         named module, but return NULL
                                         instead. */
    SCM_FIND_MODULE_PLACEHOLDING = (1L<<2) /* If module is newly created,
                                              mark it as placeholding. */
};

#define SCM_FIND_MODULE(name, flags) \
    Scm_FindModule(SCM_SYMBOL(SCM_INTERN(name)), flags)

SCM_EXTERN ScmObj Scm_ModuleNameToPath(ScmSymbol *name);
SCM_EXTERN ScmObj Scm_PathToModuleName(ScmString *path);

SCM_EXTERN ScmModule *Scm_NullModule(void);
SCM_EXTERN ScmModule *Scm_SchemeModule(void);
SCM_EXTERN ScmModule *Scm_GaucheModule(void);
SCM_EXTERN ScmModule *Scm_UserModule(void);
SCM_EXTERN ScmModule *Scm_CurrentModule(void);

#define SCM_DEFINE(module, cstr, val)           \
    Scm_Define(SCM_MODULE(module),              \
               SCM_SYMBOL(SCM_INTERN(cstr)),    \
               SCM_OBJ(val))

/* OBSOLETED */
#define Scm_SymbolValue(m, s) Scm_GlobalVariableRef(m, s, FALSE)
/* OBSOLETED */
#define SCM_SYMBOL_VALUE(module_name, symbol_name)                      \
    Scm_SymbolValue(SCM_FIND_MODULE(module_name, 0),                    \
                    SCM_SYMBOL(SCM_INTERN(symbol_name)))


#endif /*GAUCHE_MODULE_H*/
