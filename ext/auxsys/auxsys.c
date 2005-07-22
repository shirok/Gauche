/*
 * auxsys.c - Auxiliary system functions
 *
 *   Copyright (c) 2000-2004 Shiro Kawai, All rights reserved.
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
 *  $Id: auxsys.c,v 1.5 2005-07-22 09:26:54 shirok Exp $
 */

#include <gauche.h>
#include <gauche/extend.h>
#include "auxsysconf.h"

/*
 * Some emulation stuff for Windows/MinGW
 */
#ifdef __MINGW32__

const char *getlogin(void)
{
    static char buf[256]; /* this isn't thread-safe, but getlogin() is
			     inherently thread-unsafe call anyway */
    DWORD size = sizeof(buf);
    BOOL r;
    r = GetUserName(buf, &size);
    if (r) {
	return buf;
    } else {
	return NULL;
    }
}

#endif /*__MINGW32__*/

/*
 * Initialization
 */

extern void Scm_Init_auxsyslib(ScmModule *mod);

void Scm_Init_auxsys(void)
{
    ScmModule *mod;

    SCM_INIT_EXTENSION(auxsys);
    mod = SCM_FIND_MODULE("gauche.auxsys", SCM_FIND_MODULE_CREATE);
    Scm_Init_auxsyslib(mod);
}
