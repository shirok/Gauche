/*
 * auxsys.c - Auxiliary system functions
 *
 *   Copyright (c) 2000-2007  Shiro Kawai  <shiro@acm.org>
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
 *  $Id: auxsys.c,v 1.8 2007-03-28 09:32:33 shirok Exp $
 */

#include <gauche.h>
#include <gauche/extend.h>
#include "auxsysconf.h"

#ifndef __MINGW32__
#  include <sys/times.h>
#endif

/*===============================================================
 * Environment
 */

/* POSIX has putenv but not setenv.  However, the XPG spec of
   putenv is broken w.r.t freeing the passed string; there's no
   way to know when the pointer can be freed.
   Some Unix appears to violate the spec and copies the passed pointer,
   which solves the leak, but the code will break if we bring it to
   the platform with "proper" putenv(3).

   So we do not trust putenv(3).

   Our strategy: If the system has setenv(3) we use it, since we know
   it copies the passed string.  If the system doesn't have setenv(3),
   we fall back to putenv(3) and let it leak.
*/

void Scm_SetEnv(const char *name, const char *value, int overwrite)
{
#if defined(HAVE_SETENV)
    int r;
    
    SCM_SYSCALL(r, setenv(name, value, overwrite));
    if (r < 0) Scm_SysError("setenv failed on '%s=%s'", name, value);
#elif defined(HAVE_PUTENV)
    char *nameval;
    int nlen, vlen, r;
    
    if (!overwrite) {
        /* check the existence of NAME first. */
        if (getenv(name) != NULL) return;
    }
    nlen = strlen(name);
    vlen = strlen(value);
    /* we need malloc, since the pointer will be owned by the system */
    nameval = (char*)malloc(nlen+vlen+2);
    if (nameval == NULL) {
        Scm_Error("sys-setenv: out of memory");
    }
    strcpy(nameval, name);
    strcpy(nameval+nlen, "=");
    strcpy(nameval+nlen+1, value);
    SCM_SYSCALL(r, putenv(nameval));
    if (r < 0) Scm_SysError("putenv failed on '%s'", nameval);
#else /* !HAVE_SETENV && !HAVE_PUTENV */
    /* We can't do much.  we may replace environ by ourselves, but
       it is unlikely that the system have extern environ and not putenv.
    */
    return;
#endif
}


/*==============================================================
 * Emulation stuff for Windows/MinGW
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

clock_t times(struct tms *info)
{
    HANDLE process = GetCurrentProcess();
    FILETIME ctime, xtime, utime, stime;
    int64_t val;
    const int factor = 10000000/CLK_TCK;
    const int bias   = factor/2;

    if (!GetProcessTimes(process, &ctime, &xtime, &stime, &utime)) {
        Scm_SysError("GetProcessTimes failed");
    }
    val = ((int64_t)stime.dwHighDateTime << 32) + stime.dwLowDateTime;
    info->tms_stime = (u_int)((val+bias) / factor);
    val = ((int64_t)utime.dwHighDateTime << 32) + utime.dwLowDateTime;
    info->tms_utime = (u_int)((val+bias) / factor);

    info->tms_cstime = 0;
    info->tms_cutime = 0;
    return 0;
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
