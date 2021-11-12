/*
 * termios.c - termios interface
 *
 *   Copyright (c) 2000-2021  Shiro Kawai  <shiro@acm.org>
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

#include <string.h>
#include <gauche.h>
#include <gauche/extend.h>

#include "gauche-termios.h"

/*
 * Initializaion
 */

void Scm_Init_termios(void)
{
    SCM_INIT_EXTENSION(gauche__termios);
    ScmModule *mod = SCM_FIND_MODULE("gauche.termios", SCM_FIND_MODULE_CREATE);
    (void)mod; /* suppress unused var warning */

#if !defined(GAUCHE_WINDOWS)

    /* Constants for termios.  Non-POSIX symbols are guarded by #ifdef's */
#define DEFSYM(sym) \
    SCM_DEFINE(mod, #sym, Scm_MakeIntegerFromUI(sym))

    /* c_iflag masks */
    DEFSYM(IGNBRK);
    DEFSYM(BRKINT);
    DEFSYM(IGNPAR);
    DEFSYM(PARMRK);
    DEFSYM(INPCK);
    DEFSYM(ISTRIP);
    DEFSYM(INLCR);
    DEFSYM(IGNCR);
    DEFSYM(ICRNL);
    DEFSYM(IXON);
    DEFSYM(IXOFF);
#ifdef IXANY
    DEFSYM(IXANY);
#endif
#ifdef IUCLC
    DEFSYM(IUCLC);
#endif
#ifdef IMAXBEL
    DEFSYM(IMAXBEL);
#endif

    /* c_oflag masks */
    DEFSYM(OPOST);
#ifdef OLCUC
    DEFSYM(OLCUC);
#endif
#ifdef ONLCR
    DEFSYM(ONLCR);
#endif
#ifdef OCRNL
    DEFSYM(OCRNL);
#endif
#ifdef ONOCR
    DEFSYM(ONOCR);
#endif
#ifdef ONLRET
    DEFSYM(ONLRET);
#endif
#ifdef OFILL
    DEFSYM(OFILL);
#endif
#ifdef OFDEL
    DEFSYM(OFDEL);
#endif
#ifdef NLDLY
    DEFSYM(NLDLY);
#endif
#ifdef NL0
    DEFSYM(NL0);
#endif
#ifdef NL1
    DEFSYM(NL1);
#endif
#ifdef CRDLY
    DEFSYM(CRDLY);
#endif
#ifdef CR0
    DEFSYM(CR0);
#endif
#ifdef CR1
    DEFSYM(CR1);
#endif
#ifdef CR2
    DEFSYM(CR2);
#endif
#ifdef CR3
    DEFSYM(CR3);
#endif
#ifdef BSDLY
    DEFSYM(BSDLY);
#endif
#ifdef BS0
    DEFSYM(BS0);
#endif
#ifdef BS1
    DEFSYM(BS1);
#endif
#ifdef VTDLY
    DEFSYM(VTDLY);
#endif
#ifdef VT0
    DEFSYM(VT0);
#endif
#ifdef VT1
    DEFSYM(VT1);
#endif
#ifdef FFDLY
    DEFSYM(FFDLY);
#endif
#ifdef FF0
    DEFSYM(FF0);
#endif
#ifdef FF1
    DEFSYM(FF1);
#endif

    /* c_cflag masks */
    DEFSYM(CLOCAL);
    DEFSYM(CREAD);
    DEFSYM(CSIZE);
    DEFSYM(CS5);
    DEFSYM(CS6);
    DEFSYM(CS7);
    DEFSYM(CS8);
    DEFSYM(CSTOPB);
    DEFSYM(HUPCL);
    DEFSYM(PARENB);
    DEFSYM(PARODD);
#ifdef CIBAUD
    DEFSYM(CIBAUD);
#endif
#ifdef CRTSCTS
    DEFSYM(CRTSCTS);
#endif

    /* c_lflag masks */
    DEFSYM(ECHO);
    DEFSYM(ECHOE);
    DEFSYM(ECHOK);
    DEFSYM(ECHONL);
    DEFSYM(ICANON);
    DEFSYM(ISIG);
    DEFSYM(NOFLSH);
    DEFSYM(TOSTOP);
    DEFSYM(IEXTEN);
#ifdef XCASE
    DEFSYM(XCASE);
#endif
#ifdef ECHOCTL
    DEFSYM(ECHOCTL);
#endif
#ifdef ECHOPRT
    DEFSYM(ECHOPRT);
#endif
#ifdef ECHOKE
    DEFSYM(ECHOKE);
#endif
#ifdef FLUSH0
    DEFSYM(FLUSH0);
#endif
#ifdef PENDIN
    DEFSYM(PENDIN);
#endif

    /* c_cc size */
    DEFSYM(NCCS);

    /* disable character */
    DEFSYM(_POSIX_VDISABLE);

    /* c_cc subscripts */
    DEFSYM(VEOF);
    DEFSYM(VEOL);
    DEFSYM(VERASE);
    DEFSYM(VINTR);
    DEFSYM(VKILL);
    DEFSYM(VMIN);
    DEFSYM(VQUIT);
    DEFSYM(VSTART);
    DEFSYM(VSTOP);
    DEFSYM(VSUSP);
    DEFSYM(VTIME);
#ifdef VDISCARD
    DEFSYM(VDISCARD);
#endif
#ifdef VDSUSP
    DEFSYM(VDSUSP);
#endif
#ifdef EOL2
    DEFSYM(VEOL2);
#endif
#ifdef LNEXT
    DEFSYM(VLNEXT);
#endif
#ifdef VREPRINT
    DEFSYM(VREPRINT);
#endif
#ifdef VSTATUS
    DEFSYM(VSTATUS);
#endif
#ifdef WERASE
    DEFSYM(VWERASE);
#endif
#ifdef VSWTCH
    DEFSYM(VSWTCH);
#endif
#ifdef VSWTC
    DEFSYM(VSWTC);
#endif

    /* extra baudrates.   <= B38400 is defined in termiolib.stub */
#ifdef B57600
    DEFSYM(B57600);
#endif
#ifdef B115200
    DEFSYM(B115200);
#endif
#ifdef B230400
    DEFSYM(B230400);
#endif

#endif /*!GAUCHE_WINDOWS*/
}
