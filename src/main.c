/*
 * main.c - interpreter main program
 *
 *  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: main.c,v 1.12 2001-02-19 14:48:49 shiro Exp $
 */

#include <unistd.h>
#include "gauche.h"

int debug = FALSE;
int enable_inline = TRUE;
int load_initfile = TRUE;

int main(int argc, char **argv)
{
    int c;
    
    while ((c = getopt(argc, argv, "gqI")) >= 0) {
        switch (c) {
        case 'g': debug = TRUE; break;
        case 'q': load_initfile = FALSE; break;
        case 'I': enable_inline = FALSE; break;
        }
    }
    Scm_Init(load_initfile ? "gauche-init.scm" : NULL);

    if (debug) Scm_VM()->debugCompile = TRUE;
    if (!enable_inline) Scm_VM()->enableInline = FALSE;

    if (optind < argc) {
        /* file name passed. */
        Scm_Load(argv[optind]);
        exit(0);
    }

    Scm_Repl(SCM_MAKE_STR("gosh> "),
             SCM_PORT(Scm_Stdin()),
             SCM_PORT(Scm_Stdout()));

    return 0;
}
