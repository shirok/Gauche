/*
 * signal.c - signal handling
 *
 *  Copyright(C) 2002 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, disribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: signal.c,v 1.2 2002-01-14 04:50:57 shirok Exp $
 */

#include <signal.h>
#include "gauche.h"
#include "gauche/class.h"

/* Signals
 *
 *  C-application that embeds Gauche can specify a set of signals
 *  that Gauche should handle.
 *
 *  Gauche sets its internal signal handlers for them.  That signal
 *  handler merely sets flag when signal is caught.
 *
 *  Each thread of Gauche can monitor arbitrary sets of signals.
 *  Scheme handlers associated with such signals are called at
 *  the "safe" point in the thread's VM.
 */

/*=======================================================
 * Internal signal table
 */

/* TODO: MT safeness */

static struct sigcount {
    
} SigCount;


/*=======================================================
 * Initialization
 */

void Scm__InitSignal()
{
}

