/*
 * spigot.h - calculate pi and e by spigot algorithm
 *
 *  Copyright(C) 2003 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: spigot.h,v 1.1 2003-06-10 20:59:45 shirok Exp $
 */

extern ScmObj Spigot_calculate_pi(int digits);
extern ScmObj Spigot_calculate_e(int digits);
extern void Scm_Init_spigotlib(ScmModule *module);

