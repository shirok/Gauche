/*
 * spigot.h - calculate pi and e by spigot algorithm
 *
 *  Written by Shiro Kawai (shiro@acm.org)
 *  I put this program in public domain.  Use it as you like.
 *
 *  $Id: spigot.h,v 1.2 2003-06-12 11:09:23 shirok Exp $
 */

extern ScmObj Spigot_calculate_pi(int digits);
extern ScmObj Spigot_calculate_e(int digits);
extern void Scm_Init_spigotlib(ScmModule *module);

