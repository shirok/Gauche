/*
 * spigot.h - calculate pi and e by spigot algorithm
 *
 *  Written by Shiro Kawai (shiro@acm.org)
 *  I put this program in public domain.  Use it as you like.
 */

extern ScmObj Spigot_calculate_pi(int digits);
extern ScmObj Spigot_calculate_e(int digits);
extern void Scm_Init_spigotlib(ScmModule *module);

