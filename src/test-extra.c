/*
  This is to test features that aren't accessible from Scheme by default.
  We build a special executable that exposes those features.

  (This also shows how to write a simple program linking libgauche.  Scroll
  down to main() to see how.)
*/

#include <gauche.h>

void Scm_Init_libextra(ScmModule *);

/* main */
int main(int argc, const char **argv)
{
    GC_INIT();
    Scm_Init(GAUCHE_SIGNATURE);
    Scm_AddLoadPath("../src/", FALSE);
    Scm_AddLoadPath("../libsrc/", FALSE);
    Scm_AddLoadPath("../lib/", FALSE);
    Scm_Init_libextra(Scm_UserModule());
    Scm_SimpleMain(argc, argv, NULL, 0);
    return 0;
}




