/*
 * win-extern.h - auxiliary header to handle Windows DLL insanity
 */

/* This header may be included multiple times.
   Assumes gauche/win-compat.h is already included if we're on
   mingw or winvc. */

/*
 * The black magic to hide ugliness of Windows.
 * On unix, SCM_EXTERN is "extern", period.
 * On Windows, SCM_EXTERN needs to become different things depending on
 * what you're compiling and which header file you're in.  You don't want
 * to know the gory details if you want to keep your sanity.
 */
#undef SCM_EXTERN

#if defined(__CYGWIN__) || defined(GAUCHE_WINDOWS)
# if defined(LIBGAUCHE_BODY) || defined(LIBGAUCHE_EXT_BODY)
#  define SCM_EXTERN extern __declspec(dllexport)
# else
#  define SCM_EXTERN extern __declspec(dllimport)
# endif
#else  /*!(__CYGWIN__ || GAUCHE_WINDOWS)*/
# define SCM_EXTERN extern
#endif /*!(__CYGWIN__ || GAUCHE_WINDOWS)*/
