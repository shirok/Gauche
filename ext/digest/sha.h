/* SHA.H - header file for SHA.C
 */
#ifndef _SHA_H_
#define _SHA_H_

#include <sys/types.h>

#if !defined(HAVE_U_INT32_T)
#  if  !defined(HAVE_UINT32_T)
#    define u_int32_t int
#  else
#    define u_int32_t uint32_t
#  endif
#endif /*!HAVE_U_INT32_T*/

#ifdef __GNUC__
#define HAVE64 1
#if !defined(HAVE_U_INT64_T)
#  if  !defined(HAVE_UINT64_T)
#    undef HAVE64
#  else
#    define u_int64_t uint64_t
#  endif
#endif /*!HAVE_U_INT64_T*/
#endif /*__GNUC__*/

#if !defined(HAVE_U_INT8_T)
#  if  !defined(HAVE_UINT8_T)
#    define u_int8_t u_char
#  else
#    define u_int8_t uint8_t
#  endif
#endif /*!HAVE_U_INT8_T*/

/*
* Define to 1 for FIPS 180.1 version (with extra rotate in prescheduling),
* 0 for FIPS 180 version (with the mysterious "weakness" that the NSA
* isn't talking about).
*/
#define SHA_VERSION 1

#define SHA_BLOCKBYTES 64
#define SHA_BLOCKWORDS 16

#define SHA_HASHBYTES 20
#define SHA_HASHWORDS 5

/* SHA context. */
typedef struct SHAContext {
 unsigned int key[SHA_BLOCKWORDS];
 u_int32_t iv[SHA_HASHWORDS];
#if defined(HAVE64)
 u_int64_t bytes;
#else
 u_int32_t bytesHi, bytesLo;
#endif
} SHA_CTX;

extern void   SHAInit(SHA_CTX *);
extern void   SHAUpdate(SHA_CTX *, const unsigned char *, unsigned int);
extern void   SHAFinal(unsigned char [SHA_HASHBYTES], SHA_CTX *);
extern char * SHAEnd(SHA_CTX *, char *);
extern char * SHAFile(const char *, char *);
extern char * SHAData(const unsigned char *, unsigned int, char *);

#endif /* _SHA_H_ */
