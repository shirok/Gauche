#ifndef _MD5_H_
#define _MD5_H_

#include <gauche.h>
#include <gauche/extend.h>
#include <sys/types.h>
#include "renaming.h"

#define MD5_HASHBYTES 16

#if !defined(HAVE_U_INT32_T)
#  if  !defined(HAVE_UINT32_T)
#    define u_int32_t int
#  else
#    define u_int32_t uint32_t
#  endif
#endif /*!HAVE_U_INT32_T*/

typedef struct MD5Context {
	u_int32_t buf[4];
	u_int32_t bits[2];
	unsigned char in[64];
} MD5_CTX;

extern void   MD5_Init(MD5_CTX *context);
extern void   MD5_Update(MD5_CTX *context, unsigned char const *buf,
	       unsigned len);
extern void   MD5_Final(unsigned char digest[MD5_HASHBYTES], MD5_CTX *context);
extern void   MD5_Transform(u_int32_t buf[4], u_int32_t const in[16]);
extern char * MD5_End(MD5_CTX *, char *);
extern char * MD5_File(const char *, char *);
extern char * MD5_Data (const unsigned char *, unsigned int, char *);

#endif /* !_MD5_H_ */
