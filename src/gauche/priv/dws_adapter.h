/*
 * Some macro trick to make both 32bit and 64bit siphash implementation
 * coexit.
 *
 * The original implementation of Sam Trenholme is written so that you
 * can just switch 32bit and 64bit versions with the same interface.
 *
 * For us, however, we need 32bit version available across platforms
 * for the potable-hash.  We do want 64bit version on 64bit platform
 * for the default-hash.
 *
 * We modify the original source (dws32hash.c, dwsiphash.c) adding
 * a bit of #includes and #ifdefs, at the beginning and the end, so that the
 * files shall provide the following API publicly.
 *
 *                      32bit platform           64bit platform
 *
 * dws32hash.c       Scm__DwSipDefaultHash          (none)
 *                   Scm__DwSipPortableHash
 *
 * dwsiphash.c       Scm__DwSipPortableHash    Scm__DwSipDefaultHash
 */

#include <gauche/config.h>

#ifdef SCM_DWSIPHASH_INTERFACE

/* This part is used in hash.c */
#if SIZEOF_LONG == 4
u_long Scm__DwSipDefaultHash(uint8_t *str, uint32_t len,
                             u_long k1, u_long k2);
uint32_t Scm__DwSipPortableHash(uint8_t *str, uint32_t len,
                                uint32_t k1, uint32_t k2);
#else  /*SIZEOF_LONG > 4*/
u_long Scm__DwSipDefaultHash(uint8_t *str, uint32_t len,
                             u_long k1, u_long k2);
uint32_t Scm__DwSipPortableHash(uint8_t *str, uint32_t len,
                                uint32_t k1, uint32_t k2);
#endif /*SIZEOF_LONG > 4*/

#else  /* !SCM_DWSIPHASH_INTERFACE */

/* This part is used in dwsiphash.c and dws32hash.c */
#if DwSH_BWIDTH == 32
/* We're compiling dws32hash.c */
#define DwSip_round Scm__DwSip32_round
#define DwSip_ksetup Scm__DwSip32_ksetup
#define DwSip_getword Scm__DwSip32_getword
#define DwSip_hash Scm__DwSip32_hash
#else  /* DwSH_BWIDTH == 64 */
#define DwSip_round Scm__DwSip64_round
#define DwSip_ksetup Scm__DwSip64_ksetup
#define DwSip_getword Scm__DwSip64_getword
#define DwSip_hash Scm__DwSip64_hash
#endif /* DwSH_BWIDTH == 64 */

/* forward declaration to make these file-scope */
static void DwSip_round(DwSH_WORD *v0, DwSH_WORD *v1,
                        DwSH_WORD *v2, DwSH_WORD *v3);
static void DwSip_ksetup(DwSH_WORD *k0, DwSH_WORD *k1,
                         DwSH_WORD *v0, DwSH_WORD *v1,
                         DwSH_WORD *v2, DwSH_WORD *v3, DwSH_WORD *fx);
static DwSH_WORD DwSip_getword(uint32_t *offset, uint8_t *str, uint32_t len);
static DwSH_WORD DwSip_hash(uint8_t *str, uint32_t len,
                            DwSH_WORD k1, DwSH_WORD k2);

#if SIZEOF_LONG == 4

#if   DwSH_BWIDTH == 32
uint32_t Scm__DwSipDefaultHash(uint8_t *str, uint32_t len,
                               uint32_t k1, uint32_t k2)
{
    return Scm__DwSip32_hash(str, len, k1, k2);
}

uint32_t Scm__DwSipPortableHash(uint8_t *str, uint32_t len,
                                uint32_t k1, uint32_t k2)
{
    return Scm__DwSip32_hash(str, len, k1, k2);
}
#else    /*DwSH_BWIDTH == 64*/
/* nothing */
#endif   /*DwSH_BWIDTH == 64*/

#else  /*SIZEOF_LONG > 4*/

#if  DwSH_BWIDTH == 32
uint32_t Scm__DwSipPortableHash(uint8_t *str, uint32_t len,
                                uint32_t k1, uint32_t k2)
{
    return Scm__DwSip32_hash(str, len, k1, k2);
}
#else    /*DwSH_BWIDTH == 64*/
uint64_t Scm__DwSipDefaultHash(uint8_t *str, uint32_t len,
                               uint64_t k1, uint64_t k2)
{
    return Scm__DwSip64_hash(str, len, k1, k2);
}
#endif   /*DwSH_BWIDTH == 64*/

#endif /*SIZEOF_LONG > 4*/

#endif /*!SCM_DWSIPHASH_INTERFACE*/
