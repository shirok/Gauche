/*
 * Avoid conflict with procedures in system's libcrypt
 */

#define crypt(key, setting) \
    gauche_crypt(key, setting)
#define crypt_r(key, setting, data) \
    gauche_crypt_r(key, setting, data)
#define crypt_rn(key, setting, data, size) \
    gauche_crypt_rn(key, setting, data, size)
#define crypt_ra(key, setting, data, size) \
    gauche_crypt_ra(key, setting, data, size)
#define crypt_gensalt(prefix, count, input, size) \
    gauche_crypt_gensalt(prefix, count, input, size)
#define crypt_gensalt_rn(prefix, count, input, size, output, osize) \
    gauche_crypt_gensalt_rn(prefix, count, input, size, output, osize)
#define crypt_gensalt_ra(prefix, count, input, size) \
    gauche_crypt_gensalt_ra(prefix, count, input, size)

#define _crypt_output_magic  gauche_crypt_output_magic
#define _crypt_blowfish_rn   gauche_crypt_blowfish_rn
#define _crypt_gensalt_blowfish_rn gauche_crypt_gensalt_blowfish_rn
#define _crypt_gensalt_traditional_rn gauche_crypt_gensalt_traditional_rn
#define _crypt_gensalt_extended_rn gauche_crypt_gensalt_extended_rn
#define _cyprt_gensalt_md5_rn gauche_crypt_gensalt_md5_rn
