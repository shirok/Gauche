/*
 * MultiByte Immutable Strings (MBIS)
 */

/* MBIS is a library to deal with multi-byte encoded string.
 *
 * In multibyte strings, a character can be encoded in variable number
 * of bytes--typically, 7-bit ASCII characters use one byte, while
 * the other characters use two or more bytes.   Although there exist
 * various schemes to encode multibyte characteres, it is typical to
 * use this variable length encoding as an external presentation of a
 * string.
 *
 * The library is optimized for a certain access pattern.
 *
 *   - I/O is frequent.
 *   - String access is either sequential or by search.  Direct indexing
 *     is much less frequent.
 *   - String concatenation is very frequent.
 *   - Substring creation is very frequent.
 *   - String alternation (string-set!) is infrequent.
 */

#ifndef MBIS_H_
#define MBIS_H_

#include <stdio.h>

/* Terms:
 *   size - # of bytes
 *   length - # of characters
 */

typedef struct MBIS_ {
    unsigned long length;
    unsigned long size;
    const char *start;
    const char *end;
} *MBIS;

typedef long MBIS_char;
typedef const char *MBIS_pos;         /* must be treated as opaque obj */

#define MBIS_LENGTH(obj)    ((obj)->length)
#define MBIS_SIZE(obj)      ((obj)->size)

#define MBIS_CHAR_INVALID   (-1L)

/* Include appropriate encoding methods.
 * The header must define the following macros:
 *
 *   MBIS_CHAR_NFOLLOWS
 *   MBIS_CHAR_NBYTES
 *   MBIS_CHAR_MAX_BYTES
 *   MBIS_GETC
 *   MBIS_PUTC
 */

#ifdef MBIS_EUC_JP
#include "mbis_euc_jp.h"
#else
#ifdef MBIS_SJIS
#include "mbis_sjis.h"
#else
#include "mbis_utf8.h"          /* default */
#endif
#endif

/* If set, called when out-of-memory happends.   Must not return. */
extern void (*MBIS_oom_fn)(void);

/* Calculate length of multi-byte string from a given C-string.
   Returns -1 if str contains an invalid sequence of characters for
   multibyte string. */
extern int MBIS_length_from_cstring(const char *str);

/* Construct MBIS_char from string. */
extern MBIS_char MBIS_char_from_cstring(const char *str);

/* Turn NUL-terminated c-string into MBIS.  If str contains an invalid
   sequence of characters, NULL is returned.  You can pass -1 to size
   and/or len to let the function calculate it for you.
   STR is used as is, so you shouldn't alter it afterwards. */
extern MBIS MBIS_from_cstring(const char *str, int size, int len);

/* Return c-string representation of MBIS. */
extern char *MBIS_to_cstring(MBIS x);

/* Concatenation */
extern MBIS MBIS_append(MBIS x, MBIS y);
extern MBIS MBIS_append_cstring(MBIS x, const char *str, int size, int len);

/* Indexing */
extern MBIS_char MBIS_fetch(MBIS x, int pos);
extern MBIS_pos  MBIS_position(MBIS x, int pos);
extern MBIS_char MBIS_fetch_pos(MBIS x, MBIS_pos *ptr);

/* Substring */
extern MBIS MBIS_substring(MBIS x, int start, int end);
extern MBIS MBIS_substring_pos(MBIS x, MBIS_pos start, MBIS_pos end, int sublen);

/* Substitution */
extern MBIS MBIS_substitute(MBIS x, int start, int end, MBIS subs);
extern MBIS MBIS_substitute_cstring(MBIS x, int start, int end,
                                    const char *str, int size, int len);
extern MBIS MBIS_substitue_char(MBIS x, int start, int end, MBIS_char c);


/* Search */
extern int MBIS_find(MBIS x, MBIS y, int *start_index, MBIS_pos *start_pos);
extern int MBIS_find_cstring(MBIS x, char *str,
                             int *start_index, MBIS_pos *start_pos);
extern int MBIS_find_char(MBIS x, MBIS_char ch,
                          int *start_index, MBIS_pos *start_pos);

/* input and output */
extern void MBIS_putc(MBIS_char ch, FILE *out);
extern void MBIS_puts(MBIS x, FILE *out);



#endif /*MBIS_H_*/
