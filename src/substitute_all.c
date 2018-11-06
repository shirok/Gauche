/* 
 * Utility to substitute all occurrences of MARK for SUBST
 * within INPUT.
 * This file is to be included from paths.c.
 *
 * We split this file so that it can be tested (via libextra.scm)
 */

#include <string.h>

static const char *substitute_all(const char *input,
                                  const char *mark,
                                  const char *subst)
{
    size_t ilen = strlen(input);
    size_t mlen = strlen(mark);
    size_t slen = strlen(subst);
        
    int noccurs = 0;
    const char *p = input;
    const char *pend = p + ilen;
    while (p < pend) {
        const char *p1 = strstr(p, mark);
        if (p1 == NULL) break;
        noccurs++;
        p = p1 + mlen;
    }

    if (noccurs == 0) return input;
    size_t buflen = noccurs * mlen + ilen - noccurs;
    char *buf = (char*)PATH_ALLOC(buflen+1);
    char *q = buf;
    for (p = input; noccurs > 0; noccurs--) {
        const char *p1 = strstr(p, mark);
        strncpy(q, p, p1-p);
        q += p1-p;
        strncpy(q, subst, slen);
        q += slen;
        p = p1 + mlen;
    }
    strncpy(q, p, pend-p);
    buf[buflen] = '\0';
    return buf;
}

