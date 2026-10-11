/*
 * Test functions for gauche.ffi.ffiaux
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Reads entire contents of FP into a malloc-ed, NUL-terminated string.
   Returns NULL on failure. */
const char *read_file(FILE *fp)
{
    size_t cap = 256, len = 0;
    char *buf = malloc(cap);
    if (buf == NULL) return NULL;
    for (;;) {
        size_t n = fread(buf + len, 1, cap - len - 1, fp);
        len += n;
        if (n == 0) break;
        if (len + 1 >= cap) {
            char *nbuf = realloc(buf, cap * 2);
            if (nbuf == NULL) { free(buf); return NULL; }
            buf = nbuf;
            cap *= 2;
        }
    }
    if (ferror(fp)) { free(buf); return NULL; }
    buf[len] = '\0';
    return buf;
}

/* Writes STR to FP. */
void write_file(FILE *fp, const char *str)
{
    fputs(str, fp);
}
