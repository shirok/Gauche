
/* for now */

#define SCM_CHAR_NFOLLOWS(ch) \
    ((((unsigned char)(ch)) >= 0x80) ? 1 : 0)

#define SCM_CHAR_NBYTES(ch) \
    (((ch) >= 0x80) ? 2 : 1)

#define SCM_CHAR_MAX_BYTES     2

#define SCM_STR_GETC(cp, ch)                                    \
    do {                                                        \
        if (((ch) = (unsigned char)*(cp)) >= 0x80) {            \
            (ch) = ((ch) << 8) + (unsigned char)*(cp+1);        \
        }                                                       \
    } while (0)

#define SCM_STR_PUTC(cp, ch)                    \
    do {                                        \
        if (ch > 0xff) {                        \
            (cp)[0] = (ch >> 8) & 0xff;         \
            (cp)[1] = ch & 0xff;                \
        } else {                                \
            (cp)[0] = ch & 0xff;                \
        }                                       \
    } while (0)


