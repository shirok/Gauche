/*
 * char-none.h
 *
 *  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, distribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: char_none.h,v 1.1 2001-04-26 07:04:54 shiro Exp $
 */

#ifndef SCM_CHAR_ENCODING_BODY

#define SCM_CHAR_ENCODING_NAME "none"

#define SCM_CHAR_NFOLLOWS(ch)  0
#define SCM_CHAR_NBYTES(ch)    1
#define SCM_CHAR_MAX_BYTES     1

#define SCM_CHAR_GET(cp, ch) ((ch) = *(cp))
#define SCM_CHAR_PUT(cp, ch)  (*(cp) = (ch))

#define SCM_CHAR_BACKWARD(cp, start, result)                            \
    do {                                                                \
        (result) = (cp);                                                \
        while ((result) >= (start)) {                                   \
            if ((result) + SCM_CHAR_NFOLLOWS(*(result)) + 1 == (cp)) {  \
                break;                                                  \
            }                                                           \
            (result)--;                                                 \
        }                                                               \
        if ((result) < (start)) (result) = NULL;                        \
    } while (0)


#else  /* !SCM_CHAR_ENCODING_BODY */

#endif /* !SCM_CHAR_ENCODING_BODY */
