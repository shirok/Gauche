/*
 * charP.h - Character-related API private header
 *
 *   Copyright (c) 2018-2019  Shiro Kawai  <shiro@acm.org>
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   3. Neither the name of the authors nor the names of its contributors
 *      may be used to endorse or promote products derived from this
 *      software without specific prior written permission.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef GAUCHE_PRIV_CHARP_H
#define GAUCHE_PRIV_CHARP_H

/* In char.c; called by gauche.charconv */
SCM_EXTERN void Scm__InstallCharconvHooks(ScmChar (*u2c)(int),
                                          int (*c2u)(ScmChar));

/* Predefined charsets
   The enum value may change across versions, so we keep them private.

   The charset object can be obtained by Scm_GetStandardCharSet(id)
   where ID is either the enum defined below, or its negation for
   complement charset.
 */

enum {
    /* Unicode General Categories */
    SCM_CHAR_SET_Lu = 1,        /* Letter, uppercase */
    SCM_CHAR_SET_Ll,            /* Letter, lowercase */
    SCM_CHAR_SET_Lt,            /* Letter, titlecase */
    SCM_CHAR_SET_Lm,            /* Letter, modifier */
    SCM_CHAR_SET_Lo,            /* Letter, other */
    SCM_CHAR_SET_Mn,            /* Mark, nonspacing */
    SCM_CHAR_SET_Mc,            /* Mark, combining */
    SCM_CHAR_SET_Me,            /* Mark, enclosing */
    SCM_CHAR_SET_Nd,            /* Number, decimal digit */
    SCM_CHAR_SET_Nl,            /* Number, letter */
    SCM_CHAR_SET_No,            /* Number, other */
    SCM_CHAR_SET_Pc,            /* Punctuation, connector */
    SCM_CHAR_SET_Pd,            /* Punctuation, dash */
    SCM_CHAR_SET_Ps,            /* Punctuation, open */
    SCM_CHAR_SET_Pe,            /* Punctuation, close */
    SCM_CHAR_SET_Pi,            /* Punctuation, initial quote */
    SCM_CHAR_SET_Pf,            /* Punctuation, final quote */
    SCM_CHAR_SET_Po,            /* Punctuation, other */
    SCM_CHAR_SET_Sm,            /* Symbol, math */
    SCM_CHAR_SET_Sc,            /* Symbol, currency */
    SCM_CHAR_SET_Sk,            /* Symbol, modifier */
    SCM_CHAR_SET_So,            /* Symbol, other */
    SCM_CHAR_SET_Zs,            /* Separator, space */
    SCM_CHAR_SET_Zl,            /* Separator, line */
    SCM_CHAR_SET_Zp,            /* Separator, paragraph */
    SCM_CHAR_SET_Cc,            /* Other, control */
    SCM_CHAR_SET_Cf,            /* Other, format */
    SCM_CHAR_SET_Cs,            /* Other, surrogate */
    SCM_CHAR_SET_Co,            /* Other, private use */
    SCM_CHAR_SET_Cn,            /* Other, not assigned */
    /* SRFI-14 sets */
    SCM_CHAR_SET_LOWER,         /* Ll */
    SCM_CHAR_SET_ASCII_LOWER,
    SCM_CHAR_SET_UPPER,         /* Lu */
    SCM_CHAR_SET_ASCII_UPPER,
    SCM_CHAR_SET_TITLE,         /* Lt */
    SCM_CHAR_SET_LETTER,        /* Lu|Ll|Lt|Lm|Lo */
    SCM_CHAR_SET_ASCII_LETTER,  /* intersection(LETTER, ASCII) == A-Za-z */
    SCM_CHAR_SET_DIGIT,         /* Nd */
    SCM_CHAR_SET_ASCII_DIGIT,   /* [0-9] */
    SCM_CHAR_SET_LETTER_DIGIT,  /* L*|Nd */
    SCM_CHAR_SET_ASCII_LETTER_DIGIT,
    SCM_CHAR_SET_GRAPHIC,       /* L*|N*|P*|S* */
    SCM_CHAR_SET_ASCII_GRAPHIC,
    SCM_CHAR_SET_PRINTING,      /* L*|N*|P*|S*|Z* */
    SCM_CHAR_SET_ASCII_PRINTING,
    SCM_CHAR_SET_WHITESPACE,    /* Z*|\u0009-\u000d */
    SCM_CHAR_SET_ASCII_WHITESPACE, /* \u0020, \u0009-\u000d */
    SCM_CHAR_SET_ISO_CONTROL,   /* Cc */
    SCM_CHAR_SET_ASCII_ISO_CONTROL,
    SCM_CHAR_SET_PUNCTUATION,   /* P* */
    SCM_CHAR_SET_ASCII_PUNCTUATION,
    SCM_CHAR_SET_SYMBOL,        /* S* */
    SCM_CHAR_SET_ASCII_SYMBOL,
    SCM_CHAR_SET_HEX_DIGIT,     /* 0-9A-Fa-f */
    SCM_CHAR_SET_BLANK,         /* Zs|\u0009 */
    SCM_CHAR_SET_ASCII_BLANK,   /* \u0020,\u0009 */
    SCM_CHAR_SET_ASCII,         /* \u0000-\u007f */
    SCM_CHAR_SET_EMPTY,
    SCM_CHAR_SET_FULL,
    /* internal use: word constituent chars */
    SCM_CHAR_SET_WORD,
    SCM_CHAR_SET_ASCII_WORD,
    SCM_CHAR_SET_NUM_PREDEFINED_SETS
};
SCM_EXTERN ScmObj Scm_GetStandardCharSet(int id);

#endif /*GAUCHE_PRIV_CHARP_H*/
