/*
 * char_attr.h - Various character attributes
 *
 *   Copyright (c) 2011-2015  Shiro Kawai  <shiro@acm.org>
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

/* This file is not a part of public Gauche C API.  Should be included
   only for Gauche internal functions. */
#ifndef GAUCHE_CHAR_ATTR_H
#define GAUCHE_CHAR_ATTR_H

/* Unicode general categories */
enum {
    SCM_CHAR_CATEGORY_Lu,       /* Uppercase_Letter */
    SCM_CHAR_CATEGORY_Ll,       /* Lowercase_Letter */
    SCM_CHAR_CATEGORY_Lt,       /* Titlecase_Letter */
    SCM_CHAR_CATEGORY_Lm,       /* Modifier_Letter */
    SCM_CHAR_CATEGORY_Lo,       /* Other_Letter */
    SCM_CHAR_CATEGORY_Mn,       /* Nonspacing_Mark */
    SCM_CHAR_CATEGORY_Mc,       /* Spacing_Mark */
    SCM_CHAR_CATEGORY_Me,       /* Enclosing_Mark */
    SCM_CHAR_CATEGORY_Nd,       /* Decimal_Number */
    SCM_CHAR_CATEGORY_Nl,       /* Letter_Number */
    SCM_CHAR_CATEGORY_No,       /* Other_Number */
    SCM_CHAR_CATEGORY_Pc,       /* Connector_Punctuation */
    SCM_CHAR_CATEGORY_Pd,       /* Dash_Punctuation */
    SCM_CHAR_CATEGORY_Ps,       /* Open_Punctuation */
    SCM_CHAR_CATEGORY_Pe,       /* Close_Punctuation */
    SCM_CHAR_CATEGORY_Pi,       /* Initial_Punctuation */
    SCM_CHAR_CATEGORY_Pf,       /* Final_Punctuation */
    SCM_CHAR_CATEGORY_Po,       /* Other_Punctuation */
    SCM_CHAR_CATEGORY_Sm,       /* Math_Symbol */
    SCM_CHAR_CATEGORY_Sc,       /* Currency_Symbol */
    SCM_CHAR_CATEGORY_Sk,       /* Modifier_Symbol */
    SCM_CHAR_CATEGORY_So,       /* Other_Symbol */
    SCM_CHAR_CATEGORY_Zs,       /* Space_Separator */
    SCM_CHAR_CATEGORY_Zl,       /* Line_Separator */
    SCM_CHAR_CATEGORY_Zp,       /* Paragraph_Separator */
    SCM_CHAR_CATEGORY_Cc,       /* Control */
    SCM_CHAR_CATEGORY_Cf,       /* Format */
    SCM_CHAR_CATEGORY_Cs,       /* Surrogate */
    SCM_CHAR_CATEGORY_Co,       /* Private_Use */
    SCM_CHAR_CATEGORY_Cn        /* Unassigned */
};

#define SCM_CHAR_CATEGORY_MASK  (0x1f)

/* Higher two bits of a category byte are used for these flags
   00xxxxxx - non-alphabetic char
   01xxxxxx - lowercase alphabetic char
   10xxxxxx - uppercase alphabetic char
   11xxxxxx - caseless or titlecase alphabetic char
 */
#define SCM_CHAR_ALPHA_MASK      (0xc0u)
#define SCM_CHAR_ALPHABETIC_BITS (0xc0u)
#define SCM_CHAR_UPPERCASE_BITS  (0x80u)
#define SCM_CHAR_LOWERCASE_BITS  (0x40u)

/* Case mappings */

/* In Unicode 6.0, the max length of full case mapping is 3, but we reserve
   one more just in case for future ABI. */
#define SCM_CHAR_FULL_CASE_MAPPING_SIZE 4

typedef struct {
    int to_upper_simple;  /* offset to add to produce uppercase */
    int to_lower_simple;  /* offset to add to produce lowercase */
    int to_title_simple;  /* offset to add to produce titlecase */
    ScmChar to_upper_full[SCM_CHAR_FULL_CASE_MAPPING_SIZE];
    ScmChar to_lower_full[SCM_CHAR_FULL_CASE_MAPPING_SIZE];
    ScmChar to_title_full[SCM_CHAR_FULL_CASE_MAPPING_SIZE];
} ScmCharCaseMap;

/* Internal function to access case map info */
SCM_EXTERN const ScmCharCaseMap *Scm__CharCaseMap(ScmChar ch,
                                                  ScmCharCaseMap *buf,
                                                  int full);

/* Casemap entry value */
#define SCM_CHAR_NO_CASE_MAPPING 0xffff
#define SCM_CHAR_CASEMAP_TOLOWER(off)  (((unsigned int)(off))&0x3fff)
#define SCM_CHAR_CASEMAP_TOUPPER(off)  ((((unsigned int)(off))&0x3fff)|0x4000)
#define SCM_CHAR_CASEMAP_EXTENDED(off) ((((unsigned int)(off))&0x3fff)|0x8000)


#endif /*GAUCHE_CHAR_ATTR_H*/
