/*
 * charset.h - Character set implementation
 *
 *   Copyright (c) 2000-2007  Shiro Kawai  <shiro@acm.org>
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
 *
 *  $Id: charset.h,v 1.2 2007-04-05 06:50:16 shirok Exp $
 */

#ifndef GAUCHE_CHARSET_H
#define GAUCHE_CHARSET_H

/* We implement char-sets as hybrid of bitmap and treemap.
 * Bitmap is used for "small" characters, i.e. characters between
 * U+0000 and U+007F.  There, each bit represents whether the
 * character is in the set (1) or not (0).
 * For larger characters, we keep the range of included chars
 * in a treemap.  For each entry, its key is the start char code
 * and its value is the end char code (inclusive).
 * For example, if the character set has characters between
 * U+3040 and U+30FF, and U+4E00 and U_9FBF, then the treemap has
 * the following entries:
 *   #x3040 => #x30ff, #x4e00 => #x9fbf.
 * Lookup is trivial using Scm_TreeCoreClosestEntries.
 */

#define SCM_CHARSET_SMALL_CHARS 128

struct ScmCharSetRec {
    SCM_HEADER;
    ScmBits small[SCM_BITS_NUM_WORDS(SCM_CHARSET_SMALL_CHARS)];
    ScmTreeCore large;
};

SCM_CLASS_DECL(Scm_CharSetClass);
#define SCM_CLASS_CHARSET  (&Scm_CharSetClass)
#define SCM_CHARSET(obj)   ((ScmCharSet*)obj)
#define SCM_CHARSETP(obj)  SCM_XTYPEP(obj, SCM_CLASS_CHARSET)

#define SCM_CHARSET_SMALLP(obj) \
    (Scm_TreeCoreNumEntries(&SCM_CHARSET(obj)->large) == 0)

SCM_EXTERN ScmObj Scm_MakeEmptyCharSet(void);
SCM_EXTERN ScmObj Scm_CharSetCopy(ScmCharSet *src);
SCM_EXTERN int    Scm_CharSetEq(ScmCharSet *x, ScmCharSet *y);
SCM_EXTERN int    Scm_CharSetLE(ScmCharSet *x, ScmCharSet *y);
SCM_EXTERN ScmObj Scm_CharSetAddRange(ScmCharSet *cs,
				      ScmChar from, ScmChar to);
SCM_EXTERN ScmObj Scm_CharSetAdd(ScmCharSet *dest, ScmCharSet *src);
SCM_EXTERN ScmObj Scm_CharSetComplement(ScmCharSet *cs);
SCM_EXTERN ScmObj Scm_CharSetCaseFold(ScmCharSet *cs);
SCM_EXTERN ScmObj Scm_CharSetRanges(ScmCharSet *cs);
SCM_EXTERN ScmObj Scm_CharSetRead(ScmPort *input, int *complement_p,
				  int error_p, int bracket_syntax);

SCM_EXTERN int    Scm_CharSetContains(ScmCharSet *cs, ScmChar c);

#if SCM_DEBUG_HELPER
SCM_EXTERN void   Scm_CharSetDump(ScmCharSet *cs, ScmPort *port);
#endif

/* predefined character set API */
enum {
    SCM_CHARSET_ALNUM,
    SCM_CHARSET_ALPHA,
    SCM_CHARSET_BLANK,
    SCM_CHARSET_CNTRL,
    SCM_CHARSET_DIGIT,
    SCM_CHARSET_GRAPH,
    SCM_CHARSET_LOWER,
    SCM_CHARSET_PRINT,
    SCM_CHARSET_PUNCT,
    SCM_CHARSET_SPACE,
    SCM_CHARSET_UPPER,
    SCM_CHARSET_XDIGIT,
    SCM_CHARSET_WORD,           /* internal use: word constituent char. */
    SCM_CHARSET_NUM_PREDEFINED_SETS
};
SCM_EXTERN ScmObj Scm_GetStandardCharSet(int id);



#endif /*GAUCHE_CHARSET_H*/
