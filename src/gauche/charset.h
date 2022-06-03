/*
 * charset.h - Character set implementation
 *
 *   Copyright (c) 2000-2022  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_CHARSET_H
#define GAUCHE_CHARSET_H

/* We implement char-sets as hybrid of bitmap and binary tree.
 *
 * Bitmap is used for "small" characters, i.e. characters between
 * U+0000 and U+007F.  There, each bit represents whether the
 * character is in the set (1) or not (0).
 *
 * For larger characters, we keep the range of included chars
 * in a binary tree.  For each entry, its key is the start char code
 * and its value is the end char code (inclusive).
 * For example, if the character set has characters between
 * U+3040 and U+30FF, and U+4E00 and U+9FBF, then the tree has
 * the following entries:
 *   #x3040 => #x30ff, #x4e00 => #x9fbf.
 *
 * We have mutable char-set (default) and immutable or frozen char-set.
 * Mutable char-set uses ScmTreeCore for the large characters.  Immutable
 * char-set uses flat u32vector, sorted by the key and accessed by binary
 * search.
 */

#define SCM_CHAR_SET_SMALL_CHARS 128

struct ScmCharSetRec {
    SCM_HEADER;
    ScmBits small[SCM_BITS_NUM_WORDS(SCM_CHAR_SET_SMALL_CHARS)];
    u_int flags;
    union {
        ScmTreeCore tree;
        struct {
            ScmSize size; /* size of vec.  # of entries is half of this */
            const uint32_t *vec;
            uint32_t ivec[2]; /* if size==2, vec points here */
        } frozen;
    } large;
};

typedef enum {
    SCM_CHAR_SET_LARGE = 1,
    SCM_CHAR_SET_IMMUTABLE = 2,
} ScmCharSetType;

SCM_CLASS_DECL(Scm_CharSetClass);
#define SCM_CLASS_CHAR_SET  (&Scm_CharSetClass)
#define SCM_CHAR_SET(obj)   ((ScmCharSet*)obj)
#define SCM_CHAR_SET_P(obj) SCM_XTYPEP(obj, SCM_CLASS_CHAR_SET)

#define SCM_CHAR_SET_LARGE_P(obj) \
    (SCM_CHAR_SET(obj)->flags & SCM_CHAR_SET_LARGE)
#define SCM_CHAR_SET_IMMUTABLE_P(obj) \
    (SCM_CHAR_SET(obj)->flags & SCM_CHAR_SET_IMMUTABLE)

/* for backward compatibility.  deprecated. */
#define SCM_CLASS_CHARSET   SCM_CLASS_CHAR_SET
#define SCM_CHARSET(obj)    SCM_CHAR_SET(obj)
#define SCM_CHARSETP(obj)   SCM_CHAR_SET_P(obj)

SCM_EXTERN ScmObj Scm_MakeEmptyCharSet(void);
SCM_EXTERN ScmObj Scm_MakeImmutableCharSet(const ScmBits *small,
                                           const uint32_t *vec,
                                           size_t size);
SCM_EXTERN ScmObj Scm_CharSetCopy(ScmCharSet *src);
SCM_EXTERN ScmObj Scm_CharSetFreeze(ScmCharSet *src);
SCM_EXTERN ScmObj Scm_CharSetFreezeX(ScmCharSet *src);
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
SCM_EXTERN int    Scm_CharSetParseCategory(ScmPort *input, char ch);

SCM_EXTERN int    Scm_CharSetContains(ScmCharSet *cs, ScmChar c);
SCM_EXTERN void   Scm_CharSetDump(ScmCharSet *cs, ScmPort *port);

#endif /*GAUCHE_CHARSET_H*/
