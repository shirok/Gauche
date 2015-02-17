/*
 * regexp.h - Regexp module internal definitions
 *
 *   Copyright (c) 2006-2015  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_REGEXP_H
#define GAUCHE_REGEXP_H

struct ScmRegexpRec {
    SCM_HEADER;
    ScmObj pattern;      /* Source string.  For debugging/introspection.
                            Can be #f if this regexp is created from AST.
                            regexp->string will construct the string
                            representation and fill this slot in such case. */
    ScmObj ast;          /* Parsed AST. */
    const u_char *code;  /* byte code vector */
    int numGroups;       /* # of captured groups */
    int numCodes;        /* size of byte code vector */
    ScmCharSet **sets;   /* array of charset literals referred from code */
    ScmObj grpNames;     /* list of names for named groups. */
    int numSets;         /* # of charsets in sets */
    int flags;           /* internal; CASE_FOLD, BOL_ANCHORED etc. */
    ScmString *mustMatch;
    ScmObj laset;        /* lookahead set (char-set) or #f.
                            If not #f, it represents the condition that can
                            match at the beginning of the regexp.  It can be
                            used to skip input start position when regexp
                            isn't BOL_ANCHORED. */
};

struct ScmRegMatchRec {
    SCM_HEADER;
    const char *input;
    int inputSize;
    int inputLen;
    int numMatches;
    ScmObj grpNames;
    struct ScmRegMatchSub {
        int start;
        int length;
        int after;
        const char *startp;
        const char *endp;
    } **matches;
};

#define SCM_REG_MATCH_SINGLE_BYTE_P(rm) \
    ((rm)->inputSize == (rm)->inputLen)

/* Note: The structure of ScmRegexp is changed on 0.9.1.  Shuold be safe,
   for it should never be statically allocated. */

#endif /* GAUCHE_REGEXP_H */
