/*
 * symbol.c - symbol implementation
 *
 *   Copyright (c) 2000-2009  Shiro Kawai  <shiro@acm.org>
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
 *  $Id: symbol.c,v 1.41 2008-05-10 13:36:21 shirok Exp $
 */

#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/builtin-syms.h"

/*-----------------------------------------------------------
 * Symbols
 */

static void symbol_print(ScmObj obj, ScmPort *port, ScmWriteContext *);
SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_SymbolClass, symbol_print);

#define INITSYM(sym, nam)                       \
    sym = SCM_NEW(ScmSymbol);                   \
    SCM_SET_CLASS(sym, SCM_CLASS_SYMBOL);       \
    sym->name = SCM_STRING(nam)

/* These two are global resource.  Must be protected in MT environment. */
static ScmHashTable *obtable = NULL;   /* name -> symbol mapper */
static int gensym_count = 0;

/* Intern */

ScmObj Scm_Intern(ScmString *name)
{
    ScmHashEntry *e = Scm_HashTableGet(obtable, SCM_OBJ(name));
    if (e) return e->value;
    else {
        ScmObj n = Scm_CopyStringWithFlags(name, SCM_STRING_IMMUTABLE,
                                           SCM_STRING_IMMUTABLE);
        ScmSymbol *sym;
        INITSYM(sym, n);
        Scm_HashTablePut(obtable, n, SCM_OBJ(sym));
        return SCM_OBJ(sym);
    }
}

/* Default prefix string. */
static SCM_DEFINE_STRING_CONST(default_prefix, "G", 1, 1);

/* Returns uninterned symbol.
   PREFIX can be NULL*/
ScmObj Scm_Gensym(ScmString *prefix)
{
    ScmString *name;
    ScmSymbol *sym;
    char numbuf[50];
    int nc;

    if (prefix == NULL) prefix = &default_prefix;
    nc = snprintf(numbuf, 50, "%d", gensym_count++);
    name = SCM_STRING(Scm_StringAppendC(prefix, numbuf, nc, nc));
    INITSYM(sym, name);
    return SCM_OBJ(sym);
}

/* Print */

/* table of special chars.
   bit 0: bad char for symbol to begin with
   bit 1: bad char for symbol to contain
   bit 2: bad char for symbol, and should be written as \nnn
   bit 3: bad char for symbol, and should be written as \c
   bit 4: may be escaped when case fold mode
 */
static char special[] = {
 /* NUL .... */
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
 /* .... */
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
 /*    !  "  #  $  %  &  '  (  )  *  +  ,  -  .  /  */
    3, 0, 3, 3, 0, 0, 0, 3, 3, 3, 0, 1, 3, 1, 1, 0,
 /* 0  1  2  3  4  5  6  7  8  9  :  ;  <  =  >  ?  */
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 0, 0, 0, 0,
 /* @  A  B  C  D  E  F  G  H  I  J  K  L  M  N  O  */
    1, 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,
 /* P  Q  R  S  T  U  V  W  X  Y  Z  [  \  ]  ^  _  */
    16,16,16,16,16,16,16,16,16,16,16,3, 11,3, 0, 0,
 /* `  a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  */
    3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
 /* p  q  r  s  t  u  v  w  x  y  z  {  |  }  ~  ^? */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 11,3, 0, 7
};

static void symbol_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    if (SCM_WRITE_MODE(ctx) == SCM_WRITE_DISPLAY) {
        SCM_PUTS(SCM_SYMBOL(obj)->name, port);
    } else {
        /* See if we have special characters, and use |-escape if necessary. */
        /* TODO: For now, we regard chars over 0x80 is all "printable".
           Need a more consistent mechanism. */
        ScmString *snam = SCM_SYMBOL(obj)->name;
        const ScmStringBody *b = SCM_STRING_BODY(snam);
        const char *p = SCM_STRING_BODY_START(b), *q;
        int siz = SCM_STRING_BODY_SIZE(b), i;
        int escape = FALSE;
        int case_mask =
            ((SCM_WRITE_CASE(ctx) == SCM_WRITE_CASE_FOLD)? 0x12 : 0x02);
        
        if (siz == 0) {         /* special case */
            SCM_PUTZ("||", -1, port);
            return;
        }
        if (siz == 1 && (*p == '+' || *p == '-')) {
            SCM_PUTC((unsigned)*p, port);
            return;
        }
        if ((unsigned int)*p < 128 && (special[(unsigned int)*p]&1)) {
            escape = TRUE;
        } else {
            for (i=0, q=p; i<siz; i++, q++) {
                if ((unsigned int)*q < 128
                    && (special[(unsigned int)*q]&case_mask)) {
                    escape = TRUE;
                    break;
                }
            }
        }
        if (escape) {
            SCM_PUTC('|', port);
            for (q=p; q<p+siz; ) {
                unsigned int ch;
                SCM_CHAR_GET(q, ch);
                q += SCM_CHAR_NBYTES(ch);
                if (ch < 128) {
                    if (special[ch] & 8) {
                        SCM_PUTC('\\', port);
                        SCM_PUTC(ch, port);
                    } else if (special[ch] & 4) {
                        Scm_Printf(port, "\\x%02x", ch);
                    } else {
                        SCM_PUTC(ch, port);
                    }
                } else {
                    SCM_PUTC(ch, port);
                }
            }
            SCM_PUTC('|', port);
            return;
        } else {
            SCM_PUTS(snam, port);
        }
    }
}

/*
 * Initialization
 */

#include "builtin-syms.c"

void Scm__InitSymbol(void)
{
    obtable = SCM_HASH_TABLE(Scm_MakeHashTableSimple(SCM_HASH_STRING, 4096));
    init_builtin_syms();
}
