/*
 * vmstat.c - statistics gathering code for vm.c
 *
 *   Copyright (c) 2005-2015  Shiro Kawai  <shiro@acm.org>
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

/* This file is included from vm.c */

#ifdef COUNT_INSN_FREQUENCY
/* for statistics */
static u_long insn1_freq[SCM_VM_NUM_INSNS];
static u_long insn2_freq[SCM_VM_NUM_INSNS][SCM_VM_NUM_INSNS];

#define LREF_FREQ_COUNT_MAX 10
static u_long lref_freq[LREF_FREQ_COUNT_MAX][LREF_FREQ_COUNT_MAX];
static u_long lset_freq[LREF_FREQ_COUNT_MAX][LREF_FREQ_COUNT_MAX];

static ScmWord fetch_insn_counting(ScmVM *vm, ScmWord code)
{
    if (vm->base && vm->pc != vm->base->code) {
        insn2_freq[SCM_VM_INSN_CODE(code)][SCM_VM_INSN_CODE(*vm->pc)]++;
    }
    code = *vm->pc++;
    insn1_freq[SCM_VM_INSN_CODE(code)]++;
    switch (SCM_VM_INSN_CODE(code)) {
    case SCM_VM_LREF0: lref_freq[0][0]++; break;
    case SCM_VM_LREF1: lref_freq[0][1]++; break;
    case SCM_VM_LREF2: lref_freq[0][2]++; break;
    case SCM_VM_LREF3: lref_freq[0][3]++; break;
    case SCM_VM_LREF4: lref_freq[0][4]++; break;
    case SCM_VM_LREF10: lref_freq[1][0]++; break;
    case SCM_VM_LREF11: lref_freq[1][1]++; break;
    case SCM_VM_LREF12: lref_freq[1][2]++; break;
    case SCM_VM_LREF13: lref_freq[1][3]++; break;
    case SCM_VM_LREF14: lref_freq[1][4]++; break;
    case SCM_VM_LREF:
    {
        int dep = SCM_VM_INSN_ARG0(code);
        int off = SCM_VM_INSN_ARG1(code);
        if (dep >= LREF_FREQ_COUNT_MAX) dep=LREF_FREQ_COUNT_MAX-1;
        if (off >= LREF_FREQ_COUNT_MAX) off=LREF_FREQ_COUNT_MAX-1;
        lref_freq[dep][off]++;
        break;
    }
    case SCM_VM_LSET0: lset_freq[0][0]++; break;
    case SCM_VM_LSET1: lset_freq[0][1]++; break;
    case SCM_VM_LSET2: lset_freq[0][2]++; break;
    case SCM_VM_LSET3: lset_freq[0][3]++; break;
    case SCM_VM_LSET4: lset_freq[0][4]++; break;
    case SCM_VM_LSET:
    {
        int dep = SCM_VM_INSN_ARG0(code);
        int off = SCM_VM_INSN_ARG1(code);
        if (dep >= LREF_FREQ_COUNT_MAX) dep=LREF_FREQ_COUNT_MAX-1;
        if (off >= LREF_FREQ_COUNT_MAX) off=LREF_FREQ_COUNT_MAX-1;
        lset_freq[dep][off]++;
        break;
    }
    }
    return code;
}

static void dump_insn_frequency(void *data)
{
    Scm_Printf(SCM_CUROUT, "(:instruction-frequencies (");
    for (int i=0; i<SCM_VM_NUM_INSNS; i++) {
        Scm_Printf(SCM_CUROUT, "(%s %d", Scm_VMInsnName(i), insn1_freq[i]);
        for (int j=0; j<SCM_VM_NUM_INSNS; j++) {
            Scm_Printf(SCM_CUROUT, " %d", insn2_freq[i][j]);
        }
        Scm_Printf(SCM_CUROUT, ")\n");
    }
    Scm_Printf(SCM_CUROUT, ")\n :lref-frequencies (");
    for (int i=0; i<LREF_FREQ_COUNT_MAX; i++) {
        Scm_Printf(SCM_CUROUT, "(");
        for (int j=0; j<LREF_FREQ_COUNT_MAX; j++) {
            Scm_Printf(SCM_CUROUT, "%d ", lref_freq[i][j]);
        }
        Scm_Printf(SCM_CUROUT, ")\n");
    }
    Scm_Printf(SCM_CUROUT, ")\n :lset-frequencies (");
    for (int i=0; i<LREF_FREQ_COUNT_MAX; i++) {
        Scm_Printf(SCM_CUROUT, "(");
        for (int j=0; j<LREF_FREQ_COUNT_MAX; j++) {
            Scm_Printf(SCM_CUROUT, "%d ", lset_freq[i][j]);
        }
        Scm_Printf(SCM_CUROUT, ")\n");
    }
    Scm_Printf(SCM_CUROUT, ")\n");
    Scm_Printf(SCM_CUROUT, ")\n");
}

#endif /*COUNT_INSN_FREQUENCY*/
