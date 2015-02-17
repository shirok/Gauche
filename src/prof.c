/*
 * prof.c - profiler
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

#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/vm.h"
#include "gauche/code.h"
#include "gauche/vminsn.h"
#include "gauche/prof.h"

#ifdef GAUCHE_PROFILE

/* WARNING: duplicated code - see signal.c; we should integrate them later */
#ifdef GAUCHE_USE_PTHREADS
#define SIGPROCMASK pthread_sigmask
#else  /* windows doesn't care signals, so we don't check WTHREADS. */
#define SIGPROCMASK sigprocmask
#endif

/*=============================================================
 * Interval timer operation
 */

#define SAMPLING_PERIOD 10000

#define ITIMER_START()                                  \
    do {                                                \
        struct itimerval tval, oval;                    \
        tval.it_interval.tv_sec = 0;                    \
        tval.it_interval.tv_usec = SAMPLING_PERIOD;     \
        tval.it_value.tv_sec = 0;                       \
        tval.it_value.tv_usec = SAMPLING_PERIOD;        \
        setitimer(ITIMER_PROF, &tval, &oval);           \
    } while (0)

#define ITIMER_STOP()                           \
    do {                                        \
        struct itimerval tval, oval;            \
        tval.it_interval.tv_sec = 0;            \
        tval.it_interval.tv_usec = 0;           \
        tval.it_value.tv_sec = 0;               \
        tval.it_value.tv_usec = 0;              \
        setitimer(ITIMER_PROF, &tval, &oval);   \
    } while (0)

/*=============================================================
 * Statistic sampler
 */

/* Flush sample buffer to the file.
   We save the address value to the file.  The address should also be
   recorded in the call counter, thus we don't need to worry about
   the addressed object being GCed. */

#define CHK(exp)  do { if (!(exp)) goto bad; } while (0)

static void sampler_flush(ScmVM *vm)
{
    if (vm->prof == NULL) return; /* for safety */
    if (vm->prof->samplerFd < 0 || vm->prof->currentSample == 0) return;

    int nsamples = vm->prof->currentSample;
    ssize_t r = write(vm->prof->samplerFd, vm->prof->samples,
                      nsamples * sizeof(ScmProfSample[1]));
    if (r == (ssize_t)-1) {
        vm->prof->errorOccurred++;
    }
    vm->prof->currentSample = 0;
    return;
}

/* signal handler */
static void sampler_sample(int sig)
{
    ScmVM *vm = Scm_VM();
    if (vm == NULL || vm->prof == NULL) return;
    if (vm->prof->state != SCM_PROFILER_RUNNING) return;

    if (vm->prof->currentSample >= SCM_PROF_SAMPLES_IN_BUFFER) {
        ITIMER_STOP();
        sampler_flush(vm);
        ITIMER_START();
    }

    int i = vm->prof->currentSample++;
    if (vm->base) {
        /* If vm->pc is RET and val0 is a subr, it is pretty likely that
           we're actually executing that subr. */
        if (vm->pc && SCM_VM_INSN_CODE(*vm->pc) == SCM_VM_RET
            && SCM_SUBRP(vm->val0)) {
            vm->prof->samples[i].func = vm->val0;
            vm->prof->samples[i].pc = NULL;
        } else {
            vm->prof->samples[i].func = SCM_OBJ(vm->base);
            vm->prof->samples[i].pc = vm->pc;
        }
    } else {
        vm->prof->samples[i].func = SCM_FALSE;
        vm->prof->samples[i].pc = NULL;
    }
    vm->prof->totalSamples++;
}

/* register samples into the stat table.  Called from Scm_ProfilerResult */
void collect_samples(ScmVMProfiler *prof)
{
    for (int i=0; i<prof->currentSample; i++) {
        ScmObj e = Scm_HashTableRef(prof->statHash,
                                    prof->samples[i].func, SCM_UNBOUND);
        if (SCM_UNBOUNDP(e)) {
            /* NB: just for now */
            Scm_Warn("profiler: uncounted object appeared in a sample: %p (%S)\n",
                     prof->samples[i].func, prof->samples[i].func);
        } else {
            SCM_ASSERT(SCM_PAIRP(e));
            int cnt = SCM_INT_VALUE(SCM_CDR(e)) + 1;
            SCM_SET_CDR(e, SCM_MAKE_INT(cnt));
        }
    }
}

/*=============================================================
 * Call Counter
 */

/* Inserting data into array is done in a macro (prof.h).  It calls
   this flush routine when the array gets full. */

void Scm_ProfilerCountBufferFlush(ScmVM *vm)
{
    if (vm->prof == NULL) return; /* for safety */
    if (vm->prof->currentCount == 0) return;

    /* suspend itimer during hash table operation */
    sigset_t set;
    sigemptyset(&set);
    sigaddset(&set, SIGPROF);
    SIGPROCMASK(SIG_BLOCK, &set, NULL);

    int ncounts = vm->prof->currentCount;
    for (int i=0; i<ncounts; i++) {
        ScmObj e;
        int cnt;

        ScmObj func = vm->prof->counts[i].func;
        if (SCM_METHODP(func) && SCM_METHOD(func)->func == NULL) {
            /* func is Scheme-defined method.  Record the code of
               method body, so that we can match it with sampling
               profiler later. */
            func = SCM_OBJ(SCM_METHOD(func)->data);
        }

        e = Scm_HashTableSet(vm->prof->statHash,
                             vm->prof->counts[i].func,
                             SCM_FALSE,
                             SCM_DICT_NO_OVERWRITE);
        if (SCM_FALSEP(e)) {
            e = Scm_HashTableSet(vm->prof->statHash,
                                 vm->prof->counts[i].func,
                                 Scm_Cons(SCM_MAKE_INT(0), SCM_MAKE_INT(0)),
                                 0);
        }

        SCM_ASSERT(SCM_PAIRP(e));
        cnt = SCM_INT_VALUE(SCM_CAR(e)) + 1;
        SCM_SET_CAR(e, SCM_MAKE_INT(cnt));
    }
    vm->prof->currentCount = 0;

    /* resume itimer */
    SIGPROCMASK(SIG_UNBLOCK, &set, NULL);
}

/*=============================================================
 * External API
 */
void Scm_ProfilerStart(void)
{
    ScmVM *vm = Scm_VM();
    ScmObj templat = Scm_StringAppendC(SCM_STRING(Scm_TmpDir()),
                                       "/gauche-profXXXXXX", -1, -1);
    char *templat_buf = Scm_GetString(SCM_STRING(templat)); /*mutable copy*/

    if (!vm->prof) {
        vm->prof = SCM_NEW(ScmVMProfiler);
        vm->prof->state = SCM_PROFILER_INACTIVE;
        vm->prof->samplerFd = Scm_Mkstemp(templat_buf);
        vm->prof->currentSample = 0;
        vm->prof->totalSamples = 0;
        vm->prof->errorOccurred = 0;
        vm->prof->currentCount = 0;
        vm->prof->statHash =
            SCM_HASH_TABLE(Scm_MakeHashTableSimple(SCM_HASH_EQ, 0));
        unlink(templat_buf);       /* keep anonymous tmpfile */
    } else if (vm->prof->samplerFd < 0) {
        vm->prof->samplerFd = Scm_Mkstemp(templat_buf);
        unlink(templat_buf);
    }

    if (vm->prof->state == SCM_PROFILER_RUNNING) return;
    vm->prof->state = SCM_PROFILER_RUNNING;
    vm->profilerRunning = TRUE;

    /* NB: this should be done globally!!! */
    struct sigaction act;
    act.sa_handler = sampler_sample;
    sigfillset(&act.sa_mask);
    act.sa_flags = SA_RESTART;
    if (sigaction(SIGPROF, &act, NULL) < 0) {
        Scm_SysError("sigaction failed");
    }

    ITIMER_START();
}

int Scm_ProfilerStop(void)
{
    ScmVM *vm = Scm_VM();
    if (vm->prof == NULL) return 0;
    if (vm->prof->state != SCM_PROFILER_RUNNING) return 0;
    ITIMER_STOP();
    vm->prof->state = SCM_PROFILER_PAUSING;
    vm->profilerRunning = FALSE;
    return vm->prof->totalSamples;
}

void Scm_ProfilerReset(void)
{
    ScmVM *vm = Scm_VM();

    if (vm->prof == NULL) return;
    if (vm->prof->state == SCM_PROFILER_INACTIVE) return;
    if (vm->prof->state == SCM_PROFILER_RUNNING) Scm_ProfilerStop();

    if (vm->prof->samplerFd >= 0) {
        close(vm->prof->samplerFd);
        vm->prof->samplerFd = -1;
    }
    vm->prof->totalSamples = 0;
    vm->prof->currentSample = 0;
    vm->prof->errorOccurred = 0;
    vm->prof->currentCount = 0;
    vm->prof->statHash =
        SCM_HASH_TABLE(Scm_MakeHashTableSimple(SCM_HASH_EQ, 0));
    vm->prof->state = SCM_PROFILER_INACTIVE;
}

/* Returns the statHash */
ScmObj Scm_ProfilerRawResult(void)
{
    ScmVM *vm = Scm_VM();

    if (vm->prof == NULL) return SCM_FALSE;
    if (vm->prof->state == SCM_PROFILER_INACTIVE) return SCM_FALSE;
    if (vm->prof->state == SCM_PROFILER_RUNNING) Scm_ProfilerStop();

    if (vm->prof->errorOccurred > 0) {
        Scm_Warn("profiler: An error has been occurred during saving profiling samples.  The result may not be accurate");
    }

    Scm_ProfilerCountBufferFlush(vm);

    /* collect samples in the current buffer */
    collect_samples(vm->prof);

    /* collect samples in the saved file */
    off_t off;
    SCM_SYSCALL(off, lseek(vm->prof->samplerFd, 0, SEEK_SET));
    if (off == (off_t)-1) {
        Scm_ProfilerReset();
        Scm_Error("profiler: seek failed in retrieving sample data");
    }
    for (;;) {
        ssize_t r = read(vm->prof->samplerFd, vm->prof->samples,
                         sizeof(ScmProfSample[1]) * SCM_PROF_SAMPLES_IN_BUFFER);
        if (r <= 0) break;
        vm->prof->currentSample = r / sizeof(ScmProfSample[1]);
        collect_samples(vm->prof);
    }
    vm->prof->currentSample = 0;
    if (ftruncate(vm->prof->samplerFd, 0) < 0) {
        Scm_SysError("profiler: failed to truncate temporary file");
    }

    return SCM_OBJ(vm->prof->statHash);
}

#else  /* !GAUCHE_PROFILE */
void Scm_ProfilerStart(void)
{
    Scm_Error("profiler is not supported.");
}

int  Scm_ProfilerStop(void)
{
    Scm_Error("profiler is not supported.");
    return 0;
}

void Scm_ProfilerReset(void)
{
    Scm_Error("profiler is not supported.");
}

ScmObj Scm_ProfilerRawResult(void)
{
    Scm_Error("profiler is not supported.");
    return SCM_FALSE;
}
#endif /* !GAUCHE_PROFILE */
