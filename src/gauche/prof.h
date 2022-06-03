/*
 * prof.h - Profiler
 *
 *   Copyright (c) 2005-2022  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_PROF_H
#define GAUCHE_PROF_H

/*=============================================================
 * Profiler
 */

/* We have two types of profilers, a statistic sampler and call-counter.
 *
 * The statistic sampler uses ITIMER_PROF and records the current code
 * base and PC for every SIGPROF.
 * (NB: in order for this to work, VM's PC must always be saved
 * in VM structure; in another word, vm.c must be compiled with
 * SMALL_REGS == 0).
 *
 * The call counter records every event of CALL and TAIL-CALL instruction
 * execution on the thread.   Each entry just records the address of
 * the called object.
 *
 * TODO: It is not known if sampling profiler works when more than one
 * thread requests profiling.  Should be considrered later.
 *
 * When the on-memory buffer of the call counter gets full, it is collected
 * to a hash table.  When the statistic sampling buffer gets full, it
 * is flushed to a temporary file (we can't use a hashtable, since the
 * flushing may be done within a signal handler and we can't call allocator
 * in it).
 *
 * Profiler status:
 *
 *        Scm_ProfilerStart    Scm_ProfilerStop
 *            -------->         --------->
 *    INACTIVE          RUNNING           PAUSING -----\
 *                              <--------              |
 *        ^                    Scm_ProfilerStart       |
 *        |                                            |
 *        \--------------------------------------------/
 *                      Scm_ProfilerReset
 *
 * Profile result can only be examined when the profile is in "PAUSING"
 * state.
 */

/* Profiler status */
enum {
    SCM_PROFILER_INACTIVE,
    SCM_PROFILER_RUNNING,
    SCM_PROFILER_PAUSING
};

/* A sample of statistic sampler */
typedef struct ScmProfSampleRec {
    ScmObj func;                /* ScmCompiledCode or ScmSubr */
    ScmWord *pc;
} ScmProfSample;

/* # of on-memory samples for the statistic sampler. */
#define SCM_PROF_SAMPLES_IN_BUFFER  6000

/* A record of call counter */
typedef struct ScmProfCountRec {
    ScmObj func;                /* Called Function */
} ScmProfCount;

/* # of on-memory samples for the call counter. */
#define SCM_PROF_COUNTER_IN_BUFFER  12000

/* Profiling buffer.
 * It is allocated when profiler-start is called on this thread
 * for the first time.
 */
struct ScmVMProfilerRec {
    int state;                  /* profiler state */
    int samplerFd;              /* temporary file for the sampler */
    int currentSample;          /* index to the current sample */
    int totalSamples;           /* total # of samples */
    int errorOccurred;          /* TRUE if error has occurred during I/O */
    int currentCount;           /* index to the current counter */
    ScmHashTable* statHash;     /* hashtable for collected data.
                                   value is a pair of integers,
                                   (<call-count> . <sample-hits>) */
#if defined(GAUCHE_WINDOWS)
    HANDLE hTargetThread;       /* target thread */
    HANDLE hObserverThread;     /* observer thread */
    HANDLE hTimerEvent;         /* sampling timer event */
    char *samplerFileName;      /* temporary file name to remove the file */
#endif /* GAUCHE_WINDOWS */
    ScmProfSample samples[SCM_PROF_SAMPLES_IN_BUFFER];
    ScmProfCount  counts[SCM_PROF_COUNTER_IN_BUFFER];
};

SCM_EXTERN ScmObj Scm_ProfilerRawResult(void);

/* Call Counter API */

SCM_EXTERN void Scm_ProfilerCountBufferFlush(ScmVM *vm);

#ifdef GAUCHE_PROFILE
#define SCM_PROF_COUNT_CALL(vm, obj)                                    \
    do {                                                                \
        if (MOSTLY_FALSE(vm->profilerRunning)) {                        \
            if (vm->prof->currentCount == SCM_PROF_COUNTER_IN_BUFFER) { \
                Scm_ProfilerCountBufferFlush(vm);                       \
            }                                                           \
            vm->prof->counts[vm->prof->currentCount++].func = obj;      \
        }                                                               \
    } while (0)
#else  /*!GAUCHE_PROFILE*/
#define SCM_PROF_COUNT_CALL(vm, obj)  /*empty*/
#endif /*!GAUCHE_PROFILE*/


#endif  /*GAUCHE_PROF_H*/
