;;;
;;; libsys.scm - builtin system inteface
;;;
;;;   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

;; System interface functions.   Mostly I followed POSIX.1, but included
;; some non-posix functions which are important for programming on Unix.

(select-module gauche)
(inline-stub
 (declcode
  (.include <stdlib.h> <locale.h> <math.h> <sys/types.h> <sys/stat.h>)
  (.cond ["TIME_WITH_SYS_TIME" (.include <sys/time.h> <time.h>)]
         ["HAVE_SYS_TIME_H"    (.include <sys/time.h>)]
         [else                 (.include <time.h>)])
  (.if "!defined(GAUCHE_WINDOWS)"
       (.include <grp.h> <pwd.h> <sys/wait.h> <utime.h>
                 <sys/times.h> <sys/utsname.h>))
  (.if "HAVE_CRYPT_H"        (.include <crypt.h>))
  (.if "HAVE_SYS_RESOURCE_H" (.include <sys/resource.h>))
  (.if "HAVE_SYS_LOADAVG_H"  (.include <sys/loadavg.h>))
  (.if "HAVE_UNISTD_H"       (.include <unistd.h>))

  (.if "defined(GAUCHE_WINDOWS)"
       (.undef _SC_CLK_TCK)) ;; avoid undefined reference to sysconf
  ))

;;---------------------------------------------------------------------
;; dirent.h - read directory
;;   we don't have correspoinding functions, but provide these:

(select-module gauche)
(define-cproc sys-readdir (pathname::<string>) Scm_ReadDirectory)

;; Bonus

(define-cproc sys-normalize-pathname (pathname::<string>
                                      :key (absolute #f)
                                      (expand #f)
                                      (canonicalize #f))
  (let* ([flags::int 0])
    (unless (SCM_FALSEP absolute) (logior= flags SCM_PATH_ABSOLUTE))
    (unless (SCM_FALSEP expand)   (logior= flags SCM_PATH_EXPAND))
    (unless (SCM_FALSEP canonicalize) (logior= flags SCM_PATH_CANONICALIZE))
    (return (Scm_NormalizePathname pathname flags))))

(define-cproc sys-tmpdir () Scm_TmpDir)
(define-cproc sys-basename (pathname::<string>) Scm_BaseName)
(define-cproc sys-dirname (pathname::<string>) Scm_DirName)

;;---------------------------------------------------------------------
;; errno.h - error numbers

;; We won't (and can't) cover every possible errnos, including system
;; specific ones.  The following list is taken from Linux asm/errno.h.

(inline-stub
 "ScmHashTable *errno_n2y;"             ;integer -> symbol
 "ScmHashTable *errno_y2n;"             ;symbol -> integer
 (initcode
  (set! errno_n2y (SCM_HASH_TABLE (Scm_MakeHashTableSimple SCM_HASH_EQV 0)))
  (set! errno_y2n (SCM_HASH_TABLE (Scm_MakeHashTableSimple SCM_HASH_EQ 0)))))
  

(define-macro (define-errno symbol)
  `(inline-stub
    (initcode
     (.if ,#`"defined(,symbol)"
          (begin
            (Scm_Define (Scm_GaucheModule)
                        (SCM_SYMBOL ',symbol) (SCM_MAKE_INT ,symbol))
            (Scm_HashTableSet errno_n2y (SCM_MAKE_INT ,symbol) ',symbol 0)
            (Scm_HashTableSet errno_y2n ',symbol (SCM_MAKE_INT ,symbol) 0))))))

(define-cproc sys-errno->symbol (num::<fixnum>)
  (return (Scm_HashTableRef errno_n2y (SCM_MAKE_INT num) SCM_FALSE)))
(define-cproc sys-symbol->errno (name::<symbol>)
  (return (Scm_HashTableRef errno_y2n (SCM_OBJ name) SCM_FALSE)))

(define-errno E2BIG)
(define-errno EACCES)
(define-errno EADDRINUSE)
(define-errno EADDRNOTAVAIL)
(define-errno EADV)
(define-errno EAFNOSUPPORT)
(define-errno EAGAIN)
(define-errno EALREADY)
(define-errno EBADE)
(define-errno EBADF)
(define-errno EBADFD)
(define-errno EBADMSG)
(define-errno EBADR)
(define-errno EBADRQC)
(define-errno EBADSLT)
(define-errno EBFONT)
(define-errno EBUSY)
(define-errno ECANCELED)
(define-errno ECHILD)
(define-errno ECHRNG)
(define-errno ECOMM)
(define-errno ECONNABORTED)
(define-errno ECONNREFUSED)
(define-errno ECONNRESET)
(define-errno EDEADLK)
(define-errno EDEADLOCK)
(define-errno EDESTADDRREQ)
(define-errno EDOM)
(define-errno EDOTDOT)
(define-errno EDQUOT)
(define-errno EEXIST)
(define-errno EFAULT)
(define-errno EFBIG)
(define-errno EHOSTDOWN)
(define-errno EHOSTUNREACH)
(define-errno EIDRM)
(define-errno EILSEQ)
(define-errno EINPROGRESS)
(define-errno EINTR)
(define-errno EINVAL)
(define-errno EIO)
(define-errno EISCONN)
(define-errno EISDIR)
(define-errno EISNAM)
(define-errno EKEYEXPIRED)
(define-errno EKEYREJECTED)
(define-errno EKEYREVOKED)
(define-errno EL2HLT)
(define-errno EL2NSYNC)
(define-errno EL3HLT)
(define-errno EL3RST)
(define-errno ELIBACC)
(define-errno ELIBBAD)
(define-errno ELIBEXEC)
(define-errno ELIBMAX)
(define-errno ELIBSCN)
(define-errno ELNRNG)
(define-errno ELOOP)
(define-errno EMEDIUMTYPE)
(define-errno EMFILE)
(define-errno EMLINK)
(define-errno EMSGSIZE)
(define-errno EMULTIHOP)
(define-errno ENAMETOOLONG)
(define-errno ENAVAIL)
(define-errno ENETDOWN)
(define-errno ENETRESET)
(define-errno ENETUNREACH)
(define-errno ENFILE)
(define-errno ENOANO)
(define-errno ENOBUFS)
(define-errno ENOCSI)
(define-errno ENODATA)
(define-errno ENODEV)
(define-errno ENOENT)
(define-errno ENOEXEC)
(define-errno ENOKEY)
(define-errno ENOLCK)
(define-errno ENOLINK)
(define-errno ENOMEDIUM)
(define-errno ENOMEM)
(define-errno ENOMSG)
(define-errno ENONET)
(define-errno ENOPKG)
(define-errno ENOPROTOOPT)
(define-errno ENOSPC)
(define-errno ENOSR)
(define-errno ENOSTR)
(define-errno ENOSYS)
(define-errno ENOTBLK)
(define-errno ENOTCONN)
(define-errno ENOTDIR)
(define-errno ENOTEMPTY)
(define-errno ENOTNAM)
(define-errno ENOTSOCK)
(define-errno ENOTTY)
(define-errno ENOTUNIQ)
(define-errno ENXIO)
(define-errno EOPNOTSUPP)
(define-errno EOVERFLOW)
(define-errno EPERM)
(define-errno EPFNOSUPPORT)
(define-errno EPIPE)
(define-errno EPROTO)
(define-errno EPROTONOSUPPORT)
(define-errno EPROTOTYPE)
(define-errno ERANGE)
(define-errno EREMCHG)
(define-errno EREMOTE)
(define-errno EREMOTEIO)
(define-errno ERESTART)
(define-errno EROFS)
(define-errno ESHUTDOWN)
(define-errno ESOCKTNOSUPPORT)
(define-errno ESPIPE)
(define-errno ESRCH)
(define-errno ESRMNT)
(define-errno ESTALE)
(define-errno ESTRPIPE)
(define-errno ETIME)
(define-errno ETIMEDOUT)
(define-errno ETOOMANYREFS)
(define-errno ETXTBSY)
(define-errno EUCLEAN)
(define-errno EUNATCH)
(define-errno EUSERS)
(define-errno EWOULDBLOCK)
(define-errno EXDEV)
(define-errno EXFULL)

;;---------------------------------------------------------------------
;; grp.h - groups

(inline-stub
 (define-type <sys-group> "ScmSysGroup*")
 )

(define-cproc sys-getgrgid (gid::<int>) Scm_GetGroupById)
(define-cproc sys-getgrnam (name::<string>) Scm_GetGroupByName)

;; faster functions; bypassing creation of group object
(define-cproc sys-gid->group-name (gid::<int>)
  (let* ([g::(struct group*) (getgrgid gid)])
    (cond [(== g NULL) (Scm_SigCheck (Scm_VM)) (return '#f)]
          [else (return (SCM_MAKE_STR_COPYING (-> g gr_name)))])))
(define-cproc sys-group-name->gid (name::<const-cstring>)
  (let* ([g::(struct group*) (getgrnam name)])
    (cond [(== g NULL) (Scm_SigCheck (Scm_VM)) (return '#f)]
          [else (return (Scm_MakeInteger (-> g gr_gid)))])))

;;---------------------------------------------------------------------
;; locale.h

(inline-stub
 (define-enum LC_ALL)
 (define-enum LC_COLLATE)
 (define-enum LC_CTYPE)
 (define-enum LC_MONETARY)
 (define-enum LC_NUMERIC)
 (define-enum LC_TIME)

 (define-cise-expr lc-elt
  [(_ conv sym) `(Scm_Cons ',sym (,conv (-> lc ,sym)))])
)

(define-cproc sys-setlocale (category::<fixnum> locale::<const-cstring>)
  ::<const-cstring>? setlocale)

(define-cproc sys-localeconv ()
  (let* ([lc::(struct lconv*) (localeconv)])
    (return (list (lc-elt SCM_MAKE_STR_COPYING decimal_point)
                  (lc-elt SCM_MAKE_STR_COPYING thousands_sep)
                  (lc-elt SCM_MAKE_STR_COPYING grouping)
                  (lc-elt SCM_MAKE_STR_COPYING int_curr_symbol)
                  (lc-elt SCM_MAKE_STR_COPYING currency_symbol)
                  (lc-elt SCM_MAKE_STR_COPYING mon_decimal_point)
                  (lc-elt SCM_MAKE_STR_COPYING mon_thousands_sep)
                  (lc-elt SCM_MAKE_STR_COPYING mon_grouping)
                  (lc-elt SCM_MAKE_STR_COPYING positive_sign)
                  (lc-elt SCM_MAKE_STR_COPYING negative_sign)
                  (lc-elt SCM_MAKE_INT int_frac_digits)
                  (lc-elt SCM_MAKE_INT frac_digits)
                  (lc-elt SCM_MAKE_BOOL p_cs_precedes)
                  (lc-elt SCM_MAKE_BOOL p_sep_by_space)
                  (lc-elt SCM_MAKE_BOOL n_cs_precedes)
                  (lc-elt SCM_MAKE_BOOL n_sep_by_space)
                  (lc-elt SCM_MAKE_INT p_sign_posn)
                  (lc-elt SCM_MAKE_INT n_sign_posn)))))

;;---------------------------------------------------------------------
;; math.h

;; fmod, frexp, modf, ldexp, log10 - in libnum.scm

;;---------------------------------------------------------------------
;; pwd.h - passwords

(inline-stub
 (define-type <sys-passwd> "ScmSysPasswd*")
 )

(define-cproc sys-getpwuid (uid::<int>) Scm_GetPasswdById)
(define-cproc sys-getpwnam (name::<string>) Scm_GetPasswdByName)

;; faster functions; bypassing creation of passwd object
(define-cproc sys-uid->user-name (uid::<int>)
  (let* ([p::(struct passwd*) (getpwuid uid)])
    (cond [(== p NULL) (Scm_SigCheck (Scm_VM)) (return '#f)]
          [else (return (SCM_MAKE_STR_COPYING (-> p pw_name)))])))
(define-cproc sys-user-name->uid (name::<const-cstring>)
  (let* ([p::(struct passwd*) (getpwnam name)])
    (cond [(== p NULL) (Scm_SigCheck (Scm_VM)) (return '#f)]
          [else (return (Scm_MakeInteger (-> p pw_uid)))])))

;;---------------------------------------------------------------------
;; signal.h

(inline-stub
 (define-type <sys-sigset> "ScmSysSigset*")

 (define-enum SIG_SETMASK)
 (define-enum SIG_BLOCK)
 (define-enum SIG_UNBLOCK)
 )

(define-cproc sys-sigset-add! (set::<sys-sigset> :rest sigs)
  (return (Scm_SysSigsetOp set sigs FALSE)))

(define-cproc sys-sigset-delete! (set::<sys-sigset> :rest sigs)
  (return (Scm_SysSigsetOp set sigs TRUE)))

(define-cproc sys-sigset-fill! (set::<sys-sigset>)
  (return (Scm_SysSigsetFill set FALSE)))

(define-cproc sys-sigset-empty! (set::<sys-sigset>)
  (return (Scm_SysSigsetFill set TRUE)))

(define-cproc sys-signal-name (sig::<fixnum>) Scm_SignalName)

(define-cproc sys-kill (process sig::<fixnum>) ::<void> Scm_SysKill)

(define-cproc set-signal-handler! (sig proc :optional (mask::<sys-sigset>? #f))
  Scm_SetSignalHandler)
(define-cproc get-signal-handler (sig::<fixnum>) Scm_GetSignalHandler)
(define-cproc get-signal-handler-mask (sig::<fixnum>) Scm_GetSignalHandlerMask)
(define-cproc get-signal-handlers () Scm_GetSignalHandlers)

(define-cproc set-signal-pending-limit (limit::<fixnum>) ::<void>
  Scm_SetSignalPendingLimit)
(define-cproc get-signal-pending-limit () ::<int>
  Scm_GetSignalPendingLimit)

(define-cproc sys-sigmask (how::<fixnum> mask::<sys-sigset>?) Scm_SysSigmask)

(define-cproc sys-sigsuspend (mask::<sys-sigset>) Scm_SigSuspend)

(define-cproc sys-sigwait (mask::<sys-sigset>) ::<int> Scm_SigWait)

(inline-stub
 (initcode (.if "defined HAVE_SIGWAIT"
                (Scm_AddFeature "gauche.sys.sigwait" NULL))))

(define (sys-sigset . signals)
  (if (null? signals)
    (make <sys-sigset>)
    (apply sys-sigset-add! (make <sys-sigset>) signals)))

;;---------------------------------------------------------------------
;; stdio.h

(define-cproc sys-remove (filename::<const-cstring>) ::<void>
  (let* ([r::int])
    (SCM_SYSCALL r (remove filename))
    (when (< r 0) (Scm_SysError "remove failed on %s" filename))))

(define-cproc sys-rename (oldname::<const-cstring>
                          newname::<const-cstring>)
  ::<void>
  (let* ([r::int])
    (.if "defined(GAUCHE_WINDOWS)"
         ;; Windows doesn't allow renaming to the existing file, so we unlink
         ;; it first.  This breaks the atomicity of rename operation.
         ;; We don't check and raise an error here, since the error will be
         ;; caught by rename() call.
         (begin (chmod newname #o666) (unlink newname)))
    (SCM_SYSCALL r (rename oldname newname))
    (when (< r 0) (Scm_SysError "renaming %s to %s failed" oldname newname))))

;; NB: Alghough tmpnam() is in POSIX, its use is discouraged because of
;; potential security risk.  We mimic it's behavior by mkstemp() if possible.
(define-cproc sys-tmpnam ()
  (.if "HAVE_MKSTEMP"
       (let* ([nam::(.array char [*]) "/tmp/fileXXXXXX"] [fd::int])
         (SCM_SYSCALL fd (mkstemp nam))
         (when (< fd 0) (Scm_SysError "mkstemp failed"))
         (close fd)
         (unlink nam)
         (return (SCM_MAKE_STR_COPYING nam)))
       (let* ([s::char* (tmpnam NULL)])
         (return (SCM_MAKE_STR_COPYING s)))))

(define-cproc sys-mkstemp (template::<string>) Scm_SysMkstemp)

;; ctermid
(define-cproc sys-ctermid () ::<const-cstring>
  (.if "defined(GAUCHE_WINDOWS)"
       (return "CON")
       (let* ([buf::(.array char [(+ L_ctermid 1)])])
         (return (ctermid buf)))))

;;---------------------------------------------------------------------
;; stdlib.h

(define-cproc sys-exit (code::<fixnum>) ::<void> _exit)

(define-cproc sys-getenv (name::<const-cstring>) ::<const-cstring>? Scm_GetEnv)

(define-cproc sys-abort () ::<void> abort)

;; sys-realpath is in autoloaded sysutil.scm.

;; Note: the return value of system() is not portable.
;; NB: on WinNT, system("") aborts, so we filter it.
(define-cproc sys-system (command::<const-cstring>) ::<int>
  (if (== (aref command 0) 0)
    (return 0)
    (SCM_SYSCALL SCM_RESULT (system command))))

(define-cproc sys-random () ::<long>
  (.cond ["defined(HAVE_RANDOM) && defined(HAVE_SRANDOM)"
          (return (random))]
         ["defined(LRAND48) && defined(SRAND48)"
          (return (lrand48))]
         [else
          ;; fallback - we don't want to use rand(), for it is not
          ;; a very good RNG.
          (return (rand))]))

(define-cproc sys-srandom (seed) ::<void>
  (unless (SCM_EXACTP seed) (Scm_Error "exact integer required: %S" seed))
  (.cond ["defined(HAVE_RANDOM) && defined(HAVE_SRANDOM)"
          (srandom (Scm_GetUInteger seed))]
         ["defined(LRAND48) && defined(SRAND48)"
          (srand48 (Scm_GetUInteger seed))]
         [else
          ;; fallback - we don't want to use rand(), for it is not
          ;; a very good RNG.
          (srand (Scm_GetUInteger seed))]))

(inline-stub
 (define-constant RAND_MAX (c "Scm_MakeIntegerFromUI(RAND_MAX)"))
 )

(define-cproc sys-environ () Scm_Environ)

;; NB:
(define-cproc sys-setenv (name::<const-cstring>
                          value::<const-cstring>
                          :optional (overwrite::<boolean> #f))
  ::<void> Scm_SetEnv)
(define-cproc sys-unsetenv (name::<const-cstring>) ::<void> Scm_UnsetEnv)
(define-cproc sys-clearenv () ::<void> Scm_ClearEnv)

(inline-stub
 (initcode
  (.if "defined(HAVE_PUTENV) || defined(HAVE_SETENV)"
       (Scm_AddFeature "gauche.sys.setenv" NULL))
  (.if "defined HAVE_UNSETENV"
       (Scm_AddFeature "gauche.sys.unsetenv" NULL))
  (.if "defined HAVE_CLEARENV"
       (Scm_AddFeature "gauche.sys.clearenv" NULL))))

(define (sys-environ->alist :optional (envlist (sys-environ)))
  (map (^[envstr] (receive (pre post) (string-scan envstr #\= 'both)
                    (if pre (cons pre post) (cons envstr ""))))
       envlist))

;; We implement sys-putenv on top of sys-setenv, which in turn uses
;; either setenv(3) or putenv(3) based on the availability.
;; We had old sys-putenv API as (sys-putenv name value), while the new one
;; takes a single argument for the consistency to putenv(3).  We support
;; both APIs.
;; NB: We don't check the platform allows modifying environments.  If not,
;; sys-setenv throws an error.  It's up to the application to check the
;; availability by (cond-expand [gauche.sys.setenv ...]).
(define (sys-putenv name=value . other)
  (cond
   [(null? other)
    (check-arg string? name=value)
    (receive (name value) (string-scan name=value #\= 'both)
      (unless name
        (error "sys-putenv: argument doesn't contain '=':" name=value))
      (sys-setenv name value #t))]
   [else (sys-setenv name=value (car other) #t)]))

;;---------------------------------------------------------------------
;; string.h

;; TODO: for thread safety, we should use strerror_r when available.
;; unfortunately there are conflicting versions of strerror_r among
;; various systems.
(define-cproc sys-strerror (errno_::<int>) ::<const-cstring> strerror)

;;---------------------------------------------------------------------
;; sys/loadavg.h

(define-cproc sys-getloadavg (:optional (nsamples::<int> 3))
  (.if "defined HAVE_GETLOADAVG"
       (let* ([samples::(.array double [3])])
         (when (or (<= nsamples 0) (> nsamples 3))
           (Scm_Error "sys-getloadavg: argument out of range: %d" samples))
         (let* ([count::int (getloadavg samples nsamples)])
           (if (< count 0)
             (return '#f)
             (let* ([h '()] [t '()])
               (dotimes [i count]
                 (let* ([n (Scm_MakeFlonum (aref samples i))])
                   (SCM_APPEND1 h t n)))
               (return h)))))
       (Scm_Error "sys-getloadavg isn't supported on this platform")))

(inline-stub
 (initcode (.if "defined HAVE_GETLOADAVG"
                (Scm_AddFeature "gauche.sys.getloadavg" NULL))))

;;---------------------------------------------------------------------
;; sys/resource.h

(inline-stub
 (when "defined(HAVE_SYS_RESOURCE_H)"
   "
#if SIZEOF_RLIM_T == 4
#  define MAKERLIMIT(val)  Scm_MakeIntegerU(val)
#  define GETRLIMIT(obj)   Scm_GetIntegerU(obj)
#elif SIZEOF_RLIM_T == 8
#  define MAKERLIMIT(val)  Scm_MakeIntegerU64(val)
#  define GETRLIMIT(obj)   Scm_GetIntegerU64(obj)
#else
#  error \"rlim_t must be 32bit or 64bit\"
#endif
"
   (define-cproc sys-getrlimit (rsrc::<int>) ::(<integer> <integer>)
     (let* ([limit::(struct rlimit)] [ret::int])
       (SCM_SYSCALL ret (getrlimit rsrc (& limit)))
       (when (< ret 0) (Scm_SysError "getrlimit failed"))
       (return (MAKERLIMIT (ref limit rlim_cur))
               (MAKERLIMIT (ref limit rlim_max)))))

   (define-cproc sys-setrlimit (rsrc::<int> cur :optional (max #f)) ::<void>
     (let* ([limit::(struct rlimit)] [ret::int])
       (when (or (SCM_FALSEP cur) (SCM_FALSEP max))
         (SCM_SYSCALL ret (getrlimit rsrc (& limit)))
         (when (< ret 0) (Scm_SysError "getrlimit in sys-setrlimit failed")))
       (cond [(SCM_INTEGERP cur) (set! (ref limit rlim_cur) (GETRLIMIT cur))]
             [(not (SCM_FALSEP cur))
              (SCM_TYPE_ERROR cur "non-negative integer or #f")])
       (cond [(SCM_INTEGERP max) (set! (ref limit rlim_max) (GETRLIMIT max))]
             [(not (SCM_FALSEP max))
              (SCM_TYPE_ERROR max "non-negative integer or #f")])
       (SCM_SYSCALL ret (setrlimit rsrc (& limit)))
       (when (< ret 0) (Scm_SysError "setrlimit failed"))))

   (initcode (Scm_AddFeature "gauche.sys.getrlimit" NULL))

   (define-constant RLIM_INFINITY (c "MAKERLIMIT(RLIM_INFINITY)"))
   (define-enum-conditionally RLIMIT_AS)
   (define-enum-conditionally RLIMIT_CORE)
   (define-enum-conditionally RLIMIT_CPU)
   (define-enum-conditionally RLIMIT_DATA)
   (define-enum-conditionally RLIMIT_FSIZE)
   (define-enum-conditionally RLIMIT_LOCKS)
   (define-enum-conditionally RLIMIT_MEMLOCK)
   (define-enum-conditionally RLIMIT_MSGQUEUE)
   (define-enum-conditionally RLIMIT_NICE)
   (define-enum-conditionally RLIMIT_NOFILE)
   (define-enum-conditionally RLIMIT_NPROC)
   (define-enum-conditionally RLIMIT_RSS)
   (define-enum-conditionally RLIMIT_RTPRIO)
   (define-enum-conditionally RLIMIT_SIGPENDING)
   (define-enum-conditionally RLIMIT_SBSIZE)
   (define-enum-conditionally RLIMIT_STACK)
   (define-enum-conditionally RLIMIT_OFILE)
   ) ;; HAVE_SYS_RESOURCE_H
 )

;;---------------------------------------------------------------------
;; sys/stat.h

(inline-stub
 (define-type <sys-stat> "ScmSysStat*")

 ;; Commn code for stat and lstat.
 (define-cise-stmt stat-common
   [(_ statfn)
    `(let* ([s::ScmSysStat* (SCM_SYS_STAT (Scm_MakeSysStat))] [r::int]
            [p::(const char*) (check-trailing-separator path)])
       (SCM_SYSCALL r (,statfn p (SCM_SYS_STAT_STAT s)))
       (when (< r 0) (Scm_SysError "%s failed for %s" ,(x->string statfn) p))
       (return s))])

 ;; On Windows stat() fails if PATH has a trailing directory separator,
 ;; except if we're stat()-ing the root directory.  For the convenience
 ;; we remove the trailing separator if any.  It is quite complicated,
 ;; for we have to deal with optional drive letters.
 (when "defined(GAUCHE_WINDOWS)"
   (define-cfn check-trailing-separator (path::(const char*))
     ::(const char*) :static
     (let* ([size::int (strlen path)]
            [ends::(const char*) (+ path size)]
            [lastchar::(const char*)])
       (when (== size 0) (return path))
       (SCM_CHAR_BACKWARD ends path lastchar)
       (unless (and (>= lastchar path) (< lastchar ends))
         (Scm_SysError "invalid pathname: %s" path))
       (when (and (not (== lastchar path))
                  (not (and (== lastchar (+ path 2))
                            (== (aref path 1) #\:)))
                  (or (== (* lastchar) #\\)
                      (== (* lastchar) #\/)))
         (let* ([pcopy::(char *) (SCM_NEW_ATOMIC_ARRAY (char) size)])
           (memcpy pcopy path (- size 1))
           (set! (aref pcopy (- size 1)) 0)
           (set! path (cast (const char *) pcopy))))
       (return path))))
 (when "!defined(GAUCHE_WINDOWS)"
   (define-cfn check-trailing-separator (path::(const char*))
     ::(const char*) :static (return path)))

 (define-cproc sys-stat (path::<const-cstring>)
   ::<sys-stat> (stat-common stat))

 ;; On Windows we don't have lstat.  Omitting sys-lstat from Windows is
 ;; a bit inconvenient, however, to write a portable code if we do so.
 ;; Since lstat() works identical to stat() if the path is a symlink, and
 ;; on Windows path can never be a symlink, so we can just make sys-lstat
 ;; work the same as sys-stat.
 (when "!defined(GAUCHE_WINDOWS)"
   (define-cproc sys-lstat (path::<const-cstring>) ::<sys-stat>
     (stat-common lstat)))
 (when "defined(GAUCHE_WINDOWS)"
   (define-cproc sys-lstat (path::<const-cstring>) ::<sys-stat>
     (stat-common stat)))

 (when "!defined(GAUCHE_WINDOWS)"
   (define-cproc sys-mkfifo (path::<const-cstring> mode::<int>) ::<int>
     (SCM_SYSCALL SCM_RESULT (mkfifo path mode))
     (when (< SCM_RESULT 0) (Scm_SysError "mkfifo failed on %s" path))))
)

(define-cproc sys-fstat (port-or-fd)
  (let* ([s::ScmSysStat* (SCM_SYS_STAT (Scm_MakeSysStat))]
         [fd::int (Scm_GetPortFd port-or-fd FALSE)]
         [r::int])
    (cond [(< fd 0) (return SCM_FALSE)]
          [else (SCM_SYSCALL r (fstat fd (SCM_SYS_STAT_STAT s)))
                (when (< r 0) (Scm_SysError "fstat failed for %d" fd))
                (return (SCM_OBJ s))])))

(define-cproc file-exists? (path::<const-cstring>) ::<boolean>
  (let* ([r::int])
    (SCM_SYSCALL r (access path F_OK))
    (return (== r 0))))

(inline-stub
 (define-cise-stmt file-check-common
   [(_ checker)
    `(let* ([r::int] [s::(struct stat)]
            [p::(const char*) (check-trailing-separator path)])
       (SCM_SYSCALL r (access p F_OK))
       (if (== r 0)
         (begin (SCM_SYSCALL r (stat p (& s)))
                (when (< r 0)
                  (Scm_SysError "stat failed for %s" path))
                (return (,checker (ref s st_mode))))
         (return FALSE)))])

 (define-cproc file-is-regular? (path::<const-cstring>) ::<boolean>
   (file-check-common S_ISREG))
 (define-cproc file-is-directory? (path::<const-cstring>) ::<boolean>
   (file-check-common S_ISDIR))
 )

;; utime.h
(define-cproc sys-utime
  (path::<const-cstring> :optional (atime #f) (mtime #f)) ::<void>
  (let* ([tim::(struct utimbuf)] [r::int])
    (cond [(and (SCM_FALSEP atime) (SCM_FALSEP mtime))
           (SCM_SYSCALL r (utime path NULL))]
          [else
           (set! (ref tim actime)
                 (?: (SCM_FALSEP atime) (time NULL) (Scm_GetUInteger atime)))
           (set! (ref tim modtime)
                 (?: (SCM_FALSEP mtime) (time NULL) (Scm_GetUInteger mtime)))
           (SCM_SYSCALL r (utime path (& tim)))])
    (when (< r 0) (Scm_SysError "utime failed on %s" path))))

;;---------------------------------------------------------------------
;; sys/times.h

;; we have emulation of times() in auxsys.c for mingw.
(define-cproc sys-times ()
  (let* ([info::(struct tms)] [r::clock_t] [tick::long])
    (SCM_SYSCALL r (times (& info)))
    (when (== r (cast clock_t -1)) (Scm_SysError "times failed"))
    (.if "defined(_SC_CLK_TCK)"
         (set! tick (sysconf _SC_CLK_TCK))
         (.if "defined(CLK_TCK)"
              (set! tick CLK_TCK)   ; older name
              (set! tick 100)))     ; fallback
    (return (list (Scm_MakeInteger (ref info tms_utime))
                  (Scm_MakeInteger (ref info tms_stime))
                  (Scm_MakeInteger (ref info tms_cutime))
                  (Scm_MakeInteger (ref info tms_cstime))
                  (Scm_MakeInteger tick)))))

;;---------------------------------------------------------------------
;; sys/utsname.h

(define-cproc sys-uname ()
  (.if "!defined(GAUCHE_WINDOWS)"
       (let* ([info::(struct utsname)])
         (when (< (uname (& info)) 0) (Scm_SysError "uname failed"))
         (return (list (SCM_MAKE_STR_COPYING (ref info sysname))
                       (SCM_MAKE_STR_COPYING (ref info nodename))
                       (SCM_MAKE_STR_COPYING (ref info release))
                       (SCM_MAKE_STR_COPYING (ref info version))
                       (SCM_MAKE_STR_COPYING (ref info machine)))))
       ;; TODO: Fill with appropriate info.
       (return (list SCM_FALSE SCM_FALSE SCM_FALSE SCM_FALSE SCM_FALSE))))

;;---------------------------------------------------------------------
;; sys/wait.h

;; returns pid and status
(define-cproc sys-wait () (return (Scm_SysWait (SCM_MAKE_INT -1) 0)))

(define-cproc sys-waitpid (process :key (nohang #f) (untraced #f))
  (let* ([options::int 0])
    (unless (SCM_FALSEP nohang)   (logior= options WNOHANG))
    (unless (SCM_FALSEP untraced) (logior= options WUNTRACED))
    (return (Scm_SysWait process options))))

;; status interpretation
(define-cproc sys-wait-exited? (status::<int>) ::<boolean> WIFEXITED)
(define-cproc sys-wait-exit-status (status::<int>) ::<int> WEXITSTATUS)
(define-cproc sys-wait-signaled? (status::<int>) ::<boolean> WIFSIGNALED)
(define-cproc sys-wait-termsig (status::<int>) ::<int> WTERMSIG)
(define-cproc sys-wait-stopped? (status::<int>) ::<boolean> WIFSTOPPED)
(define-cproc sys-wait-stopsig (status::<int>) ::<int> WSTOPSIG)

;;---------------------------------------------------------------------
;; time.h

(inline-stub
 (define-type <time> "ScmTime*")
 (define-type <sys-tm> "ScmSysTm*")
 )

(define-cproc sys-time () (return (Scm_MakeSysTime (time NULL))))

(define-cproc sys-gettimeofday () ::(<ulong> <ulong>)
  (Scm_GetTimeOfDay (& SCM_RESULT0) (& SCM_RESULT1)))

(define-cproc current-microseconds ()   ;EXPERIMENTAL
  ::<long> Scm_CurrentMicroseconds)

;; Returns #f and #f if the system doesn't provide monotonic time.
(define-cproc sys-clock-gettime-monotonic () ::(<top> <top>)
  (let* ([sec::u_long] [nsec::u_long]
         [r::int (Scm_ClockGetTimeMonotonic (& sec) (& nsec))])
    (if r
      (begin (set! SCM_RESULT0 (Scm_MakeIntegerU sec))
             (set! SCM_RESULT1 (Scm_MakeIntegerU nsec)))
      (begin (set! SCM_RESULT0 SCM_FALSE)
             (set! SCM_RESULT1 SCM_FALSE)))))

;; Returns #f and #f if the system doesn't provide monotonic time.
(define-cproc sys-clock-getres-monotonic () ::(<top> <top>)
  (let* ([sec::u_long] [nsec::u_long]
         [r::int (Scm_ClockGetResMonotonic (& sec) (& nsec))])
    (if r
      (begin (set! SCM_RESULT0 (Scm_MakeIntegerU sec))
             (set! SCM_RESULT1 (Scm_MakeIntegerU nsec)))
      (begin (set! SCM_RESULT0 SCM_FALSE)
             (set! SCM_RESULT1 SCM_FALSE)))))

(define-cproc current-time ()           ;SRFI-18, SRFI-19, SRFI-21
  Scm_CurrentTime)

(define-cproc time? (obj)               ;SRFI-18, SRFI-19, SRFI-21
  ::<boolean> SCM_TIMEP)

(define-cproc time->seconds (t::<time>) ;SRFI-18
  Scm_TimeToSeconds)

(define-cproc seconds->time (t::<real>) ;SRFI-18
  Scm_RealSecondsToTime)

(define-cproc sys-asctime (tm::<sys-tm>)
  (return (SCM_MAKE_STR_COPYING (asctime (& (SCM_SYS_TM_TM tm))))))

(define-cproc sys-ctime (time) ::<const-cstring>
  (let* ([tim::time_t (Scm_GetSysTime time)]) (return (ctime (& tim)))))

(define-cproc sys-difftime (time1 time0) ::<double>
  (return (difftime (Scm_GetSysTime time1) (Scm_GetSysTime time0))))

(define-cproc sys-strftime (format::<const-cstring> tm::<sys-tm>)
  ::<const-cstring>
  (let* ([tmpbuf::(.array char [256])])
    (strftime tmpbuf (sizeof tmpbuf) format (& (SCM_SYS_TM_TM tm)))
    (return tmpbuf)))

(define-cproc sys-gmtime (time)
  (let* ([tim::time_t (Scm_GetSysTime time)])
    (return (Scm_MakeSysTm (gmtime (& tim))))))

(define-cproc sys-localtime (time)
  (let* ([tim::time_t (Scm_GetSysTime time)])
    (return (Scm_MakeSysTm (localtime (& tim))))))

(define-cproc sys-mktime (tm::<sys-tm>)
  (return (Scm_MakeSysTime (mktime (& (SCM_SYS_TM_TM tm))))))

;;---------------------------------------------------------------------
;; unistd.h - miscellaneous functions

(inline-stub
 (define-enum R_OK)
 (define-enum W_OK)
 (define-enum X_OK)
 (define-enum F_OK)
 )

(define-cproc sys-access (pathname::<const-cstring> amode::<int>)
  ::<boolean>
  (let* ([r::int])
    (when (Scm_IsSugid)
      (Scm_Error "cannot use sys-access in suid/sgid program."))
    (SCM_SYSCALL r (access pathname amode))
    (return (== r 0))))

(define-cproc sys-chdir (pathname::<const-cstring>) ::<void>
  (let* ([r::int])
    (SCM_SYSCALL r (chdir pathname))
    (when (< r 0) (Scm_SysError "chdir failed"))))

(define-cproc sys-chmod (pathname::<const-cstring> mode::<int>) ::<void>
  (let* ([r::int])
    (SCM_SYSCALL r (chmod pathname mode))
    (when (< r 0) (Scm_SysError "chmod failed"))))

(inline-stub
 (when "!defined(GAUCHE_WINDOWS)"
   (define-cproc sys-fchmod (port-or-fd mode::<int>) ::<void>
     (let* ([r::int] [fd::int (Scm_GetPortFd port-or-fd TRUE)])
       (SCM_SYSCALL r (fchmod fd mode))
       (when (< r 0) (Scm_SysError "fchmod failed"))))
   ) ;; !defined(GAUCHE_WINDOWS)
 )

;; chown
(define-cproc sys-chown (path::<const-cstring> owner::<int> group::<int>)
  ::<int>
  (.if "!defined(GAUCHE_WINDOWS)"
       (SCM_SYSCALL SCM_RESULT (chown path owner group))
       (set! SCM_RESULT 0))
  (when (< SCM_RESULT 0) (Scm_SysError "chown failed on %s" path)))

(inline-stub
 ;; lchown
 (when "defined HAVE_LCHOWN"
   (define-cproc sys-lchown (path::<const-cstring> owner::<int> group::<int>)
     ::<int>
     (SCM_SYSCALL SCM_RESULT (lchown path owner group))
     (when (< SCM_RESULT 0) (Scm_SysError "lchown failed on %S" path)))
   (initcode (Scm_AddFeature "gauche.sys.lchown" NULL))
   )
 )

;; NB: we force GC just before fork().  It appears necessary on some
;; platform to synchronize the page dirty bit information, so that incremental
;; GC can work properly.
(define-cproc sys-fork () ::<int>
  (let* ([pid::pid_t])
    (GC_gcollect)
    (SCM_SYSCALL pid (fork))
    (when (< pid 0) (Scm_SysError "fork failed"))
    (return pid)))

;; NB: the signature of old version was (command args :optional iomap).
;; The new version is (command args :key iomap sigmask).
;; For transitional stage, we accept both.
;; (define-cfn get-exec-args (rest iomap::ScmObj* sigmask::ScmSysSigset**)
;;   ::void :static
;;   (let* ([s_sigmask SCM_NIL])
;;     (set! (* iomap) SCM_NIL
;;           (* sigmask) NULL)
;;     (cond
;;      [(and (SCM_PAIRP rest) (SCM_NULLP (SCM_CDR rest))) ; old signature
;;       (set! (* iomap) (SCM_CAR rest))]
;;      [else
;;       (set! (* iomap) (Scm_GetKeyword ':iomap rest SCM_NIL)
;;             s_sigmask (Scm_GetKeyword ':sigmask rest SCM_FALSE))
;;       (cond [(SCM_SYS_SIGSET_P s_sigmask)
;;              (set! (* sigmask) (SCM_SYS_SIGSET s_sigmask))]
;;             [(not (SCM_FALSEP s_sigmask))
;;              (Scm_Error "<sys-sigset> or #f required, but got %S" s_sigmask)])
;;       ])))

(define-cproc sys-exec (command::<string>
                        args::<list>
                        :key (iomap ()) (sigmask::<sys-sigset>? #f)
                        (directory::<string>? #f)
                        (detached::<boolean> #f))
  ::<void>
  (let* ([flags::u_int (?: detached SCM_EXEC_DETACHED 0)])
    (Scm_SysExec command args iomap sigmask directory flags)))

(define-cproc sys-fork-and-exec (command::<string>
                                 args::<list>
                                 :key (iomap ()) (sigmask::<sys-sigset>? #f)
                                 (directory::<string>? #f)
                                 (detached::<boolean> #f))
  (let* ([flags::u_int SCM_EXEC_WITH_FORK])
    (when detached
      (set! flags (logior flags SCM_EXEC_DETACHED)))
    (return (Scm_SysExec command args iomap sigmask directory flags))))

(define-cproc sys-getcwd () Scm_GetCwd)
(define-cproc sys-getegid () ::<int> getegid)
(define-cproc sys-getgid ()  ::<int> getgid)
(define-cproc sys-geteuid () ::<int> geteuid)
(define-cproc sys-getuid ()  ::<int> getuid)

(define-cproc sys-getpid ()  ::<int> getpid)
(define-cproc sys-getppid () ::<int> getppid)

(inline-stub
 (when "!defined(GAUCHE_WINDOWS)"
   (define-cproc sys-setgid (gid::<int>) ::<int>
     (SCM_SYSCALL SCM_RESULT (setgid gid))
     (when (< SCM_RESULT 0) (Scm_SysError "setgid failed on %d" gid)))

   (define-cproc sys-setpgid (pid::<int> pgid::<int>) ::<int>
     (SCM_SYSCALL SCM_RESULT (setpgid pid pgid))
     (when (< SCM_RESULT 0)
       (Scm_SysError "setpgid failed on process %d for pgid %d" pid pgid)))

   ;; The prototype of setpgrp() differs between platforms.   We use
   ;; setpgid to implement sys-setpgrp.
   ;;(if (defined? "HAVE_SETPGRP")
   ;;    (define-cproc %sys-setpgrp ()
   ;;      "  int r = Scm_SysCall(setpgrp());
   ;;      if (r < 0) Scm_SysError(\"setpgrp failed\");
   ;;      SCM_RETURN(Scm_MakeInteger(r));"))

   (when "defined HAVE_GETPGID"
     (define-cproc sys-getpgid (pid::<int>) ::<int>
       (SCM_SYSCALL SCM_RESULT (cast int (getpgid pid)))
       (when (< SCM_RESULT 0) (Scm_SysError "getpgid failed")))
     (initcode (Scm_AddFeature "gauche.sys.getpgid" NULL))
     )

   (define-cproc sys-getpgrp () ::<int>
     (SCM_SYSCALL SCM_RESULT (cast int (getpgrp)))
     (when (< SCM_RESULT 0) (Scm_SysError "getpgrp failed")))

   (define-cproc sys-setsid () ::<int>
     (SCM_SYSCALL SCM_RESULT (setsid))
     (when (< SCM_RESULT 0) (Scm_SysError "setsid failed")))

   (define-cproc sys-setuid (uid::<int>) ::<int>
     (SCM_SYSCALL SCM_RESULT (setuid uid))
     (when (< SCM_RESULT 0) (Scm_SysError "setuid failed")))

   ;; some less-frequently used get-*

   (define-cproc sys-getgroups ()
     (let* ([size::int 32]
            [glist::(.array gid_t [32])]
            [pglist::gid_t* glist])
       (loop (let* ([n::int (getgroups size pglist)])
               (when (>= n 0)
                 (let* ([h '()] [t '()])
                   (dotimes [i n]
                     (SCM_APPEND1 h t (Scm_MakeInteger (aref pglist i))))
                   (set! SCM_RESULT h)
                   (break)))
               (cond [(== errno EINVAL)
                      (+= size size)
                      (set! pglist (SCM_NEW_ATOMIC_ARRAY gid_t size))]
                     [else (Scm_SysError "getgroups failed")])))))

   (when "defined HAVE_SETGROUPS"
     (define-cproc sys-setgroups (gids) ::<void>
       (let* ([ngid::int (Scm_Length gids)]
              [glist::gid_t* NULL]
              [k::int 0] [r::int])
         (when (< ngid 0)
           (Scm_Error "List of integer gids required, but got: %S" gids))
         (set! glist (SCM_NEW_ATOMIC_ARRAY gid_t ngid))
         (for-each (lambda (gid)
                     (unless (SCM_INTP gid)
                       (Scm_Error "gid list contains invalud value: %S" gid))
                     (set! (aref glist k) (SCM_INT_VALUE gid))
                     (post++ k))
                   gids)
         (SCM_SYSCALL r (setgroups ngid glist))
         (when (< r 0)
           (Scm_SysError "setgroups failed with %S" gids))))
     (initcode (Scm_AddFeature "gauche.sys.setgroups" NULL)))
   ) ;; !defined(GAUCHE_WINDOWS)
 )

(define (sys-setpgrp) (sys-setpgid 0 0))

(define-cproc sys-getlogin () ::<const-cstring>? getlogin)

(define-cproc sys-link (existing::<const-cstring>
                        newpath::<const-cstring>)
  ::<void>
  (let* ([r::int])
    (SCM_SYSCALL r (link existing newpath))
    (when (< r 0) (Scm_SysError "link failed"))))

(define-cproc sys-pause ()
  ;; We can't simply use pause().  If a signal is delivered after the last
  ;; Scm_SigCheck and before the call of pause(), the signal will just sit
  ;; in a queue and pause() may not return.
  Scm_Pause)

(define-cproc sys-alarm (seconds::<fixnum>) ::<int>
  (SCM_SYSCALL SCM_RESULT (alarm seconds)))

;; returns a list of two ports
(define-cproc sys-pipe (:key (name "(pipe)") (buffering #f) (buffered? #f))
  ::(<top> <top>)
  (let* ([fds::(.array int [2])] [mode::int] [r::int])
    (SCM_SYSCALL r (pipe fds))
    (when (< r 0) (Scm_SysError "pipe failed"))
    (if (SCM_TRUEP buffered?)
      (set! mode SCM_PORT_BUFFER_FULL) ; for backward compatibility
      (set! mode (Scm_BufferingMode buffering -1 SCM_PORT_BUFFER_LINE)))
    (return (Scm_MakePortWithFd name SCM_PORT_INPUT (aref fds 0) mode TRUE)
            (Scm_MakePortWithFd name SCM_PORT_OUTPUT (aref fds 1)mode TRUE))))

;; close integer file descriptor.  should only be used for
;; low-level file descriptor handling, and you know what you're doing.
;; closing a file descriptor that is still used by Scheme port would
;; result a disaster.
(define-cproc sys-close (fd::<int>) ::<void>
  (let* ([r::int])
    (SCM_SYSCALL r (close fd))
    (when (< r 0) (Scm_SysError "close failed on file descriptor %d" fd))))

(define-cproc sys-mkdir (pathname::<const-cstring> mode::<int>) ::<void>
  (let* ([r::int])
    (.if "!defined(GAUCHE_WINDOWS)"
         (SCM_SYSCALL r (mkdir pathname mode))
         (SCM_SYSCALL r (mkdir pathname)))
    (when (< r 0) (Scm_SysError "mkdir failed on %s" pathname))))

(define-cproc sys-rmdir (pathname::<const-cstring>) ::<void>
  (let* ([r::int])
    (SCM_SYSCALL r (rmdir pathname))
    (when (< r 0) (Scm_SysError "rmdir failed for %s" pathname))))

(define-cproc sys-umask (:optional mode) ::<int>
  (cond [(or (SCM_UNBOUNDP mode) (SCM_FALSEP mode))
         (let* ([prev::int (umask 0)])
           (umask prev)
           (return prev))]
        [(SCM_INTP mode) (return (umask (SCM_INT_VALUE mode)))]
        [else (SCM_TYPE_ERROR mode "fixnum or #f") (return 0)]))

(define-cproc sys-sleep (seconds::<fixnum>
                         :optional (no-retry::<boolean> #f))
  ::<int>
  (.if "defined(GAUCHE_WINDOWS)"
       (begin (Sleep (* seconds 1000)) (return 0))
       (let* ([k::u_int (cast (u_int) seconds)]
              [vm::ScmVM* (Scm_VM)])
         (while (> k 0)
           (set! k (sleep k))
           (SCM_SIGCHECK vm)
           (when no-retry (break)))
         (return k))))

(inline-stub
 (when "defined(HAVE_NANOSLEEP) || defined(GAUCHE_WINDOWS)"
   (define-cproc sys-nanosleep (nanoseconds
                                :optional (no-retry::<boolean> #f))
     (let* ([spec::ScmTimeSpec] [rem::ScmTimeSpec]
            [vm::ScmVM* (Scm_VM)])
       (cond
        [(SCM_TIMEP nanoseconds)
         (set! (ref spec tv_sec)  (-> (SCM_TIME nanoseconds) sec)
               (ref spec tv_nsec) (-> (SCM_TIME nanoseconds) nsec))]
        [(not (SCM_REALP nanoseconds))
         (Scm_Error "bad timeout spec: <time> object or real number is \
                    required, but got %S" nanoseconds)]
        [else
         (let* ([v::double (Scm_GetDouble nanoseconds)])
           (when (< v 0)
             (Scm_Error "bad timeout spec: positive number required, but got %S"
                        nanoseconds))
           (set! (ref spec tv_sec) (cast (unsigned long) (floor (/ v 1.0e9)))
                 (ref spec tv_nsec) (cast (unsigned long) (fmod v 1.0e9)))
           (while (>= (ref spec tv_nsec) 1000000000)
             (-= (ref spec tv_nsec) 1000000000)
             (+= (ref spec tv_sec) 1)))])
       (set! (ref rem tv_sec) 0 (ref rem tv_nsec) 0)
       (while (< (Scm_NanoSleep (& spec) (& rem)) 0)
         (unless (== errno EINTR)
           (Scm_SysError "nanosleep failed"))
         (SCM_SIGCHECK vm)
         (when no-retry (break))
         (set! spec rem)
         (set! (ref rem tv_sec) 0 (ref rem tv_nsec) 0))
       (if (and (== (ref rem tv_sec) 0) (== (ref rem tv_nsec) 0))
         (return '#f)
         (return (Scm_MakeTime '#f (ref rem tv_sec) (ref rem tv_nsec))))))
   (initcode (Scm_AddFeature "gauche.sys.nanosleep" NULL))
   ) ; defined(HAVE_NANOSLEEP)||defined(GAUCHE_WINDOWS)
 )

(define-cproc sys-unlink (pathname::<const-cstring>)
  (let* ([r::int])
    (.if "defined(GAUCHE_WINDOWS)"
         ;; Windows doesn't allow unlinking a read-only file.  We don't check
         ;; an error here, since the error will be caught by next unlink call.
         (when (not (access pathname F_OK))
           (chmod pathname #o600)))
    (SCM_SYSCALL r (unlink pathname))
    (if (< r 0)
      (begin
        (unless (== errno ENOENT)
          (Scm_SysError "unlink failed on %s" pathname))
        (return '#f))
      (return '#t))))

(define-cproc sys-isatty (port_or_fd) ::<boolean>
  (let* ([fd::int (Scm_GetPortFd port_or_fd FALSE)])
    (return (and (>= fd 0) (isatty fd)))))

(define-cproc sys-ttyname (port_or_fd) ::<const-cstring>?
  (let* ([fd::int (Scm_GetPortFd port_or_fd FALSE)])
    (return (?: (< fd 0) NULL (ttyname fd)))))

(define-cproc sys-truncate (path::<const-cstring> length::<integer>)
  ::<void>
  (let* ([r::int])
    (SCM_SYSCALL r (truncate path (Scm_IntegerToOffset length)))
    (when (< r 0) (Scm_SysError "truncate failed on %s" path))))

(define-cproc sys-ftruncate (port_or_fd length::<integer>) ::<void>
  (let* ([r::int] [fd::int (Scm_GetPortFd port_or_fd TRUE)])
    (SCM_SYSCALL r (ftruncate fd (Scm_IntegerToOffset length)))
    (when (< r 0) (Scm_SysError "ftruncate failed on %S" port_or_fd))))

(inline-stub
 ;; NB. Linux needs _XOPEN_SOURCE defined before unistd.h to get crypt()
 ;; prototype.  However, it screws up something else.  Just for now I
 ;; cast the return value of crypt() to avoid it...such a kludge...
 (when "defined(HAVE_CRYPT)"
   (define-cproc sys-crypt (key::<const-cstring> salt::<const-cstring>)
     ::<const-cstring> (return (cast (const char *) (crypt key salt))))
   (initcode (Scm_AddFeature "gauche.sys.crypt" NULL))
   )
 )

(inline-stub
 "#ifndef HOSTNAMELEN
#define HOSTNAMELEN 1024
#endif")

(define-cproc sys-gethostname () ::<const-cstring>
  (.if "defined HAVE_GETHOSTNAME"
       (let* ([buf::(.array char [HOSTNAMELEN])] [r::int])
         (SCM_SYSCALL r (gethostname buf HOSTNAMELEN))
         (when (< r 0) (Scm_SysError "gethostname failed"))
         (return buf))
       ;; TODO: find better alternative
       (return "localhost")))

(define-cproc sys-getdomainname () ::<const-cstring>
  (.if "defined HAVE_GETDOMAINNAME"
       (let* ([buf::(.array char [HOSTNAMELEN])] [r::int])
         (SCM_SYSCALL r (getdomainname buf HOSTNAMELEN))
         (when (< r 0) (Scm_SysError "getdomainame failed"))
         (return buf))
       ;; TODO: find better alternative
       (return "local")))

;; not supported yet:
;;  fpathconf lseek pathconf read sysconf write

;;---------------------------------------------------------------------
;; symbolic link

(inline-stub
 (when "defined(HAVE_SYMLINK)"
   (define-cproc sys-symlink (existing::<const-cstring>
                              newpath::<const-cstring>)
     ::<void>
     (let* ([r::int])
       (SCM_SYSCALL r (symlink existing newpath))
       (when (< r 0)
         (Scm_SysError "symlink from %s to %s failed" newpath existing))))
   (initcode (Scm_AddFeature "gauche.sys.symlink" NULL))
   )

 (when "defined(HAVE_READLINK)"
   (define-cproc sys-readlink (path::<const-cstring>)
     (let* ([buf::(.array char [1024])] ; TODO: needs to be configured
            [n::int])
       (SCM_SYSCALL n (readlink path buf 1024))
       (when (< n 0) (Scm_SysError "readlink failed on %s" path))
       (when (== n 1024) (Scm_Error "readlink result too long on %s" path))
       (return (Scm_MakeString buf n -1 SCM_STRING_COPYING))))
   (initcode (Scm_AddFeature "gauche.sys.readlink" NULL))
   )
 )

;;---------------------------------------------------------------------
;; select

(inline-stub
 (define-type <sys-fdset> "ScmSysFdset*")

 (when "defined(HAVE_SELECT)"
   ;; NB: On Windows, FD_SETSIZE merely indicates the maximum # of socket
   ;; descriptors fd_set can contain, and unrelated to the actual value
   ;; of the descriptor.  This check is thus only valid on unixen.
   (define-cise-stmt check-fd-range
     [(_ fd)
      (let1 fd_ (gensym)
        `(.if "!defined(GAUCHE_WINDOWS)"
              (let* ((,fd_ :: int ,fd))
                (when (or (< ,fd_ 0) (>= ,fd_ FD_SETSIZE))
                  (Scm_Error "File descriptor value is out of range: %d \
                         (must be between 0 and %d, inclusive)"
                             ,fd_ (- FD_SETSIZE 1))))))])

   (define-cproc sys-fdset-ref (fdset::<sys-fdset> pf) ::<boolean>
     (setter sys-fdset-set!)
     (let* ([fd::int (Scm_GetPortFd pf FALSE)])
       (if (< fd 0)
         (return TRUE)
         (begin (check-fd-range fd)
                (return (FD_ISSET fd (& (-> fdset fdset))))))))

   (define-cproc sys-fdset-set! (fdset::<sys-fdset> pf flag::<boolean>) ::<void>
     (let* ([fd::int (Scm_GetPortFd pf FALSE)])
       (when (>= fd 0)
         (check-fd-range fd)
         (cond [flag (FD_SET fd (& (-> fdset fdset)))
                     (when (< (-> fdset maxfd) fd) (set! (-> fdset maxfd) fd))]
               [else (FD_CLR fd (& (-> fdset fdset)))
                     (when (== (-> fdset maxfd) fd)
                       (let* ([i::int (- (-> fdset maxfd) 1)])
                         (for [() (>= i 0) (post-- i)]
                              (when (FD_ISSET i (& (-> fdset fdset))) (break)))
                         (set! (-> fdset maxfd) i)))]))))

   (define-cproc sys-fdset-max-fd (fdset::<sys-fdset>) ::<int>
     (return (-> fdset maxfd)))

   (define-cproc sys-fdset-clear! (fdset::<sys-fdset>)
     (FD_ZERO (& (-> fdset fdset)))
     (set! (-> fdset maxfd) -1)
     (return (SCM_OBJ fdset)))

   (define-cproc sys-fdset-copy! (dst::<sys-fdset> src::<sys-fdset>)
     (set! (-> dst fdset) (-> src fdset)
           (-> dst maxfd) (-> src maxfd))
     (return (SCM_OBJ dst)))

   (define-cproc sys-select (rfds wfds efds :optional (timeout #f))
     Scm_SysSelect)

   (define-cproc sys-select! (rfds wfds efds :optional (timeout #f))
     Scm_SysSelectX)

   (initcode (Scm_AddFeature "gauche.sys.select" NULL))
   ) ;; when defined(HAVE_SELECT)
 )

;;---------------------------------------------------------------------
;; miscellaneous

(inline-stub
 (define-cproc sys-available-processors () ::<int>
   Scm_AvailableProcessors))

;;;
;;; Windows-specific utility
;;;

(inline-stub
 (when "defined(GAUCHE_WINDOWS)"
   ;; Windows HANDLE wrapper
   ;; We use foreign pointer for HANDLE.  If the type of handle is known at
   ;; the creation time, it is attached as the foreign pointer attribute
   ;; with the key 'handle-type.

   (define-cfn handle-cleanup (h) ::void :static
     (CloseHandle (SCM_FOREIGN_POINTER_REF HANDLE h)))

   (define-cfn handle-print (h p::ScmPort* c::ScmWriteContext*) ::void :static
     (let* ([type (Scm_ForeignPointerAttrGet (SCM_FOREIGN_POINTER h)
                                             'handle-type '#f)])
       (cond
        [(SCM_EQ type 'process)
         (Scm_Printf p "#<win:handle process %d @%p>" (Scm_WinProcessPID h) h)]
        [else
         (Scm_Printf p "#<win:handle @%p>" h)])))

   (declcode "static ScmClass *WinHandleClass = NULL;")
   (initcode (= WinHandleClass (Scm_MakeForeignPointerClass
                                (Scm_CurrentModule)
                                "<win:handle>" handle-print handle-cleanup
                                SCM_FOREIGN_POINTER_KEEP_IDENTITY)))

   (define-cfn Scm_MakeWinHandle (wh::HANDLE type)
     (let* ([h (Scm_MakeForeignPointer WinHandleClass wh)])
       (unless (SCM_FALSEP type)
         (Scm_ForeignPointerAttrSet (SCM_FOREIGN_POINTER h) 'handle-type type))
       (return h)))

   (define-cfn Scm_WinHandleP (obj type) ::int
     (unless (SCM_XTYPEP obj WinHandleClass) (return FALSE))
     (return
      (or (SCM_FALSEP type)
          (SCM_EQ type (Scm_ForeignPointerAttrGet (SCM_FOREIGN_POINTER obj)
                                                  'handle-type SCM_FALSE)))))

   (define-cfn Scm_WinHandle (h type) ::HANDLE
     (unless (Scm_WinHandleP h type) (SCM_TYPE_ERROR h "<win:handle>"))
     (return (SCM_FOREIGN_POINTER_REF HANDLE h)))

   ;; windows process

   (define-cfn Scm_MakeWinProcess (h::HANDLE)
     (return (Scm_MakeWinHandle h 'process)))

   (define-cfn Scm_WinProcessP (obj) ::int
     (return (Scm_WinHandleP obj 'process)))

   (define-cfn Scm_WinProcess (obj) ::HANDLE
     (Scm_WinHandle obj 'process))

   (define-cproc sys-win-process? (obj) ::<boolean> Scm_WinProcessP)
   (define-cproc sys-win-process-pid (obj) ::<int> Scm_WinProcessPID)

   ;; windows handle

   (define-cproc sys-get-osfhandle (port-or-fd)
     (let* ([fd::int (Scm_GetPortFd port-or-fd TRUE)]
            [h::HANDLE (cast HANDLE (_get_osfhandle fd))])
       (when (== h INVALID_HANDLE_VALUE) (Scm_SysError "get_osfhandle failed"))
       (return (Scm_MakeWinHandle h '#f))))

   ) ;; GAUCHE_WINDOWS
 )

;; This is originally a part of shell-escape-string in gauche.process,
;; but the lower level function Scm_Exec() requires this to build
;; windows command line string from given argument list.   It would be
;; clumsy to implement this in C, so we provide this here to be shared
;; by Scm_Exec() and shell-escape-string.
;; NB:  There seems no reliable way to escape command line arguments on
;; windows, since the parsing is up to every application.  However,
;; the standard C runtime seems to obey that (a) whitespaces can be
;; embedded if the argument is surrounded by double quotes, and (b)
;; within double-quotes, consecutive two double-quotes are replaced
;; for one double-quote.
;; NB: The second condition would be clearer if we use string-index, but
;; it is in srfi-13.  string-split, otoh, is built-in.  The overhead of
;; using string-split here would be negligible.
(define (%sys-escape-windows-command-line s)
  (cond [(not (string? s))
         (%sys-escape-windows-command-line (write-to-string s))]
        [(equal? s "") "\"\""]
        [(null? (cdr (string-split s #[\s\"]))) s]
        [else (string-append "\"" (regexp-replace-all #/\"/ s "\"\"") "\"")]))

