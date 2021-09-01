;;;
;;; threads.scm - thread related procedures.  to be autoloaded
;;;
;;;   Copyright (c) 2000-2021  Shiro Kawai  <shiro@acm.org>
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

(define-module gauche.threads
  (use gauche.record)
  (export gauche-thread-type
          current-thread                ;re-exporting the builtin

          thread? make-thread thread-name thread-specific-set! thread-specific
          thread-state thread-start! thread-try-start!
          thread-yield! thread-sleep!
          thread-join! thread-terminate! thread-stop! thread-cont!

          mutex? make-mutex mutex-name mutex-state
          mutex-specific-set! mutex-specific
          with-locking-mutex mutex-lock! mutex-unlock!
          mutex-locker mutex-unlocker

          condition-variable? make-condition-variable condition-variable-name
          condition-variable-specific condition-variable-specific-set!
          condition-variable-signal! condition-variable-broadcast!

          join-timeout-exception? abandoned-mutex-exception?
          terminated-thread-exception? uncaught-exception?
          uncaught-exception-reason

          atom atom? atom-ref atomic atomic-update!

          make-semaphore semaphore? semaphore-acquire! semaphore-release!
          make-latch latch? latch-dec! latch-await
          make-barrier barrier? barrier-reset! barrier-await barrier-broken?
          ))
(select-module gauche.threads)

(inline-stub
 (declcode
  (.include "threads.h"))

 (declare-cfn Scm_Init_mutex (mod::ScmModule*) ::void)
 (declare-cfn Scm_Init_threads (mod::ScmModule*) ::void)

 (initcode
  (Scm_Init_threads (Scm_CurrentModule))
  (Scm_Init_mutex (Scm_CurrentModule))))

;;===============================================================
;; System query
;;

;; TODO: for the consistency of ./configure --enable-threads,
;; it should return 'pthreads' instead of 'pthread' on pthreads platform.
(inline-stub
 (define-cproc gauche-thread-type ()
   (.cond [(defined GAUCHE_USE_PTHREADS) (return 'pthread)]
          [(defined GAUCHE_USE_WTHREADS) (return 'win32)]
          [else (return 'none)])))

;;===============================================================
;; Thread
;;

(define (thread? obj) (is-a? obj <thread>))

(define (thread-name thread)
  (assume-type thread <thread>)
  (slot-ref thread 'name))

(define (thread-specific-set! thread value)
  (assume-type thread <thread>)
  (slot-set! thread 'specific value))

(define thread-specific
  (getter-with-setter
   (^[thread]
     (assume-type thread <thread>)
     (slot-ref thread 'specific))
   thread-specific-set!))

(define (make-thread thunk :optional (name #f))
  (rlet1 t (%make-thread thunk name)
    ((with-module gauche.internal %vm-custom-error-reporter-set!) t (^e #f))))

(inline-stub
 (define-cproc thread-state (vm::<thread>)
   (case (-> vm state)
     [(SCM_VM_NEW)       (return 'new)]
     [(SCM_VM_RUNNABLE)  (return 'runnable)]
     [(SCM_VM_STOPPED)   (return 'stopped)]
     [(SCM_VM_TERMINATED)(return 'terminated)]
     [else (Scm_Error "[internal] thread state has invalid value: %d"
                      (-> vm state))
           (return SCM_UNDEFINED)]))   ;dummy

 (define-cproc %make-thread (thunk::<procedure> name) Scm_MakeThread)

 (define-cproc thread-start! (vm::<thread>)
   (return (Scm_ThreadStart vm 0)))

 (define-cproc thread-try-start! (vm::<thread>)
   (return (Scm_ThreadStart vm SCM_THREAD_START_TRYSTART)))

 (define-cproc thread-yield! () ::<void> Scm_YieldCPU)

 (define-cproc thread-sleep! (timeout) Scm_ThreadSleep)

 (define-cproc thread-join! (vm::<thread> :optional (timeout #f) timeout-val)
   Scm_ThreadJoin)

 (define-cproc thread-terminate! (vm::<thread> :key (force #f))
   (let* ([flags::u_long 0])
     (when (not (SCM_FALSEP force))
       (set! flags SCM_THREAD_TERMINATE_FORCIBLE))
     (return (Scm_ThreadTerminate vm flags))))

 (define-cproc thread-stop! (target::<thread>
                             :optional (timeout #f) (timeout-val #f))
   Scm_ThreadStop)

 (define-cproc thread-cont! (target::<thread>) Scm_ThreadCont)
 )

;;===============================================================
;; Mutex
;;

(define (mutex? obj)  (is-a? obj <mutex>))

(define (mutex-name mutex)
  (assume-type mutex <mutex>)
  (slot-ref mutex 'name))

(define (mutex-state mutex)
  (assume-type mutex <mutex>)
  (slot-ref mutex 'state))

(define (mutex-specific-set! mutex value)
  (assume-type mutex <mutex>)
  (slot-set! mutex 'specific value))

(define mutex-specific
  (getter-with-setter
   (^[mutex]
     (assume-type mutex <mutex>)
     (slot-ref mutex 'specific))
   mutex-specific-set!))

(define-inline (with-locking-mutex mutex thunk)
  (dynamic-wind
      (mutex-locker mutex)
      thunk
      (mutex-unlocker mutex)))

(inline-stub
 (define-cproc make-mutex (:optional (name #f)) Scm_MakeMutex)

 (define-cise-stmt with-mutex
   [(_ mutex . form)
    `(begin
       (cast void (SCM_INTERNAL_MUTEX_LOCK ,mutex))
       ,@form
       (cast void (SCM_INTERNAL_MUTEX_UNLOCK ,mutex)))])

 (define-cproc mutex-lock! (mutex::<mutex> :optional (timeout #f) thread)
   (let* ([owner::ScmVM* NULL])
     (cond [(SCM_VMP thread) (set! owner (SCM_VM thread))]
           [(SCM_UNBOUNDP thread) (set! owner (Scm_VM))]
           [(not (SCM_FALSEP thread)) (SCM_TYPE_ERROR thread "thread or #f")])
     (return (Scm_MutexLock mutex timeout owner))))

 (define-cproc mutex-unlock! (mutex::<mutex> :optional (cv #f) (timeout #f))
   (let* ([cond::ScmConditionVariable* NULL])
     (cond [(SCM_CONDITION_VARIABLE_P cv) (set! cond (SCM_CONDITION_VARIABLE cv))]
           [(not (SCM_FALSEP cv)) (SCM_TYPE_ERROR cv "condition variale or #f")])
     (return (Scm_MutexUnlock mutex cond timeout))))

 (define-cproc mutex-locker (mutex::<mutex>) Scm_MutexLocker)
 (define-cproc mutex-unlocker (mutex::<mutex>) Scm_MutexUnlocker)
 )

;;===============================================================
;; Condition variable
;;

(define (condition-variable? obj) (is-a? obj <condition-variable>))

(define (condition-variable-name cv)
  (assume-type cv <condition-variable>)
  (slot-ref cv 'name))

(define (condition-variable-specific-set! cv value)
  (assume-type cv <condition-variable>)
  (slot-set! cv 'specific value))

(define condition-variable-specific
  (getter-with-setter
   (lambda (cv)
     (assume-type cv <condition-variable>)
     (slot-ref cv 'specific))
   condition-variable-specific-set!))

(inline-stub
  (define-cproc make-condition-variable (:optional (name #f))
    Scm_MakeConditionVariable)

  (define-cproc condition-variable-signal! (cv::<condition-variable>)
    Scm_ConditionVariableSignal)

  (define-cproc condition-variable-broadcast! (cv::<condition-variable>)
    Scm_ConditionVariableBroadcast)
  )

;;===============================================================
;; Exceptions
;;

(define (join-timeout-exception? obj)
  (is-a? obj <join-timeout-exception>))

(define (abandoned-mutex-exception? obj)
  (is-a? obj <abandoned-mutex-exception>))

(define (terminated-thread-exception? obj)
  (is-a? obj <terminated-thread-exception>))

(define (uncaught-exception? obj)
  (is-a? obj <uncaught-exception>))

(define (uncaught-exception-reason exc)
  (assume-type exc <uncaught-exception>)
  (slot-ref exc 'reason))

;;===============================================================
;; Atom
;;

(define-record-type <atom> %make-atom atom?
  (applier atom-applier)
  (updater atom-updater))

(define (atom . vals)
  (define m (make-mutex))
  (let-syntax ([with-lock
                (syntax-rules ()
                  [(_ timeout timeout-val timeout-vals . form)
                   (unwind-protect
                       (if (mutex-lock! m timeout)
                         (begin . form)
                         (if (null? timeout-vals)
                           timeout-val
                           (apply values (cons timeout-val timeout-vals))))
                     (when (eq? (mutex-state m) (current-thread))
                       (mutex-unlock! m)))])])
    ;; TODO: we may expand special cases like vals is 1 to 3 elements long,
    ;; avoiding creation of lists every time updater is called.
    (%make-atom
     (^[proc timeout timeout-val timeout-vals]
       (with-lock timeout timeout-val timeout-vals (apply proc vals)))
     (^[proc timeout timeout-val timeout-vals]
       (with-lock timeout timeout-val timeout-vals
                  (receive newvals (apply proc vals)
                    (unless (>= (length newvals) (length vals))
                      (errorf "atomic-update!: procedure returned too few \
                                   number of values (~a, while ~a expected)"
                              (length newvals) (length vals)))
                    (set! vals (take newvals (length vals)))
                    (apply values newvals)))))))

(define (atomic atom proc :optional (timeout #f) (timeout-val #f) :rest vals)
  (unless (atom? atom) (error "atom required, but got:" atom))
  ((atom-applier atom) proc timeout timeout-val vals))

(define (atomic-update! atom proc :optional (timeout #f) (timeout-val #f) :rest vals)
  (unless (atom? atom) (error "atom required, but got:" atom))
  ((atom-updater atom) proc timeout timeout-val vals))

(define (atom-ref atom :optional (index 0) (timeout #f) (timeout-val #f))
  (unless (atom? atom) (error "atom required, but got:" atom))
  ((atom-applier atom) (^ xs (list-ref xs index)) timeout timeout-val '()))

;;===============================================================
;; Semaphore
;;

(define-record-type <semaphore> %make-semaphore semaphore?
  name                                  ; for information only
  (count)
  mutex
  cv)

(define (make-semaphore :optional (init-value 0) (name #f))
  (%make-semaphore name init-value
                   (make-mutex)
                   (make-condition-variable)))

(define-method write-object ((s <semaphore>) port)
  (format port "#<semaphore ~d" (~ s'count))
  (if-let1 name (~ s'name)
    (format port " ~s>" name)
    (format port ">")))

(define (semaphore-acquire! sem :optional (timeout #f) (timeout-val #f))
  (assume-type sem <semaphore>)
  (let loop ()
    (mutex-lock! (~ sem'mutex))
    (cond [(> (~ sem'count) 0)
           (dec! (~ sem'count))
           (mutex-unlock! (~ sem'mutex))
           #t]
          [(mutex-unlock! (~ sem'mutex) (~ sem'cv) timeout) (loop)]
          [else timeout-val])))         ;timeout

;; We allow to release more than one tokens at once.
;; 'all means all
(define (semaphore-release! sem :optional (count 1))
  (assume-type sem <semaphore>)
  (assume (and (exact-integer? count) (> count 0)))
  (mutex-lock! (~ sem'mutex))
  (inc! (~ sem'count) count)
  (when (> (~ sem'count) 0)
    (if (= count 1)
      (condition-variable-signal! (~ sem'cv))
      (condition-variable-broadcast! (~ sem'cv))))
  (mutex-unlock! (~ sem'mutex)))

;;===============================================================
;; Latch
;;

(define-record-type <latch> %make-latch latch?
  name                                  ; for information only
  (count)
  mutex
  cv)

(define (make-latch initial-count :optional (name #f))
  (assume (and (exact-integer? initial-count)
               (positive? initial-count)))
  (%make-latch name initial-count (make-mutex) (make-condition-variable)))

(define-method write-object ((l <latch>) port)
  (format port "#<latch ~d" (~ l'count))
  (if-let1 name (~ l'name)
    (format port " ~s>" name)
    (format port ">")))

(define (latch-dec! latch :optional (n 1))
  (assume-type latch <latch>)
  (assume (exact-integer? n))
  (mutex-lock! (~ latch'mutex))
  (when (> (~ latch'count) 0)
    (dec! (~ latch'count) n)
    (when (<= (~ latch'count) 0)
      (condition-variable-broadcast! (~ latch'cv))))
  (let1 n (~ latch'count)
    (mutex-unlock! (~ latch'mutex))
    n))

(define (latch-await latch :optional (timeout #f) (timeout-val #f))
  (assume-type latch <latch>)
  (let loop ()
    (mutex-lock! (~ latch'mutex))
    (cond [(<= (~ latch'count) 0)
           (mutex-unlock! (~ latch'mutex))
           #t]
          [(mutex-unlock! (~ latch'mutex) (~ latch'cv) timeout) (loop)]
          [else timeout-val])))         ;timeout

;;===============================================================
;; Barrier
;;

(define-record-type <barrier> %make-barrier barrier?
  name                                  ; for information only
  threshold
  (count)
  (generation)
  (broken)
  action
  mutex
  cv)

(define (make-barrier threshold :optional (action #f) (name #f))
  (%make-barrier name threshold 0 0 #f action
                 (make-mutex) (make-condition-variable)))

(define-method write-object ((b <barrier>) port)
  (format port "#<barrier ~d/~d" (~ b'count) (~ b'threshold))
  (if-let1 name (~ b'name)
    (format port " ~s>" name)
    (format port ">")))

(define (barrier-reset! barrier)
  (assume-type barrier <barrier>)
  (mutex-lock! (~ barrier'mutex))
  (set! (~ barrier'broken) #f)
  (inc! (~ barrier'generation))
  (set! (~ barrier'count) 0)
  (condition-variable-broadcast! (~ barrier'cv))
  (mutex-unlock! (~ barrier'mutex)))

(define (barrier-broken? barrier)
  (assume-type barrier <barrier>)
  (~ barrier'broken))

(define (barrier-await barrier :optional (timeout #f) (timeout-val #f))
  (assume-type barrier <barrier>)
  (let loop ((gen #f)
             (place #f))
    (mutex-lock! (~ barrier'mutex))
    (unless gen                         ;first time
      (inc! (~ barrier'count)))
    (let ([gen (or gen (~ barrier'generation))]
          [place (or place (- (~ barrier'threshold) (~ barrier'count)))]
          [action-exception #f])
      (cond [(~ barrier'broken)         ;already broken
             (mutex-unlock! (~ barrier'mutex))
             timeout-val]
            [(< gen (~ barrier'generation))
             (mutex-unlock! (~ barrier'mutex))
             place]
            [(= place 0) ;I'm the last
             (when (~ barrier'action)
               ;; We run the action without holding the lock.  If there're more
               ;; threads than supposed and came after this, it gets the
               ;; same generation and negative place (so it won't run the
               ;; action) and wait on the same cv.
               (mutex-unlock! (~ barrier'mutex))
               (guard (e [else (set! (~ barrier'broken) #t)
                               (set! action-exception e)])
                 ((~ barrier'action)))
               (mutex-lock! (~ barrier'mutex)))
             (set! (~ barrier'count) 0)
             (inc! (~ barrier'generation))
             (condition-variable-broadcast! (~ barrier'cv))
             (mutex-unlock! (~ barrier'mutex))
             (when action-exception
               (raise action-exception))
             place]
            [(mutex-unlock! (~ barrier'mutex) (~ barrier'cv) timeout)
             (loop gen place)]
            [else                       ;timeout -> broken
             (mutex-lock! (~ barrier'mutex))
             (set! (~ barrier'broken) #t)
             (condition-variable-broadcast! (~ barrier'cv))
             (mutex-unlock! (~ barrier'mutex))
             timeout-val]))))
