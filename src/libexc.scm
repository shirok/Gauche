;;;
;;; libexc.scm - errors and exceptions
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

(select-module gauche)
(inline-stub
 (declcode (.include <gauche/class.h>
                     <gauche/exception.h>)))

(define <exception> <condition>) ;; backward compatibility

;;;
;;; Core procedures
;;;

(select-module gauche)
;; NB: rewind-before keyword arg is EXPERIMENTAL.
(define-cproc with-error-handler
  (handler thunk :key (rewind-before::<boolean> #f))
  (unless (SCM_PROCEDURE_TAKE_NARG_P handler 1)
    (Scm_Error "error handler must take at least 1 argument, but got %S"
               handler))
  (unless (SCM_PROCEDURE_THUNK_P thunk)
    (Scm_Error "thunk required, but got %S" thunk))
  (if rewind-before
    (result (Scm_VMWithGuardHandler handler thunk))
    (result (Scm_VMWithErrorHandler handler thunk))))

(define-cproc report-error (exception :optional port)
  ;; TRANSIENT: change this to Scm_ReportError when switching API to 0.95.
  Scm_ReportError2)

(define-cproc condition-type-name (c) Scm_ConditionTypeName)

(define (print-default-error-heading exc out)
  (guard (e [else (display "*** ERROR:" out)
                  (display exc out)
                  (display "\n" out)])
    (if (not (condition? exc))
      (format out "*** ERROR: unhandled exception: ~s\n" exc)
      (receive (mixins mains)
          (if (is-a? exc <compound-condition>)
            (partition (cut is-a? <> <mixin-condition>) (~ exc'%conditions))
            (values '() (list exc)))
        (let1 name (condition-type-name exc)
          (if (condition-has-type? exc <message-condition>)
            (format out "*** ~a: ~a\n" name (~ exc'message))
            (format out "*** ~a\n" name)))
        (for-each (cut report-mixin-condition <> out) mixins)))))

;;;
;;; Srfi-18 primitives
;;;

(select-module gauche)
(define-cproc current-exception-handler ()
  (result (-> (Scm_VM) exceptionHandler)))

(define-cproc with-exception-handler (handler thunk)
  Scm_VMWithExceptionHandler)

;; The optional arg is to support r7rs semantics
(select-module gauche.internal)
(define-cproc %raise (exception :optional (non-continuable? #f))
  (let* ([flags::u_long
          (?: (SCM_FALSEP non-continuable?) 0 SCM_RAISE_NON_CONTINUABLE)])
    (result (Scm_Raise2 exception flags))))

;; srfi-18 raise
(define-in-module gauche (raise c) (%raise c))

;;;
;;; Srfi-35
;;;

(select-module gauche)

(define-cproc condition? (obj) ::<boolean> SCM_CONDITIONP)

(define-cproc condition-has-type? (c k) ::<boolean> Scm_ConditionHasType)

(define-cproc make-compound-condition (:rest conditions)
  Scm_MakeCompoundCondition)

;;;
;;; Thread exception classes
;;;

;; TODO - set metaclass of those exceptions to <condition-meta>

(select-module gauche)
(inline-stub
 (define-cfn thread_exception_allocate (klass::ScmClass* initargs) :static
   (return (Scm_MakeThreadException klass NULL)))

 (define-cfn thread_exception_print (obj port::ScmPort* ctx::ScmWriteContext*)
   ::void :static
   (let* ([k::ScmClass* (SCM_CLASS_OF obj)]
          [exc::ScmThreadException* (SCM_THREAD_EXCEPTION obj)]
          [cname (Scm__InternalClassName k)])
     (if (SCM_UNDEFINEDP (-> exc data))
       (Scm_Printf port "#<%A %S>" cname (SCM_OBJ_SAFE (-> exc thread)))
       (Scm_Printf port "#<%A %S %S>" cname (SCM_OBJ_SAFE (-> exc thread))
                   (-> exc data)))))

 (define-cfn uncaught_exception_print (obj port::ScmPort* ctx::ScmWriteContext*)
   ::void :static
   (let* ([exc::ScmThreadException* (SCM_THREAD_EXCEPTION obj)])
     (Scm_Printf port "#<uncaught-exception in thread %S: %S>"
                 (SCM_OBJ_SAFE (-> exc thread)) (-> exc data))))

 (define-cfn terminated_thread_print (obj port::ScmPort* ctx::ScmWriteContext*)
   ::void :static
   (let* ([exc::ScmThreadException* (SCM_THREAD_EXCEPTION obj)])
     (Scm_Printf port "#<terminated-thread-exception: %S terminated by %S>"
                 (SCM_OBJ_SAFE (-> exc thread)) (-> exc data))))

 "static ScmClass *thread_exception_cpa[] = {
   SCM_CLASS_STATIC_PTR(Scm_ThreadExceptionClass),
   SCM_CLASS_STATIC_PTR(Scm_ConditionClass),
   SCM_CLASS_STATIC_PTR(Scm_TopClass),
   NULL
 };"

 (define-cclass <thread-exception>
   "ScmThreadException*" "Scm_ThreadExceptionClass"
   ("Scm_ConditionClass")
   ((thread :type <thread>))
   (allocator (c "thread_exception_allocate"))
   (printer   (c "thread_exception_print")))

 (define-type <join-timeout-exception> "ScmThreadException*" #f
   "SCM_THREAD_EXCEPTION_P" "SCM_THREAD_EXCEPTION")

 (define-cclass <join-timeout-exception>
   "ScmThreadException*" "Scm_JoinTimeoutExceptionClass"
   (c "thread_exception_cpa")
   ()
   (allocator (c "thread_exception_allocate"))
   (printer   (c "thread_exception_print")))

 (define-type <abandoned-mutex-exception> "ScmThreadException*" #f
   "SCM_THREAD_EXCEPTION_P" "SCM_THREAD_EXCEPTION")

 (define-cclass <abandoned-mutex-exception>
   "ScmThreadException*" "Scm_AbandonedMutexExceptionClass"
   (c "thread_exception_cpa")
   ((mutex  :c-name "data"))
   (allocator (c "thread_exception_allocate"))
   (printer   (c "thread_exception_print")))

 (define-type <terminated-thread-exception> "ScmThreadException*" #f
   "SCM_THREAD_EXCEPTION_P" "SCM_THREAD_EXCEPTION")

 (define-cclass <terminated-thread-exception>
   "ScmThreadException*" "Scm_TerminatedThreadExceptionClass"
   (c "thread_exception_cpa")
   ((terminator :c-name "data"))
   (allocator (c "thread_exception_allocate"))
   (printer   (c "terminated_thread_print")))

 (define-type <uncaught-exception> "ScmThreadException*" #f
   "SCM_THREAD_EXCEPTION_P" "SCM_THREAD_EXCEPTION")

 (define-cclass <uncaught-exception>
   "ScmThreadException*" "Scm_UncaughtExceptionClass"
   (c "thread_exception_cpa")
   ((reason :c-name data))
   (allocator (c "thread_exception_allocate"))
   (printer   (c "uncaught_exception_print")))
 )

;;;
;;; Mixin classes
;;;

;; TODO - set metaclass of those mixins to <condition-meta>

(select-module gauche)
(inline-stub
 "static ScmClass *mixin_condition_cpa[] = {
   SCM_CLASS_STATIC_PTR(Scm_MixinConditionClass),
   SCM_CLASS_STATIC_PTR(Scm_ConditionClass),
   SCM_CLASS_STATIC_PTR(Scm_TopClass),
   NULL
  };"

 (define-cclass <mixin-condition>
   "ScmCondition*" "Scm_MixinConditionClass"
   (c "mixin_condition_cpa+1")
   () ())
 
 (define-cfn load-condition-mixin-allocate (klass::ScmClass* initargs) :static
   (let* ([c::ScmLoadConditionMixin* (SCM_NEW_INSTANCE ScmLoadConditionMixin klass)])
     (set! (-> c history) SCM_FALSE)
     (set! (-> c port) SCM_FALSE)
     (return (SCM_OBJ c))))
 
 (define-cclass <load-condition-mixin>
   "ScmLoadConditionMixin*" "Scm_LoadConditionMixinClass"
   (c "mixin_condition_cpa")
   ((history)
    (port))
   (allocator (c "load_condition_mixin_allocate")))

 (define-cfn compile-error-mixin-allocate (klass::ScmClass* initargs) :static
   (let* ([c::ScmCompileErrorMixin* (SCM_NEW_INSTANCE ScmCompileErrorMixin klass)])
     (set! (-> c expr) SCM_FALSE)
     (return (SCM_OBJ c))))
 
 (define-cclass <compile-error-mixin>
   "ScmCompileErrorMixin*" "Scm_CompileErrorMixinClass"
   (c "mixin_condition_cpa")
   ((expr))
   (allocator (c "compile_error_mixin_allocate")))

 ;; Filename errors are defined in srfi-36.  Internally most of those filename
 ;; errors occur as <system-error> first; we compound the following conditions
 ;; as needed.  In C name we use "Mixin", but in Scheme we follow srfi-36
 ;; names and drop "-mixin".
 ;; In C-level all filename mixin classes share one struct definition, and
 ;; one allocator.
 (define-cfn filename-error-mixin-allocate (klass::ScmClass* initargs) :static
   (let* ([c::ScmFilenameErrorMixin* (SCM_NEW_INSTANCE ScmFilenameErrorMixin klass)])
     (set! (-> c filename) SCM_FALSE)
     (return (SCM_OBJ c))))

 "static ScmClass *filename_condition_cpa[] = {
   SCM_CLASS_STATIC_PTR(Scm_FileProtectionErrorMixinClass),
   SCM_CLASS_STATIC_PTR(Scm_FilenameErrorMixinClass),
   SCM_CLASS_STATIC_PTR(Scm_MixinConditionClass),
   SCM_CLASS_STATIC_PTR(Scm_ConditionClass),
   SCM_CLASS_STATIC_PTR(Scm_TopClass),
   NULL
  };"

 (define-type <io-filename-error> "ScmFilenameErrorMixin*" #f
   "SCM_FILENAME_ERROR_MIXIN_P" "SCM_FILENAME_ERROR_MIXIN")
 (define-cclass <io-filename-error>
   "ScmFilenameErrorMixin*" "Scm_FilenameErrorMixinClass"
   (c "filename_condition_cpa+2")
   ((filename))
   (allocator (c "filename_error_mixin_allocate")))

 (define-type <io-malformed-filename-error> "ScmFilenameErrorMixin*" #f
   "SCM_FILENAME_ERROR_MIXIN_P" "SCM_FILENAME_ERROR_MIXIN")
 (define-cclass <io-malformed-filename-error>
   "ScmFilenameErrorMixin*" "Scm_MalformedFilenameErrorClass"
   (c "filename_condition_cpa+1")
   ((filename))
   (allocator (c "filename_error_mixin_allocate")))

 (define-type <io-file-protection-error> "ScmFilenameErrorMixin*" #f
   "SCM_FILENAME_ERROR_MIXIN_P" "SCM_FILENAME_ERROR_MIXIN")
 (define-cclass <io-file-protection-error>
   "ScmFilenameErrorMixin*" "Scm_FileProtectionErrorMixinClass"
   (c "filename_condition_cpa+1")
   ((filename))
   (allocator (c "filename_error_mixin_allocate")))

 (define-type <io-file-is-read-only-error> "ScmFilenameErrorMixin*" #f
   "SCM_FILENAME_ERROR_MIXIN_P" "SCM_FILENAME_ERROR_MIXIN")
 (define-cclass <io-file-is-read-only-error>
   "ScmFilenameErrorMixin*" "Scm_FileIsReadOnlyErrorMixinClass"
   (c "filename_condition_cpa")
   ((filename))
   (allocator (c "filename_error_mixin_allocate")))

 (define-type <io-file-already-exists-error> "ScmFilenameErrorMixin*" #f
   "SCM_FILENAME_ERROR_MIXIN_P" "SCM_FILENAME_ERROR_MIXIN")
 (define-cclass <io-file-already-exists-error>
   "ScmFilenameErrorMixin*" "Scm_FileAlreadyExistsErrorMixinClass"
   (c "filename_condition_cpa+1")
   ((filename))
   (allocator (c "filename_error_mixin_allocate")))

 (define-type <io-no-such-file-error> "ScmFilenameErrorMixin*" #f
   "SCM_FILENAME_ERROR_MIXIN_P" "SCM_FILENAME_ERROR_MIXIN")
 (define-cclass <io-no-such-file-error>
   "ScmFilenameErrorMixin*" "Scm_NoSuchFileErrorMixinClass"
   (c "filename_condition_cpa+1")
   ((filename))
   (allocator (c "filename_error_mixin_allocate")))
 )

