;;;
;;; file.event - Interface for system file event API
;;;
;;;   Copyright (c) 2023  Shiro Kawai  <shiro@acm.org>
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

(define-module file.event
  )
(select-module file.event)

(inline-stub
 (.when (defined HAVE_SYS_INOTIFY_H)
   (.include <sys/inotify.h>)

   (define-ctype ScmSysInotifyEvent::(.struct
                                      (SCM_HEADER :: ""
                                       data::(struct inotify_event *))))

   (define-cclass <inotify-event> :private
     ScmSysInotifyEvent* "Scm_SysInotifyEventClass" ()
     ((wd     :type <int>
              :c-name "data->wd")
      (mask   :type <uint32>
              :c-name "data->mask")
      (cookie :type <uint32>
              :c-name "data->cookie")
      (name   :getter "return Scm_MakeString(obj->data->name, obj->data->len, -1, 0);"
              :setter #f)
     ))

   (define-enum IN_ACCESS)
   (define-enum IN_ATTRIB)
   (define-enum IN_CLOSE_WRITE)
   (define-enum IN_CLOSE_NOWRITE)
   (define-enum IN_CREATE)
   (define-enum IN_DELETE)
   (define-enum IN_DELETE_SELF)
   (define-enum IN_MODIFY)
   (define-enum IN_MOVE_SELF)
   (define-enum IN_MOVED_FROM)
   (define-enum IN_MOVED_TO)
   (define-enum IN_OPEN)
   ;; event masks
   (define-enum IN_ALL_EVENTS)
   (define-enum IN_MOVE)
   (define-enum IN_CLOSE)
   ;; firther event bits
   (define-enum IN_DONT_FOLLOW)
   (define-enum IN_EXCL_UNLINK)
   (define-enum IN_MASK_ADD)
   (define-enum IN_ONESHOT)
   (define-enum IN_ONLYDIR)
   (define-enum IN_MASK_CREATE)
   ;; returned bits
   (define-enum IN_IGNORED)
   (define-enum IN_ISDIR)
   (define-enum IN_Q_OVERFLOW)
   (define-enum IN_UNMOUNT)

   ;; flags for inotify_init1
   (define-enum IN_NONBLOCK)
   (define-enum IN_CLOEXEC)

   (define-cproc inotify-init () ::<int>
     (let* ([r::int (inotify_init)])
       (when (< r 0) (Scm_SysError "inotify_init failed"))
       (return r)))

   (define-cproc inotify-init1 (flags::<int>) ::<int>
     (let* ([r::int (inotify_init1 flags)])
       (when (< r 0) (Scm_SysError "inotify_init1 failed"))
       (return r)))

   (define-cproc inotify-add-watch (fd::<int>
                                    pathname::<const-cstring>
                                    mask::<uint32>)
     ::<int>
     (let* ([r::int (inotify_add_watch fd pathname mask)])
       (when (< r 0) (Scm_SysError "inotify_add_watch failed"))
       (return r)))

   (define-cproc inotify-rm-watch (fd::<int> wd::<int>)
     ::<void>
     (let* ([r::int (inotify_rm_watch fd wd)])
       (when (< r 0) (Scm_SysError "inotify_rm_watch failed"))))

   (define-cfn inotify-event-extract (buf::(struct inotify_event*))
     (let* ([z::ScmSysInotifyEvent* (SCM_NEW ScmSysInotifyEvent)])
       (SCM_SET_CLASS z (& Scm_SysInotifyEventClass))
       (set! (-> z data) (cast (struct inotify_event*) buf))
       (return (SCM_OBJ z))))

   (define-cproc inotify-event-copy (ev::<inotify-event>)
     (let* ([dst::(struct inotify_event*)
                  (SCM_NEW2 (struct inotify_event*)
                            (+ (sizeof (struct inotify_event))
                               (-> ev data len)))])
       (set! (* dst) (* (-> ev data))) ; verbatim copy
       (return (inotify-event-extract dst))))

   ;; NB: Returned events share the memory with the buffer.  Copy it
   ;; if you want to keep the event after reusing the buffer.
   (define-cproc inotify-read-events (fd::<int> buf::<u8vector>)
     (let* ([buffer::char* (cast char* (SCM_U8VECTOR_ELEMENTS buf))]
            [len::ssize_t (read fd buffer (SCM_U8VECTOR_SIZE buf))]
            [h SCM_NIL] [t SCM_NIL] [p::char* buffer])
       (when (< len 0) (Scm_SysError "reading inotify events failed"))
       (while (< p (+ buffer len))
         (let* ([ev (inotify-event-extract (cast (struct inotify_event*) p))])
           (SCM_APPEND1 h t ev)
           (set! p (+ p
                      (sizeof (struct inotify_event))
                      (-> (SCM_INOTIFY_EVENT ev) data len)))))
       (return h)))

   (initcode
    (Scm_AddFeature "gauche.sys.inotify"  NULL))

   ) ;; defined(HAVE_SYS_INOTIFY_H)
 )
