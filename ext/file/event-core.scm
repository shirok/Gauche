;;;
;;; file.event - Interface for system file event API
;;;
;;;   Copyright (c) 2023-2024  Shiro Kawai  <shiro@acm.org>
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

(select-module file.event)

(inline-stub
 (.include <gauche/priv/configP.h>)
 ;;
 ;; Linux (inotify)
 ;;
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
      (name   :getter "if (obj->data->len) {\
                         return SCM_MAKE_STR_IMMUTABLE(obj->data->name);\
                       } else {\
                         return SCM_FALSE;\
                       }"
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
   ;; further event bits
   (define-enum IN_DONT_FOLLOW)
   (define-enum IN_EXCL_UNLINK)
   (define-enum IN_MASK_ADD)
   (define-enum IN_ONESHOT)
   (define-enum IN_ONLYDIR)
   ;(define-enum IN_MASK_CREATE)
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

 ;;
 ;; kqueue (BSD)
 ;;
 (.when (defined HAVE_SYS_EVENT_H)
   (declcode
    (.include <sys/event.h>)

    (define-ctype ScmSysKevent
      ::(.struct
         (SCM_HEADER :: ""
          data::(struct kevent))))
   )

   (define-cclass <sys-kevent> :private
     ScmSysKevent* "Scm_SysKeventClass" ()
     ())

   (define-enum EV_ADD)
   (define-enum EV_ENABLE)
   (define-enum EV_DISABLE)
   (define-enum EV_DISPATCH)
   (define-enum EV_DELETE)
   (define-enum EV_RECEIPT)
   (define-enum EV_ONESHOT)
   (define-enum EV_CLEAR)
   (define-enum EV_EOF)
   (define-enum EV_ERROR)

   (define-enum EVFILT_VNODE)

   (define-enum NOTE_ATTRIB)
   ;(define-enum NOTE_CLOSE)
   ;(define-enum NOTE_CLOSE_WRITE)
   (define-enum NOTE_DELETE)
   (define-enum NOTE_LINK)
   ;(define-enum NOTE_OPEN)
   ;(define-enum NOTE_READ)
   (define-enum NOTE_RENAME)
   (define-enum NOTE_REVOKE)
   (define-enum NOTE_WRITE)

   (define-cproc make-kevent (fd::<int>
                              filter::<short>
                              flags::<ushort>
                              fflags::<uint>
                              data::<int64>)
     (let* ([kev::ScmSysKevent* (SCM_NEW ScmSysKevent)])
       (SCM_SET_CLASS kev (& Scm_SysKeventClass))
       (EV_SET (& (-> kev data)) fd filter flags fflags data
               (cast void* kev))
       (return (SCM_OBJ kev))))

   (define-cfn list->keventArray (lis nelts::int*)
     ::(struct kevent*) :static
     (let* ([n::ScmSmallInt (Scm_Length lis)])
       (when (< n 0)
         (Scm_Error "list of <sys-kevent> required, but got: %S" lis))
       (let* ([p::(struct kevent*)
                  (?: n (SCM_NEW_ATOMIC_ARRAY (struct kevent) n) NULL)]
              [i::ScmSmallInt 0])
         (while (< i n)
           (unless (SCM_SYS_KEVENT_P (SCM_CAR lis))
             (Scm_Error "<sys-kevent> required, but got: %S" (SCM_CAR lis)))
           (let* ([kev::ScmSysKevent* (SCM_SYS_KEVENT (SCM_CAR lis))])
             (EV_SET (+ p i)
                     (ref (-> kev data) ident)
                     (ref (-> kev data) filter)
                     (ref (-> kev data) flags)
                     (ref (-> kev data) fflags)
                     (ref (-> kev data) data)
                     (cast void* kev)))
           (pre++ i)
           (set! lis (SCM_CDR lis)))
         (set! (* nelts) (cast int n))
         (return p))))

   (define-cfn keventArray->list (p::(struct kevent*) nelts::int)
     (let* ([h SCM_NIL] [t SCM_NIL] [i::int 0])
       (while (< i nelts)
         (let* ([kev::ScmSysKevent* (SCM_NEW ScmSysKevent)])
           (EV_SET (& (-> kev data))
                   (-> p ident)
                   (-> p filter)
                   (-> p flags)
                   (-> p fflags)
                   (-> p data)
                   kev)
           (SCM_SET_CLASS kev (& Scm_SysKeventClass))
           (SCM_APPEND1 h t (SCM_OBJ kev))))
       (return h)))

   (define-cproc kevent (kq::<int>
                         changeList
                         timeout)
     (let* ([ts::ScmTimeSpec]
            [pts::ScmTimeSpec* (Scm_GetTimeSpec timeout (& ts))]
            [ncl::int]
            [pcl::(struct kevent*) (list->keventArray changeList (& ncl))]
            [res::(.array (struct kevent) [128])]
            [r::int 0])
       (SCM_SYSCALL r (kevent kq pcl ncl res 128 pts))
       (cond [(< r 0) (Scm_SysError "kqueue failed" r)]
             [(== r 0) (return SCM_NIL)]
             [else (return (keventArray->list res r))])))

   (initcode
    (Scm_AddFeature "gauche.sys.kqueue"  NULL))

   ) ;; defined(HAVE_SYS_EVENT_H)

 ;;
 ;; OSX
 ;;
 (.when (and (defined __APPLE__) (defined __MACH__))
   (declcode
    (.include <CoreServices/CoreServices.h>)

    (define-ctype ScmSysFileSystemEventStream
      ::(.struct
         (SCM_HEADER :: ""
          data::FSEventStreamRef
          queue
          closed::int)))

    (define-ctype ScmSysFileSystemEvent
      ::(.struct
         (SCM_HEADER :: ""
          name             ; <string>
          flags
          id)))
    )

   (define-cclass <file-system-event-stream> :private
     ScmSysFileSystemEventStream* "Scm_SysFileSystemEventStreamClass" ()
     ())

   (define-cclass <file-system-event> :private
     ScmSysFileSystemEvent* "Scm_SysFileSystemEventClass" ()
     ((name)
      (flags)
      (id)))

   (define-cfn fsestream-cleanup (stream::ScmSysFileSystemEventStream*)
     ::void :static
     (unless (-> stream closed)
       (FSEventStreamStop (-> stream data))
       (FSEventStreamInvalidate (-> stream data))
       (FSEventStreamRelease (-> stream data))
       (set! (-> stream closed) TRUE)))

   (define-cfn fsestream-finalize (stream _::void*) ::void :static
     (fsestream-cleanup (cast ScmSysFileSystemEventStream* stream)))

   (define-cfn make-fsevent (path::char* flags::uint64_t id::uint64_t)
     (let* ([z::ScmSysFileSystemEvent* (SCM_NEW ScmSysFileSystemEvent)])
       (SCM_SET_CLASS z (& Scm_SysFileSystemEventClass))
       (set! (-> z name) (SCM_MAKE_STR_COPYING path))
       (set! (-> z flags) (Scm_MakeIntegerU64 flags))
       (set! (-> z id)   (Scm_MakeIntegerU64 id))
       (return (SCM_OBJ z))))

   (define-cfn fsestream-callback (_::ConstFSEventStreamRef
                                   streamobj::void*
                                   num-events::size_t
                                   event-paths::void*
                                   event-flags::(const FSEventStreamEventFlags*)
                                   event-ids::(const FSEventStreamEventId*))
     ::void :static
     (let* ([stream::ScmSysFileSystemEventStream*
             (cast ScmSysFileSystemEventStream* streamobj)]
            [paths::char** (cast char** event-paths)]
            [h (-> stream queue)]
            [t (?: (SCM_PAIRP h) (Scm_LastPair h) SCM_NIL)]
            [i::size_t 0])
       (while (< i num-events)
         (let* ([ev (make-fsevent (aref paths i)
                                  (aref event-flags i)
                                  (aref event-ids i))])
           (SCM_APPEND1 h t ev))
         (pre++ i))
       (set! (-> stream queue) h)))

   (define-cproc make-file-system-event-stream (path::<const-cstring>
                                                flags::<uint64>
                                                id::<uint64>
                                                latency::<double>)
     (let* ([cfpath::CFStringRef
             (CFStringCreateWithCString kCFAllocatorDefault
                                        path
                                        kCFStringEncodingUTF8)]
            [cfpatharray::CFArrayRef
             (CFArrayCreate NULL
                            (cast (const void**) cfpath)
                            1 NULL)]
            [stream::ScmSysFileSystemEventStream*
             (SCM_NEW ScmSysFileSystemEventStream)])
       (SCM_SET_CLASS stream (& Scm_SysFileSystemEventStreamClass))
       (set! (-> stream data) NULL
             (-> stream queue) SCM_NIL
             (-> stream closed) FALSE)
       (Scm_RegisterFinalizer (SCM_OBJ stream) fsestream-finalize NULL)
       (set! (-> stream data)
             (FSEventStreamCreate NULL
                                  (& fsestream-callback)
                                  (cast void* stream)
                                  cfpatharray
                                  id
                                  latency
                                  flags))
       ;; TODO: We need a run loop to handle the events.
       (return (SCM_OBJ stream))))

   (define-cproc close-file-system-event-stream (stream::<file-system-event-stream>)
     ::<void>
     (fsestream-cleanup stream))
   ) ;; defined(__APPLE__) && defined(__MACH__)
 )
