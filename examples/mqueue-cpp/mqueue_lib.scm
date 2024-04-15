;;;
;;; mqueue_lib.stub
;;;

(in-module example.mqueue-cpp)

(inline-stub
 (.include "mqueue_glue.h")

 ;; Let the stub generator know about <mqueue> type.
 ;; The arguments of define-type are:
 ;;
 ;; (declare-stub-type
 ;;    scheme-name    ;; Scheme class name
 ;;    c-type         ;; C type name when unboxed
 ;;    description    ;; used in error message
 ;;    c-predicate    ;; bool c-predicate(ScmObj obj) - check whether
 ;;                   ;;   obj is the type.
 ;;    c-unbox        ;; c-type c-unbox(ScmObj obj) - unbox obj.
 ;;                   ;;   You can assume obj is of the type.
 ;;    c-box          ;; ScmObj c-box(c-type x) - box obj.
 ;;    )
 ;;
 ;; Note: when you use a foreign pointer class, care should be taken
 ;; if you want to save the unboxed value somewhere; since the ScmObj
 ;; that points to the unboxed value may be unreferenced after subr
 ;; call, you should be responsible to make sure the unboxed pointer
 ;; is visible from GC.
 (declare-stub-type <mqueue> "MQueue*" "mqueue"
   "MQUEUE_P" "MQUEUE_UNBOX" "MQUEUE_BOX")

 (define-cproc make-mqueue (name::<const-cstring>) ::<mqueue>
   (return (new (MQueue name))))

 (define-cproc mqueue-find (name::<const-cstring>) ::<mqueue>
   (return (MQueue::findByName name)))

 (define-cproc mqueue-name (mq::<mqueue>) ::<const-cstring>
   (return (ref (-> mq (getName)) (c_str))))

 (define-cproc mqueue-empty? (mq::<mqueue>) ::<boolean>
   (return (-> mq (empty))))

 (define-cproc mqueue-push! (mq::<mqueue> message::<const-cstring>
                                          :optional (urgency::<int> 0))
   ::<int>
   (return (-> mq (pushMessage message urgency))))

 (define-cproc mqueue-pop! (mq::<mqueue>)
   (catch ("MQueueException& e"
           (Scm_Error "mqueue-pop!: %s" (ref e reason (c_str)))))
   (return (SCM_MAKE_STR_COPYING (ref (-> mq (popMessage)) (c_str)))))
 )
