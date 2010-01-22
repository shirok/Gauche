;;;
;;; util.queue - queue (fifo) implementation
;;;  
;;;   Copyright (c) 2010  Shiro Kawai  <shiro@acm.org>
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
;;;  $Id: treeutil.scm,v 1.4 2008-05-10 13:35:56 shirok Exp $
;;;

;; This replaces pure Scheme implementation of util.queue.
;; The motivation of rewrite is to support mt-safe queue efficiently,
;; for such a queue turned out to be a crucial component of concurrent
;; programming.
;;
;; For mt-queue, we use layered mutex; C-level mutex and a Scheme slot
;; that keeps the locker.   For lightweight atomic operations such as
;; enqueue!, we just do the job while holding C-level mutex.  However,
;; if we need to call back to Scheme while holding a lock, it is dangerous
;; to do so with holding C-level mutex, since Scheme procedure may
;; take indefinitely long.  So we use Scheme-level slot to keep the
;; thread that is working on the queue.

(define-module util.queue
  (use srfi-1)
  (export <queue> <mtqueue>
          make-queue make-mtqueue queue? mtqueue?
          queue-empty? copy-queue
          queue-push! queue-push-unique! enqueue! enqueue-unique!
          queue-pop! dequeue! dequeue-all!
          queue-front queue-rear queue-length
          queue->list list->queue
          find-in-queue remove-from-queue!)
  )
(select-module util.queue)

(inline-stub
 "#include <gauche/class.h>"
 
 ;;
 ;; <queue>
 ;;
 "typedef struct QueueRec {"
 "  SCM_INSTANCE_HEADER;"
 "  u_int len;"
 "  ScmObj head;"
 "  ScmObj tail;"
 "} Queue;"

 "SCM_CLASS_DECL(QueueClass);"
 "#define QP(obj)          SCM_ISA(obj, &QueueClass)"
 "#define Q(obj)           ((Queue*)(obj))"
 "#define Q_HEAD(obj)      (Q(obj)->head)"
 "#define Q_TAIL(obj)      (Q(obj)->tail)"
 "#define Q_LENGTH(obj)    (Q(obj)->len)"
 "#define Q_EMPTY_P(obj)   (SCM_NULLP(Q_HEAD(obj)))"

 (define-cfn makeq (klass::ScmClass*)
   (let* ([z::Queue*
           (cast Queue* (Scm_AllocateInstance klass (sizeof Queue)))])
     (SCM_SET_CLASS z klass)
     (set! (Q_LENGTH z) 0 (Q_HEAD z) SCM_NIL (Q_TAIL z) SCM_NIL)
     (return (SCM_OBJ z))))
 
 (define-type <queue> "Queue*" "queue" "QP" "Q")
 (define-cclass <queue>
   "Queue*" "QueueClass" ()
   ((length :type <uint> :c-name "len" :setter #f))
   (allocator (return (makeq klass)))
   (printer (Scm_Printf port "#<queue %d @%p>" (Q_LENGTH obj) obj)))
 
 ;;
 ;; <mtqueue>
 ;;
 "typedef struct MtQueueReq {"
 "  Queue q;"
 "  u_int maxlen;"
 "  ScmInternalMutex mutex;"
 "  ScmObj locker;"  ;thread holding the lock.  see the comment above.
 "  ScmInternalCond lockWait;"
 "  ScmInternalCond readerWait;"
 "  ScmInternalCond writerWait;"
 "} MtQueue;"

 "SCM_CLASS_DECL(MtQueueClass);"
 "#define MTQP(obj)       SCM_ISA(obj, &MtQueueClass)"
 "#define MTQ(obj)        ((MtQueue*)(obj))"
 "#define MTQ_MAXLEN(obj) (MTQ(obj)->maxlen)"
 "#define MTQ_MUTEX(obj)  (MTQ(obj)->mutex)"
 "#define MTQ_LOCKER(obj) (MTQ(obj)->locker)"

 (define-cfn makemtq (klass::ScmClass* maxlen::u_int)
   (let* ([z::MtQueue*
           (cast MtQueue* (Scm_AllocateInstance klass (sizeof MtQueue)))])
     (SCM_SET_CLASS z klass)
     (set! (Q_LENGTH z) 0 (Q_HEAD z) SCM_NIL (Q_TAIL z) SCM_NIL
           (MTQ_MAXLEN z) maxlen
           (MTQ_LOCKER z) SCM_FALSE)
     (SCM_INTERNAL_MUTEX_INIT (MTQ_MUTEX z))
     (SCM_INTERNAL_COND_INIT (-> z lockWait))
     (SCM_INTERNAL_COND_INIT (-> z readerWait))
     (SCM_INTERNAL_COND_INIT (-> z writerWait))
     (return (SCM_OBJ z))))
 
 (define-type <mtqueue> "MtQueue*" "mt-queue" "MTQP" "MTQ")
 (define-cclass <mtqueue>
   "MtQueue*" "MtQueueClass" ("QueueClass")
   ((max-length :type <uint> :c-name "maxlen"))
   (allocator 
    (let* ([ml (Scm_GetKeyword ':max-length initargs SCM_FALSE)])
      (return (makemtq klass (?: (SCM_INTP ml) (SCM_INT_VALUE ml) 0)))))
   (printer 
    (Scm_Printf port "#<mt-queue %d @%p>" (Q_LENGTH obj) obj)))

 ;; lock macros
 (define-cise-expr big-locked?
   [(_ q) `(and (SCM_VMP (MTQ_LOCKER ,q))
                (not (== (-> (SCM_VM (MTQ_LOCKER ,q)) state)
                         SCM_VM_TERMINATED)))])
 
 (define-cise-stmt wait-mtq-big-lock    ;to be called while locking mutex
   [(_ q)
    `(while (big-locked? ,q)
       (SCM_INTERNAL_COND_WAIT (-> (MTQ ,q) lockWait) (MTQ_MUTEX ,q)))])

 (define-cise-stmt with-mtq-light-lock
   [(_ q . stmts)
    `(begin
       (SCM_INTERNAL_MUTEX_LOCK (MTQ_MUTEX ,q))
       (wait-mtq-big-lock ,q)
       ,@stmts
       (SCM_INTERNAL_MUTEX_UNLOCK (MTQ_MUTEX ,q)))])
 
 (define-cise-stmt grab-mtq-big-lock
   [(_ q)
    `(with-mtq-light-lock q (set! (MTQ_LOCKER ,q) (SCM_OBJ (Scm_VM))))])

 (define-cise-stmt wake-up-lockers
   [(_ q) `(SCM_INTERNAL_COND_BROADCAST (-> (MTQ ,q) lockWait))])
 (define-cise-stmt wake-up-writers
   [(_ q) `(SCM_INTERNAL_COND_BROADCAST (-> (MTQ ,q) writerWait))])
 (define-cise-stmt wake-up-readers
   [(_ q) `(SCM_INTERNAL_COND_BROADCAST (-> (MTQ ,q) readerWait))])

 (define-cise-stmt release-mtq-big-lock
   [(_ q)
    `(begin
       (SCM_INTERNAL_MUTEX_LOCK (MTQ_MUTEX ,q))
       (set! (MTQ_LOCKER ,q) SCM_FALSE)
       (wake-up-lockers ,q)
       (SCM_INTERNAL_MUTEX_UNLOCK (MTQ_MUTEX ,q)))])

 (define-cproc %lock-mtq (q::<mtqueue>) ::<void>   (grab-mtq-big-lock q))
 (define-cproc %unlock-mtq (q::<mtqueue>) ::<void> (release-mtq-big-lock q))
)

;; A common pattern
(define-syntax queue-op
  (syntax-rules ()
    [(_ q body)
     (cond [(mtqueue? q) (%lock-mtq q) (unwind-protect body (%unlock-mtq q))]
           [(queue? q) body]
           [else (error "queue required, but got" q)])]
    [(_ q body mtbody)
     (cond [(mtqueue? q) (%lock-mtq q) (unwind-protect mtbody (%unlock-mtq q))]
           [(queue? q) body]
           [else (error "queue required, but got" q)])]))

;;;
;;; Constructors
;;;
(inline-stub
 (define-cproc make-queue ()
   (result (makeq (& QueueClass))))
 (define-cproc make-mtqueue (:key (max-length::<int> 0))
   (result (makemtq (& MtQueueClass) max-length)))

 ;; caller must hold lock
 (define-cproc %queue-set-content! (q::<queue> list) ::<void>
   (let* ([len::int (Scm_Length list)])
     (when (< len 0) (SCM_TYPE_ERROR list "proper list"))
     (set! (Q_TAIL q) (?: (== len 0) SCM_NIL (Scm_LastPair list))
           (Q_HEAD q) list
           (Q_LENGTH q) len)))
 )

(define (list->queue lis :optional (class <queue>) :rest (initargs '()))
  (rlet1 q (apply make class initargs)
    (%queue-set-content! q (list-copy lis))))

(define-method copy-queue ((q <queue>))
  (list->queue (queue->list q) (class-of q)))

(define-method copy-queue ((q <mtqueue>))
  (list->queue (queue->list q) (class-of q)
               :max-length (mtqueue-max-length q)))

;;;
;;; Predicates
;;;
(inline-stub
 (define-cproc queue-empty? (q::<queue>) ::<boolean>
   (if (MTQP q)
     (let* ([r::int FALSE])
       (with-mtq-light-lock q (set! r (Q_EMPTY_P q)))
       (result r))
     (result (Q_EMPTY_P q))))
 )

(define-inline (queue? q)   (is-a? q <queue>))
(define-inline (mtqueue? q) (is-a? q <mtqueue>))

;;;
;;; Queries
;;;
(inline-stub
 (define-cproc queue-length (q::<queue>) ::<int> Q_LENGTH)
 (define-cproc mtqueue-max-length (q::<mtqueue>) ::<int> MTQ_MAXLEN)

 ;; caller must hold big lock
 (define-cproc %qhead (q::<queue>) (result (Q_HEAD q)))
 
 (define-cfn queue-peek-both-int (q::Queue* ph::ScmObj* pt::ScmObj*) ::int
   (when (Q_EMPTY_P q) (return FALSE))
   (set! (* ph) (SCM_CAR (Q_HEAD q))
         (* pt) (SCM_CAR (Q_TAIL q)))
   (return TRUE))

 (define-cfn queue-peek-both (q::Queue* ph::ScmObj* pt::ScmObj*) ::void
   (let* ([ok::int FALSE])
     (if (not (MTQP q))
       (set! ok (queue-peek-both-int q ph pt))
       (with-mtq-light-lock q (set! ok (queue-peek-both-int q ph pt))))
     (unless ok (Scm_Error "queue is empty: %S" q))))
 
 (define-cproc queue-front (q::<queue>)
   (let* ([head] [tail])
     (queue-peek-both q (& head) (& tail))
     (result head)))
              
 (define-cproc queue-rear (q::<queue>)
   (let* ([head] [tail])
     (queue-peek-both q (& head) (& tail))
     (result tail)))
 )

(define (queue->list q) (queue-op q (list-copy (%qhead q))))
(define (find-in-queue pred q) (queue-op q (find pred (%qhead q))))

;;;
;;; Enqueue/dequeue
;;;

;; enqueue! - add item(s) to the tail
(inline-stub
 (define-cfn enqueue_int (q::Queue* cnt::u_int head tail) ::void
   (set! (Q_LENGTH q) (+ (Q_LENGTH q) cnt))
   (cond [(Q_EMPTY_P q) (set! (Q_HEAD q) head (Q_TAIL q) tail)]
         [else          (SCM_SET_CDR (Q_TAIL q) head)
                        (set! (Q_TAIL q) tail)]))

 (define-cproc %enqueue! (q::<queue> cnt::<uint> head tail) ::<void>
   (enqueue_int q cnt head tail))

 (define-cise-expr mtq-overflows
   [(_ q cnt)
    `(and (> (MTQ_MAXLEN ,q) 0)
          (> (+ ,cnt (Q_LENGTH ,q)) (MTQ_MAXLEN ,q)))])
 
 (define-cproc enqueue! (q::<queue> obj :rest more-objs)
   (let* ([head (Scm_Cons obj more-objs)] [tail] [cnt::u_int])
     (if (SCM_NULLP more-objs)
       (set! tail head cnt 1)
       (set! tail (Scm_LastPair more-objs) cnt (Scm_Length head)))
     (if (not (MTQP q))
       (enqueue_int q cnt head tail)
       (let* ([ovf::int FALSE])
         (with-mtq-light-lock q (if (mtq-overflows q cnt)
                                  (set! ovf TRUE)
                                  (begin
                                    (enqueue_int q cnt head tail)
                                    (wake-up-readers q))))
         (when ovf (Scm_Error "queue is full: %S" q))))
     (result (SCM_OBJ q))))

 (define-cproc enqueue/wait! (q::<mtqueue> obj :optional (timeout #f)
                                                         (timeout-val #f))
   ;;WRITEME
   (result (SCM_OBJ q))
   )
 )

(define (enqueue-unique! q cmp obj . more-objs)
  (define (pick lis xs ins)
    (cond [(null? ins) xs]
          [(or (member (car ins) lis cmp) (member (car ins) xs cmp))
           (pick lis xs (cdr ins))]
          [else (pick lis (cons (car ins) xs) (cdr ins))]))
  (queue-op q (let1 xs (pick (%qhead q) '() (cons obj more-objs))
                (unless (null? xs)
                  (let1 xs_ (reverse xs)
                    (%enqueue! q (length xs) xs_ (last-pair xs_))))))
  q)

(inline-stub
 ;; queue-push! - add item(s) to the head
 (define-cfn queue-push-int (q::Queue* cnt::u_int head tail) ::void
   (SCM_SET_CDR tail (Q_HEAD q))
   (set! (Q_HEAD q) head
         (Q_TAIL q) (Scm_LastPair tail)
         (Q_LENGTH q) (+ (Q_LENGTH q) cnt)))
   
 (define-cproc queue-push! (q::<queue> obj :rest more-objs)
   (let* ([objs (Scm_Cons obj more-objs)] [head] [tail] [cnt::u_int])
     (if (SCM_NULLP more-objs)
       (set! head objs tail objs cnt 1)
       (set! head (Scm_ReverseX objs)
             tail (Scm_LastPair head)
             cnt  (Scm_Length head)))
     (if (not (MTQP q))
       (queue-push-int q cnt head tail)
       (let* ([ovf::int FALSE])
         (with-mtq-light-lock q (if (and (> (MTQ_MAXLEN q) 0)
                                         (> (+ cnt (Q_LENGTH q))
                                            (MTQ_MAXLEN q)))
                                  (set! ovf TRUE)
                                  (queue-push-int q cnt head tail)))
         (when ovf (Scm_Error "queue is full: %S" q))))
     (result (SCM_OBJ q))))

 (define-cproc queue-push/wait! (q::<mtqueue> obj :optional (timeout #f)
                                                            (timeout-val #f))
   ;; WRITEME
   (result (SCM_OBJ q))
   )
)

(define (queue-push-unique! q cmp obj . more-objs)
  (define (pick lis ins)
    (cond [(null? ins) lis]
          [(member (car ins) lis cmp) (pick lis (cdr ins))]
          [else (pick (cons (car ins) lis) (cdr ins))]))
  (queue-op q (let1 xs (pick (%qhead q) (cons obj more-objs))
                (unless (eq? xs (%qhead q))
                  (%queue-set-content! q xs))))
  q)

;; dequeue!
(inline-stub
 (define-cfn dequeue-int (q::Queue* result::ScmObj*) ::int
   (cond [(Q_EMPTY_P q) (return TRUE)]
         [else (set! (* result) (SCM_CAR (Q_HEAD q)))
               (set! (Q_HEAD q) (SCM_CDR (Q_HEAD q)))
               (dec! (Q_LENGTH q))
               (return FALSE)]))
 
 (define-cproc dequeue! (q::<queue> :optional fallback)
   (let* ([empty::int FALSE] [r SCM_UNDEFINED])
     (if (not (MTQP q))
       (set! empty (dequeue-int q (& r)))
       (with-mtq-light-lock q (set! empty (dequeue-int q (& r)))))
     (when empty
       (if (SCM_UNBOUNDP fallback)
         (Scm_Error "queue is empty: %S" q)
         (set! r fallback)))
     (result r)))

 (define-cproc dequeue/wait! (q::<mtqueue> :optional (timeout #f)
                                                     (timeout-val #f))
   (let* ([r SCM_UNDEFINED])
     (SCM_INTERNAL_MUTEX_LOCK (MTQ_MUTEX q))
     (wait-mtq-big-lock q) ;; TODO: think over this
     (while (Q_EMPTY_P q)
       ;; TODO: timeout
       (SCM_INTERNAL_COND_WAIT (-> q readerWait) (MTQ_MUTEX q)))
     (set! r (SCM_CAR (Q_HEAD q)))
     (set! (Q_HEAD q) (SCM_CDR (Q_HEAD q)))
     (SCM_INTERNAL_MUTEX_UNLOCK (MTQ_MUTEX q))
     (result r)))

 (define-cfn dequeue-all-int (q::Queue*)
   (let* ([lis (Q_HEAD q)])
     (set! (Q_LENGTH q) 0 (Q_HEAD q) SCM_NIL (Q_TAIL q) SCM_NIL)
     (return lis)))
 
 (define-cproc dequeue-all! (q::<queue>)
   (if (not (MTQP q))
     (result (dequeue-all-int q))
     (let* ([r])
       (with-mtq-light-lock q (set! r (dequeue-all-int q)))
       (result r))))
 )

(define queue-pop! dequeue!)
(define queue-pop/wait! dequeue/wait!)

(define (remove-from-queue! pred q)
  (rlet1 removed? #f
    (queue-op q (let loop ([rs '()] [xs (%qhead q)] [hit #f])
                  (cond [(null? xs) (when hit 
                                      (set! removed? #t)
                                      (%queue-set-content! q (reverse! rs)))]
                        [(pred (car xs)) (loop rs (cdr xs) #t)]
                        [else (loop (cons (car xs) rs) (cdr xs) hit)])))))

;; NB: Scheme48 has delete-from-queue!, whose argument order is
;; reversed from 'delete' in SRFI-1.   I leave it undefined here.
;;
;; (define (delete-from-queue! q item)  ;;Scheme48
;;   (remove-from-queue! (lambda (elt) (eq? item elt)) q))
