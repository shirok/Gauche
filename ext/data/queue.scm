;;;
;;; data.queue - queue (fifo) implementation
;;;
;;;   Copyright (c) 2010-2015  Shiro Kawai  <shiro@acm.org>
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

;; Originally implemented as util.queue; since it is an implementation
;; of a data structure, we renamed it to data.queue.
;;
;; This module supports <queue>, which is fast but not thread-safe,
;; and <mtqueue>, thread-safe queue that can also be used as a fundamental
;; block of multi-thread synchronization.
;;
;; For mt-queue, we use layered mutex; C-level mutex and a Scheme slot
;; that keeps the locker.   For lightweight atomic operations such as
;; enqueue!, we just do the job while holding C-level mutex.  However,
;; if we need to call back to Scheme while holding a lock, it is dangerous
;; to do so with holding C-level mutex, since Scheme procedure may
;; take indefinitely long.  So we use Scheme-level slot to keep the
;; thread that is working on the queue.

(define-module data.queue
  (export <queue> <mtqueue>
          make-queue make-mtqueue queue? mtqueue?
          queue-length mtqueue-max-length mtqueue-room
          queue-empty? copy-queue
          queue-push! queue-push-unique! enqueue! enqueue-unique!
          queue-pop! dequeue! dequeue-all!
          queue-front queue-rear queue-length
          queue->list list->queue queue-internal-list
          find-in-queue remove-from-queue!
          any-in-queue every-in-queue

          enqueue/wait! queue-push/wait! dequeue/wait! queue-pop/wait!)
  )
(select-module data.queue)

;;;
;;;  Data structures
;;;
(inline-stub
 "#include <gauche/class.h>"

 ;;
 ;; <queue>
 ;;
 "typedef struct QueueRec {"
 "  SCM_INSTANCE_HEADER;"
 "  long len;"     ;; lazily calc'd.  -1 for 'to be calculated'
 "  ScmObj head;"
 "  ScmObj tail;"
 "} Queue;"

 "SCM_CLASS_DECL(QueueClass);"
 "#define QP(obj)          SCM_ISA(obj, &QueueClass)"
 "#define Q(obj)           ((Queue*)(obj))"
 "#define Q_HEAD(obj)      (Q(obj)->head)"
 "#define Q_TAIL(obj)      (Q(obj)->tail)"
 "#define Q_LENGTH(obj)    (Q(obj)->len)"  ; can be -1; should use %qlength().
 "#define Q_EMPTY_P(obj)   (SCM_NULLP(Q_HEAD(obj)))"

 (define-cfn %qlength (q::Queue*) ::u_long  ; must be called with lock held
   (when (< (Q_LENGTH q) 0)
     (set! (Q_LENGTH q) (Scm_Length (Q_HEAD q))))
   (return (cast u_long (Q_LENGTH q))))

 (define-cfn makeq (klass::ScmClass*)
   (let* ([z::Queue* (SCM_NEW_INSTANCE Queue klass)])
     (set! (Q_LENGTH z) 0 (Q_HEAD z) SCM_NIL (Q_TAIL z) SCM_NIL)
     (return (SCM_OBJ z))))

 (define-type <queue> "Queue*" "queue" "QP" "Q")
 (define-cclass <queue>
   "Queue*" "QueueClass" ()
   ((length :type <uint> :c-name "len" :setter #f))
   (allocator (return (makeq klass)))
   (printer (Scm_Printf port "#<queue %d @%p>" (%qlength (Q obj)) obj)))

 ;;
 ;; <mtqueue>
 ;;
 "typedef struct MtQueueReq {"
 "  Queue q;"
 "  int maxlen;"     ;negative if unlimited
 "  ScmInternalMutex mutex;"
 "  ScmObj locker;"  ;thread holding the lock.  see the comment above.
 "  ScmInternalCond lockWait;"
 "  ScmInternalCond readerWait;"
 "  ScmInternalCond writerWait;"
 "  int readerSem;" ;used by zero-length queue.  # of waiting reader
 "} MtQueue;"

 "SCM_CLASS_DECL(MtQueueClass);"
 "#define MTQP(obj)       SCM_ISA(obj, &MtQueueClass)"
 "#define MTQ(obj)        ((MtQueue*)(obj))"
 "#define MTQ_MAXLEN(obj) (MTQ(obj)->maxlen)"
 "#define MTQ_MUTEX(obj)  (MTQ(obj)->mutex)"
 "#define MTQ_LOCKER(obj) (MTQ(obj)->locker)"

 "#define MTQ_LOCK(q)   (SCM_INTERNAL_MUTEX_LOCK (MTQ_MUTEX q))"
 "#define MTQ_UNLOCK(q) (SCM_INTERNAL_MUTEX_UNLOCK (MTQ_MUTEX q))"
 "#define MTQ_CV(q, kind) (MTQ(q)->kind)"
 "#define MTQ_READER_SEM(q) (MTQ(q)->readerSem)"


 (define-cfn makemtq (klass::ScmClass* maxlen::int)
   (let* ([z::MtQueue* (SCM_NEW_INSTANCE MtQueue klass)])
     (set! (Q_LENGTH z) 0 (Q_HEAD z) SCM_NIL (Q_TAIL z) SCM_NIL
           (MTQ_MAXLEN z) maxlen
           (MTQ_LOCKER z) SCM_FALSE
           (MTQ_READER_SEM z) 0)
     (SCM_INTERNAL_MUTEX_INIT (MTQ_MUTEX z))
     (SCM_INTERNAL_COND_INIT (MTQ_CV z lockWait))
     (SCM_INTERNAL_COND_INIT (MTQ_CV z readerWait))
     (SCM_INTERNAL_COND_INIT (MTQ_CV z writerWait))
     (return (SCM_OBJ z))))

 (define-cfn mtq-maxlen-get (mtq::MtQueue*)
   (let* ([ml::int (MTQ_MAXLEN mtq)])
     (if (< ml 0) (return '#f) (return (SCM_MAKE_INT ml)))))

 (define-cfn mtq-maxlen-set (mtq::MtQueue* maxlen) ::void
   (cond [(SCM_UINTP maxlen) (set! (MTQ_MAXLEN mtq) (SCM_INT_VALUE maxlen))]
         [(SCM_FALSEP maxlen) (set! (MTQ_MAXLEN mtq) -1)]
         [else (SCM_TYPE_ERROR maxlen "non-negative fixnum or #f")]))

 (define-type <mtqueue> "MtQueue*" "mt-queue" "MTQP" "MTQ")
 (define-cclass <mtqueue>
   "MtQueue*" "MtQueueClass" ("QueueClass")
   ((max-length :getter "return mtq_maxlen_get(obj);"
                :setter "mtq_maxlen_set(obj, value);"))
   (allocator
    (let* ([ml (Scm_GetKeyword ':max-length initargs SCM_FALSE)])
      (return (makemtq klass (?: (SCM_INTP ml) (SCM_INT_VALUE ml) -1)))))
   (printer
    (Scm_Printf port "#<mt-queue %d @%p>" (%qlength (Q obj)) obj)))

 ;; lock macros
 (define-cise-expr big-locked?
   [(_ q) `(and (SCM_VMP (MTQ_LOCKER ,q))
                (not (== (-> (SCM_VM (MTQ_LOCKER ,q)) state)
                         SCM_VM_TERMINATED)))])

 (define-cise-stmt with-mtq-mutex-lock
   [(_ q . body)
    `(begin (SCM_INTERNAL_MUTEX_SAFE_LOCK_BEGIN (MTQ_MUTEX ,q))
            ,@body
            (SCM_INTERNAL_MUTEX_SAFE_LOCK_END))])

 (define-cise-stmt wait-mtq-big-lock    ;to be called while locking mutex
   [(_ q) `(while (big-locked? ,q)
             (SCM_INTERNAL_COND_WAIT (MTQ_CV ,q lockWait) (MTQ_MUTEX ,q)))])

 (define-cise-stmt with-mtq-light-lock
   [(_ q . stmts) `(with-mtq-mutex-lock ,q (wait-mtq-big-lock ,q) ,@stmts)])

 (define-cise-stmt grab-mtq-big-lock
   [(_ q) `(with-mtq-light-lock ,q (set! (MTQ_LOCKER ,q) (SCM_OBJ (Scm_VM))))])

 (define-cise-stmt release-mtq-big-lock
   [(_ q) `(with-mtq-mutex-lock ,q
             (set! (MTQ_LOCKER ,q) SCM_FALSE)
             (notify-lockers ,q))])

 (define-cise-stmt notify-lockers
   [(_ q) `(SCM_INTERNAL_COND_BROADCAST (MTQ_CV ,q lockWait))])
 (define-cise-stmt notify-writers
   [(_ q) `(SCM_INTERNAL_COND_BROADCAST (MTQ_CV ,q writerWait))])
 (define-cise-stmt notify-readers
   [(_ q) `(SCM_INTERNAL_COND_BROADCAST (MTQ_CV ,q readerWait))])

 "#define CW_TIMEDOUT 1"
 "#define CW_INTR     2"
 ;; (wait-cv Q SLOT PTIMESPEC STATUS)
 ;;   Wait on Q's condition variable in SLOT.  PTIMESPEC is a pointer
 ;;   to ScmTimeSpec or NULL, specifies timeout.  Must be called
 ;;   while MTQ_MUTEX(Q) is held.  When returns, MTQ_MUTEX(Q) is held,
 ;;   and status contains either 0 (condition met), CW_TIMEOUT (timed out),
 ;;   or CW_INTR (interrupted).
 (define-cise-stmt wait-cv
   [(_ q slot ptimespec status)
    (let1 r (gensym)
      `(cond [,ptimespec
              (let* ([,r :: int
                      (SCM_INTERNAL_COND_TIMEDWAIT (-> ,q ,slot)
                                                   (MTQ_MUTEX ,q)
                                                   ,ptimespec)])
                (cond [(== ,r SCM_INTERNAL_COND_TIMEDOUT)
                       (set! ,status CW_TIMEDOUT)]
                      [(== ,r SCM_INTERNAL_COND_INTR)
                       (set! ,status CW_INTR)]
                      [else (set! ,status 0)]))]
             [else (SCM_INTERNAL_COND_WAIT (-> ,q ,slot) (MTQ_MUTEX ,q))
                   (set! ,status 0)]))])

 ;; (do-with-timeout Q RETVAL TIMEOUT TIMEOUT-VAL CVAR INIT WAIT-CHECK DO-OK)
 ;;   Lock Q and execute INIT, and wait while WAIT-CHECK satisfies.  Once
 ;;   WAIT-CHECK returns false, execute a statement DO-OK.  CVAR is a
 ;;   condition variable (-> Q CVAR) to be waited on.
 ;;   If TIMEOUT isn't NULL and waiting times out, RETVAL is set with
 ;;   TIMEOUT-VAL.  (DO-OK is supposed to set RETVAL in it).
 (define-cise-stmt do-with-timeout
   [(_ q retval timeout timeout-val cv-slot init wait-check do-ok)
    (let ([ts (gensym)] [pts (gensym)] [status (gensym)] [inited (gensym)])
      `(let* ([,ts :: (ScmTimeSpec)] [,status :: int 0]
              [,pts :: (ScmTimeSpec*) (Scm_GetTimeSpec ,timeout (& ,ts))]
              [,inited :: int FALSE])
         (while TRUE
           (with-mtq-mutex-lock ,q
             (unless ,inited ,init (set! ,inited TRUE))
             (while TRUE
               (wait-mtq-big-lock ,q)
               (cond [,wait-check (wait-cv ,q ,cv-slot ,pts ,status)
                                  (when (== ,status 0) (continue))]
                     [else ,do-ok (set! ,status 0)])
               (set! (MTQ_LOCKER ,q) SCM_FALSE)
               (notify-lockers ,q)
               (break)))
           (case ,status
             [(CW_TIMEDOUT) (set! ,retval ,timeout-val)]
             [(CW_INTR)     (Scm_SigCheck (Scm_VM)) (continue)]) ;restart op
           (break))))])

 (define-cproc %lock-mtq (q::<mtqueue>) ::<void>   (grab-mtq-big-lock q))
 (define-cproc %unlock-mtq (q::<mtqueue>) ::<void> (release-mtq-big-lock q))
 (define-cproc %notify-writers (q::<mtqueue>) ::<void> (notify-writers q))
 (define-cproc %notify-readers (q::<mtqueue>) ::<void> (notify-readers q))
 )

;; A common pattern
(define-syntax queue-op
  (syntax-rules ()
    [(_ q proc)
     (cond
      [(mtqueue? q) (%lock-mtq q) (unwind-protect (proc #t) (%unlock-mtq q))]
      [(queue? q)   (proc #f)]
      [else (error "queue required, but got" q)])]))

;;;
;;; Constructors
;;;
(inline-stub
 (define-cproc make-queue ()
   (return (makeq (& QueueClass))))
 (define-cproc make-mtqueue (:key (max-length #f))
   (return (makemtq (& MtQueueClass)
                    (?: (SCM_UINTP max-length)
                        (SCM_INT_VALUE max-length)
                        -1))))

 ;; caller must hold lock
 (define-cproc %queue-set-content! (q::<queue> list last-pair) ::<void>
   (if (SCM_PAIRP list)
     (let* ([tail (?: (SCM_PAIRP last-pair) last-pair (Scm_LastPair list))])
       (set! (Q_TAIL q) tail
             (Q_HEAD q) list
             (Q_LENGTH q) -1))
     (set! (Q_TAIL q) SCM_NIL
           (Q_HEAD q) SCM_NIL
           (Q_LENGTH q) 0)))
 )

(define (list->queue lis :optional (class <queue>) :rest initargs)
  (rlet1 q (apply make class initargs)
    (%queue-set-content! q (list-copy lis) #f)))

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
       (return r))
     (return (Q_EMPTY_P q))))
 )

(define-inline (queue? q)   (is-a? q <queue>))
(define-inline (mtqueue? q) (is-a? q <mtqueue>))

;;;
;;; Queries
;;;
(inline-stub
 (define-cise-expr mtq-overflows        ;true if adding CNT elts overflows Q.
   [(_ q cnt)
    `(and (>= (MTQ_MAXLEN ,q) 0)
          (> (+ ,cnt (%qlength (Q ,q))) (MTQ_MAXLEN ,q)))])

 ;; API
 (define-cproc queue-length (q::<queue>) ::<int> %qlength)
 (define-cproc mtqueue-max-length (q::<mtqueue>)
   (return (?: (>= (MTQ_MAXLEN q) 0) (SCM_MAKE_INT (MTQ_MAXLEN q)) '#f)))

 ;; caller must hold lock
 (define-cproc %mtqueue-overflow? (q::<mtqueue> cnt::<int>) ::<boolean>
   (return (mtq-overflows q cnt)))

 ;; API
 (define-cproc mtqueue-room (q::<mtqueue>) ::<number>
   (let* ([room::int -1])
     (with-mtq-light-lock q
       (when (>= (MTQ_MAXLEN q) 0)
         (set! room (- (MTQ_MAXLEN q) (%qlength (Q q))))))
     (if (>= room 0)
       (return (SCM_MAKE_INT room))
       (return SCM_POSITIVE_INFINITY))))

 ;; caller must hold big lock
 ;; %qtail isn't used in data.queue, but used by srfi-117
 (define-cproc %qhead (q::<queue>) (return (Q_HEAD q)))
 (define-cproc %qtail (q::<queue>) (return (Q_TAIL q)))

 (define-cfn queue-peek-both-int (q::Queue* ph::ScmObj* pt::ScmObj*) ::int
   (when (Q_EMPTY_P q) (return FALSE))
   (set! (* ph) (SCM_CAR (Q_HEAD q))
         (* pt) (SCM_CAR (Q_TAIL q)))
   (return TRUE))

 (define-cproc %queue-peek (q::<queue> :optional fallback) ::(<top> <top>)
   (let* ([ok::int FALSE] [h] [t])
     (if (not (MTQP q))
       (set! ok (queue-peek-both-int q (& h) (& t)))
       (with-mtq-light-lock q (set! ok (queue-peek-both-int q (& h) (& t)))))
     (cond [ok (return h t)]
           [(SCM_UNBOUNDP fallback) (Scm_Error "queue is empty: %S" q)]
           [else (return fallback fallback)])))
 )

;; APIs
(define queue-front
  (case-lambda
    [(q)         (values-ref (%queue-peek q) 0)]
    [(q default) (values-ref (%queue-peek q default) 0)]))
(define queue-rear
  (case-lambda
    [(q)         (values-ref (%queue-peek q) 1)]
    [(q default) (values-ref (%queue-peek q default) 1)]))
(define (queue->list q)         (queue-op q (^_(list-copy (%qhead q)))))
(define (find-in-queue pred q)  (queue-op q (^_(find pred (%qhead q)))))
(define (any-in-queue pred q)   (queue-op q (^_(any pred (%qhead q)))))
(define (every-in-queue pred q) (queue-op q (^_(every pred (%qhead q)))))

;; This returns internal list of the queue.  Many queue operation
;; mutates the internal list, so it is not safe to hold onto the
;; result value.  We specifically prohibit getting internal list of
;; mtqueue for the safety.
(define (queue-internal-list q)
  (when (mtqueue? q)
    (error "Can't get internal list of <mtqueue>:" q))
  (%qhead q))

;;;
;;; Enqueue/dequeue
;;;

(inline-stub
 ;; internal enqueue - lock must be held.
 (define-cfn enqueue_int (q::Queue* cnt::u_int head tail) ::void
   (when (>= (Q_LENGTH q) 0)
     (set! (Q_LENGTH q) (+ (Q_LENGTH q) cnt)))
   (cond [(Q_EMPTY_P q) (set! (Q_HEAD q) head (Q_TAIL q) tail)]
         [else          (SCM_SET_CDR (Q_TAIL q) head)
                        (set! (Q_TAIL q) tail)]))

 ;; to call internal enqueue from Scheme.  lock must be held.
 (define-cproc %enqueue! (q::<queue> cnt::<uint> head tail) ::<void>
   (enqueue_int q cnt head tail))

 ;; (q-write-op OP Q CNT HEAD TAIL)
 ;;   If Q isn't mtq, simply call (OP Q CNT HEAD TAIL).
 ;;   If Q is mtq, lock Q, and if queue isn't full, execute
 ;;   (OP Q CNT HEAD TAIL) then notify waiting readers.
 ;;   If queue is full, signals an error.
 (define-cise-stmt q-write-op
   [(_ op q cnt head tail)
    `(if (MTQP ,q)
       (let* ([ovf::int FALSE])
         (with-mtq-light-lock ,q
           (cond [(mtq-overflows ,q ,cnt) (set! ovf TRUE)]
                 [else (,op ,q ,cnt ,head ,tail) (notify-readers ,q)]))
         (when ovf (Scm_Error "queue is full: %S" ,q)))
       (,op ,q ,cnt ,head ,tail))])

 ;; API
 (define-cproc enqueue! (q::<queue> obj :rest more-objs)
   (let* ([head (Scm_Cons obj more-objs)] [tail] [cnt::u_int])
     (if (SCM_NULLP more-objs)
       (set! tail head cnt 1)
       (set! tail (Scm_LastPair more-objs) cnt (Scm_Length head)))
     (q-write-op enqueue_int q cnt head tail)
     (return (SCM_OBJ q))))

 ;; API
 (define-cproc enqueue/wait! (q::<mtqueue> obj :optional (timeout #f)
                                                         (timeout-val #f))
   (let* ([cell (SCM_LIST1 obj)] [retval (SCM_OBJ q)])
     (.if "defined(GAUCHE_HAS_THREADS)"
          (do-with-timeout q retval timeout timeout-val writerWait
                           (begin)
                           (?: (!= (MTQ_MAXLEN q) 0)
                               (mtq-overflows q 1)
                               (== (MTQ_READER_SEM q) 0))
                           (begin (enqueue_int (Q q) 1 cell cell)
                                  (set! retval '#t)
                                  (notify-readers (Q q))))
          (enqueue_int (Q q) 1 cell cell))
     (return retval)))
 )

(define (enqueue-unique! q cmp obj . more-objs)
  (define (pick lis xs ins)
    (cond [(null? ins) xs]
          [(or (member (car ins) lis cmp) (member (car ins) xs cmp))
           (pick lis xs (cdr ins))]
          [else (pick lis (cons (car ins) xs) (cdr ins))]))
  (queue-op q (^(mt?)
                (let1 xs (pick (%qhead q) '() (cons obj more-objs))
                  (unless (null? xs)
                    (when (and mt? (%mtqueue-overflow? q (length xs)))
                      (error "queue is full:" q))
                    (let1 xs_ (reverse xs)
                      (%enqueue! q (length xs) xs_ (last-pair xs_))
                      (when mt? (%notify-readers q)))))))
  q)

(inline-stub
 ;; queue-push! - add item(s) to the head
 (define-cfn queue-push-int (q::Queue* cnt::u_int head tail) ::void
   (SCM_SET_CDR tail (Q_HEAD q))
   (set! (Q_HEAD q) head
         (Q_TAIL q) (Scm_LastPair tail))
   (when (>= (Q_LENGTH q) 0)
     (set! (Q_LENGTH q) (+ (Q_LENGTH q) cnt))))

 (define-cproc queue-push! (q::<queue> obj :rest more-objs)
   (let* ([objs (Scm_Cons obj more-objs)] [head] [tail] [cnt::u_int])
     (if (SCM_NULLP more-objs)
       (set! head objs tail objs cnt 1)
       (set! head (Scm_ReverseX objs)
             tail (Scm_LastPair head)
             cnt  (Scm_Length head)))
     (q-write-op queue-push-int q cnt head tail)
     (return (SCM_OBJ q))))

 (define-cproc queue-push/wait! (q::<mtqueue> obj :optional (timeout #f)
                                                            (timeout-val #f))
   (let* ([cell (SCM_LIST1 obj)] [retval (SCM_OBJ q)])
     (.if "defined(GAUCHE_HAS_THREADS)"
          (do-with-timeout q retval timeout timeout-val writerWait
                           (begin)
                           (?: (!= (MTQ_MAXLEN q) 0)
                               (mtq-overflows q 1)
                               (== (MTQ_READER_SEM q) 0))
                           (begin (queue_push_int (Q q) 1 cell cell)
                                  (notify-readers (Q q))))
          (queue_push_int (Q q) 1 cell cell))
     (return retval)))
 )

(define (queue-push-unique! q cmp obj . more-objs)
  (define (pick lis ins)
    (cond [(null? ins) lis]
          [(member (car ins) lis cmp) (pick lis (cdr ins))]
          [else (pick (cons (car ins) lis) (cdr ins))]))
  (queue-op q (^[mt?]
                (let* ([h (%qhead q)]
                       [xs (pick h (cons obj more-objs))])
                  (unless (eq? xs h)
                    (when (and mt? (%mtqueue-overflow? q (- (length xs) (length h))))
                      (error "queue is full" q))
                    (%queue-set-content! q xs #f)
                    (when mt? (%notify-readers q))))))
  q)

;; dequeue!
(inline-stub
 (define-cfn dequeue-int (q::Queue* result::ScmObj*) ::int
   (cond [(Q_EMPTY_P q) (return TRUE)]
         [else
          (let* ([h (Q_HEAD q)])
            (set! (* result) (SCM_CAR h)
                  (Q_HEAD q) (SCM_CDR h))
            (set! (SCM_CAR h) SCM_NIL
                  (SCM_CDR h) SCM_NIL) ; to be friendly to GC
            (when (>= (Q_LENGTH q) 0) (dec! (Q_LENGTH q)))
            (return FALSE))]))

 (define-cproc dequeue! (q::<queue> :optional fallback)
   (let* ([empty::int FALSE] [r SCM_UNDEFINED])
     (if (not (MTQP q))
       (set! empty (dequeue-int q (& r)))
       (with-mtq-light-lock q (set! empty (dequeue-int q (& r)))))
     (if empty
       (if (SCM_UNBOUNDP fallback)
         (Scm_Error "queue is empty: %S" q)
         (set! r fallback))
       (when (MTQP q) (notify-writers q)))
     (return r)))

 (define-cproc dequeue/wait! (q::<mtqueue> :optional (timeout #f)
                                                     (timeout-val #f))
   (let* ([retval SCM_UNDEFINED])
     (.if "defined(GAUCHE_HAS_THREADS)"
          (do-with-timeout q retval timeout timeout-val readerWait
                           (begin (post++ (MTQ_READER_SEM q))
                                  (notify-writers (Q q)))
                           (Q_EMPTY_P q)
                           (begin (pre-- (MTQ_READER_SEM q))
                                  (dequeue_int (Q q) (& retval))
                                  (notify-writers (Q q))))
          ;; no threads
          (dequeue_int (Q q) (& retval)))
     (return retval)))

 (define-cfn dequeue-all-int (q::Queue*)
   (let* ([lis (Q_HEAD q)])
     (set! (Q_LENGTH q) 0 (Q_HEAD q) SCM_NIL (Q_TAIL q) SCM_NIL)
     (return lis)))

 (define-cproc dequeue-all! (q::<queue>)
   (if (not (MTQP q))
     (return (dequeue-all-int q))
     (let* ([r])
       (with-mtq-light-lock q (set! r (dequeue-all-int q)))
       (notify-writers q)
       (return r))))
 )

(define queue-pop! dequeue!)
(define queue-pop/wait! dequeue/wait!)

(define (remove-from-queue! pred q)
  (rlet1 removed? #f
    (queue-op q (^[mt?]
                  (let loop ([rs '()] [xs (%qhead q)] [hit #f])
                    (cond [(null? xs)
                           (when hit
                             (set! removed? #t)
                             (when mt? (%notify-writers q))
                             (%queue-set-content! q (reverse! rs) #f))]
                          [(pred (car xs)) (loop rs (cdr xs) #t)]
                          [else (loop (cons (car xs) rs) (cdr xs) hit)]))))))

;; NB: Scheme48 has delete-from-queue!, whose argument order is
;; reversed from 'delete' in SRFI-1.   I leave it undefined here.
;;
;; (define (delete-from-queue! q item)  ;;Scheme48
;;   (remove-from-queue! (lambda (elt) (eq? item elt)) q))
