;;;
;;; libio.scm - builtin port and I/O procedures
;;;
;;;   Copyright (c) 2000-2013  Shiro Kawai  <shiro@acm.org>
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

(select-module gauche.internal)

(inline-stub
 (declcode (.include <gauche/vminsn.h>
                     <stdlib.h>
                     <fcntl.h>)))

;;;
;;; Ports
;;;

;;
;; Predicates
;;

(select-module scheme)
(define-cproc input-port? (obj)  ::<boolean> SCM_IPORTP)
(define-cproc output-port? (obj) ::<boolean> SCM_OPORTP)
(define-cproc port? (obj)        ::<boolean> SCM_PORTP)

(select-module gauche)
(define-cproc port-closed? (obj::<port>) ::<boolean> SCM_PORT_CLOSED_P)

;;
;; Preexisting ports
;;

(select-module scheme)

(define-cproc current-input-port (:optional newport)
  (cond [(SCM_IPORTP newport)
         (result (Scm_SetCurrentInputPort (SCM_PORT newport)))]
        [(not (SCM_UNBOUNDP newport))
         (Scm_TypeError "current-input-port" "input port" newport)
         (result SCM_UNDEFINED)]
        [else (result (SCM_OBJ SCM_CURIN))]))

(define-cproc current-output-port (:optional newport)
  (cond [(SCM_OPORTP newport)
         (result (Scm_SetCurrentOutputPort (SCM_PORT newport)))]
        [(not (SCM_UNBOUNDP newport))
         (Scm_TypeError "current-output-port" "output port" newport)
         (result SCM_UNDEFINED)]
        [else (result (SCM_OBJ SCM_CUROUT))]))

(select-module gauche)

(define-cproc current-error-port (:optional newport)
  (cond
   [(SCM_OPORTP newport) (result (Scm_SetCurrentErrorPort (SCM_PORT newport)))]
   [(not (SCM_UNBOUNDP newport))
    (Scm_TypeError "current-error-port" "output port" newport)
    (result SCM_UNDEFINED)]
   [else (result (SCM_OBJ SCM_CURERR))]))

(define-cproc standard-input-port (:optional (p::<input-port>? #f))
  (result (?: p (Scm_SetStdin p) (Scm_Stdin))))
(define-cproc standard-output-port (:optional (p::<output-port>? #f))
  (result (?: p (Scm_SetStdout p) (Scm_Stdout))))
(define-cproc standard-error-port (:optional (p::<output-port>? #f))
  (result (?: p (Scm_SetStderr p) (Scm_Stderr))))


;;
;; Query and low-level properties
;;

(select-module gauche)

(define-cproc port-name (port::<port>) Scm_PortName)
(define-cproc port-current-line (port::<port>) ::<fixnum> Scm_PortLine)

(define-cproc port-file-number (port::<port>)
  (let* ([i::int (Scm_PortFileNo port)])
    (result (?: (< i 0) SCM_FALSE (Scm_MakeInteger i)))))
(define-cproc port-fd-dup! (dst::<port> src::<port>) ::<void> Scm_PortFdDup)

(define-cproc port-type (port::<port>)
  (case (SCM_PORT_TYPE port)
    [(SCM_PORT_FILE) (result 'file)]
    [(SCM_PORT_PROC) (result 'proc)]
    [(SCM_PORT_OSTR SCM_PORT_ISTR) (result 'string)]
    [else (result '#f)]))

(define-cproc port-buffering (port::<port>)
  (setter (port::<port> mode) ::<void>
          (unless (== (SCM_PORT_TYPE port) SCM_PORT_FILE)
            (Scm_Error "can't set buffering mode to non-buffered port: %S"port))
          (set! (ref (-> port src) buf mode)
                (Scm_BufferingMode mode (-> port direction) -1)))
  Scm_GetBufferingMode)

(define-cproc port-case-fold-set! (port::<port> flag::<boolean>) ::<void>
  (if flag
    (logior= (SCM_PORT_FLAGS port) SCM_PORT_CASE_FOLD)
    (logand= (SCM_PORT_FLAGS port) (lognot SCM_PORT_CASE_FOLD))))

;;
;; Open and close
;;

(select-module scheme)
(define-cproc close-input-port (port::<port>)  ::<void> Scm_ClosePort)
(define-cproc close-output-port (port::<port>) ::<void> Scm_ClosePort)

(select-module gauche.internal)
(inline-stub
 ;; NB: On MinGW, if we try to create a file and a directory with the
 ;; same name exists, open(2) throws EACCES.  Weird, eh?  We don't want
 ;; to catch EACCES on other platforms, hence this dirty trick.
 (if "defined(GAUCHE_WINDOWS)"
   "#define DIRECTORY_GETS_IN_WAY(x) ((x)==EACCES)"
   "#define DIRECTORY_GETS_IN_WAY(x) FALSE")

 ;; Some cise macros for common idioms
 (define-cise-expr %open/allow-noexist?
   [(_ if-does-not-exist-is-false)
    `(and ,if-does-not-exist-is-false
          (or (== errno ENOENT)
              (== errno ENODEV)
              (== errno ENXIO)
              (== errno ENOTDIR)))])

 (define-cise-expr %open/allow-exist?
   [(_ if-exists-is-false)
    `(and ,if-exists-is-false
          (or (== errno EEXIST)
              (== errno ENOTDIR)
              (DIRECTORY_GETS_IN_WAY errno)))])
 )

;; Primitive open routine.  The Scheme wrapper handles other keyword args.
(define-cproc %open-input-file (path::<string>
                                :key (if-does-not-exist :error)
                                (buffering #f)
                                (element-type :character))
  (let* ([ignerr::int FALSE])
    (cond [(SCM_FALSEP if-does-not-exist) (set! ignerr TRUE)]
          [(not (SCM_EQ if-does-not-exist ':error))
           (Scm_TypeError ":if-does-not-exist" ":error or #f"
                          if-does-not-exist)])
    (let* ([bufmode::int (Scm_BufferingMode buffering SCM_PORT_INPUT
                                            SCM_PORT_BUFFER_FULL)]
           [o (Scm_OpenFilePort (Scm_GetStringConst path)
                                O_RDONLY bufmode 0)])
      (when (and (SCM_FALSEP o) (not (%open/allow-noexist? ignerr)))
        (Scm_SysError "couldn't open input file: %S" path))
      (result o))))

;; Primitive open routine.  The Scheme wrapper handles other keyword args
(define-cproc %open-output-file (path::<string>
                                 :key (if-exists :supersede)
                                 (if-does-not-exist :create)
                                 (mode::<fixnum> #o666)
                                 (buffering #f)
                                 (element-type :character))
  (let* ([ignerr-noexist::int FALSE]
         [ignerr-exist::int FALSE]
         [flags::int O_WRONLY])
    ;; check if-exists flag
    (cond
     [(SCM_EQ if-exists ':append) (logior= flags O_APPEND)]
     [(SCM_EQ if-exists ':error)
      (logior= flags O_EXCL)
      (when (SCM_EQ if-does-not-exist ':error)
        (Scm_Error "bad flag combination: :if-exists and :if-does-not-exist can't be :error the same time."))]
     [(SCM_EQ if-exists ':supersede) (logior= flags O_TRUNC)]
     [(SCM_EQ if-exists ':overwrite)] ; no need to add flags
     [(SCM_FALSEP if-exists) (logior= flags O_EXCL) (set! ignerr-exist TRUE)]
     [else
      (Scm_TypeError ":if-exists" ":supersede, :overwrite, :append, :error or #f" if-exists)])
    ;; check if-does-not-exist flag
    (cond
     [(SCM_EQ if-does-not-exist ':create) (logior= flags O_CREAT)]
     [(SCM_FALSEP if-does-not-exist) (set! ignerr-noexist TRUE)]
     [(SCM_EQ if-does-not-exist ':error)] ; no need to add flags
     [else (Scm_TypeError ":if-does-not-exist" ":error, :create or #f"
                          if-does-not-exist)])
    (let* ([bufmode::int
            (Scm_BufferingMode buffering SCM_PORT_OUTPUT SCM_PORT_BUFFER_FULL)]
           [o (Scm_OpenFilePort (Scm_GetStringConst path)
                                flags bufmode mode)])
      (when (and (SCM_FALSEP o)
                 (not (%open/allow-noexist? ignerr-noexist))
                 (not (%open/allow-exist? ignerr-exist)))
        (Scm_Error "couldn't open output file: %S" path))
      (result o))))

;; Open port from fd
(select-module gauche)

(define-cproc open-input-fd-port (fd::<fixnum>
                                  :key (buffering #f)
                                  (owner?::<boolean> #f)
                                  (name #f))
  (let* ([bufmode::int (Scm_BufferingMode buffering SCM_PORT_INPUT
                                          SCM_PORT_BUFFER_FULL)])
    (when (< fd 0) (Scm_Error "bad file descriptor: %d" fd))
    (result (Scm_MakePortWithFd name SCM_PORT_INPUT fd bufmode ownerP))))

(define-cproc open-output-fd-port (fd::<fixnum>
                                   :key (buffering #f)
                                   (owner?::<boolean> #f)
                                   (name #f))
  (let* ([bufmode::int (Scm_BufferingMode buffering SCM_PORT_OUTPUT
                                          SCM_PORT_BUFFER_FULL)])
    (when (< fd 0) (Scm_Error "bad file descriptor: %d" fd))
    (result (Scm_MakePortWithFd name SCM_PORT_OUTPUT fd bufmode owner?))))

;; Buffered port
(select-module gauche)
(inline-stub
 ;; Buffered port
 ;; NB: the interface may be changed soon!!
 (define-cfn bufport-closer (p::ScmPort*) ::void :static
   (when (== (SCM_PORT_DIR p) SCM_PORT_OUTPUT)
     (let* ((scmflusher (SCM_OBJ (ref (-> p src) buf data)))
            (siz::int (cast int (- (ref (-> p src) buf current)
                                   (ref (-> p src) buf buffer)))))
       (when (> siz 0)
         (Scm_ApplyRec1 scmflusher
                        (Scm_MakeString (ref (-> p src) buf buffer) siz siz
                                        (logior SCM_STRING_INCOMPLETE
                                                SCM_STRING_COPYING))))
       (Scm_ApplyRec1 scmflusher SCM_FALSE))))

 (define-cfn bufport-filler (p::ScmPort* cnt::int) ::int :static
   (let* ([scmfiller (SCM_OBJ (ref (-> p src) buf data))]
          [r (Scm_ApplyRec1 scmfiller (Scm_MakeInteger cnt))])
     (cond [(or (SCM_EOFP r) (SCM_FALSEP r)) (return 0)]
           [(not (SCM_STRINGP r))
            (Scm_Error "buffered port callback procedure returned non-string: %S" r)])
     (let* ([b::(const ScmStringBody*) (SCM_STRING_BODY r)]
            [siz::int (SCM_STRING_BODY_SIZE b)])
       (when (> siz cnt) (set! siz cnt)) ; for safety
       (memcpy (ref (-> p src) buf end) (SCM_STRING_BODY_START b) siz)
       (return (SCM_STRING_BODY_SIZE b)))))
 )

(define-cproc open-input-buffered-port
  (filler::<procedure> buffer-size::<fixnum>)
  (let* ([bufrec::ScmPortBuffer])
    (set! (ref bufrec size)    buffer-size
          (ref bufrec buffer)  NULL
          (ref bufrec mode)    SCM_PORT_BUFFER_FULL
          (ref bufrec filler)  bufport-filler
          (ref bufrec flusher) NULL
          (ref bufrec closer)  bufport-closer
          (ref bufrec ready)   NULL
          (ref bufrec filenum) NULL
          (ref bufrec data)    (cast void* filler))
    (result (Scm_MakeBufferedPort SCM_CLASS_PORT SCM_FALSE SCM_PORT_INPUT TRUE (& bufrec)))))

(inline-stub
 (define-cfn bufport-flusher (p::ScmPort* cnt::int forcep::int) ::int :static
   (let* ([scmflusher (SCM_OBJ (ref (-> p src) buf data))]
          [s (Scm_MakeString (ref (-> p src) buf buffer) cnt cnt
                             (logior SCM_STRING_INCOMPLETE SCM_STRING_COPYING))])
     (Scm_ApplyRec1 scmflusher s)
     (return cnt)))
 )

(define-cproc open-output-buffered-port
  (flusher::<procedure> buffer-size::<fixnum>)
  (let* ([bufrec::ScmPortBuffer])
    (set! (ref bufrec size)    buffer-size
          (ref bufrec buffer)  NULL
          (ref bufrec mode)    SCM_PORT_BUFFER_FULL
          (ref bufrec filler)  NULL
          (ref bufrec flusher) bufport-flusher
          (ref bufrec closer)  bufport-closer
          (ref bufrec ready)   NULL
          (ref bufrec filenum) NULL
          (ref bufrec data)    (cast void* flusher))
    (result (Scm_MakeBufferedPort SCM_CLASS_PORT SCM_FALSE SCM_PORT_OUTPUT
                                  TRUE (& bufrec)))))

;; String ports (srfi-6)
(select-module gauche)

(define-cproc open-input-string (string::<string> :key (private?::<boolean> #f))
  Scm_MakeInputStringPort)

(define-cproc open-output-string (:key (private?::<boolean> #f))
  Scm_MakeOutputStringPort)

(define-cproc get-output-string (oport::<output-port>) ;SRFI-6
  (result (Scm_GetOutputString oport 0)))

(define-cproc get-output-byte-string (oport::<output-port>)
  (result (Scm_GetOutputString oport SCM_STRING_INCOMPLETE)))

(define-cproc get-remaining-input-string (iport::<input-port>)
  (result (Scm_GetRemainingInputString iport 0)))

;; Coding aware port
(select-module gauche)

(define-cproc open-coding-aware-port (iport::<input-port>)
  Scm_MakeCodingAwarePort)

;;
;; Miscellaneous
;;

(select-module gauche)
(inline-stub
 (define-enum SEEK_SET)
 (define-enum SEEK_CUR)
 (define-enum SEEK_END)
 )

(define-cproc port-seek
  (port::<port> offset::<integer>
                :optional (whence::<fixnum> (c "SCM_MAKE_INT(SEEK_SET)")))
  Scm_PortSeek)

(define-cproc with-port-locking (port::<port> proc) Scm_VMWithPortLocking)

;;;
;;; Input
;;;

(select-module scheme)

(define-cproc read (:optional (port::<input-port> (current-input-port)))
  (result (Scm_Read (SCM_OBJ port))))

(define-cproc read-char (:optional (port::<input-port> (current-input-port)))
  (inliner READ-CHAR)
  (let* ([ch::int])
    (SCM_GETC ch port)
    (result (?: (== ch EOF) SCM_EOF (SCM_MAKE_CHAR ch)))))

(define-cproc peek-char (:optional (port::<input-port> (current-input-port)))
  (inliner PEEK-CHAR)
  (let* ([ch::ScmChar (Scm_Peekc port)])
    (result (?: (== ch SCM_CHAR_INVALID) SCM_EOF (SCM_MAKE_CHAR ch)))))

(define-cproc eof-object? (obj) ::<boolean> :fast-flonum
  (inliner EOFP) SCM_EOFP)

(define-cproc char-ready? (:optional (port::<input-port> (current-input-port)))
  ::<boolean> Scm_CharReady)


(select-module gauche)

(define-cproc eof-object () (result SCM_EOF)) ;R6RS

(define-cproc byte-ready? (port::<input-port>) ::<boolean> Scm_ByteReady)

(define-cproc read-byte (:optional (port::<input-port> (current-input-port)))
  (let* ([b::int])
    (SCM_GETB b port)
    (result (?: (< b 0) SCM_EOF (SCM_MAKE_INT b)))))

(define-cproc peek-byte (:optional (port::<input-port> (current-input-port)))
  (let* ([b::int (Scm_Peekb port)])
    (result (?: (< b 0) SCM_EOF (SCM_MAKE_INT b)))))

(define-cproc read-line (:optional (port::<input-port> (current-input-port))
                                   (allowbytestr #f))
  (let* ([r (Scm_ReadLine port)])
    (when (and (SCM_FALSEP allowbytestr)
               (SCM_STRINGP r)
               (SCM_STRING_INCOMPLETE_P r))
      (Scm_ReadError port "read-line: encountered illegal byte sequence: %S" r))
    (result r)))

(define-cproc read-block (bytes::<fixnum>
                          :optional (port::<input-port> (current-input-port)))
  (when (< bytes 0)
    (Scm_Error "bytes must be non-negative integer: %d" bytes))
  (if (== bytes 0)
    (result (Scm_MakeString "" 0 0 0))
    (let* ([buf::char* (SCM_NEW_ATOMIC2 (C: char*) (+ bytes 1))]
           [nread::int (Scm_Getz buf bytes port)])
      (cond [(<= nread 0) (result SCM_EOF)]
            [else
             (SCM_ASSERT (<= nread bytes))
             (set! (aref buf nread) #\x00)
             (result (Scm_MakeString buf nread nread SCM_STRING_INCOMPLETE))]
            ))))

(define-cproc read-list (closer::<char>
                         :optional (port (current-input-port)))
  (result (Scm_ReadList port closer)))

(define-cproc port->byte-string (port::<input-port>)
  (let* ([ds::ScmDString] [buf::(.array char (1024))])
    (Scm_DStringInit (& ds))
    (loop (let* ([nbytes::int (Scm_Getz buf 1024 port)])
            (when (<= nbytes 0) (break))
            (Scm_DStringPutz (& ds) buf nbytes)))
    (result (Scm_DStringGet (& ds) SCM_STRING_INCOMPLETE))))

;; Read time constructor (srfi-10)
(select-module gauche)

(define-cproc define-reader-ctor (symbol proc :optional (finisher #f))
  (result (Scm_DefineReaderCtor symbol proc finisher SCM_FALSE)))

(define-cproc %get-reader-ctor (symbol)
  (result (Scm_GetReaderCtor symbol SCM_FALSE)))

(define-cproc define-reader-directive (symbol proc)
  Scm_DefineReaderDirective)

(inline-stub
 (define-type <read-context> "ScmReadContext*" "read context"
   "SCM_READ_CONTEXT_P" "SCM_READ_CONTEXT" "")

 (define-type <read-reference> "ScmReadReference*" "read reference"
   "SCM_READ_REFERENCE_P" "SCM_READ_REFERENCE" "")
 )

(define-cproc read-reference? (obj) ::<boolean> SCM_READ_REFERENCE_P)

(define-cproc read-reference-has-value? (ref::<read-reference>)
  ::<boolean> (result (not (SCM_UNBOUNDP (-> ref value)))))

(define-cproc read-reference-value (ref::<read-reference>)
  (when (SCM_UNBOUNDP (-> ref value))
    (Scm_Error "read reference hasn't been resolved"))
  (result (-> ref value)))

;; srfi-38
(define-in-module gauche read-with-shared-structure read)
(define-in-module gauche read/ss read)

;; srfi-105
(select-module gauche.internal)
(define (%xform-cexpr cex)
  (define (simple-cexpr? op tail)
    (cond [(not (pair? tail)) #f]
          [(null? (cdr tail)) #t]
          [(and (pair? (cdr tail)) (eq? op (cadr tail)))
           (simple-cexpr? op (cddr tail))]
          [else #f]))
  (define (gather-args args) ; args is a list with even elements
    (if (null? (cdr args))
      args
      (cons (car args) (gather-args (cddr args)))))
  (cond [(not (pair? cex)) cex]         ;includes {} -> ()
        [(null? (cdr cex)) (car cex)]   ; {e} -> e
        [(and (pair? (cdr cex)) (null? (cddr cex))) cex] ; {x y} -> (x y)
        [(and (pair? (cdr cex)) (pair? (cddr cex)) (symbol? (cadr cex))
              (simple-cexpr? (cadr cex) (cddr cex)))
         (cons* (cadr cex) (car cex) (gather-args (cddr cex)))]
        [else (cons 'nfx cex)]))

;;;
;;; Output
;;;

(select-module scheme)

(define-cproc write (obj :optional (port::<output-port> (current-output-port)))
  ::<void> (Scm_Write obj (SCM_OBJ port) SCM_WRITE_WRITE))

(define-cproc display
  (obj :optional (port::<output-port> (current-output-port)))
  ::<void> (Scm_Write obj (SCM_OBJ port) SCM_WRITE_DISPLAY))

(define-cproc newline (:optional (port::<output-port> (current-output-port)))
  ::<void> (SCM_PUTC #\newline port))

(define-cproc write-char
  (ch::<char> :optional (port::<output-port> (current-output-port)))
  ::<void> (inliner WRITE-CHAR) (SCM_PUTC ch port))


(select-module gauche)

(define-cproc write-byte (byte::<fixnum>
                          :optional (port::<output-port> (current-output-port)))
  ::<int>
  (when (or (< byte 0) (> byte 255))
    (Scm_Error "argument out of range: %d" byte))
  (SCM_PUTB byte port)
  (result 1))

(define-cproc write-limited (obj limit::<fixnum>
                                 :optional (port (current-output-port)))
  ::<int> (result (Scm_WriteLimited obj port SCM_WRITE_WRITE limit)))

(define-cproc write* (obj :optional (port (current-output-port)))
  ::<int> (result (Scm_WriteCircular obj port SCM_WRITE_WRITE 0)))

(define-cproc flush (:optional (oport::<output-port> (current-output-port)))
  ::<void> Scm_Flush)

(define-cproc flush-all-ports () ::<void> (Scm_FlushAllPorts FALSE))


(select-module gauche.internal)

(define-cproc %format
  (port::<output-port> fmt::<string> args shared::<boolean>) ::<void>
  Scm_Format)

(select-module gauche.internal)

;; srfi-38
(define-in-module gauche (write-with-shared-structure obj :optional (port (current-output-port)))
  (write* obj port))
(define-in-module gauche write/ss write-with-shared-structure)

(define-in-module gauche (print . args) (for-each display args) (newline))

(define (%format-common port fmt args shared?)
  (cond [(eqv? port #f)
         (let ((out (open-output-string :private? #t)))
           (%format out fmt args shared?)
           (get-output-string out))]
        [(eqv? port #t)
         (%format (current-output-port) fmt args shared?)]
        [else (%format port fmt args shared?)]))

(define-in-module gauche (format fmt . args)
  (if (string? fmt)
    (%format-common #f fmt args #f) ;; srfi-28 compatible behavior
    (%format-common fmt (car args) (cdr args) #f)))

(define-in-module gauche (format/ss fmt . args)
  (if (string? fmt)
    (%format-common #f fmt args #t) ;; srfi-28 compatible behavior
    (%format-common fmt (car args) (cdr args) #t)))

;;;
;;; With-something
;;;

(select-module gauche.internal)

;; R5RS open-{input|output}-file can be hooked by conversion port.
;; %open-{input|output}-file/conv are autoloaded.

(define-in-module scheme (open-input-file filename . args)
  (let1 e (get-keyword :encoding args #f)
    (cond [(eq? e #f) (apply %open-input-file filename args)]
          [(eq? e #t)                   ;using coding-aware port
           (and-let* ([p (apply %open-input-file filename
                                (delete-keyword :encoding args))])
             (open-coding-aware-port p))]
          [else (apply %open-input-file/conv filename args)])))

(define-in-module scheme (open-output-file filename . args)
  (if (get-keyword :encoding args #f)
    (apply %open-output-file/conv filename args)
    (apply %open-output-file filename args)))

;; File ports.

(define-in-module scheme (call-with-input-file filename proc . flags)
  (let1 port (apply open-input-file filename flags)
    (unwind-protect (proc port)
      (when port (close-input-port port)))))

(define-in-module scheme (call-with-output-file filename proc . flags)
  (let1 port (apply open-output-file filename flags)
    (unwind-protect (proc port)
      (when port (close-output-port port)))))

(define-in-module scheme (with-input-from-file filename thunk . flags)
  (let1 port (apply open-input-file filename flags)
    (and port
         (unwind-protect (with-input-from-port port thunk)
           (close-input-port port)))))

(define-in-module scheme (with-output-to-file filename thunk . flags)
  (let1 port (apply open-output-file filename flags)
    (and port
         (unwind-protect (with-output-to-port port thunk)
           (close-output-port port)))))

;; String ports
(define-in-module gauche (with-output-to-string thunk)
  (let1 out (open-output-string)
    (with-output-to-port out thunk)
    (get-output-string out)))

(define-in-module gauche (with-input-from-string str thunk)
  (with-input-from-port (open-input-string str) thunk))

(define-in-module gauche (call-with-output-string proc)
  (let1 out (open-output-string)
    (proc out)
    (get-output-string out)))

(define-in-module gauche (call-with-input-string str proc)
  (proc (open-input-string str)))

(define-in-module gauche (call-with-string-io str proc)
  (let ((out (open-output-string))
        (in  (open-input-string str)))
    (proc in out)
    (get-output-string out)))

(define-in-module gauche (with-string-io str thunk)
  (with-output-to-string (cut with-input-from-string str thunk)))

(define-in-module gauche (write-to-string obj :optional (writer write))
  (with-output-to-string (cut writer obj)))

(define-in-module gauche (read-from-string string . args)
  (with-input-from-string
      (if (null? args) string (apply %maybe-substring string args))
    read))

;; with-port

(define-syntax %with-ports
  (syntax-rules ()
    [(_ "tmp" (tmp ...) () (port ...) (param ...) thunk)
     (let ((tmp #f) ...)
       (dynamic-wind
           (^[] (when port (set! tmp (param port))) ...)
           thunk
           (^[] (when tmp (param tmp)) ...)))]
    [(_ "tmp" tmps (port . more) ports params thunk)
     (%with-ports "tmp" (tmp . tmps) more ports params thunk)]
    [(_ ((param port) ...) thunk)
     (%with-ports "tmp" () (port ...) (port ...) (param ...) thunk)]))

(define-in-module gauche (with-input-from-port port thunk)
  (%with-ports ((current-input-port port)) thunk))

(define-in-module gauche (with-output-to-port port thunk)
  (%with-ports ((current-output-port port)) thunk))

(define-in-module gauche (with-error-to-port port thunk)
  (%with-ports ((current-error-port port)) thunk))

(define-in-module gauche (with-ports iport oport eport thunk)
  (%with-ports ((current-input-port iport)
                (current-output-port oport)
                (current-error-port eport))
               thunk))

;;;
;;; #! directives
;;;

(define-reader-directive 'r6rs
  (^[sym port ctx]
    (warn "Reading R6RS source file.  Note that Gauche is not R6RS compliant.")
    ;; TODO: we could do some adjustments, such as switching the semantics of
    ;; '#,' from srfi-10 to r6rs 'unsyntax'.
    (values)))

(define-reader-directive 'fold-case
  (^[sym port ctx]
    (port-case-fold-set! port #t)
    (values)))

(define-reader-directive 'no-fold-case
  (^[sym port ctx]
    (port-case-fold-set! port #f)
    (values)))

;; HIGHLY EXPERIMENTAL
(define-reader-directive 'c-expr
  (^[sym port ctx]
    ((with-module gauche.internal vm-compiler-flag-set!)
     SCM_COMPILE_ENABLE_CEXPR)
    (values)))


