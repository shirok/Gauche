;;;
;;; logger.scm - simple use-level logging
;;;
;;;   Copyright (c) 2000-2019  Shiro Kawai  <shiro@acm.org>
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

#!no-fold-case

(define-module gauche.logger
  (use srfi-1)
  (use srfi-13)
  (use gauche.fcntl)
  (use gauche.parameter)
  (export <log-drain>
          log-open
          log-format
          log-default-drain)
  )
(select-module gauche.logger)

(autoload gauche.syslog sys-openlog sys-syslog LOG_PID LOG_INFO LOG_USER)
(autoload file.util file-mtime<?)

;; <log-drain> class
(define-class <log-drain> ()
  ((path   :init-keyword :path :initform #f)
   (program-name :init-keyword :program-name
                 :initform (let1 argv (command-line)
                             (if (pair? argv)
                               (sys-basename (car argv))
                               "")))
   (retry  :init-keyword :retry :initform 5)
   (prefix :init-keyword :prefix :initform "~T ~P[~$]: ")
   (lock-policy :init-keyword :lock-policy :initform 'tbd)
   ;; The following parameters are used for syslog.
   ;; The default values will be set when log-open is called with 'syslog.
   (syslog-option   :init-keyword :syslog-option)
   (syslog-facility :init-keyword :syslog-facility)
   (syslog-priority :init-keyword :syslog-priority)
   ))

(define log-default-drain
  (make-parameter (make <log-drain>)))

(define-method initialize ((self <log-drain>) initargs)
  (next-method)
  ;; if 'syslog is specified, we need some setup...
  (when (eq? (slot-ref self 'path) 'syslog)
    (unless (slot-bound? self 'syslog-option)
      (set! (slot-ref self 'syslog-option) LOG_PID))
    (unless (slot-bound? self 'syslog-facility)
      (set! (slot-ref self 'syslog-facility) LOG_USER))
    (unless (slot-bound? self 'syslog-priority)
      (set! (slot-ref self 'syslog-priority) LOG_INFO))
    (set! (slot-ref self 'prefix) #f)
    (sys-openlog (slot-ref self 'program-name)
                 (slot-ref self 'syslog-option)
                 (slot-ref self 'syslog-facility))
    ))

;; prefix spec
;;   ~T   current time as "MMM DD hh:mm:ss" where MMM is abbrev month.
;;   ~Y   4-digit year
;;   ~P   program name
;;   ~$   pid
;;   ~U   user name
;;   ~H   host name

(define (log-format-prefix drain pstr)
  (with-string-io pstr
    (^[]
      (let loop ([c (read-char)])
        (cond [(eof-object? c)]
              [(char=? c #\~ )
               (let1 c1 (read-char)
                 (cond [(eof-object? c1) (display c)]
                       [(char=? c1 #\P)
                        (display (slot-ref drain 'program-name))
                        (loop (read-char))]
                       [(char=? c1 #\T)
                        ;; NB: I used to use (sys-strftime "%b %e %H:%M:%S"), but
                        ;; not all implementations supports %e, which is like
                        ;; %d (day of the month) but suppressing the leading
                        ;; zeros.  It is necessary to match syslog date format.
                        (let1 t (sys-localtime (sys-time))
                          (format #t "~a ~2d ~2,'0d:~2,'0d:~2,'0d"
                                  (sys-strftime "%b" t)
                                  (slot-ref t 'mday)
                                  (slot-ref t 'hour)
                                  (slot-ref t 'min)
                                  (slot-ref t 'sec)))
                        (loop (read-char))]
                       [(char=? c1 #\Y)
                        (display (sys-strftime "%Y" (sys-localtime (sys-time))))
                        (loop (read-char))]
                       [(char=? c1 #\$)
                        (display (sys-getpid))
                        (loop (read-char))]
                       [(char=? c1 #\U)
                        (display (or (sys-uid->user-name (sys-geteuid))
                                     (sys-geteuid)))
                        (loop (read-char))]
                       [(char=? c1 #\H)
                        (display (sys-gethostname))
                        (loop (read-char))]
                       [else
                        (display c) (display c1) (loop (read-char))]))]
              [else (display c) (loop (read-char))])
        ))))

(define (log-get-prefix drain)
  (let1 pstr (slot-ref drain 'prefix)
    (cond [(string? pstr) (log-format-prefix drain pstr)]
          [(not pstr) ""]
          [else (pstr drain)])))

;; File lock handling
;;   Only called if the drain is a file and successfully opened.

(define-constant FILE_LOCK_TIMEOUT 600)

;; Generally we prefer fcntl lock, but it may not available on some
;; occasions; some versions of some OS don't support it (OSX before 10.3),
;; or the file may be on an NFS mounded filesystem.  We should determine
;; it at runtime.
;; (For the time being, we try 'fcntl' first and use 'file' as a fallback.
;; In future this kind of feature should be in separate module, such as
;; file.lock.)
(define (determine-lock-policy drain port)
  (set! (slot-ref drain 'lock-policy)
        (cond-expand
         [gauche.sys.fcntl
          (guard (e [(<system-error> e) 'file])
            (let ([lk (make <sys-flock> :type F_WRLCK :whence 0)]
                  [un (make <sys-flock> :type F_UNLCK :whence 0)])
              (and (sys-fcntl port F_SETLK lk)
                   (sys-fcntl port F_SETLK un))
              'fcntl))]
         [else 'file]))
  drain)

(define (lock-data drain port)
  (case (slot-ref drain 'lock-policy)
    [(fcntl) (make <sys-flock> :type F_WRLCK :whence 0)]
    [(file)  (string-append (slot-ref drain 'path) ".lock")]
    [(tbd)   (lock-data (determine-lock-policy drain port) port)]
    [else    #t]))

(define (lock-file drain port data)
  (case (slot-ref drain 'lock-policy)
    [(fcntl) (sys-fcntl port F_SETLKW data)]
    [(file)
     (let loop ([retry 0]
                [o (open-output-file data :if-exists #f)])
       (cond [o (close-output-port o) #t]
             [(> retry (slot-ref drain 'retry))
              (errorf "couldn't obtain lock with ~a (retry limit reached)"
                      data)]
             [(file-mtime<? data (- (sys-time) FILE_LOCK_TIMEOUT))
              ;; maybe the lock file is stale.
              (sys-unlink data)
              (loop 0 (open-output-file data :if-exists #f))]
             [else
              (sys-sleep 1)
              (loop (+ retry 1) (open-output-file data :if-exists #f))]))]
    [(tbd)
     (lock-file (determine-lock-policy drain port) port data)]
    [else #t]))

(define (unlock-file drain port data)
  (case (slot-ref drain 'lock-policy)
    [(fcntl)
     (slot-set! data 'type F_UNLCK)
     (sys-fcntl port F_SETLK data)]
    [(file)
     (sys-unlink data)]
    [(tbd)
     (unlock-file (determine-lock-policy drain port) port data)]
    [else #t]))

;; Write log
(define (with-log-output drain proc)
  (let1 path (slot-ref drain 'path)
    (cond [(string? path)
           (let* ([p (open-output-file path :if-exists :append)]
                  [l (lock-data drain p)])
             (dynamic-wind
              (^[] (lock-file drain p l))
              (^[] (proc p))
              (^[] (unlock-file drain p l) (close-output-port p))))]
          [(or (eq? path #t) (eq? path 'current-error))
           (proc (current-error-port))]
          [(eq? path 'current-output)
           (proc (current-output-port))]
          [(eq? path #f)
           (call-with-output-string proc)]
          [(eq? path 'syslog)
           (sys-syslog (logior (slot-ref drain 'syslog-facility)
                               (slot-ref drain 'syslog-priority))
                       (call-with-output-string proc))]
          [(eq? path 'ignore) #f]
          [else (error "invalid path value in log drain" path)])))

;; External APIs
;; log-format "fmtstr" arg ...
;; log-format drain "fmtstr" arg ...

(define-method log-format ((fmtstr <string>) . args)
  (apply log-format (log-default-drain) fmtstr args))

(define-method log-format ((drain <log-drain>) fmt . args)
  (unless (eq? (slot-ref drain 'path) 'ignore)
    (let* ([prefix (log-get-prefix drain)]
           [str ($ string-concatenate
                   $ fold-right (^[data rest]
                                  (if (and (null? rest) (string-null? data))
                                    '()          ;ignore trailing newlines
                                    (list* prefix data "\n" rest)))
                   '()
                   $ string-split (apply format #f fmt args) #\newline)])
      (with-log-output drain (^p (display str p))))))

;; log-open path &keyword :program-name :prefix

(define (log-open path . args)
  (log-default-drain (apply make <log-drain> :path path args)))

