;;;
;;; logger.scm - simple use-level logging
;;;
;;;  Copyright(C) 2000-2002 by Shiro Kawai (shiro@acm.org)
;;;
;;;  Permission to use, copy, modify, distribute this software and
;;;  accompanying documentation for any purpose is hereby granted,
;;;  provided that existing copyright notices are retained in all
;;;  copies and that this notice is included verbatim in all
;;;  distributions.
;;;  This software is provided as is, without express or implied
;;;  warranty.  In no circumstances the author(s) shall be liable
;;;  for any damages arising out of the use of this software.
;;;
;;;  $Id: logger.scm,v 1.5 2002-12-02 02:17:11 shirok Exp $
;;;

(define-module gauche.logger
  (use srfi-1)
  (use srfi-13)
  (use gauche.fcntl)
  (export <log-drain>
          log-open
          log-format)
  )
(select-module gauche.logger)

;; delay loading some modules until needed
(autoload gauche.syslog sys-openlog sys-syslog LOG_PID LOG_INFO LOG_USER)
(autoload file.util file-mtime<?)

;; Kludge! determine the default lock policy heuristically.
;; AFAIK, MacOSX doesn't support fcntl lock.
(define (default-lock-policy)
  (if (#/apple-darwin/ (gauche-architecture)) 'file 'fcntl))

;; <log-drain> class
(define-class <log-drain> ()
  ((path   :init-keyword :path :initform #f)
   (program-name :init-keyword :program-name
                 :initform  (sys-basename (with-module user *program-name*)))
   (retry  :init-keyword :retry :initform 5)
   (prefix :init-keyword :prefix :initform "~T ~P[~$]: ")
   (lock-policy :init-keyword :lock-policy :initform (default-lock-policy))
   ;; The following parameters are used for syslog.
   ;; The default values will be set when log-open is called with 'syslog.
   (syslog-option   :init-keyword :syslog-option)
   (syslog-facility :init-keyword :syslog-facility)
   (syslog-priority :init-keyword :syslog-priority)
   ))

(define *default-log-drain* (make <log-drain>))

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
    (lambda ()
      (let loop ((c (read-char)))
        (cond ((eof-object? c))
              ((char=? c #\~ )
               (let ((c1 (read-char)))
                 (cond ((eof-object? c1) (display c))
                       ((char=? c1 #\P)
                        (display (slot-ref drain 'program-name))
                        (loop (read-char)))
                       ((char=? c1 #\T)
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
                        (loop (read-char)))
                       ((char=? c1 #\Y)
                        (display (sys-strftime "%Y" (sys-localtime (sys-time))))
                        (loop (read-char)))
                       ((char=? c1 #\$)
                        (display (sys-getpid))
                        (loop (read-char)))
                       ((char=? c1 #\U)
                        (display (or (sys-uid->user-name (sys-geteuid))
                                     (sys-geteuid)))
                        (loop (read-char)))
                       ((char=? c1 #\H)
                        (display (sys-gethostname))
                        (loop (read-char)))
                       (else
                        (display c) (display c1) (loop (read-char))))))
              (else (display c) (loop (read-char))))
        ))))

(define (log-get-prefix drain)
  (let ((pstr (slot-ref drain 'prefix)))
    (cond ((string? pstr) (log-format-prefix drain pstr))
          ((not pstr) "")
          (else (pstr drain)))))

;; File lock handling
;;   Only called if the drain is file and successfully opened.

(define-constant FILE_LOCK_TIMEOUT 600)

(define (lock-data drain port)
  (case (slot-ref drain 'lock-policy)
    ((fcntl) (make <sys-flock> :type |F_WRLCK| :whence 0))
    ((file)  (string-append (slot-ref drain 'path) ".lock"))
    (else    #t)))

(define (lock-file drain port data)
  (case (slot-ref drain 'lock-policy)
    ((fcntl) (sys-fcntl port |F_SETLKW| data))
    ((file)
     (let loop ((retry 0)
                (o (open-output-file data :if-exists #f)))
       (cond (o (close-output-port o) #t)
             ((> retry (slot-ref drain 'retry))
              (errorf "couldn't obtain lock with ~a (retry limit reached)"
                      data))
             ((file-mtime<? data (- (sys-time) FILE_LOCK_TIMEOUT))
              ;; maybe the lock file is stale.
              (sys-unlink data)
              (loop 0 (open-output-file data :if-exists #f)))
             (else
              (sys-sleep 1)
              (loop (+ retry 1) (open-output-file data :if-exists #f))))))
    (else #t)))

(define (unlock-file drain port data)
  (case (slot-ref drain 'lock-policy)
    ((fcntl)
     (slot-set! data 'type |F_UNLCK|)
     (sys-fcntl port |F_SETLK| data))
    ((file)
     (sys-unlink data))
    (else #t)))

;; Write log
(define (with-log-output drain proc)
  (let ((path (slot-ref drain 'path)))
    (cond ((string? path)
           (let* ((p (open-output-file path :if-exists :append))
                  (l (lock-data drain p)))
             (dynamic-wind
              (lambda () (lock-file drain p l))
              (lambda () (proc p))
              (lambda () (unlock-file drain p l) (close-output-port p)))))
          ((eq? path #t)
           (proc (current-error-port)))
          ((eq? path #f)
           (call-with-output-string proc))
          ((eq? path 'syslog)
           (sys-syslog (logior (slot-ref drain 'syslog-facility)
                               (slot-ref drain 'syslog-priority))
                       (call-with-output-string proc)))
          (else
           #f))))

;; External APIs
;; log-format "fmtstr" arg ...
;; log-format drain "fmtstr" arg ...

(define-method log-format ((fmtstr <string>) . args)
  (apply log-format *default-log-drain* fmtstr args))

(define-method log-format ((drain <log-drain>) fmt . args)
  (let* ((prefix (log-get-prefix drain))
         (str (string-concatenate
               (fold-right
                (lambda (data rest)
                  (if (and (null? rest) (string-null? data))
                      '()          ;ignore trailing newlines
                      (list* prefix data "\n" rest)))
                '()
                (string-split (apply format #f fmt args) #\newline)))))
    (with-log-output drain (lambda (p) (display str p)))))

;; log-open path &keyword :program-name :prefix

(define (log-open path . args)
  (set! *default-log-drain* (apply make <log-drain> :path path args)))

(provide "gauche/logger")
