;;;
;;; logger.scm - simple use-level logging
;;;
;;;  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: logger.scm,v 1.2 2002-09-21 20:23:30 shirok Exp $
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

(define-class <log-drain> ()
  ((path   :init-keyword :path :initform #f)
   (program-name :init-keyword :program-name
                 :initform  (sys-basename (with-module user *program-name*)))
   (retry  :init-keyword :retry :initform 5)
   (prefix :init-keyword :prefix :initform "~T ~P[~$]: ")
   ))

(define *default-log-drain* (make <log-drain>))

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
                        (display (sys-strftime "%b %e %T" (sys-localtime (sys-time))))
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

(define (with-log-output drain proc)
  (let ((path (slot-ref drain 'path)))
    (cond ((string? path)
           (let ((p (open-output-file path :if-exists :append))
                 (l (make <sys-flock> :type |F_WRLCK| :whence 0)))
             (dynamic-wind
              (lambda ()
                (sys-fcntl p |F_SETLKW| l))
              (lambda () (proc p))
              (lambda ()
                (slot-set! l 'type |F_UNLCK|)
                (sys-fcntl p |F_SETLK| l)
                (close-output-port p)))))
          ((eq? path #t)
           (proc (current-error-port)))
          ((eq? path #f)
           (call-with-output-string proc))
          (else
           #f))))

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
