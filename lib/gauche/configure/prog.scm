;;;
;;; gauche.configure.prog - check for programs
;;;
;;;   Copyright (c) 2013-2024  Shiro Kawai  <shiro@acm.org>
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

(define-module gauche.configure.prog
  (use gauche.configure.base)
  (use file.util)
  (use srfi.13)
  (export cf-check-prog cf-path-prog cf-check-tool)
  )
(select-module gauche.configure.prog)

;;;
;;; Tests - programs
;;;

;; API
;; Common feature for AC_CHECK_PROG, AC_PATH_PROG etc.
;; Search one of programs listed in PROGS within PATHS.
;; PATHS can be #f - then we use PATH environment variable.
;; For each found program, FILTER is called with full path of the program;
;; it may return #f if the caller wants to exclude the program for some reason.
;; If a program found, and FILTER returns true, then this procedure returns
;; the full path of the program.  If no program found, #f is returned.
(define (check-for-program progs :key ((:paths ps) #f) (filter #f))
  (define paths (or ps
                    (string-split (or (sys-getenv "PATH") '())
                                  (cond-expand [gauche.os.windows #\;]
                                               [else #\:]))))
  (define (finder prog)
    (find-file-in-paths prog :paths paths
                        :pred (if filter
                                (^p (and (filter p)
                                         (file-is-executable? p)))
                                file-is-executable?)))
  (any (^[prog]
         (cond-expand [gauche.os.windows
                       (if (equal? (path-extension prog) "exe")

                         (finder prog)
                         (or (finder prog) (finder #"~|prog|.exe")))]
                      [else (finder prog)]))
       progs))

;; Feature Test API
;; cf-check-prog works like AC_CHECK_PROG and AC_CHECK_PROGS.
;; If SYM is already cf-subst'd, we don't do anything.
(define (cf-check-prog sym prog-or-progs
                       :key (value #f) (default #f) (paths #f) (filter #f))
  (unless (and (cf-have-subst? sym)
               (not (string-null? (cf-ref sym))))
    (cf-msg-checking "for ~a" prog-or-progs)
    (if-let1 found (check-for-program (listify prog-or-progs)
                                      :paths paths :filter filter)
      (let1 result (or value (sys-basename found))
        (cf-msg-result "~a" result)
        (cf-subst sym result))
      (begin (cf-msg-result "no")
             (and default (cf-subst sym default))))))

;; Feature Test API
;; cf-path-prog works like AC_PATH_PROG and AC_PATH_PROGS.
(define (cf-path-prog sym prog-or-progs
                      :key (value #f) (default #f) (paths #f) (filter #f))
  (unless (and (cf-have-subst? sym)
               (not (string-null? (cf-ref sym))))
    (cf-msg-checking "for ~a" prog-or-progs)
    (if-let1 found (check-for-program (listify prog-or-progs)
                                      :paths paths :filter filter)
      (let1 result (or value found)
        (cf-msg-result "~a" result)
        (cf-subst sym result))
      (begin (cf-msg-result "no")
             (and default (cf-subst sym default))))))

;; Feature Test API *COMPATIBILITY*
;; This is used in place of cf-check-prog when cross compilation needs to
;; taken care of.  At this moment we don't support cross compilation yet,
;; so we just call cf-check-prog.
(define (cf-check-tool sym prog-or-progs
                       :key (default #f) (paths #f) (filter #f) :rest keys)
  (apply cf-check-prog sym prog-or-progs keys))
