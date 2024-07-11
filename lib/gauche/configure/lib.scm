;;;
;;; gauche.configure.lib - search for specific libraries
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

(define-module gauche.configure.lib
  (use gauche.config)
  (use gauche.configure.base)
  (use gauche.configure.compile)
  (use srfi.13)
  (use util.match)
  (export cf-path-x cf-path-xtra)
  )
(select-module gauche.configure.lib)

;;;
;;; Search for X11 libraries
;;;

;; cf-path-x - Check X11 availability, and returns 3 values.
;;    The first boolean value indicates if X is available,
;;    The second and third string values are the directory to find X
;;    include files and X library files, respectively.  The second and
;;    third value can be empty strings if the compiler can find X stuff
;;    without additional options.
;;
;; NB: AC_PATH_X adds a few command-line arguments.  We can't do it
;; in the function, for the argument configuration must come before
;; cf-init.  Eventually we can provide a separate function for them.
;; For now, we just check the default locations.
(define (cf-path-x)
  (cf-msg-checking "for X")
  (cond
   [($ with-cf-subst ([LIBS "-lX11" +])
       (cf-try-compile-and-link "#include <X11/Xlib.h>"
                                '("XrmInitialize()")))
    (cf-msg-result "yes")
    (values #t "" "")]
   ;; If the default location fails, autoconf creates a skeleton Imakefile
   ;; and run xmkmf to find out X11 locations, then check the list of some
   ;; well-known paths.  These days we assume it's rare that X11 is installed
   ;; in unusual location, so we check some usual suspects.
   [(and (equal? (cf$ 'cross_compiling) "no")
         (try-usual-x-paths))
    => (match-lambda
         [(inc lib) (cf-msg-result "yes") (values #t inc lib)])]
   [else
    (cf-msg-result "no")
    (values #f "" "")]))

(define (try-usual-x-paths)
  (define usual-suspects
    '("/usr/X11/include"
      "/usr/X11R7/include"
      "/usr/X11R6/include"
      "/usr/X11R5/include"
      "/usr/X11R4/include"
      "/usr/include/X11"
      "/usr/include/X11R7"
      "/usr/include/X11R6"
      "/usr/include/X11R5"
      "/usr/include/X11R4"
      "/usr/local/X11/include"
      "/usr/local/X11R7/include"
      "/usr/local/X11R6/include"
      "/usr/local/X11R5/include"
      "/usr/local/X11R4/include"
      "/usr/local/include/X11"
      "/usr/local/include/X11R7"
      "/usr/local/include/X11R6"
      "/usr/local/include/X11R5"
      "/usr/local/include/X11R4"
      "/opt/X11/include"
      "/usr/X386/include"
      "/usr/x386/include"
      "/usr/XFree86/include/X11"
      "/usr/include"
      "/usr/local/include"
      "/usr/unsupported/include"
      "/usr/athena/include"
      "/usr/local/x11r5/include"
      "/usr/lpp/Xamples/include"
      "/usr/openwin/include"
      "/usr/openwin/share/include"))
  (define (find-header)
    (find (^p (file-exists? (build-path p "X11" "Xlib.h"))) usual-suspects))
  (define (find-lib)
    (find (^p (let ([libdir (regexp-replace #/include/ p "lib")]
                    [ext (gauche-config "--dylib-suffix")])
                (file-exists? (build-path libdir #"libX11.~ext"))))
          usual-suspects))
  (and-let* ([h (find-header)]
             [l (find-lib)])
    (list h l)))

;; cf-path-xtra - Set up a bunch of configure variables according to
;;    X11 availability.
;;    In fact, we don't have much to do here on modern systems.  We provide
;;    this function so that one can port configure.ac to configure easily.
(define (cf-path-xtra)
  ;; Find if X11 is available, and set x-includes/x-libraries if necessary.
  (define-values (have-x? incdir libdir) (cf-path-x))
  ;; Initialize
  (cf-subst 'X_CFLAGS "")
  (cf-subst 'X_PRE_LIBS "")
  (cf-subst 'X_LIBS "")
  (cf-subst 'X_EXTRA_LIBS "")
  (cond [(not have-x?)
         (cf-define 'X_DISPLAY_MISSING)]
        [else
         (unless (string-null? incdir)
           (cf-subst-append 'X_CFLAGS #"-I~|incdir|"))
         (unless (string-null? libdir)
           (cf-subst-append 'X_LIBS #"-L~|libdir|")
           ;; NB: autoconf also tries to add -R $x-libraries to X_LIBS,
           ;; seemingly required by Solaris CC.  Not sure if it is still
           ;; relevant.
           )
         ;; Autoconf checks some additional libraries, e.g. -lnsl,
         ;; -lbsd, -lsocket, etc., but we're not sure any modern systems
         ;; require them.  Here we only check -lICE.
         ($ with-cf-subst ([LDFLAGS + (if (string-null? libdir)
                                        #"-L~|libdir|"
                                        "")])
            (cf-check-lib "ICE" "IceConnectionNumber"
                          :if-found (^_ (cf-subst-append 'X_PRE_LIBS
                                                         "-lSM -lICE"))
                          :other-libs (cf$ 'X_EXTRA_LIBS)))
         ]))
