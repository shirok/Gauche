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
  (use gauche.configure.base)
  (use gauche.configure.compile)
  (use srfi.13)
  (export cf-path-x cf-path-xtra)
  )
(select-module gauche.configure.lib)

;;;
;;; Search for X11 libraries
;;;

;; cf-path-x - Check X11 availability and set configure variables
;;    have-x and no-x accordingly.  This does not modify other configure
;;    variables; see cf-path-xtra below.
;;
;; NB: AC_PATH_X adds a few command-line arguments.  We can't do it
;; in the function, for the argument configuration must come before
;; cf-init.  Eventually we can provide a separate function for them.
;; For now, we just check the default locations.
(define (cf-path-x)
  (cf-msg-checking "for X")
  (cond
   [($ with-cf-subst ((LIBS #"=lX11 ~(cf$ 'LIBS)"))
       (cf-try-compile-and-link "#include <X11/Xlib.h>"
                                '("XrmInitialize()")))
    (cf-msg-result "yes")
    (cf-subst 'have-x "yes")
    (cf-subst 'no-x "no")
    (cf-subst 'x-includes "")
    (cf-subst 'x-libraries "")]
   ;; NB: if the default location fails, autoconf creates a skeleton Imakefile
   ;; and run xmkmf to find out X11 locations. These days we assume it is rare
   ;; that X11 is installed in unsual location; if it's not in the usual place,
   ;; probably the system doesn't have one.
   [else
    (cf-msg-result "no")
    (cf-subst 'have-x "no")
    (cf-subst 'no-x "yes")]))

;; cf-path-xtra - Set up a bunch of configure variables according to
;;    X11 availability.
;;    In fact, we don't have much to do here on modern systems.  We provide
;;    this function so that one can port configure.ac to configure easily.
(define (cf-path-xtra)
  (cf-path-x)
  (cf-subst 'X_CFLAGS "")
  (cf-subst 'X_PRE_LIBS "")
  (cf-subst 'X_LIBS "")
  (cf-subst 'X_EXTRA_LIBS "")
  (cond [(equal? (cf$ 'no-x) "yes")
         (cf-define 'X_DISPLAY_MISSING)]
        [else
         (unless (string-null? (cf$ 'x-includes))
           (cf-subst-append 'X_CFLAGS #"-I~(cf$ 'x-includes)"))
         (unless (string-null? (cf$ 'x-libraries))
           (cf-subst-append 'X_LIBS #"-L~(cf$ 'x-libraries)")
           ;; NB: autoconf also tries to add -R $x-libraries to X_LIBS,
           ;; seemingly required by Solaris CC.  Not sure if it is still
           ;; relevant.
           )
         ;; Autoconf checks some additional libraries, e.g. -lnsl,
         ;; -lbsd, -lsocket, etc., but we're not sure any modern systems
         ;; require them.  Here we only check -lICE.
         ($ with-cf-subst ((LDFLAGS
                            (if (string-null? (cf$ 'x-libraries))
                              (cf$ 'LDFLAGS)
                              #"~(cf$ 'LDFLAGS) -L~(cf$ 'x-libraries)")))
            (cf-check-lib "ICE" "IceConnectionNumber"
                          :if-found (^_ (cf-subst-append 'X_PRE_LIBS
                                                         "-lSM -lICE"))
                          :other-libs (cf$ 'X_EXTRA_LIBS)))

         ]))
