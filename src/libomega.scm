;;;
;;; libomega.scm - the stuff to be run after other parts are initialized
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

;;; TEMPORARY for 0.9.x series
;;; Remove this after 1.0 release!!!
;;;
;;; Add 0.9 directories, which doesn't follow the new directory structure,
;;; to support the extension modules that are installed with 0.9.
;;; A few extension packages installs files into the "sys" directory hierarchy
;;; instead of "site" one.  It is banned after 0.9.1, but we need to add 0.9's
;;; sys directories for the backward compatibility.
;;; NB: we set! to *load-path* etc here, which is an emergency workaround.
;;; Ordinary programs should never modify *load-path*/*dynamic-load-path*
;;; directly.
(select-module gauche)
(let* ([archdir (gauche-architecture-directory)]
       [m (rxmatch #/gauche-0\.9[\/\\]0\.9[^\/\\]*[\/\\]/ archdir)]
       [oldsitedir (string-append (rxmatch-before m)
                                  "gauche/site/0.9/"
                                  (rxmatch-after m))]
       [oldarchdir (string-append (rxmatch-before m)
                                  "gauche/0.9/"
                                  (rxmatch-after m))])
  (set! *dynamic-load-path*
        (append *dynamic-load-path* (list oldsitedir oldarchdir))))
(let* ([libdir (gauche-library-directory)]
       [m (rxmatch #/gauche-0\.9[\/\\]0\.9[^\/\\]*[\/\\]/ libdir)]
       [oldsitedir (string-append (rxmatch-before m)
                                  "gauche/site/"
                                  (rxmatch-after m))]
       [oldlibdir  (string-append (rxmatch-before m)
                                  "gauche/0.9/"
                                  (rxmatch-after m))])
  (set! *load-path* (append *load-path* (list oldsitedir oldlibdir))))
