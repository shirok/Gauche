;;;
;;; gauche.interactive.init - Initialization of REPL session
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

;; This file is loaded implicitly by 'gosh' when no script file is specified.
;; It's never meant to be 'use'd or explicitly loaded.

;; We work on 'user' module.
(select-module user)
(use gauche.interactive)

;; Load user-specific default settings from ~/.gaucherc, if there's any.
;; It is evaluated in the user module.
(let ((dotfile (sys-normalize-pathname "~/.gaucherc" :expand #t)))
  (when (sys-access dotfile F_OK)
    (load dotfile)))

;; If gosh is invoked with R7RS mode, import r7rs-small libraries
;; into user module for the convenience.  The variable *r7rs-mode*
;; is set by main.c if -r7 option is given.
(when (global-variable-ref (find-module 'user) '*r7rs-mode* #f)
  (eval '(import (scheme base) (scheme case-lambda) (scheme char)
                 (scheme complex) (scheme cxr) (scheme eval)
                 (scheme file) (scheme inexact) (scheme lazy)
                 (scheme load) (scheme process-context) (scheme read)
                 (scheme repl) (scheme time) (scheme write)
                 (only (gauche base) *1 *1+ *2 *2+ *3 *3+ *e *history))
        (find-module 'r7rs.user)))

