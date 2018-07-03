;;;
;;; gauche.test.script - test-script
;;;
;;;   Copyright (c) 2017-2018  Shiro Kawai  <shiro@acm.org>
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

;; This file is autoloaded from gauche.test, to avoid gauche.test from
;; depending on other modules.
(select-module gauche.test)
(use file.util)
(use srfi-1)
(use srfi-13)

;; Check toplevel bindings of a script file.  Script files can have their
;; own modules, but they usually don't have.  For this test, we load the
;; script file into an anonymous module then use test-module on it if it
;; doesn't have its own module.
(define (test-script file :key (allow-undefined '()) (bypass-arity-check '()))
  (test-count++)
  (let ([m (make-module #f)]
        [preexisting-modules (all-modules)])
    (format #t "testing bindings in script ~a ... " file) (flush)
    ;; *program-name* and *argv* are defined in #<module user> by gosh,
    ;; but we'll load the script in a temporary module, so we fake them.
    (eval `(define *program-name* ',file) m)
    (eval `(define *argv* '()) m)
    (load file :environment m)
    (let* ([absolute-file-sans-extension
            (path-sans-extension
             (if (relative-path? file)
               (simplify-path (build-path (current-directory) file))
               file))]
           [file-modules
            (filter (^[mod]
                      (string-suffix? (module-name->path (module-name mod))
                                      absolute-file-sans-extension))
                    (lset-difference eq? (all-modules) preexisting-modules))])
      (if (null? file-modules)
        (test-module-common m file allow-undefined bypass-arity-check)
        (dolist [m file-modules]
          (test-module-common m file allow-undefined bypass-arity-check))))))
