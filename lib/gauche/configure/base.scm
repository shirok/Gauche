;;;
;;; gauche.configure.base - configuring Gauche extensions
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

(define-module gauche.configure.base
  (use gauche.logger)
  (use scheme.set)
  (use util.match)
  (export
   ;; Internal to gauche.configure.*
   <package> current-package ensure-package run-quietly arg-processors

   ;; External API commonly used from other gauche.configure.*
   cf-msg-checking cf-msg-result cf-msg-warn cf-msg-error cf-msg-notice
   cf-echo)
  )
(select-module gauche.configure.base)

;;;
;;; A package
;;;

(define-class <package> ()
  ((name       :init-keyword :name)
   (version    :init-keyword :version)
   (bug-report :init-keyword :bug-report :init-value #f) ; email addr
   (url        :init-keyword :url :init-value #f)
   (gpd        :init-keyword :gpd)       ; <gauche-package-description>
   (string     :init-keyword :string)    ; package_string
   (tarname    :init-keyword :tarname)
   (tool-prefix :init-form #f)           ; cross compilation tool prefix
   (config.h   :init-form '())           ; list of (config-header . source)
   (defs       :init-form (make-hash-table 'eq?)) ;cf-define'd thingy
   (substs     :init-form (make-hash-table 'eq?)) ;cf-subst'ed thingy
   (precious   :init-form (set eq-comparator))    ;vars overridable by env
   (features   :init-form (make-hash-table 'eq?)) ;enabled features by cmdarg
   (packages   :init-form (make-hash-table 'eq?)) ;optional packages
   ))

;;;
;;; Internal parameters & apis
;;;

(define current-package (make-parameter #f))

(define (ensure-package)
  (or (current-package)
      (error "No current package - cf-init hasn't been called")))

(define run-quietly (make-parameter #f)) ;-silent or -quiet option turn this on

;; Alist of arg processors and help strings given to cf-arg-with and
;; cf-arg-enable.
;; Each element is
;; (<name> <kind> <help-string> <proc-if-given> <proc-if-not-given>)
(define arg-processors (make-parameter '()))

;;;
;;; Basic APIs
;;;

(define (tee-msg console-fmt log-fmt args)
  (apply format #t console-fmt args)
  (apply log-format log-fmt args))

;; API
;; Like AC_MSG_*
(define (cf-msg-checking fmt . args)
  (tee-msg #"checking ~|fmt|... " #"checking: ~fmt" args))
(define (cf-msg-result fmt . args)
  (tee-msg #"~|fmt|\n" #"result: ~fmt" args))
(define (cf-msg-warn fmt . args)
  (tee-msg #"Warning: ~|fmt|\n" #"Warning: ~fmt" args))
(define (cf-msg-error fmt . args)
  (tee-msg #"Error: ~|fmt|\n" #"Error: ~fmt" args)
  (exit 1))
(define (cf-msg-notice fmt . args)
  (tee-msg #"~|fmt|\n" #"~|fmt|\n" args))

;; API
;; Convenience routine for substitute of shell's echo
;; e.g.  (cf-echo "something" > "FILE")
;; or    (cf-echo "something" >> "FILE")
;; The destination, '>' or '>>' followed by a filename, must be at the end
;; of arglist if any.  If no destination is given, output goes to the current
;; output port.
(define-macro (cf-echo . args)
  (match (take-right* args 2)
    [('> name)
     `(with-output-to-file ,name
        (cut print ,@(intersperse " " (drop-right* args 2)))
        :if-exists :supersede)]
    [('>> name)
     `(with-output-to-file ,name
        (cut print ,@(intersperse " " (drop-right* args 2)))
        :if-exists :append)]
    [_ `(,tee-msg "~a\n" "Message: ~a" (list (string-join (list ,@args))))]))
