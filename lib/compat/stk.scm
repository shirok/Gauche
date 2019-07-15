;;;
;;; port.stk - stk compatibility interface
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

(define-module compat.stk
  (use srfi-1)
  (use srfi-13)
  (use srfi-14)
  (use gauche.sequence)
  (export *argc*
          copy-tree remq remv remove string->uninterned-symbol bignum?
          string-find string-index string-lower string-upper split-string
          vector-copy vector-resize promise? continuation? catch
          procedure-body input-file-port? output-file-port? open-file
          close-port port-closed? try-load autoload?
          when-port-readable when-port-writable error
          input-string-port? output-string-port? read-from-string
          open-input-virtual open-output-virtual
          input-virtual-port? output-virtual-port?
          keyword->string
          environment? the-environment parent-environment global-environment
          environment->list procedure-environment
          eval-hook
          export-symbol export-all-symbols
          module-environment module-symbols
          macro macro? macro-expand macro-expand-1 macro-body
          address-of address?
          set-signal-handler! add-signal-handler! get-signal-handlers
          send-signal
          getcwd chdir getpid expand-file-name canonical-path
          system getenv setenv! file-is-readable? file-is-writable?
          file-is-executable? glob remove-file rename-file
          temporary-file-name
          gc gc-stats expand-heap version machine-type
          random set-random-seed! dump get-internal-info
          time uncode
          posix-perror posix-stat posix-stat->vector
          posix-access posix-pipe posix-unlink posix-symlink
          posix-chmod posix-rename posix-getlogin posix-mkdir
          posix-rmdir posix-time posix-ctime posix-localtime
          posix-gmtime posix-mktime posix-tm->vector vector->posix-tm
          posix-strftime posix-fork posix-wait posix-uname
          posix-host-name posix-domain-name
          )
  )
(select-module compat.stk)

;(define *argc* (length *argv*))

; copy-tree

(define (remq item list)   (delete item list eq?))
(define (remv item list)   (delete item list eqv?))
(define (remove item list) (delete item list equal?))

; string->uninterned-symbol
; bignum?

(define (string-find sub str)
  (number? (string-contains str sub)))
(define (string-index sub str)
  (string-contains str sub))
(define string-lower string-downcase)
(define string-upper string-upcase)
(define (split-string str :optional (delim #[\s]))
  (string-tokenize str (char-set-complement delim)))

; vector-copy
; vector-resize

; promise?
; continuation?
; catch
; procedure-body

; input-file-port?
; output-file-port?
; open-file
; close-port
; port-closed?

; try-load
; autoload?

; when-port-readable
; when-port-writable

(define error errorf)

; input-string-port?
; output-string-port?

(define (read-from-string str) (with-input-from-string str read))

; open-input-virtual
; open-output-virtual
; input-virtual-port?
; output-virtual-port?

; keyword->string

; environment?
; the-environment
; parent-environent
; global-environment
; environment->list
; procedure-environment

; eval-hook

; export-symbol
; export-all-symbols

; module-environment
; module-symbols

; macro
; macro?
; macro-expand
; macro-expand-1
; macro-body

; address-of
; address?

; set-signal-handler!
; add-signal-handler!
; get-signal-handlers
; send-signal

(define getcwd sys-getcwd)
(define chdir  sys-chdir)
(define getpid sys-getpid)
(define (expand-file-name name)
  (sys-normalize-pathname name :expand #t))
(define (canonical-path name)
  (sys-normalize-pathname name :canonical #t))
(define system sys-system)
(define getenv sys-getenv)
(define setenv! sys-putenv)

(define (file-is-readable? file)   (sys-access file R_OK))
(define (file-is-writable? file)   (sys-access file W_OK))
(define (file-is-executable? file) (sys-access file X_OK))

(define glob sys-glob)
(define remove-file sys-unlink)
(define rename-file sys-rename)
(define temporary-file-name sys-tmpnam)

(define (eval-string str) (eval (read-from-string str)))

; gc
; gc-stats
; expand-heap
(define version gauche-version)
; machine-type
(define random sys-random)
(define set-random-seed! sys-srandom)
; dump
; get-internal-info
; time
; uncode

;; POSIX
(provide "posix") ; to fool (require "posix")

; *errno*
; posix-perror

(define posix-stat sys-stat)
(define (posix-stat->vector stat)
  (apply vector
         (map (^p (p stat))
              (list sys-stat->dev
                    sys-stat->ino
                    sys-stat->mode
                    sys-stat->nlink
                    sys-stat->uid
                    sys-stat->gid
                    sys-stat->size
                    sys-stat->atime
                    sys-stat->mtime
                    sys-stat->ctime))))
(define posix-access sys-access)
(define (posix-pipe)
  (receive io (sys-pipe) io))
(define posix-unlink sys-unlink)
(define posix-symlink sys-symlink)
(define posix-chmod sys-chmod)
(define posix-rename sys-rename)
(define posix-getlogin sys-getlogin)
(define posix-rmdir sys-rmdir)
(define posix-time sys-time)
(define posix-ctime sys-ctime)
(define posix-localtime sys-localtime)
(define posix-gmtime sys-gmtime)
(define posix-mktime sys-mktime)
(define (posix-tm->vector tm)
  (error "not implemented yet"))
(define (vector->posix-tm tm)
  (error "not implemented yet"))
(define posix-strftime sys-strftime)
(define posix-fork sys-fork)
(define (posix-wait)
  (receive st (sys-wait) st))
(define (posix-uname)
  (list->vector (sys-uname)))
(define posix-host-name sys-gethostname)
(define posix-domain-name sys-getdomainname)


