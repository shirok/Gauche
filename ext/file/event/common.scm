;;;
;;; file.event - Interface for system file event API
;;;
;;;   Copyright (c) 2023  Shiro Kawai  <shiro@acm.org>
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

;; This module is not meant to be directly used.  Using file.event
;; loads appropriate subsystem, which in turn use this module.
(define-module file.event.common
  (use gauche.record)
  (use data.queue))
(select-module file.event.common)

(define-class <file-event-queue> ()
  (;; All slots are private
   (thread :init-keyword :thread)
   (filters :init-keyword :filters)
   (mtq :init-form (make-mtqueue))
   ))

(define (file-event-queue feq)
  (assume-type feq <file-event-queue>)
  (~ feq'mtq))

(define-constant *event-filter-type*
  '(created
    updated
    removed
    renamed
    owner-modified
    attribute-modified
    moved-from
    moved-to
    file?
    directory?
    symlink?
    link-count-changed
    ))

(define-record-type <file-event-filter>
    %make-file-event-filter
    file-event-filter?
  (path file-event-filter-path)
  (types file-event-filter-types)
  )

(define (make-file-event-filter path types)
  (%make-file-event-filter path types))

(define-record-type <file-event>
    %make-file-event
    file-event?
  (path file-event-path)
  (types file-event-types)
  )
