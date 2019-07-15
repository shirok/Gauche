;;;
;;; gauche.package.fetch - fetch a package
;;;
;;;   Copyright (c) 2004-2019  Shiro Kawai  <shiro@acm.org>
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

;; *EXPERIMENTAL*
;; gauche.package.fetch module is intended to automate fetching package
;; distribution from the Net.
;;
;; Eventually this module should take care of fetching a list of
;; available packages, and maintaining downloaded package cache.
;; For now, what this does is just to download tarball from the net
;; if it is specified by http or ftp url.

(define-module gauche.package.fetch
  (use rfc.uri)
  (use file.util)
  (use gauche.package.util)
  (use gauche.parameter)
  (export gauche-package-ensure))
(select-module gauche.package.fetch)

;; Default programs
(define *wget-program*     (find-file-in-paths "wget"))
(define *ncftpget-program* (find-file-in-paths "ncftpget"))

(define (gauche-package-ensure uri :key (config '()))
  (let* ([build-dir (assq-ref config 'build-dir ".")]
         [wget      (assq-ref config 'wget *wget-program*)]
         [ncftpget  (assq-ref config 'ncftpget *ncftpget-program*)]
         [dest      (build-path build-dir (sys-basename uri))])
    (rxmatch-case uri
      (#/^https?:/ (#f)
                   (sys-unlink dest)
                   (run #"~wget -P \"~build-dir\" \"~uri\"")
                   dest)
      (#/^ftp:/ (#f)
                (guard (e (else (sys-unlink dest) (raise e)))
                  (run #"~ncftpget -c \"~uri\" > \"~dest\""))
                dest)
      (else
       (unless (file-is-readable? uri)
         (error "can't read the package: " uri))
       uri))))
