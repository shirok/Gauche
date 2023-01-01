;;;
;;; gauche.test.include - include external tests
;;;
;;;   Copyright (c) 2022  Shiro Kawai  <shiro@acm.org>
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
(use util.match)
(require "r7rs-setup")
(import r7rs.import)                    ;this exports r7rs-import

;; It is common that a Gauche test script includes a generic R7RS test script.
;; Usually R7RS script has R7RS import at the beginning, which causes an issue
;; when the script is inserted into Gauche script.
;;
;; A recommended usage is to create a submodule in Gauche's test script:
;;
;;   (use gauche.test)
;;   (test-start ...)
;;     :
;;   (test-section "xxx-tests")
;;   (define-module xxx-tests
;;     (use gauche.test)
;;     (test-include-r7 "xxx-tests"))
;;
;; Sometimes the external script refers to a library that's not
;; corresponds to what Gauche provides (e.g. tests/include/srfi-222-tests.scm
;; imports (compounds) library, but Gauche provides it as srfi-222.)
;; You can list such libraries to 'exclude' clause so that import won't
;; load it:
;;
;;   (define-module srfi-222-tests
;;     (use gauche.test)
;;     (use srfi.222)
;;     (test-include-r7 "include/srfi-222-tests" (exclude (compounds))))
;;

(define-syntax test-include-r7
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ path . opts)
        (unless (string? path)
          (error "Literal string path required for test-include-r7" (cadr f)))
        (and-let1 bad (any (^p (match p
                                 [('exclude . mods) #f]
                                 [_ p]))
                           opts)
          (error "Bad test-include-r7 option clause" bad))
        (let1 exclude-mods
            (map library-name->module-name (assoc-ref opts 'exclude '()))
          (quasirename r
            `(begin
               (define-syntax ,'import r7rs-import)
               ,@(map (^m `(define-module ,m)) exclude-mods)
               (include ,(cadr f)))))]
       [_ (error "Malformed test-include-r7" f)]))))
