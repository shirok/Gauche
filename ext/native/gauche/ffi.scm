;;;
;;; gauche.ffi - foreign function interface
;;;
;;;   Copyright (c) 2021  Shiro Kawai  <shiro@acm.org>
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

;; In general, we avoid gauche.* module depending on other utility modules.
;; However, ffi mechanism requires vast infrastructure---including parser.peg,
;; that is used by lang.c.*.  So, this module is an exception of the rule.
;; Be aware that this should be tested after other modules are tested.

(define-module gauche.ffi
  (use gauche.dictionary)
  (use gauche.config)
  (use util.match)
  (use lang.c.parser)
  (use lang.c.type)
  (export <foreign-library>
          load-foreign
          call-foreign
          ))
(select-module gauche.ffi)

(define-class <foreign-library> ()
  ((header-name :init-keyword :header-name)  ; header filename, for diag info
   (dso-name :init-keyword :dso-name)        ; dso filename, for diag info
   (dlobj :init-keyword :dlobj)              ; <dlobj>
   (entries :init-keyword :entries)))        ; symbol -> <foreign-entry>

(define-method write-object ((obj <foreign-library>) port)
  (format port "#<foreign-library ~s ~s>"
          (~ obj'header-name)
          (~ obj'dso-name)))

(define-class <foreign-entry> ()
  ((name :init-keyword :name)            ; symbol
   (ptr  :init-keyword :ptr)             ; <dlptr>
   (type :init-keyword :type)            ; c-type
   (callable? :init-keyword :callable?)
   (ret-signature :init-keyword :ret-signature)
   (arg-signatures :init-keyword :arg-signatures)))

(define-method write-object ((obj <foreign-entry>) port)
  (format port "#<foreign-entry ~s ~s~a>"
          (~ obj'name)
          (match (~ obj'type)
            [('.function . _) 'function]
            [_ 'other])
          (if (~ obj'callable?)
            ""
            "(uncallable)")))

;; API
(define (load-foreign header-name dso-name)
  (let ([hdr (c-parse-file header-name)]
        [dlo (dynamic-load dso-name :init-function #f)])
    (make <foreign-library>
      :header-name header-name
      :dso-name dso-name
      :dloboj dlo
      :entries (prepare-foreign-entries hdr dlo))))

(define (prepare-foreign-entries hdr dlobj)
  (rlet1 tab (make-hash-table eq-comparator)
    (dolist [form hdr]
      (match form
        [('decl . decls)
         (dolist [decl decls]
           (process-decl tab decl dlobj))]
        [_ #f]))))

(define (process-decl tab decl dlo)
  ;; for now, only recognize functions
  (match-let1 (name _ type _) decl
    (match type
      [('.function _ rettype argtypes)
       (and-let1 dle (dlobj-get-entry-address dlo (x->string name))
         (let ([r (process-type rettype)]
               [as (map process-arg-type argtypes)])
           (let1 e (make <foreign-entry>
                     :name name :ptr dle :type type
                     :callable? (and r (every identity as))
                     :ret-signature r
                     :arg-signatures as)
             (dict-put! tab name e))))]
      [_ #f])))

(define (process-type type)
  (let1 type (c-actual-type type)
    (cond [(c-basic-type-integral? type) 'i]
          [(c-basic-type-flonum? type)
           (case (car type)
             [(float) 'f]
             [(double) 'd]
             [else #f])]                ;for now
          [else #f])))                  ;for now

(define (process-arg-type arg) ; arg := (name type)
  (process-type (cadr arg)))

(define (unsupported)
  (error "Foreign call isn't supported (yet) on this platform:"
         (gauche-config "--arch")))

(define (arch-check)
  (cond-expand
   [gauche.os.windows (unsupported)]
   [else
    (unless (#/^x86_64-/ (gauche-config "--arch"))
      (unsupported))]))

;; API
(define (call-foreign flib name . args)
  (assume-type flib <foreign-library>)
  (assume-type name <symbol>)
  (arch-check)
  (if-let1 e (dict-get (~ flib'entries) name #f)
    (cond [(not (~ e'callable?))
           (error "foreign entry is not callable:" e)]
          [else
           (unless (= (length args) (length (~ e'arg-signatures)))
             (errorf "wrong number of arguments for foreign function ~s \
                      (~s required, but got ~s)"
                     (~ e'name)
                     (length (~ e'arg-signatures))
                     (length args)))
           ((with-module gauche.internal call-amd64)
            (~ e'ptr) (map list (~ e'arg-signatures) args) (~ e'ret-signature))])
    (errorf "cannot find foreign function ~s in ~s"
            name flib)))
