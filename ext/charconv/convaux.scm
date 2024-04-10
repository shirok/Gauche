;;;
;;; convaux - auxiliary charconv routines
;;;
;;;   Copyright (c) 2000-2024  Shiro Kawai  <shiro@acm.org>
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

(select-module gauche.charconv)
(use scheme.list)
(use srfi.13)
(use gauche.sequence)
(autoload gauche.vport
          open-input-uvector open-output-uvector get-output-uvector)

;; Determine charset compatibility.  (ces-equivalent? a b) is true if CES a and
;; CES b refer to the same CES.
;; (ces-upper-compatible? a b) is true if a string in CES b can be a legal
;; string in CES a.
;; There is a case that the compatibility can't be determined.  In such
;; case, a value passed as unknown-fallback is returned, whose default is #f.

(define-values (ces-equivalent? ces-upper-compatible?)
  (let ()
    ;; Compatibility table:
    ;; ((x ...) y ...) means x ... are all equivalent, and includes y ....
    ;; Each entry is a symbol.  The CES name is first stripped off all _ and -
    ;; then made a lower case symbol before comparison.
    ;; NB: CES 'none' works as a wildcard.
    (define ces-compatibility-table
      '(((usascii ascii iso646))
        ((iso88591 88591) usascii)
        ((iso88592 88592) usascii)
        ((iso88593 88593) usascii)
        ((iso88594 88594) usascii)
        ((iso88595 88595) usascii)
        ((iso88596 88596) usascii)
        ((iso88597 88597) usascii)
        ((iso88598 88598) usascii)
        ((iso88599 88599) usascii)
        ((iso2022jp csiso2022jp) usascii) ; not exactly
        ((iso2022jp1 csiso2022jp1) iso2022jp)
        ((iso2022jp2 csiso2022jp2) iso2022jp)
        ((iso2022jp3 csiso2022jp3) iso2022jp)
        ((iso2022kr csiso2022kr) usascii)
        ((iso2022cnext csiso2022cnext) iso2022cn)
        ((iso2022cn csiso2022cn) usascii)
        ((eucjp) usascii)
        ((euckr) usascii)
        ((euccn) usascii)
        ((euctw) usascii)
        ((big5 bigfive) usascii)
        ((big5hkscs bigfivehkscs) big5)
        ((big5plus bigfiveplus) big5)
        ((johab) usascii)
        ((shiftjis sjis) usascii) ; not exactly
        ((unicode iso10646 iso10646/utf8 utf8) usascii)
        ))

    ;; canonicalize ces name
    (define (canon-name name)
      (string->symbol (string-downcase (string-delete (x->string name) #[_-]))))

    ;; find ces entry
    (define (find-entry ces)
      (find (^e (memq ces (car e))) ces-compatibility-table))

    (define (ces-equivalent? a b :optional (unknown-fallback #f))
      (let* ([ces-a   (canon-name a)]
             [ces-b   (canon-name b)]
             [entry-a (find-entry ces-a)]
             [entry-b (find-entry ces-b)])
        (cond [(or (eq? ces-a 'none) (eq? ces-b 'none)) #t]
              [(or (not entry-a) (not entry-b))
               unknown-fallback]
              [else (eq? entry-a entry-b)])))

    (define (ces-upper-compatible? a b :optional (unknown-fallback #f))
      (let* ([ces-a   (canon-name a)]
             [ces-b   (canon-name b)]
             [entry-a (find-entry ces-a)])
        (cond [(or (eq? ces-a 'none) (eq? ces-b 'none)) #t]
              [(or (not entry-a) (not (find-entry ces-b)))
               unknown-fallback]
              [else (let loop ([entry entry-a])
                      (or (boolean (memq ces-b (car entry)))
                          (any loop (map find-entry (cdr entry)))))])))

    (values ces-equivalent? ces-upper-compatible?)))

;; Returns a list of octets for replacement characters.
;; This is called from jconv_set_replacement() internal C function.
(define (%ces-replacement ces)
  (assume-type ces <string>)
  (cond
   ;; For unicode, we use U+FFFD Replacement Character.
   [(ces-equivalent? "utf8" ces)  '(#xef #xbf #xbd)]
   ;; For jis-family, we use U+3013 Geta Mark (JIS 020E)
   ;; NB: iso-2022-jp is generated via eucjp, so it is suffice to give
   ;; the replacement char in eucjp.
   [(ces-equivalent? "eucjp" ces) '(#xa2 #xae)]
   [(ces-equivalent? "sjis" ces) '(#x81 #xac)]
   [(ces-equivalent? "iso2022jp" ces) '(#xa2 #xae)]
   ;; 1-byte encoding, we use '?'
   [(ces-equivalent? "ascii" ces) '(#x3f)]
   [(ces-equivalent? "iso88591" ces) '(#x3f)]
   [(ces-equivalent? "iso88592" ces) '(#x3f)]
   [(ces-equivalent? "iso88593" ces) '(#x3f)]
   [(ces-equivalent? "iso88594" ces) '(#x3f)]
   [(ces-equivalent? "iso88595" ces) '(#x3f)]
   [(ces-equivalent? "iso88596" ces) '(#x3f)]
   [(ces-equivalent? "iso88597" ces) '(#x3f)]
   [(ces-equivalent? "iso88598" ces) '(#x3f)]
   [(ces-equivalent? "iso88599" ces) '(#x3f)]
   ;; we should detect some more unicode family to pass U+FFFD, but
   ;; for now, we return '?'.
   [else '(#x3f)]))

;; returns appropriate input port and size
(define (%ces-input input)
  (assume-type input (</> <string> <u8vector>))
  (cond [(string? input)
         (values (open-input-string input :private? #t) (string-length input))]
        ;; avoid using u8vector? so that we don't depend on gauche.uvector
        [(is-a? input <u8vector>)
         (values (open-input-uvector input) (uvector-length input))]))

;; returns appropriate output port and get-output-*
(define (%ces-output class)
  (cond
   [(eq? class <string>)
    (values (open-output-string :private? #t) get-output-string)]
   [(eq? class <u8vector>)
    (values (open-output-uvector) get-output-uvector)]
   [else (error "Only <string> or <u8vector> is supported, but got:" class)]))

;; Convert string or uvector -> string or uvector
(define (ces-convert-to class input fromcode
                        :optional (tocode #f)
                        :key illegal-output)
  (receive (inp isize) (%ces-input input)
    (receive (out get) (%ces-output class)
      (let1 in ($ open-input-conversion-port inp fromcode
                  :to-code tocode :buffer-size isize
                  :owner? #t :illegal-output illegal-output)
        (copy-port in out :unit 'byte)
        (close-input-port in)
        (flush out)
        (begin0 (get out)
          (close-output-port out))))))

(define (ces-convert input fromcode
                     :optional (tocode #f)
                     :key illegal-output)
  (ces-convert-to <string> input fromcode tocode :illegal-output illegal-output))

;; "Wrap" the given port for convering to/from native encoding if needed.
;; Unlike open-*-conversion-port, these return port itself if the conversion
;; is not required.
(define (wrap-with-input-conversion port from-code
                                    :key (to-code (gauche-character-encoding))
                                    :allow-other-keys
                                    :rest opts)
  (if (ces-upper-compatible? to-code from-code)
    port
    (apply open-input-conversion-port port from-code :owner? #t opts)))

(define (wrap-with-output-conversion port to-code
                                     :key (from-code (gauche-character-encoding))
                                     :allow-other-keys
                                     :rest opts)
  (if (ces-upper-compatible? from-code to-code)
    port
    (apply open-output-conversion-port port to-code :owner? #t opts)))

;; Call with conversion port
(define (call-with-input-conversion port proc
                                    :key ((:encoding from-code)
                                          (gauche-character-encoding))
                                         ((:conversion-buffer-size bufsiz) 0)
                                         illegal-output)
  (if (ces-upper-compatible? (gauche-character-encoding) from-code)
    (proc port)
    (let1 cvp (open-input-conversion-port port from-code
                                          :owner? #f :buffer-size bufsiz
                                          :illegal-output illegal-output)
      (unwind-protect (proc cvp)
        (close-input-port cvp)))))

(define (call-with-output-conversion port proc
                                     :key ((:encoding to-code)
                                           (gauche-character-encoding))
                                          ((:conversion-buffer-size bufsiz) 0)
                                          illegal-output)
  (if (ces-upper-compatible? (gauche-character-encoding) to-code)
    (proc port)
    (let1 cvp (open-output-conversion-port port to-code
                                           :owner? #f :buffer-size bufsiz
                                           :illegal-output illegal-output)
      (unwind-protect (proc cvp)
        (close-output-port cvp)))))

(define (with-input-conversion port thunk . opts)
  (apply call-with-input-conversion port
         (cut with-input-from-port <> thunk)
         opts))

(define (with-output-conversion port thunk . opts)
  (apply call-with-output-conversion port
         (cut with-output-to-port <> thunk)
         opts))

;; Inserts conversion port.  These are called from system's
;; open-{input|output}-file when :encoding argument is given.
(define (%open-input-file/conv name :key (encoding #f)
                                         ((:conversion-buffer-size bufsiz) 0)
                                         ((:conversion-illegal-output illegal-output) 'raise)
                                    :allow-other-keys rest)
  (if (and (not encoding)
           (not (eq? (default-file-encoding) (gauche-character-encoding))))
    (apply %open-input-file/conv name
           :encoding (default-file-encoding)
           :conversion-buffer-size bufsiz
           :conversion-illegal-output illegal-output
           rest)
    (and-let* ([port (apply (with-module gauche.internal %open-input-file)
                            name
                            (delete-keyword :encoding rest))])
      (wrap-with-input-conversion port encoding
                                  :buffer-size bufsiz
                                  :owner? #t
                                  :illegal-output illegal-output))))

(define (%open-output-file/conv name :key (encoding (default-file-encoding))
                                          ((:conversion-buffer-size bufsiz) 0)
                                          ((:conversion-illegal-output illegal-output) 'raise)
                                     :allow-other-keys rest)
  (and-let* ([port (apply (with-module gauche.internal %open-output-file)
                          name rest)])
    (wrap-with-output-conversion port encoding
                                 :buffer-size bufsiz :owner? #t
                                 :illegal-output illegal-output)))

;;
;; Low-level API
;;

(inline-stub
 (declcode
  (.include <gauche/priv/configP.h>
            "charconv.h"))

 (define-cproc ces-conversion-supported? (from to)
   ::<boolean>
   (let* ([cfrom::(const char*) (Scm_GetCESName from "from-code")]
          [cto  ::(const char*) (Scm_GetCESName to "to-code")]
          [flags::u_long 0])
     (return (Scm_ConversionSupportedP cfrom cto flags))))

 (define-cproc open-input-conversion-port (source::<input-port>
                                           from-code
                                           :key (to-code #f)
                                                (buffer-size::<fixnum> 0)
                                                (owner? #f)
                                                illegal-output)
   (let* ([fc::(const char*) (Scm_GetCESName from_code "from-code")]
          [tc::(const char*) (Scm_GetCESName to_code "to-code")]
          [flags::u_long 0])
     (unless (SCM_FALSEP ownerP)
       (logior= flags CVPORT_OWNER))
     (cond [(SCM_EQ illegal-output 'replace) (logior= flags CVPORT_REPLACE)]
           [(or (SCM_UNBOUNDP illegal-output)
                (SCM_UNDEFINEDP illegal-output)
                (SCM_EQ illegal-output 'raise))]
           [else (Scm_Error ":illegal-output argument must be either raise or \
                             replace, but got: %S" illegal-output)])
     (return (Scm_MakeInputConversionPort source fc tc buffer_size
                                          flags))))

 (define-cproc open-output-conversion-port (sink::<output-port>
                                            to-code
                                            :key (from-code #f)
                                                 (buffer-size::<fixnum> 0)
                                                 (owner? #f)
                                                 illegal-output)
   (let* ([fc::(const char*) (Scm_GetCESName from_code "from-code")]
          [tc::(const char*) (Scm_GetCESName to_code "to-code")]
          [flags::u_long 0])
     (unless (SCM_FALSEP ownerP)
       (logior= flags CVPORT_OWNER))
     (cond [(SCM_EQ illegal-output 'replace) (logior= flags CVPORT_REPLACE)]
           [(or (SCM_UNBOUNDP illegal-output)
                (SCM_UNDEFINEDP illegal-output)
                (SCM_EQ illegal-output 'raise))]
           [else (Scm_Error ":illegal-output argument must be either raise or \
                             replace, but got: %S" illegal-output)])
     (return (Scm_MakeOutputConversionPort sink tc fc buffer_size flags))))

 (define-cproc ces-guess-from-string (string::<string> scheme::<string>)
   (let* ([size::ScmSmallInt]
          [s::(const char*) (Scm_GetStringContent string (& size) NULL NULL)]
          [guessed::(const char*)
                    (Scm_GuessCES (Scm_GetStringConst scheme) s size)])
     (if guessed
       (return (SCM_MAKE_STR guessed))
       (return '#f))))
 )
