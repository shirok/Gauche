;;;
;;; mime.scm - parsing MIME (rfc2045) message
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

;; RFC2045 Multipurpose Internet Mail Extensions (MIME) Part One:
;;            Format of Internet Message Bodies
;; RFC2046 Multipurpose Internet Mail Extensions (MIME) Part Two:
;;            Media Types
;; RFC2047 Multipurpose Internet Mail Extensions (MIME) Part Three:
;;            Message Header Extensions for Non-ASCII Text
;; RFC2183 Communicating Presentation Information in Internet Messages:
;;            The Content-Disposition Header Field
;; RFC2231 MIME Parameter Value and Encoded Word Extensions:
;;            Character Sets, Languages, and Continuations

(define-module rfc.mime
  (use srfi-13)
  (use srfi-14)
  (use rfc.822)
  (use util.match)
  (export mime-parse-version
          mime-parse-content-type mime-parse-content-disposition
          mime-parse-parameters mime-compose-parameters
          mime-encode-word mime-encode-text
          mime-decode-word mime-decode-text
          <mime-part>
          mime-parse-message mime-retrieve-body
          mime-body->string mime-body->file
          mime-make-boundary mime-compose-message mime-compose-message-string
          )
  )
(select-module rfc.mime)

(autoload rfc.quoted-printable quoted-printable-decode-string
          quoted-printable-encode quoted-printable-encode-string)
(autoload rfc.base64 base64-decode-string base64-decode
          base64-encode-string base64-encode)
(autoload gauche.charconv
          ces-upper-compatible? ces-conversion-supported? ces-convert)
(autoload rfc.mime-port make-mime-port)
(autoload srfi-27 random-integer)       ;for MIME boundary generation

;;===============================================================
;; Basic utility
;;

;; returns list of major and minor versions in integers
(define (mime-parse-version field)
  (and-let* ([ field ]
             [s (string-concatenate
                 (map x->string (rfc822-field->tokens field)))]
             [m (#/^(\d+)\.(\d+)$/ s)])
    (map (^d (x->integer (m d))) '(1 2))))

(define-constant *ct-token-chars*
  (char-set-difference #[\u0021-\u007e] #[()<>@,\;:\\\"/\[\]?=]))

;; RFC2045 Content-Type header field
;; returns (<type> <subtype> (<attribute> . <value>) ...)
(define (mime-parse-content-type field)
  (and field
       (call-with-input-string field
         (^[input]
           (and-let* ([type (rfc822-next-token input `(,*ct-token-chars*))]
                      [ (string? type) ]
                      [ (eqv? #\/ (rfc822-next-token input '())) ]
                      [subtype (rfc822-next-token input `(,*ct-token-chars*))]
                      [ (string? subtype) ])
             (list* (string-downcase type)
                    (string-downcase subtype)
                    (mime-parse-parameters input)))))))

;; RFC2183 Content-Disposition header field
;; returns (<token> (<attribute> . <value>) ...)
(define (mime-parse-content-disposition field)
  (and field
       (call-with-input-string field
         (^[input]
           (and-let* ([token (rfc822-next-token input `(,*ct-token-chars*))]
                      [ (string? token) ])
             (cons (string-downcase token)
                   (mime-parse-parameters input)))))))


;; parse a parameter-values type header field
;;   ;parameter=value;parameter=value
;; => ((parameter . value) ...)
;; NB: This will support RFC2231, but not yet.
(define (mime-parse-parameters :optional (input (current-input-port)))
  (let loop ((r '()))
    (cond [(and-let* ([ (eqv? #\; (rfc822-next-token input '())) ]
                      [attr (rfc822-next-token input `(,*ct-token-chars*))]
                      [ (string? attr) ]
                      [ (eqv? #\= (rfc822-next-token input '())) ]
                      [val  (rfc822-next-token
                             input
                             `(,*ct-token-chars*
                               (#[\"] . ,rfc822-quoted-string)))]
                      [ (string? val) ])
             (cons attr val))
           => (^p (loop (cons p r)))]
          [else (reverse! r)])))

;; Inverse of mime-parse-parameters.
;; ((parameter . value) ...) => ;parameter=value;parameter=value ...
;; NB: This will support RFC2231, but not yet.
(define (mime-compose-parameters pvs
                                 :optional (port (current-output-port))
                                 :key (start-column 0))
  (define (quote-value v)               ; TODO: possibly folding?
    (if (string-every *ct-token-chars* v)
      v
      (string-append "\"" (regexp-replace-all #/[\"\\]/ v "\\\\\\0") "\"")))
  (define (valid-name p)
    (rlet1 z (x->string p)
      (unless (string-every *ct-token-chars* z)
        (error "invalid parameter value for rfc2822 header:" p))))
  (define (gen)
    (fold (^[pv column]
            (match pv
              [(p . v)
               (let* ([z #"~(valid-name p)=~(quote-value (x->string v))"]
                      [len (+ (string-length z) column)])
                 (cond [(> len 78)
                        (display ";\r\n ") (display z) (string-length z)]
                       [else
                        (display "; ") (display z) (+ len column 2)]))]
              [_ (error "bad parameter-value entry:" pv)]))
          start-column pvs))
  (match port
    [#f (with-output-to-string gen)]
    [#t (gen)]
    [(? port?) (with-output-to-port port gen)]))

;;===============================================================
;; RFC2047 header field encoding
;;

;; Decoding

;; the end is not anchored, to be used in mime-decode-text.
(define-constant *mime-encoded-header-rx*
  #/^=\?([-!#-'*+\w\^-~]+)\?([-!#-'*+\w\^-~]+)\?([!->@-~]+)\?=/)

(define (%mime-decode-word word charset encoding body)
  (if (ces-conversion-supported? charset #f)
    (guard (e (else word))  ;; capture illegal encoding
      (cond [(string-ci=? encoding "q")
             (ces-convert (quoted-printable-decode-string body) charset #f)]
            [(string-ci=? encoding "b")
             (ces-convert (base64-decode-string body) charset #f)]
            [else word])) ;; unsupported encoding
    word))

;; decode rfc2047-encoded word, i.e. "=?...?="
;; if word isn't legal encoded word, it is returned as is.
(define (mime-decode-word word)
  (cond [(string-incomplete? word) word] ;; safety net
        [(rxmatch *mime-encoded-header-rx* word)
         => (^m (if (equal? (m'after) "")
                  (%mime-decode-word word (m 1) (m 2) (m 3))
                  word))]
        [else word]))

;; Decode the entire header field body, possibly a mixture of
;; encoded-words and ordinary words.  NOTE: If you apply this to
;; a structured field body, the decoded words may contain special
;; characters meaningful to the structured body and thus confuse
;; the parser.  The correct order is to parse first, then apply
;; mime-decode-word for each token.
(define (mime-decode-text body)
  (let loop ([s body])
    (receive (pre rest) (string-scan s "=?" 'before*)
      (cond [(not pre) s]
            [(rxmatch *mime-encoded-header-rx* rest)
             => (^m (string-append pre
                                   (%mime-decode-word (m 0) (m 1) (m 2) (m 3))
                                   (loop (m'after))))]
            [else s]))))

;; Encode a single word.  This one always encode, even WORD contains
;; US-ASCII characters only.
(define (mime-encode-word word :key (charset "utf-8") (transfer-encoding 'base64))
  (let ([converted
         (cond [(ces-upper-compatible? charset (gauche-character-encoding)) word]
               [else (ces-convert word (gauche-character-encoding) charset)])]
        [enc (%canonical-encoding transfer-encoding)])
    (format "=?~a?~a?~a?=" charset enc
            ((if (eq? enc 'B)
               base64-encode-string
               quoted-printable-encode-string)
             converted :line-width #f))))

;; Encode entire body if necessary, considering line folding.
;; At this moment, this function just treat the body as unstructured
;; text.  It's not safe to pass structured text.  In future we may have
;; a keyword option to make body parsed as structured text.
(define (mime-encode-text body :key
                          (charset "utf-8")
                          (transfer-encoding 'base64)
                          (line-width 76)
                          (start-column 0)
                          (force #f))
  (let ([enc (%canonical-encoding transfer-encoding)]
        [cslen (string-length (x->string charset))]
        [pass-through? (and (not force)
                            (ces-upper-compatible? charset 'ascii)
                            (string-every #[\u0000-\u007f] body))])
    ;; estimates the length of an encoded word.  it is not trivial since
    ;; we have to ces-convert each segment separately, and there's no
    ;; general way to estimate the size of ces-converted string.
    ;; we throw in some heuristics here.
    (define (estimate-width s i)
      (let1 na (string-count s #[\u0000-\u007f] 0 i)
        (+ 6 cslen
           (if (eq? enc 'B)
             (ceiling (* (+ na (* (- i na) 3)) 4/3))
             (let1 ng (string-count s #[!-<>-~] 0 i)
               (+ (- na ng) (* 3 (+ ng (* (- i na) 3)))))))))
    (define (encode-word w)
      (mime-encode-word w :charset charset :transfer-encoding enc))
    ;; we don't need to pack optimally, so we use some heuristics.
    (define (encode str width adj)
      (or (and-let* ([estim (estimate-width str (string-length str))]
                     [ (< (* adj estim) width) ]
                     [ew (encode-word str)])
            (if (<= (string-length ew) width)
              `(,ew)
              (encode str width (* adj (/. (string-length ew) width)))))
          (let loop ([k (min (string-length str) (quotient width 2))])
            (let1 estim (* adj (estimate-width str k))
              (if (<= estim width)
                (let1 ew (encode-word (string-take str k))
                  (if (<= (string-length ew) width)
                    (list* ew "\r\n "
                           (encode (string-drop str k) (- line-width 1) adj))
                    (loop (floor->exact
                           (* k (/ width (string-length ew)))))))
                (loop (floor->exact (* k (/ width estim)))))))
          ))
    ;; fill pass-through text.  we try our best to look for an appropriate
    ;; place to insert FWS.  we know STR is all ASCII.
    (define (fill str width)
      (if (<= (string-length str) width)
        `(,str)
        (or (and-let* ([pos (string-index-right str #\space 0 width)])
              (list* (string-take str pos) "\r\n "
                     (fill (string-drop str (+ pos 1)) (- line-width 1))))
            ;; if we can't find a whitespace, we break in the middle of
            ;; word for the last resort.
            (list* (string-take str width) "\r\n "
                   (fill (string-drop str width) (- line-width 1))))))

    (cond [(or (not line-width) (zero? line-width))
           (if pass-through? body (encode-word body))]
          [(< line-width 30)
           (errorf "line width (~a) is too short to encode header field body: ~s" line-width body)]
          [(< (- line-width start-column) 30)
           ;; just in case if header name is very long.  we insert line break
           ;; first.
           (string-concatenate
            (cons "\r\n " (if pass-through?
                            (fill body (- line-width 1))
                            (encode body (- line-width 1) 1.0))))]
          [else
           (string-concatenate
            (if pass-through?
              (fill body (- line-width start-column))
              (encode body (- line-width start-column) 1.0)))])))

(define (%canonical-encoding transfer-encoding)
  (case transfer-encoding
    [(B b base64) 'B]
    [(Q q quoted-printable) 'Q]
    [else (error "unsupported MIME header encoding specifier:"
                 transfer-encoding)]))


;;===============================================================
;; Basic streaming parser
;;

;; message information packet
(define-class <mime-part> ()
  ((type     :init-keyword :type :init-value "text")
   (subtype  :init-keyword :subtype :init-value "plain")
   (parameters :init-keyword :parameters :init-value '())
   (transfer-encoding :init-keyword :transfer-encoding :init-value #f)
   (headers  :init-keyword :headers :init-value '())
   (parent   :init-keyword :parent :init-value #f)
   (index    :init-keyword :index :init-value 0)
   (content  :init-keyword :content :init-value #f)
   (source   :init-keyword :source :init-value #f) ; only used for composing
   ))

(define (mime-parse-message port headers handler)
  (internal-parse port headers handler #f 0
                  '("text" "plain" ("charset" . "us-ascii"))))

(define (internal-parse port headers handler parent index default-type)
  (let* ([ctype (or (mime-parse-content-type
                     (rfc822-header-ref headers "content-type"))
                    default-type)]
         [enc   (rfc822-header-ref headers "content-transfer-encoding" "7bit")]
         [packet (make <mime-part>
                   :type (car ctype)
                   :subtype (cadr ctype)
                   :parameters (cddr ctype)
                   :transfer-encoding enc
                   :parent parent
                   :index index
                   :headers headers)])
    (cond
     [(equal? (car ctype) "multipart")
      (multipart-parse port packet handler)]
     [(equal? (car ctype) "message")
      (message-parse port packet handler)]
     [else
      ;; normal body
      (slot-set! packet 'content (handler packet port))
      packet]
     )))

(define (multipart-parse port packet handler)
  (let* ([boundary (or (assoc-ref (ref packet 'parameters) "boundary")
                       (error "No boundary given for multipart message"))]
         [default-type (if (equal? (ref packet 'subtype) "digest")
                         '("message" "rfc822")
                         '("text" "plain" ("charset" . "us-ascii")))]
         [mime-port (make-mime-port boundary port)])
    (let loop ([index 0]
               [contents '()])
      (let* ([headers (rfc822-header->list mime-port)]
             [r (internal-parse mime-port headers handler
                                packet index
                                default-type)])
        (case (ref mime-port 'state)
          [(boundary)
           (set! (ref mime-port 'state) 'body)
           (loop (+ index 1) (cons r contents))]
          [(eof)
           (set! (ref packet 'content) (reverse! (cons r contents)))
           packet]
          [else ;; parser returned without readling entire part.
           ;; discard the rest of the part.
           (let loop ((b (read-byte port)))
             (unless (eof-object? b)
               (loop (read-byte port))))
           packet])))
    ))

(define (message-parse port packet handler)
  (let* ([headers (rfc822-header->list port)]
         [r (internal-parse port headers handler packet 0
                            '("text" "plain" ("charset" . "us-ascii")))])
    (set! (ref packet 'content) (list r)))
  packet)

;;===============================================================
;; Body readers
;;

(define (mime-retrieve-body packet inp outp)

  (define (read-line/nl)
    (let loop ([c (read-char inp)]
               [chars '()])
      (cond [(eof-object? c)
             (if (null? chars) c  (list->string (reverse! chars)))]
            [(char=? c #\newline) (list->string (reverse! (cons c chars)))]
            [(char=? c #\return)
             (let1 c (peek-char inp)
               (if (char=? c #\newline)
                 (list->string (reverse! (list* (read-char inp)
                                                #\return
                                                chars)))
                 (list->string (reverse! (cons #\return chars)))))]
            [else (loop (read-char inp) (cons c chars))])))

  (define (read-text decoder)
    (let loop ([line (read-line/nl)])
      (unless (eof-object? line)
        (display (decoder line) outp)
        (loop (read-line/nl)))))

  (define (read-base64)
    (define (base64-output string out)
      (with-input-from-string string
        (cut with-port-locking (current-input-port)
             (cut with-output-to-port out base64-decode))))
    (let1 buf (open-output-string :private? #t)
      (let loop ([line (read-line inp)])
        (unless (eof-object? line)
          (display line buf)
          (loop (read-line inp))))
      (base64-output (get-output-string buf) outp))
    )

  (with-port-locking inp
    (^[] (let1 enc (ref packet 'transfer-encoding)
           (cond
            [(string-ci=? enc "base64") (read-base64)]
            [(string-ci=? enc "quoted-printable")
             (read-text quoted-printable-decode-string)]
            [(member enc '("7bit" "8bit" "binary"))
             (let loop ((b (read-byte inp)))
               (unless (eof-object? b)
                 (write-byte b outp)
                 (loop (read-byte inp))))]
            ))))
  )

(define (mime-body->string packet inp)
  (let1 s (open-output-string :private? #t)
    (mime-retrieve-body packet inp s)
    (get-output-string s)))

(define (mime-body->file packet inp filename)
  (call-with-output-file filename
    (^[outp]
      (with-port-locking outp
        (cut mime-retrieve-body packet inp outp))))
  filename)

;;===============================================================
;; MIME composer
;;

;; Each PART must be either an instance of <mime-part>, or a list
;; of <mime-part-list>, where
;;   <mime-part-list> : (<content-type> (<header> ...) <body>)
;;   <content-type>   : (<type> <subtype> <header-param> ...)
;;   <header-param>   : (<key> . <value>) ...
;;   <header>         : (<header-name> <encoded-header-value>)
;;                    | (<header-name (<header-value> <header-param> ...))
;;   <body>           : string
;;                    | (file <filename>)
;;                    | (subparts <part> ...)
;;
;; Note: In the first form of <header>, <encoded-header-value> must
;; already be encoded using RFC2047 or RFC2231 if the original value
;; contains non-ascii characters.
;; In the second form, we plan to do RFC2231 encoding on behalf of
;; the caller; but the current version does not implement it.  The
;; caller SHOULD NOT pass encoded words in this form, since it may
;; result double-encoding when we implement the auto encoding feature;
;; for the time being, the second form restricts ASCII-only values.

;; Emits composed message to PORT, and returns boundary string.
;; The boundary string is automatically generated unless provided
;; by the optional argument.
(define (mime-compose-message parts
                              :optional (port (current-output-port))
                              :key (boundary (mime-make-boundary)))
  (dolist [p parts]
    (for-each (cut display <> port) `("\r\n--" ,boundary "\r\n"))
    (mime-generate-one-part (canonical-part p) port))
  (for-each (cut display <> port) `("\r\n--" ,boundary "--\r\n"))
  boundary)

;; Returns composed message in string, AND the boundary.
(define (mime-compose-message-string parts
                                     :key (boundary (mime-make-boundary)))
  (values (call-with-output-string
            (cut mime-compose-message parts <> :boundary boundary))
          boundary))

(define (mime-make-boundary)
  (format "boundary-~a" (number->string (* (random-integer (expt 2 64))
                                           (sys-time) (sys-getpid))
                                        36)))

;; internal stuff
(define (canonical-part p)
  (match p
    [(? (cut is-a? <> <mime-part>)) p]
    [((type subtype . params) (headers ...) body)
     (let1 hs (filter-map canonical-header headers)
       (apply make <mime-part>
              :type type :subtype subtype :parameters params :headers hs
              :transfer-encoding (rfc822-header-ref hs "content-transfer-encoding")
              (match body
                [(? string?) `(:content ,body)]
                [('file name) `(:source ,name)]
                [('subparts ps ...) `(:content ,(map canonical-part ps))]
                [_ (error "Invalid mime part body spec:" body)])))]
    [_ (error "Invalid mime part spec:" p)]))

(define (canonical-header header)
  (match header
    [(name . x) (cons (x->string name) x)]
    [_ #f]))

(define (mime-generate-one-part part port)
  (when (list? (ref part'content))
    (unless (member "boundary" (ref part'parameters))
      (push! (ref part'parameters) (cons "boundary" (mime-make-boundary)))))
  (let1 cte (mime-generate-part-header part port)
    (with-output-to-port port
      (^[]
        (cond
         [(ref part'source) =>
          (cut with-input-from-file <> (cut mime-generate-part-body part cte))]
         [(list? (ref part'content))
          (mime-compose-message (ref part'content) port
                                :boundary (assoc-ref (ref part'parameters)
                                                     "boundary"))]
         [(string? (ref part'content))
          (with-input-from-string (ref part'content)
            (cut mime-generate-part-body part cte))]
         [else (error "unsupported MIME part content")])))))

;; returns content-transfer-encoding
(define (mime-generate-part-header part port)
  (rlet1 cte (ref part'transfer-encoding)
    (rfc822-write-headers
     `(("Content-type"
        ,(format "~a/~a~a" (ref part'type) (ref part'subtype)
                 (mime-compose-parameters (ref part'parameters) #f)))
       ,@(cond-list [cte => (cut list "Content-transfer-encoding" <>)])
       ,@(filter-map gen-header-1 (ref part'headers)))
     :output port :check :ignore)))

(define (gen-header-1 h)
  (match h
    [("content-transfer-encoding" . _) #f]
    [("content-type" . _) #f]
    [(name (value pv ...))
     (let* ([sval (x->string value)]
            [spvs (mime-compose-parameters pv #f
                   :start-column (+ (string-length name) (string-length sval) 2))])
       `(,name ,(if (null? pv) sval #"~|sval|~|spvs|")))]
    [(name value) h]))

;; current-input-port -> current-output-port
(define (mime-generate-part-body part transfer-enc)
  (cond
   [(or (not transfer-enc) (member transfer-enc '("binary" "7bit") string-ci=?))
    (copy-port (current-input-port) (current-output-port))]
   [(string-ci=? transfer-enc "base64") (base64-encode)]
   [(string-ci=? transfer-enc "quoted-printable") (quoted-printable-encode)]
   [else (error "Unsupported transfer encoding encountered while composing \
                 mime message: " transfer-enc)]))

