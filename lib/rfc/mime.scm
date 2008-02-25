;;;
;;; mime.scm - parsing MIME (rfc2045) message
;;;  
;;;   Copyright (c) 2000-2007  Shiro Kawai  <shiro@acm.org>
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
;;;  $Id: mime.scm,v 1.16 2008-02-25 08:42:57 shirok Exp $
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
  (use gauche.vport)
  (use gauche.uvector)
  (use srfi-1)
  (use srfi-13)
  (use srfi-14)
  (use rfc.822)
  (use util.match)
  (use util.queue)
  (use util.list)
  (export mime-parse-version
          mime-parse-content-type mime-parse-content-disposition
          mime-parse-parameter-value
          mime-encode-word mime-encode-text
          mime-decode-word mime-decode-text
          <mime-part>
          mime-parse-message mime-retrieve-body
          mime-body->string mime-body->file
          )
  )
(select-module rfc.mime)

(autoload rfc.quoted-printable quoted-printable-decode-string
          quoted-printable-encode-string)
(autoload rfc.base64 base64-decode-string base64-decode
          base64-encode-string)
(autoload gauche.charconv
          ces-upper-compatible? ces-conversion-supported? ces-convert)

;;===============================================================
;; Basic utility
;;

;; returns list of major and minor versions in integers
(define (mime-parse-version field)
  (and-let* ([ field ]
             [s (string-concatenate
                 (map x->string (rfc822-field->tokens field)))]
             [m (#/^(\d+)\.(\d+)$/ s)])
    (map (lambda (d) (x->integer (m d))) '(1 2))))

(define-constant *ct-token-chars*
  (char-set-difference #[\x21-\x7e] #[()<>@,\;:\\\"/\[\]?=]))

;; RFC2045 Content-Type header field
;; returns (<type> <subtype> (<attribute> . <value>) ...)
(define (mime-parse-content-type field)
  (and field
       (call-with-input-string field
         (lambda (input)
           (and-let* ([type (rfc822-next-token input `(,*ct-token-chars*))]
                      [ (string? type) ]
                      [ (eqv? #\/ (rfc822-next-token input '())) ]
                      [subtype (rfc822-next-token input `(,*ct-token-chars*))]
                      [ (string? subtype) ])
             (list* (string-downcase type)
                    (string-downcase subtype)
                    (mime-parse-parameter-value input)))))))

;; RFC2183 Content-Disposition header field
;; returns (<token> (<attribute> . <value>) ...)
(define (mime-parse-content-disposition field)
  (and field
       (call-with-input-string field
         (lambda (input)
           (and-let* ([token (rfc822-next-token input `(,*ct-token-chars*))]
                      [ (string? token) ])
             (cons (string-downcase token)
                   (mime-parse-parameter-value input)))))))
           

;; parse a parameter-values type header field
;;   ;parameter=value;parameter=value
;; => ((parameter . value) ...)
;; NB: This will support RFC2231, but not yet.
(define (mime-parse-parameter-value input)
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
           => (lambda (p) (loop (cons p r)))]
          [else (reverse! r)])))

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
         => (lambda (m)
              (if (equal? (m'after) "")
                (%mime-decode-word word (m 1) (m 2) (m 3))
                word))]
        [else word]))

;; Decode the entire header field body, possibly a mixture of
;; encoded-words and orginary words.  NOTE: If you apply this to
;; a structured field body, the decoded words may contain special
;; characters meaningful to the structured body and thus confuse
;; the parser.  The correct order is to parse first, then apply
;; mime-decode-word for each token.
(define (mime-decode-text body)
  (let loop ((s body))
    (receive (pre rest) (string-scan s "=?" 'before*)
      (cond [(not pre) s]
            [(rxmatch *mime-encoded-header-rx* rest)
             => (lambda (m)
                  (string-append pre
                                 (%mime-decode-word (m 0) (m 1) (m 2) (m 3))
                                 (loop (m'after))))]
            [else s]))))

;; Encode a single word.  This one always encode, even WORD contains
;; US-ASCII characters only.
(define (mime-encode-word word . keys)
  (let-keywords keys ((charset "utf-8")
                      (transfer-encoding 'base64))
    (let ((converted
           (cond [(ces-upper-compatible? charset (gauche-character-encoding))
                  word]
                 [else
                  (ces-convert word (gauche-character-encoding) charset)]))
          (enc (%canonical-encoding transfer-encoding)))
      (format "=?~a?~a?~a?=" charset enc
              ((if (eq? enc 'B)
                 base64-encode-string
                 quoted-printable-encode-string)
               converted :line-width #f)))))

;; Encode entire body if necessary, considering line folding.
;; At this moment, this function just treat the body as unstructured
;; text.  It's not safe to pass structured text.  In future we may have
;; a keyword option to make body parsed as structured text.
(define (mime-encode-text body . keys)
  (let-keywords keys ((charset "utf-8")
                      (transfer-encoding 'base64)
                      (line-width 76)
                      (start-column 0)
                      (force #f))
    (let ((enc (%canonical-encoding transfer-encoding))
          (cslen (string-length (x->string charset)))
          (pass-through? (and (not force)
                              (ces-upper-compatible? charset 'ascii)
                              (string-every #[\x00-\x7f] body))))
      ;; estimates the length of an encoded word.  it is not trivial since
      ;; we have to ces-convert each segment separately, and there's no
      ;; general way to estimate the size of ces-converted string.
      ;; we throw in some heuristics here.
      (define (estimate-width s i)
        (let1 na (string-count s #[\x00-\x7f] 0 i)
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
            (let loop ((k (min (string-length str) (quotient width 2))))
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
                (encode body (- line-width start-column) 1.0)))]))))

(define (%canonical-encoding transfer-encoding)
  (case transfer-encoding
    [(B b base64) 'B]
    [(Q q quoted-printable) 'Q]
    [else (error "unsupported MIME header encoding specifier:"
                 transfer-encoding)]))
  

;;===============================================================
;; Virtual port to recognize mime boundary
;;

(define-class <mime-port> (<buffered-input-port>)
  ((state :init-form 'prologue)
   ;; prologue -> boundary <-> body -> eof
   ))

;; Creates a procedural port, which reads from SRCPORT until it reaches
;; either EOF or MIME boundary.  Basically it runs a DFA.
(define (make-mime-port boundary srcport)
  (define q (make-queue))
  (define --boundary (string->u8vector #`"--,boundary"))

  (define port (make <mime-port>))

  (define eof (read-from-string ""))

  (define (deq! q)
    (if (queue-empty? q) eof (dequeue! q)))
  
  (define (fifo! q b)
    (enqueue! q b) (dequeue! q))

  (define (getb)
    (if (queue-empty? q)
      (case (ref port 'state)
        [(prologue) (skip-prologue)]
        [(boundary eof) eof]
        [else (newb)])
      (dequeue! q)))

  (define (newb)
    (match (read-byte srcport)
      [(and #x0d b) ;; CR, check to see LF
       (let1 b2 (peek-byte srcport)
         (if (eqv? b2 #x0a)
           (begin
             (read-byte srcport)
             (enqueue! q b #x0a)
             (check-boundary))
           b))]
      [(and #x0a b) ;; LF, check boundary
       (enqueue! q b) (check-boundary)]
      [(? eof-object?) (set! (ref port 'state) 'eof) eof]
      [b b]))

  (define (check-boundary)
    (let loop ((b   (peek-byte srcport))
               (ind 0)
               (max (u8vector-length --boundary)))
      (cond [(eof-object? b) (deq! q)]
            [(= ind max)
             (cond [(memv b '(#x0d #x0a)) ;;found boundary
                    (read-byte srcport)   ;;consume LF or CRLF
                    (when (and (eqv? #x0d b) 
                               (eqv? #x0a (peek-byte srcport)))
                      (read-byte srcport))
                    (dequeue-all! q)
                    (set! (ref port 'state) 'boundary)
                    eof]
                   [(eqv? b #x2d) ;; maybe end boundary
                    (enqueue! q (read-byte srcport))
                    (cond [(eqv? (peek-byte srcport) #x2d);; yes, end boundary
                           (read-byte srcport)
                           (dequeue-all! q)
                           (skip-epilogue)]
                          [else (deq! q)])]
                   [else (deq! q)])]
            [(= b (u8vector-ref --boundary ind))
             (enqueue! q (read-byte srcport))
             (loop (peek-byte srcport) (+ ind 1) max)]
            [(queue-empty? q) (newb)]
            [else (dequeue! q)])))

  ;; Reads past the first boundary.  The first boundary may appear
  ;; at the beginning of the message (instead of after CRLF), so
  ;; we need slightly different handling than the normal getb.
  (define (skip-prologue)
    (let loop ((b (check-boundary)))
      (cond
       [(eof-object? b)
        (cond [(eq? (ref port 'state) 'boundary) ; we've found the boundary
               (set! (ref port 'state) 'body) (getb)]
              [else                              ; no boundary found
               (set! (ref port 'state) 'eof) eof])]
       [(queue-empty? q) (loop (newb))]
       [else (dequeue-all! q) (loop (newb))])))

  (define (skip-epilogue)
    (let loop ((b (read-byte srcport)))
      (if (eof-object? b)
        (begin (set! (ref port 'state) 'eof) b)
        (loop (read-byte srcport)))))
  
  ;; fills vector, until it sees either
  ;;   (1) vec got full
  ;;   (2) srcport reaches EOF
  ;;   (3) mime-boundary is read
  (define (fill vec)
    (let1 len (u8vector-length vec)
      (let loop ((ind 0))
        (if (= ind len)
          len
          (let1 b (getb)
            (if (eof-object? b)
              ind
              (begin (u8vector-set! vec ind b)
                     (loop (+ ind 1)))))))))

  (set! (ref port 'fill) fill)
  port)

;;===============================================================
;; Basic streaming parser
;;

(define-constant *eof-object* (read-from-string ""))

;; message information packet
(define-class <mime-part> ()
  ((type     :init-keyword :type)
   (subtype  :init-keyword :subtype)
   (parameters :init-keyword :parameters)
   (transfer-encoding :init-keyword :transfer-encoding)
   (headers  :init-keyword :headers)
   (parent   :init-keyword :parent :init-value #f)
   (index    :init-keyword :index :init-value 0)
   (content  :init-value #f)
   ))

(define (mime-parse-message port headers handler)
  (internal-parse port headers handler #f 0
                  '("text" "plain" ("charset" . "us-ascii"))))

(define (internal-parse port headers handler parent index default-type)
  (let* ((ctype (or (mime-parse-content-type
                     (rfc822-header-ref headers "content-type"))
                    default-type))
         (enc   (rfc822-header-ref headers "content-transfer-encoding" "7bit"))
         (packet (make <mime-part>
                   :type (car ctype)
                   :subtype (cadr ctype)
                   :parameters (cddr ctype)
                   :transfer-encoding enc
                   :parent parent
                   :index index
                   :headers headers))
         )
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
  (let* ((boundary (or (assoc-ref (ref packet 'parameters) "boundary")
                       (error "No boundary given for multipart message")))
         (default-type (if (equal? (ref packet 'subtype) "digest")
                         '("message" "rfc822")
                         '("text" "plain" ("charset" . "us-ascii"))))
         (mime-port (make-mime-port boundary port))
         )
    (let loop ((index 0)
               (contents '()))
      (let* ((headers (rfc822-header->list mime-port))
             (r (internal-parse mime-port headers handler
                                packet index
                                default-type)))
        (case (ref mime-port 'state)
          ((boundary)
           (set! (ref mime-port 'state) 'body)
           (loop (+ index 1) (cons r contents)))
          ((eof)
           (set! (ref packet 'content) (reverse! (cons r contents)))
           packet)
          (else ;; parser returned without readling entire part.
           ;; discard the rest of the part.
           (let loop ((b (read-byte port)))
             (unless (eof-object? b)
               (loop (read-byte port))))
           packet))))
    ))

(define (message-parse port packet handler)
  (let* ((headers (rfc822-header->list port))
         (r (internal-parse port headers handler packet 0
                            '("text" "plain" ("charset" . "us-ascii")))))
    (set! (ref packet 'content) (list r)))
  packet)

;;===============================================================
;; Body readers
;;

(define (mime-retrieve-body packet inp outp)

  (define (read-line/nl)
    (let loop ((c (read-char inp))
               (chars '()))
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
    (let loop ((line (read-line/nl)))
      (unless (eof-object? line)
        (display (decoder line) outp)
        (loop (read-line/nl)))))

  (define (read-base64)
    (define (base64-output string out)
      (with-input-from-string string
        (cut with-port-locking (current-input-port)
             (cut with-output-to-port out base64-decode))))
    (let1 buf (open-output-string :private? #t)
      (let loop ((line (read-line inp)))
	(unless (eof-object? line)
          (display line buf)
          (loop (read-line inp))))
      (base64-output (get-output-string buf) outp))
    )

  (with-port-locking inp
    (lambda ()
      (let1 enc (ref packet 'transfer-encoding)
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
    (lambda (outp)
      (with-port-locking outp
        (cut mime-retrieve-body packet inp outp))))
  filename)

(provide "rfc/mime")
