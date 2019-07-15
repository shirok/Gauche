;;;
;;; html-lite.scm - lightweight HTML construction
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

(define-module text.html-lite
  (use text.tree)
  (use srfi-1)
  (export html-escape html-escape-string html-doctype)
  )
(select-module text.html-lite)

;; Escaping ---------------------------------------------
(define (html-escape)
  (generator-for-each (^c (case c
                            [(#\<) (display "&lt;")]
                            [(#\>) (display "&gt;")]
                            [(#\&) (display "&amp;")]
                            [(#\") (display "&quot;")]
                            [else (display c)]))
                      read-char))

(define (html-escape-string string)
  (with-string-io (x->string string) html-escape))

;; Doctype ----------------------------------------------

;; Doctype database
;;  (type ...) => (xml? doctype)
(define-constant *doctype-alist*
  '(((:strict :html :html-strict :html-4.01 :html-4.01-strict)
     #f
     "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"
       \"http://www.w3.org/TR/html4/strict.dtd\">\n")
    ((:transitional :html-transitional :html-4.01-transitional)
     #f
     "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
       \"http://www.w3.org/TR/html4/loose.dtd\">\n")
    ((:frameset :html-frameset :html-4.01-frameset)
     #f
     "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\"
       \"http://www.w3.org/TR/html4/frameset.dtd\">\n")
    ((:xhtml-1.0-strict :xhtml-1.0)
     #t
     "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
       \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n")
    ((:xhtml-1.0-transitional)
     #t
     "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
       \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n")
    ((:xhtml-1.0-frameset)
     #t
     "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\"
       \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">\n")
    ((:xhtml-1.1)
     #t
     "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"
       \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">\n")
    ((:html-5)
     #t
     "<!DOCTYPE html>")
    ))

(define (html-doctype :key (type :html-4.01-strict))
  (cond ((find (^e (memq type (car e))) *doctype-alist*)
         => caddr )
        (else (error "Unknown doctype type spec" type))))

;; Elements ------------------------------------------------

(define (make-html-element name . args)
  (let ((empty? (get-keyword :empty? args #f)))
    (define (K k) (keyword->string k)) ;; we don't need leading colon
    (define (get-attr args attrs)
      (cond ((null? args) (values (reverse attrs) args))
            ((keyword? (car args))
             (cond ((null? (cdr args))
                    (values (reverse (list* (K (car args)) " " attrs)) args))
                   ((eq? (cadr args) #f)
                    (get-attr (cddr args) attrs))
                   ((eq? (cadr args) #t)
                    (get-attr (cddr args) (list* (K (car args)) " " attrs)))
                   (else
                    (get-attr (cddr args)
                              (list* (format #f "=\"~a\""
                                             (html-escape-string (x->string (cadr args))))
                                     (K (car args))
                                     " "
                                     attrs)))))
            (else (values (reverse attrs) args))))

    (if empty?
      (lambda args
        (receive (attr args) (get-attr args '())
          (unless (null? args)
            (errorf "element ~s can't have content: ~s" name args))
          (list "<" name attr " />")))
      (lambda args
        (receive (attr args) (get-attr args '())
          (list "<" name attr ">" args "</" name "\n>"))))))

(define-macro (define-html-elements . elements)
  (define (make-scheme-name name)
    (string->symbol (format #f "html:~a" name)))
  (let loop ((elements elements)
             (r '()))
    (cond ((null? elements) `(begin ,@(reverse r)))
          ((and (pair? (cdr elements)) (eqv? (cadr elements) :empty))
           (loop (cddr elements)
                 (list* `(define ,(make-scheme-name (car elements))
                           (make-html-element ',(car elements) :empty? #t))
                        `(export ,(make-scheme-name (car elements)))
                        r)))
          (else
           (loop (cdr elements)
                 (list* `(define ,(make-scheme-name (car elements))
                           (make-html-element ',(car elements)))
                        `(export ,(make-scheme-name (car elements)))
                        r))))
    ))

;; http://www.w3.org/TR/html4/sgml/dtd.html

;; TEXT MARKUP

;; %fontstyle
(define-html-elements tt i b big small)

;; %phrase
(define-html-elements em strong dfn code samp kbd var cite abbr acronym)

(define-html-elements sub sup span bdo br :empty)

;; HTML CONTENT MODELS

;; DOCUMENT BODY

(define-html-elements body address div)

;; THE ANCHOR ELEMENT
(define-html-elements a)

;; CLIENT-SIDE IMAGE MAPS
(define-html-elements map area :empty)

;; THE LINK EKEMENT
(define-html-elements link :empty)

;; IMAGES
(define-html-elements img :empty)

;; OBJECT
(define-html-elements object param :empty)

;; HORIZONTAL RULE
(define-html-elements hr :empty)

;; PARAGRAPHS
(define-html-elements p)

;; HEADINGS
(define-html-elements h1 h2 h3 h4 h5 h6)

;; PREFORMATTED
(define-html-elements pre)

;; INLINE QUOTES
(define-html-elements q)

;; BLOCK-LIKE QUOTES
(define-html-elements blockquote)

;; INSERTED/DELETED TEXT
(define-html-elements ins del)

;; LISTS
(define-html-elements dl dt dd ol ul li)

;; FORMS
(define-html-elements form label input :empty select optgroup option
                      textarea fieldset legend button)

;; TABLES
(define-html-elements table caption thead tfoot tbody colgroup
                      col :empty tr th td)

;; DOCUMENT HEAD
(define-html-elements head title base :empty meta :empty
                      style script noscript)

;; DOCUMENT STRUCTURE
(define-html-elements html)

;; FRAMES
(define-html-elements frameset frame noframes iframe)

;; Elements available since HTML5, in the order they are defined in
;; Section "4. The elements of HTML" of <https://www.w3.org/TR/html5/>.
(define-html-elements article section nav aside
                      header footer
                      figure figcaption
                      main
                      ruby rb rt rtc rp
                      data time
                      mark bdi bdo
                      wbr :empty
                      picture source
                      embed :empty
                      video audio track :empty
                      datalist
                      output
                      progress
                      meter
                      details summary dialog
                      template
                      canvas)
