;;;
;;; html-lite.scm - lightweight HTML construction
;;;
;;;  Copyright(C) 2001 by Shiro Kawai (shiro@acm.org)
;;;
;;;  Permission to use, copy, modify, distribute this software and
;;;  accompanying documentation for any purpose is hereby granted,
;;;  provided that existing copyright notices are retained in all
;;;  copies and that this notice is included verbatim in all
;;;  distributions.
;;;  This software is provided as is, without express or implied
;;;  warranty.  In no circumstances the author(s) shall be liable
;;;  for any damages arising out of the use of this software.
;;;
;;;  $Id: html-lite.scm,v 1.9 2003-02-06 22:03:20 shirok Exp $
;;;

(define-module text.html-lite
  (use text.tree)
  (export html-escape html-escape-string html-doctype)
  )
(select-module text.html-lite)

(define (html-escape)
  (port-for-each (lambda (c)
                   (case c
                     ((#\<) (display "&lt;"))
                     ((#\>) (display "&gt;"))
                     ((#\&) (display "&amp;"))
                     ((#\") (display "&quot;"))
                     (else (display c))
                     ))
                 read-char))

(define (html-escape-string string)
  (with-string-io string html-escape))

(define (html-doctype . args)
  (let ((type (get-keyword :type args :strict)))
    (case type
      ((:strict)
       "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\">\n")
      ((:transitional)
       "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n")
      ((:frameset)
       "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\">\n")
      (else
       (error "Unknown doctype" type)))))

(define (make-html-element name . args)
  (let ((empty? (get-keyword :empty? args #f)))
    (define (get-attr args attrs)
      (cond ((null? args) (values (reverse attrs) args))
            ((keyword? (car args))
             (cond ((null? (cdr args))
                    (values (reverse (list* (car args) " " attrs)) args))
                   ((eq? (cadr args) #f)
                    (get-attr (cddr args) attrs))
                   ((eq? (cadr args) #t)
                    (get-attr (cddr args) (list* (car args) " " attrs)))
                   (else
                    (get-attr (cddr args)
                              (list* (format #f "=\"~a\""
                                             (html-escape-string (x->string (cadr args))))
                                     (car args)
                                     " "
                                     attrs)))))
            (else (values (reverse attrs) args))))

    (if empty?
        (lambda args
          (receive (attr args) (get-attr args '())
            (unless (null? args)
              (error "element ~s can't have content: ~s" args))
            (list "<" name attr ">")))
        (lambda args
          (receive (attr args) (get-attr args '())
            (list "<" name attr ">" args "\n</" name ">"))))))

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

;; cLIENT-SIDE IMAGE MAPS
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

(provide "text/html-lite")


