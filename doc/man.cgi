#!/home/shiro/bin/gosh
;; -*- coding:utf-8 -*-

;;; This script is used in practical-scheme.net/gauche/man/ to redirect
;;; GET request with p=<topic> query to appropriate page.
;;; <topic> can be a procedure/syntax/macro/variable name or a node name.

(use srfi-13)
(use file.util)
(use util.match)
(use text.html-lite)
(use www.cgi)
(use gauche.charconv)
(use gauche.lazy)

(define (pick-from-file rx file)
  (call-with-input-file file
    (^p (any (^l (and-let1 m (rx l) (m 1))) (port->string-lseq p)))))

(define (multi-pick-from-file rx file)
  (call-with-input-file file
    (^p (filter-map (^l (and-let1 m (rx l) (m 1))) (port->string-lseq p)))))
  
(define (pick-initial initial file)
  (pick-from-file
   (string->regexp #`"<A class=\"(?:.*?)\" HREF=\"(.*?)\"><b>,(regexp-quote initial)</b></A>" :case-fold #t)
   file))

(define (pick-index-item item file)
  (pick-from-file
   (string->regexp #`"<A HREF=\"(.*?)\">(?:<CODE>)+,(regexp-quote item)(?:</CODE>)+</A>" :case-fold #t)
   file))

(define (file-part path)
  (cond [(string-scan path "#" 'before)]
        [else path]))

(define (file-path lang filename)
  (build-path (if (memq lang '(ja jp)) "gauche-refj" "gauche-refe") filename))

(define (get-indexed-uri lang index-file name)
  (and-let* ([index-page (pick-initial (string-upcase (string-take name 1))
                                       (file-path lang index-file))]
             [target-file (let1 p (file-part index-page)
                            (if (string-null? p)
                              (file-part index-file)
                              p))])
    (pick-index-item name (file-path lang target-file))))

(define (get-index-pages lang)
  (let1 rx (if (eq? lang 'en)
             #/<A HREF=\"([^\"]*)\">.*(?:Function and Syntax Index|Module Index|Class Index|Variable Index)<\/A>/i
             #/<A HREF=\"([^\"]*)\">.*(?:Index - 手続きと構文索引|Index - モジュール索引|Index - クラス索引|Index - 変数索引)<\/A>/i)
    ($ map file-part
       $ multi-pick-from-file rx (file-path lang "index.html"))))

(define (search-from-index lang name)
  (match-let1 (fn md cl va) (get-index-pages lang)
    (or (get-indexed-uri lang fn name) ;; from function
        (get-indexed-uri lang md name) ;; from module
        (and-let1 m (#/^&lt\;(.*)&gt\;$/ name)
          (get-indexed-uri lang cl (m 1))) ;; from class
        (get-indexed-uri lang va name) ;; from variable
        )))

(define (search-from-toc lang name)
  (let1 picker #/<A NAME=\"toc-[^\"]*\" HREF=\"([^\"]*)\">\d+\.[.\d]*\s+(.*?)<\/A>/i
    (call-with-input-file (file-path lang "index.html")
      (^p (any (^l (and-let1 m (picker l)
                     (let1 sectitle (regexp-replace-all #/<\/?CODE>/i (m 2) "")
                       (and (equal? name sectitle)
                            (m 1)))))
               (port->string-lseq p))))))
             
(define (main args)
  (cgi-main
   (^[params]
     (let* ([lang (cgi-get-parameter "l" params
                                     :default 'en :convert string->symbol)]
            [enc  (cgi-get-parameter "en" params)]
            [raw-name (cgi-get-parameter "p" params)]
            [name (html-escape-string
                   (cond [(and enc (ces-conversion-supported? enc #f))
                          (ces-convert raw-name enc)]
                         [(eq? lang 'jp)
                          (ces-convert raw-name "*JP")]
                         [else raw-name]))]
            [uri  (file-path lang
                             (or (and name
                                      (positive? (string-length name))
                                      (if (string-every #[\x21-\x7e] name)
                                        (or (search-from-index lang name)
                                            (search-from-toc lang name))
                                        (search-from-toc lang name)))
                                 "index.html"))])
       `("Status: 302 Moved\n" ,(cgi-header :location uri))))))

;; Local variables:
;; mode: scheme
;; end:
