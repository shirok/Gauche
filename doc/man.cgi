#!/home/shiro/bin/gosh
;; -*- coding:utf-8 -*-

;;; This script is used in practical-scheme.net/gauche/man/ to redirect
;;; GET request with p=<topic> query to appropriate page.
;;; <topic> can be a procedure/syntax/macro/variable name or a node name.

(use srfi.13)
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

(define (file-path lang draft? filename)
  (build-path (if (memq lang '(ja jp))
                (if draft? "gauche-refj-draft" "gauche-refj")
                (if draft? "gauche-refe-draft" "gauche-refe"))
              filename))

(define (get-indexed-uri lang draft? index-file name)
  (and-let* ([index-page (pick-initial (string-upcase (string-take name 1))
                                       (file-path lang draft? index-file))]
             [target-file (let1 p (file-part index-page)
                            (if (string-null? p)
                              (file-part index-file)
                              p))])
    (pick-index-item name (file-path lang draft? target-file))))

(define (get-index-pages lang draft?)
  (let1 rx (if (eq? lang 'en)
             #/<A HREF=\"([^\"]*)\"[^>]*>.*(?:Function and Syntax Index|Module Index|Class Index|Variable Index)<\/A>/i
             #/<A HREF=\"([^\"]*)\"[^>]*>.*(?:Index - 手続きと構文索引|Index - モジュール索引|Index - クラス索引|Index - 変数索引)<\/A>/i)
    ($ map file-part
       $ multi-pick-from-file rx (file-path lang draft? "index.html"))))

(define (search-from-index lang draft? name)
  (match (get-index-pages lang draft?)
    [(fn md cl va)
     (or (get-indexed-uri lang draft? fn name) ;; from function
         (get-indexed-uri lang draft? md name) ;; from module
         (and-let1 m (#/^&lt\;(.*)&gt\;$/ name)
           (get-indexed-uri draft? lang cl (m 1))) ;; from class
         (get-indexed-uri lang draft? va name) ;; from variable
         )]
    [_ (error "Couldn't find index")]))

(define (search-from-toc lang draft? name)
  (let1 picker #/<A id=\"toc-[^\"]*\" HREF=\"([^\"]*)\">\d+(?:\.[.\d]*)?\s+(.*?)<\/A>/i
    (call-with-input-file (file-path lang draft? "index.html")
      (^p (any (^l (and-let1 m (picker l)
                     (let1 sectitle (regexp-replace-all #/<\/?CODE>/i (m 2) "")
                       (and (equal? name sectitle)
                            (m 1)))))
               (port->string-lseq p))))))

(define (main-1 args)
  (cgi-main
   (^[params]
     (let* ([lang (cgi-get-parameter "l" params
                                     :default 'en :convert string->symbol)]
            [enc  (cgi-get-parameter "en" params)]
            [draft? (equal? (cgi-get-parameter "v" params) "draft")]
            [raw-name (string-trim-both
                       (cgi-get-parameter "p" params :default ""))]
            [name (html-escape-string
                   (cond [(and enc (ces-conversion-supported? enc #f))
                          (ces-convert raw-name enc)]
                         [(eq? lang 'jp)
                          (ces-convert raw-name "*JP")]
                         [else raw-name]))]
            [uri  (file-path lang
                             draft?
                             (or (and name
                                      (positive? (string-length name))
                                      (if (string-every #[\x21-\x7e] name)
                                        (or (search-from-index lang draft? name)
                                            (search-from-toc lang draft? name))
                                        (search-from-toc lang draft? name)))
                                 "index.html"))])
       `("Status: 302 Moved\n" ,(cgi-header :location uri))))))

(cond-expand
 [(library www.cgi.throttle)
  (use www.cgi.throttle)
  (define (main args)
    (cgi-throttle
     "memcache:localhost:11211"
     '((GET :window 30 :count 20))
     (cut main-1 args)))]
 [else
  (define (main args)
    (main-1 args))])

;; Local variables:
;; mode: scheme
;; end:
