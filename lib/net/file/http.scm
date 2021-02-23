;;;; net.file.http -- net.file access to http

(define-module net.file.http
  (use scheme.list) ;; list library
  (use srfi-13)  ;; string library
  (use net.http)
  (extend net.file.base)
  (export uri->http-object))
(select-module net.file.http)

(define-class <uri-http> (<uri-object>) ())

(define (uri->http-object specific)
  (make <uri-http> :path (string-append "http:" specific)))

(define-method uri-scheme-of ((obj <uri-http>)) "http")

(define-method open-input-uri ((obj <uri-http>))
  (open-input-http (path-of obj)))
