;;;; net.file.base -- uri base classes and generics

(define-module net.file.base
  (export <net-object> open-input-uri fetch-uri open-output-uri put-uri
          uri-directory-files))
(select-module net.file.base)

(define-class <uri-object> ()
  ((path :init-value #f :init-keyword :path :getter path-of)))

(define-generic object->uri)
(define-generic uri-scheme-of)

(define-generic open-input-uri)
(define-generic fetch-uri)
(define-generic open-output-uri)
(define-generic put-uri)

(define-generic uri-directory-uris)
(define-generic uri-directory-objects)
