;;;; net.file.file -- net.file access to the local filesystem

(define-module net.file.file
  (use rfc.uri)
  (use file.util)
  (extend net.file.base)
  (export uri->file-object))
(select-module net.file.file)

(define-class <uri-file> (<uri-object>) ())

(define (uri->file-object specific)
  (make <uri-file> :path specific))

(define-method uri-scheme-of ((uri <uri-file>)) "file")

(define-method open-input-uri ((uri <uri-file>))
  (open-input-file (path-of uri)))

(define-method open-output-uri ((uri <uri-file>))
  (open-output-file (path-of uri)))

(define-method uri-directory-objects ((uri <uri-file>))
  (let ((path (path-of uri)))
    (if (file-is-directory? path)
      (map (cut make <uri-file> :path <>) (directory-list uri :children? #t :add-path #t))
      '())))

(define-method uri-directory-uris ((uri <uri-file>))
  (let ((path (path-of uri)))
    (if (file-is-directory? path)
      (map (cut string-append "file:" <>) (directory-list uri :children? #t :add-path #t))
      '())))
