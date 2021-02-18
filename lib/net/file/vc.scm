;;;; net.file.vc -- net.file access to the version control systems

(define-module net.file.vc
  (use vc)
  (use util.debug)
  (extend net.file.base)
  (export uri->vc-object))
(select-module net.file.vc)

(define uri->vc-object
  (let ((orig-uri->vc-object uri->vc-object))
    (lambda (specific)
      (orig-uri->vc-object specific))))

(define-method uri-scheme-of ((obj <vc-object>)) "vc")

(define-method open-input-uri ((obj <vc-data>))
  (vc-open-input obj))
