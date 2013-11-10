;;;; net.file.mailto -- net.file access to the sending mail

(define-module net.file.mailto
  (use srfi-1)   ;; list library
  (use srfi-2)   ;; and-let*
  (use srfi-13)  ;; string library
  (use net.smtp)
  (extend net.file.base)
  (export uri->mailto-object))
(select-module net.file.mailto)

(define-class <uri-mailto> (<uri-object>)
  ((recipients :init-value '() :init-keyword :recipients :getter recipients-of)
   (headers    :init-value '() :init-keyword :headers    :getter headers-of)))

(define (uri->mailto-object specific)
  (rxmatch-case specific
    (#/^([^?]*)(\?(.*))?$/ (#f rcpt1 #f headers1)
      (let ((rcpt (string-split rcpt1 ","))
            (headers (map (lambda (x)
                            (and-let* ((h (string-split x "=")))
                              (list (string-downcase (car h)) (cadr h))))
                          (filter (^x (not (equal? x "")))
                                  (string-split (or headers1 "") "&")))))
        (and-let* ((to (assoc "to" headers)))
          (push! rcpt (cadr to))
          (set! headers (delete! to headers eq?)))
        (make <uri-mailto> :path specific :recipients rcpt :headers headers)))))

(define-method uri-scheme-of ((obj <uri-mailto>)) "mailto")

;; should this do any send-mail processing by default?

(define-method open-output-uri ((obj <uri-mailto>))
  (let ((mess '()))
    (open-output-buffered-port
     (lambda (data)
       (if data
         (let ((size (string-size data)))
           (push! mess data)
           size)
         ;; data is #f, message ready to send
         (apply send-mail
                :body (string-incomplete->complete (string-concatenate-reverse mess))
                :to (recipients-of obj)
                (concatenate (map (^x (list (make-keyword (car x)) (cadr x)))
                                  (headers-of obj))))))
     1024)))


