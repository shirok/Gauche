;; Example of gauche.selector
;;  $Id: echo-server.scm,v 1.1 2003-01-08 02:05:30 shirok Exp $

(use gauche.net)
(use gauche.selector)

(define (echo-server port)
  (let ((selector (make <selector>))
        (server   (make-server-socket 'inet port :reuse-addr? #t)))

    (define (accept-handler sock flag)
      (let* ((client (socket-accept server))
             (output (socket-output-port client)))
        (selector-add! selector
                       (socket-input-port client :buffered? #f)
                       (lambda (input flag)
                         (echo client input output))
                       '(r))))

    (define (echo client input output)
      (let ((str (read-block 4096 input)))
        (if (eof-object? str)
            (begin (selector-delete! selector input #f #f)
                   (socket-close client))
            (begin (display str output)
                   (flush output)))))

    (selector-add! selector
                   (socket-fd server)
                   accept-handler
                   '(r))
    (do () (#f) (selector-select selector))))

(define (main args)
  (print "echo server starting on port 3131")
  (echo-server 3131)
  0)

