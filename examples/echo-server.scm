;; Example of gauche.selector

(use gauche.net)
(use gauche.selector)
(use gauche.uvector)

(define (echo-server port)
  (let ((selector (make <selector>))
        (servers  (make-server-sockets #f port :reuse-addr? #t)))

    (define (echo client input output)
      (print "  port-name = " (port-name input))
      (let ((str (read-uvector <u8vector> 4096 input)))
        (print "  read-data = " str)
        (if (eof-object? str)
            (begin (selector-delete! selector (socket-fd client) #f #f)
                   (socket-close client))
            (begin (write-uvector str output)
                   (flush output)))
        (print "  socket-status = " (socket-status client))))

    (for-each
     (lambda (server)

       (define (accept-handler sock flag)
         (let* ((client (socket-accept server))
                (input  (socket-input-port client :buffering :none))
                (output (socket-output-port client)))
           (print "client-socket-fd = " (socket-fd client))
           (selector-add! selector
                          (socket-fd client)
                          (lambda (sock flag)
                            (echo client input output))
                          '(r))))

       (selector-add! selector
                      (socket-fd server)
                      accept-handler
                      '(r)))
     servers)
    (do () (#f) (print "select-return-value = " (selector-select selector)))))

(define (main args)
  (print "echo server starting on port 3131")
  (echo-server 3131)
  0)
