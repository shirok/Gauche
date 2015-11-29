;;
;; Sample to use <listener> and <selector> for server application
;;
;; NOTE: This program is just for demonstation.  DO NOT RUN THIS
;; PROGRAM ON THE MACHINE THAT IS NOT PROTECTED PROPERLY, or anybody
;; can obtain your account remotely.

(use gauche.net)
(use gauche.selector)
(use gauche.listener)

(define (scheme-server port)
  (let ((selector (make <selector>))
        (servers  (make-server-sockets #f port :reuse-addr? #t))
        (cid      0))

    (for-each
     (lambda (server)

       (define (accept-handler sock flag)
         (let* ((client (socket-accept server))
                (id     cid)
                (input  (socket-input-port client :buffering :none))
                (output (socket-output-port client))
                (finalize (lambda ()
                            (selector-delete! selector (socket-fd client) #f #f)
                            (socket-close client)
                            (format #t "client #~a disconnected\n" id)))
                (listener (make <listener>
                            :input-port input
                            :output-port output
                            :error-port output
                            :prompter (lambda () (format #t "client[~a]> " id))
                            :finalizer finalize))
                (handler (listener-read-handler listener))
                )
           (format #t "client #~a from ~a\n" cid (socket-address client))
           (inc! cid)
           (listener-show-prompt listener)
           (selector-add! selector (socket-fd client) (lambda _ (handler)) '(r))))

       (selector-add! selector
                      (socket-fd server)
                      accept-handler
                      '(r)))
     servers)
    (format #t "scheme server started on port ~s\n" port)
    (do () (#f) (selector-select selector))))

(define (main args)
  (scheme-server 1359)
  0)
