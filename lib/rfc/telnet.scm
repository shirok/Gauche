;;;
;;; telnet.scm - telnet protocol implementation
;;;
;;;  Copyright(C) 2001 by Shiro Kawai (shiro@acm.org)
;;;
;;;  Permission to use, copy, modify, distribute this software and
;;;  accompanying documentation for any purpose is hereby granted,
;;;  provided that existing copyright notices are retained in all
;;;  copies and that this notice is included verbatim in all
;;;  distributions.
;;;  This software is provided as is, without express or implied
;;;  warranty.  In no circumstances the author(s) shall be liable
;;;  for any damages arising out of the use of this software.
;;;
;;;  $Id: telnet.scm,v 1.1 2001-08-29 11:29:36 shirok Exp $
;;;

;;;  RFC 854  Telnet Protocol Specification
;;;  RFC 855  Telnet Option Specifications
;;;  RFC 856  Telnet Binary Transmission
;;;  RFC 857  Telnet Echo Option
;;;  RFC 858  Telnet Suppress Go Ahead option
;;;  RFC 859  Telnet Status Option
;;;  RFC 860  Telnet Timing Mark option
;;;  RFC 861  Telnet Extended Options option
;;;  RFC 1091 Telnet Terminal Type option
;;;  RFC 1096 Telnet X Display location option
;;;  RFC 1184 Telnet Linemode option
;;;  RFC 1372 Telnet Remote Flow option
;;;  RFC 1408 Telnet Environment option
;;;  RFC 1571 Telnet Environment option interoperability issues
;;;  RFC 1572 Telnet Environment option
;;;  RFC 2066 Telnet Charset option
;;;  RFC 2941 Telnet Authentication option
;;;  RFC 2840 Telnet Kermit option
;;;  RFC 2217 Telnet Com Port option
;;;  RFC 1073 Telnet Window Size option
;;;  RFC 1079 Telnet Terminal Speed option
;;;  RFC 727  Telnet logout option

(define-module rfc.telnet
  (use gauche.net)
  (export <telnet>
          ))
(select-module rfc.telnet)

(define *telnet-port*       23)

;; Control codes  (RFC 854)
(define *telnet-se*         240)        ;F0
(define *telnet-nop*        241)        ;F1
(define *telnet-data-mark*  242)        ;F2
(define *telnet-break*      243)        ;F3
(define *telnet-intr*       244)        ;F4
(define *telnet-abort*      245)        ;F5
(define *telnet-ayt*        246)        ;F6
(define *telnet-erase-char* 247)        ;F7
(define *telnet-erase-line* 248)        ;F8
(define *telnet-go-ahead*   249)        ;F9
(define *telnet-sb*         250)        ;FA
(define *telnet-will*       251)        ;FB
(define *telnet-wont*       252)        ;FC
(define *telnet-do*         253)        ;FD
(define *telnet-dont*       254)        ;FE
(define *telnet-iac*        255)        ;FF

;; Known options
(define *telnet-transmit-binary*    0)    ;RFC 856
(define *telnet-echo*               1)    ;RFC 857
(define *telnet-suppress-go-ahead*  3)    ;RFC 858
(define *telnet-status*             5)    ;RFC 859
(define *telnet-timing-mark*        6)    ;RFC 860
(define *telnet-logout*             18)   ;RFC 727
(define *telnet-terminal-type*      24)   ;RFC 1091
(define *telnet-naws*               31)   ;RFC 1073
(define *telnet-terminal-speed*     32)   ;RFC 1079
(define *telnet-toggle-flow-control* 33)  ;RFC 1372
(define *telnet-linemode*           34)   ;RFC 1184
(define *telnet-xdisploc*           35)   ;RFC 1096
(define *telnet-environ*            36)   ;RFC 1408, 1571
(define *telnet-authentication*     37)   ;RFC 2941
(define *telnet-new-environ*        39)   ;RFC 1572
(define *telnet-charset*            42)   ;RFC 2066
(define *telnet-com-port-option*    44)   ;RFC 2217
(define *telnet-kermit*             47)   ;RFC 2840
(define *telnet-exopl*              255)  ;RFC 861

;; telnet class
(define-class <telnet> ()
  ((socket  :initform #f :init-keyword :socket :getter socket-of)
   (input   :getter input-of)
   (output  :getter output-of)
   ))

(define-method initialize ((self <telnet>) initargs)
  (next-method)
  (let ((sock (setup-socket self initargs)))
    (slot-set! self 'input (socket-input-port sock :buffered? #f))
    (slot-set! self 'output (socket-output-port sock :buffered? #f))
    ))

(define (setup-socket self initargs)
  (let ((sock (socket-of self)))
    (cond ((not sock)
           (let ((host (get-keyword :host initargs #f))
                 (port (get-keyword :port initargs *telnet-port*)))
             (unless host
               (error "either connected socket or remote host name must be given to initialize a telnet session.   Given initargs is" initargs))
             (let ((sock (make-client-socket 'inet host port)))
               (slot-set! self 'socket sock)
               sock)))
          ((not (and (is-a? sock <socket>)
                     (eq? (socket-status sock) 'connected)))
           (error "connected socket required to initialize a telnet session, but got"
                  sock))
          (else sock))))

;;; Client operations

;; Send normal message.  IAC is escaped.
(define-method telnet-send-message ((self <telnet>) string)
  (let ((out (output-of self)))
    (with-input-from-string string
      (lambda ()
        (let loop ((b (read-byte)))
          (cond ((eof-object? b))
                ((= b *telnet-iac*) (write-byte b out)
                                    (write-byte b out)
                                    (loop (read-byte)))
                (else (write-byte b out)
                      (loop (read-byte)))))))
    (flush out)))

;; Send Command.
(define (%telnet-send-command self args)
  (let ((out (output-of self)))
    (write-byte *telnet-iac* out)
    (for-each (lambda (b) (write-byte b out)) args)))

(define-method telnet-recv ((self <telnet>))
  (let* ((inb (read-block 4000 (input-of self))))
    inb))

                              
