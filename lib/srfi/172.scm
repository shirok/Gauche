;; srfi-172 - subset of R7RS 'safe' procedures

(define-module srfi.172
  (use scheme.base)
  (use scheme.case-lambda)
  (use scheme.char)
  (use scheme.complex)
  (use scheme.cxr)
  (use scheme.inexact)
  (use scheme.lazy)
  (extend srfi.172.functional)
  (export
   bytevector-copy! bytevector-u8-set! call-with-port close-input-port
   close-output-port close-port input-port? list-set!  output-port?
   peek-char peek-u8 port? read-bytevector read-bytevector!  read-char
   read-error?  read-line read-string read-u8 set! set-car!  set-cdr!
   string-copy!  string-fill!  string-set! textual-port?  vector-copy!
   vector-fill! vector-set!  write-bytevector write-char write-string
   write-u8))
