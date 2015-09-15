;;;
;;; SRFI-112  Environment inquiry
;;;

(define-module srfi-112
  (export implementation-name
          implementation-version
          cpu-architecture
          machine-name
          os-name
          os-version))
(select-module srfi-112)

(define (implementation-name) "Gauche")
(define (implementation-version) (gauche-version))

(define (cpu-architecture) (~ (sys-uname) 4))  ; MACHINE field
(define (machine-name) (~ (sys-uname) 1))      ; NODENAME field
(define (os-name) (~ (sys-uname) 0))           ; SYSNAME field
(define (os-version) (~ (sys-uname) 2))        ; RELEASE field

